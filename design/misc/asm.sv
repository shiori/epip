///assembler for ip4
`define asm_msg(s, i = OVM_LOW, d = display)\
if(verb >= i)\
$d(s);

`define asm_err(s) $display({"Err: ", string'(s)});

function automatic int get_imm(string tk);
  string tk0 = tk.substr(0, 0);
  string tk1 = tk.substr(1, 1);
  string tk1n = tk.substr(1, tk.len() - 1);
  string tk2n = tk.substr(2, tk.len() - 1);
  if(tk0.tolower() == "o")
    get_imm = tk1n.atooct();
  else if(tk0.tolower() == "h")
    get_imm = tk1n.atohex();
  else if(tk0 == "0" && tk1.tolower() == "x")
    get_imm = tk2n.atohex();
  else if(tk0.tolower() == "b")
    get_imm = tk1n.atobin();
  else if(tk0.tolower() == "d")
    get_imm = tk1n.atoi();
  else
    get_imm = tk.atoi();
endfunction

function automatic void brk_token(string s, string sp[$], ref string tokens[$]);
  int cnt = 0, found = 1;
  tokens = {};

  ///break string into tokens
  for(int i = cnt; i < s.len(); i++)
    if(s[i] inside {sp}) begin
      if(!found) begin
        ///found a token end
        found = 1;
        tokens.push_back(s.substr(cnt, i - 1));
        cnt = i + 1;
      end
      else begin
        ///found, eating chars after
        cnt = i + 1;
      end
    end
    else if(i == (s.len() - 1))
      tokens.push_back(s.substr(cnt, i));
    else begin
      if(found) begin
        ///found next token start
        cnt = i;
        found = 0;
      end
    end
endfunction

class asmig;
  int ps; /// source operation point
  bit[4:0][3:0] vecOp, immOp, zeroOp, enOp, bp0Op, bp1Op, bp2Op, pdrOp, constOp;  /// operation tidOp
  bit[3:0] tagOp;  /// operation
  bit[4:0] nop;
  uchar adr[5][4], padr[5]; /// 0 of v0 is stored into adr[i][j] , 4 of p4 is stored into padr[i] 
  int imm[5][4] , cont[5][4];  /// imm[i][0] = rd;
  string tag;
  string op[5];
  bit[4:0] en, s, si;  /// option
  bit mu, su, fcrl, emsk, vxup, alocd, s2g, r3w1d;  /// option
  bit fcrPc, pb, az;
  bit ldH, ldB, ldLk, ldHu, ldBu;
  bit stH, stB, stCn;
  bit[1:0] sop, devcah, opcah, ldua, ldty, stua, stty, cmpxua, cmpxty, fetaua, fetaty, mcty;  /// option
  bit[2:0] mop, ctyp;  /// option 
  bit[3:0] mcfun, mtyp;
  uchar icnt;  
  uchar chkGrp, grpMsk;
  uchar grpsize;
  uint pc;
  bit[2:0] allAdr[25];
  bit isVec[5]; 
  uchar adrcnt;
  i_gs0_t gs0;
  i_gs1_u gs1;
  inst_u inst[5];
  bit vrfEn[CYC_VEC][NUM_VRF_BKS],
      srfEn[CYC_VEC][NUM_SRF_BKS];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_VRF_BKS];
///  bit[3:0][7:0] co[NUM_BP_CO];
  bit[3:0][7:0] co[NUM_BP_CO];
  bit coEn[NUM_BP_CO];
  int contNum;  
  uint adrBits;
  bit[1:0] mrst;
  
  function new();
    ps = 1;
    nop = 0;
    vecOp = 0;
    immOp = 0;
    zeroOp = 0;
    enOp = 0;
    tagOp = 0;
    constOp = 0 ;
    nop = 0;
    r3w1d = 0;
    fcrPc = 0;
    pb = 0;
    az = 0;
    mu = 0;
    su = 0;
    fcrl = 0;
    emsk = 0;
    vxup = 0;
    ldH = 0;
    ldB = 0;
    ldHu = 0;
    ldBu = 0;
    ldLk = 0;
    stH = 0;
    stB = 0;
    stCn = 0;
    en = 0;
    s = 0;
    si = 0;
    isVec = '{default : 0};
    chkGrp = 1;
    grpMsk = 0;
    adrcnt = 0;
    contNum = 0;
    for(int i=0; i < NUM_BP_CO; i++)
      coEn[i] = 0;
    mrst = 0;
  endfunction

  function bit pack_grp(ovm_verbosity verb);
      uchar v_icnt;
    ///assemble each inst
    foreach(inst[i]) begin
      uchar adru[3], bk[3];
      uchar bksel[3] = '{default : 15};
      bit dual = 0, three = 0, one = 0, two = 0;
      
      if(!en[i]) break;
      `asm_msg($psprintf("assemble inst %0d", i), OVM_HIGH);
      isVec[i] = vecOp[i][0];
      inst[i].i.p = padr[i];
      inst[i].i.b.ir3w1.rd = adr[i][0];
      
      case(op[i])
        "li"    :
          begin
            inst[i].i.op = iop_li;
            {inst[i].i.b.i26.imm1, inst[i].i.b.i26.imm0} = imm[i][1]; 
          end
        "lu"    :
          begin
            inst[i].i.op = iop_lu;
            {inst[i].i.b.i26.imm1, inst[i].i.b.i26.imm0} = imm[i][1]; 
          end
        "add"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_addsi : iop_addi;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
///              if(constOp[i][1])
///                case(cont[i][1])
///                  0: inst[i].i.b.ir1w1.rs = 8;
///                  1: inst[i].i.b.ir1w1.rs = 9;
///                  2: inst[i].i.b.ir1w1.rs = 10;
///                  default: `asm_err("constant port assignment failed!");
///                endcase
///              else
                one = 1;
            end
            else if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_add3;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = r3w1d;
              three = 1;
              if(r3w1d) dual = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = s[i] ? iop21_add : iop21_uadd;
              two = 1;
            end
          end
        "and"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_andsi : iop_andi;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_and;
              two = 1;
            end
          end
        "or "   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_orsi : iop_ori;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_or;
              two = 1;
            end
          end
        "xor"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_xorsi : iop_xori;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_xor;
              two = 1;
            end
          end
        "mul"   :
          begin
            if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_mul;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = r3w1d;
              three = 1;
              if(r3w1d) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "mad"   :
          begin
            if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_mad;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = r3w1d;
              three = 1;
              if(r3w1d) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "msu"   :
          begin
            if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_msu;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = r3w1d;
              three = 1;
              if(r3w1d) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "sub"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sub;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "usub"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_usub;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end 
          end
        "srl"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_srl;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "srlv"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_srlv;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "sra"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sra;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "srav"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_srav;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "nor"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_nor;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "div"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_div;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "udiv"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_udiv;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end   
        "quo"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_quo;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "uquo"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_uquo;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "res"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_res;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ures"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_ures;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "clo"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_clo;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end  
        "clz"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_clz;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ext"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_ext;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ins"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_ins;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "sll"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sll;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "sllv"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sllv;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "rot"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_rot;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "rotv"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_rotv;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "seb"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_seb;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "she"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_she;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "wsbh"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_wsbh;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "mv2s"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_mv2s;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "max"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_max;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "umax"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_umax;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "min"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_min;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "umin"  :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_umin;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "fcr"   :
          begin
            ps = 0;
            case({pb, az})
                2'b00 : inst[i].i.op = iop_fcr;
                2'b01 : inst[i].i.op = iop_fcrn;
                2'b10 : inst[i].i.op = iop_fcrp;
                2'b11 : inst[i].i.op = iop_fcrpn;
              endcase
            inst[i].i.b.fcr.mu = mu;
            inst[i].i.b.fcr.su = su;
            inst[i].i.b.fcr.l  = fcrl;
            `asm_msg($psprintf("fcr pc: %0d", fcrPc), OVM_HIGH); 
            if(fcrPc) begin              
              inst[i].i.b.fcr.ja = 14;
              `asm_msg($psprintf("fcr ja: %0d", inst[i].i.b.fcr.ja), OVM_HIGH);
            end
            else one = 1;
            if(immOp[i][1] && !tagOp[1]) 
              {inst[i].i.b.fcr.os2, inst[i].i.b.fcr.os1, inst[i].i.b.fcr.os0} = imm[i][1];
            else if(immOp[i][1] && tagOp[1]) begin
              `asm_err("fcr instruction format is illegal!");
              return 0;
            end
          end
        "b"     :
          begin
            `asm_msg($psprintf("branch tagop: %0d", tagOp[0]), OVM_HIGH);
            if(tagOp[0]) begin
              case({pb, az})
                2'b00 : inst[i].i.op = iop_b;
                2'b01 : inst[i].i.op = iop_bn;
                2'b10 : inst[i].i.op = iop_bp;
                2'b11 : inst[i].i.op = iop_bpn;
              endcase
              `asm_msg($psprintf("branch op: %0d.", inst[i].i.op), OVM_HIGH);
              inst[i].i.b.b.mop = mop;
              inst[i].i.b.b.sop = sop;
              if(immOp[i][1])
                inst[i].i.b.b.sc  = imm[i][1];
              else
                inst[i].i.b.b.sc  = 0;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ld"    :
          begin
            if(immOp[i][2]) begin
              if(ldH) inst[i].i.op = iop_lh;
              else if(ldB) inst[i].i.op = iop_lb;
              else if(ldLk) inst[i].i.op = iop_ll;
              else if(ldHu) inst[i].i.op = iop_lhu;
              else if(ldBu) inst[i].i.op = iop_lbu;
              else inst[i].i.op = iop_lw;
              {inst[i].i.b.ld.os1, inst[i].i.b.ld.os0} = imm[i][2];
              inst[i].i.b.ld.ua = ldua;
              inst[i].i.b.ld.t = ldty;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end 
          end
        "st"    :
          begin
            ps = 0;
            if(immOp[i][2]) begin
              if(stH) inst[i].i.op = iop_sh;
              else if(stB) inst[i].i.op = iop_sb;
              else if(stCn) inst[i].i.op = iop_sc;
              else inst[i].i.op = iop_sw;
              {inst[i].i.b.st.os2, inst[i].i.b.st.os1, inst[i].i.b.st.os0} = imm[i][2];
              inst[i].i.b.st.ua = stua;
              inst[i].i.b.st.t = stty;
              two = 1;
            end
          else begin
            `asm_err("op number does not match with the op_code!");
            return 0;
          end
         end
        "cmpxchg":
          begin
            ps = 0;
            if(immOp[i][2]) begin
              inst[i].i.op = iop_cmpxchg;
              {inst[i].i.b.cmpxchg.os2, inst[i].i.b.cmpxchg.os1, inst[i].i.b.cmpxchg.os0} = imm[i][2];
              inst[i].i.b.cmpxchg.ua = cmpxua;
              inst[i].i.b.cmpxchg.t = cmpxty;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "fetadd":
          begin
            if(immOp[i][3]) begin
              inst[i].i.op = iop_fetadd;
              {inst[i].i.b.ld.os1, inst[i].i.b.ld.os0} = imm[i][3];
              inst[i].i.b.ld.ua = fetaua;
              inst[i].i.b.ld.t  = fetaty;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "cache" :
          begin
            ps = 0;
            if(immOp[i][1]) begin
              inst[i].i.op = iop_mctl;
              inst[i].i.b.mctl.fun = {opcah, devcah};
              inst[i].i.b.mctl.c = 1;
              {inst[i].i.b.mctl.os1, inst[i].i.b.mctl.os0} = imm[i][1];
              inst[i].i.b.mctl.t = mcty;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "pref" :
          begin
            ps = 0;
            if(immOp[i][1]) begin
              inst[i].i.op = iop_mctl;
              inst[i].i.b.mctl.fun = mcfun;
              inst[i].i.b.mctl.c = 0;
              {inst[i].i.b.mctl.os1, inst[i].i.b.mctl.os0} = imm[i][1];
              inst[i].i.b.mctl.t = mcty;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "sync" :
          begin
            ps = 0;
            if(immOp[i][1]) begin
              inst[i].i.op = iop_mctl;
              inst[i].i.b.mctl.fun = mcfun;
              inst[i].i.b.mctl.c = 0;
              {inst[i].i.b.mctl.os1, inst[i].i.b.mctl.os0} = imm[i][1];
              inst[i].i.b.mctl.t = mcty;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "synci" :
          begin
            ps = 0;
            if(immOp[i][1]) begin
              inst[i].i.op = iop_mctl;
              inst[i].i.b.mctl.fun = 13;
              inst[i].i.b.mctl.c = 0;
              {inst[i].i.b.mctl.os1, inst[i].i.b.mctl.os0} = imm[i][1];
              inst[i].i.b.mctl.t = mcty;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end        
        "mrfrd": begin
          if(immOp[i][1]) begin
            inst[i].i.op = iop_mrfa;
            inst[i].i.b.mrfa.st = mrst;
            inst[i].i.b.mrfa.mrfa = imm[i][1];
            inst[i].i.b.mrfa.ft = 1;
          end  
          else begin
            `asm_err("op number does not match with the op_code!");
            return 0;
          end
        end
        "mrfwr": begin
          if(immOp[i][3]) begin
            inst[i].i.op = iop_mrfa;
            inst[i].i.b.mrfa.s = imm[i][3];
            inst[i].i.b.mrfa.st = mrst;
            inst[i].i.b.mrfa.mrfa = imm[i][2];
            inst[i].i.b.mrfa.ft = 0;
            one = 1;
          end  
          else begin
            `asm_err("op number does not match with the op_code!");
            return 0;
          end
        end
        "smsg" :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_cmsg;
              inst[i].i.b.cmsg.sr = 1;
              inst[i].i.b.cmsg.mrt = imm[i][0];
              inst[i].i.b.cmsg.ss = imm[i][1];
              inst[i].i.b.cmsg.vs = imm[i][2];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "gmsg" :
          begin
            if(immOp[i][0]) begin
              inst[i].i.op = iop_cmsg;
              inst[i].i.b.cmsg.sr = 0;
              inst[i].i.b.cmsg.fifos = imm[i][0];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end  
          end
        "cmp"  :
          begin
            isVec[i] = 1;
            ps = 2;
            if(enOp[i][3] && pdrOp[i][1]) begin
              inst[i].i.op = iop_cmp;
              inst[i].i.b.cmp.ctyp = ctyp;
              inst[i].i.b.cmp.mtyp = mtyp;
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "cmpu" :
          begin
            ps = 2;
            isVec[i] = 1;
             if(enOp[i][3] && pdrOp[i][1]) begin
              inst[i].i.op = iop_cmpu;
              inst[i].i.b.cmp.ctyp = ctyp;
              inst[i].i.b.cmp.mtyp = mtyp;
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "cmpi" :
          begin
            ps = 2;
            isVec[i] = 1;
            if(immOp[i][3] && pdrOp[i][1]) begin
              inst[i].i.op = iop_cmpi;
              inst[i].i.b.cmpi.ctyp = ctyp;
              inst[i].i.b.cmpi.mtyp = mtyp;
              {inst[i].i.b.cmpi.imm1, inst[i].i.b.cmpi.imm0} = imm[i][3];
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "cmpiu" :
          begin
            ps = 2;
            isVec[i] = 1;
            if(immOp[i][3] && pdrOp[i][1]) begin
              inst[i].i.op = iop_cmpiu;
              inst[i].i.b.cmpi.ctyp = ctyp;
              inst[i].i.b.cmpi.mtyp = mtyp;
              {inst[i].i.b.cmpi.imm1, inst[i].i.b.cmpi.imm0} = imm[i][3];
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "alloc" :
          begin
            inst[i].i.b.cop.fun = icop_alloc;
            inst[i].i.op =iop_cop;
            if(!alocd) begin
              inst[i].i.b.cop.code[0] = 1;
              if(immOp[i][1]) 
                inst[i].i.b.cop.code[19:16] = imm[i][1];
              else begin
                `asm_err("op number does not match with the op_code!");
                return 0;
              end
            end
            else 
              inst[i].i.b.cop.code[0] = 0;
            
            if(vecOp[i][0])
              inst[i].i.b.cop.code[1] = 1;
            else
              inst[i].i.b.cop.code[1] = 0;
          end
        "sysc"  : 
          begin
            ps = 0;
            inst[i].i.b.cop.fun = icop_alloc;
            inst[i].i.op = iop_cop;
            if(immOp[i][0]) begin
              ps = 0;
              inst[i].i.b.cop.code = imm[i][0];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ipwait" : 
          begin
            inst[i].i.b.cop.fun = icop_wait;
            inst[i].i.op = iop_cop;
          end
        "ipexit" :
          begin 
            ps = 0;
            inst[i].i.b.cop.fun = icop_exit;
            inst[i].i.op = iop_cop;
            if(immOp[i][0])
              inst[i].i.b.cop.code = imm[i][0];
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "ipbreak": 
          begin
            ps = 0;
            inst[i].i.b.cop.fun = icop_brk;
            inst[i].i.op = iop_cop;
            if(immOp[i][0])
              inst[i].i.b.cop.code = imm[i][0];
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "tsync" : 
          begin
            inst[i].i.b.cop.fun = icop_tsync;
            inst[i].i.op = iop_cop;
          end
        "msync" : 
          begin
            inst[i].i.b.cop.fun = icop_msync;
            inst[i].i.op = iop_cop;
          end
        "tlbp"  : 
          begin
            inst[i].i.b.cop.fun = icop_tlbp;
            inst[i].i.op = iop_cop;
          end
        "tlbr"  : 
          begin
            inst[i].i.b.cop.fun = icop_tlbr;
            inst[i].i.op = iop_cop;
          end  
        "tlbwi" : 
          begin
            inst[i].i.b.cop.fun = icop_tlbwi;
            inst[i].i.op = iop_cop;
          end
        "tlbwr" : 
          begin
            `asm_msg("it is tlbwr inst", OVM_HIGH);
            inst[i].i.b.cop.fun = icop_tlbwr;
            inst[i].i.op = iop_cop;            
          end
        "asr"   : 
          begin
            inst[i].i.b.cop.fun = icop_asr;
            inst[i].i.op = iop_cop;
            inst[i].i.b.cop.code[0] = s2g;
            if(s2g) begin
              enOp[i][0] = 0;
              enOp[i][1] = 0;
              enOp[i][2] = 0;
              if(immOp[i][2]) begin
                inst[i].i.b.cop.code[10:2] = imm[i][2];
                inst[i].i.b.cop.code[20:16] = imm[i][1];
              end
              else begin
                `asm_err("op number does not match with the op_code!");
                return 0;
              end
            end
            else begin
              one = 1;
              if(immOp[i][2]) begin
                inst[i].i.b.cop.code[10:2] = imm[i][2];
                inst[i].i.b.cop.code[25:21] = imm[i][0];
              end
              else begin
                `asm_err("op number does not match with the op_code!");
                return 0;
              end
            end            
          end
        "eret"  : 
          begin
            inst[i].i.b.cop.fun = icop_eret;
            inst[i].i.op = iop_cop;
          end
        "shuffle4" :
          begin
            if(immOp[i][3]) begin
              inst[i].i.op = iop_vxchg;
              inst[i].i.b.vxchg.fun = imm[i][3];
              inst[i].i.b.vxchg.t = 0;
              inst[i].i.b.vxchg.s = 0;
              inst[i].i.b.vxchg.up = vxup;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "permute32" :
          begin
            if(immOp[i][3]) begin
              inst[i].i.op = iop_vxchg;
              inst[i].i.b.vxchg.fun = imm[i][3];
              inst[i].i.b.vxchg.t = 1;
              inst[i].i.b.vxchg.s = emsk;
              inst[i].i.b.vxchg.up = vxup;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "nop": begin
          nop[i] = 1;
          enOp[i] = 0;
        end
        "vid": begin
          `asm_msg("it is vid inst", OVM_HIGH);
          inst[i].i.op = iop_r2w1;      
          inst[i].i.b.ir2w1.fun = iop21_vid; 
        end
      default: begin `asm_err("op not understood!"); return 0; end
      endcase
      
      ///alloc 3rd rs, should be vec
      if(three && dual) begin
        bit failed = 1;
        adru[2] = adr[i][3] >> WID_VRF_BKS;
        bk[2] = adr[i][3] & `GML(WID_VRF_BKS);
        bk[2] = bk[2] & `GMH(1);
        for(int k = 0; k < CYC_VEC; k ++)
          if((!vrfEn[k][bk[2]] || vrfAdr[k][bk[2]] == adru[2])
              && (!vrfEn[k][bk[2] + 1] || vrfAdr[k][bk[2] + 1] == adru[2])) begin
            vrfEn[k][bk[2]] = 1;
            vrfEn[k][bk[2] + 1] = 1;
            vrfAdr[k][bk[2]] = adru[2];
            vrfAdr[k][bk[2] + 1] = 255;
            failed = 0;
            bksel[2] = 16 + k * NUM_VRF_BKS + bk[2];
            break;
          end
        if(failed) begin
          `asm_err("vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[2];
      end
      else if(three) begin
        bit failed = 1;
        `asm_msg($psprintf("printf out adr[3] of r3w1 %0d", adr[i][3]), OVM_HIGH);
        adru[2] = adr[i][3] >> WID_VRF_BKS;
        `asm_msg($psprintf("printf out adru[2] of r3w1 %0d", adru[2]), OVM_HIGH);
        bk[2] = adr[i][3] & `GML(WID_VRF_BKS);
        for(int k = 0; k < CYC_VEC; k++)
          if(!vrfEn[k][bk[2]] || vrfAdr[k][bk[2]] == adru[2]) begin
            vrfEn[k][bk[2]] = 1;
            vrfAdr[k][bk[2]] = adru[2];
            `asm_msg($psprintf("Three printf out vrfEn[%0d][bk[2]] %0d", k,vrfEn[k][bk[2]]), OVM_HIGH);
            `asm_msg($psprintf("Three of 2 printf out vrfAdr %0d, ", adru[2]), OVM_HIGH);
            failed = 0;
            `asm_msg($psprintf("printf out bk 2 of r3w1 %0d", bk[2]), OVM_HIGH);
            bksel[2] = 16 + k * NUM_VRF_BKS + bk[2];
            break;
          end
        if(failed) begin
          `asm_err("Err: vec reg alloc failed!");
          return 0;
        end
        `asm_msg($psprintf("printf out bksel 3 of r3w1 %0d", bksel[2]), OVM_HIGH);
        inst[i].i.b.ir3w1.rs2 = bksel[2];
      end
      
      ///set rs0 rs1
      foreach(bk[j]) begin    
        `asm_msg("assign rs0 and rs1 bank!");   
        if(zeroOp[i][ps + j]) begin
          bksel[j] = 15;
          break;
        end
        if(bp0Op[i][ps + j]) begin
          bksel[j] = 12;
          break;
        end
        if(bp1Op[i][ps + j]) begin
          bksel[j] = 13;
          break;
        end
        if(bp2Op[i][ps + j]) begin
          bksel[j] = 14;
          break;
        end
///        if(tidOp[i][ps + j]) begin
///          bksel[j] = 11;
///          break;
///        end
        if(!enOp[i][ps + j]) begin
          `asm_msg("enop is not enable!");   
          break;
        end
        `asm_msg($psprintf("printf out vecOp %0d, ps: %0d, i: %0d, j: %0d", vecOp[i][ps + j], ps, i, j), OVM_HIGH);
        if(vecOp[i][ps + j]) begin
          if(adr[ps + j] > 31) begin
            `asm_err("vec reg out of bound!");
            return 0;
          end
          `asm_msg("vector reg assignment!");
          `asm_msg($psprintf("printf out original adr %0d, j:%0d", adr[i][ps + j],j), OVM_HIGH);
          adru[j] = adr[i][ps + j] >> WID_VRF_BKS;
          `asm_msg($psprintf("printf out vec adru %0d, j:%0d", adru[j],j), OVM_HIGH);
          bk[j] = adr[i][ps + j] & `GML(WID_VRF_BKS);
          `asm_msg($psprintf("printf out bk[j]: %0d, j:%0d", bk[j],j), OVM_HIGH);
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!vrfEn[k][bk[j]] || vrfAdr[k][bk[j]] == adru[j]) begin
                vrfEn[k][bk[j]] = 1;
                vrfAdr[k][bk[j]] = adru[j];
                `asm_msg($psprintf("printf out bk[%0d], vrfEn[%0d][bk[%0d]]: %0d, ", j, k,j,vrfEn[k][bk[j]]), OVM_HIGH);
                `asm_msg($psprintf("printf out vrfAdr %0d, j:%0d", adru[j],j), OVM_HIGH);
                failed = 0;
                bksel[j] = 16 + k * NUM_VRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_err("vec reg alloc failed!");
              return 0;
            end
          end
        end
        else if(enOp[i][ps + j] && !immOp[i][ps + j]) begin
          if(adr[ps + j] > 15) begin
            `asm_err("scl reg out of bound!");
            return 0;
          end
          `asm_msg("scalar reg alloc!");
          adru[j] = adr[i][ps + j] >> WID_SRF_BKS;
          bk[j] = adr[i][ps + j] & `GML(WID_SRF_BKS);
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!srfEn[k][bk[j]] || srfAdr[k][bk[j]] == adru[j]) begin
                srfEn[k][bk[j]] = 1;
                srfAdr[k][bk[j]] = adru[j];
                failed = 0;
                bksel[j] = k * NUM_SRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_msg("scalar reg alloc failed!");
              return 0;
            end
          end
        end
      end
      
      if(one) begin
        inst[i].i.b.ir3w1.rs0 = bksel[0];
        `asm_msg($psprintf("printf out bksel 0 of r1w1 %0d", bksel[0]), OVM_HIGH);
      end
      else if(two || three) begin
        inst[i].i.b.ir3w1.rs0 = bksel[0];
        inst[i].i.b.ir3w1.rs1 = bksel[1];
        `asm_msg($psprintf("printf out bksel 0 of r2w1 or r3w1 %0d", bksel[0]), OVM_HIGH);
        `asm_msg($psprintf("printf out bksel 1 of r2w1 or r3w1 %0d", bksel[1]), OVM_HIGH);
      end
    
    end
    
    ///collect all address
    for(int i = 0; i < CYC_VEC; i++) begin
      `asm_msg("collect address" , OVM_HIGH);
      for(int j = 0; j < NUM_VRF_BKS; j++) begin
        `asm_msg($psprintf("vrfEn[%0d][%0d] :%0d", i,j,vrfEn[i][j]), OVM_HIGH);
        `asm_msg($psprintf("vector address  :%0d", vrfAdr[i][j]), OVM_HIGH);
        if(vrfEn[i][j] && vrfAdr[i][j] < 32) begin
          `asm_msg($psprintf("adrcnt address:%0d", adrcnt), OVM_HIGH);
          allAdr[adrcnt] = vrfAdr[i][j];
          `asm_msg($psprintf("vector address:%0d", allAdr[adrcnt]), OVM_HIGH);
          adrcnt++;
        end
      end
         
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfEn[i][j]) begin
          `asm_msg($psprintf("count scalar address:%0d", adrcnt), OVM_HIGH);
          allAdr[adrcnt] = srfAdr[i][j];
          `asm_msg($psprintf("scalar address:%0d", allAdr[adrcnt]), OVM_HIGH);
          adrcnt++;
        end
    end
    
    /// calculate the group size
    v_icnt = 0;
    adrBits = adrcnt  * 3;
    if(en == 'b01) begin
      grpsize = (8 + (icnt + 1) * 40 +  adrBits + contNum * 32) / 8; 
      `asm_msg($psprintf("gs0 icnt grpsize %0d", icnt), OVM_HIGH);
      `asm_msg($psprintf("gs0 grpsize %0d", grpsize), OVM_HIGH);
    end
    else begin
      for (int i = 0; i < 5; i++) begin
        if(enOp[i])
          v_icnt += 1 ; 
      end      
      grpsize = (16 + v_icnt * 40 +  adrBits + contNum * 32) / 8 ;
      `asm_msg($psprintf("gs1 v_icnt grpsize %0d", v_icnt), OVM_HIGH);
      `asm_msg($psprintf("gs1 grpsize %0d", grpsize), OVM_HIGH);
    end
    
    `asm_msg("--------------------------------", OVM_HIGH);
    return 1;
  endfunction
  
  function bit wirte_out(int fo, ref asmig tag2ig[string], ovm_verbosity verb);
    bit[7:0] constPkg[NUM_BP_CO][4];
    uchar adrBytes ;
    uchar modBytes;
    bit gadr_flag;
    bit[8:0][7:0] tmp0;
    bit[23:0][2:0] tmp1;
    
    gadr_flag = 0;
    `asm_msg($psprintf("adrcnt :%0d", adrcnt), OVM_HIGH);
    if(adrcnt >= 1) begin
        modBytes = (adrcnt  * 3) % 8;
        `asm_msg($psprintf("modBytes :%0d", modBytes), OVM_HIGH);
        if(modBytes != 0) begin
          if(modBytes == 1) begin
            adrBytes = adrcnt  * 3 / 8;
            gadr_flag = 1;
          end
          else
            adrBytes = (adrcnt  * 3 / 8) + 1;
        end
    end
    else
      adrBytes = 0;

    if(tagOp[0] && tag2ig.exists(tag)) begin
      foreach(inst[i]) begin
        if(inst[i].i.op inside {iop_fcr, iop_fcrn, iop_fcrp, iop_fcrpn}) begin
          {inst[i].i.b.fcr.os2, inst[i].i.b.fcr.os1, inst[i].i.b.fcr.os0} = tag2ig[tag].pc - pc;
          `asm_msg($psprintf("fcr current pc %0d", pc), OVM_HIGH);
          `asm_msg($psprintf("fcr tag pc %0d", tag2ig[tag].pc), OVM_HIGH);
        end
        else if(inst[i].i.op inside {iop_b, iop_bn, iop_bp, iop_bpn}) begin
          `asm_msg($psprintf("b current pc %0d", pc), OVM_HIGH);
          `asm_msg($psprintf("b tag pc %0d", tag2ig[tag].pc), OVM_HIGH);
          inst[i].i.b.b.offSet = tag2ig[tag].pc - pc;
        end
      end
    end
    else if(tagOp[0]) begin
      `asm_err("tagOp and tag2ig does not match!");
      return 0;
    end
        
    if(en == 'b01) begin
      gs0.t = 0;
      gs0.chkGrp = chkGrp;
      gs0.unitEn = isVec[0];
      gs0.adrPkgB = adrBytes;
      gs0.nmsk = grpMsk;
      gs0.a = gadr_flag?allAdr[adrcnt-1][2]:0;
      
      `asm_msg($psprintf("the allAdr[adrcnt-1][0] :%0d, allAdr[%d-1]:%0d,", allAdr[adrcnt-1][0],adrcnt, allAdr[adrcnt-1]), OVM_HIGH);
            
      if(contNum < 5)
        gs1.i.coPkgW = contNum;
      else
        `asm_err("Constant Package num out of bound!");
        
      $fwrite(fo, "%8b\n", gs0);
           
      for(int i = 0; i < 5; i++)
        $fwrite(fo, "%8b\n", inst[0].b[i]);
    end
    else begin
      for(int i = 0; i< 5; i++)
        gs1.i.unitEn[i] = !nop[i];
      
      if(contNum < 5)
        gs1.i.coPkgW = contNum;
      else
        `asm_err("Constant Package num out of bound!");
        
      if(vecOp[3][0])
        gs1.i.dv = 1;
      else 
        gs1.i.dv = 0;
      
      gs1.i.t = 1;
      gs1.i.chkGrp = chkGrp;
      gs1.i.adrPkgB = adrBytes;
      gs0.nmsk = grpMsk;
      gs1.i.a = gadr_flag?allAdr[adrcnt-1][2]:0;
      `asm_msg($psprintf("the first address is %0d", allAdr[0]), OVM_HIGH);
      
      $fwrite(fo, "%16b\n", gs1);

      
      for(int i = 0; i < 5; i++)
        for(int j = 0; j < 5; j++) begin
          if(enOp[i])          
            $fwrite(fo, "%8b\n", inst[i].b[j]);
        end
    end
    
    ///write out adr package
    `asm_msg("write out address package", OVM_HIGH);
    for(int i = 0; i < adrcnt; i++) begin
        tmp1[i] = allAdr[i];
        `asm_msg($psprintf("again tmp1[i]:%0d, i:%0d", tmp1[i],i), OVM_HIGH);
      end
    
    tmp0 = tmp1;
///    foreach(tmp0[i])
///      `asm_msg($psprintf("before, tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_HIGH);
    
///    foreach(tmp0[i]) begin  // high -> low
///      `asm_msg($psprintf("tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_HIGH);
///      if(adrBytes > i) begin        
///        `asm_msg($psprintf("again tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_HIGH);
///        $fwrite(fo, "%8b\n", tmp0[i]);
///      end
///    end

    for(int i = 0; i < adrBytes; i++) begin  /// low -> high
///      `asm_msg($psprintf("tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_HIGH);
///      if(adrBytes > i) begin        
        `asm_msg($psprintf("again tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_HIGH);
        $fwrite(fo, "%8b\n", tmp0[i]);
///      end
    end
    
    ///write out Constant package
///    `asm_msg("write out Constant package", OVM_HIGH);
///    `asm_msg($psprintf("Constant package number is %0d", contNum), OVM_HIGH);
    for(int i = 0; i < NUM_BP_CO; i++) begin
///      `asm_msg($psprintf("Constant package enable is %0d", coEn[i]), OVM_HIGH);
      if(coEn[i]) begin
        for(int j = 0; j < 4; j++) begin
///          `asm_msg($psprintf("Constant package value is %0d", co[i][j]), OVM_HIGH);
          $fwrite(fo, "%8b\n",co[i][j]);
        end
      end    
    end
    $fwrite(fo, "%s", "//--------------------------------\n");
    return 1;
  endfunction
endclass

class ip4_assembler;
  string i, o;
  int fi, fo;
  uint pc;
  ovm_verbosity verb;
  asmig tag2ig[string];
  asmig cur;
  asmig igs[$];
    
  function bit translate();
    string s;
    uchar icnt = 0;
    bit isInst = 0;///, hasTag = 0;
    fi = $fopen(i, "r");
    fo = $fopen(o, "w");
        
    if(fi == 0 || fo == 0) begin
      `asm_err("Open file failed.");
      return 0;
    end
    
    `asm_msg("IP4 assembler translating...", OVM_LOW);
    
    ///first pass, translate lines
    while($fgets(s, fi)) begin
      string tokens[$];
      int state = 0, opcnt = 0;
///      bit isInst = 0;///, hasTag = 0;
      if(cur == null) cur  = new();
      `asm_msg("@@Asm code as follows:", OVM_HIGH);
      `asm_msg(s, OVM_HIGH, write);
      brk_token(s, '{" ", "\t", "\n"}, tokens);
      `asm_msg("@@Tokens:", OVM_MEDIUM);
      foreach(tokens[i])
        `asm_msg({tokens[i], "||"}, OVM_MEDIUM, write);
      `asm_msg("\n", OVM_MEDIUM, write);
      
      for(int tid = 0; tokens.size() != 0; tid++) begin
        string tk = tokens.pop_front();
        string tk0 = tk.substr(0, 0);
        string tk1 = tk.substr(1, 1);
        string tk1n = tk.substr(1, tk.len() - 1);
        string tk2n = tk.substr(2, tk.len() - 1);
        `asm_msg({"@@read token ", tk}, OVM_HIGH);
        if(tk0 == "/") begin
          `asm_msg("it's a comment.", OVM_HIGH);
          if(tid != 0) begin
            `asm_err("comment not at begining");
            return 0;
          end
          $fwrite(fo, "%s", s);
          break;
        end
        else if(tk0 == ";") begin
          `asm_msg("it's a group end.", OVM_HIGH);          
          if(!cur.pack_grp(verb)) begin
            `asm_err("pack instruction grp failed");
            return 0;
          end
          icnt = 0;
          isInst = 0;
          cur.pc = pc;
          `asm_msg($psprintf("first current pc %0d", pc), OVM_HIGH);
          pc += cur.grpsize;
          `asm_msg($psprintf("second current pc %0d", pc), OVM_HIGH);
          igs.push_back(cur);
          cur = null;
          break;
        end
        else begin
          if(icnt >= 5) begin
            `asm_err("more than 5 inst in a group");
            return 0;
          end
          
          if((tk0 == "$") && !state) begin
            `asm_msg($psprintf("it's a tag: %s.", tk1n), OVM_HIGH);
            if(tid != 0 || icnt != 0) begin
              `asm_err("tag not at begining");
              return 0;
            end
            if(tag2ig.exists(tk1n)) begin
              `asm_err("tag exists.");
              return 0;
            end
              tag2ig[tk1n] = cur;
///            hasTag = 1;
          end
          else if((tk0 == "(") && !state) begin
///            if((tid - hasTag) != 0) begin
///              `asm_err("predication not at begining");
///              return 0;
///            end
            cur.padr[icnt] = (tk1.tolower() == "p") ? tk2n.atoi() : tk1n.atoi();
            `asm_msg($psprintf("it's a predication reg :%0d", cur.padr[icnt]), OVM_HIGH);
          end
          else if(tk0 == "r") begin
            cur.fcrPc = 1;
          end
          else if(state == 0) begin
            string opts[$];
            `asm_msg($psprintf("trying to get a op for inst%0d", icnt), OVM_HIGH);
            brk_token(tk, {" ", ".", "\t", "\n"}, opts);
            cur.op[icnt] = opts.pop_front();
            `asm_msg($psprintf("cur.op[%d]: %0d", icnt, cur.op[icnt]), OVM_HIGH);
            if(cur.op[icnt].tolower() != "options") begin
              cur.en[icnt] = 1;
              state ++;
              isInst = 1;
              `asm_msg($psprintf("opcode set to %s", cur.op[icnt]), OVM_HIGH);
            end
            
            while(opts.size() > 0) begin
              string opt = opts.pop_front();
              `asm_msg($psprintf("get option: %s", opt), OVM_HIGH);
              case(opt.tolower())
              "s"   : cur.s[icnt] = 1;
              "u"   : cur.s[icnt] = 0;
              "si"  : cur.si[icnt] = 1;
              "i"   : cur.si[icnt] = 0;
              "g0"  : cur.chkGrp = 1;
              "g1"  : cur.chkGrp = 2;
              "g10" : cur.chkGrp = 3;
              "gn"  : cur.chkGrp = 0;
              "nmsk" : cur.grpMsk = 1;
              "r3w1d" : cur.r3w1d = 1; 
              "mu" : cur.mu = 1;
              "su" : cur.su = 1;
              "fcrl": cur.fcrl = 1;
              "pb" : cur.pb = 1;
              "az" : cur.az = 0;
              "naz" : cur.az = 1;
              "mnop" : cur.mop= 0;
              "bc"  : cur.mop = 1;
              "rest": cur.mop = 2;
              "lpgen": cur.mop = 3;
              "elgen": cur.mop = 4;
              "ctgen": cur.mop = 5;
              "ifgen": cur.mop = 6;
              "pop2n" : cur.sop = 0;
              "pop2nc": cur.sop = 1;
              "store": cur.sop = 2;
              "zromsc": cur.sop = 3;
              "const0": begin
                cur.co[0] = get_imm(opts.pop_front());
                cur.coEn[0] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "const1": begin
                cur.co[1] = get_imm(opts.pop_front());
                cur.coEn[1] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "const2": begin
                cur.co[2] = get_imm(opts.pop_front());
                cur.coEn[2] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "const3": begin
                cur.co[3] = get_imm(opts.pop_front());
                cur.coEn[3] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "lh" : cur.ldH = 1;
              "lb" : cur.ldB = 1;
              "lhu" : cur.ldHu = 1;
              "lbu" : cur.ldBu = 1;
              "ldl" : cur.ldLk = 1;
              "lprua" : cur.ldua = 1;
              "lnua" : cur.ldua = 0;
              "lptua" : cur.ldua = 2;
              "ldbst" : cur.ldty = 0;
              "ldran" : cur.ldty = 1;
              "ldrnu" : cur.ldty = 2;
              "sh" : cur.stH = 1;
              "sb" : cur.stB = 1;
              "sc" : cur.stCn = 1;
              "sprua" : cur.stua = 1;
              "snua" : cur.stua = 0;
              "sptua" : cur.stua = 2;
              "stbst" : cur.stty = 0;
              "stran" : cur.stty = 1;
              "strnu" : cur.stty = 2;
              "cmpxpua" : cur.cmpxua = 1;
              "cmpxnua" : cur.cmpxua = 0;
              "cmpxtua" : cur.cmpxua = 2;
              "cmpxbst" : cur.cmpxty = 0;
              "cmpxran" : cur.cmpxty = 1;
              "cmpxrnu" : cur.cmpxty = 2;
              "fetapua" : cur.fetaua = 1;
              "fetanua" : cur.fetaua = 0;
              "fetatua" : cur.fetaua = 2;
              "fetabst" : cur.fetaty = 0;
              "fetaran" : cur.fetaty = 1;
              "fetarnu" : cur.fetaty = 2;
              "alocd" : cur.alocd = 1;
              "s2g" : cur.s2g = 1;
              "g2s" : cur.s2g = 0; 
              "icah" : cur.devcah = 0;
              "dcah" : cur.devcah = 1;
              "ihit" : cur.opcah  = 0;
              "ihtw" : cur.opcah  = 1;
              "hitw" : cur.opcah  = 2;
              "felk" : cur.opcah  = 3;
              "mctbst" : cur.mcty = 0;
              "mctran" : cur.mcty = 1;
              "mctrnu" : cur.mcty = 2;              
              "pfld" : cur.mcfun  = 0;
              "pfst" : cur.mcfun  = 1;
              "pflds" : cur.mcfun = 2;
              "pfsts" : cur.mcfun = 3;
              "pfldr" : cur.mcfun = 4;
              "pfstr" : cur.mcfun = 5;
              "pfiwb" : cur.mcfun = 6;
              "syldst" : cur.mcfun = 8;
              "syld" : cur.mcfun = 9;
              "syst" : cur.mcfun = 12;
              "syl2s" : cur.mcfun = 11;
              "sys2l" : cur.mcfun = 10;
              "=" : cur.ctyp = 0;
              ">" : cur.ctyp = 1;
              ">=" : cur.ctyp = 2;
              "<" : cur.ctyp = 3;
              "<=" : cur.ctyp = 4;
              "mnop" : cur.mtyp = 0;
              "unc" : cur.mtyp = 1;
              "and" : cur.mtyp = 2;
              "andcm" : cur.mtyp = 3;
              "or" : cur.mtyp = 5;
              "orcm" : cur.mtyp = 6;
              "orandcm" : cur.mtyp = 7;
              "andorcm" : cur.mtyp = 8;
              "penmsk" : cur.emsk = 1;
              "vxup" : cur.vxup = 1;
              "mrsdev" : cur.mrst = 0;
              "mrsfr" : cur.mrst = 2;
              "mrsfl" : cur.mrst = 3;
              default : begin `asm_err("unkonwn options."); return 0; end
              endcase
            end
          end        
          else if(state == 1) begin
///            `asm_msg("it's state == 1.", OVM_HIGH);     
///            cur.icnt = icnt + 1;
            if(opcnt >= 4)
              continue;
            `asm_msg($psprintf("trying to get a reg adr or imm for op%0d", opcnt), OVM_HIGH);
            cur.enOp[icnt][opcnt] = 1;
///            /// sepecial register defined prefix u-
///            cur.srOp[icnt][opcnt] = tk0.tolower() == "u";
///            cur.constOp[icnt][opcnt] = tk0.tolower() == "c";
            cur.pdrOp[icnt][opcnt] = tk0.tolower() == "p";
            cur.tagOp[opcnt] = tk0.tolower() == "$";
            cur.bp0Op[icnt][opcnt] = tk.tolower() == "bp0";
            cur.bp1Op[icnt][opcnt] = tk.tolower() == "bp1";
            cur.bp2Op[icnt][opcnt] = tk.tolower() == "bp2";
///            cur.tidOp[icnt][opcnt] = tk0.tolower() == "tid";
            cur.vecOp[icnt][opcnt] = tk0.tolower() == "v";
            cur.zeroOp[icnt][opcnt] = tk.tolower() == "zero";
            cur.immOp[icnt][opcnt] = tk0.tolower() != "s" && !cur.vecOp[icnt][opcnt] && !cur.zeroOp[icnt][opcnt] && !cur.tagOp[opcnt]
                                     && !cur.bp0Op[icnt][opcnt] && !cur.bp1Op[icnt][opcnt] && !cur.bp2Op[icnt][opcnt]/// && !cur.tidOp[icnt][opcnt]
                                     && !cur.pdrOp[icnt][opcnt];
            if(cur.tagOp[opcnt])
              cur.tag = tk1n;
            else if(cur.immOp[icnt][opcnt])
              cur.imm[icnt][opcnt] = get_imm(tk);
            else if(!cur.zeroOp[icnt][opcnt])/// && !cur.pdrOp[icnt][opcnt])
              cur.adr[icnt][opcnt] = tk1n.atoi();
            `asm_msg($psprintf("tag branch %s", cur.tag), OVM_HIGH);
            `asm_msg($psprintf("tagOp:%0d, vecOp:%0d, zeroOp:%0d, immOp:%0d, adr:%0d, imm:%0d, opcnt:%0d, icnt:%0d, enOp: %0d", cur.tagOp[opcnt], cur.vecOp[icnt][opcnt],
                      cur.zeroOp[icnt][opcnt], cur.immOp[icnt][opcnt], cur.adr[icnt][opcnt],
                      cur.imm[icnt][opcnt], opcnt, icnt, cur.enOp[icnt][opcnt]), OVM_HIGH);
            opcnt++;
          end
        end   
      end
      icnt += isInst;
    end
    
    ///second pass
    for(int i = 0; i < igs.size(); i++)
      if(!igs[i].wirte_out(fo, tag2ig, verb)) begin
        return 0;
      end
      
    $fclose(fi);
    $fclose(fo);
    fi = 0;
    fo = 0;
    `asm_msg("Translate complete!", OVM_LOW);
    return 1;
  endfunction

  function new();
    verb = OVM_LOW;
  endfunction
endclass