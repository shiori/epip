///assembler for ip4
`define asm_msg(s, i = OVM_LOW, d = display)\
if(verb >= i)\
$d(s);

`define asm_err(s) $display({"Err: ", string'(s)});

function automatic word get_imm(string tk);
  string tk0 = tk.substr(0, 0);
  string tk1 = tk.substr(1, 1);
  string tk1n = tk.substr(1, tk.len() - 1);
  string tk2n = tk.substr(2, tk.len() - 1);
  bit neg = 0;
///  shortreal sr;
  
  if(tk0 == "-") begin
    neg = 1;
    tk0 = tk.substr(1, 1);
    tk1 = tk.substr(2, 2);
    tk1n = tk.substr(2, tk.len() - 1);
    tk2n = tk.substr(3, tk.len() - 1);    
  end
  
  if(tk0 == "o")
    get_imm = tk1n.atooct();
  else if(tk0 == "h")
    get_imm = tk1n.atohex();
  else if(tk0 == "0" && tk1 == "x")
    get_imm = tk2n.atohex();
  else if(tk0 == "b")
    get_imm = tk1n.atobin();
  else if(tk0 == "d")
    get_imm = tk1n.atoi();
  else
    get_imm = tk.atoi();
  
  if(neg)
    get_imm = ~get_imm + 1'b1;
  
///  if($sscanf(tk, "%f", sr))
///    get_imm = $shortrealtobits(sr);
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
///  int ps; /// source operation point
  bit[4:0][3:0] vecOp, immOp, zeroOp, enOp, bpOp, pdrOp, constOp;
  bit[3:0] tagOp;  /// operation
  bit[4:0] nop;
  uchar adrWr[5], adr[5][4], padr[5]; /// 0 of v0 is stored into adr[i][j] , 4 of p4 is stored into padr[i] 
  bit needWrAdr[5];
  int imm[5][4] , cont[5][4];  /// imm[i][0] = rd;
  string tag;
  string op[5];
  bit[4:0] en, s, si, dword;  /// option
  bit mu, su, fcrl, emsk, vxup, alocd;  /// option
  bit fcrPc, pb, az;
  bit ldLk, mhalfu, mbyteu;
  bit mhalf, mbyte, stCn;
  bit[1:0] sop, devcah, opcah, ua, ty, mcty;  /// option
  bit[2:0] mop, ctyp;  /// option 
  bit[3:0] mcfun, mtyp;
  uchar icnt;  
  uchar grpMsk, icc; ///chkGrp, chkGrpUp, 
  uchar grpsize;
  uint pc;
  iga_t allAdr[25];
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
  bit[1:0] mrst;
  uchar adrBytes;
  uchar modBytes;
  uint lid;
  
  function new();
    nop = 0;
    vecOp = 0;
    immOp = 0;
    zeroOp = 0;
    enOp = 0;
    tagOp = 0;
    constOp = 0 ;
    dword = 0;
    fcrPc = 0;
    pb = 0;
    az = 0;
    mu = 0;
    su = 0;
    fcrl = 0;
    emsk = 0;
    vxup = 0;
    mhalfu = 0;
    mbyteu = 0;
    ldLk = 0;
    mhalf = 0;
    mbyte = 0;
    stCn = 0;
    en = 0;
    s = 0;
    si = 0;
    ua = 0;
    ty = 0;
    isVec = '{default : 0};
    icc = 15;
///    chkGrp = 1;
///    chkGrpUp = 0;
    grpMsk = 0;
    adrcnt = 0;
    contNum = 0;
    needWrAdr = '{default : 0};
    for(int i=0; i < NUM_BP_CO; i++)
      coEn[i] = 0;
    mrst = 0;
  endfunction

  function bit pack_grp(ovm_verbosity verb);
    uchar v_icnt;
    ///assemble each inst
    foreach(inst[i]) begin
      uchar adru[3], bk[3];
      uchar bksel[3] = '{default : irs_z};
      bit dual = 0, three = 0, one = 0, two = 0;
      uchar ps = 1;
      
      if(!en[i]) break;
      `asm_msg($psprintf("--assemble inst %0d op %s", i, op[i]), OVM_HIGH);
      isVec[i] = vecOp[i][0];
      inst[i].i.p = padr[i];
      needWrAdr[i] = 1;
      if(isVec[i]) begin
        adrWr[i] = adr[i][0] >> WID_VRF_BKS;
        inst[i].i.b.ir3w1.rd = (adr[i][0] & `GML(WID_VRF_BKS)) + ird_vrfb0;
      end
      else begin
        adrWr[i] = adr[i][0] >> WID_SRF_BKS;
        inst[i].i.b.ir3w1.rd = (adr[i][0] & `GML(WID_SRF_BKS)) + ird_srfb0;
      end
      
      if(zeroOp[i][0]) begin
        inst[i].i.b.ir3w1.rd = isVec[i] ? ird_zv : ird_zs;
        needWrAdr[i] = 0;
      end
      
      case(op[i])
        "li"    :
          begin
            inst[i].i.op = iop_li;
            {inst[i].i.b.i28.imm1, inst[i].i.b.i28.imm0} = imm[i][1]; 
          end
        "lu"    :
          begin
            inst[i].i.op = iop_lu;
            {inst[i].i.b.i28.imm1, inst[i].i.b.i28.imm0} = imm[i][1]; 
          end
        "add"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_addsi : iop_addi;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
              one = 1;
            end
            else if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_add3;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = dword[i];
              three = 1;
              if(dword[i]) dual = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = s[i] ? iop21_add : iop21_uadd;
              two = 1;
            end
          end
        "fadd"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_fadd;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
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
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_mul;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = dword[i];
              two = 1;
///              three = 1;
              if(dword[i]) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "fmul"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_fmul;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = dword[i];
              two = 1;
///              three = 1;
              if(dword[i]) dual = 1;
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
              inst[i].i.b.ir3w1.d = dword[i];
              three = 1;
              if(dword[i]) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "fmad"   :
          begin
            if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop32_fmad;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = dword[i];
              three = 1;
              if(dword[i]) dual = 1;
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
              inst[i].i.b.ir3w1.d = dword[i];
              three = 1;
              if(dword[i]) dual = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "fmsu"   :
          begin
            if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_fmsu;
              inst[i].i.b.ir3w1.s = s[i];
              inst[i].i.b.ir3w1.d = dword[i];
              three = 1;
              if(dword[i]) dual = 1;
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
        "fsub"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_fsub;
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
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_srlv;
              two = 1;
            end
          end
        "sra"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sra;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_srav;
              two = 1;
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
        "fdiv"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_fdiv;
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
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_sllv;
              two = 1;
            end
          end
        "rot"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_rot;
              inst[i].i.b.ir2w1.imm = imm[i][2];
              one = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_rotv;
              two = 1;
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
        "fmax"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_fmax;
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
        "fmin"   :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = iop21_fmin;
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
            needWrAdr[i] = 0; 
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
            needWrAdr[i] = 0;
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
              if(mhalf) inst[i].i.op = iop_lh;
              else if(mbyte) inst[i].i.op = iop_lb;
              else if(ldLk) inst[i].i.op = iop_ll;
              else if(mhalfu) inst[i].i.op = iop_lhu;
              else if(mbyteu) inst[i].i.op = iop_lbu;
              else inst[i].i.op = iop_lw;
              {inst[i].i.b.ld.os1, inst[i].i.b.ld.os0} = imm[i][2];
              inst[i].i.b.ld.ua = ua;
              inst[i].i.b.ld.t = ty;
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
            needWrAdr[i] = 0;
            isVec[i] = vecOp[i][1];
            inst[i].i.b.st.s = !isVec[i];
            if(immOp[i][2]) begin
              if(mhalf) inst[i].i.op = iop_sh;
              else if(mbyte) inst[i].i.op = iop_sb;
              else if(stCn) inst[i].i.op = iop_sc;
              else inst[i].i.op = iop_sw;
              {inst[i].i.b.st.os2, inst[i].i.b.st.os1, inst[i].i.b.st.os0} = imm[i][2];
              inst[i].i.b.st.ua = ua;
              inst[i].i.b.st.t = ty;
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
            isVec[i] = vecOp[i][1];
            inst[i].i.b.st.s = !isVec[i];
            if(immOp[i][2]) begin
              inst[i].i.op = iop_cmpxchg;
              {inst[i].i.b.cmpxchg.os2, inst[i].i.b.cmpxchg.os1, inst[i].i.b.cmpxchg.os0} = imm[i][2];
              inst[i].i.b.cmpxchg.ua = ua;
              inst[i].i.b.cmpxchg.t = ty;
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
              inst[i].i.b.ld.ua = ua;
              inst[i].i.b.ld.t  = ty;
              two = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "cache" :
          begin
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
          needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
            if(immOp[i][3] && pdrOp[i][1]) begin
              ps = 2;
              isVec[i] = 1;
              inst[i].i.op = iop_cmpi;
              inst[i].i.b.cmpi.ctyp = ctyp;
              inst[i].i.b.cmpi.mtyp = mtyp;
              {inst[i].i.b.cmpi.imm1, inst[i].i.b.cmpi.imm0} = imm[i][3];
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              one = 1;
            end
            else if(enOp[i][3] && pdrOp[i][1]) begin
              isVec[i] = 1;
              ps = 2;
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
            needWrAdr[i] = 0;
            if(immOp[i][3] && pdrOp[i][1]) begin
              ps = 2;
              isVec[i] = 1;
              inst[i].i.op = iop_cmpiu;
              inst[i].i.b.cmpi.ctyp = ctyp;
              inst[i].i.b.cmpi.mtyp = mtyp;
              {inst[i].i.b.cmpi.imm1, inst[i].i.b.cmpi.imm0} = imm[i][3];
              inst[i].i.b.cmp.pr0 = adr[i][0];
              inst[i].i.b.cmp.pr1 = adr[i][1];
              one = 1;
            end
            else if(enOp[i][3] && pdrOp[i][1]) begin
              ps = 2;
              isVec[i] = 1;
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
        "cmpf"  :
          begin
            needWrAdr[i] = 0;
            if(enOp[i][3] && pdrOp[i][1]) begin
              isVec[i] = 1;
              ps = 2;
              inst[i].i.op = iop_cmp;
              inst[i].i.b.cmp.f = 1;
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
        "alloc" :
          begin
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_wait;
            inst[i].i.op = iop_cop;
          end
        "ipexit" :
          begin
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
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
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_tsync;
            inst[i].i.op = iop_cop;
          end
        "msync" : 
          begin
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_msync;
            inst[i].i.op = iop_cop;
          end
        "tlbp"  : 
          begin
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_tlbp;
            inst[i].i.op = iop_cop;
          end
        "tlbr"  : 
          begin
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_tlbr;
            inst[i].i.op = iop_cop;
          end  
        "tlbwi" : 
          begin
            needWrAdr[i] = 0;
            inst[i].i.b.cop.fun = icop_tlbwi;
            inst[i].i.op = iop_cop;
          end
        "tlbwr" : 
          begin
            needWrAdr[i] = 0;
            `asm_msg("it is tlbwr inst", OVM_FULL);
            inst[i].i.b.cop.fun = icop_tlbwr;
            inst[i].i.op = iop_cop;            
          end
        "s2g"   : 
          begin
            inst[i].i.b.cop.fun = icop_asr;
            inst[i].i.op = iop_cop;
            enOp[i][0] = 0;
            enOp[i][1] = 0;
            enOp[i][2] = 0;
            inst[i].i.b.cop.code[0] = 1;
            if(immOp[i][2]) begin
              inst[i].i.b.cop.code[9:1] = imm[i][2];
              inst[i].i.b.cop.code[17:10] = imm[i][1];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "g2s"   :
          begin
            inst[i].i.b.cop.fun = icop_asr;
            inst[i].i.op = iop_cop;
            needWrAdr[i] = 0;
            one = 1;
            inst[i].i.b.cop.code[0] = 0;
            if(immOp[i][2]) begin
              inst[i].i.b.cop.code[9:1] = imm[i][2];
              inst[i].i.b.cop.code[17:10] = imm[i][1];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "eret"  : 
          begin
            inst[i].i.b.cop.fun = icop_eret;
            inst[i].i.op = iop_cop;
          end
        "shuffle4" :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = iop_vxchg;
              inst[i].i.b.vxchg.fun = imm[i][2];
              inst[i].i.b.vxchg.t = 0;
              inst[i].i.b.vxchg.s = emsk;
              inst[i].i.b.vxchg.up = vxup;
              one = 1;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "permute32" :
          begin
            if(enOp[i][2]) begin
              inst[i].i.op = iop_vxchg;
              inst[i].i.b.vxchg.fun = 0;
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
          needWrAdr[i] = 0;
        end
        "vid": begin
          `asm_msg("it is vid inst", OVM_FULL);
          inst[i].i.op = iop_r2w1;      
          inst[i].i.b.ir2w1.fun = iop21_vid; 
        end
      default: begin `asm_err("op not understood!"); return 0; end
      endcase
      
      ///alloc 3rd rs
      if(three && dual) begin
        bit failed = 1;
        for(int k = 0; k < CYC_VEC; k ++)
          if(vecOp[i][3]) begin
            adru[2] = adr[i][3] >> WID_VRF_BKS;
            bk[2] = adr[i][3] & `GML(WID_VRF_BKS);
            bk[2] = bk[2] & `GMH(1);
            if((!vrfEn[k][bk[2]] || vrfAdr[k][bk[2]] == adru[2])
                && (!vrfEn[k][bk[2] + 1] || vrfAdr[k][bk[2] + 1] == adru[2])) begin
              vrfEn[k][bk[2]] = 1;
              vrfEn[k][bk[2] + 1] = 1;
              vrfAdr[k][bk[2]] = adru[2];
              vrfAdr[k][bk[2] + 1] = 255;
              failed = 0;
              bksel[2] = irs_vrf0 + k * NUM_VRF_BKS + bk[2];
              break;
            end
          end
          else if(!immOp[i][3]) begin
            adru[2] = adr[i][3] >> WID_SRF_BKS;
            bk[2] = adr[i][3] & `GML(WID_SRF_BKS);
            bk[2] = bk[2] & `GMH(1);
            if((!srfEn[k][bk[2]] || srfAdr[k][bk[2]] == adru[2])
                && (!srfEn[k][bk[2] + 1] || srfAdr[k][bk[2] + 1] == adru[2])) begin
              srfEn[k][bk[2]] = 1;
              srfEn[k][bk[2] + 1] = 1;
              srfAdr[k][bk[2]] = adru[2];
              srfAdr[k][bk[2] + 1] = 255;
              failed = 0;
              bksel[2] = irs_srf0 + k * NUM_SRF_BKS + bk[2];
              break;
            end
          end
        if(failed) begin
          `asm_err("vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[2];
      end
      else if(three) begin
        bit failed = 1;
        `asm_msg($psprintf("adr[3] of r3w1 %0d", adr[i][3]), OVM_FULL);
        if(vecOp[i][3]) begin
          adru[2] = adr[i][3] >> WID_VRF_BKS;
          `asm_msg($psprintf("adru[2] of r3w1 %0d", adru[2]), OVM_FULL);
          for(int k = 0; k < CYC_VEC; k++) begin
            bk[2] = adr[i][3] & `GML(WID_VRF_BKS);
            if(!vrfEn[k][bk[2]] || vrfAdr[k][bk[2]] == adru[2]) begin
              vrfEn[k][bk[2]] = 1;
              vrfAdr[k][bk[2]] = adru[2];
              `asm_msg($psprintf("Three vrfEn[%0d][bk[2]] %0d", k, vrfEn[k][bk[2]]), OVM_FULL);
              `asm_msg($psprintf("Three of 2 vrfAdr %0d, ", adru[2]), OVM_FULL);
              failed = 0;
              `asm_msg($psprintf("bk 2 of r3w1 %0d", bk[2]), OVM_FULL);
              bksel[2] = irs_vrf0 + k * NUM_VRF_BKS + bk[2];
              break;
            end
          end
        end
        else if(!immOp[i][3])  begin
          adru[2] = adr[i][3] >> WID_SRF_BKS;
          `asm_msg($psprintf("adru[2] of r3w1 %0d", adru[2]), OVM_FULL);
          for(int k = 0; k < CYC_VEC; k++) begin
            bk[2] = adr[i][3] & `GML(WID_SRF_BKS);
            if(!srfEn[k][bk[2]] || srfAdr[k][bk[2]] == adru[2]) begin
              srfEn[k][bk[2]] = 1;
              srfAdr[k][bk[2]] = adru[2];
              `asm_msg($psprintf("Three srfEn[%0d][bk[2]] %0d", k, srfEn[k][bk[2]]), OVM_FULL);
              `asm_msg($psprintf("Three of 2 srfAdr %0d, ", adru[2]), OVM_FULL);
              failed = 0;
              `asm_msg($psprintf("bk 2 of r3w1 %0d", bk[2]), OVM_FULL);
              bksel[2] = irs_srf0 + k * NUM_SRF_BKS + bk[2];
              break;
            end
          end
        end
        if(failed) begin
          `asm_err("Err: vec reg alloc failed!");
          return 0;
        end
        `asm_msg($psprintf("bksel 3 of r3w1 %0d", bksel[2]), OVM_FULL);
        inst[i].i.b.ir3w1.rs2 = bksel[2];
      end
      
      ///set rs0 rs1
      foreach(bk[j]) begin    
        `asm_msg("assign rs0 and rs1 bank!", OVM_FULL);   
        if(zeroOp[i][ps + j]) begin
          bksel[j] = irs_z;
          continue;
        end
        if(bpOp[i][ps + j]) begin
          bksel[j] = irs_bp0 + adr[i][ps + j];
          continue;
        end
        if(constOp[i][ps + j]) begin
          bksel[j] = irs_co0 + adr[i][ps + j];
          continue;
        end
        if(!enOp[i][ps + j]) begin
          `asm_msg("enop is not enable!", OVM_FULL);   
          break;
        end
        `asm_msg($psprintf("vecOp %0d, ps: %0d, i: %0d, j: %0d", vecOp[i][ps + j], ps, i, j), OVM_FULL);
        if(vecOp[i][ps + j]) begin
          if(adr[i][ps + j] > (NUM_INST_VRF - 1)) begin
            `asm_err("vec reg out of bound!");
            return 0;
          end
          `asm_msg("vector reg assignment!", OVM_FULL);
          `asm_msg($psprintf("original adr %0d, j:%0d", adr[i][ps + j],j), OVM_FULL);
          adru[j] = adr[i][ps + j] >> WID_VRF_BKS;
          `asm_msg($psprintf("vec adru %0d, j:%0d", adru[j],j), OVM_FULL);
          bk[j] = adr[i][ps + j] & `GML(WID_VRF_BKS);
          `asm_msg($psprintf("bk[j]: %0d, j:%0d", bk[j],j), OVM_FULL);
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!vrfEn[k][bk[j]] || vrfAdr[k][bk[j]] == adru[j]) begin
                vrfEn[k][bk[j]] = 1;
                vrfAdr[k][bk[j]] = adru[j];
                `asm_msg($psprintf("bk[%0d], vrfEn[%0d][bk[%0d]]: %0d, ", j, k,j,vrfEn[k][bk[j]]), OVM_FULL);
                `asm_msg($psprintf("vrfAdr %0d, j:%0d", adru[j],j), OVM_FULL);
                failed = 0;
                bksel[j] = irs_vrf0 + k * NUM_VRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_err("vec reg alloc failed!");
              return 0;
            end
          end
        end
        else if(!immOp[i][ps + j]) begin
          if(adr[i][ps + j] > (NUM_INST_SRF - 1)) begin
            `asm_err($psprintf("scl reg out of bound! %0d", adr[i][ps + j]));
            return 0;
          end
          `asm_msg("scalar reg alloc!", OVM_FULL);
          adru[j] = adr[i][ps + j] >> WID_SRF_BKS;
          bk[j] = adr[i][ps + j] & `GML(WID_SRF_BKS);
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!srfEn[k][bk[j]] || srfAdr[k][bk[j]] == adru[j]) begin
                srfEn[k][bk[j]] = 1;
                srfAdr[k][bk[j]] = adru[j];
                failed = 0;
                bksel[j] = irs_srf0 + k * NUM_SRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_msg("scalar reg alloc failed!", OVM_FULL);
              return 0;
            end
          end
        end
      end
      
      if(one) begin
        inst[i].i.b.ir3w1.rs0 = bksel[0];
        `asm_msg($psprintf("bksel 0 of r1w1 %0d", bksel[0]), OVM_FULL);
      end
      else if(two || three) begin
        inst[i].i.b.ir3w1.rs0 = bksel[0];
        inst[i].i.b.ir3w1.rs1 = bksel[1];
        `asm_msg($psprintf("bksel 0 of r2w1 or r3w1 %0d", bksel[0]), OVM_FULL);
        `asm_msg($psprintf("bksel 1 of r2w1 or r3w1 %0d", bksel[1]), OVM_FULL);
      end
    
    end
    
    ///collect all address
    `asm_msg("--collect address" , OVM_FULL);
    for(int i = 0; i < 5; i++)
      if(en[i] && !nop[i] && needWrAdr[i]) begin
        allAdr[adrcnt] = isVec[i] ? adr[i][0] >> WID_VRF_BKS : adr[i][0] >> WID_SRF_BKS;
         `asm_msg($psprintf("wr address[%0d] :%0d, inst%0d", adrcnt, allAdr[adrcnt], i), OVM_FULL);
        adrcnt++;
      end
    
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++) begin
        if(vrfEn[i][j] && vrfAdr[i][j] < NUM_INST_VRF) begin
          allAdr[adrcnt] = vrfAdr[i][j];
          `asm_msg($psprintf("rd address[%0d] = %0d, vrfEn[%0d][%0d]", adrcnt, allAdr[adrcnt], i, j), OVM_FULL);
          adrcnt++;
        end
      end
         
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfEn[i][j] && srfAdr[i][j] < NUM_INST_SRF) begin
          allAdr[adrcnt] = srfAdr[i][j];
          `asm_msg($psprintf("rd address[%0d] = %0d, srfEn[%0d][%0d]", adrcnt, allAdr[adrcnt], i, j), OVM_FULL);
          adrcnt++;
        end
    end
    
    /// calculate the group size
    v_icnt = 0;
    
    `asm_msg($psprintf("adrcnt :%0d", adrcnt), OVM_FULL);
    if(adrcnt > 0) begin
        modBytes = (adrcnt  * WID_INST_ADR) % 8;
        `asm_msg($psprintf("modBytes :%0d", modBytes), OVM_FULL);
        if(modBytes != 0) begin
          adrBytes = (adrcnt * WID_INST_ADR / 8) + 1;
        end
        else
          adrBytes = (adrcnt * WID_INST_ADR / 8);
    end
    else
      adrBytes = 0;
    
    `asm_msg($psprintf("adrBytes :%0d", adrBytes), OVM_FULL);
    
    if(en == 'b01) begin
      if(adrBytes == 0)
        adrBytes = 1;
        
      grpsize = (8 + (icnt + 1) * 40 +  contNum * 32) / 8 + adrBytes; 
      `asm_msg($psprintf("gs0 icnt %0d, adrBytes %0d, grpsize", icnt, adrBytes, grpsize), OVM_FULL);
    end
    else begin
      for (int i = 0; i < 5; i++) begin
        if(en[i] && !nop[i])
          v_icnt += 1 ; 
      end      
      grpsize = (16 + v_icnt * 40 +  contNum * 32) / 8 + adrBytes;
      `asm_msg($psprintf("gs1 v_icnt %0d, adrBytes %0d, grpsize %0d", v_icnt, adrBytes, grpsize), OVM_FULL);
    end
    
    `asm_msg("--------------------------------", OVM_HIGH);
    return 1;
  endfunction
  
  function bit wirte_out(int fo, ref asmig tag2ig[string], ovm_verbosity verb);
    bit[7:0] constPkg[NUM_BP_CO][4];
    bit[WID_INST_ADR * 3 - 1:0][7:0] tmp0;
    bit[23:0][WID_INST_ADR - 1:0] tmp1;
    
    $fwrite(fo, "%s", $psprintf("//pc: 0x%0h, source line %0d --------------------------------\n", pc, lid));
    `asm_msg($psprintf("//pc: 0x%0h --------------------------------", pc));
    
    if(tagOp[0] && tag2ig.exists(tag)) begin
      foreach(inst[i]) begin
        if(inst[i].i.op inside {iop_fcr, iop_fcrn, iop_fcrp, iop_fcrpn}) begin
          int os = tag2ig[tag].pc - pc;
          {inst[i].i.b.fcr.os2, inst[i].i.b.fcr.os1, inst[i].i.b.fcr.os0} = os;
          `asm_msg($psprintf("fcr current pc 0x%0h", pc), OVM_HIGH);
          `asm_msg($psprintf("fcr tag pc 0x%0h, os: %0d", tag2ig[tag].pc, os), OVM_HIGH);
        end
        else if(inst[i].i.op inside {iop_b, iop_bn, iop_bp, iop_bpn}) begin
          int os = tag2ig[tag].pc - pc;
          `asm_msg($psprintf("b current pc 0x%0h, os: %0d", pc, os), OVM_HIGH);
          `asm_msg($psprintf("b tag pc 0x%0h", tag2ig[tag].pc), OVM_HIGH);
          inst[i].i.b.b.offSet = os;
        end
      end
    end
    else if(tagOp[0]) begin
      `asm_err("tagOp and tag2ig does not match!");
      return 0;
    end
        
    if(en == 'b01) begin
      gs0.t = 0;
      gs0.icc = icc;
///      gs0.chkGrp = chkGrp;
///      gs0.chkGrpUp = chkGrpUp;
///      gs0.unitEn = isVec[0];
      gs0.adrPkgB = adrBytes - 1;
      gs0.nmsk = grpMsk;
///      gs0.a = modBytes == 1 ? allAdr[adrcnt-1][2] : 0;
      
      `asm_msg($psprintf("the allAdr[adrcnt-1][0] :%0d, allAdr[%d-1]:%0d,", allAdr[adrcnt-1][0],adrcnt, allAdr[adrcnt-1]), OVM_FULL);
            
      if(contNum < 5)
        gs0.coPkgW = contNum;
      else
        `asm_err("Constant Package num out of bound!");
        
      $fwrite(fo, "%8b\n", gs0);
           
      for(int i = 0; i < 5; i++)
        $fwrite(fo, "%8b\n", inst[0].b[i]);
    end
    else begin
      for(int i = 0; i< 5; i++)
        gs1.i.unitEn[i] = !nop[i] && en[i];
      
      if(contNum < 5)
        gs1.i.coPkgW = contNum;
      else
        `asm_err("Constant Package num out of bound!");
        
///      if(vecOp[3][0])
///        gs1.i.dv = 1;
///      else 
///        gs1.i.dv = 0;
      
      gs1.i.t = 1;
      gs1.i.icc = icc;
///      gs1.i.chkGrp = chkGrp;
///      gs1.i.chkGrpUp = chkGrpUp;
      gs1.i.adrPkgB = adrBytes;
///      gs1.i.dv = isVec[0];
      gs0.nmsk = grpMsk;
///      gs1.i.a = modBytes == 1 ? allAdr[adrcnt-1][2] : 0;
      `asm_msg($psprintf("the first address is %0d", allAdr[0]), OVM_FULL);
      
      $fwrite(fo, "%8b\n", gs1.b[0]);
      $fwrite(fo, "%8b\n", gs1.b[1]);
      
      for(int i = 0; i < 5; i++)
        for(int j = 0; j < 5; j++) begin
          if(!nop[i] && en[i]) ///enOp[i]
            $fwrite(fo, "%8b\n", inst[i].b[j]);
        end
    end
    
    ///write out adr package
    `asm_msg("write out address package", OVM_FULL);
    for(int i = 0; i < adrcnt; i++) begin
        tmp1[i] = allAdr[i];
        `asm_msg($psprintf("again tmp1[i]:%0d, i:%0d", tmp1[i],i), OVM_FULL);
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
        `asm_msg($psprintf("again tmp0[i]:%0d, i:%0d", tmp0[i],i), OVM_FULL);
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
  uint lid = 1;
  
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
      s = s.tolower();
      `asm_msg($psprintf("current pc 0x%0h", pc), OVM_HIGH);
      `asm_msg("--Asm code as follows:", OVM_HIGH);
      `asm_msg(s, OVM_HIGH, write);
      brk_token(s, '{" ", "\t", "\n"}, tokens);
      `asm_msg("--Tokens:", OVM_MEDIUM);
      foreach(tokens[i])
        `asm_msg({tokens[i], "||"}, OVM_MEDIUM, write);
      `asm_msg("\n", OVM_MEDIUM, write);
      
      for(int tid = 0; tokens.size() != 0; tid++) begin
        string tk = tokens.pop_front();
        string tk0 = tk.substr(0, 0);
        string tk1 = tk.substr(1, 1);
        string tk1n = tk.substr(1, tk.len() - 1);
        string tk2n = tk.substr(2, tk.len() - 1);
        `asm_msg({"--read token ", tk}, OVM_HIGH);
        if(tk0 == "/") begin
          isInst = 0;
          `asm_msg("it's a comment.", OVM_HIGH);
          if(tid != 0) begin
            `asm_err("comment not at begining");
            return 0;
          end
          $fwrite(fo, "%s", s);
          break;
        end
        else if(tk0 == ";") begin
          `asm_msg("-----it's a group end.", OVM_HIGH);          
          if(!cur.pack_grp(verb)) begin
            `asm_err("pack instruction grp failed");
            return 0;
          end
          icnt = 0;
          isInst = 0;
          cur.pc = pc;
          cur.lid = lid;
          pc += cur.grpsize;
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
            cur.padr[icnt] = (tk1 == "p") ? tk2n.atoi() : tk1n.atoi();
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
            if(cur.op[icnt] != "options") begin
              cur.en[icnt] = 1;
              state ++;
              isInst = 1;
              `asm_msg($psprintf("opcode set to %s", cur.op[icnt]), OVM_HIGH);
            end
            
            while(opts.size() > 0) begin
              string opt = opts.pop_front();
              `asm_msg($psprintf("get option: %s", opt), OVM_HIGH);
              case(opt)
              "s"   : cur.s[icnt] = 1;
              "u"   : cur.s[icnt] = 0;
              "si"  : cur.si[icnt] = 1;
              "i"   : cur.si[icnt] = 0;
///              "g0"  : cur.chkGrp = 1;
///              "g1"  : cur.chkGrp = 2;
///              "g10" : cur.chkGrp = 3;
///              "gn"  : cur.chkGrp = 0;
///              "gu0" : cur.chkGrpUp = 0;
///              "gu1" : cur.chkGrpUp = 1;
              "icc":  if(opts.size() > 0) cur.icc = get_imm(opts.pop_front());
              "nmsk" : cur.grpMsk = 1;
              "dword" : cur.dword[i] = 1; 
              "mu" : cur.mu = 1;
              "su" : cur.su = 1;
              "fcrl": cur.fcrl = 1;
              "pb" : cur.pb = 1;
              "az" : cur.az = 0;
              "naz" : cur.az = 1;
              "mnop" : cur.mop= 0;
              "bc"  : cur.mop = 1;
              "restore": cur.mop = 2;
              "loop": cur.mop = 3;
              "else": cur.mop = 4;
              "continue": cur.mop = 5;
              "if": cur.mop = 6;
              "guard": cur.mop = 7;
              "pop2n" : cur.sop = 0;
              "pop2nc": cur.sop = 1;
              "storemsc": cur.sop = 2;
              "zeromsc": cur.sop = 3;
              "c0": begin
                cur.co[0] = get_imm(opts.pop_front());
                cur.coEn[0] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "c1": begin
                cur.co[1] = get_imm(opts.pop_front());
                cur.coEn[1] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "c2": begin
                cur.co[2] = get_imm(opts.pop_front());
                cur.coEn[2] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "c3": begin
                cur.co[3] = get_imm(opts.pop_front());
                cur.coEn[3] = 1;
                cur.contNum = cur.contNum + 1;
              end
              "halfu" : cur.mhalfu = 1;
              "byteu" : cur.mbyteu = 1;
              "half" : cur.mhalf = 1;
              "byte" : cur.mbyte = 1;
              "word" : begin cur.mhalfu = 0; cur.mbyteu = 0; cur.mhalf = 0; cur.mbyte = 0; end
              "cond" : cur.stCn = 1;
              "link" : cur.ldLk = 1;
              "preua" : cur.ua = 1;
              "nua" : cur.ua = 0;
              "postua" : cur.ua = 2;
              "burst" : cur.ty = 0;
              "rand" : cur.ty = 1;
              "randnu" : cur.ty = 2;
              "alocd" : cur.alocd = 1;
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
///            cur.srOp[icnt][opcnt] = tk0 == "u";
            cur.constOp[icnt][opcnt] = tk0 == "c";
            cur.pdrOp[icnt][opcnt] = tk0 == "p";
            if(cur.op[icnt] inside {"b", "fcr"}) begin
              cur.tagOp[opcnt] = tk0 == "$";
              if(cur.tagOp[opcnt])
                cur.tag = tk1n;
            end
            cur.bpOp[icnt][opcnt] = tk0 == "b";
            cur.vecOp[icnt][opcnt] = tk0 == "v" || tk == "vzero";
            cur.zeroOp[icnt][opcnt] = tk == "zero" || tk == "vzero";
            cur.immOp[icnt][opcnt] = tk0 != "s" && !cur.vecOp[icnt][opcnt] && !cur.zeroOp[icnt][opcnt] && !cur.tagOp[opcnt]
                                     && !cur.bpOp[icnt][opcnt] && !cur.pdrOp[icnt][opcnt] && !cur.constOp[icnt][opcnt];
            
            if(cur.immOp[icnt][opcnt])
              cur.imm[icnt][opcnt] = get_imm(tk);
            else if(!cur.zeroOp[icnt][opcnt])
              cur.adr[icnt][opcnt] = tk1n.atoi();
///            `asm_msg($psprintf("tag branch %s", cur.tag), OVM_HIGH);
            `asm_msg($psprintf("tagOp:%0d, vecOp:%0d, zeroOp:%0d, immOp:%0d, adr:%0d, imm:%0d, const %0d, opcnt:%0d, icnt:%0d, enOp: %0d", cur.tagOp[opcnt], cur.vecOp[icnt][opcnt],
                      cur.zeroOp[icnt][opcnt], cur.immOp[icnt][opcnt], cur.adr[icnt][opcnt],
                      cur.imm[icnt][opcnt], cur.constOp[icnt][opcnt], opcnt, icnt, cur.enOp[icnt][opcnt]), OVM_HIGH);
            opcnt++;
          end
        end   
      end
      icnt += isInst;
      lid++;
    end
    
    ///second pass
    `asm_msg("--------------------------------\nstage two:\n", OVM_HIGH);
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