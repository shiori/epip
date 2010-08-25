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
  bit[4:0][3:0] vecOp, immOp, zeroOp, enOp;  /// operation
  bit tagOp;  /// operation
  uchar adr[5][4], padr[5]; /// 0 of v0 is stored into adr[i][j] , 4 of p4 is stored into padr[i] 
  int imm[5][4];  /// imm[i][0] = rd;
  string tag;
  string op[5];
  bit[4:0] en, s, si;  /// option
  bit mu, su, fcrl, ldua, ldty, stua, stty, cmpxua, cmpxty, fetaua, fetaty;  /// option
  bit[2:0] mop;  /// option
  bit[1:0] sop;  /// option
  uchar cop_fun;
  uchar chkGrp;
  uchar grpsize;
  uint pc;
  uchar allAdr[64];
  bit isVec[5]; 
  uchar adrcnt;
  i_gs0_t gs0;
  i_gs1_u gs1;
  inst_u inst[5];
  bit vrfEn[CYC_VEC][NUM_VRF_BKS],
      srfEn[CYC_VEC][NUM_SRF_BKS];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_VRF_BKS];
  uint co[NUM_BP_CO];
          
  function new();
    vecOp = 0;
    immOp = 0;
    zeroOp = 0;
    enOp = 0;
    tagOp = 0;
    en = 0;
    s = 0;
    si = 0;
    isVec = '{default : 0};
    chkGrp = 0;
    adrcnt = 0;
  endfunction

  function bit pack_grp(ovm_verbosity verb);
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
              one = 1;
            end
            else if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_add3;
              inst[i].i.b.ir3w1.s = s[i];
              three = 1;
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
              three = 1;
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
              three = 1;
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
              three = 1;
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
            case(inst[i].i.op[1:0])
                2'b00 : inst[i].i.op = iop_fcr;
                2'b01 : inst[i].i.op = iop_fcrn;
                2'b10 : inst[i].i.op = iop_fcrp;
                2'b11 : inst[i].i.op = iop_fcrpn;
              endcase
            inst[i].i.b.fcr.mu = mu;
            inst[i].i.b.fcr.su = su;
            inst[i].i.b.fcr.l  = fcrl; 
            if(immOp[i][1] && !tagOp) 
              {inst[i].i.b.fcr.os2, inst[i].i.b.fcr.os1, inst[i].i.b.fcr.os0} = imm[i][1];
            else if(immOp[i][1] && tagOp) begin
              `asm_err("fcr instruction format is illegal!");
              return 0;
            end
          end
        "b"     :
          begin
            if(tagOp) begin
              case(inst[i].i.op[1:0])
                2'b00 : inst[i].i.op = iop_b;
                2'b01 : inst[i].i.op = iop_bn;
                2'b10 : inst[i].i.op = iop_bp;
                2'b11 : inst[i].i.op = iop_bpn;
              endcase
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
              case(inst[i].i.op[3:0])
                4'b0000 : inst[i].i.op = iop_lw;
                4'b0010 : inst[i].i.op = iop_lh;
                4'b0100 : inst[i].i.op = iop_lb;
                4'b0110 : inst[i].i.op = iop_ll;
                4'b1010 : inst[i].i.op = iop_lhu;
                4'b1011 : inst[i].i.op = iop_lbu;
                default: begin `asm_err("load op_code not understood!"); return 0; end
              endcase
              {inst[i].i.b.ld.os1, inst[i].i.b.ld.os0} = imm[i][2];
              inst[i].i.b.ld.ua = ldua;
              inst[i].i.b.ld.t = ldty;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end 
          end
        "st"    :
          begin
            if(immOp[i][2]) begin
              case(inst[i].i.op[2:0])
                3'b001 : inst[i].i.op = iop_sw;
                3'b011 : inst[i].i.op = iop_sh;
                3'b101 : inst[i].i.op = iop_sb;
                3'b111 : inst[i].i.op = iop_sc;
                default: begin `asm_err("store op_code not understood!"); return 0; end
              endcase
              {inst[i].i.b.st.os2, inst[i].i.b.st.os1, inst[i].i.b.st.os0} = imm[i][1];
              inst[i].i.b.st.ua = stua;
              inst[i].i.b.st.t = stty;
              one = 1;
            end
          else begin
            `asm_err("op number does not match with the op_code!");
            return 0;
          end
         end
        "cmpxchg":
          begin
            if(immOp[i][3]) begin
              inst[i].i.op = iop_cmpxchg;
              {inst[i].i.b.cmpxchg.os2, inst[i].i.b.cmpxchg.os1, inst[i].i.b.cmpxchg.os0} = imm[i][1];
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
            if(immOp[i][2]) begin
              inst[i].i.op = iop_fetadd;
              {inst[i].i.b.ld.os1, inst[i].i.b.ld.os0} = imm[i][2];
              inst[i].i.b.ld.ua = fetaua;
              inst[i].i.b.ld.t  = fetaty;
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "mctl" :
          begin
            if(immOp[i][1]) begin
///              inst[i].i.op = iop_mctl;
///              inst[i].i.b.mctl.fun = ;
///              inst[i].i.b.mctl.c = ;
///              {inst[i].i.b.mctl.os1, inst[i].i.b.mctl.os0} = imm[i][1];
            end
            else begin
              `asm_err("op number does not match with the op_code!");
              return 0;
            end
          end
        "smsg" :
          begin
            
          end
        "rmsg" :
          begin
            
          end
        "cmp"  :
          begin
            
          end
        "cmpu" :
          begin
            
          end
        "cmpi" :
          begin
            
          end
        "cmpiu" :
          begin
            
          end
        "alloc" :
          begin
            inst[i].i.b.cop.fun = icop_alloc;
            inst[i].i.op =iop_cop;
            one = 1;
          end
        "sysc"  : 
          begin
            inst[i].i.b.cop.fun = icop_alloc;
            inst[i].i.op = iop_cop;
            if(immOp[i][0])
              inst[i].i.b.cop.code = imm[i][0];
          end
        "ipwait" : 
          begin
            inst[i].i.b.cop.fun = icop_wait;
            inst[i].i.op = iop_cop;
          end
        "ipexit" :
          begin 
            inst[i].i.b.cop.fun = icop_exit;
            inst[i].i.op = iop_cop;
            if(immOp[i][0])
              inst[i].i.b.cop.code = imm[i][0];
          end
        "ipbreak": 
          begin
            inst[i].i.b.cop.fun = icop_brk;
            inst[i].i.op = iop_cop;
            if(immOp[i][0]) begin
              inst[i].i.b.cop.code = imm[i][0];
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
            inst[i].i.b.cop.fun = icop_tlbwr;
            inst[i].i.op = iop_cop;
          end
        "asr"   : 
          begin
            inst[i].i.b.cop.fun = icop_sra;
            inst[i].i.op = iop_cop;
            enOp[i][0] = 0;
            enOp[i][1] = 0;
            enOp[i][2] = 0;
            if(immOp[i][2]) begin
              inst[i].i.b.cop.code = imm[i][2];
              bksel[0] = inst[i].i.b.cop.code[20:16];
              one = 1;
            end
          end
        "eret"  : 
          begin
            inst[i].i.b.cop.fun = icop_eret;
            inst[i].i.op = iop_cop;
          end
      default: begin `asm_err("op not understood!"); return 0; end
      endcase
      
      ///alloc 3rd rs, should be vec
      if(three && dual) begin
        bit failed = 1;
        bk[3] = bk[3] & ('1 << 1);
        for(int k = 0; k < CYC_VEC; k ++)
          if((!vrfEn[k][bk[3]] || vrfAdr[k][bk[3]] == adru[3])
              && (!vrfEn[k][bk[3] + 1] || vrfAdr[k][bk[3] + 1] == adru[3])) begin
            vrfEn[k][bk[3]] = 1;
            vrfEn[k][bk[3] + 1] = 1;
            vrfAdr[k][bk[3]] = adru[3];
            vrfAdr[k][bk[3] + 1] = 255;
            failed = 0;
            bksel[3] = 16 + k * NUM_VRF_BKS + bk[3];
            break;
          end
        if(failed) begin
          `asm_err("vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[3];
      end
      else if(three) begin
        bit failed = 1;
        for(int k = 0; k < CYC_VEC; k++)
          if(!vrfEn[k][bk[3]] || vrfAdr[k][bk[3]] == adru[3]) begin
            vrfEn[k][bk[3]] = 1;
            vrfAdr[k][bk[3]] = adru[3];
            failed = 0;
            bksel[3] = 16 + k * NUM_VRF_BKS + bk[3];
            break;
          end
        if(failed) begin
          `asm_err("Err: vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[3];
      end
      
      ///set rs0 rs1
      foreach(bk[j]) begin
        if(!enOp[i][1 + j]) break;
        if(vecOp[i][1 + j]) begin
          if(adr[1 + j] > 31) begin
            `asm_err("vec reg out of bound!");
            return 0;
          end
          adru[j] = adr[i][1 + j] >> BITS_VRF_BKS;
          bk[j] = adr[i][1 + j] & ~{'1 << BITS_VRF_BKS};
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!vrfEn[k][bk[j]] || vrfAdr[k][bk[j]] == adru[j]) begin
                vrfEn[k][bk[j]] = 1;
                vrfAdr[k][bk[j]] = adru[j];
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
        else begin
          if(adr[1 + j] > 15) begin
            `asm_err("scl reg out of bound!");
            return 0;
          end
          adru[j] = adr[i][1 + j] >> BITS_SRF_BKS;
          bk[j] = adr[i][1 + j] & ~{'1 << BITS_SRF_BKS};
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
              `asm_msg("vec reg alloc failed!");
              return 0;
            end
          end
        end
      end
      if(one)
        inst[i].i.b.ir3w1.rs0 = bksel[0];
      else if(two) begin
        inst[i].i.b.ir3w1.rs0 = bksel[0];
        inst[i].i.b.ir3w1.rs1 = bksel[1];
      end
    
    end
    
    ///collect all address
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfEn[i][j] && vrfAdr[i][j] < 32) begin
          allAdr[adrcnt] = vrfAdr[i][j];
          adrcnt++;
        end
         
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfEn[i][j]) begin
          allAdr[adrcnt] = srfAdr[i][j];
          adrcnt++;
        end
    end
    
    `asm_msg("--------------------------------", OVM_HIGH);
    return 1;
  endfunction
  
  function bit wirte_out(int fo, ref asmig tag2ig[string], ovm_verbosity verb);
    if(tagOp && tag2ig.exists(tag)) begin
      foreach(inst[i]) begin
        if(inst[i].i.op inside {iop_fcr, iop_fcrn, iop_fcrp, iop_fcrpn})
          {inst[i].i.b.fcr.os2, inst[i].i.b.fcr.os1, inst[i].i.b.fcr.os0} = pc - tag2ig[tag].pc;
        else if(inst[i].i.op inside {iop_b, iop_bn, iop_bp, iop_bpn})
          inst[i].i.b.b.offSet = pc - tag2ig[tag].pc;
      end
    end
    else if(tagOp) begin
      `asm_err("tagOp and tag2ig does not match!");
      return 0;
    end
        
    if(en == 'b01) begin
      gs0.t = 0;
      gs0.chkGrp = chkGrp;
      gs0.unitEn = isVec[0];
      gs0.a = allAdr[0];
      gs0.adrPkgB = (adrcnt - 1) * 3 / 8;
      $fwrite(fo, "%8b\n", gs0);
      
      if(gs0.adrPkgB > 0) begin
        i_ap0_t AdrPkg;
        foreach(AdrPkg.a[i])
          AdrPkg.a[i] = allAdr[1 + i];
        $fwrite(fo, "%8b\n", AdrPkg);
      end
            
      for(int i = 0; i < 5; i++)
        $fwrite(fo, "%8b\n", inst[0].b[i]);
    end
    else begin
      
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
      bit isInst = 0, hasTag = 0;
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
          
          if(tk0 == "$") begin
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
            hasTag = 1;
          end
          else if(tk0 == "(") begin
            if((tid - hasTag) != 0) begin
              `asm_err("predication not at begining");
              return 0;
            end
            cur.padr[icnt] = (tk1.tolower() == "p") ? tk2n.atoi() : tk1n.atoi();
            `asm_msg($psprintf("it's a predication reg :%0d", cur.padr[icnt]), OVM_HIGH);
          end
          else if(state == 0) begin
            string opts[$];
            `asm_msg($psprintf("trying to get a op for inst%0d", icnt), OVM_HIGH);
            brk_token(tk, {" ", ".", "\t", "\n"}, opts);
            cur.op[icnt] = opts.pop_front();
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
              "g0"  : cur.chkGrp = 0;
              "g1"  : cur.chkGrp = 1;
              "mu0" : cur.mu = 0;
              "mu1" : cur.mu = 1;
              "su0" : cur.su = 0;
              "su1" : cur.su = 1;
              "fcrl1": cur.fcrl = 1;
              "fcrl0": cur.fcrl = 0;
              "mnop" : cur.mop= 0;
              "bc"  : cur.mop = 1;
              "rest": cur.mop = 2;
              "lpgen": cur.mop = 3;
              "elgen": cur.mop = 4;
              "ctgen": cur.mop = 5;
              "ifgen": cur.mop = 6;
              "snop" : cur.sop = 0;
              "pop2n": cur.sop = 1;
              "store": cur.sop = 2;
              "const0": cur.co[0] = get_imm(opts.pop_front());
              "const1": cur.co[1] = get_imm(opts.pop_front());
              "const2": cur.co[2] = get_imm(opts.pop_front());
              "ldua1" : cur.ldua = 1;
              "ldua0" : cur.ldua = 0;
              "ldbst" : cur.ldty = 0;
              "ldran" : cur.ldty = 1;
              "ldrnu" : cur.ldty = 2;
              "stua1" : cur.stua = 1;
              "stua0" : cur.stua = 0;
              "stbst" : cur.stty = 0;
              "stran" : cur.stty = 1;
              "strnu" : cur.stty = 2;
              "cmpxua1" : cur.cmpxua = 1;
              "cmpxua0" : cur.cmpxua = 0;
              "cmpxbst" : cur.cmpxty = 0;
              "cmpxran" : cur.cmpxty = 1;
              "cmpxrnu" : cur.cmpxty = 2;
              "fetaua1" : cur.fetaua = 1;
              "fetaua0" : cur.fetaua = 0;
              "fetabst" : cur.fetaty = 0;
              "fetaran" : cur.fetaty = 1;
              "fetarnu" : cur.fetaty = 2;
              default : begin `asm_err("unkonwn options."); return 0; end
              endcase
            end
          end        
          else if(state == 1) begin
            if(opcnt >= 4)
              continue;
            `asm_msg($psprintf("trying to get a reg adr or imm for op%0d", opcnt), OVM_HIGH);
            cur.enOp[icnt][opcnt] = 1;
            cur.tagOp = tk0.tolower() == "$";
            cur.vecOp[icnt][opcnt] = tk0.tolower() == "v";
            cur.zeroOp[icnt][opcnt] = tk.tolower() == "zero";
            cur.immOp[icnt][opcnt] = tk0.tolower() != "s" && !cur.vecOp[icnt][opcnt] && !cur.zeroOp[icnt][opcnt] && !cur.tagOp;
            if(cur.tagOp)
              cur.tag = tk1n;
            else if(cur.immOp[icnt][opcnt])
              cur.imm[icnt][opcnt] = get_imm(tk);
            else if(!cur.zeroOp[icnt][opcnt])
              cur.adr[icnt][opcnt] = tk1n.atoi();
            `asm_msg($psprintf("vecOp:%0d, zeroOp:%0d, immOp:%0d, adr:%0d, imm:%0d", cur.vecOp[icnt][opcnt],
                      cur.zeroOp[icnt][opcnt], cur.immOp[icnt][opcnt], cur.adr[icnt][opcnt],
                      cur.imm[icnt][opcnt]), OVM_HIGH);
            opcnt++;
          end
        end   
      end
      icnt += isInst;
    end
    
    ///second pass
    for(int i = 0; i < igs.size(); i++)
      if(!igs[i].wirte_out(fo, tag2ig, verb))
        return 0;
      
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