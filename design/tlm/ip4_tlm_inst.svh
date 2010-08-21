
parameter uchar NUM_MAX_IGRP_BYTES  = 44;
parameter uchar NUM_IBUF_BYTES      = NUM_MAX_IGRP_BYTES + NUM_IFET_BYTES;

typedef bit[5] irsa_t;
typedef bit[5] irda_t;
typedef bit[4] isrsa_t;
typedef bit[4] isrda_t;
typedef bit[3] ipra_t; 

typedef enum bit[5:0] {
  iop_lu  = 'b00000,        iop_li = 'b000001,      iop_addi = 'b011_0_00,    iop_andi = 'b011_0_01,
  iop_ori = 'b011_0_10,     iop_xori = 'b011_0_11,  iop_addsi = 'b011_1_00,   iop_andsi = 'b011_1_01, 
  iop_orsi = 'b011_1_10,    iop_xorsi = 'b011_1_11, iop_r3w1 = 'b000010,      iop_r2w1 = 'b000011,
  iop_fcr = 'b000100,       iop_fcrn = 'b000101,    iop_fcrp = 'b000110,      iop_fcrpn = 'b000111,
  iop_b = 'b001000,         iop_bn = 'b001001,      iop_bp = 'b001010,        iop_bpn = 'b001011,
  iop_lw = 'b110000,        iop_sw = 'b110001,      iop_lh = 'b110010,        iop_sh = 'b110011,
  iop_lb = 'b110100,        iop_sb = 'b110101,      iop_ll = 'b110110,        iop_sc = 'b110111,
  iop_cmpxchg = 'b111000,   iop_fetadd = 'b111001,  iop_lhu = 'b111010,       iop_lbu = 'b111011,
  iop_mctl = 'b111100,      iop_smsg = 'b001100,    iop_rmsg = 'b001101,
  iop_cmp = 'b001110,       iop_cmpu = 'b001111,    iop_cmpi = 'b010000,      iop_cmpiu = 'b010001,
  iop_cop = 'b010010,       iop_vxchg = 'b010100    ///iop_grag = 'b010011,    
} iop_e;

typedef struct packed{
  irda_t rd;
  bit[19] imm0;
  bit[7] imm1;
}i_i26;

typedef struct packed{
  irsa_t rd;
  irsa_t rs;
  bit[14] imm0;
  bit[7] imm1;
}i_r1w1;

typedef enum bit[4:0] {
  iop31_mul,    iop31_mad,    iop31_msu,
  iop31_add3
} iop_r3w1_e;

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1, rs2;
  bit[4] dummy0;
  iop_r3w1_e fun;
  bit s, d;
}i_r3w1;

typedef enum bit[6:0] {
  iop21_uadd,   iop21_usub,   iop21_srl,    iop21_srlv,
  iop21_or,     iop21_div,    iop21_quo,    iop21_res,
  iop21_clo,    iop21_ext,    iop21_sll,    iop21_rot,
  iop21_and,    iop21_seb,    iop21_wsbh,   iop21_max,
  iop21_min,
  iop21_add = 'b1000000,  
                iop21_sub,    iop21_sra,    iop21_srav,
  iop21_nor,    iop21_udiv,   iop21_uquo,   iop21_ures,
  iop21_clz,    iop21_ins,    iop21_sllv,   iop21_rotv,
  iop21_xor,    iop21_she,    iop21_mv2s,   iop21_umax,
  iop21_umin
} iop_r2w1_e;

/// iop21_vror,   iop21_vsr,    iop21_vsl,
///,   iop21_vroru,  iop21_vsru,   iop21_vslu

iop_r2w1_e iop21_spu_ops[] = {
  iop21_uadd,   iop21_usub,   iop21_srl,    iop21_srlv,
  iop21_or,
  iop21_clo,    iop21_ext,    iop21_sll,    iop21_rot,
  iop21_and,    iop21_seb,    iop21_wsbh,   iop21_max,
  iop21_min,
  iop21_add,    iop21_sub,    iop21_sra,    iop21_srav,
  iop21_nor,
  iop21_clz,    iop21_ins,    iop21_sllv,   iop21_rotv,
  iop21_xor,    iop21_she,    iop21_mv2s,   iop21_umax,
  iop21_umin
};

iop_r2w1_e iop21_sfu_ops[] = {
  iop21_div,    iop21_quo,    iop21_res,
  iop21_udiv,   iop21_uquo,   iop21_ures
};

iop_r2w1_e iop11_ops[] = {
  iop21_srl,    iop21_clo,    iop21_ext,    iop21_sll,
  iop21_rot,    iop21_seb,    iop21_wsbh,   iop21_sra,
  iop21_clz,    iop21_ins,    iop21_she,    iop21_mv2s
};

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1;
  bit[9] imm;
  iop_r2w1_e fun;
}i_r2w1;

typedef struct packed{
  bit[6] os2;
  isrsa_t ja;
  bit[14] os1;
  bit[4] os0;
  bit mu, su, l;
}i_fcr;

typedef struct packed{
  bit[8] sc;
  bit[18] offSet;
  bit[2] sop;
  bit[3] mop;
///  bit[2] bt;
}i_b;

typedef struct packed{
  irda_t rd;
///  bit os2;
  irsa_t rb;
  bit[14] os1;
  bit[4] os0;
  bit[2] ua;
  bit b;
}i_load;

typedef struct packed{
  bit[5] os2;
  irsa_t rb;
  irsa_t rs;
  bit[9] os1;
  bit[4] os0;
  bit[2] ua;
  bit b;
}i_store;

typedef struct packed{
  bit[5] os1;
  isrsa_t rb;
  bit dummy;
  bit[13] os0;
  bit[5] fun;
  bit c;
  bit b;
}i_mctl;

typedef struct packed{
  bit[5] os1;
  irsa_t rb;
  irsa_t rs0, rs1;
  bit[3] os0;
  bit[4] os2;
  bit[2] ua;
  bit b;
}i_cmpxchg;

typedef struct packed{
  irda_t rd;
  bit dummy0;
  isrsa_t rss;
  bit[2] dummy1;
  bit[3] rt;
  irsa_t rvs;
  bit dummy2;
  bit[5] s;
  bit[2] t;
  bit b;
  bit[2] mid;
}i_smsg;

typedef struct packed{
  irda_t rvd;
  irsa_t rsv;
  bit dummy0;
  isrda_t rd;
  bit dummy1;
  bit[8] fifos;
  bit[4] dummy2;
  bit b;
  bit[2] mid;
}i_rmsg;

typedef struct packed{
  bit[5] dummy0;
  irsa_t rs0, rs1;
  bit[3] dummy;
  ipra_t pr0, pr1;
  bit[3] ctyp;
  bit[4] mtyp;
}i_cmp;

typedef struct packed{
  bit[5] imm0;
  irsa_t rs;
  bit[8] imm1;
  ipra_t pr0, pr1;
  bit[3] ctyp;
  bit[4] mtyp;
}i_cmpi;

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1;
  bit[12] fun;
  bit dummy;
  bit s, up, t;
}i_vxchg;

typedef enum bit[4:0] {
  icop_sysc,    icop_eret,      icop_wait,      icop_exit,
  icop_brk,     icop_tsync,     icop_msync,     icop_alloc,
  icop_pint,
  icop_tlbp = 'b10000,
  icop_tlbr,    icop_tlbwi,     icop_tlbwr,     icop_sra
} iop_cop_e;

typedef struct packed{
  bit[26] code;
  iop_cop_e fun;
}i_cop;

typedef union packed{
  i_i26 i26;
  i_r1w1 ir1w1;
  i_r3w1 ir3w1;
  i_r2w1 ir2w1;
  i_fcr fcr;
  i_b b;
  i_load ld;
  i_store st;
  i_mctl mctl;
  i_cmpxchg cmpxchg;
  i_smsg smsg;
  i_rmsg rmsg;
  i_cmp cmp;
  i_cmpi cmpi;
  i_cop cop;
  i_vxchg vxchg;
} i_body;

typedef struct packed{
///  bit g;
  ipra_t p;
  iop_e op;
  i_body b;
}inst_t;

typedef union packed{
  inst_t i;
  bit [4:0][7:0] b;
} inst_u;

typedef bit[3] iga_t;

typedef struct packed{
  bit t, chkGrp;
  bit[5] unitEn;
  bit[3] adrPkgB;
  bit[2] immPkgW;
  bit dv;
  iga_t a;
}i_gs1_t;

typedef union packed{
  i_gs1_t i;
  bit [1:0][7:0] b;
} i_gs1_u;

typedef struct packed{
  bit t, chkGrp, unitEn, adrPkgB, immPkgW;
  iga_t a;
}i_gs0_t;

typedef struct packed{
  iga_t[2] a;
  bit[2] dummy;
}i_ap0_t;

typedef struct packed{
  iga_t[5] a;
  bit dummy;
}i_ap1_t;

typedef union packed{
  i_ap1_t i;
  bit [1:0][7:0] b;
} i_ap1_u;

typedef struct packed{
  iga_t[8] a;
}i_ap2_t;

typedef union packed{
  i_ap2_t i;
  bit [2:0][7:0] b;
} i_ap2_u;

parameter iop_e iop_i26[] = '{
        iop_lu,     iop_li
        };
        
parameter iop_e iop_r1w1i[] = '{
        iop_addi,   iop_andi,   iop_ori,
        iop_xori,   iop_addsi,  iop_andsi,  iop_orsi,   iop_xorsi
        };

parameter iop_e iop_bs[] = '{
        iop_b,      iop_bn,     iop_bp,     iop_bpn 
        };

parameter iop_e iop_fcrs[] = '{
        iop_fcr,    iop_fcrn,   iop_fcrp,   iop_fcrpn
        };
        
parameter iop_e iop_sp_dse[] = '{
        iop_cmpxchg,    iop_fetadd,   iop_mctl
        };

parameter iop_e iop_ls_dse[] = '{
        iop_lw,     iop_sw,    iop_lh,   iop_sh,    iop_lb,
        iop_sb,     iop_ll,    iop_sc,   iop_lhu,   iop_lbu
        };

parameter iop_e iop_msg[] = '{
        iop_smsg,     iop_rmsg
        };                                
        
parameter iop_e iop_cmps[] = '{
        iop_cmp,    iop_cmpu,   iop_cmpi,   iop_cmpiu
        };
        
class inst_c extends ovm_object;
  inst_u inst;
  bit decoded, decodeErr, vecRd, priv, isVec;
  opcode_e op;
  rbk_sel_e rdBkSel[NUM_FU_RP];
  uchar CntVrfRd, CntSrfRd, prRdAdr, prWrAdr[2], fuid,
        grpWr[2], adrWr[2], bkWr[2],
        grpRMsg[2], adrRMsg[2], bkRMsg[2];
  uint imm, offSet;
  bit vrfEn[CYC_VEC][NUM_VRF_BKS], srfEn[CYC_VEC][NUM_SRF_BKS], 
      wrEn[2], prRdEn, prWrEn[2], brDep;
  cmp_opcode_e cmpOp;
  pr_merge_e mergeOp;
  msc_opcode_e mscOp;
  msk_opcode_e mskOp;
  br_opcode_e brOp;
  uchar mB, mUpdateAdr, mFun, mS, mRt, mT, mMid, mFifos;
  bit enSPU, enDSE, enFu[NUM_FU];
  
  `ovm_object_utils_begin(inst_c)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decodeErr, OVM_ALL_ON)
    `ovm_field_int(isVec, OVM_ALL_ON)
    `ovm_field_int(fuid, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(inst, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_sarray_int(enFu, OVM_ALL_ON)
    `ovm_field_int(priv, OVM_ALL_ON)
    `ovm_field_sarray_enum(rbk_sel_e, rdBkSel, OVM_ALL_ON)
    `ovm_field_int(vecRd, OVM_ALL_ON)
    `ovm_field_int(CntVrfRd, OVM_ALL_ON)
    `ovm_field_int(CntSrfRd, OVM_ALL_ON)
    `ovm_field_int(prRdAdr, OVM_ALL_ON)
    `ovm_field_int(prRdEn, OVM_ALL_ON)
    `ovm_field_sarray_int(prWrAdr, OVM_ALL_ON)
    `ovm_field_sarray_int(prWrEn, OVM_ALL_ON)
    `ovm_field_int(brDep, OVM_ALL_ON)
///    `ovm_field_sarray_int(grpWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(adrWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(bkWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(grpRMsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(adrRMsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(bkRMsg, OVM_ALL_ON)
    `ovm_field_int(imm, OVM_ALL_ON)
    `ovm_field_int(offSet, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_enum(cmp_opcode_e, cmpOp, OVM_ALL_ON)
    `ovm_field_enum(pr_merge_e, mergeOp, OVM_ALL_ON)
    `ovm_field_enum(msc_opcode_e, mscOp, OVM_ALL_ON)
    `ovm_field_enum(msk_opcode_e, mskOp, OVM_ALL_ON)
    `ovm_field_enum(br_opcode_e, brOp, OVM_ALL_ON)
///    `ovm_field_int(mB, OVM_ALL_ON)
///    `ovm_field_int(mUpdateAdr, OVM_ALL_ON)
///    `ovm_field_int(mFun, OVM_ALL_ON)
///    `ovm_field_int(mS, OVM_ALL_ON)
///    `ovm_field_int(mRt, OVM_ALL_ON)
///    `ovm_field_int(mT, OVM_ALL_ON)
///    `ovm_field_int(mMid, OVM_ALL_ON)
///    `ovm_field_int(mFifos, OVM_ALL_ON)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
		super.do_print(printer);
		if(enDSE) begin
		  `PF(mB, OVM_BIN)
		  `PF(mUpdateAdr, OVM_BIN)
		  `PF(mFun, OVM_BIN)
		  `PF(mS, OVM_BIN)
		  `PF(mRt, OVM_BIN)
		  `PF(mT, OVM_BIN)
		  `PF(mMid, OVM_BIN)
		  `PF(mFifos, OVM_BIN)
	  end
	endfunction : do_print
		  
	function new (string name = "inst_c");
		super.new(name);
		decoded = 0;
	endfunction : new

  function void set_rf_en(input uchar adr, inout rbk_sel_e sel, bit hasVec, 
                          ref bit vrfEn[CYC_VEC][NUM_VRF_BKS], srfEn[CYC_VEC][NUM_SRF_BKS],
                          inout uchar vrf, srf);
    uchar cyc, bk;
    if(adr < 8) begin
      cyc = adr >> BITS_SRF_BKS;
      bk = adr & ~{'1 << BITS_SRF_BKS};
      srf = (srf > cyc) ? vrf : cyc;
      srfEn[cyc][bk] = 1;
      sel = rbk_sel_e'(sels0 + cyc * NUM_SRF_BKS + bk);
    end
    else if(adr > 15) begin
      adr -= 16;
      cyc = adr >> BITS_VRF_BKS;
      bk = adr & ~{'1 << BITS_VRF_BKS};
      vrf = (vrf > cyc) ? vrf : cyc;
      vrfEn[cyc][bk] = 1;
      hasVec = 1;
      sel = rbk_sel_e'(selv0 + cyc * NUM_VRF_BKS + bk);
    end
    else if(adr == 15)
      sel = selz;
    else if(adr inside {[12:14]}) begin
      sel = rbk_sel_e'(selfu0 + adr - 12);
    end
///    else if(adr == 14)
///      sel = selspu;
///    else if(adr == 13)
///      sel = seldse;
///    else if(adr == 12) begin
///      if(fuid > 1)
///        sel = rbk_sel_e'(selfu0 + fuid - 1);
///      else
///        sel = selfu0;
///    end
  endfunction : set_rf_en
  
	function void decode();
    decoded = 1;
    rdBkSel = '{default : selnull};
    prRdAdr = inst.i.p;
    prRdEn = prRdAdr != 0;
///    rfbp = '{default : fu_null};
    
    if(inst.i.op inside {iop_i26}) begin
      imm = {inst.i.b.i26.imm1, inst.i.b.i26.imm0};
      adrWr[0] = inst.i.b.i26.rd;
      wrEn[0] = 1;
      rdBkSel[1] = selii;
      case(inst.i.op)
      iop_lu    : begin op = op_bp1; imm = imm << (WORD_WIDTH / 2); end
      iop_li    : begin op = op_bp1; end
      endcase
    end
    else if(inst.i.op inside {iop_r1w1i}) begin
      uint imms = {{WORD_WIDTH{inst.i.b.ir1w1.imm1[$bits(inst.i.b.ir1w1.imm1) - 1]}}, inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      imm = {inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      adrWr[0] = inst.i.b.i26.rd;
      wrEn[0] = 1;
      set_rf_en(inst.i.b.ir1w1.rs, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      rdBkSel[1] = selii;
      case(inst.i.op)
      iop_addi  : begin op = op_add; end
      iop_andi  : begin op = op_and; end
      iop_ori   : begin op = op_or; end
      iop_xori  : begin op = op_xor; end
      iop_addsi : begin op = op_add; imm = imms; end
      iop_andsi : begin op = op_and; imm = imms; end
      iop_orsi  : begin op = op_or; imm = imms; end
      iop_xorsi : begin op = op_xor; imm = imms; end
      endcase
    end
    else if(inst.i.op == iop_r3w1) begin
      set_rf_en(inst.i.b.ir3w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      set_rf_en(inst.i.b.ir3w1.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      set_rf_en(inst.i.b.ir3w1.rs2, rdBkSel[2], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);

      if(inst.i.b.ir3w1.d) begin
        wrEn = '{default : 1};
        adrWr[0] = inst.i.b.ir3w1.rd & ('1 << 1);
        adrWr[1] = adrWr[0] + 1;
        rdBkSel[3] = rbk_sel_e'(rdBkSel[2] + 1);
      end
      else begin
        wrEn[0] = 1;
        adrWr[0] = inst.i.b.ir3w1.rd;
      end
      
      case(inst.i.b.ir3w1.fun)
      iop31_mul   : begin op = inst.i.b.ir3w1.s ? op_smul : op_umul; end
      iop31_mad   : begin op = inst.i.b.ir3w1.s ? op_smad : op_umad; end
      iop31_msu   : begin op = inst.i.b.ir3w1.s ? op_smsu : op_umsu; end
      iop31_add3  : begin op = inst.i.b.ir3w1.s ? op_add3 : op_uadd3; end
      endcase
    end
    else if(inst.i.op == iop_r2w1) begin
      set_rf_en(inst.i.b.ir2w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      if(!(inst.i.op inside {iop11_ops}))
        set_rf_en(inst.i.b.ir2w1.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      if(inst.i.b.ir2w1.fun inside {iop21_div, iop21_udiv}) begin
        wrEn = '{default : 1};
        adrWr[0] = inst.i.b.ir2w1.rd & ('1 << 1);
        adrWr[1] = adrWr[0] + 1;
        rdBkSel[3] = rbk_sel_e'(rdBkSel[2] + 1);
      end
      else begin
        wrEn[0] = 1;
        adrWr[0] = inst.i.b.ir2w1.rd;
      end

      case(inst.i.b.ir2w1.fun)
      iop21_div   : begin op = op_div; end
      iop21_quo   : begin op = op_quo; end
      iop21_res   : begin op = op_res; end
      iop21_udiv  : begin op = op_udiv; end
      iop21_uquo  : begin op = op_uquo; end
      iop21_ures  : begin op = op_ures; end
      iop21_uadd  : begin op = op_uadd; end
      iop21_usub  : begin op = op_usub; end
      iop21_srl   : begin op = op_srl; rdBkSel[1] = selii; end
      iop21_srlv  : begin op = op_srl; end
      iop21_or    : begin op = op_or; end
      iop21_clo   : begin op = op_clo; end
      iop21_ext   : begin op = op_ext; rdBkSel[1] = selii; end
      iop21_sll   : begin op = op_sll; rdBkSel[1] = selii; end
      iop21_rot   : begin op = op_ror; rdBkSel[1] = selii; end
      iop21_and   : begin op = op_and; end
      iop21_seb   : begin op = op_seb; rdBkSel[1] = selii; end
      iop21_wsbh  : begin op = op_wsbh; rdBkSel[1] = selii; end
      iop21_max   : begin op = op_max; end
      iop21_min   : begin op = op_min; end
      iop21_add   : begin op = op_add; end
      iop21_sub   : begin op = op_sub; end
      iop21_sra   : begin op = op_sra; rdBkSel[1] = selii; end
      iop21_srav  : begin op = op_sra; end
      iop21_nor   : begin op = op_nor; end
      iop21_clz   : begin op = op_clz; end
      iop21_ins   : begin op = op_ins; end
      iop21_sllv  : begin op = op_sll; end
      iop21_rotv  : begin op = op_ror; end
      iop21_xor   : begin op = op_xor; end
      iop21_she   : begin op = op_she; end
      ///mvs is special, a spu inst require vec reg
      iop21_mv2s  : begin op = op_mvs; rdBkSel[1] = selii; if(CntVrfRd > CntSrfRd) CntSrfRd = CntVrfRd; end
      iop21_umax  : begin op = op_umax; end
      iop21_umin  : begin op = op_umin; end
      endcase
    end
    else if(inst.i.op inside {iop_fcrs}) begin
      offSet = {{WORD_WIDTH{inst.i.b.fcr.os2[$bits(inst.i.b.fcr.os2)-1]}}, inst.i.b.fcr.os2, inst.i.b.fcr.os1, inst.i.b.fcr.os0};
      set_rf_en(inst.i.b.fcr.ja, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      op = op_fcr;
      case(inst.i.op)
      iop_fcr   : begin brDep = 0; brOp = bop_az; end
      iop_fcrn  : begin brDep = 0; brOp = bop_naz; end
      iop_fcrp  : begin brDep = 1; brOp = bop_az; end
      iop_fcrpn : begin brDep = 1; brOp = bop_naz; end
      endcase
      adrWr[0] = 0;
      wrEn[0] = inst.i.b.fcr.l;
      mskOp = inst.i.b.fcr.mu ? (inst.i.b.fcr.l ? mop_if : mop_rstor) : mop_nop;
      mscOp = inst.i.b.fcr.su ? (inst.i.b.fcr.l ? sop_store : sop_pop2n) : sop_nop;
    end
    else if(inst.i.op inside {iop_bs}) begin
      imm = inst.i.b.b.sc;
      offSet = {{WORD_WIDTH{inst.i.b.b.offSet[$bits(inst.i.b.b.offSet)-1]}}, inst.i.b.b.offSet};
      op = op_br;
      case(inst.i.op)
      iop_b   : begin brDep = 0; brOp = bop_az; end
      iop_bn  : begin brDep = 0; brOp = bop_naz; end
      iop_bp  : begin brDep = 1; brOp = bop_az; end
      iop_bpn : begin brDep = 1; brOp = bop_naz; end
      endcase
      case(inst.i.b.b.sop)
      2'b00 : mscOp = sop_nop;
      2'b01 : mscOp = sop_pop2n;
      2'b10 : mscOp = sop_store;
      endcase
      case(inst.i.b.b.sop)
      3'b000 : mskOp = mop_nop;
      3'b001 : mskOp = mop_bc;
      3'b010 : mskOp = mop_rstor;
      3'b011 : mskOp = mop_loop;
      3'b100 : mskOp = mop_else;
      3'b101 : mskOp = mop_cont;
      3'b110 : mskOp = mop_if;
      3'b111 : mskOp = mop_brk;
      endcase
    end
    else if(inst.i.op inside {iop_cmps}) begin
      imm = {inst.i.b.cmpi.imm1, inst.i.b.cmpi.imm0};
      set_rf_en(inst.i.b.cmpi.rs, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      case(inst.i.op)
      iop_cmp   :
      begin
        op = op_cmp;
        set_rf_en(inst.i.b.cmp.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      iop_cmpu  :
      begin
        op = op_ucmp;
        set_rf_en(inst.i.b.cmp.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      iop_cmpi  : begin op = op_cmp; rdBkSel[1] = selii; end
      iop_cmpiu : begin op = op_ucmp; rdBkSel[1] = selii; end
      endcase
      case(inst.i.b.cmp.ctyp)
      3'b000 : cmpOp = cop_e;
      3'b001 : cmpOp = cop_g;
      3'b010 : cmpOp = cop_ge;
      3'b011 : cmpOp = cop_l;
      3'b100 : cmpOp = cop_le;
      endcase
      case(inst.i.b.cmp.mtyp)
      4'b0000 : mergeOp = pm_nop;
      4'b0001 : mergeOp = pm_unc;
      4'b0010 : mergeOp = pm_and;
      4'b0011 : mergeOp = pm_andcm;
      4'b0100 : mergeOp = pm_or;
      4'b0101 : mergeOp = pm_orcm;
      4'b0111 : mergeOp = pm_or_andcm;
      4'b0111 : mergeOp = pm_and_orcm;
      endcase
      prWrAdr[0] = inst.i.b.cmp.pr0;
      prWrAdr[1] = inst.i.b.cmp.pr1;
      prWrEn[0] = prWrAdr[0] == 0;
      prWrEn[1] = prWrAdr[1] == 0;
    end
    else if(inst.i.op inside {iop_sp_dse, iop_ls_dse}) begin
      rdBkSel[2] = selii;
      mB = inst.i.b.ld.b;
      mUpdateAdr = inst.i.b.ld.ua;
      adrWr[0] = inst.i.b.ld.rd;
      if(inst.i.op inside {iop_lw, iop_lh, iop_lb, iop_ll, iop_lhu, iop_lbu}) begin
        imm = {inst.i.b.ld.os1, inst.i.b.ld.os0};
        set_rf_en(inst.i.b.ld.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      case(inst.i.op)
      iop_lw    : op = op_lw;
      iop_lh    : op = op_lh;
      iop_lb    : op = op_lb;
      iop_ll    : op = op_ll;
      iop_lhu   : op = op_lhu;
      iop_lbu   : op = op_lbu;
      endcase
      end
      else if(inst.i.op inside {iop_sw, iop_sh, iop_sb, iop_sc}) begin
        imm = {inst.i.b.st.os2, inst.i.b.st.os1, inst.i.b.st.os0};
        set_rf_en(inst.i.b.st.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.st.rs, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      case(inst.i.op)
      iop_sw    : op = op_sw;
      iop_sh    : op = op_sh;
      iop_sb    : op = op_sb;
      iop_sc    : op = op_sc;
      endcase
      end
      else if(inst.i.op == iop_cmpxchg) begin
        op = op_cmpxchg;
        imm = {inst.i.b.cmpxchg.os2, inst.i.b.cmpxchg.os1, inst.i.b.cmpxchg.os0};
        set_rf_en(inst.i.b.cmpxchg.rs0, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.cmpxchg.rs1, rdBkSel[2], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      else if(inst.i.op == iop_fetadd) begin
        op = op_fetadd;
        imm = {inst.i.b.ld.os1, inst.i.b.ld.os0};
      end
      else if(inst.i.op == iop_mctl) begin
        mFun = inst.i.b.mctl.fun;
        imm = {inst.i.b.mctl.os1, inst.i.b.mctl.os0};
        if(inst.i.b.mctl.c)
          op = op_cache;
        if(mFun < 7)
          op = op_pref;
        else if(mFun < 13)
          op = op_sync;
        else if(mFun == 13)
          op = op_synci;
      end
    end
    else if(inst.i.op == iop_smsg) begin
      op = op_smsg;
      adrWr[0] = inst.i.b.smsg.rd;
      wrEn[0] = 1;
      set_rf_en(inst.i.b.smsg.rss, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      set_rf_en(inst.i.b.smsg.rvs, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      mS = inst.i.b.smsg.s;
      mT = inst.i.b.smsg.t;
      mB = inst.i.b.smsg.b;
      mMid = inst.i.b.smsg.mid;
    end
    else if(inst.i.op == iop_rmsg) begin
      op = op_rmsg;
      mMid = inst.i.b.rmsg.mid;
      mB = inst.i.b.rmsg.b;
      mFifos = inst.i.b.rmsg.fifos;
      adrWr[0] = inst.i.b.rmsg.rvd;
      wrEn[0] = 1;
      adrRMsg[0] = inst.i.b.rmsg.rd & ('1 << 1);
      adrRMsg[1] = adrRMsg[0] + 1;
      prWrAdr[0] = inst.i.p;
      prWrEn[0] = prWrAdr[0] == 0;
    end    
    else if(inst.i.op == iop_cop) begin
      case(inst.i.b.cop.fun)
      icop_sysc  : op = op_sys;
      icop_wait  : op = op_wait;
      icop_exit  : op = op_exit;
      icop_brk   : op = op_brk;
      icop_tsync : op = op_tsync;
      icop_msync : op = op_msync;
      icop_alloc : op = op_alloc;
      icop_pint  : op = op_pint;
      icop_tlbp  :
      begin
        op = op_tlbp;
        priv = 1;
      end
      icop_tlbr  :
      begin
        op = op_tlbr;
        priv = 1;
      end
      icop_tlbwi  :
      begin
        op = op_tlbwi;
        priv = 1;
      end
      icop_tlbwr  :
      begin
        op = op_tlbwr;
        priv = 1;
      end
      icop_sra  :
      begin
        op = inst.i.b.cop.code[0] ? op_s2gp : op_gp2s;
        adrWr[0] = inst.i.b.ir2w1.rd;
        wrEn[0] = inst.i.b.cop.code[0];
        if(!wrEn[0])
          set_rf_en(inst.i.b.ir2w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        imm = (inst.i.b.cop.code >> 2) & 9'b111111111;
      end
      icop_eret :
      begin
        op = op_eret;
        priv = 1;
      end      
      endcase
    end
    
	  if(isVec)
	    foreach(adrWr[i]) begin
	      bkWr[i] = adrWr[i] & ~{'1 << BITS_VRF_BKS};
		    grpWr[i] = adrWr[i] >> BITS_PRF_P_GRP;
		    adrWr[i] = (adrWr[i] >> BITS_VRF_BKS) & ~{'1 << (BITS_PRF_P_GRP - BITS_VRF_BKS)};
		  end
		else
	    foreach(adrWr[i]) begin
	      bkWr[i] = adrWr[i] & ~{'1 << BITS_SRF_BKS};
		    grpWr[i] = adrWr[i] >> BITS_PRF_P_GRP;
		    adrWr[i] = (adrWr[i] >> BITS_SRF_BKS) & ~{'1 << (BITS_PRF_P_GRP - BITS_SRF_BKS)};
		  end

   foreach(adrRMsg[i]) begin
      bkRMsg[i] = adrRMsg[i] & ~{'1 << BITS_SRF_BKS};
      grpRMsg[i] = adrRMsg[i] >> BITS_PRF_P_GRP;
      adrRMsg[i] = (adrRMsg[i] >> BITS_SRF_BKS) & ~{'1 << (BITS_PRF_P_GRP - BITS_SRF_BKS)};
    end
    
	endfunction : decode
	
	function bit is_pd_br();
	  if(!decoded) decode();
    return brDep; /// && inst.i.op inside {iop_bs, iop_fcrs};
	endfunction : is_pd_br

	function bit is_unc_br();
	  if(!decoded) decode();
    return (inst.i.op inside {iop_bs, iop_fcrs}) && (brOp == bop_naz && prRdAdr == 0);
	endfunction : is_unc_br

	function bit is_br();
	  if(!decoded) decode();
    return inst.i.op inside {iop_bs, iop_fcrs};
	endfunction : is_br
		
	function bit is_priv();
	  if(!decoded) decode();
    return priv;
	endfunction : is_priv

	function bit is_ise_inst();
	  if(!decoded) decode();
    return op inside {ise_ops};
	endfunction : is_ise_inst
		
	function void set_wcnt(inout uchar wCnt);
	  uchar t;
	  if(isVec) begin
	    if(op inside {spu_only_ops}) begin
	      t = STAGE_RRF_RRC + STAGE_EEX_VWBP;
	    end
	    else begin
	      t = STAGE_RRF_VWBP;
	    end
	  end
	  else if(op inside {ise_ops})
	    t = 0;
	  else
	    t = STAGE_RRF_SWB;
	  if(wCnt < t)
	    wCnt = t;
	endfunction : set_wcnt
		
	function void setData(const ref uchar data[$], input uchar start, id = 0, bit vec = 0);
    fuid = id;
    isVec = vec;
    decoded = 0;
    priv = 0;
    enDSE = 0;
    enSPU = 0;
    enFu = '{default : 0};
    prWrAdr = '{default : 0};
    vrfEn = '{default : 0};
    srfEn = '{default : 0};
    wrEn = '{default : 0};
    prRdEn = 0;
    prWrEn = '{default : 0};
    brDep = 0;    
    op = op_nop;
    CntSrfRd = 0;
    CntVrfRd = 0;
    
    foreach(inst.b[i])
      inst.b[i] = data[start+i];
      
    decode();
    if(op inside {dse_ops})
      enDSE = 1;
    else if(vec)  
      enFu[fuid] = 1;
    else if(op inside {spu_only_ops, spu_com_ops})
      enSPU = 1;    
	endfunction : setData
	
  function void analyze_rs(input uchar vmode, ref bit v_en[CYC_VEC][NUM_VRF_BKS], s_en[CYC_VEC][NUM_SRF_BKS], inout uchar vrf, srf, dse);
    if(!decoded) decode();
    foreach(vrfEn[i,j])
      v_en[i][j] = v_en[i][j] | vrfEn[i][j];

    foreach(srfEn[i,j])
      s_en[i][j] = s_en[i][j] | srfEn[i][j];
    
    if(CntSrfRd > srf)
      srf = CntSrfRd;
    if(CntVrfRd > vrf)
      vrf = CntVrfRd;
    if(vmode > vrf)
      vrf = vmode;
    if(isVec)
      dse = vmode;
    else
      dse = 1;
  endfunction : analyze_rs

  function void analyze_rd(ref uchar vrf[NUM_VRF_BKS], srf[NUM_SRF_BKS], inout uchar pr);
    if(!decoded) decode();
    foreach(wrEn[i])
      if(wrEn[i]) begin
        if(isVec)
          vrf[bkWr[i]]++;
        else
          srf[bkWr[i]]++;
      end
      
    foreach(prWrEn[i])
      pr += prWrEn[i];
      
    if(op == op_rmsg)
      foreach(bkRMsg[i])
        srf[bkRMsg[i]]++;
  endfunction : analyze_rd
  
  ///reallocate en set by setData
  function void analyze_fu(inout bit spu, dse, ref bit fu[NUM_FU]);
    enDSE = 0;
    enSPU = 0;
    enFu = '{default : 0};
    if(!decoded) decode();
    if(op inside {dse_ops})
      enDSE = 1;
    else if(op inside {spu_ops})
      enSPU = 1;
    else if(isVec) begin
      if(op inside {spu_only_ops}) begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == sfu) begin
            enFu[i] = 1;
            break;
          end
      end
      else begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == alu) begin
            enFu[i] = 1;
            break;
          end
      end
    end
    else if(op inside {spu_com_ops})
      enSPU = 1;
      
    spu = enSPU;
    dse = enDSE;
    fu = enFu;
  endfunction : analyze_fu
  
  function rbk_sel_e cvt_sel(input rbk_sel_e s, uchar i);
    cvt_sel = s;
    if(s inside {[selv0:selv_e], [sels0:sels_e]})
      cvt_sel = selnull;
    if(s inside {[selv0+i*NUM_VRF_BKS:selv0+(i+1)*NUM_VRF_BKS-1]})
      cvt_sel = rbk_sel_e'(s - i * NUM_VRF_BKS);
    if(s inside {[sels0+i*NUM_SRF_BKS:sels0+(i+1)*NUM_SRF_BKS-1]})
      cvt_sel = rbk_sel_e'(s - i * NUM_SRF_BKS);
  endfunction
  
  function void fill_rfm(input tr_ise2rfm rfm, uchar i);
    if(!decoded) decode();
    if(enSPU) begin
      rfm.spuEn = 1;
      rfm.spuImm = imm;
      foreach(rfm.spuRdBk[i])
        rfm.spuRdBk[i] = cvt_sel(rdBkSel[i], i);
    end
    else if(enDSE) begin
      rfm.dseEn = 1;
      rfm.dseImm = imm;
      foreach(rfm.dseRdBk[i])
        rfm.dseRdBk[i] = cvt_sel(rdBkSel[i], i);
    end
    else begin
      rfm.fu[fuid].en = 1;
      rfm.fu[fuid].imm = imm;
      foreach(rfm.fu[0].rdBkSel[i])
        rfm.fu[fuid].rdBkSel[i] = cvt_sel(rdBkSel[i], i);
    end
  endfunction : fill_rfm

  function void fill_spu(input tr_ise2spu spu);
    if(!decoded) decode();
    
    spu.prNMsk = '{default : 0};
    if(enSPU) begin
      spu.sop = mscOp;
      spu.mop = mskOp;
      spu.bop = brOp;
      spu.op = op;
      spu.srfWrAdr = adrWr[0];
      spu.srfWrBk = bkWr[0];
      spu.prRdAdrSPU = prRdAdr;
      spu.prInvSPU = 0;
      spu.prNMskSPU = 0;
      spu.brDep = brDep;
      spu.srfWrDSel = 0;
      spu.srfWrAdr = adrWr[0];
      spu.srfWrBk = bkWr[0];
    end
    else if(enDSE) begin
      spu.prRdAdrDSE = prRdAdr;
      spu.prNMskDSE = 0;
      spu.prInvDSE = 0;
      spu.prWrAdr2 = prWrAdr[0];
      spu.enDSE = 1;
    end
    else begin
      spu.enFu[fuid] = 1;
      spu.prInv[fuid] = 0;
      spu.prNMsk[fuid] = 0;
      spu.prRdAdr[fuid] = prRdAdr;
      if(op inside {op_cmp, op_ucmp}) begin
        spu.prWrAdr0 = prWrAdr[0];
        spu.prWrAdr1 = prWrAdr[1];
      end
    end
  endfunction : fill_spu

  function void fill_dse(input tr_ise2dse dse);
    if(!decoded) decode();
    if(enDSE) begin
      dse.wrAdr = adrWr[0];
      dse.wrBk = bkWr[0];
      dse.updateAdrWrBk = (rdBkSel[0] >= selv0 && rdBkSel[0] <= selv_e) ? rdBkSel[0] - selv0 : 0;
      dse.updateAdrWr = mUpdateAdr != 0 ? 1 : 0;
      dse.op = op;
///      dse.bp_data = (rdBkSel[1] inside {selspu, [selfu0:selfu0+NUM_FU-1]}) ? 1 : 0;
    end
  endfunction : fill_dse

  function void fill_spa(input tr_ise2spa spa);
    if(!decoded) decode();
    if(!isVec) return;
    if(op inside {op_cmp, op_ucmp}) begin
      spa.prMerge = mergeOp;
    end
    
    if(op inside {dse_ops}) begin
      spa.bpRfDSE = rdBkSel[1];
      spa.bpRfDSEWp = 0;
    end
    else begin
      spa.fu[fuid].en = 1;
      spa.fu[fuid].op = op;
      spa.fu[fuid].cop = cmpOp;
      foreach(spa.fu[0].bpSel[i])
        spa.fu[fuid].bpSel[i] = rdBkSel[i];
      spa.fu[fuid].vrfWrBk = bkWr[0];
      spa.fu[fuid].vrfWrAdr = adrWr[0];
    end
  endfunction : fill_spa

  function bit dse_block(input bit noLd, noSt, noSMsg, noRMsg);
    if(!decoded) decode();
    if(enSPU) begin
      if(noLd && op inside {iop_lw, iop_lh, iop_lb, iop_ll, iop_lhu, iop_lbu})
        return 1;
      if(noSt && op inside {iop_sw, iop_sh, iop_sb, iop_sc})
        return 1;
      if(noSMsg && op == op_smsg)
        return 1;
      if(noRMsg && op == op_rmsg)
        return 1;
    end
    return 0;
  endfunction : dse_block

  function void map_wr_grp(const ref uchar
        vrf_map[NUM_INST_VRF/NUM_PRF_P_GRP], 
        srf_map[NUM_INST_SRF/NUM_PRF_P_GRP]);
    if(!decoded) decode();
    if(isVec) begin
      grpWr[0] = vrf_map[grpWr[0]];
      grpWr[1] = grpWr[0];
    end
    else begin
      grpWr[0] = srf_map[grpWr[0]];
      grpWr[1] = grpWr[0];
      grpRMsg[0] = srf_map[grpRMsg[0]];
      grpRMsg[1] = grpRMsg[0];
    end
  endfunction : map_wr_grp
endclass

class inst_fg_c extends ovm_object;
  uchar data[NUM_IFET_BYTES];
  
  `ovm_object_utils_begin(inst_fg_c)
    `ovm_field_sarray_int(data, OVM_ALL_ON + OVM_BIN)
  `ovm_object_utils_end
  
	function new(string name = "inst_fg_c");
		super.new(name);
	endfunction : new

	function void fill(const ref uchar i[NUM_IFET_BYTES]);
	  data = i;
	endfunction : fill
endclass

parameter uchar NUM_INST_BYTES = $bits(inst_u) / 8;