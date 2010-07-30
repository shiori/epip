
parameter uchar num_max_igrp_bytes  = 44;
parameter uchar num_ibuf_bytes      = num_max_igrp_bytes + num_ifet_bytes;

typedef bit[5] irsa_t;
typedef bit[5] irda_t;
typedef bit[4] isrsa_t;
typedef bit[4] isrda_t;
typedef bit[3] ipra_t; 

typedef enum bit[5:0] {
  iop_lu  = 'b00000,        iop_li = 'b000001,      iop_addi = 'b011_0_00,    iop_andi = 'b011_0_01,
  iop_ori = 'b011_0_10,     iop_xori = 'b011_0_11,  iop_addsi = 'b011_1_00,   iop_andsi = 'b011_1_01, 
  iop_orsi = 'b011_1_10,    iop_xorsi = 'b011_1_11, iop_r3w1 = 'b000010,      iop_r2w1 = 'b000011,
  iop_fcr = 'b000100,       iop_fcrn = 'b000101,    iop_fcrb = 'b000110,      iop_fcrbn = 'b000111,
  iop_b = 'b001000,         iop_bn = 'b001001,      iop_bb = 'b001010,        iop_bbn = 'b001011,
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
  bit[18] os;
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
  bit t, nc;
  bit[5] fua;
  bit[3] apb;
  bit[2] ipw;
  bit dv;
  iga_t a;
}i_gs0_t;

typedef union packed{
  i_gs0_t i;
  bit [1:0][7:0] b;
} i_gs0_u;

typedef struct packed{
  bit t, nc, fua, apb, ipw;
  iga_t a;
}i_gs1_t;

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
        iop_b,      iop_bn,     iop_bb,     iop_bbn 
        };

parameter iop_e iop_fcrs[] = '{
        iop_fcr,    iop_fcrn,   iop_fcrb,   iop_fcrbn
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
  bit decoded, vec_rd, priv, is_vec;
  opcode_e op;
  rbk_sel_e rd_bk[num_fu_rp];
  uchar cnt_vrf_wr, cnt_srf_wr, pr_adr_rd, pr_adr_wr[2], fuid,
        grp_wr[2], adr_wr[2], bk_wr[2],
        grp_rmsg[2], adr_rmsg[2], bk_rmsg[2];
  uint imm;
  bit vrf_en[cyc_vec][num_vrf_bks], srf_en[cyc_vec][num_srf_bks], 
      wr_en[2], pr_rd_en, pr_wr_en[2], pr_br_dep;
  cmp_opcode_e cmp_op;
  pr_merge_e merge_op;
  msc_opcode_e msc_op;
  msk_opcode_e msk_op;
  br_opcode_e br_op;
  uchar m_b, m_ua, m_fun, m_s, m_rt, m_t, m_mid, m_fifos;
  bit en_spu, en_dse, en_fu[num_fu];
  
  `ovm_object_utils_begin(inst_c)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(is_vec, OVM_ALL_ON)
    `ovm_field_int(fuid, OVM_ALL_ON)
    `ovm_field_int(inst, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(en_spu, OVM_ALL_ON)
    `ovm_field_int(en_dse, OVM_ALL_ON)
    `ovm_field_sarray_int(en_fu, OVM_ALL_ON)
    `ovm_field_int(priv, OVM_ALL_ON)
    `ovm_field_sarray_enum(rbk_sel_e, rd_bk, OVM_ALL_ON)
    `ovm_field_int(vec_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_vrf_wr, OVM_ALL_ON)
    `ovm_field_int(cnt_srf_wr, OVM_ALL_ON)
    `ovm_field_int(pr_adr_rd, OVM_ALL_ON)
    `ovm_field_int(pr_rd_en, OVM_ALL_ON)
    `ovm_field_sarray_int(pr_adr_wr, OVM_ALL_ON)
    `ovm_field_sarray_int(pr_wr_en, OVM_ALL_ON)
    `ovm_field_int(pr_br_dep, OVM_ALL_ON)
///    `ovm_field_sarray_int(grp_wr, OVM_ALL_ON)
///    `ovm_field_sarray_int(adr_wr, OVM_ALL_ON)
///    `ovm_field_sarray_int(bk_wr, OVM_ALL_ON)
///    `ovm_field_sarray_int(grp_rmsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(adr_rmsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(bk_rmsg, OVM_ALL_ON)
    `ovm_field_int(imm, OVM_ALL_ON)
    `ovm_field_sarray_int(wr_en, OVM_ALL_ON)
    `ovm_field_enum(cmp_opcode_e, cmp_op, OVM_ALL_ON)
    `ovm_field_enum(pr_merge_e, merge_op, OVM_ALL_ON)
    `ovm_field_enum(msc_opcode_e, msc_op, OVM_ALL_ON)
    `ovm_field_enum(msk_opcode_e, msk_op, OVM_ALL_ON)
    `ovm_field_enum(br_opcode_e, br_op, OVM_ALL_ON)
///    `ovm_field_int(m_b, OVM_ALL_ON)
///    `ovm_field_int(m_ua, OVM_ALL_ON)
///    `ovm_field_int(m_fun, OVM_ALL_ON)
///    `ovm_field_int(m_s, OVM_ALL_ON)
///    `ovm_field_int(m_rt, OVM_ALL_ON)
///    `ovm_field_int(m_t, OVM_ALL_ON)
///    `ovm_field_int(m_mid, OVM_ALL_ON)
///    `ovm_field_int(m_fifos, OVM_ALL_ON)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
		super.do_print(printer);
		if(en_dse) begin
		  `PF(m_b, OVM_BIN)
		  `PF(m_ua, OVM_BIN)
		  `PF(m_fun, OVM_BIN)
		  `PF(m_s, OVM_BIN)
		  `PF(m_rt, OVM_BIN)
		  `PF(m_t, OVM_BIN)
		  `PF(m_mid, OVM_BIN)
		  `PF(m_fifos, OVM_BIN)
	  end
	endfunction : do_print
		  
	function new (string name = "inst_c");
		super.new(name);
		decoded = 0;
	endfunction : new

  function void set_rf_en(input uchar adr, inout rbk_sel_e sel, bit has_vec, 
                          ref bit vrf_en[cyc_vec][num_vrf_bks], srf_en[cyc_vec][num_srf_bks],
                          inout uchar vrf, srf);
    uchar cyc, bk;
    if(adr < 8) begin
      cyc = adr >> bits_srf_bks;
      bk = adr & ~{'1 << bits_srf_bks};
      srf = (srf > cyc) ? vrf : cyc;
     srf_en[cyc][bk] = 1;
     sel = rbk_sel_e'(sels0 + cyc * num_srf_bks + bk);
    end
    else if(adr > 15) begin
      cyc = adr >> (bits_srf_bks + 3);
      bk = (adr >> 3) & ~{'1 << bits_srf_bks};
      vrf = (vrf > cyc) ? vrf : cyc;
      vrf_en[cyc][bk] = 1;
      has_vec = 1;
      sel = rbk_sel_e'(selv0 + cyc * num_vrf_bks + bk);
    end
    else if(adr == 15)
      sel = selz;
    else if(adr == 14)
      sel = selspu;
    else if(adr == 13)
      sel = seldse;
    else if(adr == 12) begin
      if(fuid > 1)
        sel = rbk_sel_e'(selfu0 + fuid - 1);
      else
        sel = selfu0;
    end
  endfunction : set_rf_en
  
	function void decode();
    decoded = 1;
    rd_bk = '{default : selnull};
    pr_adr_rd = inst.i.p;
    pr_rd_en = pr_adr_rd != 0;
///    rfbp = '{default : fu_null};
    
    if(inst.i.op inside {iop_i26}) begin
      imm = {inst.i.b.i26.imm1, inst.i.b.i26.imm0};
      adr_wr[0] = inst.i.b.i26.rd;
      wr_en[0] = 1;
      rd_bk[1] = selii;
      case(inst.i.op)
      iop_lu    : begin op = op_bp1; imm = imm << (word_width / 2); end
      iop_li    : begin op = op_bp1; end
      endcase
    end
    else if(inst.i.op inside {iop_r1w1i}) begin
      uint imms = {{word_width{inst.i.b.ir1w1.imm1[$bits(inst.i.b.ir1w1.imm1) - 1]}}, inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      imm = {inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      adr_wr[0] = inst.i.b.i26.rd;
      wr_en[0] = 1;
      set_rf_en(inst.i.b.ir1w1.rs, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      rd_bk[1] = selii;
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
      set_rf_en(inst.i.b.ir3w1.rs0, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      set_rf_en(inst.i.b.ir3w1.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      set_rf_en(inst.i.b.ir3w1.rs2, rd_bk[2], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);

      if(inst.i.b.ir3w1.d) begin
        wr_en = '{default : 1};
        adr_wr[0] = inst.i.b.ir3w1.rd & ('1 << 1);
        adr_wr[1] = adr_wr[0] + 1;
        rd_bk[3] = rbk_sel_e'(rd_bk[2] + 1);
      end
      else begin
        wr_en[0] = 1;
        adr_wr[0] = inst.i.b.ir3w1.rd;
      end
      
      case(inst.i.b.ir3w1.fun)
      iop31_mul   : begin op = inst.i.b.ir3w1.s ? op_smul : op_umul; end
      iop31_mad   : begin op = inst.i.b.ir3w1.s ? op_smad : op_umad; end
      iop31_msu   : begin op = inst.i.b.ir3w1.s ? op_smsu : op_umsu; end
      iop31_add3  : begin op = inst.i.b.ir3w1.s ? op_add3 : op_uadd3; end
      endcase
    end
    else if(inst.i.op == iop_r2w1) begin
      set_rf_en(inst.i.b.ir2w1.rs0, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      set_rf_en(inst.i.b.ir2w1.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      if(inst.i.b.ir2w1.fun inside {iop21_div, iop21_udiv}) begin
        wr_en = '{default : 1};
        adr_wr[0] = inst.i.b.ir2w1.rd & ('1 << 1);
        adr_wr[1] = adr_wr[0] + 1;
        rd_bk[3] = rbk_sel_e'(rd_bk[2] + 1);
      end
      else begin
        wr_en[0] = 1;
        adr_wr[0] = inst.i.b.ir2w1.rd;
      end

      case(inst.i.b.ir2w1.fun)
      iop21_div   : begin op = op_div; end
      iop21_quo   : begin op = op_quo; end
      iop21_res   : begin op = op_res; end
      iop21_udiv  : begin op = op_udiv; end
      iop21_uquo  : begin op = op_uquo; end
      iop21_ures  : begin op = op_ures; end
      endcase
    end
    else if(inst.i.op inside {iop_fcrs}) begin
      imm = {inst.i.b.fcr.os2, inst.i.b.fcr.os1, inst.i.b.fcr.os0};
      set_rf_en(inst.i.b.fcr.ja, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      op = op_fcr;
      case(inst.i.op)
      iop_fcr   : begin pr_br_dep = 0; br_op = bop_az; end
      iop_fcrn  : begin pr_br_dep = 0; br_op = bop_naz; end
      iop_fcrb  : begin pr_br_dep = 1; br_op = bop_az; end
      iop_fcrbn : begin pr_br_dep = 1; br_op = bop_naz; end
      endcase
      adr_wr[0] = 0;
      wr_en[0] = inst.i.b.fcr.l;
      msk_op = inst.i.b.fcr.mu ? (inst.i.b.fcr.l ? mop_if : mop_rstor) : mop_nop;
      msc_op = inst.i.b.fcr.su ? (inst.i.b.fcr.l ? sop_store : sop_pop2n) : sop_nop;
    end
    else if(inst.i.op inside {iop_bs}) begin
      imm = {inst.i.b.b.sc, inst.i.b.b.os};
      op = op_br;
      case(inst.i.op)
      iop_b   : begin pr_br_dep = 0; br_op = bop_az; end
      iop_bn  : begin pr_br_dep = 0; br_op = bop_naz; end
      iop_bb  : begin pr_br_dep = 1; br_op = bop_az; end
      iop_bbn : begin pr_br_dep = 1; br_op = bop_naz; end
      endcase
      case(inst.i.b.b.sop)
      2'b00 : msc_op = sop_nop;
      2'b01 : msc_op = sop_pop2n;
      2'b10 : msc_op = sop_store;
      endcase
      case(inst.i.b.b.sop)
      3'b000 : msk_op = mop_nop;
      3'b001 : msk_op = mop_bc;
      3'b010 : msk_op = mop_rstor;
      3'b011 : msk_op = mop_loop;
      3'b100 : msk_op = mop_else;
      3'b101 : msk_op = mop_cont;
      3'b110 : msk_op = mop_if;
      3'b111 : msk_op = mop_brk;
      endcase
    end
    else if(inst.i.op inside {iop_cmps}) begin
      imm = {inst.i.b.cmpi.imm1, inst.i.b.cmpi.imm0};
      set_rf_en(inst.i.b.cmpi.rs, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      case(inst.i.op)
      iop_cmp   :
      begin
        op = op_cmp;
        set_rf_en(inst.i.b.cmp.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      end
      iop_cmpu  :
      begin
        op = op_ucmp;
        set_rf_en(inst.i.b.cmp.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      end
      iop_cmpi  : begin op = op_cmp; rd_bk[1] = selii; end
      iop_cmpiu : begin op = op_ucmp; rd_bk[1] = selii; end
      endcase
      case(inst.i.b.cmp.ctyp)
      3'b000 : cmp_op = cop_e;
      3'b001 : cmp_op = cop_g;
      3'b010 : cmp_op = cop_ge;
      3'b011 : cmp_op = cop_l;
      3'b100 : cmp_op = cop_le;
      endcase
      case(inst.i.b.cmp.mtyp)
      4'b0000 : merge_op = pm_nop;
      4'b0001 : merge_op = pm_unc;
      4'b0010 : merge_op = pm_and;
      4'b0011 : merge_op = pm_andcm;
      4'b0100 : merge_op = pm_or;
      4'b0101 : merge_op = pm_orcm;
      4'b0111 : merge_op = pm_or_andcm;
      4'b0111 : merge_op = pm_and_orcm;
      endcase
      pr_adr_wr[0] = inst.i.b.cmp.pr0;
      pr_adr_wr[1] = inst.i.b.cmp.pr1;
      pr_wr_en[0] = pr_adr_wr[0] == 0;
      pr_wr_en[1] = pr_adr_wr[1] == 0;
    end
    else if(inst.i.op inside {iop_sp_dse, iop_ls_dse}) begin
      rd_bk[2] = selii;
      m_b = inst.i.b.ld.b;
      m_ua = inst.i.b.ld.ua;
      adr_wr[0] = inst.i.b.ld.rd;
      if(inst.i.op inside {iop_lw, iop_lh, iop_lb, iop_ll, iop_lhu, iop_lbu}) begin
        imm = {inst.i.b.ld.os1, inst.i.b.ld.os0};
        set_rf_en(inst.i.b.ld.rb, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
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
        set_rf_en(inst.i.b.st.rb, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
        set_rf_en(inst.i.b.st.rs, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
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
        set_rf_en(inst.i.b.cmpxchg.rs0, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
        set_rf_en(inst.i.b.cmpxchg.rs1, rd_bk[2], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      end
      else if(inst.i.op == iop_fetadd) begin
        op = op_fetadd;
        imm = {inst.i.b.ld.os1, inst.i.b.ld.os0};
      end
      else if(inst.i.op == iop_mctl) begin
        m_fun = inst.i.b.mctl.fun;
        imm = {inst.i.b.mctl.os1, inst.i.b.mctl.os0};
        if(inst.i.b.mctl.c)
          op = op_cache;
        if(m_fun < 7)
          op = op_pref;
        else if(m_fun < 13)
          op = op_sync;
        else if(m_fun == 13)
          op = op_synci;
      end
    end
    else if(inst.i.op == iop_smsg) begin
      op = op_smsg;
      adr_wr[0] = inst.i.b.smsg.rd;
      wr_en[0] = 1;
      set_rf_en(inst.i.b.smsg.rss, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      set_rf_en(inst.i.b.smsg.rvs, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
      m_s = inst.i.b.smsg.s;
      m_t = inst.i.b.smsg.t;
      m_b = inst.i.b.smsg.b;
      m_mid = inst.i.b.smsg.mid;
    end
    else if(inst.i.op == iop_rmsg) begin
      op = op_rmsg;
      m_mid = inst.i.b.rmsg.mid;
      m_b = inst.i.b.rmsg.b;
      m_fifos = inst.i.b.rmsg.fifos;
      adr_wr[0] = inst.i.b.rmsg.rvd;
      wr_en[0] = 1;
      adr_rmsg[0] = inst.i.b.rmsg.rd & ('1 << 1);
      adr_rmsg[1] = adr_rmsg[0] + 1;
      pr_adr_wr[0] = inst.i.p;
      pr_wr_en[0] = pr_adr_wr[0] == 0;
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
        adr_wr[0] = inst.i.b.ir2w1.rd;
        wr_en[0] = inst.i.b.cop.code[0];
        if(!wr_en[0])
          set_rf_en(inst.i.b.ir2w1.rs0, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_wr, cnt_srf_wr);
        imm = (inst.i.b.cop.code >> 2) & 9'b111111111;
      end
      icop_eret :
      begin
        op = op_eret;
        priv = 1;
      end      
      endcase
    end
    
	  if(is_vec)
	    foreach(adr_wr[i]) begin
	      bk_wr[i] = adr_wr[i] & ~{'1 << bits_vrf_bks};
		    grp_wr[i] = adr_wr[i] >> bits_prf_p_grp;
		    adr_wr[i] = (adr_wr[i] >> bits_vrf_bks) & ~{'1 << (bits_prf_p_grp - bits_vrf_bks)};
		  end
		else
	    foreach(adr_wr[i]) begin
	      bk_wr[i] = adr_wr[i] & ~{'1 << bits_srf_bks};
		    grp_wr[i] = adr_wr[i] >> bits_prf_p_grp;
		    adr_wr[i] = (adr_wr[i] >> bits_srf_bks) & ~{'1 << (bits_prf_p_grp - bits_srf_bks)};
		  end

   foreach(adr_rmsg[i]) begin
      bk_rmsg[i] = adr_rmsg[i] & ~{'1 << bits_srf_bks};
      grp_rmsg[i] = adr_rmsg[i] >> bits_prf_p_grp;
      adr_rmsg[i] = (adr_rmsg[i] >> bits_srf_bks) & ~{'1 << (bits_prf_p_grp - bits_srf_bks)};
    end
    
	endfunction : decode
	
	function bit is_pd_br();
	  if(!decoded) decode();
    return pr_br_dep; /// && inst.i.op inside {iop_bs, iop_fcrs};
	endfunction : is_pd_br

	function bit is_priv();
	  if(!decoded) decode();
    return priv;
	endfunction : is_priv

	function bit is_ise_inst();
	  if(!decoded) decode();
    return op inside {ise_ops};
	endfunction : is_ise_inst
		
	function void set_wcnt(inout uchar wcnt);
	  uchar t;
	  if(is_vec) begin
	    if(op inside {spu_only_ops}) begin
	      t = stage_rrf_rrc + stage_eex_vwbp + lat_ise;
	    end
	    else begin
	      t = stage_rrf_vwbp + lat_ise;
	    end
	  end
	  else if(op inside {ise_ops})
	    t = 0;
	  else
	    t = stage_rrf_swb + lat_ise;
	  if(wcnt < t)
	    wcnt = t;
	endfunction : set_wcnt
		
	function void set_data(const ref uchar data[num_ibuf_bytes], input uchar start, id = 0, bit vec = 0);
    fuid = id;
    is_vec = vec;
    decoded = 0;
    pr_br_dep = 0;
    priv = 0;
    en_dse = 0;
    en_spu = 0;
    en_fu = '{default : 0};
    pr_adr_wr = '{default : 0};
    op = op_nop;
    foreach(inst.b[i])
      inst.b[i] = data[start+i];
      
    decode();
    if(op inside {dse_ops})
      en_dse = 1;
    else if(vec)  
      en_fu[fuid] = 1;
    else if(op inside {spu_only_ops, spu_com_ops})
      en_spu = 1;    
	endfunction : set_data
	
  function void analyze_rs(input uchar vmode, ref bit v_en[cyc_vec][num_vrf_bks], s_en[cyc_vec][num_srf_bks], inout uchar vrf, srf, dse);
    uchar cnt_v = 0, cnt_s = 0;
    if(!decoded) decode();
    foreach(vrf_en[i,j]) begin
      v_en[i][j] = v_en[i][j] | vrf_en[i][j];
      cnt_v += v_en[i][j];
    end
    foreach(srf_en[i,j]) begin
      s_en[i][j] = s_en[i][j] | srf_en[i][j];
      cnt_s += s_en[i][j];
    end
    if(cnt_s > srf)
      srf = cnt_s;
    if(cnt_v > vrf)
      vrf = cnt_v;
    if(vmode > vrf)
      vrf = vmode;
    if(is_vec)
      dse = vmode;
    else
      dse = 1;
  endfunction : analyze_rs

  function void analyze_rd(ref uchar vrf[num_vrf_bks], srf[num_srf_bks], inout uchar pr);
    if(!decoded) decode();
    foreach(wr_en[i])
      if(wr_en[i]) begin
        if(is_vec)
          vrf[bk_wr[i]]++;
        else
          srf[bk_wr[i]]++;
      end
      
    foreach(pr_wr_en[i])
      pr++;
      
    if(op == op_rmsg)
      foreach(bk_rmsg[i])
        srf[bk_rmsg[i]]++;
  endfunction : analyze_rd
  
  ///reallocate en set by set_data
  function void analyze_fu(inout bit spu, dse, ref bit fu[num_fu]);
    en_dse = 0;
    en_spu = 0;
    en_fu = '{default : 0};
    if(!decoded) decode();
    if(op inside {dse_ops})
      en_dse = 1;
    else if(op inside {spu_ops})
      en_spu = 1;
    else if(is_vec) begin
      if(op inside {spu_only_ops}) begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == sfu) begin
            en_fu[i] = 1;
            break;
          end
      end
      else begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == alu) begin
            en_fu[i] = 1;
            break;
          end
      end
    end
    else if(op inside {spu_com_ops})
      en_spu = 1;
      
    spu = en_spu;
    dse = en_dse;
    fu = en_fu;
  endfunction : analyze_fu

  function void fill_rfm(input tr_ise2rfm rfm);
    if(!decoded) decode();
    if(en_spu) begin
      rfm.spu_en = 1;
      rfm.spu_imm = imm;
      foreach(rfm.spu_rd_bk[i])
        rfm.spu_rd_bk[i] = rd_bk[i];
    end
    else if(en_dse) begin
      rfm.dse_en = 1;
      rfm.dse_imm = imm;
      foreach(rfm.dse_rd_bk[i])
        rfm.dse_rd_bk[i] = rd_bk[i];      
    end
    else begin
      rfm.en[fuid] = 1;
      rfm.fu[fuid].imm = imm;
      foreach(rfm.fu[0].rd_bk[i])
        rfm.fu[fuid].rd_bk[i] = rd_bk[i];       
    end
  endfunction : fill_rfm

  function void fill_spu(input tr_ise2spu spu);
    if(!decoded) decode();
    
    spu.pr_nmsk = '{default : 0};
    if(en_spu) begin
      spu.sop = msc_op;
      spu.mop = msk_op;
      spu.bop = br_op;
      spu.cop = cmp_op;
      spu.op = op;
      spu.srf_wr_adr = adr_wr[0];
      spu.srf_wr_bk = bk_wr[0];
      spu.pr_rd_adr_spu = pr_adr_rd;
      spu.pr_inv_spu = 0;
      spu.pr_nmsk_spu = 0;
      spu.pr_br_dep = pr_br_dep;
      spu.srf_wr_dsel = 0;
      spu.srf_wr_adr = adr_wr[0];
      spu.srf_wr_bk = bk_wr[0];
    end
    else if(en_dse) begin
      spu.pr_rd_adr_dse = pr_adr_rd;
      spu.pr_nmsk_dse = 0;
      spu.pr_inv_dse = 0;
      spu.pr_wr_adr2 = pr_adr_wr[0];
    end
    else begin
      spu.pr_inv[fuid] = 0;
      spu.pr_nmsk[fuid] = 0;
      spu.pr_rd_adr[fuid] = pr_adr_rd;
      if(op inside {op_cmp, op_ucmp}) begin
        spu.pr_wr_adr0 = pr_adr_wr[0];
        spu.pr_wr_adr1 = pr_adr_wr[1];
      end
    end
  endfunction : fill_spu

  function void fill_dse(input tr_ise2dse dse);
    if(!decoded) decode();
    if(en_dse) begin
      dse.wr_adr = adr_wr[0];
      dse.wr_bk = bk_wr[0];
      dse.ua_wr_bk = (rd_bk[0] >= selv0 && rd_bk[0] <= selv_e) ? rd_bk[0] - selv0 : 0;
      dse.ua_wr = m_ua != 0 ? 1 : 0;
      dse.op = op;
      dse.bp_data = (rd_bk[1] inside {selspu, [selfu0:selfu0+num_fu-1]}) ? 1 : 0;
    end
  endfunction : fill_dse

  function void fill_spa(input tr_ise2spa spa);
    if(!decoded) decode();
    if(!is_vec) return;
    if(op inside {op_cmp, op_ucmp}) begin
      spa.fmerge = merge_op;
    end
    
    if(op inside {dse_ops}) begin
      spa.bp_rf_dse = rd_bk[1];
      spa.bp_rf_dse_wp = 0;
    end
    else begin
      spa.fu[fuid].en = 1;
      spa.fu[fuid].op = op;
      spa.fu[fuid].cop = cmp_op;
      foreach(spa.fu[0].bp_sel[i])
        spa.fu[fuid].bp_sel[i] = rd_bk[i];
      spa.fu[fuid].vrf_wr_bk = bk_wr[0];
      spa.fu[fuid].vrf_wr_adr = adr_wr[0];
    end
  endfunction : fill_spa

  function bit dse_block(input bit no_ld, no_st, no_smsg, no_rmsg);
    if(!decoded) decode();
    if(en_spu) begin
      if(no_ld && op inside {iop_lw, iop_lh, iop_lb, iop_ll, iop_lhu, iop_lbu})
        return 1;
      if(no_st && op inside {iop_sw, iop_sh, iop_sb, iop_sc})
        return 1;
      if(no_smsg && op == op_smsg)
        return 1;
      if(no_rmsg && op == op_rmsg)
        return 1;
    end
    return 0;
  endfunction : dse_block

  function void map_wr_grp(const ref uchar
        vrf_map[num_inst_vrf/num_prf_p_grp], 
        srf_map[num_inst_srf/num_prf_p_grp]);
    if(!decoded) decode();
    if(is_vec) begin
      grp_wr[0] = vrf_map[grp_wr[0]];
      grp_wr[1] = grp_wr[0];
    end
    else begin
      grp_wr[0] = srf_map[grp_wr[0]];
      grp_wr[1] = grp_wr[0];
      grp_rmsg[0] = srf_map[grp_rmsg[0]];
      grp_rmsg[1] = grp_rmsg[0];
    end
  endfunction : map_wr_grp
endclass

class inst_fg_c extends ovm_object;
  uchar data[num_ifet_bytes];
  
  `ovm_object_utils_begin(inst_fg_c)
    `ovm_field_sarray_int(data, OVM_ALL_ON + OVM_BIN)
  `ovm_object_utils_end
  
	function new(string name = "inst_fg_c");
		super.new(name);
	endfunction : new

	function void fill(const ref uchar i[num_ifet_bytes]);
	  data = i;
	endfunction : fill
endclass

parameter uchar num_inst_bytes = $bits(inst_u) / 8;