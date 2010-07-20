
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
  iop_pref = 'b111100,      iop_cache = 'b111101,   iop_smsg = 'b001100,      iop_rmsg = 'b001101,
  iop_cmp = 'b001110,       iop_cmpu = 'b001111,    iop_cmpi = 'b010000,      iop_cmpiu = 'b010001,
  iop_cop = 'b010010,       iop_grag = 'b010011,    iop_shuf4 = 'b010100
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
  iop31_add3,   iop31_perm
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
  iop21_min,    iop21_vror,   iop21_vsr,    iop21_vsl,
  iop21_add = 'b1000000,  
                iop21_sub,    iop21_sra,    iop21_srav,
  iop21_nor,    iop21_udiv,   iop21_uquo,   iop21_ures,
  iop21_clz,    iop21_ins,    iop21_sllv,   iop21_rotv,
  iop21_xor,    iop21_she,    iop21_mv2s,   iop21_umax,
  iop21_umin,   iop21_vroru,  iop21_vsru,   iop21_vslu
} iop_r2w1_e;

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
  bit[6] sc;
  bit[18] os;
  bit[2] sop;
  bit[3] mop;
  bit[2] bt;
}i_b;

typedef struct packed{
  irda_t rd;
  bit os2;
  isrsa_t rb;
  bit[14] os1;
  bit[5] os0;
  bit[2] ua;
}i_load;

typedef struct packed{
  bit[6] os2;
  isrsa_t rb;
  irsa_t rs;
  bit[9] os1;
  bit[5] os0;
  bit[2] ua;
}i_store;

typedef struct packed{
  bit[6] os1;
  isrsa_t rb;
  irsa_t rs0, rs1;
  bit[4] os0;
  bit[5] os2;
  bit[2] ua;
}i_cmpexchg;

typedef struct packed{
  bit[6] dummy0;
  isrsa_t rss;
  bit[2] dummy1;
  bit[3] rt;
  irsa_t rvs;
  bit[6] dummy2;
  bit t, pb, b;
  bit[2] mid;
}i_smsg;

typedef struct packed{
  irda_t rdv;
  bit dummy0;
  isrda_t rds;
  bit[6] dummy1;
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
  bit[8] imm;
  ipra_t pr0, pr1;
  bit[3] ctyp;
  bit[4] mtyp;
}i_cmpi;

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
  i_cmpexchg cmpexchg;
  i_smsg smsg;
  i_rmsg rmsg;
  i_cmp cmp;
  i_cmpi cmpi;
  i_cop cop;
} i_body;

typedef struct packed{
///  bit g;
  ipra_t p;
  iop_e op;
  i_body b;
}inst_t;

typedef union packed{
  inst_t i;
  bit [5][8] b;
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
  bit [2][8] b;
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
  bit [2][8] b;
} i_ap1_u;

typedef struct packed{
  iga_t[8] a;
}i_ap2_t;

typedef union packed{
  i_ap2_t i;
  bit [3][8] b;
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
        iop_cmpxchg,    iop_fetadd,   iop_pref
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
  bit decoded, vec_rd;
  opcode_e op;
  rbk_sel_e rd_bk[num_fu_rp];
  uchar rd[2], cnt_vrf_rd, cnt_srf_rd, pr_rd, pr_wr[2], fuid;
  uint imm;
  bit vrf_en[cyc_vec][num_vrf_bks], srf_en[cyc_vec][num_srf_bks], 
      rd_en[2], pr_rd_en, pr_wr_en[2], pr_br_dep;
  cmp_opcode_e cmp_op;
  pr_merge_e merge_op;
  msc_opcode_e msc_op;
  msk_opcode_e msk_op;
  br_opcode_e br_op;
  
  `ovm_object_utils_begin(inst_c)
    `ovm_field_int(inst, OVM_ALL_ON)
  `ovm_object_utils_end
  
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
      sel = seldse;
    else if(adr == 13)
      sel = selspu;
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
    pr_rd = inst.i.p;
    pr_rd_en = pr_rd != 0;
///    rfbp = '{default : fu_null};
    
    if(inst.i.op inside {iop_i26}) begin
      imm = {inst.i.b.i26.imm1, inst.i.b.i26.imm0};
      rd[0] = inst.i.b.i26.rd;
      rd_en[0] = 1;
      rd_bk[1] = selii;
      case(inst.i.op)
      iop_lu    : begin op = op_bp1; imm = imm << (word_width / 2); end
      iop_li    : begin op = op_bp1; end
      endcase
    end
    else if(inst.i.op inside {iop_r1w1i}) begin
      uint imms = {{word_width{inst.i.b.ir1w1.imm1[$bits(inst.i.b.ir1w1.imm1) - 1]}}, inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      imm = {inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      rd[0] = inst.i.b.i26.rd;
      rd_en[0] = 1;
      set_rf_en(inst.i.b.ir1w1.rs, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);
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
      set_rf_en(inst.i.b.ir3w1.rs0, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);
      set_rf_en(inst.i.b.ir3w1.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);
      set_rf_en(inst.i.b.ir3w1.rs2, rd_bk[2], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);

      if(inst.i.b.ir3w1.d) begin
        rd_en = '{default : 1};
        rd[0] = inst.i.b.ir3w1.rd & ('1 << 1);
        rd[1] = rd[0] + 1;
        rd_bk[3] = rbk_sel_e'(rd_bk[2] + 1);
      end
      else begin
        rd_en[0] = 1;
        rd[0] = inst.i.b.ir3w1.rd;
      end
      
      case(inst.i.op)
      iop31_mul   : begin op = inst.i.b.ir3w1.s ? op_smul : op_umul; end
      iop31_mad   : begin op = inst.i.b.ir3w1.s ? op_smad : op_umad; end
      iop31_msu   : begin op = inst.i.b.ir3w1.s ? op_smsu : op_umsu; end
      iop31_add3  : begin op = inst.i.b.ir3w1.s ? op_add3 : op_uadd3; end
      iop31_perm  : begin op = inst.i.b.ir3w1.s ? op_pera : op_perb; end
      endcase
    end
    else if(inst.i.op == iop_r2w1) begin
      set_rf_en(inst.i.b.ir2w1.rs0, rd_bk[0], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);
      set_rf_en(inst.i.b.ir2w1.rs1, rd_bk[1], vec_rd, vrf_en, srf_en, cnt_vrf_rd, cnt_srf_rd);
      if(inst.i.b.ir2w1.fun inside {iop21_div, iop21_udiv}) begin
        rd_en = '{default : 1};
        rd[0] = inst.i.b.ir2w1.rd & ('1 << 1);
        rd[1] = rd[0] + 1;
        rd_bk[3] = rbk_sel_e'(rd_bk[2] + 1);
      end
      else begin
        rd_en[0] = 1;
        rd[0] = inst.i.b.ir2w1.rd;
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
      case(inst.i.op)
      iop_fcr   : begin pr_br_dep = 0; br_op = bop_az; end
      iop_fcrn  : begin pr_br_dep = 0; br_op = bop_naz; end
      iop_fcrb  : begin pr_br_dep = 1; br_op = bop_az; end
      iop_fcrbn : begin pr_br_dep = 1; br_op = bop_naz; end
      endcase
      
    end
    else if(inst.i.op inside {iop_bs}) begin
    end
    else if(inst.i.op inside {iop_cmps}) begin
      
    end
    else if(inst.i.op inside {iop_sp_dse, iop_ls_dse, iop_msg}) begin
    end
	endfunction : decode
///	
///	function bit is_scl_dse(input bit vec);
///    if(inst.i.op inside {iop_ls_dse} && !vec)
///      return 1;
///    if(is_sp_dse())
///      return 1;
///    return 0;
///	endfunction : is_scl_dse
///
///	function bit is_scl_ma(input bit vec);
///    if(inst.i.op == iop_r3w1 && inst.i.b.ir3w1.fun == iop31_mul && !vec)
///      return 1;
///    return 0;
///	endfunction : is_scl_ma	
	
	function void set_data(const ref uchar data[num_ibuf_bytes], input uchar start, id = 0);
    fuid = id;
    decoded = 0;
	  foreach(inst.b[i])
		  inst.b[i] = data[start+i];
	endfunction : set_data
	
  function void analyze_rs(input bit vec, ref bit vrf_en[cyc_vec][num_vrf_bks], srf_en[cyc_vec][num_srf_bks], inout bit dsev, uchar vrf, srf, dse);
///    bit has_vec = 0;
///    if(inst.i.op inside {iop_r1w1i}) begin
///      set_rf_en(inst.i.b.ir1w1.rs, has_vec, vrf_en, srf_en, vrf, srf);
///      if(!vec && has_vec)
///        ovm_report_warning("ISE", "decode error, non vec inst access vrf");
///    end
///    else if(inst.i.op == iop_r2w1) begin
///      set_rf_en(inst.i.b.ir2w1.rs0, has_vec, vrf_en, srf_en, vrf, srf);
///      set_rf_en(inst.i.b.ir2w1.rs1, has_vec, vrf_en, srf_en, vrf, srf);
///      if(!vec && has_vec && inst.i.b.ir2w1.fun != iop21_mv2s)
///        ovm_report_warning("ISE", "decode error, non vec inst access vrf");      
///      return;
///    end
///    else if(inst.i.op == iop_r3w1) begin
///      set_rf_en(inst.i.b.ir3w1.rs0, has_vec, vrf_en, srf_en, vrf, srf);
///      set_rf_en(inst.i.b.ir3w1.rs1, has_vec, vrf_en, srf_en, vrf, srf);
///      set_rf_en(inst.i.b.ir3w1.rs2, has_vec, vrf_en, srf_en, vrf, srf);
///      if(!vec && has_vec)
///        ovm_report_warning("ISE", "decode error, non vec inst access vrf");       
///    end
  endfunction : analyze_rs

  function void analyze_rd(input bit vec, ref uchar vrf[num_vrf_bks], srf[num_srf_bks], inout uchar pr);
  endfunction : analyze_rd
  
  function void analyze_fu(input bit vec, inout bit en_spu, en_dse, ref bit en_fu[num_fu]);
    if(inst.i.op inside {iop_r1w1i}) begin
      if(vec) begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == mac || fu_cfg[i] == alu) begin
            en_fu[i] = 1;
            break;
          end
      end
      else
        en_spu = 1;
      return;
    end
    
    if(inst.i.op == iop_r3w1) begin
      if(inst.i.b.ir3w1.fun == iop31_mul && !vec)
        en_spu = 1;
      else begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == mac || fu_cfg[i] == alu) begin
            en_fu[i] = 1;
            break;
          end
      end
      return;
    end

    if(inst.i.op == iop_r2w1) begin
      if(inst.i.b.ir2w1.fun inside {iop21_spu_ops})
        if(vec) begin
          foreach(fu_cfg[i])
            if(fu_cfg[i] == mac || fu_cfg[i] == alu) begin
              en_fu[i] = 1;
              break;
            end
        end
        else
          en_spu = 1;
                
      if(inst.i.b.ir2w1.fun inside {iop21_sfu_ops})
        foreach(fu_cfg[i])
          if(fu_cfg[i] == sfu) begin
            en_fu[i] = 1;
            break;
          end
      return;
    end
    
    if(inst.i.op inside {iop_fcrs, iop_bs})
      en_spu = 1;
    else if(inst.i.op inside {iop_sp_dse, iop_ls_dse, iop_msg})
      en_dse = 1;
        
  endfunction : analyze_fu

  function void fill_rfm(input tr_ise2rfm rfm);
    
  endfunction : fill_rfm

  function void fill_spu(input tr_ise2spu spu);
    
  endfunction : fill_spu

  function void fill_dse(input tr_ise2dse dse);
    
  endfunction : fill_dse

  function void fill_spa(input tr_ise2spa spa);
    
  endfunction : fill_spa

  function bit dse_block(input bit no_ld, no_st, no_smsg, no_rmsg);
    return 0;
  endfunction : dse_block
                
endclass

class inst_fg_c extends ovm_object;
  uchar data[num_ifet_bytes];
  
  `ovm_object_utils_begin(inst_fg_c)
    `ovm_field_sarray_int(data, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "inst_fg_c");
		super.new(name);
	endfunction : new
endclass

parameter uchar num_inst_bytes = $bits(inst_u) / 8;