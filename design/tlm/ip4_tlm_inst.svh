
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
  iop_orsi = 'b011_1_10,    iop_xorsi = 'b011_1_11, iop_ma = 'b000010,        iop_2r1w = 'b000011,
  iop_fcr = 'b000100,       iop_fcrn = 'b000101,    iop_fcrb = 'b000110,      iop_fcrbn = 'b000111,
  iop_b = 'b001000,         iop_bn = 'b001001,      iop_bb = 'b001010,        iop_bbn = 'b001011,
  iop_lw = 'b110000,        iop_sw = 'b110001,      iop_lh = 'b110010,        iop_sh = 'b110011,
  iop_lb = 'b110100,        iop_sb = 'b110101,      iop_ll = 'b110110,        iop_sc = 'b110111,
  iop_cmpxchg = 'b111000,   iop_fetadd = 'b111001,  iop_lhu = 'b111010,       iop_lbu = 'b111011,
  iop_pref = 'b111100,      iop_cache = 'b111101,   iop_smsg = 'b001100,      iop_rmsg = 'b001101,
  iop_cmp = 'b001110,       iop_cmpu = 'b001111,    iop_cmpi = 'b010000,      iop_cmpiu = 'b010001,
  iop_cop = 'b010010,       iop_grag = 'b010011,    iop_shuf4 = 'b010100,     iop_libp = 'b010101
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
}i_1r1w;

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1, rs2;
  bit[4] dummy0;
  bit[5] fun;
  bit s, d;
}i_3r1w;

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
  iop21_xor,    iop21_she,    iop21_dum0,   iop21_umax,
  iop21_umin,   iop21_vroru,  iop21_vsru,   iop21_vslu
} iop_2r1w_e;
    
typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1;
  bit[9] imm;
  iop_2r1w_e fun;
}i_2r1w;

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
  i_1r1w alui;
  i_3r1w ma;
  i_2r1w alu;
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


class inst_c extends ovm_object;
  inst_u inst;
  
  `ovm_object_utils_begin(inst_c)
    `ovm_field_int(inst, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "inst_c");
		super.new(name);
	endfunction : new

	function void set_data(const ref uchar data[num_ibuf_bytes], input uchar start);
	  foreach(inst.b[i])
		  inst.b[i] = data[start+i];
	endfunction : set_data

  function void analyze_rs(ref bit vrf_en[cyc_vec][num_vreg_bks], srf_en[cyc_vec][num_sreg_bks], inout uchar sv, vec, scl, dse);
    
  endfunction : analyze_rs

  function void analyze_rd(ref uchar vrf[num_vreg_bks], srf[num_sreg_bks], inout uchar pr);
    
  endfunction : analyze_rd
  
  function void analyze_fu(inout bit spu, dse, ref bit en_fu[num_fu], input bit sgl);
    if(inst.i.op inside {
        iop_lu
        }) begin
    end
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