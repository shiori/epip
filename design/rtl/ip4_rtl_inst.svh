typedef bit[4:0] irsa_t;
typedef bit[2:0] irda_t;
typedef bit[3:0] isrsa_t;
typedef bit[1:0] isrda_t;
typedef bit[2:0] ipra_t; 

typedef enum bit[4:0] {
  irs_z,    irs_bp[0:1],  irs_pc,   irs_co[0:3],    irs_srf[0:7],   irs_vrf[0:15]
} irs_e;

parameter irs_e irs_bp_e  = irs_bp1,
                irs_co_e  = irs_co3,
                irs_srf_e = irs_srf7,
                irs_vrf_e = irs_vrf15;
                
typedef enum bit[3:0] {
  ird_zs,   ird_zv,       ird_srfb[0:1],  ird_vrfb[0:3]
} ird_e;

parameter ird_e ird_srfb_e  = ird_srfb1,
                ird_vrfb_e  = ird_vrfb3;

typedef enum bit[5:0] {
  iop_lu  = 'b00000,        iop_li = 'b000001,      iop_addi = 'b011_0_00,    iop_andi = 'b011_0_01,
  iop_ori = 'b011_0_10,     iop_xori = 'b011_0_11,  iop_addsi = 'b011_1_00,   iop_andsi = 'b011_1_01, 
  iop_orsi = 'b011_1_10,    iop_xorsi = 'b011_1_11, iop_r3w1 = 'b000010,      iop_r2w1 = 'b000011,
  iop_fcr = 'b000100,       iop_fcrn = 'b000101,    iop_fcrp = 'b000110,      iop_fcrpn = 'b000111,
  iop_b = 'b001000,         iop_bn = 'b001001,      iop_bp = 'b001010,        iop_bpn = 'b001011,
  iop_lw = 'b110000,        iop_sw = 'b110001,      iop_lh = 'b110010,        iop_sh = 'b110011,
  iop_lb = 'b110100,        iop_sb = 'b110101,      iop_ll = 'b110110,        iop_sc = 'b110111,
  iop_cmpxchg = 'b111000,   iop_fetadd = 'b111001,  iop_lhu = 'b111010,       iop_lbu = 'b111011,
  iop_mctl = 'b111100,      iop_mrfa = 'b001100,    iop_cmsg = 'b001101,
  iop_cmp = 'b001110,       iop_cmpu = 'b001111,    iop_cmpi = 'b010000,      iop_cmpiu = 'b010001,
  iop_cop = 'b010010,       iop_vxchg = 'b010100
} iop_e;

typedef struct packed{
  irda_t rd;
  bit[20:0] imm0;
  bit[6:0] imm1;
}i_i28;

typedef struct packed{
  irda_t rd;
  irsa_t rs;
  bit[15:0] imm0;
  bit[6:0] imm1;
}i_r1w1;

typedef enum bit[4:0] {
  iop31_mul,    iop31_mad,    iop31_msu,
  iop31_add3,   iop31_fmul,   iop32_fmad,
  iop31_fmsu
} iop_r3w1_e;

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1, rs2;
  bit[5:0] dummy0;
  iop_r3w1_e fun;
  bit s, d;
}i_r3w1;

typedef enum bit[6:0] {
  iop21_uadd,   iop21_usub,   iop21_srl,    iop21_srlv,
  iop21_or,     iop21_div,    iop21_quo,    iop21_res,
  iop21_clo,    iop21_ext,    iop21_sll,    iop21_rot,
  iop21_and,    iop21_seb,    iop21_wsbh,   iop21_max,
  iop21_min,    iop21_vid,    iop21_fadd,   iop21_fmax,
  iop21_fdiv,   iop21_fexp2,  iop21_frootn, iop21_fpown,
  iop21_fround, iop21_ftrunc, iop21_fsqrt,  iop21_fabs,
  iop21_fsin,   iop21_ftan,   iop21_fsinh,  iop21_ftanh,
  iop21_add = 'b1000000,  
                iop21_sub,    iop21_sra,    iop21_srav,
  iop21_nor,    iop21_udiv,   iop21_uquo,   iop21_ures,
  iop21_clz,    iop21_ins,    iop21_sllv,   iop21_rotv,
  iop21_xor,    iop21_she,    iop21_mv2s,   iop21_umax,
  iop21_umin,   iop21_dummy0, iop21_fsub,   iop21_fmin,
  iop21_fhypot, iop21_flog2,  iop21_fpow,   iop21_fpowr,
  iop21_ffloor, iop21_fceil,  iop21_frsqrt, iop21_fdim,
  iop21_cos,    iop21_fatan,  iop21_fcosh,  iop21_fatanh
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

iop_r2w1_e iop11_ops[] = {
  iop21_srl,    iop21_clo,    iop21_ext,    iop21_sll,
  iop21_rot,    iop21_seb,    iop21_wsbh,   iop21_sra,
  iop21_clz,    iop21_ins,    iop21_she,    iop21_mv2s
};

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1;
  bit[10:0] imm;
  iop_r2w1_e fun;
}i_r2w1;

typedef struct packed{
  bit[2:0] os2;
  isrsa_t ja;
  bit[16:0] os1;
  bit[3:0] os0;
  bit mu, su, l;
}i_fcr;

typedef struct packed{
  bit[7:0] sc;
  bit[17:0] offSet;
  bit[1:0] sop;
  bit[2:0] mop;
}i_b;

typedef struct packed{
  irda_t rd;
  irsa_t rb;
  bit[15:0] os1;
  bit[2:0] os0;
  bit[1:0] ua;
  bit[1:0] t;
}i_load;

typedef struct packed{
  bit s;
  bit[1:0] os2;
  irsa_t rb;
  irsa_t rs;
  bit[10:0] os1;
  bit[2:0] os0;
  bit[1:0] ua;
  bit[1:0] t;
}i_store;

typedef struct packed{
  irda_t rd;
  irsa_t rb;
  irsa_t rs;
  bit[10:0] os1;
  bit[2:0] os0;
  bit[1:0] ua;
  bit[1:0] t;
}i_fetadd;

typedef struct packed{
  bit s;
  bit[1:0] os1;
  irsa_t rb;
  bit[1:0] dummy;
  bit[13:0] os0;
  bit[3:0] fun;
  bit c;
  bit[1:0] t;
}i_mctl;

typedef struct packed{
  bit s;
  bit[1:0] os2;
  irsa_t rb;
  irsa_t rs;
  bit[10:0] os1;
  bit[2:0] os0;
  bit[1:0] ua;
  bit[1:0] t;
}i_cmpxchg;

typedef struct packed{
  irda_t rd;
  irsa_t rs;
  bit[11:0] dummy1;
  bit[4:0] s;
  bit[1:0] st;
  bit sup;
  bit[1:0] mrfa;
  bit ft;
}i_mrfa;

typedef struct packed{
  bit[14:0] dummy;
  bit[7:0] fifos;
  bit[2:0] mrt;
  bit[1:0] vs, ss;
  bit sr;
}i_cmsg;

typedef struct packed{
  bit f;
  bit[1:0] dummy0;
  irsa_t rs0, rs1;
  bit[4:0] dummy;
  ipra_t pr0, pr1;
  bit[2:0] ctyp;
  bit[3:0] mtyp;
}i_cmp;

typedef struct packed{
  bit[2:0] imm0;
  irsa_t rs;
  bit[9:0] imm1;
  ipra_t pr0, pr1;
  bit[2:0] ctyp;
  bit[3:0] mtyp;
}i_cmpi;

typedef struct packed{
  irda_t rd;
  irsa_t rs0, rs1;
  bit[14:0] fun;
  bit s, up, t;
}i_vxchg;

typedef enum bit[4:0] {
  icop_sysc,    icop_eret,      icop_wait,      icop_exit,
  icop_brk,     icop_tsync,     icop_msync,     icop_alloc,
  icop_tlbp = 'b10000,
  icop_tlbr,    icop_tlbwi,     icop_tlbwr,     icop_asr
} iop_cop_e;

typedef struct packed{
  bit[25:0] code;
  iop_cop_e fun;
}i_cop;

typedef union packed{
  i_i28 i28;
  i_r1w1 ir1w1;
  i_r3w1 ir3w1;
  i_r2w1 ir2w1;
  i_fcr fcr;
  i_b b;
  i_load ld;
  i_store st;
  i_mctl mctl;
  i_fetadd fetadd;
  i_cmpxchg cmpxchg;
  i_mrfa mrfa;
  i_cmsg cmsg;
  i_cmp cmp;
  i_cmpi cmpi;
  i_cop cop;
  i_vxchg vxchg;
} i_body;

typedef struct packed{
  ipra_t p;
  iop_e op;
  i_body b;
} inst_t;

typedef union packed{
  inst_t i;
  bit [4:0][7:0] b;
} inst_u;

typedef bit[WID_INST_ADR - 1:0] iga_t;
parameter uchar NUM_INST_BYTES = $bits(inst_u) / 8;

typedef struct packed{
  bit[4:0] unitEn;
  bit[2:0] adrPkgB;
  bit t;
  bit[3:0] icc;
  bit nmsk;
  bit[1:0] coPkgW;  
} i_gs1_t;

typedef union packed{
  i_gs1_t i;
  bit [1:0][7:0] b;
} i_gs1_u;

typedef struct packed{
  bit t;
  bit[3:0] icc;
  bit nmsk, adrPkgB, coPkgW;
} i_gs0_t;

parameter iop_e iop_i28[] = '{
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

parameter iop_e iop_cmps[] = '{
        iop_cmp,    iop_cmpu,   iop_cmpi,   iop_cmpiu
        };
