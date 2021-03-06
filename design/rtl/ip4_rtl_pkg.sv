/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_pkg.sv
/// Title            : Package header for ip4
/// Version          : 0.1
/// Last modified    : Mar 7 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Mar 7 2010

`ifndef IP4_RTL_PKG
`define IP4_RTL_PKG
///`include "ip4_rtl.svh"

package ip4_rtl_pkg;
`include "ip4_tlm_ts.svh"

typedef byte unsigned       uchar;  /// 8bits
typedef shortint unsigned   ushort; /// 16bits
typedef int unsigned        uint;
typedef longint unsigned    ulong;

parameter uchar WORD_BYTES        = 4,
                HALF_BYTES        = WORD_BYTES / 2,
                WORD_BITS         = WORD_BYTES * 8,
                HALF_BITS         = HALF_BYTES * 8;

typedef bit[HALF_BYTES - 1 : 0][7:0] halfb;
typedef bit [HALF_BITS - 1: 0] half;
typedef bit[WORD_BYTES - 1 : 0][7:0] wordb;
typedef bit [WORD_BITS - 1: 0] word;
typedef bit [1:0][HALF_BITS - 1: 0] wordh;

typedef union packed{
  bit[HALF_BYTES - 1 : 0][7:0] b;
  bit [HALF_BITS - 1: 0] h;
} halfu;

typedef union packed{
  bit[WORD_BYTES - 1 : 0][7:0] b;
  bit [WORD_BITS - 1: 0] w;
  halfu [1:0] h;
} wordu;

///Basic functions for parameters etc
  
function automatic ulong clogb2(
  input ulong value
);
  ulong v;
  clogb2 = 0;
  for (v = value; v > 0; clogb2 = clogb2 + 1)
    v = v >> 1;
endfunction

function automatic ulong n2w(
  input ulong value
);
  n2w = clogb2(value);
  if(n2w > 0)
    n2w--;
endfunction
  
function automatic ulong max2(
  input ulong a0, a1
);
  max2 = a0;
  if (a0 < a1)
    max2 = a1;
endfunction

function automatic ulong min2(
  input ulong a0, a1
);
  min2 = a0;
  if (a0 > a1)
    min2 = a1;
endfunction

parameter uchar WID_AXI_DATA    = 64,
                WID_AXI_ADDR    = 32,
                WID_AXI_ID      = 4,
                BYTES_AXI_DATA  = WID_AXI_DATA / 8;

parameter uint  NUM_SP            = 8,
                NUM_VEC           = 32,
                NUM_SFU           = NUM_SP,
                NUM_THREAD        = 4,
                NUM_FU            = 3,
                NUM_FU_RP         = 4,
                NUM_PHY_VRF_GRP   = 64,
                NUM_PHY_SRF_GRP   = 32,
                NUM_PRF_P_GRP     = 16,
                NUM_VRF_BKS       = 4,
                NUM_SRF_BKS       = 2,
                NUM_BP_CO         = 4,
                NUM_PR            = 7,
                NUM_W_CNT         = 2,
                NUM_IFET_BYTES    = 16,
                NUM_INST_VRF      = 64,
                NUM_INST_SRF      = 32;

parameter uchar CYC_VEC       = NUM_VEC / NUM_SP,     ///4
                CYC_HVEC      = CYC_VEC / 2,          ///2
                CYC_SFU_BUSY  = NUM_VEC / NUM_SFU;    ///4 

parameter uchar LAT_MAC           = 5,
                LAT_SFU           = 16,
                LAT_RF            = 1,
                LAT_RBP           = 1,
                LAT_VWBP          = 1,    ///vector writeback bypass time
                LAT_WB            = 4,
                LAT_ISE           = 2,
                LAT_IFE           = 2,
                LAT_L1M           = 1,
                LAT_SEL           = 1,
                LAT_SWBP          = 1;    ///dse writeback bypass time
                
parameter uint  NUM_SMEM_GRP      = 4,
                NUM_SMEM_GRP_W    = 512,
                NUM_DCHE_CL       = CYC_VEC,
                NUM_DCHE_ASO      = 4,
                NUM_DCHE_ENT      = NUM_SMEM_GRP_W / (NUM_DCHE_CL * NUM_DCHE_ASO),
                NUM_BR_HISTORY    = 32,
                NUM_FCR_RET       = 8,
                NUM_STQUE         = 8,
                NUM_LDQUE         = 16,
                NUM_LLCK          = 4,
                NUM_FIFO          = 8,
                NUM_INST_ADR      = max2(NUM_INST_VRF / NUM_VRF_BKS, NUM_INST_SRF / NUM_SRF_BKS);
                
parameter uchar WID_WORD        = n2w(WORD_BYTES),
                WID_HALF        = n2w(HALF_BYTES),
                WID_VRF_BKS     = n2w(NUM_VRF_BKS),
                WID_SRF_BKS     = n2w(NUM_SRF_BKS),
                WID_TID         = n2w(NUM_THREAD),
                WID_VID         = n2w(NUM_VEC),
                WID_SP          = n2w(NUM_SP),
                WID_CYC         = n2w(CYC_VEC),
                WID_XCHG        = n2w(CYC_HVEC),
                WID_IFET        = n2w(NUM_IFET_BYTES),
                WID_PRF_P_GRP   = n2w(NUM_PRF_P_GRP),
                WID_INST_ADR    = n2w(NUM_INST_ADR),
                WID_SMEM_BK     = n2w(NUM_SP),
                WID_SMEM_ADR    = n2w(NUM_SMEM_GRP_W),
                WID_SMEM_GRP    = n2w(NUM_SMEM_GRP),
                WID_DCHE_CL     = n2w(NUM_DCHE_CL),
                WID_DCHE_IDX    = n2w(NUM_DCHE_ENT),
                WID_DCHE_ASO    = n2w(NUM_DCHE_ASO);

/*
                                           pipeline stages:
ise,ife:      | ife0 | ife1 | ise0 | ise1 | rrf |

                                           pipeline stages:
                                            * scl                       *dse exp             * msc  * vec
exe:      | rrf | rrc0 | rrc1 | rrc2 | rrc3 | exe0 | exe1 | exe2 | exe3 | exe4 | vwbp | vwb  | vwb  | vwb  | vwb_end |
load:     | rrf | rrc0 |  ag  |  tag |  sel |  ad0 | ad1  | dc   | lxg0 | lxg1 | 
store:    | rrf | rrc0 |  ag  |  tag |  sel | sxg0 | sxg1 | dc   |
dse pr:   | rrf | rrc0 |  ag  |  tag | sel0 | sel1 | sel2 | sel3 | dprb | dprw |
dse emsk: | rrf | rrc0 |  ag  |  tag |  sel | dem  | dbr  |
spu:      | rrf | rrc0 | rrc1 | exs0 | exs1 | exs2 | exs3 | exs4 | exs5 | exs6 |
spu sr:   | rrf | rrc0 | rrc1 | exs0 | exs1 | exs2 |      | srb  |  sra | src  |
cmp/fcmp: | rrf | rrc0 | rrc1 | rrc2 | rrc3 | cmp0 | cmp1 | cmp2 | cem  | cbr  |
          0     1      2      3      4      5      6      7      8      9      10     11     12     13     14        15
                                            0      1      2      3      4      5      6      7      8      9         10
                       0      1      2      3      4      5      6    
  */  
                
parameter uchar STAGE_RRF_RRC0    = LAT_RF + LAT_RBP - 1,           ///1
                STAGE_RRF_RRC1    = STAGE_RRF_RRC0 + 1,             ///2
                STAGE_RRF_EXS0    = STAGE_RRF_RRC0 + 2,             ///3
                STAGE_RRF_EXS1    = STAGE_RRF_EXS0 + 1,             ///4
                STAGE_RRF_EXS2    = STAGE_RRF_EXS0 + 2,             ///5
                STAGE_RRF_EPS     = STAGE_RRF_EXS2,                 ///
                STAGE_RRF_EXS3    = STAGE_RRF_EXS0 + 3,             ///6
                STAGE_RRF_RRC     = STAGE_RRF_RRC0 + CYC_VEC - 1,   ///4
                STAGE_RRF_EXE0    = STAGE_RRF_RRC + 1,              ///5
                STAGE_RRF_EXE     = STAGE_RRF_RRC + LAT_MAC,        ///9
                STAGE_RRF_CMP     = STAGE_RRF_RRC + NUM_FU,         ///7
                STAGE_RRF_CEM     = STAGE_RRF_CMP + 1,              ///8
                STAGE_RRF_CBR     = STAGE_RRF_CEM + 1,              ///8
                STAGE_RRF_AG      = STAGE_RRF_RRC0 + LAT_RF,        ///2
                STAGE_RRF_TAG     = STAGE_RRF_AG + 1,               ///3
                STAGE_RRF_SEL     = STAGE_RRF_TAG + 1,              ///4
                STAGE_RRF_DPRB    = STAGE_RRF_SEL + CYC_VEC,        ///8
                STAGE_RRF_DPRW    = STAGE_RRF_DPRB + 1,             ///9
                STAGE_RRF_SXG0    = STAGE_RRF_SEL + LAT_SEL,        ///5
                STAGE_RRF_SXG     = STAGE_RRF_SXG0 + CYC_HVEC - 1,  ///6
                STAGE_RRF_DEM     = STAGE_RRF_SEL + LAT_SEL,        ///5
                STAGE_RRF_DBR     = STAGE_RRF_DEM + 1,              ///6
                STAGE_RRF_DC      = STAGE_RRF_SXG + LAT_L1M,        ///7
                STAGE_RRF_LXG0    = STAGE_RRF_DC + 1,               ///8
                STAGE_RRF_LXG     = STAGE_RRF_DC + CYC_HVEC,        ///9 must > STAGE_RRF_DPRW
                STAGE_RRF_VWBP    = STAGE_RRF_EXE + LAT_VWBP,       ///10
                STAGE_RRF_VWB     = STAGE_RRF_VWBP + 1,             ///11
                STAGE_RRF_VWB_END = STAGE_RRF_VWBP + CYC_VEC,       ///14
                STAGE_RRF_SRB     = STAGE_RRF_VWBP - 3,             ///6
                STAGE_RRF_SRA     = STAGE_RRF_VWBP - 2,             ///7
                STAGE_RRF_SRC     = STAGE_RRF_VWBP - 1,             ///9
                STAGE_EXE         = LAT_MAC - 1,                    ///3
                STAGE_EXE_VWBP    = STAGE_EXE + LAT_VWBP,           ///4
                STAGE_EXE_CMP     = NUM_FU - 1,                     ///2
                STAGE_EEX         = LAT_SFU + CYC_SFU_BUSY - CYC_VEC - 1,     ///27
                STAGE_EEX_VWBP    = STAGE_EEX + LAT_VWBP,           ///28
                STAGE_EEX_VWB     = STAGE_EEX_VWBP + 1,             ///29
                STAGE_ISE         = LAT_ISE - 1,                    ///1
                STAGE_IFE         = LAT_IFE - 1,                    ///1
                STAGE_ISE_EXS1    = LAT_ISE + STAGE_RRF_EXS1,       ///
                STAGE_ISE_EXS2    = LAT_ISE + STAGE_RRF_EXS2,       ///
                STAGE_ISE_EXE     = LAT_ISE + STAGE_RRF_EXE,        ///11
                STAGE_ISE_VWBP    = LAT_ISE + STAGE_RRF_VWBP,       ///12
                STAGE_ISE_CMP     = LAT_ISE + STAGE_RRF_CMP,        ///7
                STAGE_ISE_RRC0    = LAT_ISE + STAGE_RRF_RRC0,
                STAGE_ISE_DC      = LAT_ISE + STAGE_RRF_DC,
                STAGE_ISE_VWB     = LAT_ISE + STAGE_RRF_VWB,        ///13
                STAGE_ISE_VWB_END = LAT_ISE + STAGE_RRF_VWB_END,    ///16
                STAGE_ISE_DEM     = LAT_ISE + STAGE_RRF_DEM,        ///7
                STAGE_ISE_DBR     = LAT_ISE + STAGE_RRF_DBR,        ///8
                STAGE_ISE_CEM     = LAT_ISE + STAGE_RRF_CEM,        ///10
                STAGE_ISE_CBR     = LAT_ISE + STAGE_RRF_CBR,        ///9
                STAGE_ISE_EPS     = LAT_ISE + STAGE_RRF_EPS,        ///
                STAGE_ISE_SRA     = LAT_ISE + STAGE_RRF_SRA,        ///9
                STAGE_ISE_DPRW    = LAT_ISE + STAGE_RRF_DPRW;       ///10

parameter uchar CYC_VEXP_DSE      = STAGE_RRF_VWB - STAGE_RRF_DC,
                CYC_BR_DSE        = STAGE_RRF_CBR - STAGE_RRF_DC;
                
parameter uchar CK_STAGE_SFU1     = STAGE_EEX - STAGE_RRF_EXE,      ///19
                CK_STAGE_SFU0     = CK_STAGE_SFU1 - CYC_VEC + 1;    ///16

parameter uchar INDEX_ENT    = 7 , /// entry bits
                NUM_TLB_E    = 1 << INDEX_ENT,  ///128
                VPN2_WIDTH   = 18,
                TYPE_WIDTH   = 3,  /// Page Size Type bit width
                ASID_WIDTH   = 8,
                IFE_REQ_BUF  = 2,
                VADR_START   = max2(14, WID_WORD + WID_SMEM_BK + WID_DCHE_CL + WID_SMEM_GRP),  /// 8K 14BIT START for tlb and dse
                PFN_WIDTH    = 26,
                PADR_WIDTH   = VADR_START + PFN_WIDTH;    ///36

parameter uint CFG_START_ADR      = 'hf000_0000,
               CFG_MAX_MSC        = 'hffff_fff0;
               
parameter uint VADR_MAPPED = 'h0000_0000,
               VADR_NMAPNC = 'hF000_0000,
               VADR_EJTAGS = 'hF7F0_0000,
               VADR_NMAPCH = 'hF800_0000,
               SMEM_OFFSET = 'h00_0000,
               TFIF_OFFSET = 'h20_0000,
               CTLR_OFFSET = 'h24_0000,
               EJTG_OFFSET = 'h24_1000,
               EBUS_OFFSET = 'h24_2000;
               
parameter uint SGRP_SIZE = NUM_SP * NUM_SMEM_GRP_W * 4,
               SMEM_SIZE = SGRP_SIZE * NUM_SMEM_GRP,
               MSGE_SIZE = 256, /// 256BYTE
               CTLR_SIZE = 128, /// each control register of pb 128byte
               EJTG_SIZE = 128; /// each ejtag of pb 128byte

///typedef bit[PADR_WIDTH - 1:0]     padr_t;
///typedef bit[PADR_WIDTH - WORD_BYTES - WID_SMEM_BK - 1:0] exadr_t;

typedef struct packed{
  bit[WID_DCHE_ASO - 1:0] aso;
  bit[WID_DCHE_IDX - 1:0] idx;
  bit[WID_DCHE_CL - 1:0] cl;
} smadr_t;

typedef struct packed{
  bit[PADR_WIDTH - WID_WORD - WID_SMEM_BK - WID_DCHE_CL - WID_DCHE_IDX - WID_SMEM_GRP - 1:0] tag;
  bit[WID_SMEM_GRP - 1:0] grp;
} tag_t;

typedef struct packed{
  tag_t t;
  bit[WID_DCHE_IDX - 1:0] idx;
  bit[WID_DCHE_CL - 1:0] cl;
} exadr_ch_t;

typedef struct packed{
  bit[PADR_WIDTH - WID_WORD - WID_SMEM_BK - WID_SMEM_ADR - WID_SMEM_GRP - 1:0] d;
  bit[WID_SMEM_GRP - 1:0] grp;
  smadr_t a;
} exadr_sm_t;

typedef union packed{
  exadr_sm_t s;
  exadr_ch_t c;
} exadr_t;

typedef struct packed{
   exadr_t ex;
   bit[WID_SMEM_BK - 1:0] bk;
   bit[WID_WORD - 1:0] os;
} padr_t;

typedef enum uchar {
  selnull, selv[0:127], sels[0:31], selc[0:7], selz,
  selii, selpc, selspu, seldse, selfu[0:15]
} rbk_sel_e;
  
parameter rbk_sel_e selv_e = rbk_sel_e'(selv0 + NUM_VRF_BKS * CYC_VEC - 1),
                    sels_e = rbk_sel_e'(sels0 + NUM_SRF_BKS * CYC_VEC - 1),
                    selc_e = rbk_sel_e'(selc0 + NUM_BP_CO - 1),
                    selb_e = rbk_sel_e'(selfu0 + NUM_FU - 1);

typedef enum uchar {
  mac, alu, dse, sfu, spu
} unit_typ_e;

typedef enum uchar {
  mac0 = 0, alu0, sfu0, spu0, dse0, fu_null
} unit_inst_e;

typedef enum uchar {
  cop_e,    cop_g,    cop_ge,    cop_l,   cop_le,
  cop_ug,   cop_uge,  cop_ul,    cop_ule
} cmp_opcode_e;

typedef enum uchar {
  pm_nop,     pm_and,     pm_or,      pm_or_andcm,
  pm_unc,     pm_andcm,   pm_orcm,    pm_and_orcm
} pr_merge_e;

typedef enum uchar {
  sop_p2n,    sop_p2nc,   sop_store,  sop_zero
} msc_opcode_e;

typedef enum uchar {
  mop_nop,      mop_bc,     mop_rstor,    mop_loop,
  mop_else,     mop_cont,   mop_if,       mop_guard   ///mop_brk
} msk_opcode_e;

typedef enum bit {
  bop_az,     bop_naz
} br_opcode_e;

typedef enum uchar {
  ts_disabled, ts_rdy, ts_w_mrf, ts_w_rst, ///ts_b_pred, 
  ts_w_tsyn, ts_w_syna, ts_w_synst, ts_w_synld
}thread_state_t;

typedef enum uchar {
  priv_user,    priv_kernel,  priv_event
}priv_mode_t;

typedef enum uchar {
  ua_no,    ua_pre,     ua_post
}update_adr_t;

typedef enum uchar {
  at_burst,   at_rand,    at_randnu
}access_typ_t;

typedef enum uchar {
  rnd_even,     rnd_zero,     rnd_posi,     rnd_negi,     rnd_up,     rnd_away
}round_mode_t;

///the MOESI protocol plus a dirty state meaning no cc and modified
typedef enum bit[2:0] {
  cs_inv,     cs_shared,    cs_owned,     cs_exclusive,   cs_modified,
  cs_dirty
}cache_state_t;

function automatic bit need_writeback(
  input cache_state_t s
);
  return s inside {cs_owned, cs_modified, cs_dirty};
endfunction

typedef enum uchar {
  ///bypass opcodes
  op_nop,     op_cmp,     op_ucmp,    op_fcmp,
  op_bp0,     op_bp1,     op_bp2,     op_bp3,      
  ///multiply opcodes
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3,   op_fmul,
  op_fmad,    op_fmsu,
  ///alu opcodes
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_lid,     op_max,     
  op_min,     op_umin,    op_umax,
  op_ext,     op_ins,     op_seb,     op_she,
  op_wsbh,    op_vid,
  ///floating point
  op_fadd,    op_fsub,    op_fmax,    op_fmin,
  op_fround,  op_ffloor,  op_ftrunc,  op_fceil,
  op_fabs,    op_fdim,    
  ///sfu opcodes
  op_div,     op_udiv,    op_quo,
  op_uquo,    op_res,     op_ures,
  op_fdiv,    op_fexp2,
  op_flog2,   op_frootn,  op_fpow,
  op_fpown,   op_fpowr,   op_fsqrt,
  op_frsqrt,  op_fhypot,  op_fsin,
  op_fcos,    op_ftan,    op_fatan,
  op_fsinh,   op_fcosh,   op_ftanh,
  op_fatanh,
  ///dse opcodes
  op_pera,    op_perb,    op_shf4a,   op_shf4b,
  op_lw,      op_sw,      op_lh,      op_sh,
  op_lb,      op_sb,      op_ll,      op_sc,
  op_cmpxchg, op_fetadd,  op_lhu,     op_lbu,
  op_pref,    op_synci,   op_cache,   op_tmrf,
  op_fmrf,    op_syna,    op_synld,   op_synst,
  ///spu opcodes
  op_gp2s,    op_s2gp,    op_br,      op_fcr,
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_tlbp,    op_tlbr,    op_tlbwi,   op_tlbwr,
  op_mvs,     op_rmsg,    op_smsg
} opcode_e;

parameter opcode_e bp_ops[36] = '{
  op_cmp,     op_ucmp,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_lid,     op_seb,     op_she,     op_wsbh,
  op_max,     op_min,     op_umin,    op_umax,
  op_fadd,    op_fsub,    op_fmax,    op_fmin,
  op_fround,  op_ffloor,  op_ftrunc,  op_fceil,
  op_fabs,    op_fdim
};

parameter opcode_e sfu_only_ops[24] = '{
  op_div,     op_udiv,    op_quo,
  op_uquo,    op_res,     op_ures,
  op_fdiv,    op_fexp2,
  op_flog2,   op_frootn,  op_fpow,
  op_fpown,   op_fpowr,   op_fsqrt,
  op_frsqrt,  op_fhypot,  op_fsin,
  op_fcos,    op_ftan,    op_fatan,
  op_fsinh,   op_fcosh,   op_ftanh,
  op_fatanh
};

parameter opcode_e mac_ops[24] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3,   op_fmul,
  op_fmad,    op_fmsu
};

parameter opcode_e alu_ops[58] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3,   op_fmul,
  op_fmad,    op_fmsu,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_max,     op_min,     
  op_umin,    op_umax,    op_lid,
  op_ext,     op_ins,     op_seb,     op_she,
  op_wsbh,
  op_fadd,    op_fsub,    op_fmax,    op_fmin,
  op_fround,  op_ffloor,  op_ftrunc,  op_fceil,
  op_fabs,    op_fdim
};

parameter opcode_e sfu_ops[41] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_lid,     op_seb,     op_she,     op_wsbh,
  op_max,     op_min,     op_umin,    op_umax,
  op_fadd,    op_fsub,    op_fmax,    op_fmin,
  op_fround,  op_ffloor,  op_ftrunc,  op_fceil,
  op_fabs,    op_fdim
};

parameter opcode_e dse_ops[22] = '{
  op_pera,    op_perb,    op_shf4a,   op_shf4b,
  op_lw,      op_sw,      op_lh,      op_sh,
  op_lb,      op_sb,      op_ll,      op_sc,
  op_cmpxchg, op_fetadd,  op_lhu,     op_lbu,
  op_pref,    op_syna,    op_synci,   op_cache,
  op_tmrf,    op_fmrf
};

parameter opcode_e ld_ops[6] = '{
  op_lw,      op_lh,      op_lb,      op_ll,
  op_lhu,     op_lbu
};

parameter opcode_e st_ops[4] = '{
  op_sw,      op_sh,      op_sb,      op_sc
};

parameter opcode_e spu_ops[17] = '{
  op_gp2s,    op_s2gp,    op_br,      op_fcr,
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_tlbp,    op_tlbr,    op_tlbwi,   op_tlbwr,
  op_mvs
};

parameter opcode_e ise_ops[11] = '{
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_syna,    op_synld,
  op_synst,   op_msync,   op_alloc
};

parameter opcode_e tlb_ops[4] = '{
  op_tlbp,    op_tlbr,    op_tlbwi,   op_tlbwr
};

parameter opcode_e spu_com_ops[36] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_seb,     op_she,     op_wsbh,
  op_fadd,    op_fsub,    op_fmax,    op_fmin,
  op_fround,  op_ffloor,  op_ftrunc,  op_fceil,
  op_fabs,    op_fdim
};

parameter opcode_e ise_zw_ops[5] = '{
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk
};

typedef enum uchar {
  SR_PROC_CTL,  SR_SUPMSG,    SR_EBASE,     SR_MBASE,       SR_INDEX,
  SR_RANDOM,    SR_ENTRY_L0,  SR_ENTRY_L1,  SR_ENTRY_AT,    SR_ENTRY_HI,
  SR_TIMER,     SR_CMP,       SR_OCMC,      SR_PCNT[0:1],   SR_PCNTC[0:1],
  SR_IIDX,      SR_IIDY,      SR_IIDZ,      SR_EXPFV,       SR_DSEEV,
  SR_MSCO,      SR_MSCU,      SR_THD_CTL,   SR_THD_ST,      SR_EXEC,
  SR_CONTENT,   SR_EPC,       SR_ERET,      SR_WIDX,        SR_WIDY,
  SR_WIDZ,      SR_ILM,       SR_CM,        SR_UEE,         SR_UER,
  SR_ASID,      SR_MD[0:7],   SR_MCS[0:2],  SR_FFS,         SR_FFC[0:1],
  SR_SUPM[0:1]
}special_reg_t;

parameter special_reg_t tlb_sr[6] = '{
  SR_INDEX,     SR_RANDOM,    SR_ENTRY_L0,    SR_ENTRY_L1,
  SR_ENTRY_HI,  SR_ASID
};

parameter special_reg_t non_kernel_sr[3] = '{
  SR_EXEC,     SR_UEE,       SR_UER
};

typedef enum uchar {
  EC_NOEXP,     EC_TLBIFET,   EC_NOTEXE,    EC_EXEPRIV,
  EC_DECODE,    EC_SYSCAL,    EC_BREAK,     EC_SUPMSG,
  EC_IFACC,     EC_LSACC,     EC_SCLFU,     EC_MSC,
  EC_TIMER,     EC_PCNT[0:1]
}cause_spu_t;

typedef enum uchar {
  EC_NODSE,     EC_TLBINV,    EC_TLBMOD,    EC_TLBPRIV,
  EC_ADRALG,    EC_SMBOND
}cause_dse_t;

typedef enum uchar {
  UE_FFCLN = 'h0,
  UE_FFREV = 'h40,
  UE_FFRTG = 'h50,
  UE_FFSTG = 'h60,
  UE_FFRFL = 'h70,
  UE_FFSFL = 'h80,
  UE_FFSYN = 'h90
}user_event_os_t;

parameter uchar NUM_MAX_IGRP_BYTES  = 44;
parameter uchar NUM_IBUF_BYTES      = NUM_MAX_IGRP_BYTES + NUM_IFET_BYTES;

typedef struct{
  tag_t[NUM_DCHE_ASO - 1:0] tag;
  cache_state_t[NUM_DCHE_ASO - 1:0] state;
  bit[1:0][NUM_DCHE_ASO - 1:0] cnt;
}cache_t;

`include "ip4_rtl_inst.svh"
`include "ip4_rtl_def.svh"

endpackage : ip4_rtl_pkg

`endif