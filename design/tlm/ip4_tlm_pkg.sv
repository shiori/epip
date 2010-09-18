/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_pkg.sv
/// Title            : Package header for ip4
/// Version          : 0.1
/// Last modified    : Mar 7 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Mar 7 2010

`ifndef IP4_TLM_PKG
`define IP4_TLM_PKG
`include "ip4_tlm.svh"

package ip4_tlm_pkg;
`include "ip4_tlm_ts.svh"
import ovm_pkg::*;

parameter VERSION = 0.1;

typedef byte unsigned       uchar;  /// 8bits
typedef shortint unsigned   ushort; /// 16bits
typedef int unsigned        uint;
typedef longint unsigned    ulong;


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

parameter time CLK_P              = 2ns;
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

parameter uchar LAT_MAC           = 5,
                LAT_SFU           = 16,
                LAT_RF            = 1,
                LAT_RBP           = 1,
                LAT_VWBP          = 1,    ///vector writeback bypass time
                LAT_WB            = 4,
                LAT_ISE           = 2,
                LAT_IFE           = 2,
                LAT_L1M           = 1,
                LAT_XCHG          = 2,
                LAT_SWBP          = 1,    ///dse writeback bypass time
                LAT_EXM           = 100;  ///external memory latency
                
parameter uint  NUM_SP            = 8,
                NUM_VEC           = 32,
                NUM_SFU           = 2,
                NUM_THREAD        = 4,
                NUM_FU            = 3,
                NUM_FU_RP         = 4,
                NUM_PHY_VRF_GRP   = 64,
                NUM_PHY_SRF_GRP   = 32,
                NUM_PRF_P_GRP     = 8,
                NUM_VRF_BKS       = 4,
                NUM_SRF_BKS       = 2,
                NUM_BP_CO         = 4,
                NUM_PR            = 7,
                NUM_W_CNT         = 2,
                NUM_IFET_BYTES    = 16,
                NUM_INST_VRF      = 32,
                NUM_INST_SRF      = 16,
///                NUM_SMEM_BK       = NUM_SP,   /// register file bank number, must be NUM_SP
                NUM_SMEM_GRP      = 4,
                NUM_SMEM_GRP_W    = 512,
                NUM_DCHE_CL       = LAT_XCHG,
                NUM_DCHE_ASO      = 4,
                NUM_DCHE_TAG      = NUM_SMEM_GRP_W / (NUM_DCHE_CL * NUM_DCHE_ASO),
                NUM_BR_HISTORY    = 32,
                NUM_FCR_RET       = 8,
///                NUM_BURST_CL      = LAT_XCHG,
///                NUM_BURST_LEN     = NUM_BURST_CL * NUM_SMEM_BK,
///                NUM_EBUS_WORDS    = 2,
///                NUM_STBUF_LINE    = LAT_XCHG,
                NUM_STQUE         = 8,
                NUM_LDQUE         = 16,
                NUM_LLCK          = 4,
                NUM_FIFO          = 8;

parameter uint CFG_START_ADR      = 'hf000_0000,
               CFG_MAX_MSC        = 'hffff_fff0;

parameter uchar CYC_VEC       = NUM_VEC / NUM_SP,     ///4
                CYC_SFU_BUSY  = NUM_VEC / NUM_SFU;    ///16 
                
parameter uchar WID_WORD        = n2w(WORD_BYTES),
                WID_HALF        = n2w(HALF_BYTES),
                WID_VRF_BKS     = n2w(NUM_VRF_BKS),
                WID_SRF_BKS     = n2w(NUM_SRF_BKS),
                WID_TID         = n2w(NUM_THREAD),
                WID_VID         = n2w(NUM_VEC),
                WID_SP          = n2w(NUM_SP),
                WID_CYC         = n2w(CYC_VEC),
                WID_IFET        = n2w(NUM_IFET_BYTES),
                WID_PRF_P_GRP   = n2w(NUM_PRF_P_GRP),
                WID_SMEM_BK     = n2w(NUM_SP),
                WID_SMEM_ADR    = n2w(NUM_SMEM_GRP_W),
                WID_SMEM_GRP    = n2w(NUM_SMEM_GRP),
                WID_DCHE_CL     = n2w(NUM_DCHE_CL),
                WID_DCHE_IDX    = n2w(NUM_DCHE_TAG),
                WID_DCHE_ASO    = n2w(NUM_DCHE_ASO),
                WID_DCHE_STAG   = 2;
///                WID_BURST      = n2w(NUM_BURST_LEN),
///                WID_STBUFL     = n2w(NUM_STBUF_LINE);

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
spu:      | rrf | rrc0 | rrc1 | exs0 | exs1 | exs2 | exs3 | exs4 | swbp |  swb |
spu sr:   | rrf | rrc0 | rrc1 | exs0 | exs1 | exs2 | rsrb | rsr  | wsrb | wsr  |
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
                STAGE_RRF_SXG0    = STAGE_RRF_SEL + 1,              ///5
                STAGE_RRF_SXG     = STAGE_RRF_SEL + LAT_XCHG,       ///6
                STAGE_RRF_DEM     = STAGE_RRF_SEL + 1,              ///5
                STAGE_RRF_DBR     = STAGE_RRF_DEM + 1,              ///5
                STAGE_RRF_DC      = STAGE_RRF_SXG + LAT_L1M,        ///7
                STAGE_RRF_LXG0    = STAGE_RRF_DC + 1,               ///8
                STAGE_RRF_LXG     = STAGE_RRF_DC + LAT_XCHG,        ///9 must > STAGE_RRF_DPRW
                STAGE_RRF_SWBP    = STAGE_RRF_DC + LAT_SWBP,        ///8
                STAGE_RRF_SWB     = STAGE_RRF_SWBP + 1,             ///9
                STAGE_RRF_RSR     = STAGE_RRF_SWBP - 1,             ///7
                STAGE_RRF_WSR     = STAGE_RRF_DPRW,                 ///8
                STAGE_RRF_RSRB    = STAGE_RRF_RSR - 1,              ///6
                STAGE_RRF_WSRB    = STAGE_RRF_WSR - 1,              ///7
                STAGE_RRF_VWBP    = STAGE_RRF_EXE + LAT_VWBP,       ///10
                STAGE_RRF_VWB     = STAGE_RRF_VWBP + 1,             ///11
                STAGE_RRF_VWB_END = STAGE_RRF_VWBP + CYC_VEC,       ///14
                STAGE_EXE         = LAT_MAC - 1,                    ///3
                STAGE_EXE_VWBP    = STAGE_EXE + LAT_VWBP,           ///4
                STAGE_EXE_CMP     = NUM_FU - 1,                     ///2
                STAGE_EXE_SWBP    = STAGE_RRF_SWBP - STAGE_RRF_EXE0,///3
                STAGE_EXE_SWB     = STAGE_EXE_SWBP + 1,             ///4
                STAGE_EEX         = LAT_SFU + CYC_SFU_BUSY - CYC_VEC - 1,     ///27
                STAGE_EEX_VWBP    = STAGE_EEX + LAT_VWBP,           ///28
                STAGE_EEX_VWB     = STAGE_EEX_VWBP + 1,             ///29
                STAGE_ISE         = LAT_ISE - 1,                    ///1
                STAGE_IFE         = LAT_IFE - 1,                    ///1
                STAGE_ISE_EXS1    = LAT_ISE + STAGE_RRF_EXS1,       ///
                STAGE_ISE_EXS2    = LAT_ISE + STAGE_RRF_EXS2,       ///
                STAGE_ISE_EXE     = LAT_ISE + STAGE_RRF_EXE,        ///11
                STAGE_ISE_VWBP    = LAT_ISE + STAGE_RRF_VWBP,       ///12
                STAGE_ISE_SWBP    = LAT_ISE + STAGE_RRF_SWBP,       ///10
                STAGE_ISE_SWB     = LAT_ISE + STAGE_RRF_SWB,        ///11
                STAGE_ISE_CMP     = LAT_ISE + STAGE_RRF_CMP,        ///7
                STAGE_ISE_RRC0    = LAT_ISE + STAGE_RRF_RRC0,
                STAGE_ISE_DC      = LAT_ISE + STAGE_RRF_DC,
                STAGE_ISE_VWB     = LAT_ISE + STAGE_RRF_VWB,        ///13
                STAGE_ISE_VWB_END = LAT_ISE + STAGE_RRF_VWB_END,    ///16
                STAGE_ISE_DEM     = LAT_ISE + STAGE_RRF_DEM,        ///7
                STAGE_ISE_DBR     = LAT_ISE + STAGE_RRF_DBR,        ///6
                STAGE_ISE_CEM     = LAT_ISE + STAGE_RRF_CEM,        ///10
                STAGE_ISE_CBR     = LAT_ISE + STAGE_RRF_CBR,        ///9
                STAGE_ISE_EPS     = LAT_ISE + STAGE_RRF_EPS,        ///
                STAGE_ISE_RSR     = LAT_ISE + STAGE_RRF_RSR,        ///9
                STAGE_ISE_WSR     = LAT_ISE + STAGE_RRF_WSR,        ///10
                STAGE_ISE_DPRW    = LAT_ISE + STAGE_RRF_DPRW;       ///10

parameter uchar CYC_VEXP_DSE      = STAGE_RRF_VWB - STAGE_RRF_DC,
                CYC_BR_DSE        = STAGE_RRF_CBR - STAGE_RRF_DC;
                
parameter uchar CK_STAGE_SFU1     = STAGE_EEX - STAGE_RRF_EXE,      ///19
                CK_STAGE_SFU0     = CK_STAGE_SFU1 - CYC_VEC + 1;    ///16
                 
`ovm_nonblocking_transport_imp_decl(_rfm)
`ovm_nonblocking_transport_imp_decl(_ise)
`ovm_nonblocking_transport_imp_decl(_spu)
`ovm_nonblocking_transport_imp_decl(_spa)
`ovm_nonblocking_transport_imp_decl(_dse)
`ovm_nonblocking_transport_imp_decl(_ife)
`ovm_nonblocking_transport_imp_decl(_tlb)
`ovm_nonblocking_transport_imp_decl(_eif)
  
class tlm_vif_object extends ovm_object;
  `ovm_object_utils(tlm_vif_object)
    
  virtual tlm_sys_if vif;
    
  function new(string name="tlm_vif_object");
    super.new(name);
  endfunction
    
  function virtual tlm_sys_if get_vif();
    return vif;
  endfunction /// virtual
    
  function  void set_vif( virtual tlm_sys_if pins);
    vif = pins;
  endfunction      
    
  function void do_copy (ovm_object rhs);
    tlm_vif_object tmp;

    super.do_copy(rhs);
    $cast(tmp,rhs);
    vif= tmp.vif;
  endfunction /// void
endclass : tlm_vif_object
  
typedef enum uchar {
  selnull, selv[0:127], sels[0:31], selc[0:7], selz,
  selii, selspu, seldse, selfu[0:15]
} rbk_sel_e;
  
parameter rbk_sel_e selv_e = rbk_sel_e'(selv0 + NUM_VRF_BKS * CYC_VEC - 1),
                    sels_e = rbk_sel_e'(sels0 + NUM_SRF_BKS * CYC_VEC - 1),
                    selc_e = rbk_sel_e'(selc0 + NUM_BP_CO - 1);

typedef enum uchar {
  mac, alu, dse, sfu, spu
} unit_typ_e;

typedef enum uchar {
  mac0 = 0, alu0, sfu0, spu0, dse0, fu_null
} unit_inst_e;

parameter unit_typ_e fu_cfg[NUM_FU] = '{
  mac0  : mac, 
  alu0  : alu,
  sfu0  : sfu
};

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
  ts_disabled, ts_rdy, ts_b_pred, ts_w_mrf,
  ts_w_rst, ts_w_tsyn, ts_w_syna, ts_w_synst, ts_w_synld
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

typedef enum uchar {
  gprv_styp,    gprs_styp,    mem_styp,     sr_styp,      pr_styp,
  br_styp,      min_styp
}storage_type_t;

///the MOESI protocol plus a dirty state meaning no cc and modified
typedef enum uchar {
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
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  ///multiply opcodes
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3,
  ///alu opcodes
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_lid,     op_max,     
  op_min,     op_umin,    op_umax,
  op_ext,     op_ins,     op_seb,     op_she,
  op_wsbh,    op_vid,
  ///sfu opcodes
  op_div,     op_udiv,    op_quo,
  op_uquo,    op_res,     op_ures,
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

parameter opcode_e bp_ops[] = '{
  op_cmp,     op_ucmp,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_lid,     op_seb,     op_she,     op_wsbh,
  op_max,     op_min,     op_umin,    op_umax
};

parameter opcode_e sfu_only_ops[] = '{
  op_div,     op_udiv,    op_quo,
  op_uquo,    op_res,     op_ures
};

parameter opcode_e mac_ops[] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3
};

parameter opcode_e alu_ops[] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_umul,    op_smul,    op_umad,    op_smad,    op_umsu,    
  op_smsu,    op_udmul,   op_sdmul,   op_udmad,   op_sdmad,
  op_udmsu,   op_sdmsu,   op_add3,    op_uadd3,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_max,     op_min,     
  op_umin,    op_umax,    op_lid,
  op_ext,     op_ins,     op_seb,     op_she,
  op_wsbh
};

parameter opcode_e sfu_ops[] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,      
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_lid,     op_seb,     op_she,     op_wsbh,
  op_max,     op_min,     op_umin,    op_umax
};

parameter opcode_e dse_ops[] = '{
  op_pera,    op_perb,    op_shf4a,   op_shf4b,
  op_lw,      op_sw,      op_lh,      op_sh,
  op_lb,      op_sb,      op_ll,      op_sc,
  op_cmpxchg, op_fetadd,  op_lhu,     op_lbu,
  op_pref,    op_syna,    op_synci,   op_cache,
  op_tmrf,    op_fmrf
};

parameter opcode_e ld_ops[] = '{
  op_lw,      op_lh,      op_lb,      op_ll,
  op_lhu,     op_lbu
};

parameter opcode_e st_ops[] = '{
  op_sw,      op_sh,      op_sb,      op_sc
};

parameter opcode_e spu_ops[] = '{
  op_gp2s,    op_s2gp,    op_br,      op_fcr,
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_tlbp,    op_tlbr,    op_tlbwi,   op_tlbwr,
  op_mvs
};

parameter opcode_e tlb_ops[] = '{
  op_tlbp,    op_tlbr,    op_tlbwi,   op_tlbwr
};

parameter opcode_e spu_com_ops[] = '{
  op_nop,     op_cmp,     op_ucmp,    op_bp0,
  op_bp1,     op_bp2,     op_bp3,
  op_and,     op_or,      op_xor,     op_nor,
  op_add,     op_uadd,    op_sub,     op_usub,
  op_srl,     op_sra,     op_sll,     op_ror,
  op_clo,     op_clz,     op_ext,     op_ins,
  op_seb,     op_she,     op_wsbh
};

parameter opcode_e ise_zw_ops[] = '{
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_eret
};

typedef enum uchar {
  SR_PROC_CTL,  SR_SUPMSG,    SR_EBASE,     SR_MBASE,       SR_INDEX,
  SR_RANDOM,    SR_ENTRY_L0,  SR_ENTRY_L1,  SR_ENTRY_HI,    SR_TIMER,
  SR_CMP,       SR_OCMC,      SR_PCNT[0:1], SR_PCNTC[0:1],  SR_IIDX,
  SR_IIDY,      SR_IIDZ,      SR_EXPFV,     SR_DSEEV,       SR_MSCO,
  SR_MSCU,      SR_THD_CTL,   SR_THD_ST,    SR_EXEC,        SR_CONTENT,
  SR_EPC,       SR_ERET,      SR_WIDX,      SR_WIDY,        SR_WIDZ,
  SR_ILM,       SR_CM,        SR_UEE,       SR_UER,         SR_ASID,
  SR_MD[0:7],   SR_MCS[0:2],  SR_FFS,       SR_FFC[0:1],    SR_SUPM[0:1]
}special_reg_t;

parameter special_reg_t tlb_sr[] = '{
  SR_INDEX,     SR_RANDOM,    SR_ENTRY_L0,    SR_ENTRY_L1,
  SR_ENTRY_HI,  SR_ASID
};

parameter special_reg_t non_kernel_sr[] = '{
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

parameter uchar INDEX_ENT    = 7 , /// entry bits
                NUM_TLB_E    = 1 << INDEX_ENT,  ///128
                VPN2_WIDTH   = 18,
                TYPE_WIDTH   = 3,  /// Page Size Type bit width
                ASID_WIDTH   = 8,
                IFE_REQ_BUF  = 2,
                VADR_START   = 14,  /// 8K 14BIT START for tlb and dse
                PFN_WIDTH    = 22,
                PADR_WIDTH   = VADR_START + PFN_WIDTH;    ///36

typedef bit[PADR_WIDTH - 1:0]     padr_t;
typedef bit[PADR_WIDTH - WORD_BYTES - WID_SMEM_BK - 1:0] exadr_t;

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
                               
class ip4_printer extends ovm_table_printer;
  virtual function void print_object (string name, ovm_object value, byte scope_separator=".");
    ovm_component comp; ///only print components
    ovm_port_component_base p;
    if($cast(comp, value)) begin
      if(!$cast(p, value)) begin
        super.print_object(name, value, scope_separator);
      end
    end
  endfunction
endclass

`include "ip4_tlm_tr.svh"  
`include "ip4_tlm_inst.svh"
`include "../misc/asm.sv"

`include "ip4_tlm_rfm.sv"
`include "ip4_tlm_spa.sv"
`include "ip4_tlm_spu.sv"
`include "ip4_tlm_ise.sv"
`include "ip4_tlm_tlb.sv"
`include "ip4_tlm_dse.sv"
`include "ip4_tlm_eif.sv"
`include "ip4_tlm_ife.sv"
`include "ip4_tlm_agent.sv"
`include "ip4_tlm_core.sv"

`include "../misc/test_sys.sv"
`include "../misc/test.sv"
`include "../misc/test_algorithm.sv"

endpackage : ip4_tlm_pkg

`endif