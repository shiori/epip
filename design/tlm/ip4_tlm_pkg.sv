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

parameter time CLK_P              = 2ns;
parameter uchar WORD_WIDTH        = 32;
parameter uchar NUM_WORD_BYTES    = WORD_WIDTH / 8;

parameter uchar NUM_SP            = 8,
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
                NUM_BP_IMM        = 1,
                NUM_PR            = 7,
                NUM_IFET_BYTES    = 16,
                NUM_INST_VRF      = 32,
                NUM_INST_SRF      = 16,
                NUM_SMEM_BK       = NUM_SP,   /// register file bank number, default equal to NUM_SP
                NUM_W_CNT         = 2;
                
parameter uchar LAT_MAC           = 5,
                LAT_SFU           = 16,
                LAT_RF            = 1,
                LAT_RBP           = 1,
                LAT_VWBP          = 1,    ///vector writeback bypass time
                LAT_WB            = 4,
                LAT_ISE           = 2,
                LAT_IFE           = 2,
                LAT_DSE           = 4,
                LAT_DWBP          = 1;    ///dse writeback bypass time

parameter uint CFG_START_ADR      = 'hf000_0000;

parameter uchar CYC_VEC       = NUM_VEC/NUM_SP,     ///4
                CYC_SFU_BUSY  = NUM_VEC/NUM_SFU,    ///16 
                CYC_ISS_SFU   = LAT_RF + LAT_RBP + CYC_VEC -1 + LAT_SFU + CYC_SFU_BUSY + LAT_VWBP,
                CYC_ISS_SPU   = LAT_RF + LAT_RBP + LAT_DSE + LAT_DWBP,
                CYC_ISS_DSE   = CYC_ISS_SPU,
                CYC_ISS_VEC   = LAT_RF + LAT_RBP + CYC_VEC -1 + LAT_MAC + LAT_DWBP;


/*
                                           pipeline stages:
ise,ife:      | ife0 | ife1 | ise0 | ise1 | rrf |

                                           pipeline stages:
load:     | rrf | rrc0 |  ag  |  tag |  ad0 | ad1  | dc   | lxg0 | lxg1 | 
store:    | rrf | rrc0 |  ag  |  tag | sxg0 | sxg1 | dc   |
dse emsk: | rrf | rrc0 |  ag  |  tag |  sel | dem0 | dem1 | dem2 | dem3 |
spu:      | rrf | rrc0 | rrc1 | exs0 | exs1 | exs2 | exs3 | swbp |  swb |
exe:      | rrf | rrc0 | rrc1 | rrc2 | rrc3 | exe0 | exe1 | exe2 | exe3 | exe4 | vwbp | vwb0 | vwb1 | vwb2 | vwb3 |
cmp/fcmp: | rrf | rrc0 | rrc1 | rrc2 | rrc3 | cmp0 | cmp1 | cmp2 | cem0 | cem1 | cem2 | cem3 |
          0     1      2      3      4      5      6      7      8      9      10     11     12     13     14     15
                                            0      1      2      3      4      5      6      7      8      9      10
                       0      1      2      3      4      5      6    
  */  
  
parameter uchar STAGE_RRF_RRC0    = LAT_RF + LAT_RBP - 1,           ///1
                STAGE_RRF_EXS0    = STAGE_RRF_RRC0 + 2,             ///3
                STAGE_RRF_EXS1    = STAGE_RRF_EXS0 + 1,             ///4
                STAGE_RRF_RRC     = STAGE_RRF_RRC0 + CYC_VEC - 1,   ///4
                STAGE_RRF_EXE0    = STAGE_RRF_RRC + 1,              ///5
                STAGE_RRF_EXE     = STAGE_RRF_RRC + LAT_MAC,        ///8
                STAGE_RRF_CMP     = STAGE_RRF_RRC + NUM_FU,         ///7
                STAGE_RRF_CEM0    = STAGE_RRF_CMP + 1,              ///8
                STAGE_RRF_DEM0    = STAGE_RRF_RRC0 + LAT_DSE,       ///5
                STAGE_RRF_VWBP    = STAGE_RRF_EXE + LAT_VWBP,       ///9
                STAGE_RRF_SWBP    = STAGE_RRF_RRC0 + LAT_DSE + LAT_DWBP,      ///6
                STAGE_RRF_SWB     = STAGE_RRF_SWBP + 1,             ///7
                STAGE_RRF_VWB0    = STAGE_RRF_VWBP + 1,             ///10
                STAGE_EXE         = LAT_MAC - 1,                    ///3
                STAGE_EXE_VWBP    = STAGE_EXE + LAT_VWBP,           ///4
                STAGE_EXE_VWB0    = STAGE_EXE_VWBP + 1,             ///5
                STAGE_EXE_CMP     = NUM_FU - 1,                     ///2
                STAGE_EXE_SWBP    = LAT_DSE - CYC_VEC + LAT_DWBP,   ///1
                STAGE_EXE_SWB     = STAGE_EXE_SWBP + 1,             ///2
                STAGE_EEX         = LAT_SFU + CYC_SFU_BUSY -CYC_VEC - 1,     ///27
                STAGE_EEX_VWBP    = STAGE_EEX + LAT_VWBP,           ///28
                STAGE_EEX_VWB0    = STAGE_EEX_VWBP + 1,             ///29
                STAGE_ISE         = LAT_ISE - 1,                    ///1
                STAGE_IFE         = LAT_IFE - 1,                    ///1
                STAGE_AG_SWB      = LAT_DSE + LAT_DWBP ,            ///5
                STAGE_RRF_AG      = STAGE_RRF_RRC0 + LAT_RF,        ///2
                STAGE_RRF_TAG     = STAGE_RRF_AG + 1,               ///3
                STAGE_RRF_SEL     = STAGE_RRF_TAG + 1,              ///4
                STAGE_ISE_VWB     = LAT_ISE + LAT_RF + CYC_VEC + LAT_MAC + LAT_VWBP + CYC_VEC - 1,  ///15
                STAGE_ISE_VWBP    = LAT_ISE + LAT_RF + CYC_VEC + LAT_MAC + LAT_VWBP - 1,            ///11
                STAGE_ISE_DC      = LAT_ISE + LAT_RF + LAT_DSE;     ///7
                                

parameter uchar CK_STAGE_SFU1     = STAGE_EEX - STAGE_RRF_EXE,      ///19
                CK_STAGE_SFU0     = CK_STAGE_SFU1 - CYC_VEC + 1;    ///16
                 
typedef bit[WORD_WIDTH-1:0]     word;
    
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

parameter uchar BITS_VRF_BKS    = n2w(NUM_VRF_BKS),
                BITS_SRF_BKS    = n2w(NUM_SRF_BKS),
                bits_tid        = n2w(NUM_THREAD),
                BITS_IFET       = n2w(NUM_IFET_BYTES),
                BITS_PRF_P_GRP  = n2w(NUM_PRF_P_GRP);

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
  selv[0:127], sels[0:31], seli[0:7], selz, selii, selspu, seldse, selfu[0:15], selnull
} rbk_sel_e;
  
parameter rbk_sel_e selv_e = rbk_sel_e'(selv0 + NUM_VRF_BKS - 1),
                    sels_e = rbk_sel_e'(sels0 + NUM_SRF_BKS - 1),
                    seli_e = rbk_sel_e'(seli0 + NUM_BP_IMM - 1);

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
  sop_nop,      sop_pop2n,    sop_store
} msc_opcode_e;

typedef enum uchar {
  mop_nop,      mop_bc,     mop_rstor,    mop_loop,
  mop_else,     mop_cont,   mop_if,       mop_brk
} msk_opcode_e;

typedef enum bit {
  bop_az,     bop_naz
} br_opcode_e;

typedef enum uchar {
  ts_disabled,    ts_rdy,     ts_w_ls,    ts_w_msg,
  ts_w_b
}ise_thread_state;

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
  op_wsbh,
///  op_gglw,    op_gglb,    op_gglh,    op_ggsw,
///  op_ggsh,    op_ggsb,
///  op_vror,    op_vroru,   op_vsr,     op_vsru,
///  op_vsl,     op_vslu,
  ///sfu opcodes
  op_div,     op_udiv,    op_quo,
  op_uquo,    op_res,     op_ures,
  ///dse opcodes
  op_pera,    op_perb,    op_shf4,
  op_lw,      op_sw,      op_lh,      op_sh,
  op_lb,      op_sb,      op_ll,      op_sc,
  op_cmpxchg, op_fetadd,  op_lhu,     op_lbu,
  op_pref,    op_sync,    op_synci,   op_cache,
  op_smsg,    op_rmsg,
  ///spu opcodes
  op_gp2s,    op_s2gp,    op_br,      op_fcr,
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_pint,    op_tlbp,    op_tlbr,    op_tlbwi,
  op_tlbwr,   op_mvs
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

parameter opcode_e spu_only_ops[] = '{
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
///op_nop,   op_bp0,  op_bp1,     op_bp2,     op_bp3,      
  op_pera,    op_perb,    op_shf4, 
  op_lw,      op_sw,      op_lh,      op_sh,
  op_lb,      op_sb,      op_ll,      op_sc,
  op_cmpxchg, op_fetadd,  op_lhu,     op_lbu,
  op_pref,    op_sync,    op_synci,   op_cache,
  op_smsg,    op_rmsg
};

parameter opcode_e spu_ops[] = '{
  op_gp2s,    op_s2gp,    op_br,      op_fcr,
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_pint,    op_tlbp,    op_tlbr,    op_tlbwi,
  op_tlbwr,   op_mvs
};

parameter opcode_e tlb_ops[] = '{
  op_tlbp,    op_tlbr,    op_tlbwi,
  op_tlbwr,   op_gp2s,    op_s2gp
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

parameter opcode_e ise_ops[] = '{
  op_sys,     op_eret,    op_wait,    op_exit,
  op_brk,     op_tsync,   op_msync,   op_alloc,
  op_pint
};

class ip4_printer extends ovm_table_printer;
  virtual function void print_object (string name, ovm_object value, byte scope_separator=".");
    ovm_component comp; //only print components
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
`include "ip4_tlm_rfm.sv"
`include "ip4_tlm_spa.sv"
`include "ip4_tlm_spu.sv"
`include "ip4_tlm_ise.sv"
`include "ip4_tlm_tlb.sv"
`include "ip4_tlm_dse.sv"
`include "ip4_tlm_eif.sv"
`include "ip4_tlm_ife.sv"
`include "ip4_tlm_agent.sv"

`include "../misc/test_sys.sv"
`include "../misc/test.sv"

endpackage : ip4_tlm_pkg

`endif