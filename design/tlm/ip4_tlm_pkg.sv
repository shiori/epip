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
ovm_verbosity tr_verb = OVM_LOW;

typedef byte unsigned       uchar;
typedef shortint unsigned   ushort;
typedef int unsigned        uint;
typedef longint unsigned    ulong;

parameter time clk_p              = 2ns;
parameter uchar word_width        = 32;
parameter uchar num_word_bytes    = word_width / 8;

parameter uchar num_sp            = 8,
                num_vec           = 32,
                num_sfu           = 2,
                num_thread        = 4,
                num_fu            = 3,
                num_fu_rp         = 4,
///                num_fu_wp         = 2,
                num_phy_vrf_grp   = 64,
                num_phy_srf_grp   = 32,
                num_prf_p_grp   = 8,
                num_vrf_bks       = 4,
                num_srf_bks       = 2,
                num_bp_imm        = 1,
                num_pr            = 7,
                num_ifet_bytes    = 16,
                num_inst_vrf      = 32,
                num_inst_srf      = 16;
                
parameter uchar lat_mac           = 4,
                lat_sfu           = 16,
                lat_rf            = 1,
                lat_rbp           = 1,
                lat_vwbp          = 1,    ///vector writeback bypass time
                lat_wb            = 4,
                lat_ise           = 2,
                lat_dse           = 4,
                lat_dwbp          = 1;    ///dse writeback bypass time

parameter uint cfg_start_adr      = 'hf000_0000;

parameter uchar cyc_vec       = num_vec/num_sp,     ///4
                cyc_sfu_busy  = num_vec/num_sfu,    ///16 
                cyc_iss_sfu   = lat_rf + lat_rbp + cyc_vec -1 + lat_sfu + cyc_sfu_busy + lat_vwbp,
                cyc_iss_spu   = lat_rf + lat_rbp + lat_dse + lat_dwbp,
                cyc_iss_dse   = cyc_iss_spu,
                cyc_iss_vec   = lat_rf + lat_rbp + cyc_vec -1 + lat_mac + lat_dwbp;

/*
                                           pipeline stages:
ise,ife:      | if0 | if1 | ii0 | ii1 | rrf |

                                           pipeline stages:
dse:    | rrf | rrc0 |  ag  |  tag |  sel |  dc  | dwbp |  dwb |
spu:    | rrf | rrc0 | exs0 | exs1 | exs2 | exs3 | swbp |  swb |
exe:    | rrf | rrc0 | rrc1 | rrc2 | rrc3 | exe0 | vsbp | vswb |
exe:    | rrf | rrc0 | rrc1 | rrc2 | rrc3 | exe0 | exe1 | exe2 | exe3 | vwbp | vwb0 | vwb1 | vwb2 | vwb3 |
        0     1      2      3      4      5      6      7      8      9      10     11     12     13     14
                                          0      1      2      3      4      5      6      7      8      9
  */  
  
parameter uchar stage_rrf_rrc0    = lat_rf + lat_rbp - 1,           ///1
                stage_rrf_rrc1    = stage_rrf_rrc0 + 1,             ///2
                stage_rrf_rrc     = stage_rrf_rrc0 + cyc_vec - 1,   ///4
                stage_rrf_exe0    = stage_rrf_rrc + 1,              ///5
                stage_rrf_exe     = stage_rrf_rrc + lat_mac,        ///8
                stage_rrf_vwbp    = stage_rrf_exe + lat_vwbp,       ///9
                stage_rrf_swbp    = stage_rrf_rrc0 + lat_dse + lat_dwbp,      ///6
                stage_rrf_swb     = stage_rrf_swbp + 1,             ///7
                stage_rrf_vwb0    = stage_rrf_vwbp + 1,             ///10
                stage_exe         = lat_mac - 1,                    ///3
                stage_exe_vwbp    = stage_exe + lat_vwbp,           ///4
                stage_exe_vwb0    = stage_exe_vwbp + 1,             ///5
                stage_exe_dwbp    = lat_dse - cyc_vec + lat_dwbp,   ///1
                stage_exe_dwb     = stage_exe_dwbp + 1,             ///2
                stage_eex         = lat_sfu + cyc_sfu_busy -cyc_vec - 1,     ///27
                stage_eex_vwbp    = stage_eex + lat_vwbp,           ///28
                stage_eex_vwb0    = stage_eex_vwbp + 1,             ///29
                stage_rrf_dwbp    = stage_rrf_rrc + stage_exe_dwbp + 1,    ///6
                stage_rrf_dwb     = stage_rrf_dwbp + 1,             ///7
                stage_ise         = lat_ise - 1,                    ///1
                stage_ise_rrf     = stage_ise + 1;                  ///2

parameter uchar ck_stage_sfu1     = stage_eex - stage_rrf_exe,      ///19
                ck_stage_sfu0     = ck_stage_sfu1 - cyc_vec + 1;    ///16
                 
typedef bit[word_width-1:0]     word;
    
///Basic functions for parameters etc
  
function automatic ulong clogb2(
  input ulong value
);
  ulong v;
  clogb2 = 0;
  for (v = value; v > 0; clogb2 = clogb2 + 1)
    v = v >> 1;
endfunction
  
function automatic ulong max2(
  input ulong a0, a1
);
  max2 = a0;
  if (a0 < a1)
    max2 = a1;
endfunction

parameter uchar bits_vrf_bks    = clogb2(num_vrf_bks),
                bits_srf_bks    = clogb2(num_srf_bks),
                bits_tid        = clogb2(num_thread),
                bits_ifet       = clogb2(num_ifet_bytes),
                bits_prf_p_grp  = clogb2(num_prf_p_grp);

`ovm_nonblocking_transport_imp_decl(_rfm)
`ovm_nonblocking_transport_imp_decl(_ise)
`ovm_nonblocking_transport_imp_decl(_spu)
`ovm_nonblocking_transport_imp_decl(_spa)
`ovm_nonblocking_transport_imp_decl(_dse)
`ovm_nonblocking_transport_imp_decl(_ife)
  
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
  
parameter rbk_sel_e selv_e = rbk_sel_e'(selv0 + num_vrf_bks - 1),
                    sels_e = rbk_sel_e'(sels0 + num_srf_bks - 1),
                    seli_e = rbk_sel_e'(seli0 + num_bp_imm - 1);

typedef enum uchar {
  mac, alu, dse, sfu, spu
} unit_typ_e;

typedef enum uchar {
  mac0 = 0, alu0, sfu0, spu0, dse0, fu_null
} unit_inst_e;

parameter unit_typ_e fu_cfg[num_fu] = '{
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
  ts_w_b,         ts_w_pip
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
  op_tlbwr
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

parameter opcode_e mc_ops[] = '{
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
///  op_gglw,    op_gglb,    op_gglh,    op_ggsw,
///  op_ggsh,    op_ggsb,
///  op_vror,    op_vroru,   op_vsr,     op_vsru,
///  op_vsl,     op_vslu  
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
  op_tlbwr
};

parameter opcode_e tlb_ops[] = '{
  op_tlbp,    op_tlbr,    op_tlbwi,
  op_tlbwr,   op_gp2s,    op_s2gp
};

parameter opcode_e spu_possible_ops[] = '{
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

`include "ip4_tlm_tr.svh"  
`include "ip4_tlm_inst.svh"
`include "ip4_tlm_rfm.sv"
`include "ip4_tlm_spa.sv"
`include "ip4_tlm_spu.sv"
`include "ip4_tlm_ise.sv"
`include "ip4_tlm_agent.sv"

endpackage : ip4_tlm_pkg

`endif