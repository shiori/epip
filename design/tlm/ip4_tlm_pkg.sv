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
import ip4_rtl_pkg::*;

parameter VERSION = 0.1;

parameter time CLK_P              = 2ns;

parameter uchar   LAT_EXM           = 4;  ///external memory latency

                
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

`ovm_nonblocking_transport_imp_decl(_rfm)
`ovm_nonblocking_transport_imp_decl(_ise)
`ovm_nonblocking_transport_imp_decl(_spu)
`ovm_nonblocking_transport_imp_decl(_spa)
`ovm_nonblocking_transport_imp_decl(_dse)
`ovm_nonblocking_transport_imp_decl(_ife)
`ovm_nonblocking_transport_imp_decl(_tlb)
`ovm_nonblocking_transport_imp_decl(_eif)

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