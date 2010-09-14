/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_core.sv
/// Title            : ip4 external interface
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

class ip4_tlm_core_vars extends ovm_component;

  `ovm_component_utils_begin(ip4_tlm_core_vars)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_core_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_core extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_core_vars v, vn;  
  local uchar pbId;
  
  `ovm_component_utils_begin(ip4_tlm_core)
    `ovm_field_int(pbId, OVM_ALL_ON)
  `ovm_component_utils_end
///      
///  ovm_nonblocking_transport_imp_dse #(tr_dse2core, tr_dse2core, ip4_tlm_core) dse_tr_imp;
///  ovm_nonblocking_transport_imp_ise #(tr_ise2core, tr_ise2core, ip4_tlm_core) ise_tr_imp;
///  ovm_nonblocking_transport_imp_spu #(tr_spu2core, tr_spu2core, ip4_tlm_core) spu_tr_imp;
///  
///  ovm_nonblocking_transport_port #(tr_core2dse, tr_core2dse) dse_tr_port;
///  ovm_nonblocking_transport_port #(tr_core2ise, tr_core2ise) ise_tr_port;
///  ovm_nonblocking_transport_port #(tr_core2spu, tr_core2spu) spu_tr_port;
///  
  function void comb_proc();
    
    ovm_report_info("core", "comb_proc procing...", OVM_FULL); 
  endfunction
  
  function void req_proc();
    
    ovm_report_info("core", "req_proc procing...", OVM_FULL); 
    
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time == stamp) begin
       ovm_report_info("sync", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("sync", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
    ///--------------------synchronizing-------------------
    v.copy(vn);
    comb_proc();
  endfunction : sync

  task run();
    forever begin
      @(posedge sysif.clk);
      sync();
      req_proc();
    end
  endtask : run

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
    
  virtual function void build();
    ovm_object tmp;
    tlm_vif_object vifCfg;
    
    super.build();

    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_core

///-------------------------------------other functions-----------------------------------------
  
