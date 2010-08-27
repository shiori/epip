/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_eif.sv
/// Title            : ip4 external interface
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

class ip4_tlm_eif_vars extends ovm_component;
  tr_dse2eif fmDSE;
  
  `ovm_component_utils_begin(ip4_tlm_eif_vars)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_eif_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_eif extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_eif_vars v, vn;  
      
  `ovm_component_utils_begin(ip4_tlm_eif)
    
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_dse #(tr_dse2eif, tr_dse2eif, ip4_tlm_eif) dse_tr_imp;
  ovm_nonblocking_transport_imp_ise #(tr_ise2eif, tr_ise2eif, ip4_tlm_eif) ise_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_eif2dse, tr_eif2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_eif2ise, tr_eif2ise) ise_tr_port;
  
  function void comb_proc();
    
    ovm_report_info("EIF", "comb_proc procing...", OVM_FULL); 
    
    if(v.fmDSE != null) end_tr(v.fmDSE);
    vn.fmDSE = null;
    
  endfunction
  
  function void req_proc();
   
    ovm_report_info("EIF", "req_proc procing...", OVM_FULL); 
  endfunction

///------------------------------nb_transport functions---------------------------------------
 

  function bit nb_transport_dse(input tr_dse2eif req, output tr_dse2eif rsp);
    ovm_report_info("eif_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_ise(input tr_ise2eif req, output tr_ise2eif rsp);
    ovm_report_info("eif_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    return 1;
  endfunction : nb_transport_ise
            
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
    dse_tr_imp = new("dse_tr_imp", this);
    ise_tr_imp = new("ise_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_eif

///-------------------------------------other functions-----------------------------------------
  
