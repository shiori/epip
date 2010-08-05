/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_shm.sv
/// Title            : ip4 shared memory
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

class ip4_tlm_shm_vars extends ovm_component;
  tr_dse2shm fm_dse;
  
  `ovm_component_utils_begin(ip4_tlm_shm_vars)
    `ovm_field_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_shm_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_shm extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_shm_vars v, vn;  
      
  `ovm_component_utils_begin(ip4_tlm_shm)
    
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_dse #(tr_dse2shm, tr_dse2shm, ip4_tlm_shm) dse_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_shm2dse, tr_shm2dse) dse_tr_port;
        
  function void comb_proc();
    
    ovm_report_info("SHM", "comb_proc procing...", OVM_FULL); 
    
    if(v.fm_dse != null) end_tr(v.fm_dse);
    vn.fm_dse = null;
    
  endfunction
  
  function void req_proc();
   
    ovm_report_info("SHM", "req_proc procing...", OVM_FULL); 
  endfunction

///------------------------------nb_transport functions---------------------------------------
 

  function bit nb_transport_dse(input tr_dse2shm req, output tr_dse2shm rsp);
    ovm_report_info("SHM_TR", $psprintf("Get DSE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse = req;
    return 1;
  endfunction : nb_transport_dse
          
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time == stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
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
    tlm_vif_object vif_cfg;
    
    super.build();
    dse_tr_imp = new("dse_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
   
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_shm

///-------------------------------------other functions-----------------------------------------
  
