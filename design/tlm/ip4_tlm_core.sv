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
  ip4_tlm_rfm rfm;  
  ip4_tlm_spa spa;
  ip4_tlm_spu spu;
  ip4_tlm_ise ise;
  ip4_tlm_ife ife;
  ip4_tlm_tlb tlb;
  ip4_tlm_dse dse;
  ip4_tlm_eif eif;
  
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

    rfm = ip4_tlm_rfm::type_id::create("rfm", this);
    spa = ip4_tlm_spa::type_id::create("spa", this);
    spu = ip4_tlm_spu::type_id::create("spu", this);
    ise = ip4_tlm_ise::type_id::create("ise", this);
    ife = ip4_tlm_ife::type_id::create("ife", this);
    tlb = ip4_tlm_tlb::type_id::create("tlb", this);
    dse = ip4_tlm_dse::type_id::create("dse", this);
    eif = ip4_tlm_eif::type_id::create("eif", this);
    
    set_config_int("*thread0*", "threadState", ts_rdy);
    set_config_int("*thread0*", "privMode", priv_kernel);
  endfunction : build

  virtual function void connect();
    super.connect();
    rfm.spa_tr_port.connect(spa.rfm_tr_imp);
    spa.rfm_tr_port.connect(rfm.spa_tr_imp);
    
    rfm.spu_tr_port.connect(spu.rfm_tr_imp);
    spu.rfm_tr_port.connect(rfm.spu_tr_imp);
    
///    rfm.ise_tr_port.connect(ise.rfm_tr_imp);
    ise.rfm_tr_port.connect(rfm.ise_tr_imp);
    
    rfm.dse_tr_port.connect(dse.rfm_tr_imp);
    dse.rfm_tr_port.connect(rfm.dse_tr_imp);
    
    ise.spa_tr_port.connect(spa.ise_tr_imp);
    spa.ise_tr_port.connect(ise.spa_tr_imp);

    spu.spa_tr_port.connect(spa.spu_tr_imp);
    spa.spu_tr_port.connect(spu.spa_tr_imp);    

    dse.spa_tr_port.connect(spa.dse_tr_imp);
    spa.dse_tr_port.connect(dse.spa_tr_imp); 
    
    ise.spu_tr_port.connect(spu.ise_tr_imp);
    spu.ise_tr_port.connect(ise.spu_tr_imp);

    dse.spu_tr_port.connect(spu.dse_tr_imp);
    spu.dse_tr_port.connect(dse.spu_tr_imp);

    ise.ife_tr_port.connect(ife.ise_tr_imp);
    ife.ise_tr_port.connect(ise.ife_tr_imp);

    tlb.ife_tr_port.connect(ife.tlb_tr_imp);
    ife.tlb_tr_port.connect(tlb.ife_tr_imp);

    tlb.spu_tr_port.connect(spu.tlb_tr_imp);
    spu.tlb_tr_port.connect(tlb.spu_tr_imp);
    
    dse.ise_tr_port.connect(ise.dse_tr_imp);
    ise.dse_tr_port.connect(dse.ise_tr_imp);

    dse.tlb_tr_port.connect(tlb.dse_tr_imp);
    tlb.dse_tr_port.connect(dse.tlb_tr_imp);

    dse.eif_tr_port.connect(eif.dse_tr_imp);
    eif.dse_tr_port.connect(dse.eif_tr_imp);

    ise.eif_tr_port.connect(eif.ise_tr_imp);
    eif.ise_tr_port.connect(ise.eif_tr_imp);
    
    spu.eif_tr_port.connect(eif.spu_tr_imp);
    eif.spu_tr_port.connect(spu.eif_tr_imp);
  endfunction
endclass : ip4_tlm_core

///-------------------------------------other functions-----------------------------------------
  
