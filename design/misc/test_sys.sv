///`include "ip4_tlm.svh"

class test_sys_env extends ovm_env;
  ip4_tlm_rfm rfm;  
  ip4_tlm_spa spa;
  ip4_tlm_spu spu;
  ip4_tlm_ise ise;
  ip4_tlm_ife ife;
  ip4_tlm_tlb tlb;
  ip4_tlm_dse dse;
  ip4_tlm_shm shm;
 
  virtual tlm_sys_if.mods sysif;

  `ovm_component_utils_begin(test_sys_env)
  `ovm_component_utils_end
  
  virtual function void build();
    super.build();
     
    rfm = ip4_tlm_rfm::type_id::create("rfm", this);
    spa = ip4_tlm_spa::type_id::create("spa", this);
    spu = ip4_tlm_spu::type_id::create("spu", this);
    ise = ip4_tlm_ise::type_id::create("ise", this);
    ife = ip4_tlm_ife::type_id::create("ife", this);
    tlb = ip4_tlm_tlb::type_id::create("tlb", this);
    dse = ip4_tlm_dse::type_id::create("dse", this);
    shm = ip4_tlm_shm::type_id::create("shm", this);
  endfunction

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

    dse.shm_tr_port.connect(shm.dse_tr_imp);
    shm.dse_tr_port.connect(dse.shm_tr_imp);
  endfunction

  virtual task run();
  endtask

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass

/// vsim +OVM_TESTNAME=ip4_sys_test work.top

class ip4_sys_test extends ovm_test;
  test_sys_env env;
  `ovm_component_utils_begin(ip4_sys_test)
  `ovm_component_utils_end

  virtual function void build();
    set_config_int("*", "run_delay", 6ns);
    set_config_int("*.sequencer", "count", 200);
    set_config_int("*", "recording_detail", 1);
    set_config_int("*", "im_base", cfg_start_adr);
    set_config_int("*", "im_size", 1024);
    set_config_string("*", "im_file_path", "../misc/code.txt");
    
    env = new("env", this);
    super.build();
  endfunction

///  virtual function void connect();
///    super.connect();
///  endfunction
  
  virtual task run();
    set_report_verbosity_level_hier(OVM_HIGH);
  endtask
    
  function new(string name = "test_sys", ovm_component parent);
    super.new(name, parent);
  endfunction : new  
endclass