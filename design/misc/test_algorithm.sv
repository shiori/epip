class test_alg_env extends ovm_env;
  ip4_tlm_core core;
  
  virtual tlm_sys_if.mods sysif;

  `ovm_component_utils_begin(test_alg_env)
  `ovm_component_utils_end
  
  virtual function void build();
    super.build();
    core = ip4_tlm_core::type_id::create("core", this);
  endfunction

  virtual function void connect();
    super.connect();
  endfunction

  virtual task run();
  endtask

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass

/// vsim +OVM_TESTNAME=ip4_alg_test work.top
/// -novopt

class ip4_alg_test extends ovm_test;
  test_alg_env env;
  `ovm_component_utils_begin(ip4_alg_test)
  `ovm_component_utils_end

  virtual function void build();
    super.build();
    set_config_int("*", "runDelay", 6ns);
    set_config_int("*.sequencer", "count", 200);
    set_config_int("*", "recording_detail", 1);
    set_config_int("*", "imBase", CFG_START_ADR);
    set_config_int("*", "imSize", 1024);
    set_config_string("*", "imFilePath", "../misc/average_filter.txt");
    set_config_string("*", "smFilePath", "../misc/sm.txt");
    set_config_int("*", "pbId", 2);
   
    set_config_int("*thread0*", "vrfMap[0]", 0);
    set_config_int("*thread0*", "vrfMap[1]", 1);
    set_config_int("*thread0*", "vrfMap[2]", 2);
    set_config_int("*thread0*", "vrfMap[3]", 3);
    
    set_config_int("*thread0*", "srfMap[0]", 0);
    set_config_int("*thread0*", "srfMap[1]", 1);
    
    set_config_int("*thread0*", "srExpMsk", 1);
    
    set_config_int("*tlb*", "vpn2[0]", 0);
    set_config_int("*tlb*", "pageTyp[0]", page_64K);
    set_config_int("*tlb*", "pfn2e[0]", (SMEM_OFFSET + 2 * SMEM_SIZE) >> VADR_START);
    set_config_int("*tlb*", "v[0]", 2'b01);
    set_config_int("*tlb*", "d[0]", 2'b01);
       
    env = new("env", this);
  endfunction

  virtual task run();
    set_report_verbosity_level_hier(OVM_MEDIUM);
  endtask
    
  function new(string name = "test_sys", ovm_component parent);
    super.new(name, parent);
  endfunction : new  
endclass