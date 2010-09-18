///`include "ip4_tlm.svh"

class test_sys_env extends ovm_env;
  ip4_tlm_core core;
  
  virtual tlm_sys_if.mods sysif;

  `ovm_component_utils_begin(test_sys_env)
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

/// vsim +OVM_TESTNAME=ip4_sys_test work.top -c -do "run 90ns ; quit -f" > ..\tlm\log.txt
/// -novopt
/// cd E:\work\ip4\design\sim
/// vlog -sv ..\tlm\ip4.sv +incdir+..\tlm\ +incdir+d:\questasim_6.6b\verilog_src\ovm-2.1.1\src\

class ip4_sys_test extends ovm_test;
  test_sys_env env;
  `ovm_component_utils_begin(ip4_sys_test)
  `ovm_component_utils_end

  virtual function void build();
    super.build();
    set_config_int("*", "runDelay", 6ns);
    set_config_int("*.sequencer", "count", 200);
    set_config_int("*", "recording_detail", 1);
    set_config_int("*", "imBase", CFG_START_ADR);
    set_config_int("*", "imSize", 1024);
    set_config_string("*", "imFilePath", "../misc/code.txt");
    set_config_int("*", "pbId", 2);
   
    set_config_int("*thread0*", "vrfMap[0]", 0);
    set_config_int("*thread0*", "vrfMap[1]", 1);
    set_config_int("*thread0*", "vrfMap[2]", 2);
    set_config_int("*thread0*", "vrfMap[3]", 3);
    
    set_config_int("*thread0*", "srfMap[0]", 0);
    set_config_int("*thread0*", "srfMap[1]", 1);
    
    set_config_int("*tlb*", "vpn2[0]", 0);
    set_config_int("*tlb*", "pageTyp[0]", page_64K);
///    set_config_int("*tlb*", "pfn2e[0]", 0);
    set_config_int("*tlb*", "pfn2e[0]", (SMEM_OFFSET + 2 * SMEM_SIZE) >> VADR_START);
///    set_config_int("*tlb*", "pfn2o[0]", (SMEM_OFFSET + 2 * SMEM_SIZE) >> VADR_START);
    set_config_int("*tlb*", "v[0]", 2'b01);
    set_config_int("*tlb*", "d[0]", 2'b01);
    
    env = new("env", this);
  endfunction

///  virtual function void connect();
///    super.connect();
///  endfunction
  
  virtual task run();
    set_report_verbosity_level_hier(OVM_HIGH);
  endtask

///  function void start_of_simulation();
///    set_report_id_action_hier("CFGOVR", OVM_DISPLAY);
///    set_report_id_action_hier("CFGSET", OVM_DISPLAY);
///    check_config_usage();
///  endfunction
    
  function new(string name = "test_sys", ovm_component parent);
    super.new(name, parent);
  endfunction : new  
endclass