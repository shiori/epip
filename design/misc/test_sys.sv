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

/// vsim +OVM_TESTNAME=ip4_sys_test work.top -c -do "transcript off ; run 90ns ; quit -f" > log.txt
/// -novopt
/// cd E:\work\ip4\design\sim
/// vlog -sv ..\tlm\ip4.sv +incdir+..\tlm\ +incdir+d:\questasim_6.6c\verilog_src\ovm-2.1.1\src\

class ip4_sys_test extends ovm_test;
  test_sys_env env;
  `ovm_component_utils_begin(ip4_sys_test)
  `ovm_component_utils_end

  virtual function void build();
    super.build();
    set_config_int("*", "runDelay", 6ns);
    set_config_int("*.sequencer", "count", 200);
///    set_config_int("*", "recording_detail", 1);
    set_config_int("*", "imBase", CFG_START_ADR);
    set_config_int("*", "imSize", 1024);
    set_config_int("*", "dmBase", 0);
    set_config_int("*", "dmSize", 64 * 1024);
    set_config_string("*", "imFilePath", "../misc/fft1.txt"); ///average_filter
    set_config_string("*", "smFilePath", "../misc/sm.txt");
    set_config_string("*", "dmFilePath", "../misc/mem.txt");
    set_config_int("*", "pbId", 2);
   
    set_config_int("*thread0*", "vrfMap[0]", 0);
    set_config_int("*thread0*", "vrfMap[1]", 1);
    set_config_int("*thread0*", "vrfMap[2]", 2);
    set_config_int("*thread0*", "vrfMap[3]", 3);
    set_config_int("*thread0*", "srfMap[0]", 0);
    set_config_int("*thread0*", "srfMap[1]", 1);
    set_config_int("*thread0*", "srExpMsk", 1);

    set_config_int("*thread1*", "vrfMap[0]", 4);
    set_config_int("*thread1*", "vrfMap[1]", 5);
    set_config_int("*thread1*", "vrfMap[2]", 6);
    set_config_int("*thread1*", "vrfMap[3]", 7);
    set_config_int("*thread1*", "srfMap[0]", 2);
    set_config_int("*thread1*", "srfMap[1]", 3);
    set_config_int("*thread1*", "srExpMsk", 1);
    
    set_config_int("*thread2*", "vrfMap[0]", 8);
    set_config_int("*thread2*", "vrfMap[1]", 9);
    set_config_int("*thread2*", "vrfMap[2]", 10);
    set_config_int("*thread2*", "vrfMap[3]", 11);
    set_config_int("*thread2*", "srfMap[0]", 4);
    set_config_int("*thread2*", "srfMap[1]", 5);
    set_config_int("*thread2*", "srExpMsk", 1);
    
    set_config_int("*thread3*", "vrfMap[0]", 12);
    set_config_int("*thread3*", "vrfMap[1]", 13);
    set_config_int("*thread3*", "vrfMap[2]", 14);
    set_config_int("*thread3*", "vrfMap[3]", 15);
    set_config_int("*thread3*", "srfMap[0]", 6);
    set_config_int("*thread3*", "srfMap[1]", 7);
    set_config_int("*thread3*", "srExpMsk", 1);
    
///    set_config_int("*thread0*", "threadState", ts_disabled);
    
    set_config_int("*thread1*", "threadState", ts_rdy);
    set_config_int("*thread2*", "threadState", ts_rdy);
    set_config_int("*thread3*", "threadState", ts_rdy);
    
    set_config_int("*thread1*", "privMode", priv_kernel);
    set_config_int("*thread2*", "privMode", priv_kernel);
    set_config_int("*thread3*", "privMode", priv_kernel);
        
    set_config_int("*tlb*", "vpn2[0]", 0);
    set_config_int("*tlb*", "pageTyp[0]", page_64K);
    set_config_int("*tlb*", "pfn2e[0]", (SMEM_OFFSET + 2 * SMEM_SIZE) >> VADR_START);
    set_config_int("*tlb*", "v[0]", 2'b01);
    set_config_int("*tlb*", "d[0]", 2'b01);
    
    env = new("env", this);
  endfunction

///  virtual function void connect();
///    super.connect();
///  endfunction

  virtual function void end_of_elaboration();
    set_report_verbosity_level_hier(OVM_LOW); ///OVM_FULL OVM_MEDIUM OVM_HIGH OVM_LOW
///    env.core.ise.set_report_verbosity_level_hier(OVM_HIGH);    
  endfunction
    
  virtual task run();
    $display($psprintf("size %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d %0d",
              $bits(i_r1w1), $bits(i_r3w1), $bits(i_r2w1), $bits(i_fcr), $bits(i_b), 
              $bits(i_load), $bits(i_store), $bits(i_mctl), $bits(i_fetadd), $bits(i_cmpxchg), 
              $bits(i_mrfa), $bits(i_cmsg), $bits(i_cmp), $bits(i_cmpi), $bits(i_cop),
              $bits(i_vxchg), $bits(i_i26)));
///    #2761ns;
///    set_report_verbosity_level_hier(OVM_FULL);
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