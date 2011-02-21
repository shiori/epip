///`include "ip4_tlm.svh"

class test_direct_env extends ovm_env;
  
  virtual tlm_sys_if.mods sysif;

  `ovm_component_utils_begin(test_direct_env)
  `ovm_component_utils_end
  
  virtual function void build();
    super.build();
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

/// vsim +OVM_TESTNAME=ip4_direct_test work.top -c -do "transcript off ; run 90ns ; quit -f" > log.txt
/// -novopt
/// cd E:\work\ip4\design\sim
/// vlog -sv ..\tlm\ip4.sv +incdir+..\tlm\ +incdir+d:\questasim_6.6d\verilog_src\ovm-2.1.1\src\

class ip4_direct_test extends ovm_test;
  test_direct_env env;
  `ovm_component_utils_begin(ip4_direct_test)
  `ovm_component_utils_end

  virtual function void build();
    super.build();
    
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

  endtask

  function new(string name = "ip4_direct_test", ovm_component parent);
    super.new(name, parent);
  endfunction : new  
endclass