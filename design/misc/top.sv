`include "ovm_macros.svh"

module top;
  `include "ip4_tlm_ts.svh"
  import ovm_pkg::*;
  import ip4_tlm_pkg::*;
  
  tlm_vif_object vif;
  bit clk;
  tlm_sys_if sysif(.*);
  always #(CLK_P/2) clk = !clk;
  
  initial begin
    vif = new("vif");
    vif.set_vif(sysif);
    set_config_object("*", "vifCfg", vif);
    run_test("ip4_module_rand_test");
  end    
endmodule : top