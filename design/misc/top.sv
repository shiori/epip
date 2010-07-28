module top;
  `include "ip4_tlm_ts.svh"
  tlm_vif_object vif;
  bit clk;
  tlm_sys_if sysif(.*);
  always #(clk_p/2) clk = !clk;
  
  initial begin
    vif = new("vif");
    vif.set_vif(sysif);
    set_config_object("*", "vif_cfg", vif);
    run_test("ip4_module_rand_test");
  end    
endmodule : top