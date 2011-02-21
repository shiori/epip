`include "ovm_macros.svh"

module top;
  `include "ip4_tlm_ts.svh"
  import ovm_pkg::*;
  import ip4_tlm_pkg::*;
  
  tlm_vif_object vif;
  bit clk, rst_n;
  tlm_sys_if sysif(.*);
  always #(CLK_P/2) clk = !clk;
  
  ip4_axi_if axim(
    .aclk  (clk)
  );

  ip4_axi_if axis(
    .aclk  (clk)
  );
    
  ip4_rtl_core #(
    .pbId   (2)
  )core(
    .clk,
    .rst_n,
    .axim,
    .axis
  );
  
  initial begin
    vif = new("vif");
    vif.set_vif(sysif);
    set_config_object("*", "vifCfg", vif);
    run_test("ip4_module_rand_test");
  end    
endmodule : top