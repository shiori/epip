/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_core.sv
/// Title            : ip4 core
/// Version          : 0.1
/// Last modified    : Feb 21 2011
/// =============================================================================
///Log:
///Created by Andy Chen on Feb 21 2011

module ip4_rtl_core(
  input logic clk, rst_n,
  ip4_axi_if.mst axim,
  ip4_axi_if.slv axis
  );
  `include "ip4_rtl.svh"
  `IP4_DEF_PARAM
  
  ip4_int_if intf(.*);
  
  ip4_rtl_spa #(`IP4_PARAM) spa(.*);

endmodule : ip4_rtl_core

