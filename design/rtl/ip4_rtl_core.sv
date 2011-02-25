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

`include "ip4_rtl.svh"

module ip4_rtl_core(
  input logic clk, rst_n,
  ip4_axi_if.mst axim,
  ip4_axi_if.slv axis
  );
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  `IP4_DEF_PARAM
  
  ip4_int_if inf();
  
  ip4_rtl_spa #(`IP4_PARAM) spa(.*);
  ip4_rtl_dse #(`IP4_PARAM) dse(.*);
  
endmodule : ip4_rtl_core

