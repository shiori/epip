/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_spa.sv
/// Title            : ip4 stream processor array
/// Version          : 0.1
/// Last modified    : Feb 20 2011
/// =============================================================================
///Log:
///Created by Andy Chen on Feb 20 2011

module ip4_rtl_sp import ip4_rtl_pkg::*; (
  input logic clk,
        wordu op[3][4],
        opcode_e opcode,
  output wordu res[3][2]
);
  `include "ip4_rtl.svh"
  
    
endmodule


module ip4_rtl_spa(
  input logic clk, rst_n,
  ip4_int_if.spa inf
);
  `include "ip4_rtl.svh"
  `IP4_DEF_PARAM
  

endmodule : ip4_rtl_spa

