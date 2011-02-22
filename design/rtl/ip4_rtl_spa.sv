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

module ip4_rtl_sp(
  clk, op0, op1, op2, op3,
  op, fr, nr0, nr1, lr0, lr1
);
  `include "ip4_rtl.svh"
  parameter bit has_fp_op   = 1,
                has_lng_op  = 1,
                stage       = 0;
  input logic clk;
  input wordu op0, op1, op2, op3;
  input opcode_e op;
  output wordu fr, nr0, nr1, lr0, lr1;
  
    
endmodule


module ip4_rtl_spa(ip4_int_if.spa intf);
  `include "ip4_rtl.svh"
  `IP4_DEF_PARAM
  

endmodule : ip4_rtl_spa

