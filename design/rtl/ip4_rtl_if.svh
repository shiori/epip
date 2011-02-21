
`ifndef IP4_RTL_IF_SVH
`define IP4_RTL_IF_SVH

interface ip4_rtl_if(input logic clk, rst_n);
  `include "ip4_tlm_ts.svh"
 	modport spa(
   	input clk, rst_n
   );
endinterface

`endif