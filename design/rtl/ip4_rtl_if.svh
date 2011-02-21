
`ifndef IP4_RTL_IF_SVH
`define IP4_RTL_IF_SVH

///axi interface for ip4
interface ip4_axi_if(input logic aclk);
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  
  logic [WID_AXI_ID-1:0] awid, wid, rid;
  logic [WID_AXI_DATA-1:0] wdata, rdata;
  logic [WID_AXI_ADDR-1:0] awaddr;
  
  logic [3:0] awlen;
  logic [2:0] awsize;
  
  logic [BYTES_AXI_DATA-1:0] wstrb;
  logic wlast, wvalid, wready;
  
  
  
 	modport mst(
   	input aclk
   );
   
   modport slv(
   	input aclk
   );
endinterface

///ip4 internal interface
interface ip4_int_if(input logic clk, rst_n);
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  
 	modport spa(
   	input clk, rst_n
   );
endinterface

`endif