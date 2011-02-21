
`ifndef IP4_RTL_IF_SVH
`define IP4_RTL_IF_SVH

///axi interface for ip4
interface ip4_axi_if(input logic aclk);
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  
  logic [WID_AXI_ID-1:0] awid, wid, rid, bid, arid;
  logic [WID_AXI_DATA-1:0] wdata, rdata;
  logic [WID_AXI_ADDR-1:0] awaddr, araddr;
  
  ///write address channel
  logic [3:0] awlen, awcache;
  logic [2:0] awsize, awprot;
  logic [1:0] awburst, awlock;
  logic awvalid, awready;
  
  ///write data channel
  logic [BYTES_AXI_DATA-1:0] wstrb;
  logic wlast, wvalid, wready;
  
  ///write response channel
  logic [1:0] bresp;
  logic bvalid, bready;
  
  ///read address channel
  logic [3:0] arlen, arcache;
  logic [2:0] arsize, arprot;
  logic [1:0] arburst, arlock;
  logic arvalid, arready;
  
  ///read response channel
  logic [1:0] rresp;
  logic rlast, rvalid, rready;
  
 	modport mst(
   	input aclk,
   	      awready,
   	      wready,
   	      bid, bresp, bvalid,
   	      arready,
   	      rid, rdata, rresp, rlast, rvalid,
   	output awid, awaddr, awlen, awsize, awburst, awlock, awcache, awprot, awvalid,      
   	       wid, wdata, wstrb, wlast, wvalid, 
   	       bready,
   	       arid, araddr, arlen, arsize, arburst, arlock, arcache, arprot, arvalid,
   	       rready
   );
   
  modport slv(
   	input aclk,
   	      awid, awaddr, awlen, awsize, awburst, awlock, awcache, awprot, awvalid,      
          wid, wdata, wstrb, wlast, wvalid, 
   	      bready,
   	      arid, araddr, arlen, arsize, arburst, arlock, arcache, arprot, arvalid,
   	      rready,
   	output awready,
   	       wready,
   	       bid, bresp, bvalid,
   	       arready
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