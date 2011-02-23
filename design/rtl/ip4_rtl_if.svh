
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

interface ip4_int_if;
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  
  typedef struct{
    bit en, vec, wrEn[2];
    opcode_e op;
    cmp_opcode_e cop;
    uchar wrBk, wrAdr, wrGrp;
    rbk_sel_e bpSel[NUM_FU_RP];
  }ise2spa_fu_s;

  typedef struct{
    ise2spa_fu_s fu[NUM_FU];
    pr_merge_e prMerge;
    uchar subVec, tid;  ///vecMode = 3
    uchar bpRfDSEwp;
    rbk_sel_e bpRfDSE;
    round_mode_t rndMode;
    uchar expMsk;
    bit noExp;
  }ise2spa_s;
    
  typedef struct{
    bit noFu[NUM_FU];
    bit exp;
    uchar tid;
  }spa2ise_s;

  ise2spa_s ise2spa;
  spa2ise_s spa2ise;
      
 	modport spa(
   	input ise2spa,
   	output spa2ise
   );

 	modport ise(
   	input spa2ise,
   	output ise2spa
   );
   
endinterface

`endif