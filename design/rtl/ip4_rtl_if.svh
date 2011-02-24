
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
  
  ise2spa_s ise2spa;
  spa2ise_s spa2ise;
  
  spu2spa_s spu2spa;
  spa2spu_s spa2spu;
  
  rfm2spa_s rfm2spa;
  spa2rfm_s spa2rfm;
  
///  dse2spa_s dse2spa;
  spa2dse_s spa2dse;
  
  dse2eif_s dse2eif;
  eif2dse_s eif2dse;
  
  rfm2dse_s rfm2dse;
  dse2rfm_s dse2rfm;
  
  spu2dse_s spu2dse;
  dse2spu_s dse2spu;
  
  ise2dse_s ise2dse;
  dse2ise_s dse2ise;
  
  tlb2dse_s tlb2dse;
  dse2tlb_s dse2tlb;
  
 	modport spa(
   	input ise2spa,
   	      spu2spa,
   	      rfm2spa,
   	output spa2ise,
   	       spa2spu,
   	       spa2rfm,
   	       spa2dse
  );

 	modport ise(
   	input spa2ise,
   	output ise2spa,
   	       ise2dse
  );
   
  modport spu(
    input spa2spu,
    output spu2spa,
           spu2dse
  );
   
  modport rfm(
    input spa2rfm,
    output rfm2spa,
           rfm2dse
  );
   
  modport dse(
    input spa2dse,
          ise2dse,
          eif2dse,
          rfm2dse,
          spu2dse,
          tlb2dse,
    output dse2eif,
           dse2rfm,
           dse2spu,
           dse2ise,
           dse2tlb
  );
  
  modport eif(
    input dse2eif,
    output eif2dse
  );
    
  modport tlb(
    input dse2tlb,
    output tlb2dse
  );
endinterface

`endif