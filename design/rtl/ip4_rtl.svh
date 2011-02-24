
`include "ip4_tlm_ts.svh"
import ip4_rtl_pkg::*;

`ifndef IP4_RTL_SVH
`define IP4_RTL_SVH

`define IP4_ASIC_MODE
`define IP4_FPGA_MODE

`define IP4_DEF_PARAM \
  parameter uchar pbId = 0;\
  parameter padr_t srMapBase = 0;
  
`define IP4_PARAM \
  .pbId       (pbId),\
  .srMapBase  (srMapBase)
  
`endif