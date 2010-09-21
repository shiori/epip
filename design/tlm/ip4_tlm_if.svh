
`ifndef IP4_TLM_IF_SVH
`define IP4_TLM_IF_SVH

interface tlm_sys_if(input bit clk);
  `include "ip4_tlm_ts.svh"
 	modport mods(
   	input clk
   );
endinterface

`endif