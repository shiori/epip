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
  
  spu2spa_s fmSPU;
  ise2spa_s fmISE[STAGE_RRF_EXE0:0], fmISEn[STAGE_RRF_EXE0:0];
  
  always_ff @(posedge clk or negedge rst_n)
    if(!rst_n) begin
      fmSPU <= '{default : 0};
      fmISE <= '{default : '{default : 0}};
    end
    else begin
      fmSPU <= inf.spu2spa;
      fmISE <= fmISEn;
    end
  
  always_comb
  begin : comb_proc
    for(int i = STAGE_RRF_EXE0; i > 0; i--)
      fmISEn[i] = fmISE[i - 1];
    fmISEn[0] = inf.ise2spa;
    
    
    
  end : comb_proc
    
endmodule : ip4_rtl_spa

