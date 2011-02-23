/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_bb.sv
/// Title            : ip4 building block
/// Version          : 0.1
/// Last modified    : Feb 21 2011
/// =============================================================================
///Log:
///Created by Andy Chen on Feb 21 2011

module ip4_sm_bk import ip4_rtl_pkg::*; (
  input logic clk, wen,
        smadr_t adr,
        word datai,
  output word datao      
);

`ifdef IP4_ASIC_MODE
`else
`ifdef IP4_FPGA_MODE


`endif
`endif  
endmodule

module ip4_tm_bk import ip4_rtl_pkg::*; (
  input logic clk, wen,
        bit[WID_DCHE_IDX - 1:0] adr[2],
        cache_t datai[2],
  output cache_t datao[2]      
);

`ifdef IP4_ASIC_MODE
`else
`ifdef IP4_FPGA_MODE


`endif
`endif  
endmodule
