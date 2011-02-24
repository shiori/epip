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

`include "../model/DW_fp_cmp.v"

module ip4_sm_bk import ip4_rtl_pkg::*; (
  input logic clk, wr,
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

module ip4_tm import ip4_rtl_pkg::*; (
  input logic clk, wrTag, wrCnt, wrSt,
        bit[WID_SMEM_GRP - 1:0] grp0, grp1,
        bit[WID_DCHE_IDX - 1:0] adr0, adr1,
        cache_t datai,
  output cache_t datao0, datao1      
);

`ifdef IP4_ASIC_MODE
`else
`ifdef IP4_FPGA_MODE


`endif
`endif  
endmodule

module ip4_fcmp import ip4_rtl_pkg::*; (
  input word op0, op1,
  output word max, min,
         bit[7:0] st0, st1,
         bit eq, lt, gt, uo
);
  `include "ip4_rtl.svh"
  
`ifdef IP4_ASIC_MODE

  DW_fp_cmp cmp(
    .a        (op0), 
    .b        (op1), 
    .zctr     (1'b1), 
    .aeqb     (eq),
    .altb     (lt), 
    .agtb     (gt), 
    .unordered (uo), 
    .z0       (max), 
    .z1       (min), 
    .status0  (st0), 
    .status1  (st1)
  );
  
`else
`ifdef IP4_FPGA_MODE


`endif
`endif    
    
endmodule