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

`include "ip4_rtl.svh"
`include "../model/ram.sv"
`include "../model/DW_fp_cmp.v"

module ip4_sm_bk import ip4_rtl_pkg::*; (
  input logic clk, wr,
        smadr_t wadr, radr,
        word datai,
  output word datao      
);
  `include "ip4_tlm_ts.svh"
  
`ifdef IP4_ASIC_MODE
`ifdef IP4_SIM_MODE

  ip4_ram #(
    .addr_width   ($bits(smadr_t)),
    .word_width   ($bits(word))
  )ram(
    .clk,
    .radr,
    .wadr,
    .wr,
    .be     ('1),
    .datai,
    .datao,
    .datao_d  ()
  );
  
`else

`endif
`else
`ifdef IP4_FPGA_MODE

`endif
`endif  
endmodule

module ip4_tm import ip4_rtl_pkg::*; (
  input bit clk, wrTag, wrCnt, wrSt,
        bit[WID_SMEM_GRP - 1:0] grp0, grp1, wgrp,
        bit[WID_DCHE_IDX - 1:0] adr0, adr1, wadr,
        cache_t datai,
  output cache_t datao0, datao1      
);
  `include "ip4_tlm_ts.svh"
  ///datao1 has no cnt output
  
`ifdef IP4_ASIC_MODE
`ifdef IP4_SIM_MODE
  ip4_ram #(
    .addr_width   (WID_SMEM_GRP + WID_DCHE_IDX),
    .word_width   ($bits(datao0.tag))
  )t_ram0(
    .clk,
    .radr   ({grp0, adr0}),
    .wadr   ({wgrp, wadr}),
    .wr     (wrTag),
    .be     ('1),
    .datai  (datai.tag),
    .datao  (datao0.tag),
    .datao_d  ()
  );

  ip4_ram #(
    .addr_width   (WID_SMEM_GRP + WID_DCHE_IDX),
    .word_width   ($bits(datao1.tag))
  )t_ram1(
    .clk,
    .radr   ({grp1, adr1}),
    .wadr   ({wgrp, wadr}),
    .wr     (wrTag),
    .be     ('1),
    .datai  (datai.tag),
    .datao  (datao1.tag),
    .datao_d  ()
  );

  ip4_ram #(
    .addr_width   (WID_SMEM_GRP + WID_DCHE_IDX),
    .word_width   ($bits(datao0.state)),
    .two_port     (1)
  )s_ram0(
    .clk,
    .radr   ({grp0, adr0}),
    .wadr   ({wgrp, wadr}),
    .wr     (wrSt),
    .be     ('1),
    .datai  (datai.state),
    .datao  (datao0.state),
    .datao_d  ()
  );

  ip4_ram #(
    .addr_width   (WID_SMEM_GRP + WID_DCHE_IDX),
    .word_width   ($bits(datao1.state)),
    .two_port     (1)
  )s_ram1(
    .clk,
    .radr   ({grp1, adr1}),
    .wadr   ({wgrp, wadr}),
    .wr     (wrSt),
    .be     ('1),
    .datai  (datai.state),
    .datao  (datao1.state),
    .datao_d  ()
  );

  ip4_ram #(
    .addr_width   (WID_SMEM_GRP + WID_DCHE_IDX),
    .word_width   ($bits(datao0.cnt)),
    .two_port     (1)
  )c_ram(
    .clk,
    .radr   ({grp0, adr0}),
    .wadr   ({wgrp, wadr}),
    .wr     (wrCnt),
    .be     ('1),
    .datai  (datai.cnt),
    .datao  (datao0.cnt),
    .datao_d  ()
  );
    
`else
`endif
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
  `include "ip4_tlm_ts.svh"
  
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