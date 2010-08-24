
`ifdef IP4_TLM_PKG

`define GM(i) ~(-1 << i)

`define PF(NM, F)  printer.print_field(`"NM`", NM, $bits(NM), F);
`define PAF1(NM, F) foreach(NM[i])      printer.print_field($psprintf(`"  NM[%0d]`",i), NM[i], $bits(NM[i]), F);
`define PAF2(NM, F) foreach(NM[i,j])    printer.print_field($psprintf(`"  NM[%0d][%0d]`",i,j), NM[i][j], $bits(NM[i][j]), F);
`define PAF3(NM, F) foreach(NM[i,j,k])  printer.print_field($psprintf(`"  NM[%0d][%0d][%0d]`",i,j,k), NM[i][j][k], $bits(NM[i][j][k]), F);
`define PAF1P(NM, F)    printer.print_field($psprintf(`"  NM[%0d]`",i), NM[i], $bits(NM[i]), F);
`define PAF2P(NM, F)    printer.print_field($psprintf(`"  NM[%0d][%0d]`",i,j), NM[i][j], $bits(NM[i][j]), F);
`define PAF3P(NM, F)    printer.print_field($psprintf(`"  NM[%0d][%0d][%0d]`",i,j,k), NM[i][j][k], $bits(NM[i][j][k]), F);

`define PE(NM)  printer.print_string(`"NM`", NM.name);
`define PAE1(NM) foreach(NM[i])      printer.print_string($psprintf(`"  NM[%0d]`",i), NM[i].name);
`define PAE2(NM) foreach(NM[i,j])    printer.print_string($psprintf(`"  NM[%0d][%0d]`",i,j), NM[i][j].name);
`define PAE3(NM) foreach(NM[i,j,k])  printer.print_string($psprintf(`"  NM[%0d][%0d][%0d]`",i,j,k), NM[i][j][k].name);
`define PAE1P(NM)    printer.print_string($psprintf(`"  NM[%0d]`",i), NM[i].name);
`define PAE2P(NM)    printer.print_string($psprintf(`"  NM[%0d][%0d]`",i,j), NM[i][j].name);
`define PAE3P(NM)    printer.print_string($psprintf(`"  NM[%0d][%0d][%0d]`",i,j,k), NM[i][j][k].name);

`define CA1(NM) do_compare &= comparer.compare_field_int($psprintf(`"  NM[%0d]`",i), NM[i], rhs_.NM[i], $bits(NM[i]));
`define CA2(NM) do_compare &= comparer.compare_field_int($psprintf(`"  NM[%0d][%0d]`",i,j), NM[i][j], rhs_.NM[i][j], $bits(NM[i][j]));
`define CA3(NM) do_compare &= comparer.compare_field_int($psprintf(`"  NM[%0d][%0d][%0d]`",i,j,k), NM[i][j][k], rhs_.NM[i][j][k], $bits(NM[i][j][k]));
`include "ovm_macros.svh"

`else

`ifndef IP4_TLM_SVH
`define IP4_TLM_SVH

`include "ip4_tlm_ts.svh"
import ovm_pkg::*;
`include "ovm_macros.svh"
import ip4_tlm_pkg::*;
import tlm_rec_pkg::*;

`endif

`endif