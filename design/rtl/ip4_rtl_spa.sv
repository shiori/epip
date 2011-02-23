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
  ise2spa_fu_s fu;
  rfm2spa_s fmRFM[STAGE_EXE_VWBP:1], fmRFMn[STAGE_EXE_VWBP:1];
  ise2spu_s fmSPU[STAGE_EXE_CMP:1], fmSPUn[STAGE_EXE_CMP:1];
  word op[NUM_FU_RP][NUM_SP];
  bit exeExp;
  
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
    
    for(int fid = NUM_FU - 1; fid >= 0; fid--) begin ///SNUM_FU = 3  foreach(ise.fu[fid])
      fu = fmISE.fu[fid];
      if(!fu.en) continue;
      
      if(fu.op inside {sfu_only_ops}) begin
      end
      
      else beign
      ///normal operations
        fmRFMn[1].fu[fid].wrGrp = fu.wrGrp;
        fmRFMn[1].fu[fid].wrAdr = fu.wrAdr;
        fmRFMn[1].fu[fid].wrBk  = fu.wrBk;
        fmRFMn[1].fu[fid].vec  = fu.vec;
        fmRFMn[1].fu[fid].wrEn = fmSPU.fu[fid].emsk;
        fmRFMn[1].fu[fid].wr = fmISE.fu[fid].wrEn;
        fmRFMn[1].fu[fid].tid = fmISE.tid;
        fmRFMn[1].fu[fid].subVec = fmISE.subVec;
        fmRFMn[1].fu[fid].en = 1;
        
        for(int i = 3; i >= 0; i--)
          op[i] = fmRFM.fu[fid].rp[i].op;
         
        ///bypass op
        if(fu.op inside {bp_ops}) begin
          for(int i = NUM_FU - 1; i >= 0; i-- )   ///NUM_FU = 3  foreach(fu_cfg[i])
            for(int rp = NUM_FU_RP - 1; rp >= 0; rp-- ) ///NUM_FU_RP = 4  foreach(op[rp])
              if(i < fid && fu.bpSel[rp] == rbk_sel_e'(selfu0 + i))
                op[rp] = fmRFM[1].fu[i].res0;
        end
        
        proc_data(fu.op, fu.cop, fmISE.prMerge, fmISE.subVec, fmISE.rndMode, fmSPU.fu[fid].emsk, op,
                    presCmp0, presCmp1, fmRFMn[1].fu[fid].res0, fmRFMn[1].fu[fid].res1,
                    fmRFMn[1].fu[fid].expFlag);
                    
        for(int sp = NUM_SP - 1; sp >= 0; sp--)///NUM_SP = 8
          if(fmRFMn[1].fu[fid].expFlag[sp] != 0)
            exeExp = 1;
      end
                    
  end : comb_proc
  ///-------------------------------------other functions-----------------------------------------
 
  bit pres[NUM_SP];
  bit[WORD_BITS:0] op0[NUM_SP], op1[NUM_SP], op2[NUM_SP], op3[NUM_SP], r0[NUM_SP] = '{default:0};
  
  case(op)
  op_nop,
  op_s2gp,
  op_bp0:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op0[i];
  op_bp1:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op1[i];
  op_bp2:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op2[i];
  op_bp3:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op3[i];
    
endmodule : ip4_rtl_spa

