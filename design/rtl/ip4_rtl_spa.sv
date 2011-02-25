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
`include "ip4_rtl.svh"

module ip4_rtl_ffu import ip4_rtl_pkg::*; (
  input logic clk,
        wordu op[NUM_FU][NUM_FU_RP][NUM_SP],
        opcode_e opcode[NUM_FU],
  output wordu r[2][NUM_FU][NUM_SP],
         bit gt[NUM_FU][NUM_SP],
             lt[NUM_FU][NUM_SP],
             eq[NUM_FU][NUM_SP],
             uo[NUM_FU][NUM_SP],
         bit [7:0] st[2][NUM_FU][NUM_SP]
);
  `include "ip4_tlm_ts.svh"
  
  bit [7:0] st0Cmp[NUM_FU][NUM_SP],
            st1Cmp[NUM_FU][NUM_SP];
            
  wordu rCmp[2][NUM_FU][NUM_SP];
  
  always_comb
  begin
    st = '{default : '0};
    r = '{default : '0};
    
    for(int i = 0; i < NUM_FU; i++) begin
      case(opcode[i])
        op_fmax:
        begin
          r[0] = rCmp[0];
          st[0] = st0Cmp;
        end
        op_fmin:
        begin
          r[0] = rCmp[1];
          st[0] = st1Cmp;
        end
      endcase
    end
  end
  
  genvar i, j;
   
  for(i = 0; i < NUM_FU; i++) begin : fu
    for(j = 0; j < NUM_SP; j++) begin : sp
      ip4_fcmp fcmp(
        .op0      (op[0][i][j]),
        .op1      (op[1][i][j]),
        .gt       (gt[i][j]),
        .lt       (lt[i][j]),
        .eq       (eq[i][j]),
        .uo       (uo[i][j]),
        .st0      (st0Cmp[i][j]),
        .st1      (st1Cmp[i][j]),
        .max      (rCmp[0][i][j]),
        .min      (rCmp[1][i][j])
       );
    end
  end
endmodule

module ip4_rtl_nfu import ip4_rtl_pkg::*; (
  input logic clk,
        wordu op[NUM_FU][NUM_FU_RP][NUM_SP],
        opcode_e opcode[NUM_FU],
  output wordu r[2][NUM_FU][NUM_SP]
);
  `include "ip4_tlm_ts.svh"
    
endmodule

module ip4_rtl_lfu import ip4_rtl_pkg::*; (
  input logic clk,
        wordu op[NUM_FU][NUM_FU_RP][NUM_SP],
        opcode_e opcode[NUM_FU],
  output wordu r[2][NUM_FU][NUM_SP]
);
  `include "ip4_tlm_ts.svh"
    
endmodule

module ip4_rtl_spa(
  input logic clk, rst_n,
  ip4_int_if.spa inf
);
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;  
  `IP4_DEF_PARAM
  
  typedef struct {
    spu2spa_s fmSPU;
    rfm2spa_s fmRFM;
    ise2spa_s fmISE[STAGE_RRF_EXE0:0];

    spa2rfm_s rfm[STAGE_EXE_VWBP:1];
    spa2ise_s ise[STAGE_EXE_VWBP:1];
    spa2spu_s spu[STAGE_EXE_CMP:1];
    spa2dse_s dse[STAGE_EXE_VWBP:1];
  }vars;
  
  vars v, vn;
  
  opcode_e opcode[NUM_FU];
  wordu opN[NUM_FU][NUM_FU_RP][NUM_SP],
        opF[NUM_FU][NUM_FU_RP][NUM_SP],
        opL[NUM_FU][NUM_FU_RP][NUM_SP],
        rN[2][NUM_FU][NUM_SP],
        rF[2][NUM_FU][NUM_SP],
        rL[2][NUM_FU][NUM_SP];
  bit gt[NUM_FU][NUM_SP],
      lt[NUM_FU][NUM_SP],
      eq[NUM_FU][NUM_SP],
      uo[NUM_FU][NUM_SP];
  bit[7:0] st[2][NUM_FU][NUM_SP];
             
  always_ff @(posedge clk or negedge rst_n)
    if(!rst_n) begin
      v <= '{default : '0};
    end
    else begin
      v <= vn;
    end
  
  always_comb
  begin : comb_proc
    ise2spa_s ise;
    spu2spa_s spu;
    rfm2spa_s rfm;
    
    vn = '{default : '0};
    ise = v.fmISE[STAGE_RRF_EXE0];
    spu = v.fmSPU;
    rfm = v.fmRFM;
    
    for(int i = STAGE_RRF_EXE0; i > 0; i--)
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[0] = inf.ise2spa;
   
  
    for(int fid = NUM_FU - 1; fid >= 0; fid--) begin ///SNUM_FU = 3  foreach(ise.fu[fid])
      ise2spa_fu_s fu;
      bit[WORD_BITS:0] op[NUM_FU_RP][NUM_SP], r0[NUM_SP];
      word r1[NUM_SP];
      bit exeExp;
      
      op = '{default :0};
      r0 = '{default : '0};
      r1 = '{default : '0};
      fu = ise.fu[fid];
      exeExp = 0;
      
      if(!fu.en) continue;
      
      if(fu.opcode inside {sfu_only_ops}) begin
      end
      else begin
        ///fast operations
        vn.rfm[1].fu[fid].wrGrp = fu.wrGrp;
        vn.rfm[1].fu[fid].wrAdr = fu.wrAdr;
        vn.rfm[1].fu[fid].wrBk  = fu.wrBk;
        vn.rfm[1].fu[fid].vec  = fu.vec;
        vn.rfm[1].fu[fid].wrEn = spu.fu[fid].emsk;
        vn.rfm[1].fu[fid].wr = ise.fu[fid].wrEn;
        vn.rfm[1].fu[fid].tid = ise.tid;
        vn.rfm[1].fu[fid].subVec = ise.subVec;
        vn.rfm[1].fu[fid].en = 1;
        
        for(int i = 0; i < NUM_FU_RP; i++)
          for(int j = 0; j < NUM_SP; j++)
            op[i][j] = {rfm.fu[fid].rp[i].op[j][WORD_BITS - 1], rfm.fu[fid].rp[i].op[j]};
         
        ///bypass op
        if(fu.opcode inside {bp_ops}) begin
          for(int i = NUM_FU - 1; i >= 0; i-- )   ///NUM_FU = 3  foreach(fu_cfg[i])
            for(int rp = NUM_FU_RP - 1; rp >= 0; rp-- ) ///NUM_FU_RP = 4  foreach(op[rp])
              if(i < fid && fu.bpSel[rp] == rbk_sel_e'(selfu0 + i))
                for(int j = 0; j < NUM_SP; j++)
                  op[i][j] = {vn.rfm[1].fu[i].res0[j][WORD_BITS - 1], vn.rfm[1].fu[i].res0[j]};
        end
        
        case(fu.opcode)
          op_nop,
          op_s2gp,
          op_bp0:   for(int i = 0; i < NUM_SP; i++) r0[i] = op[0][i];
          op_fmax,  
          op_fmin:  for(int i = 0; i < NUM_SP; i++) r0[i] = rF[0][fid][i];
        endcase
                           
        for(int sp = NUM_SP - 1; sp >= 0; sp--)///NUM_SP = 8
          if(vn.rfm[1].fu[fid].expFlag[sp] != 0)
            exeExp = 1;
      end
    end
  end : comb_proc
      
  ip4_rtl_nfu nfu(
    .clk,
    .op     (opN),
    .opcode (opcode),
    .r      (rN)
  );

  ip4_rtl_ffu ffu(
    .clk,
    .op     (opF),
    .opcode (opcode),
    .r      (rF),
    .gt,
    .lt,
    .eq,
    .uo,
    .st
  );
    
  ip4_rtl_lfu lfu(
    .clk,
    .op     (opL),
    .opcode (opcode),
    .r      (rL)
  );

/* 
  bit pres[NUM_SP];
  bit[WORD_BITS:0] op0[NUM_SP], op1[NUM_SP], op2[NUM_SP], op3[NUM_SP], r0[NUM_SP] = '{default:0};
  
  case(op)
  op_nop,
  op_s2gp,
  op_bp0:   for(int i = NUM_SP - 1; i >= 0; i--)///foreach(r0[i]) NUM_SP = 8
              r0[i] = op0[i];
  op_bp1:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op1[i];
  op_bp2:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op2[i];
  op_bp3:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op3[i];
              
  op_and:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] =o[0][i] & o[1][i];
  op_or:    for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] | o[1][i];
  op_xor:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] ^ o[1][i];
  op_nor:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = ~(o[0][i] | o[1][i]);
  op_add:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = signed'(op0[i]) + signed'(op1[i]);
  op_uadd:  for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op0[i] + op1[i];
  op_sub:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = signed'(op0[i]) - signed'(op1[i]);
  op_usub:  for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = unsigned'(o[0][i]) - unsigned'(o[1][i]);
  op_srl:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] >> o[1][i];
  op_sra:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] >>> o[1][i];
  op_sll:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] << o[1][i];
  op_ror:   for(int i = NUM_SP - 1; i >= 0; i--)
              {r1[i], r0[i]} = {o[0][i], o[0][i]} >> o[1][i];
  op_lid:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] + i + subVec;
  op_shf4a: for(int i = NUM_SP - 1; i >= 0; i--) 
              r0[i] = op0[op1[i][2:0]];
  op_max:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op0[i] > op1[i] ? op0[i] : op1[i];
  op_min:   for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = op0[i] > op1[i] ? op1[i] : op0[i];
  op_umax:  for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] > o[1][i] ? o[0][i] : o[1][i];
  op_umin:  for(int i = NUM_SP - 1; i >= 0; i--)
              r0[i] = o[0][i] > o[1][i] ? o[1][i] : o[0][i];
  op_clo:   for(int i = NUM_SP - 1; i >= 0; i--)
              for(int j=WORD_BITS-1; j>=0; j--)
                if(o[0][i][j])
                  r0[i]++;
                else
                  break;
  op_clz:   for(int i = NUM_SP - 1; i >= 0; i--)
              for(int j=WORD_BITS-1; j>=0; j--)
                if(!o[0][i][j])
                  r0[i]++;
                else
                  break;
  
  */
endmodule : ip4_rtl_spa

