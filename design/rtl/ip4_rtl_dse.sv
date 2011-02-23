/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_dse.sv
/// Title            : ip4 stream processor array
/// Version          : 0.1
/// Last modified    : Feb 23 2011
/// =============================================================================
///Log:
///Created by Andy Chen on Feb 23 2011

module ip4_rtl_dse(
  input logic clk, rst_n,
  ip4_int_if.dse inf
);
  `include "ip4_rtl.svh"
  `IP4_DEF_PARAM
  ise2dse_s fmISE[STAGE_RRF_VWB:0], fmISEn[STAGE_RRF_VWB:0];
  spu2dse_s fmSPU[STAGE_RRF_LXG:STAGE_RRF_AG], fmSPUn[STAGE_RRF_LXG:STAGE_RRF_AG];
  rfm2dse_s fmRFM[STAGE_RRF_LXG:STAGE_RRF_AG], fmRFMn[STAGE_RRF_LXG:STAGE_RRF_AG];
  eif2dse_s fmEIF[STAGE_RRF_LXG:STAGE_RRF_AG], fmEIFn[STAGE_RRF_LXG:STAGE_RRF_AG];
  spa2dse_s fmSPA;
  tlb2dse_s fmTLB;

  dse2rfm_s rfm[STAGE_RRF_VWBP:STAGE_RRF_SXG0], rfmN[STAGE_RRF_VWBP:STAGE_RRF_SXG0];
  dse2eif_s eif[STAGE_RRF_LXG:STAGE_RRF_SXG0], eifN[STAGE_RRF_LXG:STAGE_RRF_SXG0];
  dse2spu_s spu[STAGE_RRF_DPRB:STAGE_RRF_DEM], spuN[STAGE_RRF_DPRB:STAGE_RRF_DEM];
  
  wordu dmi[NUM_SP], dmo[NUM_SP];
  smadr_t dmAdr[NUM_SP];
  logic dmWen[NUM_SP];
  
  always_ff @(posedge clk or negedge rst_n)
    if(!rst_n) begin
      fmISE <= '{default : '{default : 0}};
      fmSPU <= '{default : '{default : 0}};
      fmRFM <= '{default : '{default : 0}};
      fmEIF <= '{default : '{default : 0}};
      fmSPA <= '{default : 0};
      fmTLB <= '{default : 0};
      rfm <= '{default : '{default : 0}};
      eif <= '{default : '{default : 0}};
      spu <= '{default : '{default : 0}};
    end
    else begin
      fmISE <= fmISEn;
      fmSPU <= fmSPUn;
      fmRFM <= fmRFMn;
      fmEIF <= fmEIFn;
      fmSPA <= inf.spa2dse;
      fmTLB <= inf.tlb2dse;
      rfm <= rfmN;
      eif <= eifN;
      spu <= spuN;
    end
  
  always_comb
  begin : comb_proc
    fmISEn[0] = inf.ise2dse;
    fmRFMn[STAGE_RRF_TAG] = inf.rfm2dse;
    fmSPUn[STAGE_RRF_AG] = inf.spu2dse;
    fmEIFn[STAGE_RRF_AG] = inf.eif2dse;
    for (int i = STAGE_RRF_VWB; i > 0; i--) 
      fmISEn[i] = fmISE[i - 1];

    for (int i = STAGE_RRF_LXG; i > STAGE_RRF_AG; i--) begin
      fmEIFn[i] = fmEIF[i - 1];
      fmSPUn[i] = fmSPU[i - 1];
      fmRFMn[i] = fmRFM[i - 1];
    end
    
    for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_SXG0; i--) 
      rfmN[i] = rfm[i - 1];
    rfmN[STAGE_RRF_SXG0] = '{default : 0};

    for(int i = STAGE_RRF_DPRB; i > STAGE_RRF_DEM; i--)
      spuN[i] = spu[i - 1];
    spuN[STAGE_RRF_DEM] = '{default : 0};
    
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SXG0; i--)
      eifN[i] = eif[i - 1];
    eifN[STAGE_RRF_SXG0] = '{default : 0};
    
    
  end : comb_proc
  
  genvar i;
  
  for(i = 0; i < NUM_SP; i++) begin : mem
    ip4_sm_bk d(
      .clk,
      .wen    (dmWen[i]),
      .adr    (dmAdr[i]),
      .datai  (dmi[i]),
      .datao  (dmo[i])
    );
    
  end
    
endmodule : ip4_rtl_dse

