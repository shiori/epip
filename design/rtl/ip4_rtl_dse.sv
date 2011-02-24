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
  
  typedef struct {
    bit cancel[NUM_THREAD],
        is_ld,
        is_st,
        is_word,
        is_half,
        is_byte;
    word tlbReqVAdr;
  } dvars;
    
  typedef struct {
    ise2dse_s fmISE[STAGE_RRF_VWB:0];
    spu2dse_s fmSPU[STAGE_RRF_LXG:STAGE_RRF_AG];
    rfm2dse_s fmRFM[STAGE_RRF_LXG:STAGE_RRF_AG];
    eif2dse_s fmEIF[STAGE_RRF_LXG:STAGE_RRF_AG];
    spa2dse_s fmSPA;
    tlb2dse_s fmTLB;
  
    dse2rfm_s rfm[STAGE_RRF_VWBP:STAGE_RRF_SXG0];
    dse2eif_s eif[STAGE_RRF_LXG:STAGE_RRF_SXG0];
    dse2spu_s spu[STAGE_RRF_DPRB:STAGE_RRF_DEM];
    
    dvars d[STAGE_RRF_VWB:0];
    bit tlbRdy;
  } vars;
  
  vars v, vn;
  
  wordu dmi[NUM_SP], dmo[NUM_SP];
  smadr_t dmAdr[NUM_SP];
  logic dmWr[NUM_SP];

  logic tmWr0;
  logic[WID_DCHE_IDX - 1:0] tmAdr0, tmAdr1;
  cache_t tmi, tmo0, tmo1;
    
  always_ff @(posedge clk or negedge rst_n)
    if(!rst_n)
      v <= '{default : '0};
    else
      v <= vn;
  
  always_comb
  begin : comb_proc
    begin : pip_init
      vn = '{default : '0};
      inf.dse2eif = '{default : '0};
      inf.dse2rfm = '{default : '0};
      inf.dse2spu = '{default : '0};
      inf.dse2ise = '{default : '0};
      
      vn.fmSPA = inf.spa2dse;
      vn.fmTLB = inf.tlb2dse;
      vn.fmISE[0] = inf.ise2dse;
      vn.fmRFM[STAGE_RRF_TAG] = inf.rfm2dse;
      vn.fmSPU[STAGE_RRF_AG] = inf.spu2dse;
      vn.fmEIF[STAGE_RRF_AG] = inf.eif2dse;
      
      for (int i = STAGE_RRF_VWB; i > 0; i--) begin
        vn.fmISE[i] = v.fmISE[i - 1];
        vn.d[i] = v.d[i - 1];
      end
  
      for (int i = STAGE_RRF_LXG; i > STAGE_RRF_AG; i--) begin
        vn.fmEIF[i] = v.fmEIF[i - 1];
        vn.fmSPU[i] = v.fmSPU[i - 1];
        vn.fmRFM[i] = v.fmRFM[i - 1];
      end
      
      for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_SXG0; i--) 
        vn.rfm[i] = v.rfm[i - 1];
      vn.rfm[STAGE_RRF_SXG0] = '{default : '0};
  
      for(int i = STAGE_RRF_DPRB; i > STAGE_RRF_DEM; i--)
        vn.spu[i] = v.spu[i - 1];
      vn.spu[STAGE_RRF_DEM] = '{default : '0};
      
      for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SXG0; i--)
        vn.eif[i] = v.eif[i - 1];
      vn.eif[STAGE_RRF_SXG0] = '{default : '0};
      
      dmWr = '{default : '0};
      dmAdr = '{default : '0};
      dmi = '{default : '0};
      tmWr0 = '0;
      tmAdr0 = '0;
      tmAdr1 = '0;
      tmi = '{default : '0};
    end : pip_init
    
    begin : dv_init
      if(vn.fmISE[0].en) begin
        automatic ise2dse_s ise = vn.fmISE[0];
        automatic dvars dn = '{default : '0};
        
        dn.is_ld = ise.op inside {ld_ops};
        dn.is_st = ise.op inside {st_ops};
        dn.is_word = ise.op inside {op_lw, op_ll, op_sw, op_sc};
        dn.is_half = ise.op inside {op_lh, op_lhu, op_sh};
        dn.is_byte = ise.op inside {op_lb, op_lbu, op_sb};
        vn.d[0] = dn;
      end
    end : dv_init
    
    begin : ag_stage
      automatic ise2dse_s ise = v.fmISE[STAGE_RRF_AG];
      automatic rfm2dse_s rfm = v.fmRFM[STAGE_RRF_AG], rfmn = vn.fmRFM[STAGE_RRF_AG];
      automatic spu2dse_s spu = v.fmSPU[STAGE_RRF_AG];
      automatic dvars d = v.d[STAGE_RRF_AG];
      
      automatic word vadr = 0;
      automatic bit found = 0;

      ///AG stage: select vadr from ise req to tlb for translation
      if(v.fmISE[STAGE_RRF_AG].en && v.fmRFM[STAGE_RRF_AG].en 
          && v.fmSPU[STAGE_RRF_AG].en)
      begin
        if(!ise.vec) begin
          found = 1;
          rfmn.base[0] = rfm.base[0] + rfm.os;
          vadr = rfm.base[0];
        end
        else if(d.is_ld || d.is_st || ise.op == op_tmrf) begin
          for(int i = 0; i < NUM_SP; i++) begin
            automatic uchar vecId = NUM_SP * ise.subVec + i;
            if(spu.emsk[i]) begin
              rfmn.base[i] = rfm.base[i] + rfm.os;
              if(ise.at == at_burst || ise.op == op_tmrf) begin
                if(d.is_word)
                  rfmn.base[i] += vecId << 2;
                else if(d.is_half)
                  rfmn.base[i] += vecId << 1;
                else
                  rfmn.base[i] += vecId;
              end
              if(rfmn.base[i] >= VADR_MAPPED && rfmn.base[i] < VADR_NMAPNC) begin
               found = 1;
               vadr = rfmn.base[i];
              end
            end
          end
        end
        
        inf.dse2tlb.vAdr = vadr >> VADR_START;
        if(found && (!v.tlbRdy || (v.d[STAGE_RRF_TAG].tlbReqVAdr != inf.dse2tlb.vAdr))) begin
          vn.tlbRdy = 1;
          inf.dse2tlb.en = 1;
          inf.dse2tlb.op = ise.op;
          inf.dse2tlb.tid = ise.tid;
          inf.dse2tlb.k = ise.priv;
          vn.d[STAGE_RRF_TAG].tlbReqVAdr = inf.dse2tlb.vAdr;
        end
        vn.fmRFM[STAGE_RRF_AG] = rfmn;
      end
    end : ag_stage
    
    begin
    end
  end : comb_proc
  
  genvar i;
  
  for(i = 0; i < NUM_SP; i++) begin : mem
    ip4_sm_bk d(
      .clk,
      .wr     (dmWr[i]),
      .adr    (dmAdr[i]),
      .datai  (dmi[i]),
      .datao  (dmo[i])
    );
    
  end
  
  ip4_tm mem_t (
    .clk, 
    .wr0    (tmWr0),
    .adr0   (tmAdr0),
    .adr1   (tmAdr1),
    .datai  (tmi),
    .datao0 (tmo0),
    .datao1 (tmo1)
);

endmodule : ip4_rtl_dse

