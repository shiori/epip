/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_dse.sv
/// Title            : ip4 data stream engine
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

class ip4_tlm_dse_vars extends ovm_component;
  tr_ise2dse fmISE[STAGE_RRF_VWB:0];
  tr_spu2dse fmSPU[STAGE_RRF_LXG:STAGE_RRF_AG];
  tr_rfm2dse fmRFM[STAGE_RRF_LXG:STAGE_RRF_AG];
  tr_eif2dse fmEIF[STAGE_RRF_LXG:STAGE_RRF_AG];    /// external interfaces
  tr_spa2dse fmSPA;
  tr_tlb2dse fmTLB;

  tr_dse2rfm rfm[STAGE_RRF_VWBP:STAGE_RRF_SXG0];
  tr_dse2eif eif[STAGE_RRF_LXG:STAGE_RRF_SXG0];
  tr_dse2spu spu[STAGE_RRF_DPRB:STAGE_RRF_LXG0];
  
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_sarray_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(eif, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_dse_vars

typedef struct{
  uchar xhg[LAT_XCHG][NUM_SP];
  bit wrEn[LAT_XCHG][NUM_SP], en, endian;
  uchar wrGrp, wrAdr, wrBk, tid, subVec;
  opcode_e op;
  padr_t padr;
}ldQue_t;

typedef struct{
  uchar tid;
  padr_t padr;
  bit en, endian;
}stQue_t;

typedef struct{
  uint tagHi, tagLo[NUM_SMEM_GRP];
  bit tagV[NUM_SMEM_GRP], dirty[NUM_SMEM_GRP];
  uchar lo;
}cache_t;

class sm_t;
///sel stage data struct
  bit sMemOpy,   ///occupy for onchip shared mem
      exEn[WORD_BYTES],  ///ext enabled
      sMemWEn[WORD_BYTES],
      ocEn;  ///onchip enabled
  uint sMemAdr, sMemGrp;  ///on chip adr grp
  wordu stData; ///store exchange buffer
endclass

class sp_t;
  bit exp, oc, ex, re;
  uchar slot;
  ushort xhg; ///cl + bk + os
endclass

///---------------------------------------main component----------------------------------------
class ip4_tlm_dse extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_dse_vars v, vn;  
  local wordu sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SP];
  local cache_t cache[NUM_DCHE_TAG][NUM_DCHE_ASO];
  local uchar cacheSelHi[NUM_DCHE_TAG];
  
  local bit cacheFlush[STAGE_RRF_LXG:STAGE_RRF_DEM],
            exReq[STAGE_RRF_LXG:STAGE_RRF_DEM],
            endian[STAGE_RRF_LXG:STAGE_RRF_DEM],
            expReq[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local cause_dse_t expCause[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local bit[STAGE_RRF_LXG:0] cancel[NUM_THREAD];

  local word tlbReqVAdr[STAGE_RRF_SEL:STAGE_RRF_TAG];
  local tr_tlb2dse tlbCached;
  local bit tlbRdy;
  
  local bit selExRdy, selExp, selExpReq, selValidReq, selNoCache;
  local padr_t selExAdr;
  local cause_dse_t selCause;

  local uchar srCacheGrp;
  local uint srMapBase;
  local bit cacheGrpEn[NUM_SMEM_GRP];
  local bit sendExp;
  local bit[CYC_DSE - 1 : 0] dprbReRun;
  
  local sm_t ck[LAT_XCHG][NUM_SP],
             smi[STAGE_RRF_LXG:STAGE_RRF_SEL][NUM_SP];
  local sp_t spi[STAGE_RRF_LXG:STAGE_RRF_SXG0][NUM_SP];
  
  local bit dprbQueRdy;
  local uchar dprbQueId;
  local wordu dcXhgData[STAGE_RRF_LXG:STAGE_RRF_LXG0][NUM_SP],
              dcFlushData[STAGE_RRF_LXG:STAGE_RRF_LXG0][NUM_SP];
  
  local ldQue_t ldQue[NUM_LDQUE];
  local stQue_t stQue[NUM_STQUE];
  local uchar pbId;
  
  `ovm_component_utils_begin(ip4_tlm_dse)
    `ovm_field_int(pbId, OVM_ALL_ON)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2dse, tr_ise2dse, ip4_tlm_dse) ise_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2dse, tr_rfm2dse, ip4_tlm_dse) rfm_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2dse, tr_spu2dse, ip4_tlm_dse) spu_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2dse, tr_spa2dse, ip4_tlm_dse) spa_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2dse, tr_tlb2dse, ip4_tlm_dse) tlb_tr_imp;
  ovm_nonblocking_transport_imp_eif #(tr_eif2dse, tr_eif2dse, ip4_tlm_dse) eif_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_dse2ise, tr_dse2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2rfm, tr_dse2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spu, tr_dse2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spa, tr_dse2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2tlb, tr_dse2tlb) tlb_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2eif, tr_dse2eif) eif_tr_port;
        
  function void comb_proc();
             
    ovm_report_info("dse", "comb_proc procing...", OVM_FULL); 
   
    if(v.fmISE[STAGE_RRF_RRC0] != null) end_tr(v.fmISE[STAGE_RRF_RRC0]);
    if(v.fmRFM[STAGE_RRF_AG] != null) end_tr(v.fmRFM[STAGE_RRF_AG]); 
    if(v.fmSPU[STAGE_RRF_AG] != null) end_tr(v.fmSPU[STAGE_RRF_AG]);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmTLB != null) end_tr(v.fmTLB);
    if(v.fmEIF[STAGE_RRF_AG] != null) end_tr(v.fmEIF[STAGE_RRF_AG]);
    
    vn.fmISE[STAGE_RRF_RRC0] = null;
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    vn.fmEIF[STAGE_RRF_AG] = null;
    vn.fmSPA = null;
    vn.fmTLB = null;

    foreach(cancel[i])
      cancel[i] = cancel[i] << 1;
      
    ///cancel from spa
    if(v.fmSPA != null && v.fmSPA.cancel)
      cancel[v.fmSPA.tid] |= `GML(STAGE_RRF_DC);

    ///cancel from spu
    if(v.fmSPU[0] != null) begin
      if(v.fmSPU[0].missBr || v.fmSPU[0].expMSC)
        cancel[v.fmSPU[0].tidExpMSC] |= `GML(STAGE_RRF_DC);
      if(v.fmSPU[0].expFu)
        cancel[v.fmSPU[0].tidExpFu] |= `GML(STAGE_RRF_EPS - v.fmSPU[0].vecModeExpFu);
    end
    
    ///cancel from self
    if(v.fmISE[STAGE_RRF_DPRB] != null && sendExp)
      cancel[v.fmISE[STAGE_RRF_DPRB].tid] |= `GML(STAGE_RRF_DPRB);
      
    for (int i = STAGE_RRF_VWB; i > 0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[0] = null;

    for (int i = STAGE_RRF_LXG; i > STAGE_RRF_AG; i--) begin
      vn.fmEIF[i] = v.fmEIF[i - 1];
      vn.fmSPU[i] = v.fmSPU[i - 1];
      vn.fmRFM[i] = v.fmRFM[i - 1];
    end
    vn.fmEIF[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    vn.fmRFM[STAGE_RRF_AG] = null;
    
    for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_SXG0; i--) 
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_SXG0] = null;

    for(int i = STAGE_RRF_DPRB; i > STAGE_RRF_LXG0; i--)
      vn.spu[i] = v.spu[i - 1];
    vn.spu[STAGE_RRF_LXG0] = null;
    
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SXG0; i--)
      vn.eif[i] = v.eif[i - 1];
    vn.eif[STAGE_RRF_SXG0] = null;
    
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_LXG0; i--) begin
      dcXhgData[i] = dcXhgData[i - 1];
      dcFlushData[i] = dcFlushData[i - 1];
    end
      
    if(v.fmTLB != null)
      tlbCached = v.fmTLB;
  endfunction
  
  function void req_proc();
    tr_dse2rfm toRFM;
    tr_dse2spu toSPU;
    tr_dse2eif toEIF;
    tr_dse2ise toISE;
    tr_dse2tlb toTLB;
    
    ovm_report_info("dse", "req_proc procing...", OVM_FULL); 
         
    toSPU = v.spu[STAGE_RRF_DPRB];
    toRFM = v.rfm[STAGE_RRF_VWBP];
    toEIF = v.eif[STAGE_RRF_LXG];
            
    ///dc stage
    begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DC];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DC];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DC];
      wordu xhgData[NUM_SP], eifRes[NUM_SP], spRes[NUM_SP], smWData[NUM_SP];
      bit last = 0, exRsp = 0; ///exp = 0, ex = 0, oc = 0, 
      if(ise != null && ise.en)
        last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(eif != null && (eif.loadRsp || eif.storeRsp)) begin
        exRsp = 1;
        last = eif.last;
      end
      
      ///shared memory write source select
      if(exRsp && v.fmEIF[STAGE_RRF_TAG] != null) begin
        ///eif data comes late
        uchar st = `SG(STAGE_RRF_DC, STAGE_RRF_TAG, STAGE_RRF_AG);
        if(endian[STAGE_RRF_DC])
          foreach(smWData[bk])
            smWData[bk] = v.fmEIF[st].data[bk];
        else
          foreach(smWData[bk])
            smWData[bk] = v.fmEIF[st].data[WORD_BYTES - 1 - bk];
      end
      else begin
        if(ise != null && ise.op inside {st_ops} && smi[STAGE_RRF_DC][0] != null)
          foreach(smWData[bk])
            smWData[bk] = smi[STAGE_RRF_DC][bk].stData;
      end
      
      ///shared memory write
      if(smi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else begin
        for(int bk = 0; bk < NUM_SP; bk++) begin
          uint adr = smi[STAGE_RRF_DC][bk].sMemAdr,
               grp = smi[STAGE_RRF_DC][bk].sMemGrp;
          bit wEn[WORD_BYTES] = smi[STAGE_RRF_DC][bk].sMemWEn;
          foreach(wEn[os])
            if(wEn[os])
              sharedMem[grp][adr][bk].b[os] = smWData[bk].b[os];
        end
      end
      
      ///sharedMem read
      if(smi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else begin
        for(int bk = 0; bk < NUM_SP; bk++) begin
          uint adr = smi[STAGE_RRF_DC][bk].sMemAdr,
               grp = smi[STAGE_RRF_DC][bk].sMemGrp,
               adr2 = adr ^ 'b01;   ///flip last bit
          if(exRsp) begin
            dcFlushData[STAGE_RRF_DC][bk] = sharedMem[grp][adr2][bk];
            xhgData[bk] = smWData[bk];
          end
          else
            xhgData[bk] = sharedMem[grp][adr][bk];
        end
      end
            
      ///load data exchange
      if(spi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else begin
        for(int sp = 0; sp < NUM_SP; sp++) begin
          for(int slot = 0; slot < LAT_XCHG; slot++) begin
            uchar st = STAGE_RRF_LXG0 + slot;
            uchar bk = spi[st][sp].xhg >> WID_WORD & `GML(WID_SMEM_BK),
                  slot = spi[st][sp].slot;
            if(spi[st][sp].slot != slot) continue;
            dcXhgData[st][sp] = xhgData[bk];
          end
        end
      end
    end
    
    if(cacheFlush[STAGE_RRF_LXG]) begin
      if(toEIF == null)
        ovm_report_warning("lxg", "eif write out tr missing");
      else
        toEIF.data = dcFlushData[STAGE_RRF_LXG];
    end
    
    ///now exception is resolved, allocate que
    if(v.fmISE[STAGE_RRF_DPRB] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DPRB];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DPRB];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DPRB];
///      tr_spu2dse ise = v.fmSPU[STAGE_RRF_DPRB];
      bit last = 0, exRsp = 0;
      
      if(ise != null && ise.en)
        last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(eif != null && (eif.loadRsp || eif.storeRsp)) begin
        exRsp = 1;
        last = eif.last;
      end
                    
      if(spi[STAGE_RRF_DPRB][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      ///ex rsp
      else if(exRsp) begin
        dprbQueRdy = 0;

      end
      ///dse que fill
      else if(exReq[STAGE_RRF_DPRB] && ise != null) begin
        uchar cyc = ise.subVec & `GML(WID_DCHE_CL),
              queId = 0;
        bit found = 0, noVecSt = 0, noSglSt = 0, noLd = 0;
        padr_t padr;
        if(vn.eif[STAGE_RRF_DPRB] == null) vn.eif[STAGE_RRF_DPRB] = tr_dse2eif::type_id::create("toEIF", this);
        if(v.eif[STAGE_RRF_DPRB] != null)
          padr = v.eif[STAGE_RRF_DPRB].pAdr;
        if(v.fmEIF[STAGE_RRF_AG] != null) begin
          noVecSt = v.fmEIF[STAGE_RRF_AG].noVecSt;
          noSglSt = v.fmEIF[STAGE_RRF_AG].noSglSt;
          noLd = v.fmEIF[STAGE_RRF_AG].noLd;
        end
        for(int bk = 0; bk < NUM_SP; bk++)
          for(int os = 0; os < WORD_BYTES; os++)
            vn.eif[STAGE_RRF_DPRB].byteEn[os] = smi[STAGE_RRF_DPRB][bk].exEn[os];
        
        if(ise.op inside {ld_ops}) begin
          if(!dprbQueRdy) begin
            foreach(ldQue[i])
              if(!ldQue[i].en) begin
                ldQue[i].en = 1;
                queId = i;
                found = 1;
                dprbQueRdy = 1;
                dprbQueId = i;
                break;
              end
            if(!found)
              ovm_report_info("dc", "ld queue overrun!", OVM_HIGH);
          end
          else begin
            queId = dprbQueId;
            found = 1;
          end
          if(!found || (noLd && ise.subVec == 0))
            dprbReRun[ise.subVec] = '{default : 1};
          if(!dprbReRun[ise.subVec]) begin
            ldQue[queId].en = 1;
            ldQue[queId].wrGrp = ise.wrGrp;
            ldQue[queId].wrBk = ise.wrBk;
            ldQue[queId].wrAdr = ise.wrAdr;
            ldQue[queId].tid = ise.tid;
            ldQue[queId].op = ise.op;
            ldQue[queId].padr = selExAdr;
          end
          for(int sp = 0; sp < NUM_SP; sp++) begin
            ldQue[queId].wrEn[cyc][sp] = spi[STAGE_RRF_DPRB][sp].oc;
            ldQue[queId].xhg[cyc][sp] = spi[STAGE_RRF_DPRB][sp].xhg;
          end
          if(last)
            ldQue[queId].subVec = ise.subVec;
        end
        else if(ise.op inside {st_ops}) begin
          if(!dprbQueRdy) begin
            foreach(stQue[i])
              if(!stQue[i].en) begin
                stQue[i].en = 1;
                queId = i;
                found = 1;
                dprbQueRdy = 1;
                dprbQueId = i;
                break;
              end
            if(!found)
              ovm_report_info("dc", "ld queue overrun!", OVM_HIGH);
          end
          else begin
            queId = dprbQueId;       
            found = 1;
          end
          if(!found)
            dprbReRun[ise.subVec] = 1;
          else if(noVecSt && ise.vec && ise.vecMode != 0)
            dprbReRun[ise.subVec] = 1;
          else if(noSglSt && (ise.vecMode == 0 || !ise.vec))
            dprbReRun[ise.subVec] = 1;
              
          if(!dprbReRun[ise.subVec]) begin
            stQue[queId].en = 1;
            stQue[queId].tid = ise.tid;
            stQue[queId].padr = selExAdr;
          end
          for(int bk = 0; bk < NUM_SP; bk++)
            vn.eif[STAGE_RRF_DPRB].data[cyc][bk] = smi[STAGE_RRF_DPRB][bk].stData;
        end
        vn.eif[STAGE_RRF_DPRB].id = queId;
        
        if(last)
          dprbQueRdy = 0;
      end

      
      if(last && !cancel[STAGE_RRF_DPRB]) begin
        toISE = tr_dse2ise::type_id::create("toISE", this);
        toISE.rsp = 1;
        toISE.exp = expReq[STAGE_RRF_DPRB];
        toISE.scl = ise.vec == 0;
        toISE.tid = ise.tid;
        toISE.vecMode = ise.vecMode;
        toISE.pendExLoad = 0;
        toISE.pendExStore = 0;
        toISE.cause = expCause[STAGE_RRF_DPRB];
        foreach(ldQue[i])
          if(ldQue[i].en && ldQue[i].tid == ise.tid)
            toISE.pendExLoad++;
        foreach(stQue[i])
          if(stQue[i].en && stQue[i].tid == ise.tid)
            toISE.pendExStore++;
        if(expReq[STAGE_RRF_DPRB]) begin
          if(toSPU == null) toSPU = tr_dse2spu::type_id::create("toSPU", this);
          if(toRFM == null) toRFM = tr_dse2rfm::type_id::create("toRFM", this);
          toSPU.cancel = 1;
          toSPU.tidCancel = v.fmISE[STAGE_RRF_DPRB] == null ? 0 : v.fmISE[STAGE_RRF_DPRB].tid;
          sendExp = 1;
        end
      end
    end
    
    ///now load exchange data is ready, set them to eif & rfm tr
    begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_LXG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_LXG];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_LXG];
      bit rfmReq = 0;
      opcode_e op;
      
      if(eif != null && eif.loadRsp) begin
        if(v.eif[STAGE_RRF_LXG] != null)
          op = v.eif[STAGE_RRF_LXG].op;
        rfmReq = 1;
      end
      else if(ise != null && exReq[STAGE_RRF_LXG] && !cancel[ise.tid][STAGE_RRF_LXG]
         && !expReq[STAGE_RRF_LXG] && ise.op inside {ld_ops}) begin
        rfmReq = 1;
        op = ise.op;
      end
      
      if(rfmReq) begin
        if(vn.rfm[STAGE_RRF_LXG] == null) vn.rfm[STAGE_RRF_LXG] = tr_dse2rfm::type_id::create("toRFM", this);
        for(int sp = 0; sp < NUM_SP; sp++) begin
          uchar os = spi[STAGE_RRF_LXG][sp].xhg & `GML(WORD_BYTES);
          wordu res = dcXhgData[STAGE_RRF_LXG][sp];
          case(op)
          op_lw:  vn.rfm[STAGE_RRF_LXG].res[sp] = res;
          op_lh:  vn.rfm[STAGE_RRF_LXG].res[sp] = {{WORD_BITS{res.h[os >> WID_HALF].b[7]}}, res.h[os >> WID_HALF]};
          op_lhu: vn.rfm[STAGE_RRF_LXG].res[sp] = res.h[os >> WID_HALF];
          op_lb:  vn.rfm[STAGE_RRF_LXG].res[sp] = res.b[os];
          op_lbu: vn.rfm[STAGE_RRF_LXG].res[sp] = {{WORD_BITS{res.b[os][7]}}, res.b[os]};
          endcase
        end
      end
      
      if(cacheFlush[STAGE_RRF_LXG]) begin
        if(vn.eif[STAGE_RRF_LXG] == null) vn.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
        vn.eif[STAGE_RRF_LXG].data = dcFlushData[STAGE_RRF_LXG];
      end
      else if(ise != null && exReq[STAGE_RRF_LXG] && !cancel[ise.tid][STAGE_RRF_LXG] && !expReq[STAGE_RRF_LXG]) begin
        if(ise.op inside {st_ops}) begin
          if(vn.eif[STAGE_RRF_LXG] == null) vn.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
          vn.eif[STAGE_RRF_LXG].data = dcXhgData[STAGE_RRF_LXG];
        end
        else
          vn.eif[STAGE_RRF_LXG] = null;
      end
    end
    
    ///spu ops
    begin
      uchar st = `SG(STAGE_RRF_RSR, STAGE_RRF_RSR, STAGE_RRF_AG);
      tr_spu2dse spu = v.fmSPU[st];
      if(spu != null && spu.s2gp) begin
        if(toSPU == null)
          toSPU = tr_dse2spu::type_id::create("toSPU", this);
        toSPU.rsp = 1;
        case(spu.srAdr)
        SR_MBASE: toSPU.srRes = srMapBase;
        SR_OCMC:  toSPU.srRes = srCacheGrp;
        endcase
      end
      if(spu != null && spu.op == op_gp2s && !cancel[spu.tid][STAGE_RRF_WSR]) begin
        case(spu.srAdr)
        SR_MBASE: srMapBase = spu.op0;
        SR_OCMC:
        begin
          srCacheGrp = spu.op0 & `GML(NUM_SMEM_GRP);
          foreach(cacheGrpEn[i])
            if(i >= (NUM_SMEM_GRP - srCacheGrp))
              cacheGrpEn[i] = 1;
        end
        endcase
      end
      else
        tlbRdy = 0;
    end

    ///now check if scalar dse pass pr
    if(v.fmISE[STAGE_RRF_EXE0] != null && v.fmSPU[STAGE_RRF_AG] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_EXE0];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
      if(spu.sclEn && !ise.vec && ise.en) begin
        exReq[STAGE_RRF_EXE0] = 0;
        for(int bk = 0; bk < NUM_SP; bk++)
          smi[STAGE_RRF_EXE0][bk].ocEn = '{default : 0};
      end
    end
    
    ///do pip shift before sel stage
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_DEM; i--) begin
      spi[i] = spi[i - 1];
      expReq[i] = expReq[i - 1];
      expCause[i] = expCause[i- 1];
      cacheFlush[i] = cacheFlush[i - 1];
      exReq[i] = exReq[i - 1];
      endian[i] = endian[i - 1];
    end
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SEL; i--)
      smi[i] = smi[i - 1];
    spi[STAGE_RRF_SXG0] = '{default: null};
    smi[STAGE_RRF_SEL] = '{default: null};
                  
    ///**sel stage, ise request
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
             smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_GRP - srCacheGrp) * SGRP_SIZE,
             smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
      uint vAdrHi;
      uint dcIdx, dcRdy = 0;
      bit ed = 0, wa = 0, wt = 0;
      bit last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      
      exReq[STAGE_RRF_DEM] = 0;
      if(tlb == null) tlb = tlbCached;
      if(tlb != null) begin
        vAdrHi = tlbReqVAdr[STAGE_RRF_SEL] >> tlb.eobit;
        ed = tlb.endian;
        wa = tlb.writeAlloc;
        wt = tlb.writeThru;
      end
      
      foreach(rfm.base[sp]) begin
        padr_t padr;
        bit nc, exp;
        bit oc = spu.emsk[sp] && ise.en,
            ex = spu.emsk[sp] && ise.en && !ise.noExt;
        uchar grp, adr, bk, os, slot;
        wordu res;
        wordu st = rfm.st[sp];
                
        ///vadr to padr translation stage
        if(rfm.base[sp] >= VADR_NMAPCH) begin
          padr = rfm.base[sp];
        end
        else if(rfm.base[sp] >= VADR_EJTAGS) begin
          nc = 1;
          oc = 0;
          padr = srMapBase + EJTG_OFFSET + pbId * EJTG_SIZE + rfm.base[sp] - VADR_EJTAGS;
        end
        else if(rfm.base[sp] >= VADR_NMAPNC) begin
          nc = 1;
          oc = 0;
          padr = srMapBase + rfm.base[sp] - VADR_NMAPNC;
        end
        else if(tlb != null && tlb.hit && (rfm.base[sp] >> (VADR_START + tlb.eobit)) == vAdrHi) begin
          padr = rfm.base[sp];
          if(!spu.emsk[sp]) continue;
          if(!selExp && tlb.exp) begin
            selExp = 1;
            exp = 1;
            selCause = tlb.cause;
            continue;
          end
          nc = tlb.cached == 0;
          for(int j = (VADR_START + tlb.eobit); j < PADR_WIDTH; j++)
            padr = tlb.pfn[j - VADR_START];
        end
        else begin
          oc = 0;
          ex = 0;
        end
      
        if(padr > smStart && padr < smEnd2)
          ex = 0;
        
        ///access data
        if(!oc && !ex) continue;
        
        ///align exp
        if((ise.op inside {ld_ops, op_fetadd} && padr[1:0] != 2'b0)
           || (ise.op inside {op_lh, op_sh, op_lhu, op_cmpxchg} && padr[0] != 1'b0)) begin
          if(!selExp) begin
            selCause = EC_ADRALG;
            exp = 1;
            selExp = 1;
            continue;
          end
        end

        os = padr & `GML(WID_WORD);        
        bk = (padr >> WID_WORD) & `GML(WID_SMEM_BK);
        adr = padr >> (WID_WORD + WID_SMEM_BK) & `GML(WID_SMEM_ADR);
        
        ///----------------------start access----------------------------
        ///external mem
        ///**cache address:   | tagHi | tagLo | idx | cl | bk | offset |
        if(ex) begin
          bit hit = 0;
          uint idx;
          
          ///chk cache for match
          if(!nc && cacheGrpEn[grp] && oc) begin
            uint tagLo = padr >> (WID_WORD + WID_SMEM_BK + WID_DCHE_CL + WID_DCHE_IDX),
                 tagHi = tagLo >> WID_DCHE_STAG;
            tagLo = tagLo & `GML(WID_DCHE_STAG);
            idx = adr >> WID_DCHE_IDX;
            
            for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++) begin
              for(int loTagIdx = 0; loTagIdx < NUM_SMEM_GRP; loTagIdx++) begin
                if(!dcRdy || dcIdx == idx) begin
                  dcRdy = 1;
                  dcIdx = idx;
                  hit = cache[idx][hiTagIdx].tagV[loTagIdx] && cache[idx][hiTagIdx].tagHi == tagHi && 
                        cache[idx][hiTagIdx].tagLo[loTagIdx] == tagLo;
                  dcRdy = 1;
                  dcIdx = idx;
                  if(hit) begin
                    grp = loTagIdx;
                    break;
                  end
                end
              end
              if(hit)
                adr = (adr & `GML(WID_SMEM_ADR - WID_DCHE_ASO)) | (hiTagIdx << (WID_SMEM_ADR - WID_DCHE_ASO));
            end
          end
          
          oc = hit;
          ///external access
          if(wt && oc && selExRdy) begin
            bit exhit = selExAdr == (padr >> (WID_DCHE_CL + WID_SMEM_BK + WID_WORD));
            if(wt && !exhit) begin
                ex = 0;
                oc = 0;
              end
          end
          
          ///cache hit
          if(oc) begin
            oc = 0;
            for(int s = 0; s < LAT_XCHG; s++) begin
              if((ck[s][bk].sMemAdr == adr && ck[s][bk].sMemGrp == grp) || ck[s][bk].sMemOpy) begin
                ck[s][bk].sMemOpy = 1;
                slot = s;
                oc = 1;
                break;
              end
            end
          end

          ///external access
          if((wt || !oc) && ex) begin
            if(!selExRdy) begin
              ex = oc ? wt : 1;
              selExRdy = ex;
              selNoCache = nc;
              selExAdr = padr >> (WID_DCHE_CL + WID_SMEM_BK + WID_WORD);
            end
            else begin
              bit exhit = selExAdr == (padr >> (WID_DCHE_CL + WID_SMEM_BK + WID_WORD));
              if(wt && !exhit) begin
                ex = 0;
                oc = 0;
              end
              else
                ex = !oc;
            end
          end
        end
        ///**shared mem
        else if(oc) begin
          if(padr >= smEnd) begin
            if(!selExp) selCause = EC_SMBOND;
            selExp = 1;
            exp = 1;
            continue;
          end
          
          for(int s = 0; s < LAT_XCHG; s++) begin
            oc = 0;
            if((ck[s][bk].sMemAdr == adr && ck[s][bk].sMemGrp == grp) || ck[s][bk].sMemOpy) begin
              ck[s][bk].sMemOpy = 1;
              slot = s;
              oc = 1;
              break;
            end
          end
        end
        
        ///filling ck
        if(oc) begin
          ck[slot][bk].sMemAdr = adr;
          ck[slot][bk].sMemGrp = grp;
///          ck[slot][bk].sMemWEn = os;
          ck[slot][bk].ocEn = oc;
        end

        case(ise.op)
        op_sw   : 
          for(int j = 0; j < WORD_BYTES; j++) begin
            ck[grp][bk].stData.b[j] = st.b[j];
            ck[grp][bk].sMemWEn[j] = (ise.op inside {st_ops}) && oc;
            ck[grp][bk].exEn[j] = ex;
          end
        op_sh   :
          for(int j = 0; j < HALF_BYTES; j++) begin
            uchar adr2 = os & `GML(WID_HALF);
            ck[grp][bk].stData.b[adr2 + j] = st.b[adr2 + j];
            ck[grp][bk].sMemWEn[adr2 + j] = (ise.op inside {st_ops}) && oc;
            ck[grp][bk].exEn[adr2 + j] = ex;
          end
        op_sb   :
        begin
          ck[grp][bk].stData.b[os] = st.b[os];
          ck[grp][bk].sMemWEn[os] = (ise.op inside {st_ops}) && oc;
          ck[grp][bk].exEn[os] = ex;
        end
        endcase
        
        spi[STAGE_RRF_SXG0][sp] = new();
        spi[STAGE_RRF_SXG0][sp].xhg = padr[sp] & `GML(WID_DCHE_CL + WID_SMEM_BK + WID_WORD);
        spi[STAGE_RRF_SXG0][sp].exp = exp;
        spi[STAGE_RRF_SXG0][sp].oc = oc;
        spi[STAGE_RRF_SXG0][sp].ex = ex;
        spi[STAGE_RRF_SXG0][sp].re = spu.emsk[sp] && !oc && !ex;
        spi[STAGE_RRF_SXG0][sp].slot = slot;
        selValidReq |= oc || ex;
        exReq[STAGE_RRF_DEM] |= ex;
      end
        
      expCause[STAGE_RRF_DEM] = selCause;
      endian[STAGE_RRF_DEM] = ed;
      
      selExpReq |= selExp;
      
      if(ise.vecMode == ise.subVec) begin
        bit res = 0;
        if(ise.nonBlock)
          res = selValidReq;
        else
          res = selExpReq;
          
        for(int i = 0; i <= ise.subVec; i++)
          expReq[STAGE_RRF_DEM + i] = res;
                    
        selExpReq = 0;
        selValidReq = 0;
      end
        expReq[STAGE_RRF_DEM] = 1;

      ///finish one half wrap or whole request
      if(last) begin
        uchar lvl = ise.subVec & `GML(WID_DCHE_CL);
        for(int i = 0; i <= lvl; i++)
          smi[STAGE_RRF_DEM + i] = ck[LAT_XCHG - 1 - i];
        selExp = 0;
        selExRdy = 0;
        selNoCache = 0;
        foreach(ck[i, j])
          ck[i][j] = new();
      end
            
      ///ext access allocate tr
      if(exReq[STAGE_RRF_DEM]) begin
        if(vn.eif[STAGE_RRF_DEM] == null) vn.eif[STAGE_RRF_DEM] = tr_dse2eif::type_id::create("toSPU", this);
        if(vn.rfm[STAGE_RRF_DEM] == null) vn.rfm[STAGE_RRF_DEM] = tr_dse2rfm::type_id::create("toRFM", this);
        if(vn.spu[STAGE_RRF_DEM] == null) vn.spu[STAGE_RRF_DEM] = tr_dse2spu::type_id::create("toSPU", this);
        vn.eif[STAGE_RRF_DEM].op = ise.op;
        vn.eif[STAGE_RRF_DEM].req = 1;
        vn.eif[STAGE_RRF_DEM].pAdr = selExAdr;
        vn.eif[STAGE_RRF_DEM].cacheFill = wa || !selNoCache;
        vn.eif[STAGE_RRF_DEM].sgl = !ise.vec || ise.vecMode == 0;
        vn.eif[STAGE_RRF_DEM].cyc = ise.subVec & `GML(WID_DCHE_CL);
        for(int sp = 0; sp < NUM_SP; sp++) begin
          vn.rfm[STAGE_RRF_DEM].updateAdrRes[sp] = rfm.base[sp];
          if(spi[STAGE_RRF_DEM][sp].exp)
            vn.spu[STAGE_RRF_DEM].pres[sp] = 0;
          else
            vn.spu[STAGE_RRF_DEM].pres[sp] = spi[STAGE_RRF_DEM][sp].re;
        end
        
        vn.rfm[STAGE_RRF_DEM].wrGrp = ise.wrGrp;
        vn.rfm[STAGE_RRF_DEM].wrAdr = ise.wrAdr;
        vn.rfm[STAGE_RRF_DEM].wrBk = ise.wrBk;
        vn.rfm[STAGE_RRF_DEM].updateAdrWrBk = ise.updateAdrWrBk;
        vn.rfm[STAGE_RRF_DEM].updateAdrWrAdr = ise.updateAdrWrAdr;
        vn.rfm[STAGE_RRF_DEM].updateAdrWrGrp = ise.updateAdrWrGrp;
        vn.rfm[STAGE_RRF_DEM].subVec = ise.subVec;
        vn.spu[STAGE_RRF_DEM].tid = ise.tid;
        vn.spu[STAGE_RRF_DEM].wrEn = ise.updatePr;
      end
    end
    
    ///**sel stage, eif request
    if(v.fmEIF[STAGE_RRF_SEL] != null) begin
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_SEL];
      padr_t padr;
      uint adr, tagLo, tagHi, idx;
      bit hit = 0, hihit = 0, ehit = 0;
      uchar hi = 0, grp = 0, hiAso = 0, eAso = 0, cyc = eif.cyc;

      if(eif.loadRsp) begin
        padr = ldQue[eif.id].padr;
        for(int sp = 0; sp < NUM_SP; sp++) begin
          spi[STAGE_RRF_SXG0][sp] = new();
          spi[STAGE_RRF_SXG0][sp].xhg = ldQue[eif.id].xhg[eif.cyc][sp];
          spi[STAGE_RRF_SXG0][sp].exp = 0;
          spi[STAGE_RRF_SXG0][sp].oc = ldQue[eif.id].wrEn[eif.cyc][sp];
          spi[STAGE_RRF_SXG0][sp].ex = 0;
          spi[STAGE_RRF_SXG0][sp].re = 0;
          spi[STAGE_RRF_SXG0][sp].slot = ldQue[eif.id].xhg[eif.cyc][sp] >> (WID_WORD + WID_SMEM_BK);
          endian[STAGE_RRF_DEM] = ldQue[eif.id].endian;
        end
      end
      else if(eif.storeRsp) begin
        padr = stQue[eif.id].padr;
        endian[STAGE_RRF_DEM] = stQue[eif.id].endian;
      end
      else
        ovm_report_warning("sel", "eif tr has no rsp!");

      adr = padr & `GML(WID_SMEM_ADR);
      adr = adr & `GMH(1) + eif.cyc;
      tagLo = padr >> (WID_DCHE_IDX);
      tagHi = tagLo >> WID_DCHE_STAG;
      idx = adr >> WID_DCHE_IDX;
      tagLo = tagLo & `GML(WID_DCHE_STAG);
      
      for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++) begin
        bit empty = 1;
        for(int loTagIdx = 0; loTagIdx < NUM_SMEM_GRP; loTagIdx++) begin
          empty = empty && !cache[idx][hiTagIdx].tagV[loTagIdx];
          if(cache[idx][hiTagIdx].tagV[loTagIdx] && cache[idx][hiTagIdx].tagHi == tagHi && 
             cache[idx][hiTagIdx].tagLo[loTagIdx] == tagLo) begin
            hit = 1;
            hi = hiTagIdx;
            grp = loTagIdx;
          end
          if(cache[idx][hiTagIdx].tagHi == tagHi) begin
            hiAso = hiTagIdx;
            hihit = 1;
          end
        end
        if(empty)
          eAso = hiTagIdx;
      end
      if(hit) begin
        ///already have, discard this
      end
      else if(hihit) begin
        for(grp = cache[idx][hiAso].lo; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP && !cache[idx][hiAso].tagV[grp]; grp++);
        if(grp >= NUM_SMEM_GRP)
          for(grp = cache[idx][hiAso].lo; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP; grp++);
        cache[idx][hiAso].lo = grp;
        hi = hiAso;
      end
      else if(ehit) begin
        for(grp = 0; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP; grp++);
        cache[idx][hiAso].lo = grp;
        hi = eAso;
      end
      else begin
        uchar hi;
        hi = cacheSelHi[idx];
        hi = (hi++) & `GML(WID_DCHE_ASO);
        cacheSelHi[idx] = hi;
        for(grp = cache[idx][hiAso].lo; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP && !cache[idx][hiAso].tagV[grp]; grp++);
        cache[idx][hi].tagHi = tagHi;
        cacheFlush[STAGE_RRF_DEM] =  eif.alloc;
      end
      adr = (adr & `GML(WID_SMEM_ADR - WID_DCHE_ASO)) | (hi << (WID_SMEM_ADR - WID_DCHE_ASO));
      
      for(int bk = 0; bk < NUM_SP; bk++) begin
        ck[cyc][bk].sMemAdr = adr;
        ck[cyc][bk].sMemGrp = grp;
        ck[cyc][bk].sMemWEn = '{default : eif.alloc};
        ck[cyc][bk].ocEn = 1;
      end
      if(eif.last) begin
        if(eif.alloc) begin
          cache[idx][hi].tagV[grp] = 1;
          cache[idx][hi].tagLo[grp] = tagLo;
        end
        for(int i = 0; i < LAT_XCHG; i++)
          smi[STAGE_RRF_SXG0 - i] = ck[i];
        selExp = 0;
        selExRdy = 0;
        foreach(ck[i, j])
          ck[i][j] = new();
      end

      if(eif.loadRsp || eif.storeRsp) begin
        if(vn.rfm[STAGE_RRF_SXG0] == null) vn.rfm[STAGE_RRF_SXG0] = tr_dse2rfm::type_id::create("toRFM", this);
        
        vn.rfm[STAGE_RRF_SXG0].wrGrp = ldQue[eif.id].wrGrp;
        vn.rfm[STAGE_RRF_SXG0].wrAdr = ldQue[eif.id].wrAdr;
        vn.rfm[STAGE_RRF_SXG0].wrBk = ldQue[eif.id].wrBk;
        vn.rfm[STAGE_RRF_SXG0].updateAdrWrBk = 0;
        vn.rfm[STAGE_RRF_SXG0].updateAdrWrAdr = 0;
        vn.rfm[STAGE_RRF_SXG0].updateAdrWrGrp = 0;
        vn.rfm[STAGE_RRF_SXG0].subVec = ldQue[eif.id].subVec;
        
        if(cacheFlush[STAGE_RRF_SEL]) begin
          if(vn.eif[STAGE_RRF_SXG0] == null) vn.eif[STAGE_RRF_SXG0] = tr_dse2eif::type_id::create("toRFM", this);
          vn.eif[STAGE_RRF_SXG0].op = ldQue[eif.id].op;
          vn.eif[STAGE_RRF_SXG0].req = 1;
          vn.eif[STAGE_RRF_SXG0].cacheFlush = 1;
          vn.eif[STAGE_RRF_SXG0].pAdr = padr;
          vn.eif[STAGE_RRF_SXG0].cyc = eif.cyc;
        end
      end
      
      ///que is released here
      if(eif.last) begin
        if(eif.storeRsp)
          stQue[eif.id].en = 0;
        if(eif.loadRsp)
          ldQue[eif.id].en = 0;
      end
    end

    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_TAG; i--) 
      tlbReqVAdr[i] = tlbReqVAdr[i - 1];    

    ///select vadr from ise req to tlb for translation
    if(v.fmISE[STAGE_RRF_AG] != null && v.fmRFM[STAGE_RRF_AG] != null
       && v.fmSPU[STAGE_RRF_AG] != null && v.fmISE[STAGE_RRF_AG].en) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_AG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_AG];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
      int vadr = 0;
      bit found = 0;
      
      if(!ise.vec) begin
        found = 1;
        vadr = rfm.base[0];
      end
      else begin
        foreach(spu.emsk[i]) begin
          if(spu.emsk[i]) begin
            rfm.base[i] = rfm.base[i] + rfm.os[i];
            if(rfm.base[i] >= VADR_MAPPED && rfm.base[i] < VADR_NMAPNC) begin
             found = 1;
             vadr = rfm.base[i];
            end
          end
        end
      end
      
      if(found && ise.en && (!tlbRdy || (tlbReqVAdr[STAGE_RRF_TAG] != toTLB.vAdr))) begin
        tlbRdy = 1;
        toTLB = tr_dse2tlb::type_id::create("toTLB", this);
        toTLB.vAdr = vadr >> VADR_START;
        toTLB.op = ise.op;
        toTLB.tid = ise.tid;
        toTLB.req = 1;
        toTLB.k = ise.priv;
        tlbReqVAdr[STAGE_RRF_TAG] = toTLB.vAdr;
      end
    end
    
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toEIF != null) void'(eif_tr_port.nb_transport(toEIF, toEIF));
    if(toTLB != null) void'(tlb_tr_port.nb_transport(toTLB, toTLB));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2dse req, output tr_ise2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[STAGE_RRF_RRC0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2dse req, output tr_rfm2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM[STAGE_RRF_AG] = req;
    if(vn.fmRFM[STAGE_RRF_TAG] != null)
      vn.fmRFM[STAGE_RRF_TAG].st = req.st;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2dse req, output tr_spu2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU[STAGE_RRF_AG] = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2dse req, output tr_spa2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_tlb(input tr_tlb2dse req, output tr_tlb2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get tlb Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmTLB = req;
    return 1;
  endfunction : nb_transport_tlb

  function bit nb_transport_eif(input tr_eif2dse req, output tr_eif2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get EIF Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmEIF[STAGE_RRF_AG] = req;
    return 1;
  endfunction : nb_transport_eif
          
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time == stamp) begin
       ovm_report_info("sync", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("sync", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
    ///--------------------synchronizing-------------------
    v.copy(vn);
    comb_proc();
  endfunction : sync

  task run();
    forever begin
      @(posedge sysif.clk);
      sync();
      req_proc();
    end
  endtask : run

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
    
  virtual function void build();
    ovm_object tmp;
    tlm_vif_object vifCfg;
    
    super.build();
    ise_tr_imp = new("ise_tr_imp", this);
    rfm_tr_imp = new("rfm_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    tlb_tr_imp = new("tlb_tr_imp", this);
    eif_tr_imp = new("eif_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    eif_tr_port = new("eif_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
     
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    srCacheGrp = 0;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
