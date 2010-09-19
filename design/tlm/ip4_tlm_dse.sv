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
  ushort xhg[LAT_XCHG][NUM_SP];
  bit wrEn[LAT_XCHG][NUM_SP], en;///, endian;
  uchar wrGrp, wrAdr, wrBk, tid, subVec;
  opcode_e op;
///  exadr_t exAdr;
}ldQue_t;

typedef struct{
  uchar tid;
///  exadr_t exAdr;
  bit en;///, endian;
}stQue_t;

typedef struct{
  padr_t adr;
  uchar tid[LAT_XCHG][NUM_SP];
  bit en[LAT_XCHG][NUM_SP];
}ll_ck_t;

typedef struct{
  uint tagHi, tagLo[NUM_SMEM_GRP];
  cache_state_t state[NUM_SMEM_GRP];
  uchar lo, hi;
}cache_t;

class sm_t;
///sel stage data struct
  bit sMemOpy[NUM_SP],   ///occupy for onchip shared mem
      exEn[NUM_SP][WORD_BYTES],  ///ext enabled
      sMemWEn[NUM_SP][WORD_BYTES];
  uint sMemAdr[NUM_SP], sMemGrp[NUM_SP];  ///on chip adr grp
  wordu stData[NUM_SP]; ///store exchange buffer

  bit exp[NUM_SP], oc[NUM_SP], ex[NUM_SP], re[NUM_SP];
  uchar slot[NUM_SP];
  ushort xhg[NUM_SP]; ///cl + bk + os
endclass

///class sp_t;
///  bit exp[NUM_SP], oc[NUM_SP], ex[NUM_SP], re[NUM_SP];
///  uchar slot[NUM_SP];
///  ushort xhg[NUM_SP]; ///cl + bk + os
///endclass

///---------------------------------------main component----------------------------------------
class ip4_tlm_dse extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_dse_vars v, vn;  
  local wordu sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SP];
  local cache_t cache[NUM_DCHE_TAG][NUM_DCHE_ASO];
  
  local bit cacheFlush[STAGE_RRF_LXG:STAGE_RRF_DEM],
            exReq[STAGE_RRF_LXG:STAGE_RRF_DEM],
            endian[STAGE_RRF_LXG:STAGE_RRF_DEM],
            expReq[STAGE_RRF_LXG:STAGE_RRF_DEM],
            exQueAlloc[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local uchar exQueId[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local cause_dse_t expCause[STAGE_RRF_LXG:STAGE_RRF_DEM];
  
  local bit[STAGE_RRF_LXG:0] cancel[NUM_THREAD];

  local word tlbReqVAdr[STAGE_RRF_SEL:STAGE_RRF_TAG];
  local tr_tlb2dse tlbCached;
  local bit tlbRdy;
  
  local bit selExRdy, selExp, selExpReq, selValidReq, selNoCache,
            selNeedLock2CL, selLock2CL, selCacheRdy, selEndian, selCoherency,
            selWriteAlloc, selQueRdy;
  local exadr_t selExAdr;
  local cause_dse_t selCause;
  local uint selCacheIdx;
  local uchar selCacheGrp, selCacheAso;
  
  local uchar srCacheGrp;
  local uint srMapBase;
  local bit cacheGrpEn[NUM_SMEM_GRP];
  local bit sendExp;
  local bit[CYC_VEC - 1 : 0] dcReRun;
  
  local sm_t ck[LAT_XCHG],
             smi[STAGE_RRF_LXG:STAGE_RRF_DEM];
///  local sp_t spi[STAGE_RRF_LXG:STAGE_RRF_SXG0];
  
  local wordu dcXhgData[STAGE_RRF_LXG:STAGE_RRF_DC][NUM_SP],
              dcFlushData[STAGE_RRF_LXG:STAGE_RRF_DC][NUM_SP];
  local bit dcVrfWEn[STAGE_RRF_LXG:STAGE_RRF_DC][NUM_SP];
    
  local ldQue_t ldQue[NUM_LDQUE];
  local stQue_t stQue[NUM_STQUE];
  local uchar pbId;
  local string smFilePath;
  
  local ll_ck_t llCk[NUM_LLCK];
  local uchar llNext;
  
  `ovm_component_utils_begin(ip4_tlm_dse)
    `ovm_field_int(pbId, OVM_ALL_ON)
    `ovm_field_string(smFilePath, OVM_ALL_ON)
    `ovm_field_int(srCacheGrp, OVM_ALL_ON)
    `ovm_field_int(srMapBase, OVM_ALL_ON)
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
   
    if(v.fmISE[0] != null) end_tr(v.fmISE[0]);
    if(v.fmRFM[STAGE_RRF_AG] != null) end_tr(v.fmRFM[STAGE_RRF_AG]); 
    if(v.fmSPU[STAGE_RRF_AG] != null) end_tr(v.fmSPU[STAGE_RRF_AG]);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmTLB != null) end_tr(v.fmTLB);
    if(v.fmEIF[STAGE_RRF_AG] != null) end_tr(v.fmEIF[STAGE_RRF_AG]);
    
    vn.fmISE[0] = null;
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
      cancel[v.fmISE[STAGE_RRF_DPRB].tid] |= `GML(STAGE_RRF_DPRW);
    sendExp = 0;
    
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
    
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_DC; i--) begin
      dcXhgData[i] = dcXhgData[i - 1];
      dcFlushData[i] = dcFlushData[i - 1];
      dcVrfWEn[i] = dcVrfWEn[i - 1];
    end
      
    if(v.fmTLB != null)
      tlbCached = v.fmTLB;
      
    ///rfm data ring & bypass to support perb instructions
    if(v.fmISE[STAGE_RRF_SEL] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      if(ise.op inside {op_pera, op_perb, op_tmrf}) begin
        if(ise.subVec >= (CYC_VEC >> 1)) begin
          word tmp[NUM_SP];
          ///SXG stages loop mode start
          if(v.fmRFM[STAGE_RRF_SXG] != null)
            tmp = v.fmRFM[STAGE_RRF_SXG].st;
          if(vn.fmRFM[STAGE_RRF_DC] != null && vn.fmRFM[STAGE_RRF_SXG0] != null)
            vn.fmRFM[STAGE_RRF_DC].st = vn.fmRFM[STAGE_RRF_SXG0].st;
          if(vn.fmRFM[STAGE_RRF_SXG0] != null)
            vn.fmRFM[STAGE_RRF_SXG0].st = tmp;
          if(ise.op inside {op_pera, op_tmrf}) begin
            bit tmp[NUM_SP];
            ///spu need loop too
            if(v.fmSPU[STAGE_RRF_SXG] != null)
              tmp = v.fmSPU[STAGE_RRF_SXG].emsk;
            if(vn.fmSPU[STAGE_RRF_DC] != null && vn.fmSPU[STAGE_RRF_SXG0] != null)
              vn.fmSPU[STAGE_RRF_DC].emsk = vn.fmSPU[STAGE_RRF_SXG0].emsk;
            if(vn.fmSPU[STAGE_RRF_SXG0] != null)
              vn.fmSPU[STAGE_RRF_SXG0].emsk = tmp;            
          end
        end
      end
    end

    if(v.fmISE[STAGE_RRF_DC] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DC];
      if(ise.op inside {op_pera, op_perb, op_tmrf}) begin
        if(ise.subVec >= (CYC_VEC >> 1)) begin
          if(vn.fmRFM[STAGE_RRF_LXG0] != null && vn.fmRFM[STAGE_RRF_LXG] != null)
            vn.fmRFM[STAGE_RRF_SXG0].st = vn.fmRFM[STAGE_RRF_LXG].st;
          if(ise.op inside {op_pera, op_tmrf}) begin
            if(vn.fmSPU[STAGE_RRF_SXG0] != null && vn.fmSPU[STAGE_RRF_SXG] != null)
              vn.fmSPU[STAGE_RRF_SXG0].emsk = vn.fmSPU[STAGE_RRF_SXG].emsk;
          end
        end
      end
    end
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
    
    ///eif query cache state
    if(v.fmEIF[STAGE_RRF_AG] != null) begin
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_AG];
      exadr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
              smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_GRP - srCacheGrp) * SGRP_SIZE,
              smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
      if(eif.queryCacheState) begin
        exadr_t exAdr = eif.exAdr;
        uint adr, tagLo, tagHi, idx;
        adr = exAdr & `GML(WID_SMEM_ADR);
        adr = adr & `GMH(1) + eif.cyc;
        tagLo = exAdr >> (WID_DCHE_CL + WID_DCHE_IDX);
        tagHi = tagLo >> WID_DCHE_STAG;
        idx = adr >> WID_DCHE_IDX;
        tagLo = tagLo & `GML(WID_DCHE_STAG);
        if(toEIF == null) toEIF = tr_dse2eif::type_id::create("toEIF", this);
        toEIF.queryNoHit = 1;
        if(exAdr < smStart && exAdr >= smEnd && srCacheGrp > 0) begin
          ///in ext rang, check cache
          for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++) begin        
            for(int loTagIdx = (NUM_SMEM_GRP - srCacheGrp); loTagIdx < NUM_SMEM_GRP; loTagIdx++) begin
              if(cache[idx][hiTagIdx].state[loTagIdx] != cs_inv && cache[idx][hiTagIdx].tagHi == tagHi && 
                 cache[idx][hiTagIdx].tagLo[loTagIdx] == tagLo) begin
                toEIF.queryRes = cache[idx][hiTagIdx].state[loTagIdx];
                toEIF.queryNoHit = 0;
                if(eif.queryAndUpdate)
                  cache[idx][hiTagIdx].state[loTagIdx] = eif.state;
                break;
              end
            end
          end
        end
      end
    end
    
    ///**dc stage
    if(smi[STAGE_RRF_DC] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DC];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DC];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DC];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_DC];
      
      wordu xhgData[NUM_SP], eifRes[NUM_SP], spRes[NUM_SP], smWData[NUM_SP];
      uchar st = `SG(STAGE_RRF_DC, STAGE_RRF_DC - 1, STAGE_RRF_AG),
            cyc;
      bit last = 0, exRsp = 0, shf4 = 0, per = 0, tmsg = 0, fmsg = 0, 
          vXhgEn, exWrSM, exRd = 0, exFlush = 0;
      if(ise != null && ise.en) begin
        last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
        shf4 = ise.op inside {op_shf4a, op_shf4b};
        per = ise.op inside {op_pera, op_perb};
        vXhgEn = ise.op inside {op_pera, op_shf4a, op_tmrf};
        tmsg = ise.op == op_tmrf;
        fmsg = ise.op == op_fmrf;
        cyc = ise.subVec & `GML(WID_DCHE_CL);
      end
      if(eif != null) begin
        bit allocFail = 0;
        exFlush = cacheFlush[STAGE_RRF_DC];
        exRd = eif.rd;
        if(v.eif[STAGE_RRF_DC] != null)
          allocFail = v.eif[STAGE_RRF_DC].allocFail;
        exRsp = (eif.loadRsp || eif.storeRsp);
        exWrSM = eif.wr && !allocFail;
        if(eif.loadRsp || eif.storeRsp)
          last = eif.last;
      end
      
      ///shared memory write source select
      if(exWrSM && v.fmEIF[st] != null) begin
        ///eif data comes late
        if(endian[STAGE_RRF_DC])
          foreach(smWData[bk])
            smWData[bk] = v.fmEIF[st].data[bk];
        else
          foreach(smWData[bk])
            smWData[bk] = v.fmEIF[st].data[WORD_BYTES - 1 - bk];
      end
      else begin
        if(ise != null && ise.op inside {st_ops} && smi[STAGE_RRF_DC] != null)
          foreach(smWData[bk])
            smWData[bk] = smi[STAGE_RRF_DC].stData[bk];
      end
      
      ///shared memory write
      for(int bk = 0; bk < NUM_SP; bk++) begin
        uint adr = smi[STAGE_RRF_DC].sMemAdr[bk],
             grp = smi[STAGE_RRF_DC].sMemGrp[bk];
        bit wEn[WORD_BYTES] = smi[STAGE_RRF_DC].sMemWEn[bk];
        if(exWrSM && eif != null)
          foreach(wEn[os])
            wEn[os] = eif.byteEn[bk][os];
        foreach(wEn[os])
          if(wEn[os]) begin
            sharedMem[grp][adr][bk].b[os] = smWData[bk].b[os];
            ovm_report_info("dc_wr", $psprintf("grp %0d, adr %0d, bk %0d, os %0d, data 0x%0h", grp, adr, bk, os, smWData[bk].b[os]), OVM_HIGH);
          end
      end
      
      ///sharedMem read
      for(int bk = 0; bk < NUM_SP; bk++) begin
        uint adr = smi[STAGE_RRF_DC].sMemAdr[bk],
             grp = smi[STAGE_RRF_DC].sMemGrp[bk],
             adr2 = adr ^ 'b01;   ///flip last bit
        if(exFlush)
          dcFlushData[STAGE_RRF_DC][bk] = sharedMem[grp][adr2][bk];
        else if(exRd)
          dcFlushData[STAGE_RRF_DC][bk] = sharedMem[grp][adr][bk];
        
        if(exFlush)
          xhgData[bk] = smWData[bk];
        else if((per || shf4 || (ise != null && ise.op inside {st_ops})) && rfm != null)
          xhgData[bk] = rfm.st[bk];
        else
          xhgData[bk] = sharedMem[grp][adr][bk];
        ovm_report_info("dc_rd", $psprintf("grp %0d, adr %0d, bk %0d, xhgData 0x%0h", grp, adr, bk, xhgData[bk]), OVM_HIGH);
      end
          
      ///load data exchange
      for(int sp = 0; sp < NUM_SP; sp++) begin
        bit wEn[NUM_SP];
        uchar cmp;
        ///dcXhgData dcVrfWEn initial value
        if(per || shf4 || tmsg) begin
          dcXhgData[STAGE_RRF_DC][sp] = smi[STAGE_RRF_DC].stData[sp];
          if(vXhgEn)
            dcVrfWEn[STAGE_RRF_DC][sp] = smi[STAGE_RRF_DC].sMemOpy[sp];
          else
            dcVrfWEn[STAGE_RRF_DC][sp] = spu.emsk[sp];
        end
        else ///ld reqs
          dcVrfWEn[STAGE_RRF_DC][sp] = smi[STAGE_RRF_DC].oc[sp] || smi[STAGE_RRF_DC].ex[sp];
          
        if(per || tmsg) begin
          cmp = cyc + LAT_XCHG;
          if(spu != null)
            wEn = spu.emsk;
        end
        else if(shf4) begin
          cmp = CYC_VEC;    ///not exchange, finished in sxg stages
        end
        else begin ///ld ex
          cmp = cyc;
///          foreach(wEn[i])
///            wEn[i] = smi[st].oc[i] || smi[st].ex[i];              
        end
                    
        for(int slot = 0; slot < LAT_XCHG; slot++) begin
          uchar st = STAGE_RRF_DC + slot,
                bk;
          
          if(smi[st] == null) continue;
          bk = smi[st].xhg[sp] >> WID_WORD & `GML(WID_SMEM_BK);
          
          if(smi[st].slot[sp] == cmp) begin
            dcXhgData[st][sp] = xhgData[bk];
            if(vXhgEn)
              dcVrfWEn[st][sp] = wEn[bk];
///            else
///              dcVrfWEn[stt][sp] = wEn[sp];
            ovm_report_info("dc_lxg", $psprintf("slot %0d, sp %0d, cmp %0d, bk %0d, dcXhgData[st][sp] 0x%0h, dcVrfWEn[st][sp] %0d",
                            slot, sp, cmp, bk, xhgData[bk], dcVrfWEn[st][sp]), OVM_HIGH);
          end
        end
      end
    end
    
    ///now exception & cancel is resolved
    if(v.fmISE[STAGE_RRF_DPRB] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DPRB];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DPRB];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DPRB];
      bit last = 0, exRsp = 0;
      uchar queId = exQueId[STAGE_RRF_DPRB],
            cyc = ise.subVec & `GML(WID_DCHE_CL);
      
      if(ise.en)
        last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(eif != null && (eif.loadRsp || eif.storeRsp)) begin
        exRsp = 1;
        last = eif.last;
      end
      
      if(exReq[STAGE_RRF_DPRB] && smi[STAGE_RRF_DPRB] != null) begin
        if(v.eif[STAGE_RRF_DPRB] == null) v.eif[STAGE_RRF_DPRB] = tr_dse2eif::type_id::create("toEIF", this);
        v.eif[STAGE_RRF_DPRB].id = queId;
        if(ise.op inside {ld_ops}) begin
          if(last) begin
            ldQue[queId].wrGrp = ise.wrGrp;
            ldQue[queId].wrBk = ise.wrBk;
            ldQue[queId].wrAdr = ise.wrAdr;
            ldQue[queId].tid = ise.tid;
            ldQue[queId].op = ise.op;
          end
          ldQue[queId].wrEn[cyc] = smi[STAGE_RRF_DPRB].oc;
          ldQue[queId].xhg[cyc] = smi[STAGE_RRF_DPRB].xhg;
          if(last)
            ldQue[queId].subVec = ise.subVec;
        end
        else if(ise.op inside {st_ops})
          stQue[queId].tid = ise.tid;
      end
      
      if(cancel[ise.tid][STAGE_RRF_DPRB]) begin
        if(v.rfm[STAGE_RRF_DPRB] != null) begin
          v.rfm[STAGE_RRF_DPRB].srfWr = 0;
          v.rfm[STAGE_RRF_DPRB].vrfWr = 0;
          v.rfm[STAGE_RRF_DPRB].uaWrEn = 0;
          v.rfm[STAGE_RRF_DPRB].exp = 0;
        end
        if(v.spu[STAGE_RRF_DPRB] != null)
          v.spu[STAGE_RRF_DPRB].wrEn = 0;
        if(ise.op inside {ld_ops, st_ops, op_tmrf} && v.eif[STAGE_RRF_DPRB] != null)
          v.eif[STAGE_RRF_DPRB].op = op_nop;
        if(exQueAlloc[STAGE_RRF_DPRB]) begin
          if(ise.op inside {ld_ops})
            ldQue[queId].en = 0;
          else if(ise.op inside {st_ops})
            stQue[queId].en = 0;
        end
      end
    end
    
    ///send exp to other module
    if(v.fmISE[STAGE_RRF_DEM] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DEM];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DEM];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DEM];
      bit last = 0, exRsp = 0;
      
      if(ise.en)
        last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(eif != null && (eif.loadRsp || eif.storeRsp)) begin
        exRsp = 1;
        last = eif.last;
      end
      
      if(ise.vecMode == ise.subVec) begin
        if(toISE == null) toISE = tr_dse2ise::type_id::create("toISE", this);
        toISE.rsp = 1;
        toISE.exp = expReq[STAGE_RRF_DEM];
        toISE.scl = ise.vec == 0;
        toISE.tid = ise.tid;
        toISE.vecMode = ise.vecMode;
        toISE.pendExLoad = 0;
        toISE.pendExStore = 0;
        toISE.cause = expCause[STAGE_RRF_DEM];
        toISE.reRun = dcReRun;
        dcReRun = 0;
        foreach(ldQue[i])
          if(ldQue[i].en && ldQue[i].tid == ise.tid)
            toISE.pendExLoad++;
        foreach(stQue[i])
          if(stQue[i].en && stQue[i].tid == ise.tid)
            toISE.pendExStore++;
                  
        if(expReq[STAGE_RRF_DEM]) begin
          if(toSPU == null) toSPU = tr_dse2spu::type_id::create("toSPU", this);
          if(toRFM == null) toRFM = tr_dse2rfm::type_id::create("toRFM", this);
          toSPU.cancel = 1;
          toSPU.tidCancel = ise.tid;
          toRFM.exp = 1;
          toRFM.tidExp = ise.tid;
          toRFM.vecModeExp = ise.vecMode;
        end
      end
    end
    
    ///now load exchange data is ready, set them to eif & rfm tr
    if(smi[STAGE_RRF_LXG] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_LXG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_LXG];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_LXG];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_LXG];
      bit rfmReq = 0, last = 0;
      opcode_e op;
        
      if(eif != null && eif.loadRsp) begin
        if(v.eif[STAGE_RRF_LXG] != null)
          op = v.eif[STAGE_RRF_LXG].op;
        rfmReq = 1;
        last = v.eif[STAGE_RRF_LXG].last;
      end
      else if(ise != null && ise.op inside {ld_ops, op_shf4a, op_shf4b, op_pera, op_perb, op_fmrf, op_tmrf}) begin
        rfmReq = 1;
        op = ise.op;
        last = ise.subVec == ise.vecMode;
      end
      
      if(rfmReq) begin
        if(v.rfm[STAGE_RRF_LXG] == null) v.rfm[STAGE_RRF_LXG] = tr_dse2rfm::type_id::create("toRFM", this);
        for(int sp = 0; sp < NUM_SP; sp++) begin
          uchar os = smi[STAGE_RRF_LXG].xhg[sp] & `GML(WORD_BYTES);
          wordu res = dcXhgData[STAGE_RRF_LXG][sp];
          case(op)
          op_shf4a, 
          op_shf4b, 
          op_pera, 
          op_perb,
          op_lw:    v.rfm[STAGE_RRF_LXG].res[sp] = res;
          op_lh:    v.rfm[STAGE_RRF_LXG].res[sp] = {{WORD_BITS{res.h[os >> WID_HALF].b[7]}}, res.h[os >> WID_HALF]};
          op_lhu:   v.rfm[STAGE_RRF_LXG].res[sp] = res.h[os >> WID_HALF];
          op_lb:    v.rfm[STAGE_RRF_LXG].res[sp] = res.b[os];
          op_lbu:   v.rfm[STAGE_RRF_LXG].res[sp] = {{WORD_BITS{res.b[os][7]}}, res.b[os]};
          op_fmrf:  v.rfm[STAGE_RRF_LXG].res[sp] = res;
          op_tmrf:  v.rfm[STAGE_RRF_LXG].res[sp] = res;
          endcase
        end
        v.rfm[STAGE_RRF_LXG].wrEn = dcVrfWEn[STAGE_RRF_LXG];
        v.rfm[STAGE_RRF_LXG].expVec = smi[STAGE_RRF_LXG].exp;
      end
      
      if(cacheFlush[STAGE_RRF_LXG]) begin
        ///cache flush
        if(v.eif[STAGE_RRF_LXG] == null) v.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
        v.eif[STAGE_RRF_LXG].data = dcFlushData[STAGE_RRF_LXG];
        v.eif[STAGE_RRF_LXG].byteEn = '{default : '1};
      end
      else if(ise != null && exReq[STAGE_RRF_LXG] && !cancel[ise.tid][STAGE_RRF_LXG] && !expReq[STAGE_RRF_LXG]) begin
        if(v.eif[STAGE_RRF_LXG] == null) v.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
        ///dse reqs
        if(ise.op inside {st_ops}) begin
          v.eif[STAGE_RRF_LXG].data = dcXhgData[STAGE_RRF_LXG];
          foreach(v.eif[STAGE_RRF_LXG].byteEn[i, j])
            v.eif[STAGE_RRF_LXG].byteEn[i][j] = smi[STAGE_RRF_LXG].exEn[i][j];
        end
        else if(ise.op inside {op_tmrf}) begin
          ///tmrf will use part of dcVrfWEn
          if(v.eif[STAGE_RRF_LXG] == null) v.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
          v.eif[STAGE_RRF_LXG].data = dcXhgData[STAGE_RRF_LXG];
          v.eif[STAGE_RRF_LXG].mrfAdr = ise.mrfAdr;
          v.eif[STAGE_RRF_LXG].op = ise.op;
          if(rfm != null) begin
            uchar shiftNum;
            uint mask, maskEx, maskRf;
            shiftNum = rfm.os;
            if(shiftNum > (ise.subVec * NUM_SP))
              shiftNum -= ise.subVec * NUM_SP;
            mask = -1 << shiftNum;
            if(ise.sendRotRight) begin
              maskEx = ~mask;
              maskRf = mask;
            end
            else begin
              maskEx = mask;
              maskRf = ~mask;
            end
            foreach(v.eif[STAGE_RRF_LXG].byteEn[i, j])
              v.eif[STAGE_RRF_LXG].byteEn[i][j] = maskEx[i];
            foreach(v.rfm[STAGE_RRF_LXG].wrEn[i])
              v.rfm[STAGE_RRF_LXG].wrEn[i] = maskRf[i];  
          end
        end
        else if(ise.op inside {op_fmrf}) begin
          if(v.eif[STAGE_RRF_LXG] == null) v.eif[STAGE_RRF_LXG] = tr_dse2eif::type_id::create("toEIF", this);
          v.eif[STAGE_RRF_LXG].mrfAdr = ise.mrfAdr;
          v.eif[STAGE_RRF_LXG].op = ise.op;
        end
        else
          v.eif[STAGE_RRF_LXG] = null;
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
        if(smi[STAGE_RRF_EXE0] != null)
          smi[STAGE_RRF_EXE0].sMemWEn = '{default : 0};
        if(smi[STAGE_RRF_EXE0] != null) begin
          smi[STAGE_RRF_EXE0].oc = '{default : 0};
          smi[STAGE_RRF_EXE0].ex = '{default : 0};
          smi[STAGE_RRF_EXE0].re = '{default : 0};
        end 
      end
    end
    
    ///do pip shift before sel stage
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_DEM; i--) begin
      smi[i] = smi[i - 1];
      expReq[i] = expReq[i - 1];
      expCause[i] = expCause[i- 1];
      cacheFlush[i] = cacheFlush[i - 1];
      exReq[i] = exReq[i - 1];
      endian[i] = endian[i - 1];
      exQueId[i] = exQueId[i - 1];
      exQueAlloc[i] = exQueAlloc[i - 1];
    end
    smi[STAGE_RRF_DEM] = null;
    exQueAlloc[STAGE_RRF_DEM] = 0;
                  
    ///**sel stage, ld st request
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]
        && v.fmISE[STAGE_RRF_SEL].op inside {ld_ops, st_ops}) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
             smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_GRP - srCacheGrp) * SGRP_SIZE,
             smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
      uint tlbVAdr;
      bit last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec,
          needExOc = 0, accCache = 0;
      uchar maxSlot = ise.vecMode, cyc = ise.subVec & `GML(WID_DCHE_CL);
      padr_t lladr;
      bit llrdy;
      uchar llid;
      
      if((ise.vecMode - ise.subVec) < LAT_XCHG) begin
        while(maxSlot > LAT_XCHG)
          maxSlot -= LAT_XCHG;
      end
      else
        maxSlot = LAT_XCHG - 1;
        
      exReq[STAGE_RRF_DEM] = 0;
      if(tlb == null) tlb = tlbCached;
      if(tlb != null) begin
        tlbVAdr = tlbReqVAdr[STAGE_RRF_SEL] >> tlb.eobit;
        if(!selExRdy) begin
          selEndian = tlb.endian;
          selWriteAlloc = tlb.writeAlloc;
          selCoherency = tlb.coherency;
          selNoCache = !tlb.cached;
          needExOc = tlb.writeThru && ise.op inside {st_ops};
          selNeedLock2CL |= tlb.coherency && ise.op inside {st_ops};
///          selNeedLock2CL |= ise.op inside {op_ll, op_sc};
        end
      end
      
      foreach(rfm.base[sp]) begin
        padr_t padr;
        bit nc, exp;
        bit oc = spu.emsk[sp] && ise.en,
            ex = spu.emsk[sp] && ise.en && !ise.noExt,
            ocWEn;
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
        else if(tlb != null) begin
          padr = rfm.base[sp];
          if(!spu.emsk[sp]) begin
            ex = 0;
            oc = 0;
          end
          if(!selExp && tlb.exp) begin
            selExp = 1;
            exp = 1;
            selCause = tlb.cause;
          end
          if((rfm.base[sp] >> (VADR_START + tlb.eobit)) != tlbVAdr || tlb.exp) begin
            oc = 0;
            ex = 0;
          end
          nc = !tlb.cached;
          for(int j = (VADR_START + tlb.eobit); j < PADR_WIDTH; j++)
            padr[j] = tlb.pfn[j - VADR_START];
        end
        else begin
          if(tlb == null)
            ovm_report_warning("sel", "tlb info missing!");
          oc = 0;
          ex = 0;
        end
      
        if(padr > smStart && padr < smEnd2)
          ex = 0;
        
        ///access data
        if(oc || ex) begin
          ///align exp
          if((ise.op inside {op_lh, op_sh, op_lhu, op_cmpxchg} && padr[0] != 1'b0)
             || (ise.op inside {ld_ops, op_fetadd} && padr[1:0] != 2'b0)) begin
            if(!selExp) begin
              selCause = EC_ADRALG;
              exp = 1;
              selExp = 1;
            end
            ex = 0;
            oc = 0;
          end
  
          os = padr & `GML(WID_WORD);        
          bk = (padr >> WID_WORD) & `GML(WID_SMEM_BK);
          adr = padr >> (WID_WORD + WID_SMEM_BK) & `GML(WID_SMEM_ADR);
          grp = (padr >> (WID_WORD + WID_SMEM_BK + WID_SMEM_ADR)) & `GML(WID_SMEM_GRP);
          
          ///----------------------start access----------------------------
          ///external mem
          ///**cache address:   | tagHi | tagLo | idx | cl | bk | offset |
          if(ex) begin
            bit hit = 0;
            uint idx;
            
            ///chk cache for match
            if(!nc && oc) begin ///when oc is possible
              uint tagLo = padr >> (WID_WORD + WID_SMEM_BK + WID_DCHE_CL + WID_DCHE_IDX),
                   tagHi = tagLo >> WID_DCHE_STAG;
              tagLo = tagLo & `GML(WID_DCHE_STAG);
              idx = adr >> WID_DCHE_IDX;
              
              for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++) begin
                for(int loTagIdx = NUM_SMEM_GRP - srCacheGrp; loTagIdx < NUM_SMEM_GRP; loTagIdx++) begin
                  if(!selCacheRdy || selCacheIdx == idx) begin
                    ///echo cyc can noly access one cache tag
                    selCacheRdy = 1;
                    selCacheIdx = idx;
                    if(!selLock2CL || selCacheGrp == loTagIdx) begin
                      selCacheGrp = loTagIdx;
                      selCacheAso = hiTagIdx;
                      hit = cache[idx][hiTagIdx].tagHi == tagHi && 
                            cache[idx][hiTagIdx].tagLo[loTagIdx] == tagLo;
                      if(cache[idx][hiTagIdx].state[loTagIdx] == cs_inv)
                        hit = 0;
                      if(ise.op inside {st_ops} && cache[idx][hiTagIdx].state[loTagIdx] == cs_shared)
                        hit = 0;
                      if(hit) begin
                        grp = loTagIdx;
                        adr = (adr & `GML(WID_SMEM_ADR - WID_DCHE_ASO)) | (hiTagIdx << (WID_SMEM_ADR - WID_DCHE_ASO));
                        if(cache[idx][hiTagIdx].hi >= 2)
                          cache[idx][hiTagIdx].hi -= 2;
                        else
                          cache[idx][hiTagIdx].hi = 0;
                        if(!needExOc)
                          ex = 0;
                        break;
                      end                    
                    end
                  end
                  else begin
                    ///can't access this time, unknown if onchip or not
                    oc = 0;
                    ex = 0;
                  end
                end
              end
            end
            else
              oc = 0;
              
            ///case when ex can fail, then bank select is not necessory
            if(needExOc && selExRdy && selExAdr != (padr >> (WID_SMEM_BK + WID_WORD)))
              oc = 0;
              
            ///cache hit, allocate exchange bank
            if(oc) begin
              oc = 0;
              for(int s = 0; s <= maxSlot; s++) begin
                if((ck[s].sMemAdr[bk] == adr && ck[s].sMemGrp[bk] == grp) || !ck[s].sMemOpy[bk]) begin
                  ck[s].sMemOpy[bk] = 1;
                  slot = s;
                  oc = 1;
                  break;
                end
              end
            end
            
            if(needExOc && !oc)
              ex = 0;
              
            ///external access
            if(ex) begin
              if(!selExRdy) begin
                selExRdy = 1;
                selExAdr = padr >> (WID_SMEM_BK + WID_WORD);
              end
              else begin
                bit exhit = (selExAdr >> maxSlot) == (padr >> (maxSlot + WID_SMEM_BK + WID_WORD));
                if(!exhit)
                  ex = 0;
              end
            end
            
            if(needExOc && !ex)
              oc = 0;
                        
            accCache |= oc;
            ///lock to this cache line if accessed
            selLock2CL = selNeedLock2CL && accCache;
            ///if write to owner without need2lockcl change it to dirty
            if(oc && !selNeedLock2CL && cache[selCacheIdx][selCacheAso].state[selCacheGrp] == cs_exclusive)
              cache[selCacheIdx][selCacheAso].state[selCacheGrp] = cs_dirty;
          end
          ///**shared mem
          else if(oc) begin
            if(padr >= smEnd) begin
              if(!selExp) begin
                selCause = EC_SMBOND;
                selExp = 1;
                exp = 1;
              end
              oc = 0;
            end
            
            begin
              bit find = 0;
              for(int s = 0; s < LAT_XCHG; s++) begin
                if((ck[s].sMemAdr[bk] == adr && ck[s].sMemGrp[bk] == grp) || !ck[s].sMemOpy[bk]) begin
                  ck[s].sMemOpy[bk] = oc;
                  slot = s;
                  find = 1;
                  break;
                end
              end
              oc = oc && find;
            end
          end
          
          ///load link & store conditional
          if(ise.op inside {op_ll, op_sc} && (oc || ex)) begin
            bit found = 0, failed = 1;
            uchar cl = padr >> (WID_WORD + WID_SMEM_BK) & `GML(WID_DCHE_CL);
            uint tag = adr >> (WID_WORD + WID_SMEM_BK + WID_DCHE_CL);
            if(!llrdy) begin
              ///one cycle can only check one valid address in vector
              llrdy = 1;
              foreach(llCk[i]) begin
                if(llCk[i].adr == tag) begin
                  found = 1;
                  llid = i;
                  lladr = tag;
                end
              end
              if(!found && ise.op == op_ll) begin
                ///no entry found, if its a ll, allocate one
                llNext++;
                if(llNext >= NUM_LLCK) llNext = 0;
                llid = llNext;
                found = 1;
                llCk[llid].adr = tag;
                llCk[llid].en = '{default : 0};
              end
            end
            else
              found = lladr == tag;
            
            if(found) begin /// && !llAdrCk[cl][bk] && 
              if(ise.op == op_ll) begin
                llCk[llid].en[cl][bk] = 1;
                llCk[llid].tid[cl][bk] = ise.tid;
              end
              else begin
                ///store conditional
                failed = !(llCk[llid].en[cl][bk] && llCk[llid].tid[cl][bk] == ise.tid);
                llCk[llid].en[cl][bk] = 0;  ///so following sc failed
              end
            end
            
            if(failed && ise.op == op_sc) begin
              oc = 0;
              ex = 0;
            end
          end
          
          ///filling ck
          if(oc) begin
            ck[slot].sMemAdr[bk] = adr;
            ck[slot].sMemGrp[bk] = grp;
          end
          
          ocWEn = (ise.op inside {st_ops}) && oc;
          case(ise.op)
          op_sw   : 
            for(int j = 0; j < WORD_BYTES; j++) begin
              ck[slot].stData[bk].b[j] = st.b[j];
              ck[slot].sMemWEn[bk][j] = ocWEn;
              ck[slot].exEn[bk][j] = ex;
            end
          op_sh   :
            for(int j = 0; j < HALF_BYTES; j++) begin
              uchar adr2 = os & `GML(WID_HALF);
              ck[slot].stData[bk].b[adr2 + j] = st.b[adr2 + j];
              ck[slot].sMemWEn[bk][adr2 + j] = ocWEn;
              ck[slot].exEn[bk][adr2 + j] = ex;
            end
          op_sb   :
          begin
            ck[slot].stData[bk].b[os] = st.b[os];
            ck[slot].sMemWEn[bk][os] = ocWEn;
            ck[slot].exEn[bk][os] = ex;
          end
          endcase
        end
        
        ///        spi[STAGE_RRF_SXG0] = new();
        ck[cyc].xhg[sp] = padr & `GML(WID_DCHE_CL + WID_SMEM_BK + WID_WORD);
        ck[cyc].exp[sp] = exp;
        ck[cyc].oc[sp] = oc;
        ck[cyc].ex[sp] = ex;
        ck[cyc].re[sp] = spu.emsk[sp] && !oc && !ex;
        ck[cyc].slot[sp] = slot;
        selValidReq |= oc || ex;
        exReq[STAGE_RRF_DEM] |= ex;
        ovm_report_info("sel_dse", $psprintf("sp %0d, grp %0d, adr %0d, bk %0d, ex %0d, oc %0d, exp %0d %s, slot %0d, re %0d, xhg %0d", 
                        sp, grp, adr, bk, ex, oc, exp, selCause.name, slot, ck[slot].re[sp], ck[slot].xhg[sp]), OVM_HIGH);  
      end
        
      expCause[STAGE_RRF_DEM] = selCause;
      endian[STAGE_RRF_DEM] = selEndian;
      
      selExpReq |= selExp;
      
      ///if not lock 2 cache line, next cyc can try new cache aso
      if(!selLock2CL)
        selCacheRdy = 0;
      
      ///start resolve exp
      if(ise.vecMode == ise.subVec) begin
        bit res = selExpReq;
        if(ise.nonBlock)
          res &= !selValidReq;
          
        for(int i = 0; i <= ise.subVec; i++)
          expReq[STAGE_RRF_DEM + i] = res;
        sendExp = res;            
      
        selExpReq = 0;
        selValidReq = 0;
      end
      else
        expReq[STAGE_RRF_DEM] = 1;
      
      ///ext access allocate tr
      if(exReq[STAGE_RRF_DEM]) begin
        if(vn.eif[STAGE_RRF_DEM] == null) vn.eif[STAGE_RRF_DEM] = tr_dse2eif::type_id::create("toSPU", this);
        vn.eif[STAGE_RRF_DEM].op = ise.op;
        vn.eif[STAGE_RRF_DEM].req = 1;
        vn.eif[STAGE_RRF_DEM].exAdr = selExAdr;
        vn.eif[STAGE_RRF_DEM].endian = selEndian;
        vn.eif[STAGE_RRF_DEM].cacheFill = selWriteAlloc || !selNoCache;
        vn.eif[STAGE_RRF_DEM].sgl = !ise.vec || ise.vecMode == 0;
        vn.eif[STAGE_RRF_DEM].cyc = ise.subVec & `GML(WID_DCHE_CL);
        vn.eif[STAGE_RRF_DEM].last = last;
        vn.eif[STAGE_RRF_DEM].coherency = selCoherency;
        vn.eif[STAGE_RRF_DEM].uncachable = selNoCache;
        vn.eif[STAGE_RRF_DEM].priv = ise.priv;
        vn.eif[STAGE_RRF_DEM].state = cache[selCacheIdx][selCacheAso].state[selCacheGrp];
      end
      
      ///rfm & spu allocate tr
      if(vn.rfm[STAGE_RRF_DEM] == null) vn.rfm[STAGE_RRF_DEM] = tr_dse2rfm::type_id::create("toRFM", this);
      if(vn.spu[STAGE_RRF_DEM] == null) vn.spu[STAGE_RRF_DEM] = tr_dse2spu::type_id::create("toSPU", this);
      vn.rfm[STAGE_RRF_DEM].vrfWr = ise.vec && ise.wr;
      vn.rfm[STAGE_RRF_DEM].srfWr = !ise.vec && ise.wr;
      vn.rfm[STAGE_RRF_DEM].wrGrp = ise.wrGrp;
      vn.rfm[STAGE_RRF_DEM].wrAdr = ise.wrAdr;
      vn.rfm[STAGE_RRF_DEM].wrBk = ise.wrBk;
      vn.rfm[STAGE_RRF_DEM].uaWrEn = ise.uaWrEn;
      vn.rfm[STAGE_RRF_DEM].uaWrBk = ise.uaWrBk;
      vn.rfm[STAGE_RRF_DEM].uaWrAdr = ise.uaWrAdr;
      vn.rfm[STAGE_RRF_DEM].uaWrGrp = ise.uaWrGrp;
      vn.rfm[STAGE_RRF_DEM].subVec = ise.subVec;
      vn.rfm[STAGE_RRF_DEM].uaRes = rfm.base; ///ise.ua == ua_post ?? todo
      vn.spu[STAGE_RRF_DEM].tid = ise.tid;
      vn.spu[STAGE_RRF_DEM].wrEn = ise.ua != ua_no;
      if(ise.op == op_fmrf && eif != null && spu != null)
        foreach(eif.byteEn[i])
          if(eif.byteEn[i][0])
            vn.spu[STAGE_RRF_DEM].pres[i] = 1;
          else
            vn.spu[STAGE_RRF_DEM].pres[i] = spu.emsk[i];
                              
      ///finish one half wrap or whole request
      if(last) begin
        ///cache state (silent) transitions
        if(selLock2CL) begin
          case(cache[selCacheIdx][selCacheAso].state[selCacheGrp])
          cs_exclusive: cache[selCacheIdx][selCacheAso].state[selCacheGrp] = cs_modified;
          cs_owned,
          cs_shared:    cache[selCacheIdx][selCacheAso].state[selCacheGrp] = cs_inv;
          ///this case happens only when tlb cc bit changed
          cs_dirty:     cache[selCacheIdx][selCacheAso].state[selCacheGrp] = cs_modified;
          endcase
        end
        ovm_report_info("sel_dse_last", $psprintf("cyc %0d, Lock2CL %0d, selCacheIdx %0d, selCacheAso %0d, selCacheGrp %0d, expReq %0d", 
                        cyc, selLock2CL, selCacheIdx, selCacheAso, selCacheGrp, expReq[STAGE_RRF_DEM]), OVM_HIGH);  
        for(int i = 0; i <= cyc; i++)
          smi[STAGE_RRF_DEM + i] = ck[LAT_XCHG - 1 - i];
        selExp = 0;
        selExRdy = 0;
        selNoCache = 0;
        selLock2CL = 0;
        selNeedLock2CL = 0;
        selEndian = 0;
        selWriteAlloc = 0;
        selCoherency = 0;
        selCacheRdy = 0;
        foreach(ck[i])
          ck[i] = new();
      end
    end
    
    ///**sel stage, vxchg request
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]
        && v.fmISE[STAGE_RRF_SEL].op inside {op_pera, op_perb, op_shf4a, op_shf4b}) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      bit last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      uchar cyc = ise.subVec & `GML(WID_DCHE_CL);
      
      foreach(rfm.base[sp]) begin
        uchar xhg, slot;
        if(ise.op inside {op_shf4a, op_shf4b}) begin
          xhg = rfm.base[0] >> (2 * (sp >> 2)) & `GML(2);
          xhg += sp & `GMH(2);
          slot = cyc;
        end
        else begin
          xhg = rfm.base[sp];
          xhg = xhg & `GML(WID_SMEM_BK + WID_CYC);
          slot = xhg >> WID_SMEM_BK & `GML(WID_CYC);
          xhg = xhg & `GML(WID_SMEM_BK);
        end

        smi[STAGE_RRF_SXG0].slot[sp] = slot;
        smi[STAGE_RRF_SXG0].xhg[sp] = xhg << WID_WORD;
        ck[cyc].sMemAdr[sp] = xhg;
        ck[cyc].sMemGrp[sp] = slot;
        
        smi[STAGE_RRF_SXG0] = new();                  
        for(int i = 0; i < LAT_XCHG; i++) begin
          if(cyc == ck[i].sMemGrp[sp]) begin
            ck[i].stData[sp] = rfm.st[ck[i].sMemAdr[sp]];
            ck[i].sMemOpy[sp] = spu.emsk[ck[i].sMemAdr[sp]];
///            smi[STAGE_RRF_SXG0][sp].oc = spu.emsk[ck[slot][sp].sMemAdr];
          end
        end
                
        smi[STAGE_RRF_SXG0].exp[sp] = 0;
        smi[STAGE_RRF_SXG0].ex[sp] = 0;
        smi[STAGE_RRF_SXG0].re[sp] = 0;
        smi[STAGE_RRF_SXG0].oc[sp] = 1;
      end
          
      ck[cyc].sMemWEn = '{default : 0};
      ck[cyc].exEn = '{default : 0};
        
      ///finish one half wrap or whole request
      if(last) begin
        for(int i = 0; i <= cyc; i++)
          smi[STAGE_RRF_DEM + i] = ck[LAT_XCHG - 1 - i];
        foreach(ck[i])
          ck[i] = new();
      end
    end
    
    ///**sel stage, eif request
    if(v.fmEIF[STAGE_RRF_SEL] != null) begin
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_SEL];
      exadr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
              smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_GRP - srCacheGrp) * SGRP_SIZE,
              smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;
      bit smWEn = eif.wr, allocFail = 0, first;
      smStart >>= WID_WORD + WID_SMEM_BK;
      smEnd >>= WID_WORD + WID_SMEM_BK;
      smEnd2 >>= WID_WORD + WID_SMEM_BK;
      first = (eif.cyc & `GML(WID_DCHE_CL)) == 0;
      
      if(eif.alloc || eif.loadRsp || eif.wr || eif.rd) begin
        exadr_t exAdr = eif.exAdr;
        uint adr, tagLo, tagHi, idx, tag;
        bit hit = 0, hihit = 0, ehit = 0, fhit = 0, updateLo = 0, updateHi = 0, flush = 0;
        uchar aso = 0, grp = 0, hiAso = 0, eAso = 0, fAso = 0, fGrp = 0, cyc = eif.cyc, cl, flushGrp;
        cache_state_t flushCacheState;
        endian[STAGE_RRF_DEM] = eif.endian;
        if(eif.loadRsp) begin
          for(int sp = 0; sp < NUM_SP; sp++) begin
            smi[STAGE_RRF_SXG0] = new();
            smi[STAGE_RRF_SXG0].xhg[sp] = ldQue[eif.id].xhg[eif.cyc][sp];
            smi[STAGE_RRF_SXG0].exp[sp] = 0;
            smi[STAGE_RRF_SXG0].oc[sp] = ldQue[eif.id].wrEn[eif.cyc][sp];
            smi[STAGE_RRF_SXG0].ex[sp] = 0;
            smi[STAGE_RRF_SXG0].re[sp] = 0;
            smi[STAGE_RRF_SXG0].slot[sp] = ldQue[eif.id].xhg[eif.cyc][sp] >> (WID_WORD + WID_SMEM_BK);
          end
        end

        ///check cache
        adr = exAdr & `GML(WID_SMEM_ADR);
        adr = adr & `GMH(1) + eif.cyc;
        grp = (exAdr >> WID_SMEM_ADR) & `GML(WID_SMEM_GRP);
        cl = exAdr & `GML(WID_DCHE_CL);
        tag = exAdr >> WID_DCHE_CL;
        tagLo = exAdr >> (WID_DCHE_CL + WID_DCHE_IDX);
        tagHi = tagLo >> WID_DCHE_STAG;
        idx = adr >> WID_DCHE_CL;
        tagLo = tagLo & `GML(WID_DCHE_STAG);

        if(exAdr < smStart && exAdr >= smEnd && srCacheGrp > 0) begin
          ///in ext rang, check cache
          for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++) begin
            bit empty = 1;
            uchar wb = 0;
            for(int loTagIdx = (NUM_SMEM_GRP - srCacheGrp); loTagIdx < NUM_SMEM_GRP; loTagIdx++) begin
              empty = empty && cache[idx][hiTagIdx].state[loTagIdx] == cs_inv;
              if(need_writeback(cache[idx][hiTagIdx].state[loTagIdx])) begin
                wb++;
                if(!fhit)
                  fGrp = loTagIdx;
              end
              if(cache[idx][hiTagIdx].state[loTagIdx] != cs_inv && cache[idx][hiTagIdx].tagHi == tagHi && 
                 cache[idx][hiTagIdx].tagLo[loTagIdx] == tagLo) begin
                hit = 1;
                aso = hiTagIdx;
                grp = loTagIdx;
              end
              if(cache[idx][hiTagIdx].tagHi == tagHi) begin
                hiAso = hiTagIdx;
                hihit = 1;
              end
            end
            if(empty) begin
              eAso = hiTagIdx;
              ehit = 1;
            end
            else
              hihit = 0;  ///a full hihit aso is not needed
            if(wb < 2) begin
              fAso = hiTagIdx;
              fhit = 1;
            end
          end
          
          if(hit) begin
            ///already have, ignore
          end
          else if(hihit || ehit) begin
            bit found = 0;
            updateLo = 1;
            grp = cache[idx][hiAso].lo;
            aso = hiAso;
            for(int i = 0; i < NUM_SMEM_GRP; i++) begin
              ///find a inv first
              if(grp >= (NUM_SMEM_GRP - srCacheGrp) && cache[idx][aso].state[grp] == cs_inv) begin
                found = 1;
                break;
              end
              else begin
                grp++;
                grp = grp & `GML(WID_SMEM_GRP);
              end
            end
            
            if(!found) begin
              ///find a grp need no flush
              for(int i = 0; i < NUM_SMEM_GRP; i++) begin
                if(grp >= (NUM_SMEM_GRP - srCacheGrp) && !need_writeback(cache[idx][aso].state[grp])) begin
                  found = 1;
                  break;
                end
                else begin
                  grp++;
                  grp = grp & `GML(WID_SMEM_GRP);
                end
              end
            end

            if(!found && ehit) begin
              ///use a empty aso instead
              aso = eAso;
              for(grp = 0; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP; grp++);
            end
          end
          else begin
            ///aso full or no hihit
            uchar highest = 0;
            for(aso = 0; aso < NUM_DCHE_ASO; aso++) begin
              if(cache[idx][aso].hi > highest)
                highest = cache[idx][aso].hi;
            end
            
            if(fhit) begin
              ///this aso need flush only one cl
              aso = fAso;
              updateLo = 1;
              updateHi = 1;
              ///find a grp need no flush, can't write the flushing grp
              for(grp = 0; grp >= (NUM_SMEM_GRP - srCacheGrp) && grp < NUM_SMEM_GRP; grp++)
                if(!need_writeback(cache[idx][aso].state[grp]))
                  break;
              flush = need_writeback(cache[idx][aso].state[fGrp]);
              flushGrp = fGrp;
              flushCacheState = cache[idx][aso].state[flushGrp];
            end
            else begin
              ///now alloc fail, we can't flush more than one cl
              allocFail = 1;
              flush = 1;
              flushGrp = cache[idx][hiAso].lo;
              grp = cache[idx][hiAso].lo;
              flushCacheState = cache[idx][aso].state[flushGrp];
              smWEn = 0;
            end
          end
          adr = (adr & `GML(WID_SMEM_ADR - WID_DCHE_ASO)) | (aso << (WID_SMEM_ADR - WID_DCHE_ASO));
        end
        else if(exAdr >= smEnd2)
          smWEn = 0;
        
        cacheFlush[STAGE_RRF_DEM] = flush && eif.alloc && !allocFail;
                        
        ///check llCk
        if(eif.wr) begin
          foreach(llCk[i]) begin
            if(llCk[i].adr == tag) begin
              foreach(eif.byteEn[bk]) begin
                if(|eif.byteEn[bk])
                  llCk[i].en[cl][bk] = 0;
              end
              break;
            end
          end
        end
        
        if(eif.last) begin
          if(eif.alloc) begin
            if(updateLo) begin
              cache[idx][aso].lo = grp;
              cache[idx][aso].tagLo[grp] = tagLo;
            end
            if(updateHi) begin
              ///bias towards the flushed aso
              for(int i = 0; i < NUM_DCHE_ASO; i++) begin
                if(i == aso) continue;
                cache[idx][aso].hi++;
              end
              ///the aso is reloaded, clear old states
              for(int i = 0; i < NUM_SMEM_GRP; i++) begin
                if(i == grp) continue;
                cache[idx][aso].state[i] = cs_inv;
              end
              cache[idx][aso].tagHi = tagHi;
            end
            if(cacheFlush[STAGE_RRF_DEM]) begin
              case(cache[idx][aso].state[flushGrp])
              cs_owned,
              cs_modified:  cache[idx][aso].state[flushGrp] = cs_shared;
              cs_dirty:     cache[idx][aso].state[flushGrp] = cs_exclusive;
              endcase
            end
            cache[idx][aso].state[grp] = eif.state;
          end
          
          for(int i = 0; i < LAT_XCHG; i++)
            smi[STAGE_RRF_SXG0 + i] = ck[LAT_XCHG - 1 - i];///??smi[STAGE_RRF_SXG0 - i] = ck[i];
          selExp = 0;
          selExRdy = 0;
          foreach(ck[i])
            ck[i] = new();
        end
        
        ///fill ck & cache
        for(int bk = 0; bk < NUM_SP; bk++) begin
          ck[cyc].sMemAdr[bk] = adr;
          ck[cyc].sMemGrp[bk] = grp;
          ck[cyc].sMemWEn[bk] = '{default : smWEn};
///          ck[cyc][bk].ocEn = 1;
        end
        
        if(vn.rfm[STAGE_RRF_SXG0] == null) vn.rfm[STAGE_RRF_SXG0] = tr_dse2rfm::type_id::create("toRFM", this);
        
        vn.rfm[STAGE_RRF_SXG0].wrGrp = ldQue[eif.id].wrGrp;
        vn.rfm[STAGE_RRF_SXG0].wrAdr = ldQue[eif.id].wrAdr;
        vn.rfm[STAGE_RRF_SXG0].wrBk = ldQue[eif.id].wrBk;
        vn.rfm[STAGE_RRF_SXG0].uaWrBk = 0;
        vn.rfm[STAGE_RRF_SXG0].uaWrAdr = 0;
        vn.rfm[STAGE_RRF_SXG0].uaWrGrp = 0;
        vn.rfm[STAGE_RRF_SXG0].subVec = ldQue[eif.id].subVec;
        
        if(cacheFlush[STAGE_RRF_DEM] || allocFail) begin
          if(vn.eif[STAGE_RRF_SXG0] == null) vn.eif[STAGE_RRF_SXG0] = tr_dse2eif::type_id::create("toRFM", this);
          vn.eif[STAGE_RRF_SXG0].op = ldQue[eif.id].op;
          vn.eif[STAGE_RRF_SXG0].req = 1;
          vn.eif[STAGE_RRF_SXG0].cacheFlush = cacheFlush[STAGE_RRF_DEM];
          vn.eif[STAGE_RRF_SXG0].state = flushCacheState;
          vn.eif[STAGE_RRF_SXG0].cyc = eif.cyc;
          vn.eif[STAGE_RRF_DEM].last = eif.last;
          vn.eif[STAGE_RRF_DEM].uncachable = 0;
          vn.eif[STAGE_RRF_DEM].allocFail = allocFail;
          vn.eif[STAGE_RRF_SXG0].exAdr = cache[idx][aso].tagHi << (WID_DCHE_STAG + WID_DCHE_IDX + WID_DCHE_CL);
          vn.eif[STAGE_RRF_SXG0].exAdr += cache[idx][aso].tagLo[flushGrp] << (WID_DCHE_IDX + WID_DCHE_CL);
          vn.eif[STAGE_RRF_SXG0].exAdr += idx << WID_DCHE_CL;
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
    
    ///allocate que, current only dse req need allocate que
    if(v.fmISE[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL].op inside {ld_ops, st_ops}) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      bit last = ((ise.subVec + 1) & `GML(WID_DCHE_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;

      if(exReq[STAGE_RRF_DEM] && !cancel[ise.tid][STAGE_RRF_DEM]) begin
        uchar queId = 0;
        bit found = 0, noVecSt = 0, noSglSt = 0, noLd = 0;
        if(v.fmEIF[STAGE_RRF_AG] != null) begin
          noVecSt = v.fmEIF[STAGE_RRF_AG].noVecSt;
          noSglSt = v.fmEIF[STAGE_RRF_AG].noSglSt;
          noLd = v.fmEIF[STAGE_RRF_AG].noLd;
        end
        
        if(ise.op inside {ld_ops}) begin
          if(!selQueRdy && ise.subVec == 0) begin
            foreach(ldQue[i])
              if(!ldQue[i].en) begin
                ldQue[i].en = 1;
                queId = i;
                found = 1;
                selQueRdy = 1;
                exQueId[STAGE_RRF_DEM] = i;
                exQueAlloc[STAGE_RRF_DEM] = 1;
                break;
              end
              if(!found)
                ovm_report_info("dc", "ld queue overrun!", OVM_HIGH);
          end
          else begin
            queId = exQueId[STAGE_RRF_DEM];
            found = 1;
          end
          if(!found)
            dcReRun[ise.subVec] = 1;
          else if(noLd && ise.subVec == 0)
            dcReRun = '1;
        end
        else if(ise.op inside {st_ops}) begin
          if(!selQueRdy) begin
            foreach(stQue[i])
              if(!stQue[i].en) begin
                stQue[i].en = 1;
                queId = i;
                found = 1;
                selQueRdy = 1;
                exQueId[STAGE_RRF_DEM] = i;
                exQueAlloc[STAGE_RRF_DEM] = 1;
                break;
              end
            if(!found)
              ovm_report_info("dc", "ld queue overrun!", OVM_HIGH);
          end
          else begin
            queId = exQueId[STAGE_RRF_DEM];       
            found = 1;
          end
          if(!found)
            dcReRun[ise.subVec] = 1;
          else if(noVecSt && ise.vec && ise.vecMode != 0 && ise.subVec == 0)
            dcReRun = '1;
          else if(noSglSt && (ise.vecMode == 0 || !ise.vec) && ise.subVec == 0)
            dcReRun = '1;
        end
        
        if(last)
          selQueRdy = 0;
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
        rfm.base[0] = rfm.base[0] + rfm.os;
        vadr = rfm.base[0];
      end
      else if(ise.op inside {ld_ops, st_ops, op_tmrf}) begin
        foreach(spu.emsk[i]) begin
          uchar vecId = NUM_SP * ise.subVec + i;
          if(spu.emsk[i]) begin
            rfm.base[i] = rfm.base[i] + rfm.os;
            if(ise.at == at_burst || ise.op == op_tmrf) begin
              if(ise.op inside {op_lw, op_ll, op_sw, op_sc})
                rfm.base[i] += vecId << 2;
              else if(ise.op inside {op_lh, op_lhu, op_sh})
                rfm.base[i] += vecId << 1;
              else
                rfm.base[i] += vecId;
            end
            ovm_report_info("ag", $psprintf("vec %0d adr: 0x%0h", vecId, rfm.base[i]), OVM_HIGH);
            if(rfm.base[i] >= VADR_MAPPED && rfm.base[i] < VADR_NMAPNC) begin
             found = 1;
             vadr = rfm.base[i];
            end
          end
        end
      end
      
      if(found && ise.en && (!tlbRdy || (tlbReqVAdr[STAGE_RRF_TAG] != (vadr >> VADR_START)))) begin
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
    vn.fmISE[0] = req;
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
    foreach(ck[i])
      ck[i] = new();
      
    if(smFilePath != "")
      $readmemh(smFilePath, sharedMem);
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
