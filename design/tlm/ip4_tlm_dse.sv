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
  tr_spu2dse fmSPU[STAGE_RRF_SEL:STAGE_RRF_AG];
  tr_rfm2dse fmRFM[STAGE_RRF_SEL:STAGE_RRF_AG];
  tr_spa2dse fmSPA;
  tr_tlb2dse fmTLB;
  tr_eif2dse fmEIF[STAGE_RRF_DC:STAGE_RRF_AG];    /// external interfaces
  
///  tr_dse2ise ise;
///  tr_dse2spu spu[STAGE_RRF_SR0:STAGE_RRF_EXS2];
  tr_dse2rfm rfm[STAGE_RRF_VWBP:STAGE_RRF_LXG0];
///  tr_dse2spa spa;
///  tr_dse2tlb tlb;
  tr_dse2eif eif[STAGE_RRF_DPRW:STAGE_RRF_LXG0];
  tr_dse2spu spu[STAGE_RRF_DPRW:STAGE_RRF_LXG0];
  
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_sarray_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE)  
///     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
///     `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
///     `ovm_field_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
///     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_sarray_object(eif, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_dse_vars

typedef struct{
  uchar xhg[LAT_XCHG][NUM_SP];
  bit wrEn[LAT_XCHG][NUM_SP], en;
  uchar wrGrp, wrAdr, wrBk, tid, subVec;
  opcode_e op;
  padr_t padr;
}ldQue_t;

typedef struct{
  uchar tid;
  padr_t padr;
  bit en;
}stQue_t;

typedef struct{
  uint tagHi, tagLo[NUM_SMEM_GRP];
  bit tagV[NUM_SMEM_GRP], dirty[NUM_SMEM_GRP];
  uchar lo;
}cache_t;

class sm_t;
///sel stage data struct
  bit sMemOpy,   ///occupy for onchip shared mem
      xhgOpy,    ///occupy for exchange network
      exEn[WORD_BYTES],  ///ext enabled
      ocEn;  ///onchip enabled
  uint sMemAdr, sMemGrp;  ///on chip adr grp
  uchar sMemOs;
  wordu stData;
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
  local wordu sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SMEM_BK];
  local cache_t cache[NUM_DCHE_TAG][NUM_DCHE_ASO];
  local uchar cacheSelHi[NUM_DCHE_TAG];
  local bit cacheFlush[STAGE_RRF_LXG:STAGE_RRF_DEM];
  
  local word tlbReqVAdr[STAGE_RRF_SEL:STAGE_RRF_TAG];
  local bit tlbRdy;
    
  local bit selExRdy, selExp, selExpReq, selValidReq;
  local padr_t selExAdr;
  local cause_dse_t selCause;

  local uchar srCacheGrp;
  local uint srMapBase;
  local bit cacheGrpEn[NUM_SMEM_GRP];
  local bit[STAGE_RRF_SWBP:0] cancel[NUM_THREAD];
  
  local sm_t ck[LAT_XCHG][NUM_SMEM_BK],
             smi[STAGE_RRF_LXG:STAGE_RRF_SEL][NUM_SMEM_BK];
  local sp_t spi[STAGE_RRF_LXG:STAGE_RRF_SXG0][NUM_SP];
  local padr_t exPAdr[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local bit expReq[STAGE_RRF_LXG:STAGE_RRF_DEM];
  local cause_dse_t expCause[STAGE_RRF_LXG:STAGE_RRF_DEM];
  
  local bit dcExRdy, dcExp, dcValid;
  local uchar dcQueId;
  
  local ldQue_t ldQue[NUM_LDQUE];
  local stQue_t stQue[NUM_STQUE];
  
  `ovm_component_utils_begin(ip4_tlm_dse)
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
        cancel[v.fmSPU[0].tid] |= `GML(STAGE_RRF_DC);
      if(v.fmSPU[0].expFu)
        cancel[v.fmSPU[0].tid] |= `GML(STAGE_RRF_EPS - v.fmSPU[0].vecMode);
    end
        
    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_TAG; i--) 
      tlbReqVAdr[i] = tlbReqVAdr[i - 1];
      
    for (int i = STAGE_RRF_VWB; i > 0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[0] = null;

    for (int i = STAGE_RRF_DC; i > STAGE_RRF_AG; i--) 
      vn.fmEIF[i] = v.fmEIF[i - 1];
    vn.fmEIF[STAGE_RRF_AG] = null;
    
    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_AG; i--) begin
      vn.fmSPU[i] = v.fmSPU[i - 1];
      vn.fmRFM[i] = v.fmRFM[i - 1];
    end
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    
    for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_DEM; i--) 
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_DEM] = null;

    for(int i = STAGE_RRF_DPRW; i > STAGE_RRF_LXG0; i--) begin
      vn.eif[i] = v.eif[i - 1];
      vn.spu[i] = v.spu[i - 1];
    end
    vn.eif[STAGE_RRF_LXG0] = null;
    vn.spu[STAGE_RRF_LXG0] = null;
        
    ///**sel stage, ise request
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      uchar pbId = ise.pbId;
      padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
             smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_BK - srCacheGrp) * SGRP_SIZE,
             smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
      uint vAdrHi;
      uint dcIdx, dcRdy = 0;
      bit ed = 0;
      bit last = ((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      
      if(tlb != null) begin
        vAdrHi = tlbReqVAdr[STAGE_RRF_SEL] >> tlb.eobit;
        ed = tlb.e;
      end
      
      foreach(rfm.base[sp]) begin
        padr_t padr;
        bit nc, exp;
        bit oc = spu.emsk[sp] && ise.en,
            ex = spu.emsk[sp] && ise.en;
        uchar grp, adr, bk, os, slot;
        wordu res;
                
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
          nc = tlb.c == 0;
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
          if(!nc && cacheGrpEn[grp]) begin
            uint tagLo = padr >> (WID_WORD + WID_SMEM_BK + WID_DCH_CL + WID_DCH_IDX),
                 tagHi = tagLo >> WID_DCH_STAG;
            tagLo = tagLo & `GML(WID_DCH_STAG);
            idx = adr >> WID_DCH_IDX;
            
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

          ///cache hit
          if(hit) begin
            for(int s = 0; s < LAT_XCHG; s++)
              if(!ck[s][bk].xhgOpy) begin
                ck[s][bk].xhgOpy = 1;
                slot = s;
                oc = 1;
                break;
              end
          end
          ///external access
          else begin
            ///a external store still need xchg network            
            if(ise.op inside {st_ops}) begin
              ex = 0;
              for(int s = 0; s < LAT_XCHG; s++)
                if(!ck[s][bk].xhgOpy) begin
                  ck[s][bk].xhgOpy = 1;
                  slot = s;
                  ex = 1;
                  break;
                end
            end
            
            if(!selExRdy) begin
              selExRdy = ex;
              selExAdr = padr >> (WID_DCH_CL + WID_SMEM_BK + WID_WORD);
            end
            else
              ex = selExAdr == (padr >> (WID_DCH_CL + WID_SMEM_BK + WID_WORD));
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
          
          for(int s = 0; s < LAT_XCHG; s++)
            if((ck[s][bk].sMemAdr == adr && ck[s][bk].sMemGrp == grp 
                && ck[s][bk].sMemOpy) || !ck[s][bk].xhgOpy) begin
              ck[s][bk].sMemOpy = 1;
              ck[s][bk].xhgOpy = 1;
              slot = s;
              oc = 1;
              break;
            end
        end
        
        if(oc) begin
          ck[slot][bk].sMemAdr = adr;
          ck[slot][bk].sMemGrp = grp;
          ck[slot][bk].sMemOs = os;
          ck[slot][bk].ocEn = oc;
        end
        
        if(ex) begin
          wordu st = rfm.st[sp];
          if(ise.op inside {op_lbu, op_lb, op_sb})
            ck[slot][bk].exEn[ed ? adr : (WORD_BYTES - 1 - adr)] = 1;
          else if(ise.op inside {op_lhu, op_lh, op_sh}) begin
            for(int j = 0; j < HALF_BYTES; j++)
              ck[slot][bk].exEn[j] = 1;
          end
          else if(ise.op inside {op_lw, op_sw})
            for(int j = 0; j < WORD_BYTES; j++)
              ck[slot][bk].exEn[j] = 1;
              
          case(ise.op)
          op_sw   : 
            for(int j = 0; j < WORD_BYTES; j++)
              ck[grp][bk].stData.b[j] = st.b[ed ? j : (WORD_BYTES - 1 - j)];
          op_sh   : begin
            int adr2 = adr & `GMH(WID_HALF - 1);
            adr2 = ed ? adr2 : (HALF_BYTES - adr2);
            for(int j = 0; j < HALF_BYTES; j++)
              ck[grp][bk].stData.b[adr2 + j] = st.b[ed ? (adr2 + j) : (adr2 + HALF_BYTES - 1 - j)];
          end
          op_sb   :
            ck[grp][bk].stData.b[ed ? adr : (WORD_BYTES - 1 - adr)] = st.b[adr];
          endcase
        end
        spi[STAGE_RRF_SXG0][sp] = new();
        spi[STAGE_RRF_SXG0][sp].xhg = padr[sp] & `GML(WID_DCH_CL + WID_SMEM_BK + WID_WORD);
        spi[STAGE_RRF_SXG0][sp].exp = exp;
        spi[STAGE_RRF_SXG0][sp].oc = oc;
        spi[STAGE_RRF_SXG0][sp].ex = ex;
        spi[STAGE_RRF_SXG0][sp].re = spu.emsk[sp] && !oc && !ex;
        spi[STAGE_RRF_SXG0][sp].slot = slot;
        selValidReq |= oc || ex;
      end
      exPAdr[STAGE_RRF_DEM] = selExAdr;
      expCause[STAGE_RRF_DEM] = selCause;
      
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
        uchar lvl = ise.subVec & `GML(WID_DCH_CL);
        for(int i = 0; i <= lvl; i++)
          smi[STAGE_RRF_DEM + i] = ck[LAT_XCHG - 1 - i];
        selExp = 0;
        selExRdy = 0;
        foreach(ck[i, j])
          ck[i][j] = new();
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
        end
      end
      else if(eif.storeRsp)
        padr = stQue[eif.id].padr;
      else
        ovm_report_warning("sel", "eif tr has no rsp!");

      adr = padr & `GML(WID_SMEM_ADR);
      tagLo = padr >> (WID_DCH_IDX);
      tagHi = tagLo >> WID_DCH_STAG;
      idx = adr >> WID_DCH_IDX;
      tagLo = tagLo & `GML(WID_DCH_STAG);
      
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
        cacheFlush[STAGE_RRF_DEM] = 1;
      end
      adr = (adr & `GML(WID_SMEM_ADR - WID_DCHE_ASO)) | (hi << (WID_SMEM_ADR - WID_DCHE_ASO));
      cache[idx][hi].tagV[grp] = 1;
      cache[idx][hi].tagLo[grp] = tagLo;
      
      for(int bk = 0; bk < NUM_SMEM_BK; bk++) begin
        ck[cyc][bk].sMemAdr = adr;
        ck[cyc][bk].sMemGrp = grp;
        ck[cyc][bk].sMemOs = 0;
        ck[cyc][bk].ocEn = 1;
      end
      if(eif.last) begin
        for(int i = 0; i < LAT_XCHG; i++)
          smi[STAGE_RRF_DEM + i] = ck[LAT_XCHG - 1 - i];
        selExp = 0;
        selExRdy = 0;
        foreach(ck[i, j])
          ck[i][j] = new();
      end
    end
    
    ///do pip shift after sel stage
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_DEM; i--) begin
      spi[i] = spi[i - 1];
      exPAdr[i] = exPAdr[i - 1];
      expReq[i] = expReq[i - 1];
      expCause[i] = expCause[i- 1];
      cacheFlush[i] = cacheFlush[i - 1];
    end
    for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SEL; i--)
      smi[i] = smi[i - 1];
      
    spi[STAGE_RRF_SXG0] = '{default: null};
    smi[STAGE_RRF_SEL] = '{default: null};
        
    ///dc stage
///    if((v.fmISE[STAGE_RRF_DC] != null && v.fmISE[STAGE_RRF_DC].en && v.fmRFM[STAGE_RRF_DC] != null)
///        || (v.fmEIF[STAGE_RRF_DC] != null)) begin
    begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DC];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_DC];
      tr_eif2dse eif = v.fmEIF[STAGE_RRF_DC];
      wordu res[NUM_SMEM_BK];
      bit exp = 0, ex = 0, oc = 0, last = 0, exRsp = 0;
      if(ise != null && ise.en)
        last = ((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(eif != null && (eif.loadRsp || eif.storeRsp)) begin
        exRsp = 1;
        last = eif.last;
      end
        
      if(spi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else if(exRsp) begin
        exp = 0;
        ex = 0;
        oc = 0;
        dcExp = 0;
        dcValid = 0;
      end
      else begin
        for(int i = 0; i < NUM_SP; i++) begin
          exp |= spi[STAGE_RRF_DC][i].exp;
          ex |= spi[STAGE_RRF_DC][i].ex;
          oc |= spi[STAGE_RRF_DC][i].oc;
        end
        dcExp |= exp;
        dcValid |= oc || ex;
      end
      
      ///shared memory write
      if(smi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else begin
        for(int bk = 0; bk < NUM_SMEM_BK; bk++) begin
          uint adr = smi[STAGE_RRF_DC][bk].sMemAdr,
               os = smi[STAGE_RRF_DC][bk].sMemOs,
               grp = smi[STAGE_RRF_DC][bk].sMemGrp;
          if(smi[STAGE_RRF_DC][bk].ocEn) begin
            if(eif != null) begin
              res[bk] = eif.data[bk];
              sharedMem[grp][adr][bk] = eif.data[bk];
            end
            else if(ise != null && ise.op inside {ld_ops})
              res[bk] = sharedMem[grp][adr][bk];
            else if(ise != null && !cancel[ise.tid][STAGE_RRF_DC])
              case(ise.op)
              op_sb:    sharedMem[grp][adr][bk].b[os] = smi[STAGE_RRF_DC][bk].stData;
              op_sh:    sharedMem[grp][adr][bk].h[os >> (WID_HALF - 1)] = smi[STAGE_RRF_DC][bk].stData;
              op_sw:    sharedMem[grp][adr][bk] = smi[STAGE_RRF_DC][bk].stData;
              endcase
          end
        end
      end
      
      if(vn.rfm[STAGE_RRF_LXG0] == null) vn.rfm[STAGE_RRF_LXG0] = tr_dse2rfm::type_id::create("toRFM", this);
      if(vn.spu[STAGE_RRF_LXG0] == null) vn.spu[STAGE_RRF_LXG0] = tr_dse2spu::type_id::create("toSPU", this);
        
      if(spi[STAGE_RRF_DC][0] == null) begin
        ovm_report_warning("dc", "previous data missing");
      end
      else begin
        if(rfm != null) begin
          for(int sp = 0; sp < NUM_SP; sp++) begin
            vn.rfm[STAGE_RRF_LXG0].updateAdrRes[sp] = rfm.base[sp];
            if(spi[STAGE_RRF_DC][sp].exp)
              vn.spu[STAGE_RRF_LXG0].pres[sp] = 0;
            else
              vn.spu[STAGE_RRF_LXG0].pres[sp] = spi[STAGE_RRF_DC][sp].re;
          end
        end
        
        if(ise.op inside {ld_ops}) begin
          for(int sp = 0; sp < NUM_SP; sp++) begin
            for(int slot = 0; slot < LAT_XCHG; slot++) begin
              uchar st = STAGE_RRF_LXG0 + slot;
              uchar bk = spi[st][sp].xhg >> WID_WORD & `GML(WID_SMEM_BK),
                    os = spi[st][sp].xhg & `GML(WID_WORD);
              if(vn.rfm[st] == null) continue;
              if(spi[st][sp].slot != slot) continue;
              case(ise.op)
              op_lw   : vn.rfm[STAGE_RRF_LXG0].res[sp] = res[bk];
              op_lbu  : vn.rfm[STAGE_RRF_LXG0].res[sp] = {'0, res[bk].b[os]};
              op_lb   : vn.rfm[STAGE_RRF_LXG0].res[sp] = {{WORD_BITS{res[bk].b[os][7]}}, res[bk].b[os]};
              op_lhu  : vn.rfm[STAGE_RRF_LXG0].res[sp] = {'0, res[bk].h[os >> WID_HALF]};
              op_lh   : vn.rfm[STAGE_RRF_LXG0].res[sp] = {{WORD_BITS{res[bk].h[os >> WID_HALF].b[HALF_BYTES - 1][7]}}, res[bk].h[os >> WID_HALF]};
              endcase
            end
            vn.rfm[STAGE_RRF_LXG0].wrEn[sp] = spi[STAGE_RRF_DC][sp].oc;
          end
          vn.rfm[STAGE_RRF_LXG0].wrGrp = ise.wrGrp;
          vn.rfm[STAGE_RRF_LXG0].wrAdr = ise.wrAdr;
          vn.rfm[STAGE_RRF_LXG0].updateAdrWrBk = ise.updateAdrWrBk;
          vn.rfm[STAGE_RRF_LXG0].updateAdrWrAdr = ise.updateAdrWrAdr;
          vn.rfm[STAGE_RRF_LXG0].updateAdrWrGrp = ise.updateAdrWrGrp;
          vn.rfm[STAGE_RRF_LXG0].subVec = ise.subVec;
          vn.spu[STAGE_RRF_LXG0].tid = ise.tid;
          vn.spu[STAGE_RRF_LXG0].wrEn = ise.updatePr;
        end
        
        ///que eif fill
        if(ex) begin
          uchar cyc = ise.subVec & `GML(WID_DCH_CL),
                queId = 0;
          if(vn.eif[STAGE_RRF_LXG0] == null) vn.eif[STAGE_RRF_LXG0] = tr_dse2eif::type_id::create("toSPU", this);
          for(int bk = 0; bk < NUM_SMEM_BK; bk++)
            for(int os = 0; os < WORD_BYTES; os++)
              vn.eif[STAGE_RRF_LXG0].wrEn[os] = smi[STAGE_RRF_DC][bk].exEn[os];
            
          if(ise.op inside {ld_ops}) begin
            if(!dcExRdy) begin
              bit found = 0;
              foreach(ldQue[i])
                if(!ldQue[i].en) begin
                  ldQue[i].en = 1;
                  queId = i;
                  found = 1;
                  dcExRdy = 1;
                  dcQueId = i;
                  break;
                end
              if(!found)
                ovm_report_warning("dc", "ld queue overrun!");
            end
            else
              queId = dcQueId;
            vn.eif[STAGE_RRF_LXG0].op = ise.op;
            vn.eif[STAGE_RRF_LXG0].req = 1;
            vn.eif[STAGE_RRF_LXG0].pAdr = exPAdr[STAGE_RRF_DC];
            ldQue[queId].en = 1;
            ldQue[queId].wrGrp = ise.wrGrp;
            ldQue[queId].wrBk = ise.wrBk;
            ldQue[queId].wrAdr = ise.wrAdr;
            ldQue[queId].tid = ise.tid;
            ldQue[queId].op = ise.op;
            ldQue[queId].padr = exPAdr[STAGE_RRF_DC];
            
            for(int sp = 0; sp < NUM_SP; sp++) begin
              ldQue[queId].wrEn[cyc][sp] = spi[STAGE_RRF_DC][sp].oc;
              ldQue[queId].xhg[cyc][sp] = spi[STAGE_RRF_DC][sp].xhg;
            end
            if(last)
              ldQue[queId].subVec = ise.subVec;
          end
          else if(ise.op inside {st_ops}) begin
            if(!dcExRdy) begin
              bit found = 0;
              foreach(stQue[i])
                if(!stQue[i].en) begin
                  stQue[i].en = 1;
                  queId = i;
                  found = 1;
                  dcExRdy = 1;
                  dcQueId = i;
                  break;
                end
              if(!found)
                ovm_report_warning("dc", "ld queue overrun!");
            end
            else
              queId = dcQueId;       
            stQue[queId].en = 1;
            stQue[queId].tid = ise.tid;
            stQue[queId].padr = exPAdr[STAGE_RRF_DC];
            for(int bk = 0; bk < NUM_SMEM_BK; bk++)
              vn.eif[STAGE_RRF_LXG0].data[cyc][bk] = smi[STAGE_RRF_DC][bk].stData;
          end
          vn.eif[STAGE_RRF_LXG0].id = queId;
        end
          
        if(last) begin
          dcExp = 0;
          dcValid = 0;
          dcExRdy = 0;
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
    
    ///select vadr from ise req to tlb for translation
    if(v.fmISE[STAGE_RRF_AG] != null && v.fmRFM[STAGE_RRF_AG] != null
       && v.fmSPU[STAGE_RRF_AG] != null && v.fmISE[STAGE_RRF_AG].en) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_AG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_AG];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
      int vadr = 0;
      bit found = 0;
      bit last = ((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      
      foreach(spu.emsk[i])
        if(spu.emsk[i]) begin
          rfm.base[i] = rfm.base[i] + rfm.os[i];
          if(rfm.base[i] >= VADR_MAPPED && rfm.base[i] < VADR_NMAPNC) begin
           found = 1;
           vadr = rfm.base[i];
          end
        end
        
      if(found && ise.en && !tlbRdy) begin
        tlbRdy = 1;
        toTLB = tr_dse2tlb::type_id::create("toTLB", this);
        toTLB.vAdr = vadr >> VADR_START;
        toTLB.op = ise.op;
        toTLB.tid = ise.tid;
        toTLB.req = 1;
        toTLB.k = ise.priv;
        tlbReqVAdr[STAGE_RRF_TAG] = toTLB.vAdr;
      end
      
      if(last)
        tlbRdy = 0;
    end
    else
      tlbReqVAdr[STAGE_RRF_TAG] = tlbReqVAdr[STAGE_RRF_TAG + 1];
    
    toSPU = v.spu[STAGE_RRF_DPRW];
    toRFM = v.rfm[STAGE_RRF_VWBP];
    toEIF = v.eif[STAGE_RRF_DPRW];

    if(v.fmISE[STAGE_RRF_DPRW] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_DPRW];
      bit last = ((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0 || (ise.subVec == ise.vecMode) || !ise.vec;
      if(expReq[STAGE_RRF_DPRW]) begin
        toEIF = null;
        if(last && toEIF != null && !cancel[STAGE_RRF_DPRW]) begin
          if(toEIF.op inside {ld_ops})
            ldQue[toEIF.id].en = 0;
          else if(toEIF.op inside {st_ops})
            stQue[toEIF.id].en = 0;
        end
      end
      if(last && !cancel[STAGE_RRF_DPRW]) begin
        toISE = tr_dse2ise::type_id::create("toISE", this);
        toISE.rsp = 1;
        toISE.exp = expReq[STAGE_RRF_DPRW];
        toISE.scl = ise.vec == 0;
        toISE.tid = ise.tid;
        toISE.vecMode = ise.vecMode;
        toISE.pendExLoad = 0;
        toISE.pendExStore = 0;
        toISE.cause = expCause[STAGE_RRF_DPRW];
        foreach(ldQue[i])
          if(ldQue[i].en && ldQue[i].tid == ise.tid)
            toISE.pendExLoad++;
        foreach(stQue[i])
          if(stQue[i].en && stQue[i].tid == ise.tid)
            toISE.pendExStore++;
      end
    end
    
    ///spu ops
    if(v.fmSPU[STAGE_RRF_ASR] != null && v.fmSPU[STAGE_RRF_ASR].srReq) begin
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_ASR];
      case(spu.op)
      op_gp2s:
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
      op_s2gp:
      begin
        if(toSPU == null)
          toSPU = tr_dse2spu::type_id::create("toSPU", this);
        toSPU.rsp = 1;
        case(spu.srAdr)
        SR_MBASE: toSPU.srRes = srMapBase;
        SR_OCMC:  toSPU.srRes = srCacheGrp;
        endcase
      end
      endcase
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
  
