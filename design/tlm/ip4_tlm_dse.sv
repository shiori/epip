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
  tr_eif2dse fmEIF;    /// external interfaces
  
  tr_dse2ise ise;
///  tr_dse2spu spu[STAGE_RRF_SR0:STAGE_RRF_EXS2];
  tr_dse2rfm rfm[STAGE_RRF_VWBP:STAGE_RRF_DEM];
  tr_dse2spa spa;
  tr_dse2tlb tlb;
  tr_dse2eif eif;
  
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
///     `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(eif, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_dse_vars

typedef struct{
  uchar xhgCtl[LAT_XCHG][NUM_SMEM_BK];
  bit wrEn[LAT_XCHG][NUM_SMEM_BK], en;
  uchar grp, adr, bk, tid, subVec;
  opcode_e op;
}ldQue_t;

typedef struct{
  uchar tid;
  bit en;
}stQue_t;

typedef struct{
  uint tagHi, tagLo[NUM_SMEM_GRP];
  bit tagV[NUM_SMEM_GRP], dirty[NUM_SMEM_GRP];
}cache_t;

class sm_t;
///sel stage data struct
  bit sMemOpy,   ///occupy for onchip shared mem
      xhgOpy,    ///occupy for exchange network
      exEn[WORD_BYTES];  ///ext enabled
  uint sMemAdr, sMemGrp;  ///on chip adr grp
  uchar sMemBk, sMemOs;
  wordu stData;
endclass

class sp_t;
  bit exp, oc, ex;
  uchar slot;
endclass

///---------------------------------------main component----------------------------------------
class ip4_tlm_dse extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_dse_vars v, vn;  
  local wordu sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SMEM_BK];
  local word tlbReqVAdr[STAGE_RRF_SEL:STAGE_RRF_TAG];
  local cache_t cache[NUM_DCHE_TAG][NUM_DCHE_ASO];
  
  local bit selOcValid, selExValid, selHasEx, selExRdy;
  local padr_t selExAdr;
  local uchar srCacheGrp;
  local uint srMapBase;
  local bit cacheGrpEn[NUM_SMEM_GRP];
    
  local sm_t smSel[LAT_XCHG][NUM_SMEM_BK],
             smDc[LAT_XCHG][NUM_SMEM_BK];
  local sp_t spDc[LAT_XCHG][NUM_SP];
             
  local ldQue_t ldQue[NUM_LDQUE];
  local stQue_t stQue[NUM_STQUE];
  
  local bit selExp;
  local cause_dse_t expCause;

  local tr_dse2eif eifTr[LAT_XCHG];
  local tr_dse2rfm rfmTr[LAT_XCHG];
  local tr_dse2ise iseTr[LAT_XCHG];
  local tr_dse2spu spuTr[LAT_XCHG];
  
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
    if(v.fmEIF != null) end_tr(v.fmEIF);
    
    vn.fmISE[STAGE_RRF_RRC0] = null;
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    vn.fmSPA = null;
    vn.fmTLB = null;
    vn.fmEIF = null;
    vn.fmEIF = null;

///    for (int i = STAGE_RRF_SR0; i > STAGE_RRF_EXS2; i--)
///      vn.spu[i] = v.spu[i-1];
///    vn.spu[STAGE_RRF_EXS2] = null;
        
    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_TAG; i--) 
      tlbReqVAdr[i] = tlbReqVAdr[i - 1];
      
    for (int i = STAGE_RRF_VWB; i > 0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[0] = null;

    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_AG; i--) begin
      vn.fmSPU[i] = v.fmSPU[i - 1];
      vn.fmRFM[i] = v.fmRFM[i - 1];
    end
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    
    for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_DEM; i--) 
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_DEM] = null;
    
    for(int i = LAT_XCHG; i > 0; i--) begin
      eifTr[i] = eifTr[i - 1];
      rfmTr[i] = rfmTr[i - 1];
      iseTr[i] = iseTr[i - 1];
      spuTr[i] = spuTr[i - 1];
    end
    eifTr[0] = null;
    rfmTr[0] = null;
    iseTr[0] = null;
    spuTr[0] = null;
    
    for(int i = LAT_XCHG; i > 0; i--) begin
      spDc[i] = spDc[i - 1];
      smDc[i] = smDc[i - 1];
    end
    
    spDc[0] = '{default: null};
    smDc[0] = '{default: null};
    
    if(v.fmSPA != null && v.fmSPA.cancel)
      for(int i = 0; i < STAGE_RRF_DC; i++)
        if(v.fmISE[i] != null &&  v.fmISE[i].tid == v.fmSPA.tid)
          v.fmISE[i].en = 0;
                    
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      uchar pbId = ise.pbId, cyc = ise.subVec & `GML(WID_DCH_CL);
      padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
             smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_BK - srCacheGrp) * SGRP_SIZE,
             smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
      uint vAdrHi;
      uint dcIdx, dcRdy = 0;
      bit ed = 0;

      
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
            expCause = tlb.cause;
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
            expCause = EC_ADRALG;
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
        ///**cache address:   | taghi | taglo | idx | cl | bk | offset |
        if(ex) begin
          bit hit = 0;
          uint idx;
          
          ///chk cache for match
          if(!nc && cacheGrpEn[grp]) begin
            uint taglo = padr >> (WID_WORD + WID_SMEM_BK + WID_DCH_CL + WID_DCH_IDX),
                 taghi = taglo >> WID_DCHE_STAG;
            taglo = taglo & `GML(WID_DCHE_STAG);
            idx = adr >> WID_DCH_IDX;
            
            for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++)
              for(int loTagIdx = 0; loTagIdx < NUM_SMEM_GRP; loTagIdx++)
                if(!dcRdy || dcIdx == idx) begin
                  dcRdy = 1;
                  dcIdx = idx;
                  hit = cache[idx][hiTagIdx].tagV[loTagIdx] && cache[idx][hiTagIdx].tagHi == taghi && 
                        cache[idx][hiTagIdx].tagLo[loTagIdx] == taglo;
                  dcRdy = 1;
                  dcIdx = idx;
                  if(hit)
                    grp = loTagIdx;
                end
          end

          ///cache hit
          if(hit) begin
            foreach(smSel[j])
              if(!smSel[j][bk].xhgOpy) begin
                smSel[j][bk].xhgOpy = 1;
                slot = j;
                oc = 1;
                break;
              end
          end
          ///external access
          else begin
            ///a external store still need xchg network            
            if(ise.op inside {st_ops}) begin
              ex = 0;
              foreach(smSel[j])
                if(!smSel[j][bk].xhgOpy) begin
                  smSel[j][bk].xhgOpy = 1;
                  slot = j;
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
            if(!selExp) expCause = EC_SMBOND;
            selExp = 1;
            exp = 1;
            continue;
          end
          
          foreach(smSel[j])
            if((smSel[j][bk].sMemAdr == adr && smSel[j][bk].sMemGrp == grp 
                && smSel[j][bk].sMemOpy) || !smSel[j][bk].xhgOpy) begin
              smSel[j][bk].sMemOpy = 1;
              smSel[j][bk].xhgOpy = 1;
              slot = j;
              oc = 1;
              break;
            end
        end
        
        selOcValid = selOcValid || oc;
        selExValid = selExValid || ex;
      
        if(oc) begin
///          res = sharedMem[grp][adr][bk];
          smSel[slot][bk].sMemAdr = adr;
          smSel[slot][bk].sMemGrp = grp;
          smSel[slot][bk].sMemGrp = grp;
          smSel[slot][bk].sMemOs = os;
///          selXhgCyc[sp] = slot;
          
///          case(ise.op)
///          op_lbu  : res = {'0, res.b[adr]};
///          op_lb   : res = {{WORD_BITS{res.b[adr][7]}}, res.b[adr]};
///          op_lhu  : res = {'0, res.h[adr >> WID_HALF]};
///          op_lh   : res = {{WORD_BITS{res.h[adr >> WID_HALF].b[HALF_BYTES - 1][7]}}, res.h[adr >> WID_HALF]};
///          op_sb   : sharedMem[grp][adr][bk].b[adr] = rfm.st[sp];
///          op_sh   : sharedMem[grp][adr][bk].h[adr >> WID_HALF] = rfm.st[sp];
///          op_sw   : sharedMem[grp][adr][bk] = rfm.st[sp];
///          endcase
///          ldXchgBuf[cyc][bk] = res;
        end
        
        if(ex) begin
          wordu st = rfm.st[sp];
          if(ise.op inside {op_lbu, op_lb, op_sb})
            smSel[grp][bk].exEn[ed ? adr : (WORD_BYTES - 1 - adr)] = 1;
          else if(ise.op inside {op_lhu, op_lh, op_sh}) begin
            int adr2 = adr & `GMH(WID_HALF - 1);
            adr2 = ed ? adr2 : (HALF_BYTES - adr2);
            for(int j = 0; j < HALF_BYTES; j++)
              smSel[grp][bk].exEn[j] = 1;
          end
          else if(ise.op inside {op_lw, op_sw})
            for(int j = 0; j < WORD_BYTES; j++)
              smSel[grp][bk].exEn[j] = 1;
              
          case(ise.op)
          op_sw   : 
            for(int j = 0; j < WORD_BYTES; j++)
              smSel[grp][bk].stData.b[j] = st.b[ed ? j : (WORD_BYTES - 1 - j)];
          op_sh   : begin
            int adr2 = adr & `GMH(WID_HALF - 1);
            adr2 = ed ? adr2 : (HALF_BYTES - adr2);
            for(int j = 0; j < HALF_BYTES; j++)
              smSel[grp][bk].stData.b[adr2 + j] = st.b[ed ? (adr2 + j) : (adr2 + HALF_BYTES - 1 - j)];
          end
          op_sb   :
            smSel[grp][bk].stData.b[ed ? adr : (WORD_BYTES - 1 - adr)] = st.b[adr];
          endcase
        end
        spDc[0][sp] = new();
        spDc[0][sp].exp = exp;
        spDc[0][sp].oc = oc;
        spDc[0][sp].ex = ex;
        spDc[0][sp].slot = slot;
      end
      
      ///finish one half wrap or whole request
      if((((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0) || (ise.subVec == ise.vecMode) || !ise.vec) begin
        uchar lvl = ise.subVec & `GML(WID_DCH_CL);
        lvl = ise.vec ? lvl : (LAT_XCHG - 1);
        for(int i = lvl; i < LAT_XCHG; i++) begin
          smDc[i] = smSel[i - lvl];
            for(int j = 0; j < NUM_SMEM_BK; j++)
              smSel[i - lvl][j] = new();
        end
          
///        ///is ex?
///        if(selExValid && (!selExp || ise.nonBlock)) begin
///          uchar ldQueFreeId, stQueFreeId;
///          bit fndLdQue = 0, fndStQue = 0;
///          foreach(stQue[i])
///            if(!stQue[i].en) begin
///              stQue[i].en = 1;
///              stQueFreeId = i;
///              fndStQue = 1;
///              break;
///            end
///      
///          foreach(ldQue[i])
///            if(!ldQue[i].en) begin
///              ldQue[i].en = 1;
///              ldQueFreeId = i;
///              fndLdQue = 1;
///              break;
///            end
///            
///          foreach(eifTr[i]) begin
///            if(i < lvl) continue;
///            if(eifTr[i] == null) eifTr[i] = tr_dse2eif::type_id::create("toEIF", this);
///            eifTr[i].op = ise.op;
///            eifTr[i].id = ldQueFreeId;
///            eifTr[i].req = 1;
///            eifTr[i].data = stXchgBuf[i];
///            eifTr[i].wrEn = selExEMsk;
///            eifTr[i].pAdr = selExAdr;
///          end
///          
///          if(ise.op inside {ld_ops}) begin
///            ldQue[ldQueFreeId].en = 1;
///            ldQue[ldQueFreeId].grp = ise.wrGrp;
///            ldQue[ldQueFreeId].adr = ise.wrAdr;
///            ldQue[ldQueFreeId].bk = ise.wrBk;
///            ldQue[ldQueFreeId].tid = ise.tid;
///            ldQue[ldQueFreeId].subVec = ise.subVec & `GMH(WID_DCH_CL);
///            ldQue[ldQueFreeId].op = ise.op;
///            ldQue[ldQueFreeId].xhgCtl = ldXchgCtl;
///            ldQue[ldQueFreeId].wrEn = selExEMsk;
///          end
///          else if(ise.op inside {st_ops}) begin
///            stQue[stQueFreeId].en = 1;
///            stQue[stQueFreeId].tid = ise.tid;
///          end
///        end
///        
///        if(selOcValid && (!selExp || ise.nonBlock)) begin
///          foreach(rfmTr[i]) begin
///            tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL + i];
///            tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL + i];
///            if(i < lvl) continue;
///            if(rfmTr[i] == null) rfmTr[i] = tr_dse2rfm::type_id::create("toRFM", this);
///            if(rfm == null || ise == null) begin
///              ovm_report_warning("dse", "ise or rfm req missing");
///              continue;
///            end
///            rfmTr[i].res = ldXchgBuf[i];
///            rfmTr[i].updateAdrRes = rfm.base;
///            rfmTr[i].wrEn = selOcEMsk;
///            rfmTr[i].wrGrp = ise.wrGrp;
///            rfmTr[i].wrAdr = ise.wrAdr;
///            rfmTr[i].updateAdrWrBk = ise.updateAdrWrBk;
///            rfmTr[i].updateAdrWrAdr = ise.updateAdrWrAdr;
///            rfmTr[i].updateAdrWrGrp = ise.updateAdrWrGrp;
///            rfmTr[i].subVec = ise.subVec;
///          end
///        end
///        
///        ///whole request finished
///        if((ise.subVec == ise.vecMode) || !ise.vec) begin
///          if(iseTr[0] == null) iseTr[0] = tr_dse2ise::type_id::create("toISE", this);
///          iseTr[0].exp = selExp && !ise.nonBlock;
///          iseTr[0].ext = selHasEx && !ise.nonBlock;
///          iseTr[0].rsp = 1;
///          iseTr[0].pendExLoad = 1;
///          iseTr[0].pendExStore = 1;
///          selExp = 0;
///          selHasEx = 0;
///        end
///        else
///          selHasEx = selHasEx || selExValid;
///          
///        selOcValid = 0;
///        selExValid = 0;
///        selsMemOpy = '{default : 0};
///        selXchgBk = '{default : 0};
///        selSMemAdr = '{default : 0};
///        selExRdy = 0;
      end
    end
    
    ///spu ops
    if(v.fmSPU[STAGE_RRF_AG] != null && v.fmSPU[STAGE_RRF_AG].srReq) begin
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
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
        if(spuTr[0] == null)
          spuTr[0] = tr_dse2spu::type_id::create("toSPU", this);
        spuTr[0].rsp = 1;
        case(spu.srAdr)
        SR_MBASE: spuTr[0].srRes = srMapBase;
        SR_OCMC:  spuTr[0].srRes = srCacheGrp;
        endcase
      end
      endcase
    end
    vn.rfm[STAGE_RRF_DEM] = rfmTr[LAT_XCHG - 1];

  endfunction
  
  function void req_proc();
    tr_dse2rfm toRFM;
    tr_dse2spu toSPU;
    tr_dse2eif toEIF;
    tr_dse2ise toISE;
    tr_dse2tlb toTLB;
    
    ovm_report_info("dse", "req_proc procing...", OVM_FULL); 
    
    ///select vadr from ise req to tlb for translation
    if(v.fmISE[STAGE_RRF_AG] != null && v.fmRFM[STAGE_RRF_AG] != null && v.fmISE[STAGE_RRF_AG].subVec == 0
       && v.fmSPU[STAGE_RRF_AG] != null && v.fmISE[STAGE_RRF_AG].en) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_AG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_AG];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
      int vadr = 0;
      bit found = 0;
      
      foreach(spu.emsk[i])
        if(spu.emsk[i]) begin
          rfm.base[i] = rfm.base[i] + rfm.os[i];
          if(rfm.base[i] >= VADR_MAPPED && rfm.base[i] < VADR_NMAPNC) begin
           found = 1;
           vadr = rfm.base[i];
          end
        end
        
      if(found && ise.en) begin
        toTLB = tr_dse2tlb::type_id::create("toTLB", this);
        toTLB.vAdr = vadr >> VADR_START;
        toTLB.op = ise.op;
        toTLB.tid = ise.tid;
        toTLB.req = 1;
        tlbReqVAdr[STAGE_RRF_TAG] = toTLB.vAdr;
      end
    end
    else
      tlbReqVAdr[STAGE_RRF_TAG] = tlbReqVAdr[STAGE_RRF_TAG + 1];
    
    toSPU = spuTr[LAT_XCHG - 1];
    toRFM = rfmTr[LAT_XCHG - 1];
    toEIF = eifTr[LAT_XCHG - 1];
    toISE = iseTr[LAT_XCHG - 1];
    
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
    vn.fmEIF = req;
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

    foreach(smSel[i, j])
      smSel[i][j] = new();
      
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    srCacheGrp = 0;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
