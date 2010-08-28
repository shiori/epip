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
  tr_ise2dse fmISE[STAGE_RRF_VWB0:STAGE_RRF_RRC0];
  tr_spu2dse fmSPU[STAGE_RRF_SEL:STAGE_RRF_AG];
  tr_rfm2dse fmRFM[STAGE_RRF_SEL:STAGE_RRF_AG];
  tr_spa2dse fmSPA;
  tr_tlb2dse fmTLB;
  tr_eif2dse fmEIF;    /// external interfaces
  
  tr_dse2ise ise;
///  tr_dse2spu spu[STAGE_RRF_SR0:STAGE_RRF_EXS2];
  tr_dse2rfm rfm[STAGE_RRF_VWBP:STAGE_RRF_DEM0];
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


///---------------------------------------main component----------------------------------------
class ip4_tlm_dse extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_dse_vars v, vn;  
  local wordu sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SMEM_BK];
  local uint cacheTagHi[NUM_DCHE_TAG][NUM_DCHE_ASO],
             cacheTagLo[NUM_DCHE_TAG][NUM_DCHE_ASO][NUM_SMEM_GRP];
  local bit cacheTagV[NUM_DCHE_TAG][NUM_DCHE_ASO][NUM_SMEM_GRP];
  local bit cacheDirty[NUM_DCHE_TAG][NUM_DCHE_ASO][NUM_SMEM_GRP];
  local word tlbReqVAdr[STAGE_RRF_SEL:STAGE_RRF_TAG];
  local bit selOcValid, selExValid, reqHasEx;
  local bit selOcEMsk[LAT_XCHG][NUM_SP], selExEMsk[LAT_XCHG][NUM_SP];
  local bit selSMemBk[LAT_XCHG][NUM_SMEM_BK], selXchgBk[LAT_XCHG][NUM_SMEM_BK];
  local uint selSMemAdr[LAT_XCHG][NUM_SMEM_BK];
  local padr_t exAdr;
  local bit exRdy, exAccEn[LAT_XCHG][NUM_SMEM_BK][WORD_BYTES];
  local wordu stXchgBuf[LAT_XCHG][NUM_SMEM_BK],
              ldXchgBuf[LAT_XCHG][NUM_SMEM_BK],
              exStBuf[LAT_XCHG][NUM_SMEM_BK];
  local uchar ldXchgCtl[LAT_XCHG][NUM_SMEM_BK];
             
  local uchar ldQueXhgCtl[NUM_LDQUE][LAT_XCHG][NUM_SMEM_BK];
  local bit ldQueWrEn[NUM_LDQUE][LAT_XCHG][NUM_SMEM_BK], ldQueEn[NUM_LDQUE];
  local uchar ldQueGrp[NUM_LDQUE], ldQueAdr[NUM_LDQUE], ldQueBk[NUM_LDQUE], 
              ldQueTid[NUM_LDQUE], ldQueSubVec[NUM_LDQUE];
  local opcode_e ldQueOP[NUM_LDQUE];
  local uchar stQueTid[NUM_STQUE];
  local bit stQueEn[NUM_STQUE];

  local uchar srCacheGrp;
  local uint srMapBase;
  local bit cacheGrpEn[NUM_SMEM_GRP];
  local bit selExp;
  local uchar expVid;
  local cause_typs expCause;

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
    padr_t selPAdr[NUM_SP];
    bit selNoCache[NUM_SP];
    
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
      
    for (int i = STAGE_RRF_VWB0; i > STAGE_RRF_RRC0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[STAGE_RRF_RRC0] = null;

    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_AG; i--) begin
      vn.fmSPU[i] = v.fmSPU[i - 1];
      vn.fmRFM[i] = v.fmRFM[i - 1];
    end
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    
    for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_DEM0; i--) 
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_DEM0] = null;
    
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
    
    if(v.fmSPU[STAGE_RRF_SEL] != null && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      uchar pbId = ise.pbId, cyc = ise.subVec & `GML(WID_DCH_CL);
      uint vAdrHi;
      uint dcIdx, dcRdy = 0;
      bit ed = 0;

      ///vadr to padr translation stage
      if(tlb != null) begin
        vAdrHi = tlbReqVAdr[STAGE_RRF_SEL] >> tlb.eobit;
        ed = tlb.e;
      end
      foreach(rfm.base[i]) begin
        if(rfm.base[i] >= VADR_NMAPNC) begin
          if(rfm.base[i] >= VADR_NMAPCH)
            selPAdr[i] = rfm.base[i];
          else begin
            selNoCache[i] = 1;
            if(rfm.base[i] < VADR_EJTAGS)
              selPAdr[i] = srMapBase + rfm.base[i] - VADR_NMAPNC;
            else
              selPAdr[i] = srMapBase + EJTG_OFFSET + pbId * EJTG_SIZE + rfm.base[i] - VADR_EJTAGS;
          end
        end
        else if(tlb != null && tlb.hit && (rfm.base[i] >> (VADR_START + tlb.eobit)) == vAdrHi) begin
          selPAdr[i] = rfm.base[i];
          if(!spu.emsk[i]) continue;
          if(!selExp) begin
            selExp = tlb.exp;
            expVid = ise.subVec * NUM_SP + i;
            expCause = tlb.cause;
            selOcEMsk[cyc][i] = spu.emsk[i];
            selExEMsk[cyc][i] = spu.emsk[i];
          end
          selNoCache[i] = tlb.c == 0;
          for(int j = (VADR_START + tlb.eobit); j < PADR_WIDTH; j++)
            selPAdr[i][j] = tlb.pfn[j - VADR_START];
        end
      end
      
      ///access data
      foreach(selPAdr[i]) begin
        padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
               smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_BK - srCacheGrp) * SGRP_SIZE,
               smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;
        uchar bk, grp, adr, os;
        wordu res;
        bit oc = 0, ex = 0;
        
        if(!selOcEMsk[cyc][i] && !selExEMsk[cyc][i]) continue;
        
        ///align exp
        if((ise.op inside {ld_ops, op_fetadd} && selPAdr[i][1:0] != 2'b0)
           || (ise.op inside {op_lh, op_sh, op_lhu, op_cmpxchg} && selPAdr[0] != 1'b0)) begin
          if(!selExp) begin
            expCause = EC_ADRALG;
            expVid = ise.subVec * NUM_SP + i;
            selExp = 1;
          end
        end

        os = selPAdr[i] & `GML(WID_WORD);        
        bk = (selPAdr[i] >> WID_WORD) & `GML(WID_SMEM_BK);
        adr = selPAdr[i] >> (WID_WORD + WID_SMEM_BK) & `GML(WID_SMEM_ADR);
        
        ///external mem
        ///cache address:   | taghi | taglo | idx | cl | bk | offset |
        if(selPAdr[i] < smStart && selPAdr[i] >= smEnd2) begin
          bit hit = 0;
          uint idx;
          ///chk cache for match
          if(!selNoCache[i] && cacheGrpEn[i]) begin
            uint taglo = selPAdr[i] >> (WID_WORD + WID_SMEM_BK + WID_DCH_CL + WID_DCH_IDX),
                 taghi = taglo >> WID_DCHE_STAG;
            taglo = taglo & `GML(WID_DCHE_STAG);
            idx = adr >> WID_DCH_IDX;
            
            for(int hiTagIdx = 0; hiTagIdx < NUM_DCHE_ASO; hiTagIdx++)
              for(int loTagIdx = 0; loTagIdx < NUM_SMEM_GRP; loTagIdx++)
                if(!dcRdy || dcIdx == idx) begin
                  dcRdy = 1;
                  dcIdx = idx;
                  hit = cacheTagV[idx][hiTagIdx][loTagIdx] && cacheTagHi[idx][hiTagIdx] == taghi && 
                        cacheTagLo[idx][hiTagIdx][loTagIdx] == taglo;
                  dcRdy = 1;
                  dcIdx = idx;
                end
          end

          ///cache hit
          if(hit) begin
            foreach(selXchgBk[j])
              if(!selXchgBk[j][bk]) begin
                selXchgBk[j][bk] = 1;
                oc = 1;
                break;
              end
          end
          ///external access
          else begin
            grp = (selPAdr[i] >> (WID_WORD + WID_SMEM_BK)) & `GML(WID_DCH_CL);
///            adr = selPAdr[i] & `GML(WID_WORD);

            ///a external store still need xchg network            
            if(ise.op inside {op_sw, op_sh, op_sb, op_sc}) begin
              foreach(selXchgBk[j])
                if(!selXchgBk[j][bk]) begin
                  selXchgBk[j][bk] = 1;
                  ex = 1;
                  break;
                end
            end
            else begin
              ldXchgCtl[cyc][i] = selPAdr[i] & `GML(WID_DCH_CL + WID_SMEM_BK + WID_WORD);
              ex = 1;
            end
              
            if(!exRdy) begin
              exRdy = ex;
              exAdr = selPAdr[i] >> (WID_DCH_CL + WID_SMEM_BK + WID_WORD);
            end
            else
              ex = exAdr == (selPAdr[i] >> (WID_DCH_CL + WID_SMEM_BK + WID_WORD));
          end
        end
        ///shared mem
        else begin
          uint adr1;
          if(selPAdr[i] >= smEnd) begin
            if(!selExp) expCause = EC_SMBOND;
            selExp = 1;
            expVid = ise.subVec * NUM_SP + i;
            continue;
          end
          
          ///load req  
          adr1 = (selPAdr[i] >> (WID_SMEM_BK + WID_WORD)) & `GML(WID_SMEM_ADR + WID_SMEM_GRP);
          foreach(selXchgBk[j])
            if((selSMemAdr[j][bk] == adr1 && selSMemBk[j][bk]) || !selXchgBk[j][bk]) begin
              selSMemBk[j][bk] = 1;
              selXchgBk[j][bk] = 1;
              selSMemAdr[j][bk] = adr1;
              oc = 1;
              break;
            end
        end
        
        selOcEMsk[cyc][i] = oc;
        selExEMsk[cyc][i] = ex;
        selOcValid = selOcValid || oc;
        selExValid = selExValid || ex;
      
        if(oc) begin
          res = sharedMem[grp][adr][bk];
            
          case(ise.op)
          op_lbu  : res = {'0, res.b[adr]};
          op_lb   : res = {{WORD_BITS{res.b[adr][7]}}, res.b[adr]};
          op_lhu  : res = {'0, res.h[adr >> WID_HALF]};
          op_lh   : res = {{WORD_BITS{res.h[adr >> WID_HALF].b[HALF_BYTES - 1][7]}}, res.h[adr >> WID_HALF]};
          op_sb   : sharedMem[grp][adr][bk].b[adr] = rfm.st[i];
          op_sh   : sharedMem[grp][adr][bk].h[adr >> WID_HALF] = rfm.st[i];
          op_sw   : sharedMem[grp][adr][bk] = rfm.st[i];
          endcase
          ldXchgBuf[cyc][i] = res;
        end
        
        if(ex) begin
          wordu st = rfm.st[i];
          if(ise.op inside {op_lbu, op_lb, op_sb})
            exAccEn[grp][bk][ed ? adr : (WORD_BYTES - 1 - adr)] = 1;
          else if(ise.op inside {op_lhu, op_lh, op_sh}) begin
            int adr2 = adr & `GMH(WID_HALF - 1);
            adr2 = ed ? adr2 : (HALF_BYTES - adr2);
            for(int j = 0; j < HALF_BYTES; j++)
              exAccEn[grp][bk][j] = 1;
          end
          else if(ise.op inside {op_lw, op_sw})
            for(int j = 0; j < WORD_BYTES; j++)
              exAccEn[grp][bk][j] = 1;
              
          case(ise.op)
          op_sw   : 
            for(int j = 0; j < WORD_BYTES; j++)
              stXchgBuf[grp][bk].b[j] = st.b[ed ? j : (WORD_BYTES - 1 - j)];
          op_sh   : begin
            int adr2 = adr & `GMH(WID_HALF - 1);
            adr2 = ed ? adr2 : (HALF_BYTES - adr2);
            for(int j = 0; j < HALF_BYTES; j++)
              stXchgBuf[grp][bk].b[adr2 + j] = st.b[ed ? (adr2 + j) : (adr2 + HALF_BYTES - 1 - j)];
          end
          op_sb   :
            stXchgBuf[grp][bk].b[ed ? adr : (WORD_BYTES - 1 - adr)] = st.b[adr];
          endcase
        end
      end
      
      ///finish one half wrap or whole request
      if((((ise.subVec + 1) & `GML(WID_DCH_CL)) == 0)
          || (ise.subVec == ise.vecMode) || !ise.vec) begin
        uchar lvl = ise.subVec & `GML(WID_DCH_CL);
        lvl = ise.vec ? lvl : (LAT_XCHG - 1);
        ///is ex?
        if(selExValid && (!selExp || ise.nonBlock)) begin
          uchar ldQueFreeId, stQueFreeId;
          bit fndLdQue = 0, fndStQue = 0;
          foreach(stQueEn[i])
            if(!stQueEn[i]) begin
              stQueEn[i] = 1;
              stQueFreeId = i;
              fndStQue = 1;
              break;
            end
      
          foreach(ldQueEn[i])
            if(!ldQueEn[i]) begin
              ldQueEn[i] = 1;
              ldQueFreeId = i;
              fndLdQue = 1;
              break;
            end
            
          foreach(eifTr[i]) begin
            if(i < lvl) continue;
            if(eifTr[i] == null) eifTr[i] = tr_dse2eif::type_id::create("toEIF", this);
            eifTr[i].op = ise.op;
            eifTr[i].id = ldQueFreeId;
            eifTr[i].req = 1;
            eifTr[i].data = stXchgBuf[i];
            eifTr[i].wrEn = selExEMsk[i];
            eifTr[i].pAdr = exAdr;
          end
          
          if(ise.op inside {ld_ops}) begin
            ldQueEn[ldQueFreeId] = 1;
            ldQueGrp[ldQueFreeId] = ise.wrGrp;
            ldQueAdr[ldQueFreeId] = ise.wrAdr;
            ldQueBk[ldQueFreeId] = ise.wrBk;
            ldQueTid[ldQueFreeId] = ise.tid;
            ldQueSubVec[ldQueFreeId] = ise.subVec & `GMH(WID_DCH_CL);
            ldQueOP[ldQueFreeId] = ise.op;
            ldQueXhgCtl[ldQueFreeId] = ldXchgCtl;
            ldQueWrEn[ldQueFreeId] = selExEMsk;
          end
          else if(ise.op inside {st_ops}) begin
            stQueEn[stQueFreeId] = 1;
            stQueTid[stQueFreeId] = ise.tid;
          end
        end
        
        if(selOcValid && (!selExp || ise.nonBlock)) begin
          foreach(rfmTr[i]) begin
            tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL + i];
            tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL + i];
            if(i < lvl) continue;
            if(rfmTr[i] == null) rfmTr[i] = tr_dse2rfm::type_id::create("toRFM", this);
            if(rfm == null || ise == null) begin
              ovm_report_warning("dse", "ise or rfm req missing");
              continue;
            end
            rfmTr[i].res = ldXchgBuf[i];
            rfmTr[i].updateAdrRes = rfm.base;
            rfmTr[i].wrEn = selOcEMsk[i];
            rfmTr[i].wrGrp = ise.wrGrp;
            rfmTr[i].wrAdr = ise.wrAdr;
            rfmTr[i].updateAdrWrBk = ise.updateAdrWrBk;
            rfmTr[i].updateAdrWrAdr = ise.updateAdrWrAdr;
            rfmTr[i].updateAdrWrGrp = ise.updateAdrWrGrp;
            rfmTr[i].subVec = ise.subVec;
          end
        end
        
        ///whole request finished
        if((ise.subVec == ise.vecMode) || !ise.vec) begin
          if(iseTr[0] == null) iseTr[0] = tr_dse2ise::type_id::create("toISE", this);
          iseTr[0].exp = selExp && !ise.nonBlock;
          iseTr[0].ext = reqHasEx && !ise.nonBlock;
          iseTr[0].rstCnt = ise.rstCnt;
          iseTr[0].rsp = 1;
          iseTr[0].pendExLoad = 1;
          iseTr[0].pendExStore = 1;
          selExp = 0;
          reqHasEx = 0;
        end
        else
          reqHasEx = reqHasEx || selExValid;
          
        selOcValid = 0;
        selExValid = 0;
        selSMemBk = '{default : 0};
        selXchgBk = '{default : 0};
        selSMemAdr = '{default : 0};
        exRdy = 0;
        exStBuf = '{default : 0};
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
    vn.rfm[STAGE_RRF_DEM0] = rfmTr[LAT_XCHG - 1];

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
       && v.fmSPU[STAGE_RRF_AG] != null) begin
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
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    srCacheGrp = 0;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
