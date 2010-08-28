/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_ise.sv
/// Title            : ip4 instruction stream engine
/// Version          : 0.1
/// Last modified    : Mar 16 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Mar 16 2010

typedef enum uchar {
  exp_decode_err,   exp_dse_err,    exp_priv_err,     exp_msc_err,
  exp_ife_err,      exp_fu_err
}ise_exp_t;

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fmSPU;
  tr_rfm2ise fmRFM;
  tr_ife2ise fmIFE;
  tr_spa2ise fmSPA;
  tr_eif2ise fmEIF, pendEIF;
  tr_dse2ise fmDSE[STAGE_ISE_VWBP:STAGE_ISE_DEM0];
  
  tr_ise2rfm rfm[STAGE_ISE:1];
  tr_ise2spa spa[STAGE_ISE:1];
  tr_ise2spu spu[STAGE_ISE:1];
  tr_ise2dse dse[STAGE_ISE:1];
  
  uchar TIdIssueLast, TIdFetchLast;
  bit cancel[NUM_THREAD];
  uint pcRst[STAGE_ISE_VWB:1];
  uint igSizeRst[STAGE_ISE_VWB:1];
  uchar pdMemAccRst[STAGE_ISE_VWB:1];
  bit privRst[STAGE_ISE_VWB:1];
///      pdLdRst[STAGE_ISE_VWB:1],
///      pdStRst[STAGE_ISE_VWB:1];
  
  `ovm_component_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(pendEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_int(TIdIssueLast, OVM_ALL_ON)
    `ovm_field_int(TIdFetchLast, OVM_ALL_ON)
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
    `ovm_field_sarray_int(pcRst, OVM_ALL_ON)
    `ovm_field_sarray_int(igSizeRst, OVM_ALL_ON)
    `ovm_field_sarray_int(privRst, OVM_ALL_ON)
  `ovm_component_utils_end

  function new(string name, ovm_component parent);
    super.new(name, parent);
    TIdFetchLast = 0;
    TIdIssueLast = 0;
    print_enabled = 0;
  endfunction : new
endclass : ip4_tlm_ise_vars

class ise_thread_inf extends ovm_component;
  ise_thread_state threadState;
  uchar iBuf[$];
  bit dseVec;
  uchar iGrpBytes, adrPkgBytes, numImms,
        cntSrfRd, cntVrfRd, cntDSERd;
  wordu co[NUM_BP_CO];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS], vrfGrp[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_SRF_BKS], srfGrp[CYC_VEC][NUM_SRF_BKS];
  bit vrfRdEn[CYC_VEC][NUM_VRF_BKS], srfRdEn[CYC_VEC][NUM_SRF_BKS];
  uchar cntPRWr, cntVrfWr[NUM_VRF_BKS], cntSrfWr[NUM_SRF_BKS];
  
  bit enSPU, enDSE, enVec, enFu[NUM_FU];
  bit privMode,  ///privilege running status
      ejtagMode,
      decoded,
      decodeErr,
      cancel;
  uchar wCnt[NUM_W_CNT], wCntNext, wCntSel, vecMode;
  
  uchar vrfMap[NUM_INST_VRF / NUM_PRF_P_GRP], 
        srfMap[NUM_INST_SRF / NUM_PRF_P_GRP];
  bit pendLoad, pendStore, lpRndMemMode, pendIFetchExp, IFetchExp;
  uchar pendIFetch, pendMemAcc, pendBr;
  uchar srThreadGrp, srFIFOMask, srCause, srUserEvent, srFIFOPend;
  round_mode srExeMode;
  cause_typs pendIFetchCause;
  
  inst_c iSPU, iDSE, iFu[NUM_FU];
  uint pc, pcEret, pcUEret, srUEE;
  bit brPred;
    
  `ovm_component_utils_begin(ise_thread_inf)
    `ovm_field_enum(ise_thread_state, threadState, OVM_ALL_ON)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decodeErr, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
    `ovm_field_int(privMode, OVM_ALL_ON)
    `ovm_field_int(ejtagMode, OVM_ALL_ON)
    `ovm_field_queue_int(iBuf, OVM_ALL_ON)
    `ovm_field_int(wCntSel, OVM_ALL_ON)
    `ovm_field_int(iGrpBytes, OVM_ALL_ON)
    `ovm_field_int(adrPkgBytes, OVM_ALL_ON)
    `ovm_field_int(numImms, OVM_ALL_ON)
    `ovm_field_int(cntSrfRd, OVM_ALL_ON)
    `ovm_field_int(cntVrfRd, OVM_ALL_ON)
    `ovm_field_int(cntDSERd, OVM_ALL_ON)
    `ovm_field_sarray_int(co, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_sarray_int(enFu, OVM_ALL_ON)
    `ovm_field_int(enVec, OVM_ALL_ON)
    `ovm_field_sarray_int(wCnt, OVM_ALL_ON)
    `ovm_field_int(wCntNext, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(pcEret, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_int(pendLoad, OVM_ALL_ON)
    `ovm_field_int(pendStore, OVM_ALL_ON)
    `ovm_field_int(lpRndMemMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(pendMemAcc, OVM_ALL_ON)
    `ovm_field_int(pendBr, OVM_ALL_ON)
    `ovm_field_sarray_int(vrfMap, OVM_ALL_ON)
    `ovm_field_sarray_int(srfMap, OVM_ALL_ON)
    `ovm_field_object(iSPU, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_object(iDSE, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_object(iFu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end

	virtual function void do_print(ovm_printer printer);
		super.do_print(printer);
	  if(get_report_verbosity_level() >= OVM_HIGH) begin
  		if(enSPU)
  		  printer.print_object("spu", iSPU);
  		if(enDSE)
  		  printer.print_object("dse", iDSE);
  		foreach(enFu[i])
  		  if(enFu[i])
  		    printer.print_object($psprintf("fu%0d", i), iFu[i]);
    end
	    
    `PAF2(vrfAdr, OVM_DEC)
    `PAF2(srfAdr, OVM_DEC)
    `PAF2(vrfGrp, OVM_DEC)
    `PAF2(srfGrp, OVM_DEC)
	endfunction : do_print
	
  function new(string name, ovm_component parent);
    super.new(name, parent);
    iSPU = new();
    iDSE = new();
    foreach(iFu[i])
      iFu[i] = new();
    threadState = ts_disabled;
    privMode = 0;
    pc = CFG_START_ADR;
    vecMode = CYC_VEC - 1;
    decoded = 0;
    decodeErr = 0;
    pendIFetchExp = 0;
    IFetchExp = 0;
    print_enabled = 0;
  endfunction : new
 
  function void map_iadr(input bit v, uchar orgAdr, output uchar grp, adr);
    uchar adrBits =  v ? (WID_PRF_P_GRP - WID_VRF_BKS) : (WID_PRF_P_GRP - WID_SRF_BKS);
    adr = orgAdr & `GML(adrBits);
    grp = orgAdr >> adrBits;
    grp = v ? vrfMap[grp] : srfMap[grp];
  endfunction : map_iadr

  function void cyc_new();
    cancel = 0;
    foreach(wCnt[i])
      if(wCnt[i] != 0) wCnt[i]--;
  endfunction : cyc_new

  function void br_pred();
    pendBr++;
    if(iSPU.offSet == 0) begin
      brPred = 1;
      threadState = ts_b_self;
    end
    else begin
      brPred = 0;
      threadState = (threadState == ts_rdy) ? ts_w_b : ts_b_pred;
    end
  endfunction : br_pred
    
  function void decode_igrp_start();
    i_gs0_t grpStart = iBuf[0];
    enSPU = 0;
    enDSE = 0;
    enVec = 0;
    enFu = '{default : 0};

    if(!grpStart.t) begin
      wCntSel = grpStart.chkGrp;
      adrPkgBytes = grpStart.adrPkgB;
      numImms = grpStart.immPkgW;
      dseVec = grpStart.unitEn;
      iGrpBytes = 1 + adrPkgBytes + numImms * WORD_BYTES + NUM_INST_BYTES;
    end
    else begin
      i_gs1_u grpStart;
      uchar tmp = 0;
      foreach(grpStart.b[i])
        grpStart.b[i] = iBuf[i];
      foreach(grpStart.i.unitEn[i])
        tmp += grpStart.i.unitEn[i];
      if(tmp == 0) begin
        ovm_report_warning("decode_igrp_start", "igs decode error, unitEn not valid");
        decodeErr = 1;
      end
      wCntSel = grpStart.i.chkGrp;
      adrPkgBytes = grpStart.i.adrPkgB;
      numImms = grpStart.i.immPkgW;
      iGrpBytes = 2 + adrPkgBytes + numImms * WORD_BYTES + tmp * NUM_INST_BYTES;
      enSPU = grpStart.i.unitEn[0];
      enDSE = grpStart.i.unitEn[1];
      dseVec = grpStart.i.dv;
      foreach(enFu[i])
        enFu[i] = grpStart.i.unitEn[2+i];
    end
        
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [NUM_FU - 1 : 0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = enFu[i];
        
      ovm_report_info("decode_igrp_start",
        $psprintf("inst grp len %0d bytes includes: spu:%0b, dse:%0b, fu:%b. dv:%0b, wCntSel:%0b, adrPkgB:%0d, immPkgW:%0d", 
                   iGrpBytes, enSPU, enDSE, enFuTmp, dseVec, wCntSel, adrPkgBytes, numImms),
        OVM_HIGH);
    end
  endfunction : decode_igrp_start
    
  function void decode_igrp();
    uchar tmp = 0;
    iga_t adrs[12];
    uchar offSet;
    i_gs0_t grpStart = iBuf[0];
    
    vrfRdEn = '{default : 0};
    srfRdEn = '{default : 0};
    cntVrfRd = 0;
    cntSrfRd = 0;
    cntDSERd = 0;
    cntVrfWr = '{default : 0};
    cntSrfWr = '{default : 0};
    cntPRWr = 0;
    wCntNext = 0;
        
    if(!grpStart.t) begin
      tmp = 1;
      offSet = 1;
///      if(adrPkgBytes != 0) adrPkgBytes --;???
      iSPU.set_data(iBuf, offSet, 0, dseVec);
      iDSE.set_data(iBuf, offSet, 0, dseVec);
      foreach(iFu[i])
        iFu[i].set_data(iBuf, offSet, i, 1);
        
      offSet += NUM_INST_BYTES;
      iSPU.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
      iSPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
      iSPU.analyze_fu(enSPU, enDSE, enFu);
      adrs[0] = grpStart.a;
///      if(adrPkgBytes) begin
///        i_ap0_t AdrPkg = iBuf[offSet];
///        foreach(AdrPkg.a[i])
///          adrs[i] = AdrPkg.a[i];
///        offSet ++;
///      end
    end
    else begin
      i_gs1_u grpStart;
      foreach(grpStart.b[i])
        grpStart.b[i] = iBuf[i];
      offSet = 2;
      tmp = 1;
///      if(adrPkgBytes != 0) adrPkgBytes --; why is this???
      
      if(enSPU) begin
        iSPU.set_data(iBuf, offSet, 0, 0);
        iSPU.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
        iSPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
        offSet += NUM_INST_BYTES;
      end
      
      if(enDSE) begin
        iDSE.set_data(iBuf, offSet, 0, dseVec);
        iDSE.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
        iDSE.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
        offSet += NUM_INST_BYTES;
      end
      
      foreach(iFu[i])
        if(enFu[i]) begin
          iFu[i].set_data(iBuf, offSet, i, 1);
          iFu[i].analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
          iSPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
          offSet += NUM_INST_BYTES;          
        end

      adrs[0] = grpStart.i.a;
    end

    foreach(enFu[i])
      enVec |= enFu[i];
          
    ///fill in rf address
    while(adrPkgBytes != 0) begin
      if(adrPkgBytes >= 3) begin
        i_ap2_u AdrPkg;
        foreach(AdrPkg.b[i]) begin
          AdrPkg.b[i] = iBuf[offSet];
          offSet++;
        end
        foreach(AdrPkg.i.a[i])
          adrs[tmp++] = AdrPkg.i.a[i];
        adrPkgBytes -= 3;
      end
      else if(adrPkgBytes >= 2) begin
        i_ap1_u AdrPkg;
        foreach(AdrPkg.b[i]) begin
          AdrPkg.b[i] = iBuf[offSet];
          offSet++;
        end
        foreach(AdrPkg.i.a[i])
          adrs[tmp++] = AdrPkg.i.a[i];
        adrPkgBytes -= 2;
      end
      else if(adrPkgBytes >= 1) begin
        i_ap0_t AdrPkg;
        AdrPkg = iBuf[offSet];
        offSet++;
        foreach(AdrPkg.a[i])
          adrs[tmp++] = AdrPkg.a[i];
        adrPkgBytes -= 1;
      end
    end
      
    for(int i = 0; i < numImms; i++) begin
      for(int j = 0; j < WORD_BYTES; j++)
        co[i].b[j] = iBuf[i + j];
      offSet += WORD_BYTES;
    end
      
    ///allocate reg read address
    tmp = 0;
    
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfRdEn[i][j]) begin
          map_iadr(1, adrs[tmp], vrfGrp[i][j], vrfAdr[i][j]);
          tmp++;
        end
        else if(j > 0) begin
           vrfGrp[i][j] =  vrfGrp[i][j - 1];
           vrfAdr[i][j] = vrfAdr[i][j - 1];
        end
          
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfRdEn[i][j]) begin
          map_iadr(0, adrs[tmp], srfGrp[i][j], srfAdr[i][j]);
          tmp++;
        end
    end
    
    foreach(iFu[fid]) begin
      iFu[fid].set_wcnt(wCntNext);
      decodeErr |= iFu[fid].decodeErr;
    end
    iSPU.set_wcnt(wCntNext);
    iDSE.set_wcnt(wCntNext);
    decodeErr |= iSPU.decodeErr;
    decodeErr |= iDSE.decodeErr;
    
    decoded = 1;
    ovm_report_info("decode_igrp", {"\n", sprint()}, OVM_HIGH);
  endfunction : decode_igrp

  function void flush();
    iBuf = {};
    iGrpBytes = 0;
    decoded = 0;
    decodeErr = 0;
    pendIFetch = 0;
    threadState = ts_rdy;
    cancel = 1;
    pendIFetchExp = 0;
    IFetchExp = 0;
  endfunction : flush

  function bit can_req_ifetch();
    ovm_report_info("can_req_ifetch", $psprintf("threadState:%s, iBuf lv:%0d, pd:%0d", threadState.name, iBuf.size(), pendIFetch), OVM_HIGH);
    if(threadState == ts_disabled)
      return 0;
    if(iBuf.size() + pendIFetch * NUM_IFET_BYTES >=  NUM_IBUF_BYTES)
      return 0;
    if(iGrpBytes == 0)
      return 1;
    if(iBuf.size() < iGrpBytes)
      return 1;
    return 0;
  endfunction : can_req_ifetch
      
  function void update_inst(input inst_fg_c fetchGrp);
    uchar offSet = 0, LvlLast = iBuf.size();
    if(LvlLast  >= NUM_MAX_IGRP_BYTES)
      ovm_report_warning("ise", "iBuf overflow!");
    if(LvlLast == 0) ///only calculate offSet when iBuf size is reset to 0
      offSet = pc & `GML(WID_IFET);

    if(pendIFetch > 0)
      pendIFetch--;
    
    if(cancel) begin
      ovm_report_info("update_inst", $psprintf("cancel, pc:0x%0h, offSet:%0h, pd:%0d", pc, offSet, pendIFetch), OVM_HIGH);
      return;
    end
    
    if(fetchGrp.exp) begin
      pendIFetchCause = EC_TLBLOAD;
      pendIFetchExp = 1;
      return;
    end
    else if(fetchGrp.accErr) begin
      pendIFetchCause = EC_IFACC;
      pendIFetchExp = 1;
      return;
    end
    else if(fetchGrp.k && !privMode) begin
      pendIFetchCause = EC_EXEPRIV;
      pendIFetchExp = 1;
      return;
    end
    
    foreach(fetchGrp.data[i])
      if(i >= offSet)
        iBuf.push_back(fetchGrp.data[i]);

    ovm_report_info("update_inst", $psprintf("pc:0x%0h, offSet:%0h, pd:%0d, iBuf lv %0d->%0d", pc, offSet, pendIFetch, LvlLast, iBuf.size()), OVM_HIGH);
  endfunction : update_inst

  function void fill_ife(input tr_ise2ife ife);
    ife.fetchReq = 1;
    ife.pc = (pc + NUM_IFET_BYTES * pendIFetch) & `GMH(WID_IFET);
    pendIFetch++;
  endfunction : fill_ife
  
endclass : ise_thread_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;

  local uchar cntVrfRd, cntSrfRd, cntDSERd, cntVecProc,
              cntPRWr, cntSrfWr[NUM_SRF_BKS], cntVrfWr[NUM_VRF_BKS];
        
  local bit noFu[NUM_FU];
  local uchar noLd, noSt, noSMsg, noRMsg;
  
  local tr_ise2rfm ciRFM[CYC_VEC];
  local tr_ise2spa ciSPA[CYC_VEC];
  local tr_ise2spu ciSPU[CYC_VEC];
  local tr_ise2dse ciDSE[CYC_VEC];
  
  ovm_nonblocking_transport_imp_spu #(tr_spu2ise, tr_spu2ise, ip4_tlm_ise) spu_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2ise, tr_spa2ise, ip4_tlm_ise) spa_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2ise, tr_rfm2ise, ip4_tlm_ise) rfm_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2ise, tr_ife2ise, ip4_tlm_ise) ife_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2ise, tr_dse2ise, ip4_tlm_ise) dse_tr_imp;
  ovm_nonblocking_transport_imp_eif #(tr_eif2ise, tr_eif2ise, ip4_tlm_ise) eif_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_ise2rfm, tr_ise2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spu, tr_ise2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spa, tr_ise2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2ife, tr_ise2ife) ife_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2dse, tr_ise2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2eif, tr_ise2eif) eif_tr_port;

  local ip4_tlm_ise_vars v, vn;
  local ise_thread_inf thread[NUM_THREAD];
  local ip4_printer printer;
  local uchar srPBId, rstCnt;
  local uint srExpBase;
  local bit srSupMsgMask, srPerfCntMask, srTimerMask, srReducePower, srDisableTimer,
            srTimerPend,  srSupMsgPend;
  local bit[1:0] srPerfCntPend;
  local tr_ise2eif toEIF;
  
  `ovm_component_utils_begin(ip4_tlm_ise)
    `ovm_field_int(cntVecProc, OVM_ALL_ON)
    `ovm_field_int(cntVrfRd, OVM_ALL_ON)
    `ovm_field_int(cntSrfRd, OVM_ALL_ON)
    `ovm_field_int(cntDSERd, OVM_ALL_ON)
    `ovm_field_int(noLd, OVM_ALL_ON)
    `ovm_field_int(noSt, OVM_ALL_ON)
    `ovm_field_int(noSMsg, OVM_ALL_ON)
    `ovm_field_int(noRMsg, OVM_ALL_ON)
    `ovm_field_sarray_int(noFu, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
    `ovm_field_int(srPBId, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(srExpBase, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end

  function void restorePC(input uchar tid, bit nxt = 0, uchar stage = 0, ejtag = 0);
    ise_thread_inf t = thread[tid];
    uint target = stage == 0 ? t.pc : v.pcRst[stage],
         offSet = stage == 0 ? t.iGrpBytes : v.igSizeRst[stage];
    offSet = nxt == 0 ? 0 : nxt;
    
    t.privMode = stage == 0 ? t.privMode : v.privRst[stage];
    if(t.ejtagMode || ejtag) begin
      t.pc = VADR_EJTAGS;
      t.ejtagMode = 1;
    end
    else begin
      t.pcEret = target + offSet;
      t.pc = srExpBase;
    end
  endfunction
  
  function void enter_exp(input uchar tid, ise_exp_t expType, uchar stage = 0, cause_typs cause = EC_DECODE);
    ise_thread_inf t = thread[tid];
    t.privMode = 1;
    restorePC(tid, 0, stage);
    case(expType)
    exp_decode_err:
      t.srCause = EC_DECODE;
    exp_dse_err:
      t.srCause = cause;
    exp_priv_err:
      t.srCause = EC_EXEPRIV;
    exp_msc_err:
    begin
      t.srCause = EC_MSC;
      restorePC(tid, 1, stage);
    end
    exp_ife_err:
      t.srCause = t.pendIFetchCause;
    exp_fu_err:
      t.srCause = EC_FUEXP;
    endcase
    t.flush();
  endfunction : enter_exp

  function void dse_wait(input uchar tid);
    ise_thread_inf t = thread[tid];
    ///its possible a previous blocking mem access followed by nonBlock access
    t.pendMemAcc = 0;
    t.pendLoad = 0;
    t.pendStore = 0; 
    t.threadState = ts_rdy;
    t.cancel = 1;   
    restorePC(tid, 1, STAGE_ISE_DEM0);
  endfunction : dse_wait

  function void resolve_br(input uchar tid, bit br, uchar stage);
    ise_thread_inf t = thread[tid];
    if(t.threadState inside {ts_w_b, ts_b_pred, ts_b_self}) begin
      if(t.pendBr > 0) t.pendBr--;
      if(t.pendBr == 0)
        t.threadState = ts_rdy;
      if(br != t.brPred) begin
        t.flush();
        restorePC(tid, 1, stage);
        t.cancel = 1;
      end
    end
  endfunction : resolve_br
    
  function word exe_ise(input uchar tid, opcode_e op, word op0 = 0, uchar sr = 0);
    ise_thread_inf t = thread[tid];
    word res;
    case(op)
    op_exit,
    op_sys: 
    begin
      restorePC(tid); ///, t.iGrpByte); 
      t.privMode = 1;
      t.srCause = EC_SYSCAL;
      t.flush();     
    end
    op_brk  :
    begin
      restorePC(tid, 0, 0, 1); 
      t.privMode = 1;
      t.srCause = EC_BREAK;
      t.flush();     
    end
    op_wait:
    begin
    end
    op_eret:
    begin
      t.pc = t.pcEret;
      t.privMode = 0;
      t.flush();
    end
    op_tsync:
    begin
    end
    op_msync:
    begin
    end
    op_alloc:
    begin
      if(op0[2])
        t.vrfMap[op0[24:21]] = op0[19:16];
      else
        t.srfMap[op0[24:21]] = op0[19:16];
    end
    op_gp2s:
    begin
      case(sr)
      SR_PROC_CTL :
      begin
        foreach(thread[i])
          if(thread[i].threadState == ts_disabled && op0[i])
            thread[i].threadState = ts_rdy;
        srPBId = op0[19:16];
        srDisableTimer = op0[25];
        srReducePower = op0[26];
        srTimerMask = op0[27];
        srPerfCntMask = op0[29:28];
        srSupMsgMask = op0[29];
      end
      SR_EBASE    :
///      begin
        srExpBase = op0;
///      end
      SR_THD_CTL  :
      begin
        t.privMode = op0[0];
        t.srThreadGrp = op0[15];
        t.srFIFOMask = op0[23:16];
        t.srExeMode = round_mode'(op0[26:24]);
      end
      SR_UEE    :
        t.srUEE = op0;
      SR_UER    :
        t.srUEE = op0;
      endcase
    end
    op_s2gp:
    begin
      case(sr)
      SR_PROC_CTL :
      begin
        foreach(thread[i])
          res[i] = thread[i].threadState != ts_disabled;
        res[19:16] = srPBId;
        res[23:20] = tid;
        res[25] = srDisableTimer;
        res[26] = srReducePower;
        res[27] = srTimerMask;
        res[29:28] = srPerfCntMask;
        res[29] = srSupMsgMask;
      end
      SR_EBASE    :
        res = srExpBase;
      SR_THD_CTL  :
      begin
        res[0] = t.privMode;
        res[2] = t.srThreadGrp;
        res[23:16] = t.srFIFOMask;
        res[26:24] = t.srExeMode;
      end
      SR_THD_ST   :
      begin
        res[4:0] = t.srUserEvent;
        res[8:5] = t.srCause;
        res[9] = srSupMsgPend;
        res[17:10] = t.srFIFOPend;
        res[19:18] = srPerfCntPend;        
        res[20] = srTimerPend;        
      end
      endcase
    end
    endcase
    return res;
  endfunction : exe_ise

  function bit can_issue(input uchar tid);
    /// the vec value indicate 4 cyc issue style is needed
    ise_thread_inf t = thread[tid];
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [NUM_FU-1:0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = t.enFu[i];
        
      ovm_report_info("can_issue",
        $psprintf("threadState:%s, decoded:%0d, Err:%0d, wCnt:%0d, pc:%0h spu:%0b, dse:%0b, fu:%b. dv:%0b, wCntSel:%0b", 
                   t.threadState.name, t.decoded, t.decodeErr, t.wCnt[t.wCntSel], t.pc, t.enSPU, t.enDSE,
                   enFuTmp, t.dseVec, t.wCntSel),
        OVM_HIGH);
    end
    
    if(t.enVec && cntVecProc >= t.vecMode)
      return 0;
      
    ///issue disable check
    if(t.iDSE.dse_block(noLd, noSt, noSMsg, noRMsg))
      return 0;
    
    foreach(noFu[i])
      if(noFu[i] && t.enFu[i])
        return 0;
    
    ///read cyc check
    if(cntSrfRd > 0 && t.cntSrfRd > 0)
      return 0;

    if(cntVrfRd > 0 && t.cntVrfRd > 0)
      return 0;

    if(cntDSERd > 0 && t.cntDSERd > 0)
      return 0;

    /// write buf overflow check
    if(cntPRWr + t.cntPRWr > CYC_VEC)
      return 0;
    
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] + t.cntVrfWr[i] > CYC_VEC)
        return 0;      

    foreach(cntSrfWr[i])
      if(cntSrfWr[i] + t.cntSrfWr[i] > CYC_VEC)
        return 0;
    
    if(!t.lpRndMemMode && t.pendMemAcc > 0)
      return 0;
      
    return t.decoded && (t.threadState inside {ts_rdy, ts_w_b, ts_b_self} && t.wCnt[t.wCntSel] == 0);
  endfunction : can_issue

  function void fill_issue(input uchar tid);
    ise_thread_inf t = thread[tid];
    if(ciRFM[0] == null) ciRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    if(ciRFM[cntVrfRd] == null) ciRFM[cntVrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    ciRFM[0].start = 1;
    ciRFM[cntVrfRd].vecEnd = 1;

    foreach(t.iFu[i])
      t.iFu[i].map_wr_grp(t.vrfMap, t.srfMap);

    /// spu or scalar dse issue
    if(t.enSPU) begin
      bit brDepDSE = 0, brDepSPA = 0;
      t.iSPU.map_wr_grp(t.vrfMap, t.srfMap);
      if(ciRFM[0] == null) ciRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciRFM[cntSrfRd] == null) ciRFM[cntSrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciSPU[cntSrfRd] == null) ciSPU[cntSrfRd] = tr_ise2spu::type_id::create("toSPU", get_parent());
      ciRFM[0].start = 1;
      ciRFM[cntSrfRd].sclEnd = 1;
      ciSPU[cntSrfRd].start = 1;
      if(t.iSPU.is_br() && !t.iSPU.is_unc_br()) begin ///a conditional branch need vecMode cycles
        cntSrfRd = t.vecMode;
        ///find what br is depend on
        foreach(t.enFu[fid])
          if(t.enFu[fid] && t.iFu[fid].op inside {op_cmp, op_ucmp})
            brDepSPA = 1;
        if(t.enDSE)
          brDepDSE = !brDepSPA;
      end
      ciSPU[cntSrfRd].brEnd = t.iSPU.is_br();
      for(int i = 0; i <= cntSrfRd; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(ciSPA[i] == null) ciSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
        t.iSPU.fill_rfm(ciRFM[i], i);
        t.iSPU.fill_spu(ciSPU[i]);
        ciSPU[i].brDepSPA = brDepSPA;
        ciSPU[i].brDepDSE = brDepDSE;
        ciSPU[i].tid = srPBId;
        ciSPU[i].rstCnt = rstCnt;
      end
    end
    
    if(t.enDSE) begin
      t.iDSE.map_wr_grp(t.vrfMap, t.srfMap);
      for(int i = 0; i <= cntDSERd; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
        if(ciDSE[i] == null) ciDSE[i] = tr_ise2dse::type_id::create("toDSE", get_parent());
        t.iDSE.fill_dse(ciDSE[i]);
        t.iDSE.fill_rfm(ciRFM[i], i);
        t.iDSE.fill_spu(ciSPU[i]);
        ciDSE[i].subVec = i;
        ciDSE[i].vecMode = t.vecMode;
        ciDSE[i].nonBlock = t.lpRndMemMode;
        ciDSE[i].tid = tid;
        ciDSE[i].pbId = srPBId;
        ciDSE[i].rstCnt = rstCnt;
      end
    end
          
    for(int i = 0; i <= cntSrfRd || i <= cntVrfRd; i++) begin
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      ciRFM[i].bpCo = t.co;
      ciRFM[i].vrfRdGrp = t.vrfGrp[i];
      ciRFM[i].vrfRdAdr = t.vrfAdr[i];
      ciRFM[i].srfRdGrp = t.srfGrp[i];
      ciRFM[i].srfRdAdr = t.srfAdr[i];
    end
    
    for(int i = 0; i <= cntVrfRd; i++) begin
      if(ciSPA[i] == null) ciSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
      if(t.enDSE)
        t.iDSE.fill_spa(ciSPA[i]);
      foreach(t.iFu[fid])
        if(t.enFu[fid]) begin
          t.iFu[fid].fill_rfm(ciRFM[i], i);
          t.iFu[fid].fill_spa(ciSPA[i]);
          t.iFu[fid].fill_spu(ciSPU[i]);
        end
      ciSPA[i].vecMode = t.vecMode;
      ciSPU[i].vecMode = t.vecMode;
      ciSPA[i].tid = tid;
      ciSPA[i].rstCnt = rstCnt;
      ciRFM[i].tid = tid;
      ciRFM[i].cyc = i;
      ciSPA[i].subVec = i;
      ciSPU[i].subVec = i;
      ciSPA[i].rndMode = t.srExeMode;
    end
  endfunction : fill_issue

  function void issue(input uchar tid, pbId);
    ise_thread_inf t = thread[tid];
    vn.pcRst[rstCnt] = t.pc;
    vn.igSizeRst[rstCnt] = t.iGrpBytes;
    vn.privRst[rstCnt] = t.privMode;
///    vn.pdLdRst[rstCnt] t.pendLoad;
///    vn.pdStRst[rstCnt] t.pendStore;
///    vn.pdMemAccRst[rstCnt] t.pendMemAcc;
    
    if(t.decodeErr) begin
      if(t.threadState == ts_rdy)
        enter_exp(tid, exp_decode_err);
      return;
    end
    else if(t.IFetchExp) begin
      if(t.threadState == ts_rdy)
        enter_exp(tid, exp_ife_err);
      return;
    end
    
    if(t.enSPU) begin
      if(t.iSPU.is_unc_br()) begin
        t.pc = t.pc + t.iSPU.offSet;
        t.brPred = 1;
      end
      else if(t.iSPU.is_br())
        t.br_pred();
      
    /// spu or scalar dse issue
      if(t.iSPU.is_priv()) begin
        if(t.privMode)
          void'(exe_ise(tid, t.iSPU.op));
        else
          enter_exp(tid, exp_priv_err);
      end
      else if(t.iSPU.is_ise_inst())
        void'(exe_ise(tid, t.iSPU.op));
    end

    ///branch taken
    if(t.enSPU && t.iSPU.is_br() && t.brPred) begin
      ///jmp to current?
      if(t.iSPU.offSet == 0)
        t.decoded = 1;
      else begin
        t.flush();
        t.decoded = 0;
      end
    end
    ///not branch or branch not taken
    else begin
      t.pc += t.iGrpBytes;
      t.decoded = 0;
      t.iBuf = t.iBuf[t.iGrpBytes:$];
      t.decodeErr = 0;
    end
            
    if(t.enDSE) begin
      cntDSERd = t.cntDSERd;
      t.pendMemAcc++;
      if(t.iDSE.op inside {op_lw, op_lh, op_lhu, op_lb, op_lbu})
        t.pendLoad = 1;
      else if(t.iDSE.op inside {op_sw, op_sh, op_sb})
        t.pendStore = 1;
      t.lpRndMemMode =  (t.iDSE.mT == 1 && t.enSPU && t.iSPU.is_br()
        && t.threadState == ts_b_self && t.iSPU.prWrAdr[0] == t.iDSE.prRdAdr
        && t.iDSE.prRdAdr != 0 && t.brPred);
    end

    if(t.wCntNext > t.wCnt[t.wCntSel])
      t.wCnt[t.wCntSel] = t.wCntNext;
          
    cntSrfRd = t.cntSrfRd;
    cntVrfRd = t.cntVrfRd;
    cntDSERd = t.cntDSERd;
    cntPRWr += t.cntPRWr;
  
    foreach(cntVrfWr[i])
      cntVrfWr[i] += t.cntVrfWr[i];

    foreach(cntSrfWr[i])
      cntSrfWr[i] += t.cntSrfWr[i];
    
    if(t.enVec)
      cntVecProc = t.vecMode;

    ///rdy to issue the ig 
    fill_issue(tid); 
  endfunction : issue
      
  function void comb_proc();
    ovm_report_info("ise", "comb_proc procing...", OVM_FULL); 
    
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmRFM != null) end_tr(v.fmRFM);
    if(v.fmIFE != null) end_tr(v.fmIFE);
    if(v.fmDSE[STAGE_ISE_DEM0] != null) end_tr(v.fmDSE[STAGE_ISE_DEM0]);
    
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmRFM = null;
    vn.fmIFE = null;
    vn.fmDSE[STAGE_ISE_DEM0] = null;
    toEIF = null;
    
    for(int i = STAGE_ISE; i > 1; i--) begin
      vn.rfm[i] = v.rfm[i - 1];  
      vn.spa[i] = v.spa[i - 1];
      vn.spu[i] = v.spu[i - 1];
      vn.dse[i] = v.dse[i - 1];
    end
    vn.rfm[1] = null; 
    vn.spa[1] = null;
    vn.spu[1] = null;
    vn.dse[1] = null;
    
///    for(int i = STAGE_ISE_VWB; i > 1; i--) begin
///      vn.pcRst[i] = v.pcRst[i - 1];
///      vn.igSizeRst[i] = v.igSizeRst[i - 1];
///      vn.privRst[i] = v.privRst[i - 1];
///    end
///    vn.pcRst[1] = v.pcRst[1];
///    vn.igSizeRst[1] = v.igSizeRst[1];
///    vn.privRst[1] = v.privRst[1];
    
    if(rstCnt >= STAGE_ISE_VWB) rstCnt = 1;
    else rstCnt++;
    
    foreach(thread[i])
      thread[i].cyc_new();
    
    vn.cancel = '{default : 0};
    
    for(int i = 0; i < (CYC_VEC - 1); i++) begin
      ciRFM[i] = ciRFM[i + 1];
      ciSPA[i] = ciSPA[i + 1];
      ciSPU[i] = ciSPU[i + 1];
      ciDSE[i] = ciDSE[i + 1];
    end

    ciRFM[CYC_VEC-1] = null;
    ciSPA[CYC_VEC-1] = null;
    ciSPU[CYC_VEC-1] = null;
    ciDSE[CYC_VEC-1] = null;
    
    if(cntVrfRd != 0) cntVrfRd--;
    if(cntDSERd != 0) cntDSERd--;
    if(cntSrfRd != 0) cntSrfRd--;
    if(cntPRWr != 0) cntPRWr--;
    if(cntVecProc != 0) cntVecProc--;
    
    foreach(cntSrfWr[i])
      if(cntSrfWr[i] != 0) cntSrfWr[i]--;
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] != 0) cntVrfWr[i]--;
    
    noFu = '{default: 0};
    if(noLd > 0) noLd--;
    if(noSt > 0) noSt--;
    if(noSMsg > 0) noSMsg--;
    if(noRMsg > 0) noRMsg--;

    for(int i = STAGE_ISE_VWBP; i > STAGE_ISE_DEM0; i--)
      vn.fmDSE[i] = v.fmDSE[i-1];  
    
    ///SR Requests
    if(v.fmSPU != null && v.fmSPU.srReq) begin
      uchar stage = STAGE_RRF_SR0 - STAGE_RRF_EXS2 + 1;
      if(ciSPU[stage] != null) ciSPU[stage] = tr_ise2spu::type_id::create("toSPU", this);
      ciSPU[stage].srRes = exe_ise(v.fmSPU.tid, v.fmSPU.op, v.fmSPU.op0, v.fmSPU.srAdr);
    end
    
    ///cancel condition 1 branch mispredication, msc exp
    if(v.fmSPU != null && v.fmSPU.brRsp) begin
      tr_spu2ise spu = v.fmSPU;
      resolve_br(spu.tid, spu.brTaken, spu.rstCnt);
      if(spu.mscExp)
        enter_exp(spu.tid, exp_msc_err, spu.rstCnt);
    end
    
    ///cancel condition 2, spa exp
    if(v.fmSPA != null && v.fmSPA.exp)
      enter_exp(v.fmSPA.tid, exp_fu_err, v.fmSPA.rstCnt);
    
    ///cancel condition 3 dse exp or cache miss
    if(v.fmDSE[STAGE_ISE_DEM0] != null) begin
      tr_dse2ise dse = v.fmDSE[STAGE_ISE_DEM0];
      ise_thread_inf t = thread[dse.tid];
      if(dse.cancel)
        dse_wait(dse.tid);
      else begin
        if(t.pendMemAcc > 0)
          t.pendMemAcc--;
        if(t.pendMemAcc == 0) begin
          t.pendLoad = 0;
          t.pendStore = 0;
        end
      end
      if(dse.exp) begin
        t.pendStore = dse.pendStore;
        t.pendLoad = dse.pendLoad;
        t.pendMemAcc = dse.pendMemAcc;
        enter_exp(dse.tid, exp_dse_err, dse.rstCnt, dse.cause);
      end
    end
    
    ///update no_* for issue & check
    if(v.fmSPA != null)
      noFu = v.fmSPA.noFu;
      
    if(v.pendEIF != null || v.fmEIF != null) begin
      tr_eif2ise eif;
      if(v.pendEIF != null) begin
        eif = v.pendEIF;
        vn.pendEIF = null;
      end
      else
        eif = v.fmEIF;
      if(ciDSE[0] == null || !ciDSE[0].en) begin
        noLd += eif.noLd;
        noSt += eif.noSt;
        noSMsg += eif.noSMsg;
        noRMsg += eif.noRMsg;
        vn.pendEIF = null;
        if(toEIF == null) toEIF = tr_ise2eif::type_id::create("toEIF", this);
          toEIF.rsp = 1;
      end
      else
        vn.pendEIF = eif;
    end
    
    ///check & issue, cancel condition 3, ise decode Err, priv enter, uncond branch
    ovm_report_info("iinf", $psprintf("\n%s", sprint(printer)), OVM_HIGH);
    for(int i = 1; i <= NUM_THREAD; i++) begin
      uchar tid = i + v.TIdIssueLast;
      tid = tid & `GML(WID_TID);
      
      ovm_report_info("issue", $psprintf("checking thread %0d", tid), OVM_HIGH);
      if(can_issue(tid)) begin
        ovm_report_info("issue", $psprintf("issuing thread %0d", tid), OVM_HIGH);
        issue(tid, srPBId);
        vn.TIdIssueLast = tid;
        break;
      end
    end
    
    ///cancel from one cycle delayed
    if(v.fmIFE != null && v.cancel[v.fmIFE.tid])
      v.fmIFE = null;
      
    ///update ife data into thread
    if(v.fmIFE != null && v.fmIFE.instEn)
      thread[v.fmIFE.tid].update_inst(v.fmIFE.fetchGrp);
    
    ///try to decode one inst grp
    foreach(thread[i])
      if(thread[i].threadState != ts_disabled && thread[i].iBuf.size() > 1 && !thread[i].decoded) begin
        thread[i].decode_igrp_start();
        if(thread[i].iBuf.size() >= thread[i].iGrpBytes) begin
          thread[i].decode_igrp();
          break;
        end
        else if(thread[i].pendIFetchExp) begin
          thread[i].IFetchExp = 1;
          thread[i].decoded = 1;
        end
      end
  endfunction
  
  function void req_proc();
    tr_ise2rfm toRFM;
    tr_ise2spu toSPU;
    tr_ise2spa toSPA;
    tr_ise2ife toIFE;
    tr_ise2dse toDSE;
    
    ovm_report_info("ise", "req_proc procing...", OVM_FULL); 
    
    vn.rfm[1] = ciRFM[0];
    vn.spa[1] = ciSPA[0];
    vn.spu[1] = ciSPU[0];
    vn.dse[1] = ciDSE[0];  
        
    toRFM = v.rfm[STAGE_ISE];
    toSPA = v.spa[STAGE_ISE];
    toSPU = v.spu[STAGE_ISE];
    toDSE = v.dse[STAGE_ISE];
    
    ///ife req search
    for(int i = 1; i <= NUM_THREAD; i++) begin
      uchar tid = i + v.TIdFetchLast;
      tid = tid & `GML(WID_TID);
      if(thread[tid].can_req_ifetch()) begin
        toIFE = tr_ise2ife::type_id::create("toIFE", this);
        thread[tid].fill_ife(toIFE);
        toIFE.tid = tid;
        vn.TIdFetchLast = tid;
        break;
      end
    end
    
    ///delay cancel one cycle
    foreach(thread[i])
      if(thread[i].cancel) begin
        if(toIFE == null) toIFE = tr_ise2ife::type_id::create("toIFE", this);
        toIFE.cancel[i] = 1;
        vn.cancel[i] = 1;
      end
    
    ///send dse cancel to spa
    if(v.fmDSE[STAGE_ISE_VWBP] != null && v.fmDSE[STAGE_ISE_VWBP].cancel) begin
      tr_dse2ise dse = v.fmDSE[STAGE_ISE_VWBP];
      if(toSPA == null) toSPA = tr_ise2spa::type_id::create("toSPA", this);
      toSPA.cancel[dse.tid] = 1;
    end
      
    ///------------req to other module----------------
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toIFE != null) void'(ife_tr_port.nb_transport(toIFE, toIFE));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toEIF != null) void'(eif_tr_port.nb_transport(toEIF, toEIF));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ife(input tr_ife2ise req, output tr_ife2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get ife Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    end_tr(req);
    rsp = req;
    vn.fmIFE = req;
    return 1;
  endfunction : nb_transport_ife

  function bit nb_transport_spu(input tr_spu2ise req, output tr_spu2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2ise req, output tr_spa2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa
  
  function bit nb_transport_rfm(input tr_rfm2ise req, output tr_rfm2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_dse(input tr_dse2ise req, output tr_dse2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE[STAGE_ISE_DEM0] = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_eif(input tr_eif2ise req, output tr_eif2ise rsp);
    ovm_report_info("ise_tr", $psprintf("Get eif Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmEIF = req;
    return 1;
  endfunction : nb_transport_eif
      
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time==stamp) begin
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
    ife_tr_imp = new("ife_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    rfm_tr_imp = new("rfm_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    eif_tr_imp = new("eif_tr_imp", this);
    
    ife_tr_port = new("ife_tr_port", this);
    rfm_tr_port = new("rfm_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    eif_tr_port = new("eif_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    
    foreach(thread[i])
      thread[i] = new($psprintf("thread%0d", i), this);
    thread[0].threadState = ts_rdy;
    thread[0].privMode = 1;
    
    cntVrfRd = 0;
    cntSrfRd = 0;
    cntDSERd = 0;
    cntPRWr = 0;
    cntSrfWr = '{default: 0};
    cntVrfWr = '{default: 0};
    srExpBase = CFG_START_ADR;
    srPBId = 0;
    
    printer = new();
    printer.knobs.depth = 1;
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
