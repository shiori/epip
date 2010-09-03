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
  exp_decode_err,   exp_priv_err,   exp_msc_err,
  exp_ife_err,      exp_vfu_err,    exp_scl_err
}ise_exp_t;

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fmSPU;
  tr_rfm2ise fmRFM;
  tr_ife2ise fmIFE;
  tr_spa2ise fmSPA;
  tr_eif2ise fmEIF, pendEIF;
  tr_dse2ise fmDSE;
  
  tr_ise2rfm rfm[STAGE_ISE:1];
  tr_ise2spa spa[STAGE_ISE:1];
  tr_ise2spu spu[STAGE_ISE:1];
  tr_ise2dse dse[STAGE_ISE:1];
  
  uchar TIdIssueLast, TIdFetchLast;
///  bit cancel[NUM_THREAD];
  uint pcRst[STAGE_ISE_VWB_END:1], bpcRst[STAGE_ISE_VWB_END:1], igbRst[STAGE_ISE_VWB_END:1];
  bit privRst[STAGE_ISE_VWB_END:1];
///      pdLdRst[STAGE_ISE_VWB_END:1],
///      pdStRst[STAGE_ISE_VWB_END:1];
  
  `ovm_component_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(pendEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_int(TIdIssueLast, OVM_ALL_ON)
    `ovm_field_int(TIdFetchLast, OVM_ALL_ON)
///    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
    `ovm_field_sarray_int(pcRst, OVM_ALL_ON)
    `ovm_field_sarray_int(bpcRst, OVM_ALL_ON)
    `ovm_field_sarray_int(igbRst, OVM_ALL_ON)
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
  uchar wCnt[NUM_W_CNT][6], wCntNext[7], wCntSel, vecMode;
  bit wCntDep[5];
  
  uchar vrfMap[NUM_INST_VRF / NUM_PRF_P_GRP], 
        srfMap[NUM_INST_SRF / NUM_PRF_P_GRP];
  bit isLastLoad, isLastStore, isLastVecDse, lpRndMemMode, pendIFetchExp, iFetchExp;
  uchar pendIFetch, pendExLoad, pendExStore, pendBr;
  uchar srThreadGrp, srFIFOMask, srUserEvent, srFIFOPend;
  round_mode_t srExeMode;
  bit srKD, srCauseFu;
  cause_spu_t srCauseSPU, pendIFetchCause;
  cause_dse_t srCauseDSE;
  
  inst_c iSPU, iDSE, iFu[NUM_FU];
  uint pc, pcExp, pcEret, pcUEret, srUEE;
  bit brPred;
  bit[NUM_BR_HISTORY - 1 : 0] brHistory;
  uint fcrRet[$];
  
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
///    `ovm_field_sarray_int(wCnt, OVM_ALL_ON)
    `ovm_field_sarray_int(wCntNext, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(pcEret, OVM_ALL_ON)
    `ovm_field_int(pcExp, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_int(isLastLoad, OVM_ALL_ON)
    `ovm_field_int(isLastStore, OVM_ALL_ON)
    `ovm_field_int(isLastVecDse, OVM_ALL_ON)
    `ovm_field_int(lpRndMemMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(pendExStore, OVM_ALL_ON)
    `ovm_field_int(pendExLoad, OVM_ALL_ON)
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
	    `PAF3(wCnt, OVM_DEC)
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
    iFetchExp = 0;
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
    foreach(wCnt[i, j])
      if(wCnt[i][j] != 0) wCnt[i][j]--;
///    if(wExpCnt != 0) wExpCnt--;
  endfunction : cyc_new

  function uint br_pred();
    pendBr++;
    
    ///simple prediction
    brPred = iSPU.offSet == 0;

    if(brPred) begin
      threadState = ts_b_self;
      br_pred = pc + iGrpBytes;
      if(iSPU.op == op_fcr) begin
        ///function call
        if(iSPU.op == op_fcr && brPred) begin
          if(iSPU.rdBkSel[0] == selfu2) begin
            while(fcrRet.size() > NUM_FCR_RET)
              void'(fcrRet.pop_back());
            fcrRet.push_front(pc);
          end
          ///function return
          else if(iSPU.fcRet && fcrRet.size() > 0)
            pc = fcrRet.pop_front();
        end
      end
      else
        pc = pc + iSPU.offSet;
    end
    else begin
      threadState = (threadState == ts_rdy) ? ts_w_b : ts_b_pred;
      br_pred = pc + iSPU.offSet;
      pc = pc + iGrpBytes;
    end
    brHistory = brHistory << 1;
    brHistory[0] = brPred;
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
///    bit noVecExp = 0;
    vrfRdEn = '{default : 0};
    srfRdEn = '{default : 0};
    cntVrfRd = 0;
    cntSrfRd = 0;
    cntDSERd = 0;
    cntVrfWr = '{default : 0};
    cntSrfWr = '{default : 0};
    cntPRWr = 0;
    wCntNext = '{default : 0};
        
    if(!grpStart.t) begin
      tmp = 1;
      offSet = 1;
///      if(adrPkgBytes != 0) adrPkgBytes --;???
      iSPU.set_data(iBuf, offSet, 0, dseVec);
      iDSE.set_data(iBuf, offSet, 0, dseVec);
      foreach(iFu[i])
        iFu[i].set_data(iBuf, offSet, i, 1);
        
      offSet += NUM_INST_BYTES;
      iSPU.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd, cntVrfWr, cntSrfWr, cntPRWr, wCntDep);
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
        iSPU.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd, cntVrfWr, cntSrfWr, cntPRWr, wCntDep);
        offSet += NUM_INST_BYTES;
        iSPU.enSPU = 1;
      end
      
      if(enDSE) begin
        iSPU.enDSE = 1;
        iDSE.set_data(iBuf, offSet, 0, dseVec);
        iDSE.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd, cntVrfWr, cntSrfWr, cntPRWr, wCntDep);
        offSet += NUM_INST_BYTES;
      end
      
      foreach(iFu[i])
        if(enFu[i]) begin
          iFu[i].enFu = 1;
          iFu[i].set_data(iBuf, offSet, i, 1);
          iFu[i].analyze(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd, cntVrfWr, cntSrfWr, cntPRWr, wCntDep);
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
    
///    foreach(iFu[fid])
///      noVecExp |= iFu[fid].noVecExp;
///    noVecExp |= iSPU.noVecExp;
///    noVecExp |= iDSE.noVecExp;
    
    foreach(iFu[fid]) begin
      if(enFu[fid])
        iFu[fid].set_wcnt(wCntNext, vecMode);
      decodeErr |= iFu[fid].decodeErr;
    end
    if(enSPU)
      iSPU.set_wcnt(wCntNext, vecMode);
    if(enDSE)
      iDSE.set_wcnt(wCntNext, vecMode, lpRndMemMode); ///, isLastLoad, isLastStore, isLastVecDse);
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
    iFetchExp = 0;
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
      pendIFetchCause = EC_TLBIFET;
      pendIFetchExp = 1;
      ovm_report_info("update_inst", "EC_TLBIFET", OVM_HIGH);
      return;
    end
    else if(fetchGrp.accErr) begin
      pendIFetchCause = EC_IFACC;
      pendIFetchExp = 1;
      ovm_report_info("update_inst", "EC_IFACC", OVM_HIGH);
      return;
    end
    else if(fetchGrp.k && !privMode) begin
      pendIFetchCause = EC_EXEPRIV;
      pendIFetchExp = 1;
      ovm_report_info("update_inst", "EC_EXEPRIV", OVM_HIGH);
      return;
    end
    else if(!fetchGrp.ex) begin
      pendIFetchCause = EC_NOTEXE;
      pendIFetchExp = 1;
      ovm_report_info("update_inst", "EC_NOTEXE", OVM_HIGH);
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
  local uchar srPBId;
  local uint srExpBase;
  local bit srSupMsgMask, srPerfCntMask, srTimerMask, srReducePower, srDisableTimer,
            srTimerPend,  srSupMsgPend;
  local bit[1:0] srPerfCntPend;
  local tr_ise2eif toEIF;
  local bit[STAGE_ISE_VWBP:0] cancel[NUM_THREAD];
  
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
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_component_utils_end

  function void restorePC(input uchar tid, uchar sel = 0, stage = 0, bit exp = 0, ejtag = 0);
    ise_thread_inf t = thread[tid];
    uint res,
         pc = stage == 0 ? t.pc : v.pcRst[stage],
         npc = stage == 0 ? (t.pc + t.iGrpBytes) : (v.pcRst[stage] + v.igbRst[stage]),
         bpc = stage == 0 ? (t.pc + t.iSPU.offSet) : v.bpcRst[stage];
    case(sel)
    0:  res = pc;
    1:  res = npc;
    2:  res = bpc;
    endcase
    
    t.privMode = stage == 0 ? t.privMode : v.privRst[stage];
    if(exp)
      t.privMode = 1;
    t.pc = res;

    if(exp || (!t.ejtagMode && ejtag)) begin
      t.pcEret = res;
      t.pcExp = pc;
      t.pc = srExpBase;
    end
    
    if(t.ejtagMode || ejtag) begin
      t.pc = VADR_EJTAGS;
      t.ejtagMode = 1;
      if(t.srKD)
        t.privMode = 1;
    end
  endfunction
  
  function void enter_exp(input uchar tid, ise_exp_t expType, uchar vecMode = 0);
    ise_thread_inf t = thread[tid];
    uchar st = 0, sel = 0;
    case(expType)
    exp_decode_err:
      t.srCauseSPU = EC_DECODE;
    exp_priv_err:
      t.srCauseSPU = EC_EXEPRIV;
    exp_msc_err:
    begin
      if(cancel[tid][STAGE_ISE_CEM])
        return;
      sel = 1;
      t.srCauseSPU = EC_MSC;
      st = STAGE_ISE_CEM + t.vecMode;
      cancel[tid] |= `GML(STAGE_ISE_CBR);
    end
    exp_ife_err:
      t.srCauseSPU = t.pendIFetchCause;
    exp_vfu_err:
    begin
      t.srCauseFu = 1;
      sel = 1;
      st = STAGE_ISE_VWB + t.vecMode;
      cancel[tid] |= `GML(STAGE_ISE_VWB);
    end
    exp_scl_err:
    begin
      if(cancel[tid][STAGE_ISE_SWBP])
        return;
      t.srCauseSPU = EC_SCLFU;
      st = STAGE_ISE_EPS;
      cancel[tid] |= `GML(STAGE_ISE_SWB + vecMode);
    end
    endcase
    restorePC(tid, sel, st, 1);
    t.flush();
  endfunction : enter_exp

  function void resolve_br(input uchar tid, bit br);
    ise_thread_inf t = thread[tid];
    if(t.threadState inside {ts_w_b, ts_b_pred, ts_b_self} && !cancel[tid][STAGE_ISE_CBR]) begin
      if(t.pendBr == 0)
        t.threadState = ts_rdy;
      ///miss prediction
      if(br != t.brPred) begin
        t.flush();
        restorePC(tid, 2, STAGE_ISE_CEM + t.vecMode);
        t.lpRndMemMode = 0;
        t.cancel = 1;
        t.brHistory = t.brHistory >> t.pendBr;
        cancel[tid] |= `GML(STAGE_ISE_CBR);
      end
      if(t.pendBr > 0) t.pendBr--;
    end
    else begin
      if(!cancel[tid][STAGE_ISE_CBR])
        ovm_report_warning("resolve_br", "called with wrong threadState!");
      t.pendBr = 0;
    end
  endfunction : resolve_br
    
  function word exe_ise(input uchar tid, opcode_e op, word op0 = 0, uchar sr = 0);
    ise_thread_inf t = thread[tid];
    word res;
    case(op)
    op_exit,
    op_sys: 
    begin
      restorePC(tid, 0, 0, 1);
      t.srCauseSPU = EC_SYSCAL;
      t.flush();     
    end
    op_brk  :
    begin
      restorePC(tid, 0, 0, 0, 1); 
      t.srCauseSPU = EC_BREAK;
      t.flush();     
    end
    op_wait:
    begin
    end
    op_eret:
    begin
      t.pc = t.pcEret;
      t.privMode = 0;
      t.srCauseSPU = EC_NOEXP;
      t.srCauseDSE = EC_NODSE;
      t.srCauseFu = 0;
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
      SR_EPC:
        t.pcExp = op0;
      SR_ERET: 
        t.pcEret = op0;
      SR_PROC_CTL :
      begin
        foreach(thread[i])
          if(thread[i].threadState == ts_disabled && op0[i])
            thread[i].threadState = ts_rdy;
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
        t.srExeMode = round_mode_t'(op0[26:24]);
        t.srKD = op0[27];
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
      SR_EPC:
        res = t.pcExp;
      SR_ERET: 
        res = t.pcEret;
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
        res[27] = t.srKD;
      end
      SR_THD_ST   :
      begin
        res[4:0] = t.srUserEvent;
        res[9:5] = t.srCauseSPU;
        res[13:10] = t.srCauseDSE;
        res[14] = t.srCauseFu;
        res[15] = srSupMsgPend;
        res[23:16] = t.srFIFOPend;
        res[25:24] = srPerfCntPend;        
        res[26] = srTimerPend;        
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
      uchar tmp = 0;
      foreach(enFuTmp[i])
        enFuTmp[i] = t.enFu[i];
      
      foreach(t.wCntDep[i])
        if(t.wCnt[t.wCntSel][i] > tmp && i != br_styp)
          tmp = t.wCnt[t.wCntSel][i];
          
      ovm_report_info("can_issue",
        $psprintf("threadState:%s, decoded:%0d, Err:%0d, wCnt:%0d, brCnt:%0d, pc:%0h spu:%0b, dse:%0b, fu:%b. dv:%0b, wCntSel:%0b", 
                   t.threadState.name, t.decoded, t.decodeErr, tmp, t.wCnt[t.wCntSel][br_styp], t.pc, t.enSPU, t.enDSE,
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
    
    if(!t.lpRndMemMode && t.pendExLoad > 0)
      return 0;
    
    if(!(t.threadState inside {ts_rdy, ts_w_b, ts_b_self}))
      return 0;
    
    foreach(t.wCntDep[i])
      if(t.wCntDep[i] && t.wCnt[t.wCntSel][i] > 0)
        return 0;
    
    if(t.wCntNext[min_styp] != 0 && t.wCntNext[min_styp] > t.wCnt[br_styp])
      return 0;
///    if(t.wCnt[t.wCntSel] != 0  || t.wCntNext[2] < t.wExpCnt)
///      return 0;
      
    return t.decoded;
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
        ciSPU[i].tid = tid;
        ciSPU[i].brPred = t.brPred;
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
      ciRFM[i].tid = tid;
      ciRFM[i].cyc = i;
      ciSPA[i].subVec = i;
      ciSPU[i].subVec = i;
      ciSPA[i].rndMode = t.srExeMode;
    end
  endfunction : fill_issue

  function void issue(input uchar tid, pbId);
    ise_thread_inf t = thread[tid];
    
    vn.privRst[1] = t.privMode;
    
    if(t.decodeErr) begin
      if(t.threadState == ts_rdy)
        enter_exp(tid, exp_decode_err);
      return;
    end
    else if(t.iFetchExp) begin
      if(t.threadState == ts_rdy)
        enter_exp(tid, exp_ife_err);
      return;
    end
    
    vn.pcRst[1] = t.pc;
    vn.igbRst[1] = t.iGrpBytes;
   
    if(t.enSPU) begin
      /// spu or scalar dse issue
      if(t.iSPU.is_priv()) begin
        if(t.privMode)
          void'(exe_ise(tid, t.iSPU.op));
        else
          enter_exp(tid, exp_priv_err);
      end
      else if(t.iSPU.op inside {ise_zw_ops})
        void'(exe_ise(tid, t.iSPU.op));
    end

    if(t.enSPU && !t.iSPU.is_unc_br() && t.iSPU.is_br())
      vn.bpcRst[1] = t.br_pred();
    else
      vn.bpcRst[1] = t.pc + t.iGrpBytes;
      
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
///      t.pendMemAcc++;
      if(t.iDSE.op inside {op_lw, op_lh, op_lhu, op_lb, op_lbu})
        t.isLastLoad = 1;
      else if(t.iDSE.op inside {op_sw, op_sh, op_sb})
        t.isLastStore = 1;
      t.isLastVecDse = t.iDSE.isVec;
      t.lpRndMemMode =  (t.iDSE.mT == 1 && t.enSPU && t.iSPU.is_br()
        && t.threadState == ts_b_self && t.iSPU.prWrAdr[0] == t.iDSE.prRdAdr
        && t.iDSE.prRdAdr != 0 && t.brPred);
    end
    
    foreach(t.wCntNext[i])
      if(t.wCntNext[i] > t.wCnt[t.wCntSel][i])
        t.wCnt[t.wCntSel][i] = t.wCntNext[i];
///    if(t.wCntNext[1] > t.wExpCnt)
///      t.wExpCnt = t.wCntNext[1];
                
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
    if(v.fmDSE != null) end_tr(v.fmDSE);
    
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmRFM = null;
    vn.fmIFE = null;
    vn.fmDSE = null;
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
    
    for(int i = STAGE_ISE_VWB_END; i > 1; i--) begin
      vn.pcRst[i] = v.pcRst[i - 1];
      vn.bpcRst[i] = v.bpcRst[i - 1];
      vn.privRst[i] = v.privRst[i - 1];
      vn.igbRst[i] = v.igbRst[i - 1];
    end
    vn.pcRst[1] = v.pcRst[1];
    vn.bpcRst[1] = v.bpcRst[1];
    vn.privRst[1] = v.privRst[1];
    vn.igbRst[1] = v.igbRst[1];
    
    foreach(thread[i]) begin
      cancel[i] = cancel[i] << 1;
      cancel[i][0] = thread[i].cancel;
      thread[i].cyc_new();
    end
    
///    vn.cancel = '{default : 0};
    
    for(int i = 0; i < (CYC_VEC - 1); i++) begin
      ciRFM[i] = ciRFM[i + 1];
      ciSPA[i] = ciSPA[i + 1];
      ciSPU[i] = ciSPU[i + 1];
      ciDSE[i] = ciDSE[i + 1];
    end

    ciRFM[CYC_VEC - 1] = null;
    ciSPA[CYC_VEC - 1] = null;
    ciSPU[CYC_VEC - 1] = null;
    ciDSE[CYC_VEC - 1] = null;
    
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
    
///    for(int i = STAGE_ISE_WSR; i > STAGE_ISE_RSR; i--) 
///      vn.fmSPU[i] = v.fmSPU[i - 1];
///    vn.fmSPU[STAGE_ISE_RSR] = null;
    
    ///SR Requests
    if(v.fmSPU != null && v.fmSPU.srReq) begin
      tr_spu2ise spu = v.fmSPU;
      if(spu != null && spu.s2gp) begin
        if(ciSPU[0] != null) ciSPU[0] = tr_ise2spu::type_id::create("toSPU", this);
        ciSPU[0].srRes = exe_ise(spu.tid, spu.op, spu.op0, spu.srAdr);
      end
      if(spu.op != op_s2gp)
        void'(exe_ise(spu.tid, spu.op, spu.op0, spu.srAdr));
    end
    
    ///cancel condition 1 branch mispredication, msc exp
    if(v.fmSPU != null && v.fmSPU.brRsp) begin
      tr_spu2ise spu = v.fmSPU;
      resolve_br(spu.tid, spu.brTaken);
      if(spu.sclExp)
        enter_exp(spu.tidSclExp, exp_scl_err, spu.vecModeSclExp);
      if(spu.mscExp)
        enter_exp(spu.tid, exp_msc_err, spu.vecMode);
    end
    
    ///cancel condition 2, spa exp
    if(v.fmSPA != null && v.fmSPA.exp)
      enter_exp(v.fmSPA.tid, exp_vfu_err);
    
    ///cancel condition 3 dse exp or cache miss
    if(v.fmDSE != null && v.fmDSE.rsp) begin
      tr_dse2ise dse = v.fmDSE;
      uchar st = STAGE_ISE_DEM;
      ise_thread_inf t = thread[dse.tid];
      t.pendExLoad = dse.pendExLoad;
      t.pendExStore = dse.pendExStore;
      if(dse.scl) st += t.vecMode;
      if(dse.exp && !cancel[dse.tid][STAGE_ISE_DEM]) begin
        t.srCauseDSE = dse.cause;
        t.cancel = 1;  
        restorePC(dse.tid, 1,st, 1);
        t.flush();
        cancel[dse.tid] |= `GML(STAGE_ISE_DEM + dse.vecMode);
      end
      else if(dse.ext && !cancel[dse.tid][STAGE_ISE_DEM]) begin
        t.cancel = 1;   
        restorePC(dse.tid, 1,st);
        t.flush();
        cancel[dse.tid] |= `GML(STAGE_ISE_DEM);
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
///    if(v.fmIFE != null && v.cancel[v.fmIFE.tid]) begin
///      v.fmIFE = null;
///      ovm_report_info("cancel", "cancel IFE", OVM_HIGH);
///    end
    
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
          thread[i].iFetchExp = 1;
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
    
    foreach(ciDSE[i]) begin
      if(ciDSE[i] != null && cancel[ciDSE[i].tid][0])
        ciDSE[i] = null;
      if(ciRFM[i] != null && cancel[ciRFM[i].tid][0])
        ciRFM[i] = null;
      if(ciSPU[i] != null && cancel[ciSPU[i].tid][0])
        ciSPU[i] = null;
      if(ciSPA[i] != null && cancel[ciSPA[i].tid][0])
        ciSPA[i] = null;
    end

    foreach(v.rfm[i]) begin
      if(vn.dse[i] != null && cancel[vn.dse[i].tid][0])
        vn.dse[i] = null;
      if(vn.rfm[i] != null && cancel[vn.rfm[i].tid][0])
        vn.rfm[i] = null;
      if(vn.spu[i] != null && cancel[vn.spu[i].tid][0])
        vn.spu[i] = null;
      if(vn.spa[i] != null && cancel[vn.spa[i].tid][0])
        vn.spa[i] = null;
    end
            
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
      end
    
    ///send dse cancel to spa
///    if(v.fmDSE[STAGE_ISE_VWBP] != null && v.fmDSE[STAGE_ISE_VWBP].ext) begin
///      tr_dse2ise dse = v.fmDSE[STAGE_ISE_VWBP];
///      if(toSPA == null) toSPA = tr_ise2spa::type_id::create("toSPA", this);
///      toSPA.cancel[dse.tid] = 1;
///    end
      
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
    if(cancel[req.tid][0])
      ovm_report_info("ise_tr", $psprintf("canceling tid:%0d", req.tid), OVM_HIGH);
    else
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
    vn.fmDSE = req;
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
    
    printer = new();
    printer.knobs.depth = 1;
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
