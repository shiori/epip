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

class ip4_tlm_ise_rst extends ovm_object;
  uint pc, bpc, igb, pcUEret;
  priv_mode_t priv;
  uchar tid, sel;
  bit en, roll, exp, ejtag, brSrf, il;

  `ovm_object_utils_begin(ip4_tlm_ise_rst)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(il, OVM_ALL_ON)
    `ovm_field_int(bpc, OVM_ALL_ON)
    `ovm_field_int(brSrf, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_int(pcUEret, OVM_ALL_ON)
    `ovm_field_int(igb, OVM_ALL_ON)
    `ovm_field_int(roll, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_int(exp, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_int(en, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_int(sel, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_int(ejtag, OVM_ALL_ON + OVM_NOCOPY)
    `ovm_field_enum(priv_mode_t, priv, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function void do_copy (ovm_object rhs);
    ip4_tlm_ise_rst tmp;

    super.do_copy(rhs);
    $cast(tmp,rhs);
    roll = 0;
    en = 0;
    sel = 0;
    exp = 0;
    brSrf = 0;
  endfunction
  
  function new (string name = "rst_vars");
    super.new(name);
  endfunction : new  
endclass : ip4_tlm_ise_rst

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fmSPU;
  tr_rfm2ise fmRFM;
  tr_ife2ise fmIFE;
  tr_spa2ise fmSPA;
  tr_eif2ise fmEIF;
  tr_dse2ise fmDSE;
  
  tr_ise2rfm rfm[STAGE_ISE:1];
  tr_ise2spa spa[STAGE_ISE:1];
  tr_ise2spu spu[STAGE_ISE:1];
  tr_ise2dse dse[STAGE_ISE:1];
  
  ip4_tlm_ise_rst rst[STAGE_EEX_VWB:1];
  
  uchar TIdIssueSel, TIdFetchSel, TIdDecodeSel;
///  bit cancel[NUM_THREAD];
  
  `ovm_component_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
///    `ovm_field_object(pendEIF, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_int(TIdIssueSel, OVM_ALL_ON)
    `ovm_field_int(TIdFetchSel, OVM_ALL_ON)
    `ovm_field_int(TIdDecodeSel, OVM_ALL_ON)
    `ovm_field_sarray_object(rst, OVM_ALL_ON + OVM_REFERENCE)
///    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_component_utils_end

  virtual function void build();
    TIdFetchSel = 0;
    TIdIssueSel = 0;
    TIdDecodeSel = 0;
    print_enabled = 0;
    super.build();
    foreach(rst[i])
      rst[i] = new();
  endfunction
  
  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_ise_vars

class ise_thread_inf extends ovm_component;
  thread_state_t threadState;
  uchar iBuf[$];
  uchar iGrpBytes, adrPkgBytes, numConst,
        cntSrfBusy, cntVrfBusy, cntDSEBusy, cntFuBusy, cntSPUBusy;
  wordu co[NUM_BP_CO];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS], vrfGrp[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_SRF_BKS], srfGrp[CYC_VEC][NUM_SRF_BKS];
  bit vrfRdEn[CYC_VEC][NUM_VRF_BKS], srfRdEn[CYC_VEC][NUM_SRF_BKS];
  uchar cntPRWr, cntVrfWr[NUM_VRF_BKS], cntSrfWr[NUM_SRF_BKS];
  
  bit enSPU, enDSE, enVec, enFu[NUM_FU];
  priv_mode_t privMode, privEret;  ///privilege running status
  bit ejtagMode,
      decoded,
      decodeErr,
      cancel,
      noMsk,
      waitLng,
      waitPip;
  uchar iwCnt, iCnt, ilCnt, wCntBr, wCntBrNext, wCntWr, vecMode;
  bit wCntBrDep;
///  uchar wCnt[NUM_W_CNT][5], wCntNext[7];
///  bit[1:0] wCntDep;
///  bit[NUM_W_CNT - 1 : 0] wCntSel;
///  bit wCntUp;
  bit[CYC_VEC - 1 : 0] noExt;
  uchar srLSID, srLRID;
  bit[NUM_FIFO - 1 : 0] srFFST, srFFRT, srREE, srLRRID, msgRdy;

  uchar vrfMap[NUM_INST_VRF / NUM_PRF_P_GRP], 
        srfMap[NUM_INST_SRF / NUM_PRF_P_GRP];
  bit isLastLoad, isLastStore, isLastVecDse, lpRndMemMode, pendIFetchExp, iFetchExp;
  uchar pendIFetch, pendExLoad, pendExStore, pendSMsg, pendBr;
  uchar srThreadGrp;
  round_mode_t srRndMode;
  uchar srExpMsk;
  bit srKD, srCauseFu, mrfLocked;
  cause_spu_t srCauseSPU, pendIFetchCause;
  cause_dse_t srCauseDSE;
  
  inst_c iSPU, iDSE, iFu[NUM_FU];
  uint pc, pcExp, pcEret, pcUEret, srUEE;
  bit brPred;
  bit[NUM_BR_HISTORY - 1 : 0] brHistory;
  uint fcrRet[$];
  
  bit useOffset;
  
  `ovm_component_utils_begin(ise_thread_inf)
    `ovm_field_enum(thread_state_t, threadState, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decodeErr, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
    `ovm_field_int(waitLng, OVM_ALL_ON)
    `ovm_field_int(waitPip, OVM_ALL_ON)
///    `ovm_field_int(wCntSel, OVM_ALL_ON + OVM_BIN)
///    `ovm_field_int(wCntUp, OVM_ALL_ON + OVM_BIN)
///    `ovm_field_int(wCntDep, OVM_ALL_ON + OVM_BIN)
    `ovm_field_int(iwCnt, OVM_ALL_ON)
    `ovm_field_int(iCnt, OVM_ALL_ON)
    `ovm_field_int(ilCnt, OVM_ALL_ON)
    `ovm_field_int(wCntWr, OVM_ALL_ON)
    `ovm_field_int(wCntBr, OVM_ALL_ON)
    `ovm_field_int(wCntBrDep, OVM_ALL_ON)
    `ovm_field_int(wCntBrNext, OVM_ALL_ON)
///    `ovm_field_sarray_int(wCntNext, OVM_ALL_ON + OVM_UNSIGNED)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_sarray_int(enFu, OVM_ALL_ON)
    `ovm_field_int(enVec, OVM_ALL_ON)
    `ovm_field_enum(priv_mode_t, privMode, OVM_ALL_ON)
    `ovm_field_enum(priv_mode_t, privEret, OVM_ALL_ON)
    `ovm_field_int(ejtagMode, OVM_ALL_ON)
    `ovm_field_queue_int(iBuf, OVM_ALL_ON)
    `ovm_field_int(iGrpBytes, OVM_ALL_ON)
    `ovm_field_int(adrPkgBytes, OVM_ALL_ON)
    `ovm_field_int(numConst, OVM_ALL_ON)
    `ovm_field_sarray_int(co, OVM_ALL_ON)
    `ovm_field_int(cntSrfBusy, OVM_ALL_ON)
    `ovm_field_int(cntVrfBusy, OVM_ALL_ON)
    `ovm_field_int(cntDSEBusy, OVM_ALL_ON)
    `ovm_field_int(cntFuBusy, OVM_ALL_ON)
    `ovm_field_int(cntSPUBusy, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(pcEret, OVM_ALL_ON)
    `ovm_field_int(pcExp, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_int(isLastLoad, OVM_ALL_ON)
    `ovm_field_int(isLastStore, OVM_ALL_ON)
    `ovm_field_int(isLastVecDse, OVM_ALL_ON)
    `ovm_field_int(lpRndMemMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(pendIFetchExp, OVM_ALL_ON)
    `ovm_field_int(pendExStore, OVM_ALL_ON)
    `ovm_field_int(pendExLoad, OVM_ALL_ON)
    `ovm_field_int(pendSMsg, OVM_ALL_ON)
    `ovm_field_int(pendBr, OVM_ALL_ON)
    `ovm_field_int(useOffset, OVM_ALL_ON)
    `ovm_field_sarray_int(vrfMap, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_int(srfMap, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(srExpMsk, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(srKD, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(srThreadGrp, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_object(iSPU, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_object(iDSE, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_object(iFu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
    
  virtual function void build();
    threadState = ts_disabled;
    privMode = priv_user;
    pc = CFG_START_ADR;
    vecMode = CYC_VEC - 1;
    decoded = 0;
    decodeErr = 0;
    pendIFetchExp = 0;
    iFetchExp = 0;
    print_enabled = 0;
    super.build();
    iSPU = new();
    iDSE = new();
    foreach(iFu[i])
      iFu[i] = new();
  endfunction : build

	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
///    if(get_report_verbosity_level() >= OVM_FULL) begin
///   `PAF2(wCnt, OVM_UNSIGNED)
  	if(enSPU)
      printer.print_object("spu", iSPU);
  	if(enDSE)
      printer.print_object("dse", iDSE);
  	foreach(enFu[i])
      if(enFu[i])
       printer.print_object($psprintf("fu%0d", i), iFu[i]);
///    end
    
    if(get_report_verbosity_level() >= OVM_FULL) begin  
      `PAF2(vrfAdr, OVM_UNSIGNED)
      `PAF2(srfAdr, OVM_UNSIGNED)
      `PAF2(vrfGrp, OVM_UNSIGNED)
      `PAF2(srfGrp, OVM_UNSIGNED)
    end
	endfunction : do_print
  
  function void map_iadr(input bit v, uchar orgAdr, output uchar grp, adr);
    uchar adrBits =  v ? (WID_PRF_P_GRP - WID_VRF_BKS) : (WID_PRF_P_GRP - WID_SRF_BKS);
    adr = orgAdr & `GML(adrBits);
    grp = orgAdr >> adrBits;
    grp = v ? vrfMap[grp] : srfMap[grp];
  endfunction : map_iadr

  function void cyc_new();
    cancel = 0;
///    foreach(wCnt[i, j])
///      if(wCnt[i][j] > 0) wCnt[i][j]--;
    if(wCntBr > 0) wCntBr--;
    if(wCntWr > 0) wCntWr--;
  endfunction : cyc_new

  function void br_pred(output uint bpc, bit brSrf);
    bit brSelf = 0, goNext = 0;
    if(!enSPU || !iSPU.is_br()) begin
      ///not br
      goNext = 1;
    end  
    else if(!iSPU.is_unc_br()) begin
      pendBr++;
      
      ///simple prediction
      brPred = iSPU.offSet == 0;
      
      bpc = pc + iSPU.offSet;
      if(brPred) begin
        if(iSPU.op == op_br)
          brSelf = iSPU.offSet == 0;
          
        if(iSPU.op == op_fcr) begin
          ///function call
          if(iSPU.rdBkSel[0] == selfu2) begin
            while(fcrRet.size() > NUM_FCR_RET)
              void'(fcrRet.pop_back());
            fcrRet.push_front(pc + iGrpBytes);
            pc = pc + iSPU.offSet;
            brSelf = iSPU.offSet == 0;
          end
          else if(iSPU.rdBkSel[0] == selz)
            pc = 0;
          else if(iSPU.rdBkSel[0] inside {[selc0:selc7]})
            pc = co[iSPU.rdBkSel[0] - selc0];
          else begin
            brSrf = 1;
            ///function return
            if(iSPU.fcRet && fcrRet.size() > 0)
              pc = fcrRet.pop_front();
          end
        end
          
        lpRndMemMode =  (enDSE && iDSE.mat == at_rand && brSrf
              && iDSE.prWrAdr[0] == iSPU.prRdAdr && iSPU.brDep && iSPU.op == op_br
              && iSPU.mscOp == mop_bc && iSPU.mskOp == sop_p2n && iSPU.imm == 0);
      end
      else
        goNext = 1;
      
      brHistory = brHistory << 1;
      brHistory[0] = brPred;
///      threadState = ts_b_pred;
    end
    else begin
      ///unconditional branch
      brSelf = iSPU.offSet == 0;
      pc = pc + iSPU.offSet;
      bpc = pc;
    end
    
    if(goNext) begin
      pc += iGrpBytes;
      decoded = 0;
      iBuf = iBuf[iGrpBytes:$];
      decodeErr = 0;
    end
    else if(!brSelf) begin
      decoded = 0;
      flush();
    end
  endfunction : br_pred
    
  function void decode_igrp_start();
    i_gs0_t grpStart = iBuf[0];
    uchar icc;
    enSPU = 0;
    enDSE = 0;
    enVec = 0;
    enFu = '{default : 0};

    if(!grpStart.t) begin
///      wCntSel = grpStart.chkGrp;
///      wCntUp = grpStart.chkGrpUp;
      icc = grpStart.icc;
      adrPkgBytes = grpStart.adrPkgB + 1;
      numConst = grpStart.coPkgW;
      noMsk = grpStart.nmsk;
      iGrpBytes = 1 + adrPkgBytes + numConst * WORD_BYTES + NUM_INST_BYTES;
    end
    else begin
      i_gs1_u grpStart;
      uchar tmp = 0;
      foreach(grpStart.b[i])
        grpStart.b[i] = iBuf[i];
                 
      foreach(grpStart.i.unitEn[i])
        tmp += grpStart.i.unitEn[i];
      if(tmp == 0) begin
        ovm_report_warning("decode_igrp_start",
                           $psprintf("igs decode error, unitEn not valid, pc %0h, gs %b, size %0d",
                           pc, grpStart, iBuf.size()));
        decodeErr = 1;
      end
///      wCntSel = grpStart.i.chkGrp;
///      wCntUp = grpStart.i.chkGrpUp;
      icc = grpStart.i.icc;
      adrPkgBytes = grpStart.i.adrPkgB;
      numConst = grpStart.i.coPkgW;
      iGrpBytes = 2 + adrPkgBytes + numConst * WORD_BYTES + tmp * NUM_INST_BYTES;
      enDSE = grpStart.i.unitEn[0];
      enSPU = grpStart.i.unitEn[1];
      noMsk = grpStart.i.nmsk;
      foreach(enFu[i])
        enFu[i] = grpStart.i.unitEn[2+i];
    end
    
    if(icc == 0) begin
      waitPip = 0;
      waitLng = 0;
    end
    else if(icc == 15) begin
      waitLng = 1;
      waitPip = 1;
      iwCnt = 0;
    end
    else if(icc == 14) begin
      waitLng = 0;
      waitPip = 1;
      iwCnt = 0;
    end
    else if(icc < 13) begin
      waitLng = 0;
      waitPip = 1;
      iwCnt = icc;
    end
    else begin
      ovm_report_warning("decode_igrp_start", "icc illegal value");
      waitLng = 1;
      waitPip = 1;
      iwCnt = 0;
    end
    
    if(get_report_verbosity_level() >= OVM_FULL) begin
      bit [NUM_FU - 1 : 0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = enFu[i];
        
      `ip4_info("decode_igrp_start",
        $psprintf("inst grp len %0d bytes includes: spu:%0b, dse:%0b, fu:%b. waitPip:%0b, waitLng:%0d, ilCnt:%0d, adrPkgB:%0d, coPkgW:%0d", 
                   iGrpBytes, enSPU, enDSE, enFuTmp, waitPip, waitLng, ilCnt, adrPkgBytes, numConst),
        OVM_FULL)
    end
  endfunction : decode_igrp_start
    
  function void decode_igrp();
    uchar tmp = 0;
    iga_t[23:0] adrs;
    uchar offSet;
    i_gs0_t grpStart = iBuf[0];
///    bit noVecExp = 0;
    vrfRdEn = '{default : 0};
    srfRdEn = '{default : 0};
    cntVrfBusy = 0;
    cntSrfBusy = 0;
    cntDSEBusy = 0;
    cntFuBusy = 0;
    cntSPUBusy = 0;
    cntVrfWr = '{default : 0};
    cntSrfWr = '{default : 0};
    cntPRWr = 0;
    wCntBrNext = 0;
    wCntBrDep = 0;
///    wCntNext = '{default : 0};
        
    if(!grpStart.t) begin
      tmp = 1;
      offSet = 1;

      iSPU.set_data(iBuf, offSet, 0);
      if(iSPU.op inside {spu_com_ops})
        iSPU.noExp = srExpMsk;
      iDSE.set_data(iBuf, offSet, 0);
      foreach(iFu[i]) begin
        iFu[i].set_data(iBuf, offSet, i);
        iFu[i].noExp = srExpMsk;
      end
      
      iSPU.analyze_fu(enSPU, enDSE, enFu);      
      iDSE.enDSE = enDSE;
      foreach(enFu[i])
        iFu[i].enFu = enFu[i];
      
      iSPU.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfBusy, cntSrfBusy, cntDSEBusy, cntSPUBusy, cntFuBusy, cntVrfWr, cntSrfWr, cntPRWr);
///      gsa = grpStart.a;
      offSet += NUM_INST_BYTES;
    end
    else begin
      i_gs1_u grpStart;
      foreach(grpStart.b[i])
        grpStart.b[i] = iBuf[i];
      offSet = 2;
      tmp = 1;
      
      if(enDSE) begin
        iDSE.set_data(iBuf, offSet, 0);
        iDSE.enDSE = 1;
        iDSE.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfBusy, cntSrfBusy, cntDSEBusy, cntSPUBusy, cntFuBusy, cntVrfWr, cntSrfWr, cntPRWr);
        offSet += NUM_INST_BYTES;
      end
                  
      if(enSPU) begin
        iSPU.set_data(iBuf, offSet, 0);
        iSPU.enSPU = 1;
        iSPU.analyze(vecMode, vrfRdEn, srfRdEn, cntVrfBusy, cntSrfBusy, cntDSEBusy, cntSPUBusy, cntFuBusy, cntVrfWr, cntSrfWr, cntPRWr);
        if(iSPU.op inside {spu_com_ops})
          iSPU.noExp = srExpMsk;
        offSet += NUM_INST_BYTES;
      end
      
      foreach(iFu[i])
        if(enFu[i]) begin
          iFu[i].set_data(iBuf, offSet, i);
          iFu[i].enFu = 1;
          iFu[i].analyze(vecMode, vrfRdEn, srfRdEn, cntVrfBusy, cntSrfBusy, cntDSEBusy, cntSPUBusy, cntFuBusy, cntVrfWr, cntSrfWr, cntPRWr);
          iFu[i].noExp = srExpMsk;
          offSet += NUM_INST_BYTES;          
        end

///      gsa = grpStart.i.a;
    end

    foreach(enFu[i])
      enVec |= enFu[i];
          
    ///fill in rf address
    begin
      bit[8:0][7:0] tmp0;
      for(int i = 0; i < adrPkgBytes; i++) begin
        tmp0[i] = iBuf[offSet];
        offSet++;
      end
///      tmp0[adrPkgBytes][0] = gsa;
      adrs = tmp0;
    end
      
    for(int i = 0; i < numConst; i++) begin
      for(int j = 0; j < WORD_BYTES; j++)
        co[i].b[j] = iBuf[offSet + j];
      offSet += WORD_BYTES;
    end
      
    ///allocate reg read address
    tmp = 0;
    
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfRdEn[i][j]) begin
          map_iadr(1, adrs[tmp], vrfGrp[i][j], vrfAdr[i][j]);
          `ip4_info("assign adr", $psprintf("get vrf adr %0d, cnt %0d, cyc:%0d, bk:%0d",
                    adrs[tmp], tmp, i, j), OVM_FULL)
          tmp++;
        end
        else if(j > 0) begin
           vrfGrp[i][j] =  vrfGrp[i][j - 1];
           vrfAdr[i][j] = vrfAdr[i][j - 1];
        end
          
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfRdEn[i][j]) begin
          map_iadr(0, adrs[tmp], srfGrp[i][j], srfAdr[i][j]);
          `ip4_info("assign adr", $psprintf("get srf adr %0d, cnt %0d, cyc:%0d, bk:%0d",
                    adrs[tmp], tmp, i, j), OVM_FULL)
          tmp++;
        end
        else if(j > 0) begin
           srfGrp[i][j] =  srfGrp[i][j - 1];
           srfAdr[i][j] = srfAdr[i][j - 1];
        end
    end
    
///    foreach(iFu[fid])
///      noVecExp |= iFu[fid].noVecExp;
///    noVecExp |= iSPU.noVecExp;
///    noVecExp |= iDSE.noVecExp;
    
    foreach(iFu[fid]) begin
      if(enFu[fid])
        iFu[fid].set_wcnt(wCntBrNext, wCntBrDep, vecMode);
      decodeErr |= iFu[fid].decodeErr;
    end
    if(enSPU)
      iSPU.set_wcnt(wCntBrNext, wCntBrDep, vecMode);
    if(enDSE)
      iDSE.set_wcnt(wCntBrNext, wCntBrDep, vecMode, lpRndMemMode);
    decodeErr |= iSPU.decodeErr;
    decodeErr |= iDSE.decodeErr;
    
    decoded = 1;
    
    foreach(iFu[i])
      foreach(iFu[0].grpWr[j])
        iFu[i].grpWr[j] = iFu[i].isVec ? vrfMap[iFu[i].grpWr[j]] : srfMap[iFu[i].grpWr[j]];
    
    foreach(iDSE.grpWr[i])
      iDSE.grpWr[i] = iDSE.isVec ? vrfMap[iDSE.grpWr[i]] : srfMap[iDSE.grpWr[i]];

    foreach(iSPU.grpWr[i])
      iSPU.grpWr[i] = srfMap[iSPU.grpWr[i]];
            
    `ip4_info("decode_igrp", {"\n", sprint()}, OVM_FULL)
  endfunction : decode_igrp

  function void flush();
    `ip4_info("flush()", $psprintf("cur pc 0x%0h", pc), OVM_HIGH)
    iBuf = {};
    iGrpBytes = 0;
    decoded = 0;
    decodeErr = 0;
    pendIFetch = 0;
    threadState = ts_rdy;
    cancel = 1;
    pendIFetchExp = 0;
    iFetchExp = 0;
    useOffset = 1;
  endfunction : flush
      
  function void update_inst(input inst_fg_c fetchGrp);
    uchar offSet = 0, LvlLast = iBuf.size();
    if(useOffset) ///only calculate offSet when iBuf size is reset to 0
      offSet = pc & `GML(WID_IFET);
    useOffset = 0;
    
    if((LvlLast + NUM_IFET_BYTES - offSet) > NUM_IBUF_BYTES)
      ovm_report_warning("ise", "iBuf overflow!");
    
    if(pendIFetch > 0)
      pendIFetch--;
    
///    if(cancel) begin
///      `ip4_info("update_inst", $psprintf("cancel, pc:0x%0h, offSet:%0h, pd:%0d", pc, offSet, pendIFetch), OVM_FULL)
///      return;
///    end
    
    if(fetchGrp.exp) begin
      pendIFetchCause = EC_TLBIFET;
      pendIFetchExp = 1;
      `ip4_info("update_inst", "EC_TLBIFET", OVM_FULL)
      return;
    end
    else if(fetchGrp.accErr) begin
      pendIFetchCause = EC_IFACC;
      pendIFetchExp = 1;
      `ip4_info("update_inst", "EC_IFACC", OVM_FULL)
      return;
    end
    else if(fetchGrp.k && privMode != priv_kernel) begin
      pendIFetchCause = EC_EXEPRIV;
      pendIFetchExp = 1;
      `ip4_info("update_inst", "EC_EXEPRIV", OVM_FULL)
      return;
    end
    else if(!fetchGrp.ex) begin
      pendIFetchCause = EC_NOTEXE;
      pendIFetchExp = 1;
      `ip4_info("update_inst", "EC_NOTEXE", OVM_FULL)
      return;
    end
        
    for(int i = offSet; i < NUM_IFET_BYTES; i++)
      iBuf.push_back(fetchGrp.data[i]);
          
    `ip4_info("update_inst", $psprintf("pc 0x%0h, adr 0x%0h, offSet %0h, pd %0d, iBuf lv %0d->%0d",
              pc, fetchGrp.pc, offSet, pendIFetch, LvlLast, iBuf.size()), OVM_HIGH)
  endfunction : update_inst

  function void fill_ife(input tr_ise2ife ife);
    ife.fetchReq = 1;
    ife.pc = (pc + iBuf.size() + NUM_IFET_BYTES * pendIFetch) & `GMH(WID_IFET);
    pendIFetch++;
  endfunction : fill_ife
endclass : ise_thread_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;

  local uchar cntVrfBusy, cntSrfBusy, cntFuBusy, cntDSEBusy, cntSPUBusy, cntSFUBusy,
              cntPRWr, cntSrfWr[NUM_SRF_BKS], cntVrfWr[NUM_VRF_BKS];
        
  local bit noFu[NUM_FU];
  local uchar noLd, noSt, noTMsg, noFMsg;
  
  local tr_ise2rfm ciRFM[CYC_VEC];
  local tr_ise2spa ciSPA[CYC_VEC];
  local tr_ise2spu ciSPU[CYC_VEC];
  local tr_ise2dse ciDSE[CYC_VEC];
  local tr_eif2ise pendEIF[$];
  
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
  local uchar pbId;
  local uint srExpBase;
  local bit srSupMsgMask, srTimerMask, srReducePower, srDisableTimer,
            srTimerPend,  srSupMsgPend;
  local bit[1:0] srPerfCntPend;
  local tr_ise2eif toEIF;
  local bit[STAGE_ISE_VWB_END:0] cancel[NUM_THREAD];
  local bit noSMsg, fifoCleanUp;
  local uint srTimer, srPCnt[2], srCmp;
  local uchar srPCntES[2], tidInt;
  local bit srPCntK[2], srPCntU[2], srPCntIE[2], srPCntGS;
  local uchar ldq, stq, pipSt, pipLd;
  
  `ovm_component_utils_begin(ip4_tlm_ise)
    `ovm_field_int(cntFuBusy, OVM_ALL_ON)
    `ovm_field_int(cntSPUBusy, OVM_ALL_ON)
    `ovm_field_int(cntSFUBusy, OVM_ALL_ON)
    `ovm_field_int(cntDSEBusy, OVM_ALL_ON)
    `ovm_field_int(cntVrfBusy, OVM_ALL_ON)
    `ovm_field_int(cntSrfBusy, OVM_ALL_ON)
    `ovm_field_int(noLd, OVM_ALL_ON)
    `ovm_field_int(noSt, OVM_ALL_ON)
    `ovm_field_int(noTMsg, OVM_ALL_ON)
    `ovm_field_int(noFMsg, OVM_ALL_ON)
    `ovm_field_int(ldq, OVM_ALL_ON)
    `ovm_field_int(stq, OVM_ALL_ON)
    `ovm_field_int(pipSt, OVM_ALL_ON)
    `ovm_field_int(pipLd, OVM_ALL_ON)
    `ovm_field_sarray_int(noFu, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
    `ovm_field_int(pbId, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(srExpBase, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_component_utils_end

  function void restore_pc(input uchar tid, sel = 0, stage = 0, bit exp = 0, ejtag = 0);
    ise_thread_inf t = thread[tid];
    if(stage == 0) begin
      uint res,
           pc = t.pc,
           npc = t.pc + t.iGrpBytes,
           bpc = t.pc + t.iSPU.offSet;
      case(sel)
      0:  res = pc;
      1:  res = npc;
      2:  res = bpc;
      endcase
      `ip4_info("restore_pc", $psprintf("tid %0d, stage0 pc to 0x%0h", tid, res), OVM_MEDIUM)
      if(exp)
        t.privMode = priv_kernel;
      t.pc = res;
  
      if(exp || (!t.ejtagMode && ejtag)) begin
        t.pcEret = res;
        t.pcExp = pc;
        t.pc = srExpBase;
      end
      
      if(exp && ejtag) begin
        t.pc = VADR_EJTAGS;
        t.ejtagMode = 1;
        if(t.srKD)
          t.privMode = priv_kernel;
      end
    end
    else begin
      `ip4_info("restore_pc", $psprintf("tid %0d, log stage %0d, sel %0d", tid, stage, sel), OVM_MEDIUM)      
      v.rst[stage].roll = 1;
      v.rst[stage].exp = exp;
      v.rst[stage].sel = sel;
      t.threadState = ts_w_rst;
      if(v.rst[stage].tid != tid || !v.rst[stage].en) begin
        ovm_report_warning("restore_pc", $psprintf("tid %0d, stage %0d, rst info inconsistent!", tid, stage));
        for(int j = STAGE_ISE_VWB_END; j > 0; j--)
          `ip4_info("rst buf", $psprintf("tid %0d, stage %0d, en %0d, bpc 0x%0h, pc 0x%0h, sel: %0d",
                    v.rst[j].tid, j, v.rst[j].en, v.rst[j].bpc, v.rst[j].pc, v.rst[j].sel), OVM_LOW)
      end
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
      st = STAGE_ISE_CBR + t.vecMode;
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
      if(cancel[tid][STAGE_ISE_VWBP])
        return;
      t.srCauseSPU = EC_SCLFU;
      st = STAGE_ISE_EPS;
      cancel[tid] |= `GML(STAGE_ISE_VWB + vecMode);
    end
    endcase
    t.flush();
    restore_pc(tid, sel, st, 1);
  endfunction : enter_exp

  function void resolve_br(input uchar tid, bit br, miss);
    ise_thread_inf t = thread[tid];
        
    if(!cancel[tid][STAGE_ISE_CBR]) begin
      if(t.pendBr == 0)
        t.threadState = ts_rdy;
      ///miss prediction
      if(miss) begin
        uchar sel = br ? 2 : 1;
        t.flush();
        restore_pc(tid, sel, STAGE_ISE_CBR + t.vecMode);
        t.lpRndMemMode = 0;
        t.brHistory = t.brHistory >> t.pendBr;
        cancel[tid] |= `GML(STAGE_ISE_CBR);
      end
      if(t.pendBr > 0) t.pendBr--;
    end
    else begin
///      if(!cancel[tid][STAGE_ISE_CBR])
///        ovm_report_warning("resolve_br", "called with wrong threadState!");
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
      t.flush();
      restore_pc(tid, 0, 0, 1);
      t.srCauseSPU = EC_SYSCAL;
    end
    op_brk  :
    begin
      t.flush();
      restore_pc(tid, 0, 0, 0, 1); 
      t.srCauseSPU = EC_BREAK;
    end
    op_wait:
    begin
    end
    op_eret:
    begin
      t.flush();
      t.pc = t.privMode == priv_user ? t.pcUEret : t.pcEret;
      t.privMode = t.privEret;
      t.srCauseSPU = EC_NOEXP;
      t.srCauseDSE = EC_NODSE;
      t.srCauseFu = 0;
    end
    op_tsync:
    begin
      bit sync = 1;
      foreach(thread[i])
        if(thread[i].srThreadGrp == t.srThreadGrp 
          && thread[i].threadState != ts_w_tsyn
          && thread[i].threadState != ts_disabled
          && i != tid)
          sync = 0;
      if(!sync) begin
        t.flush();
        restore_pc(tid, 1, STAGE_ISE_SRA); 
        t.threadState = ts_w_tsyn;
        `ip4_info("tsync", $psprintf("thread %0d grp %0d halted", tid, t.srThreadGrp), OVM_LOW)
      end
      else begin
        `ip4_info("tsync", $psprintf("exit halt for grp %0d", t.srThreadGrp), OVM_LOW)
        foreach(thread[i])
          if(thread[i].srThreadGrp == t.srThreadGrp 
            && thread[i].threadState != ts_disabled)
            thread[i].threadState = ts_rdy;
      end
    end
    op_syna:
    begin
      if(t.pendExLoad > 0 || t.pendExStore > 0) begin
        t.flush();
        restore_pc(tid, 0, STAGE_ISE_SRA); 
        t.threadState = ts_w_syna;
      end
    end
    op_synld:
    begin
      if(t.pendExLoad > 0) begin
        t.flush();
        restore_pc(tid, 0, STAGE_ISE_SRA); 
        t.threadState = ts_w_synld;
      end
    end
    op_synst:
    begin
      if(t.pendExStore > 0) begin
        t.flush();
        restore_pc(tid, 0, STAGE_ISE_SRA); 
        t.threadState = ts_w_synst;
      end
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
      SR_PROC_CTL:
      begin
        foreach(thread[i])
          if(thread[i].threadState == ts_disabled && op0[i])
            thread[i].threadState = ts_rdy;
        srDisableTimer = op0[25];
        srTimerMask = op0[26];
        srTimerPend = op0[27];
        srSupMsgMask = op0[28];
        srSupMsgPend = op0[29];
        srReducePower = op0[30];
      end
      SR_EBASE:
        srExpBase = op0;
      SR_THD_CTL:
      begin
        t.privMode = priv_mode_t'(op0[1:0]);
        t.srThreadGrp = op0[2];
        t.srKD = op0[3];
      end
      SR_EXEC:
      begin
        t.srRndMode = round_mode_t'(op0[2:0]);
        t.srExpMsk = op0[9:3];
      end
      SR_UEE:
        t.srUEE = op0;
      SR_UER:
        t.srUEE = op0;
      SR_FFC0:
      begin
        t.srFFST = op0[7:0];
        t.srFFRT = op0[15:8];
        t.srREE = op0[23:16];
      end
      SR_FFC1:
      begin
        t.srLSID = op[2:0];
        t.srLRID = op0[5:3];
        t.srLRRID = op0[13:6];
      end
      SR_TIMER:
        srTimer = op0;
      SR_PCNT0:
        srPCnt[0] = op0;
      SR_PCNT1:
        srPCnt[1] = op0;
      endcase
    end
    op_s2gp:
    begin
      case(sr)
      SR_EPC:
        res = t.pcExp;
      SR_ERET: 
        res = t.pcEret;
      SR_PROC_CTL:
      begin
        foreach(thread[i])
          res[i] = thread[i].threadState != ts_disabled;
        res[19:16] = pbId;
        res[23:20] = tid;
        res[25] = srDisableTimer;
        res[26] = srTimerMask;
        res[27] = srTimerPend;
        res[28] = srSupMsgMask;
        res[29] = srSupMsgPend;
        res[30] = srReducePower;
      end
      SR_EBASE:
        res = srExpBase;
      SR_THD_CTL:
      begin
        res[1:0] = t.privMode;
        res[2] = t.srThreadGrp;
        res[3] = t.srKD;
      end
      SR_EXEC:
      begin
        res[2:0] = t.srRndMode;
        res[9:3] = t.srExpMsk;
      end
      SR_THD_ST:
      begin
        res[3:0] = t.srCauseSPU;
        res[7:4] = t.srCauseDSE;
        res[8] = t.srCauseFu;
        res[9] = srSupMsgPend;
        res[11:10] = srPerfCntPend;        
        res[12] = srTimerPend;        
      end
      SR_FFC0:
      begin
        op0[7:0] = t.srFFST;
        op0[15:8] = t.srFFRT;
        op0[23:16] = t.srREE;
      end
      SR_FFC1:
      begin
        op[2:0] = t.srLSID;
        op0[5:3] = t.srLRID;
        op0[13:6] = t.srLRRID;
      end
      SR_TIMER:
        op0 = srTimer;
      SR_PCNT0:
        op0 = srPCnt[0];
      SR_PCNT1:
        op0 = srPCnt[1];
      endcase
    end
    endcase
    return res;
  endfunction : exe_ise
  
  function void enter_event(input uchar tid, user_event_os_t os);
    ise_thread_inf t = thread[tid];
    t.pcUEret = t.pc;
    t.privMode = priv_event;
    t.flush();
  endfunction

  function bit can_req_ifetch(input uchar tid);
    ise_thread_inf t = thread[tid];
    `ip4_info("can_req_ifetch", $psprintf("tid %0d, %s, iBuf lv %0d, pd %0d, sel %0d",
              tid, t.threadState.name, t.iBuf.size(), t.pendIFetch, v.TIdFetchSel), OVM_MEDIUM)
    if(t.threadState inside {ts_disabled, ts_w_rst})
      return 0;
    if((t.iBuf.size() + (t.pendIFetch + 1) * NUM_IFET_BYTES) >  NUM_IBUF_BYTES)
      return 0;
    return 1;
  endfunction : can_req_ifetch
    
  function bit can_issue(input uchar tid);
    /// the vec value indicate 4 cyc issue style is needed
    ise_thread_inf t = thread[tid];
                
    bit [NUM_FU-1:0] enFuTmp;
    uchar tmp = 0;
    foreach(enFuTmp[i])
      enFuTmp[i] = t.enFu[i];
          
    `ip4_info("can_issue",
      $psprintf("tid %0d, %s, decoded:%0d, Err:%0d, wCntBr:%0d,%0d, iCnt:%0d, ilCnt:%0d, pc:%0h spu:%0b, dse:%0b, fu:%b", 
                 tid, t.threadState.name, t.decoded, t.decodeErr, t.wCntBr, t.wCntBrDep, t.iCnt, t.ilCnt, t.pc, t.enSPU, t.enDSE,
                 enFuTmp), OVM_MEDIUM)
    
    if(!t.decoded)
      return 0;
    
    if(t.waitPip && t.iCnt > 0)
      return 0;
    
    if(t.iwCnt > t.iCnt && t.iwCnt != 0)
      return 0;
      
    if(t.waitLng && t.ilCnt > 0)
      return 0;

    ///wait out the vecmode
    if(t.wCntWr > 0)
      return 0;
    
    if(t.wCntBrDep && t.wCntBr > 0)
      return 0;
                  
    ///issue disable check
    if(t.iDSE.dse_block(noLd, noSt, noTMsg, noFMsg))
      return 0;
    
    foreach(noFu[i])
      if(noFu[i] && t.enFu[i])
        return 0;
        
    ///the three unit busy
    if(cntFuBusy > 0 && t.cntFuBusy > 0)
      return 0;

    if(cntDSEBusy > 0 && t.cntDSEBusy > 0)
      return 0;

    if(cntSPUBusy > 0 && t.cntSPUBusy > 0)
      return 0;
      
    ///read cyc check
    if(cntSrfBusy > 0 && t.cntSrfBusy > 0)
      return 0;

    if(cntVrfBusy > 0 && t.cntVrfBusy > 0)
      return 0;

    foreach(t.enFu[i])
      if(t.enFu[i] && t.iFu[i].op inside {sfu_only_ops} && cntSFUBusy > 0)
        return 0;
    
///    if(t.wCntNext[min_styp] != 0 && t.wCntNext[min_styp] < t.wCntBr)
///      return 0;

    ///because 3 lane can issue back to back in cycle, 
    ///if previous has exp, following vrf write may not be cancelled
///    if(t.wCntBr > 0 && t.wCntWr > 0)
///      return 0;
            
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
    
    if(t.enDSE && t.iDSE.op inside {ld_ops} && pipLd >= ldq)
      return 0;
    if(t.enDSE && t.iDSE.op inside {st_ops} && pipSt >= stq)
      return 0;
      
    if(t.threadState != ts_rdy)
      return 0;
    
    ///start event only in user mode
    if(t.privMode == priv_user) begin
      if(t.msgRdy & t.srREE)
        enter_event(tid, UE_FFREV);
      else if(t.enSPU && t.iSPU.op inside {op_rmsg, op_smsg}) begin
        if(t.srFFST[t.iSPU.mRt] && t.iSPU.op == op_smsg)
          enter_event(tid, UE_FFSTG);
        else if((t.srFFRT & t.iSPU.mFifos) && t.iSPU.op == op_rmsg) begin
          enter_event(tid, UE_FFRTG);
          t.srLRRID = t.iSPU.mFifos;
        end
      end
      else if(t.msgRdy && fifoCleanUp)
        enter_event(tid, UE_FFCLN);
    end
    
    if(t.decoded && t.enSPU) begin
      if(t.iSPU.op == op_rmsg) begin
        ///the rmsg is going exe, select a fifo to read
        ///only find a non event, non triggered fifo
        bit[NUM_FIFO - 1 : 0] tmp = (~t.srFFRT) & (~t.srREE) & t.msgRdy;
        if(tmp) begin
          foreach(tmp[i])
            if(tmp[i])
              t.iSPU.mRt = i;
          t.srLSID = t.iSPU.mRt;
        end
        else begin
          /// recieve failed
           enter_event(tid, UE_FFRFL);
           t.srLRRID = t.iSPU.mFifos;
        end
      end
      else if(t.iSPU.op == op_smsg) begin
        t.srLSID = t.iSPU.mRt;
      end
    end
    
    if(t.enDSE && t.iDSE.op == op_tmrf && t.mrfLocked) begin
      ///the mrf is locked for send msg, a move to mrf is waiting
      t.threadState = ts_w_mrf;
    end
    
    return 1;
  endfunction : can_issue

  function void fill_issue(input uchar tid);
    ise_thread_inf t = thread[tid];
    uchar cntMax;
    
    /// spu or scalar dse issue
    if(t.enSPU) begin
      bit brDepDSE = 0, brDepSPA = 0;
      if(t.iSPU.is_br() && !t.iSPU.is_unc_br()) begin
        ///find what br is depend on
        foreach(t.enFu[fid])
          if(t.enFu[fid] && t.iFu[fid].op inside {op_cmp, op_ucmp})
            brDepSPA = 1;
        if(t.enDSE)
          brDepDSE = !brDepSPA;
      end
      for(int i = 0; i < t.cntSPUBusy; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
        if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", this);
        t.iSPU.fill_rfm(ciRFM[i], i);
        t.iSPU.fill_spu(ciSPU[i]);
        ciSPU[i].brDepSPA = brDepSPA;
        ciSPU[i].brDepDSE = brDepDSE;
        ciSPU[i].tidSPU = tid;
        ciSPU[i].brPred = t.brPred;
        ciSPU[i].brSrf = vn.rst[1].brSrf;
        ciSPU[i].predPc = t.pc;
        ciSPU[i].prNMsk = '{default : t.noMsk};
        ciSPU[i].prNMskDSE = t.noMsk;
        ciSPU[i].prNMskSPU = t.noMsk;
        ciSPU[i].vecModeSPU = t.vecMode;
        ciSPU[i].subVecSPU = i;
        ciRFM[i].cycSPU = i;
      end
    end
    
    if(t.enDSE) begin
      for(int i = 0; i < t.cntDSEBusy; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
        if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", this);
        if(ciDSE[i] == null) ciDSE[i] = tr_ise2dse::type_id::create("toDSE", this);
        t.iDSE.fill_dse(ciDSE[i]);
        t.iDSE.fill_rfm(ciRFM[i], i);
        t.iDSE.fill_spu(ciSPU[i]);
        ciDSE[i].subVec = i;
        ciDSE[i].vecMode = t.vecMode;
        ciDSE[i].nonBlock = t.lpRndMemMode;
        ciDSE[i].tid = tid;
        ciDSE[i].noExt = t.noExt[i];
        ciDSE[i].priv = t.privMode == priv_kernel;
        ciSPU[i].vecModeDSE = t.vecMode;
        ciSPU[i].subVecDSE = i;
        ciSPU[i].tidDSE = tid;
        t.noExt[i] = 0;
        ciRFM[i].cycDSE = i;
      end
    end
    
    cntMax = max2(t.cntSrfBusy, t.cntVrfBusy);
    cntMax = max2(cntMax, t.cntDSEBusy);
    cntMax = max2(cntMax, t.cntSPUBusy);
    cntMax = max2(cntMax, t.cntFuBusy);
    
    for(int i = 0; i < t.cntSrfBusy; i++) begin
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
      ciRFM[i].srfRdGrp = t.srfGrp[i];
      ciRFM[i].srfRdAdr = t.srfAdr[i];
    end
    
    for(int i = 0; i < t.cntVrfBusy; i++) begin
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
      ciRFM[i].vrfRdGrp = t.vrfGrp[i];
      ciRFM[i].vrfRdAdr = t.vrfAdr[i];
    end
    
    for(int i = 0; i < cntMax; i++) begin
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
      ciRFM[i].bpCo = t.co; ///possible overwrite!!
      foreach(t.iFu[fid])
        if(t.enFu[fid]) begin
          t.iFu[fid].fill_rfm(ciRFM[i], i);
          ciRFM[i].cycFu = i;
          ciRFM[i].vecModeFu = t.vecMode;
        end
    end
    
    for(int i = 0; i < t.cntFuBusy; i++) begin
      if(ciSPA[i] == null) ciSPA[i] = tr_ise2spa::type_id::create("toSPA", this);
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", this);
      if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", this);
      if(t.enDSE)
        t.iDSE.fill_spa(ciSPA[i]);
      foreach(t.iFu[fid])
        if(t.enFu[fid]) begin
          t.iFu[fid].fill_spa(ciSPA[i]);
          t.iFu[fid].fill_spu(ciSPU[i]);
        end
      ciSPA[i].tid = tid;
      ciSPA[i].vecMode = t.vecMode;
      ciSPA[i].subVec = i;
      ciSPA[i].rndMode = t.srRndMode;
      ciSPA[i].expMsk = t.srExpMsk; 
      ciSPU[i].vecModeFu = t.vecMode;
      ciSPU[i].subVecFu = i;
      ciSPU[i].tidFu = tid;
    end
  endfunction : fill_issue

  function void issue(input uchar tid);
    ise_thread_inf t = thread[tid];
    uchar wCntWrNext = 0;
    
    vn.rst[1].priv = t.privMode;
    
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
    
    vn.rst[1].tid = tid;
    vn.rst[1].pc = t.pc;
    vn.rst[1].igb = t.iGrpBytes;
    vn.rst[1].en = 1;
    vn.rst[1].ejtag = t.ejtagMode;
    vn.rst[1].pcUEret = t.pcUEret;
    vn.rst[1].il = 0;
    
    if(t.enSPU) begin
      /// spu or scalar dse issue
      if(t.iSPU.is_priv()) begin
        if(t.privMode == priv_kernel)
          void'(exe_ise(tid, t.iSPU.op));
        else
          enter_exp(tid, exp_priv_err);
      end
      else if(t.iSPU.op inside {ise_zw_ops})
        void'(exe_ise(tid, t.iSPU.op));
    end

    t.br_pred(vn.rst[1].bpc, vn.rst[1].brSrf);
            
    if(t.enDSE) begin
      if(t.iDSE.op inside {ld_ops}) begin
        t.isLastLoad = 1;
        pipLd++;
      end
      else if(t.iDSE.op inside {st_ops}) begin
        t.isLastStore = 1;
        pipSt++;
      end
      t.isLastVecDse = t.iDSE.isVec;
      if(t.iDSE.isVec)
        wCntWrNext = t.vecMode + 1;
    end
    
    foreach(t.enFu[i])
      if(t.enFu[i])
        wCntWrNext = t.vecMode + 1;
    
    if(wCntWrNext > t.wCntWr)
      t.wCntWr = wCntWrNext;
            
    ///update wcnt
///    foreach(t.wCntDep[i])
///      if(t.wCntNext[i] > t.wCnt[t.wCntUp][i])
///        t.wCnt[t.wCntUp][i] = t.wCntNext[i];
///    if(t.wCntNext[br_styp] > t.wCntBr)
///      t.wCntBr = t.wCntNext[br_styp];
    if(t.wCntBrNext > t.wCntBr)
      t.wCntBr = t.wCntBrNext;
    
    if(t.cntSrfBusy > cntSrfBusy)
      cntSrfBusy = t.cntSrfBusy;
    if(t.cntVrfBusy > cntVrfBusy)
      cntVrfBusy = t.cntVrfBusy;
    if(t.cntDSEBusy > cntDSEBusy)
      cntDSEBusy = t.cntDSEBusy;
    if(t.cntFuBusy > cntFuBusy)
      cntFuBusy = t.cntFuBusy;
    if(t.cntDSEBusy > cntDSEBusy)
      cntDSEBusy = t.cntDSEBusy;
    if(t.cntSPUBusy > cntSPUBusy)
      cntSPUBusy = t.cntSPUBusy;
      
    cntPRWr += t.cntPRWr;
    foreach(cntVrfWr[i])
      cntVrfWr[i] += t.cntVrfWr[i];
    foreach(cntSrfWr[i])
      cntSrfWr[i] += t.cntSrfWr[i];
    
    foreach(t.enFu[i])
      if(t.enFu[i] && t.iFu[i].op inside {sfu_only_ops})
        cntSFUBusy = CYC_SFU_BUSY;///experiential number
    
    t.iCnt++;
    foreach(t.iFu[i])
      if(t.enFu[i] && t.iFu[i].op inside {sfu_only_ops}) begin
        t.ilCnt++;
        vn.rst[1].il = 1;
        break;
      end
        
    ///rdy to issue the ig 
    fill_issue(tid);
    `ip4_info("issue", {"\n", t.sprint()}, OVM_FULL)
  endfunction : issue
      
  function void comb_proc();
    `ip4_info("ise", "comb_proc procing...", OVM_DEBUG) 
    
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmRFM != null) end_tr(v.fmRFM);
    if(v.fmIFE != null) end_tr(v.fmIFE);
    if(v.fmDSE != null) end_tr(v.fmDSE);
    if(v.fmEIF != null) end_tr(v.fmEIF);
    
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmRFM = null;
    vn.fmIFE = null;
    vn.fmDSE = null;
    vn.fmEIF = null;
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
    
    for(int i = STAGE_EEX_VWB; i > 1; i--)
      vn.rst[i] = v.rst[i - 1];
    
    vn.rst[1] = new();
///    vn.rst[1].copy(v.rst[1]);
    
    foreach(thread[i]) begin
      cancel[i] = cancel[i] << 1;
      cancel[i][0] = thread[i].cancel;
      thread[i].cyc_new();
    end
    
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
    
    if(cntVrfBusy != 0) cntVrfBusy--;
    if(cntSrfBusy != 0) cntSrfBusy--;
    if(cntFuBusy != 0) cntFuBusy--;
    if(cntDSEBusy != 0) cntDSEBusy--;
    if(cntSPUBusy != 0) cntSPUBusy--;
    if(cntSFUBusy != 0) cntSFUBusy--;
    
    if(cntPRWr != 0) cntPRWr--;
    foreach(cntSrfWr[i])
      if(cntSrfWr[i] != 0) cntSrfWr[i]--;
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] != 0) cntVrfWr[i]--;
    
    noFu = '{default: 0};
    if(noLd > 0) noLd--;
    if(noSt > 0) noSt--;
    if(noTMsg > 0) noTMsg--;
    if(noFMsg > 0) noFMsg--;
    if(!srDisableTimer) srTimer++;
    if(srTimer == srCmp && !srTimerMask)
      srTimerPend = 1;
    
    begin
      ip4_tlm_ise_rst rst = v.rst[STAGE_RRF_VWBP];
      
      if(rst != null) begin
        uchar tid = rst.tid;
        ise_thread_inf t = thread[tid];
        if(rst.en) begin
          if(t.iCnt > 0)
            t.iCnt--;
          if(t.ilCnt > 0 && rst.roll && rst.il)
            t.ilCnt--;
        end
      end
    end
    
    begin
      ip4_tlm_ise_rst rst = v.rst[STAGE_EEX_VWB];
      if(rst != null) begin
        uchar tid = rst.tid;
        ise_thread_inf t = thread[tid];
        if(t.ilCnt > 0 && !rst.roll && rst.il)
          t.ilCnt--;
      end
    end
            
    if((srPerfCntPend || srTimerPend || srSupMsgPend) 
        && (thread[tidInt].threadState != ts_rdy || thread[tidInt].privMode != priv_kernel)) begin
      ///need to find a thread a handle this timer interrupt
      foreach(thread[i])
        if(thread[i].threadState == ts_rdy && thread[i].privMode != priv_kernel) begin
          if(srTimerPend)
            thread[i].srCauseSPU = EC_SUPMSG;
          else if(srTimerPend)
            thread[i].srCauseSPU = EC_TIMER;
          else if(srPerfCntPend[0])
            thread[i].srCauseSPU = EC_PCNT0;
          else
            thread[i].srCauseSPU = EC_PCNT1;
            
          tidInt = i;
          thread[i].flush();
          restore_pc(i, 1, 0, 1);
          break;
        end
    end
    
    ///brSrf bpc
    if(v.fmSPU != null && v.rst[STAGE_ISE_EXS1].brSrf)
      vn.rst[STAGE_ISE_EXS2].bpc = v.fmSPU.bpc;
    
    ///SR Requests
    if(v.fmSPU != null && v.fmSPU.srReq) begin
      tr_spu2ise spu = v.fmSPU;
      if(spu != null && spu.s2gp) begin
        if(ciSPU[0] != null) ciSPU[0] = tr_ise2spu::type_id::create("toSPU", this);
        ciSPU[0].srRes = exe_ise(spu.tidSPU, spu.op, spu.op0, spu.srAdr);
      end
      if(spu.op != op_s2gp)
        void'(exe_ise(spu.tidSPU, spu.op, spu.op0, spu.srAdr));
    end
    
    ///cancel condition 1 branch mispredication, msc exp
    if(v.fmSPU != null && v.fmSPU.brRsp) begin
      tr_spu2ise spu = v.fmSPU;
      resolve_br(spu.tidBr, spu.brTaken, spu.missBr);
      if(spu.sclExp)
        enter_exp(spu.tidSclExp, exp_scl_err, spu.vecModeSclExp);
      if(spu.mscExp)
        enter_exp(spu.tidBr, exp_msc_err, spu.vecMode);
    end
    
    ///cancel condition 2, spa exp
    if(v.fmSPA != null && v.fmSPA.exp)
      enter_exp(v.fmSPA.tid, exp_vfu_err);
    
    ///cancel condition 3 dse exp or cache miss
    if(v.fmDSE != null && v.fmDSE.rsp) begin
      tr_dse2ise dse = v.fmDSE;
      ise_thread_inf t = thread[dse.tid],
                     t2 = thread[dse.tidNoExt];
      uchar st = STAGE_ISE_DBR + dse.vecMode;
      t.pendExLoad = dse.pendExLoad;
      t.pendExStore = dse.pendExStore;
      t.pendSMsg = dse.pendSMsg;
      ldq = dse.ldq;
      stq = dse.stq;
      
      if(dse.rlsPipSt) pipSt--;
      if(dse.rlsPipLd) pipLd--;
      
      if(dse.noExtSt) begin
        t2.pendExStore = 0;
        if(t2.threadState inside {ts_w_synst, ts_w_syna})
          t2.threadState = ts_rdy;
      end
      if(dse.noExtLd) begin
        t2.pendExLoad = 0;
        if(t2.threadState inside {ts_w_synld, ts_w_syna})
          t2.threadState = ts_rdy;
      end
      if(dse.exp && !cancel[dse.tid][STAGE_ISE_DBR]) begin
        t.srCauseDSE = dse.cause;
        t.flush();
        restore_pc(dse.tid, 0, st, 1);
        cancel[dse.tid] |= `GML(STAGE_ISE_DBR);
      end
      else if(dse.extLd && !cancel[dse.tid][STAGE_ISE_DBR]) begin
        t.flush();
        restore_pc(dse.tid, 1, st);
        cancel[dse.tid] |= `GML(STAGE_ISE_DBR);
      end
///      else if(|dse.reRun && !cancel[dse.tid][STAGE_ISE_DBR]) begin
///        ///todo rerun is not in dbr!!
///        t.flush();
///        restore_pc(dse.tid, 0, st);
///        t.noExt = ~dse.reRun;
///        cancel[dse.tid] |= `GML(STAGE_ISE_DBR);
///      end
    end
    
    ///update no_* for issue & check
    if(v.fmSPA != null)
      noFu = v.fmSPA.noFu;
      
    if(pendEIF.size() > 0 || (v.fmEIF != null && v.fmEIF.reqNo)) begin
      if(ciDSE[0] == null || !ciDSE[0].en) begin
        tr_eif2ise eif;
        if(pendEIF.size() > 0) begin
          eif = pendEIF.pop_front();
          if(v.fmEIF != null && v.fmEIF.reqNo)
            pendEIF.push_back(v.fmEIF);
        end
        else
          eif = v.fmEIF;
        noLd += eif.noLd;
        noSt += eif.noSt;
        noTMsg += eif.noTMsg;
        noFMsg += eif.noFMsg;
        foreach(cntVrfWr[i])
          cntVrfWr[i] += eif.vecCnt;
        foreach(cntSrfWr[i])
          cntSrfWr[i] += eif.sclCnt;
        if(toEIF == null) toEIF = tr_ise2eif::type_id::create("toEIF", this);
          toEIF.rsp = 1;
      end
      else if(v.fmEIF != null && v.fmEIF.reqNo)
        pendEIF.push_back(v.fmEIF);
    end
    
    if(v.fmEIF != null) begin
      tr_eif2ise eif = v.fmEIF;
      fifoCleanUp = eif.reqCleanUp;
      noSMsg = eif.noSMsg;
      foreach(thread[i]) begin
        thread[i].msgRdy = eif.msgRdy[i];
        thread[i].mrfLocked = eif.mrfLocked[i];
        if(!thread[i].mrfLocked && thread[i].threadState == ts_w_mrf)
          thread[i].threadState = ts_rdy;
      end
    end
    
    ///check & issue, cancel condition 3, ise decode Err, priv enter, uncond branch
    `ip4_info("ise inf", $psprintf("\n%s", sprint(printer)), OVM_FULL)
    for(int i = 0; i < NUM_THREAD; i++) begin
      uchar tid = i + v.TIdIssueSel;
      ise_thread_inf t;
      tid = tid & `GML(WID_TID);
      t = thread[tid];
      
      `ip4_info("issue", $psprintf("checking thread %0d, %s, decoded %0d", tid, t.threadState, t.decoded), OVM_FULL)
      if(can_issue(tid)) begin
        `ip4_info("issue", $psprintf("issuing thread %0d, pc 0x%0h, sel %0d", tid, t.pc, v.TIdIssueSel), OVM_LOW)
        issue(tid);
        if(v.TIdIssueSel == tid || thread[v.TIdIssueSel].threadState != ts_rdy) begin
          for(int j = 1; i < NUM_THREAD; j++) begin
            uchar nxt = (v.TIdIssueSel + j) & `GML(WID_TID);
            if(!thread[nxt].threadState inside {ts_disabled, ts_w_rst}) begin
              vn.TIdIssueSel = nxt;
              break;
            end
          end
        end
        break;
      end
    end
    
    ///update ife data into thread
    if(v.fmIFE != null && v.fmIFE.instEn) begin
      if(!cancel[v.fmIFE.tid][0])
        thread[v.fmIFE.tid].update_inst(v.fmIFE.fetchGrp);
      else
        `ip4_info("update_inst", $psprintf("canceling, tid: %0d", v.fmIFE.tid), OVM_FULL)
    end
    
    ///try to decode one inst grp
    for(int i = 0; i < NUM_THREAD; i++) begin
      uchar tid = i + v.TIdDecodeSel;
      ise_thread_inf t;
      tid = tid & `GML(WID_TID);
      t = thread[tid];
      
      if(t.iBuf.size() > 1 && !t.decoded)
        t.decode_igrp_start();
              
      if(!(t.threadState inside {ts_disabled, ts_w_rst}) && t.iBuf.size() > 2
           && !t.decoded && t.iBuf.size() >= t.iGrpBytes) begin
        t.decode_igrp();
        if(v.TIdDecodeSel == tid || thread[v.TIdDecodeSel].threadState != ts_rdy) begin
          for(int j = 1; i < NUM_THREAD; j++) begin
            uchar nxt = (v.TIdDecodeSel + j) & `GML(WID_TID);
            if(!thread[nxt].threadState inside {ts_disabled, ts_w_rst}) begin
              vn.TIdDecodeSel = nxt;
              break;
            end
          end
        end
        break;
      end
    end
    
    foreach(thread[i])
      if(thread[i].pendIFetchExp) begin
        thread[i].iFetchExp = 1;
        thread[i].decoded = 1;
      end
    
    ///rollback
    for(int i = STAGE_ISE_VWB_END; i > 0; i--) begin
      uchar tid = v.rst[i].tid;
      ise_thread_inf t = thread[tid];
      uint res,
           pc = v.rst[i].pc,
           npc = (v.rst[i].pc + v.rst[i].igb),
           bpc = v.rst[i].bpc;
      if(!v.rst[i].en || !v.rst[i].roll || cancel[tid][STAGE_ISE_VWB_END])
        continue;
      case(v.rst[i].sel)
      0:  res = pc;
      1:  res = npc;
      2:  res = bpc;
      endcase
      
      t.privMode = v.rst[i].priv;
      t.pc = res;
      t.pcUEret = v.rst[i].pcUEret;
      t.ejtagMode = v.rst[i].ejtag;
      
      `ip4_info("rollback", $psprintf("tid %0d, stage %0d, exp %0d, pc 0x%0h, srExpBase 0x%0h, %s",
                  v.rst[i].tid, i, v.rst[i].exp, res, srExpBase, t.privMode.name), OVM_LOW)  
      
      if(v.rst[i].exp) begin
        if(v.rst[i].ejtag) begin
          t.pc = VADR_EJTAGS;
          t.ejtagMode = 1;
          if(t.srKD)
            t.privMode = priv_kernel;
        end
        else begin
          t.pcEret = res;
          t.pcExp = pc;
          t.pc = srExpBase;
          t.privMode = priv_kernel;
        end
      end
      v.rst[i].roll = 0;
      if(t.threadState == ts_w_rst)
        t.threadState = ts_rdy;
      break;
    end
  endfunction
  
  function void req_proc();
    tr_ise2rfm toRFM;
    tr_ise2spu toSPU;
    tr_ise2spa toSPA;
    tr_ise2ife toIFE;
    tr_ise2dse toDSE;
    
    `ip4_info("ise", "req_proc procing...", OVM_DEBUG) 
    
    vn.rfm[1] = ciRFM[0];
    vn.spa[1] = ciSPA[0];
    vn.spu[1] = ciSPU[0];
    vn.dse[1] = ciDSE[0];  
    
    foreach(ciDSE[i]) begin
      if(ciDSE[i] != null && cancel[ciDSE[i].tid][0]) begin
        ciDSE[i].op = op_nop;
        ciDSE[i].en = 0;
        ciDSE[i].wr = 0;
        ciDSE[i].uaWrEn = 0;
      end
      if(ciSPU[i] != null && cancel[ciSPU[i].tidSPU][0]) begin
        ciSPU[i].op = op_nop;
        ciSPU[i].wrEn = 0;
      end
      if(ciSPA[i] != null && cancel[ciSPA[i].tid][0])
        foreach(ciSPA[0].fu[j]) begin
          ciSPA[i].fu[j].op = op_nop;
          ciSPA[i].fu[j].wrEn = '{default : 0};
        end
    end

    foreach(v.rfm[i]) begin
      if(vn.dse[i] != null && cancel[vn.dse[i].tid][0]) begin
        vn.dse[i].op = op_nop;
        vn.dse[i].en = 0;
        vn.dse[i].wr = 0;
        vn.dse[i].uaWrEn = 0;
      end
      if(vn.spu[i] != null && cancel[vn.spu[i].tidSPU][0]) begin
        vn.spu[i].op = op_nop;
        vn.spu[i].wrEn = 0;
      end
      if(vn.spa[i] != null && cancel[vn.spa[i].tid][0])
        foreach(vn.spa[1].fu[j]) begin
          vn.spa[i].fu[j].op = op_nop;
          vn.spa[i].fu[j].wrEn = '{default : 0};
        end
    end
            
    toRFM = v.rfm[STAGE_ISE];
    toSPA = v.spa[STAGE_ISE];
    toSPU = v.spu[STAGE_ISE];
    toDSE = v.dse[STAGE_ISE];
    
    ///ife req search
    for(int i = 0; i < NUM_THREAD; i++) begin
      uchar tid = i + v.TIdFetchSel;
      tid = tid & `GML(WID_TID);
      if(can_req_ifetch(tid)) begin
        toIFE = tr_ise2ife::type_id::create("toIFE", this);
        thread[tid].fill_ife(toIFE);
///        `ip4_info("ifetch", $psprintf("tid %0d, sel %0d, pc 0x%0h", tid, v.TIdFetchSel, toIFE.pc), OVM_LOW);
        toIFE.tid = tid;
        if(v.TIdFetchSel == tid || thread[v.TIdFetchSel].threadState != ts_rdy) begin
          for(int j = 1; i < NUM_THREAD; j++) begin
            uchar nxt = (v.TIdFetchSel + j) & `GML(WID_TID);
            if(!thread[nxt].threadState inside {ts_disabled, ts_w_rst}) begin
              vn.TIdFetchSel = nxt;
              break;
            end
          end
        end
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
    
    if(toDSE != null && toDSE.op inside {ld_ops, st_ops, op_tmrf, op_fmrf}) begin
      if(toEIF == null) toEIF = tr_ise2eif::type_id::create("toEIF", this);
      toEIF.issueLd = toDSE.op inside {ld_ops};
      toEIF.issueSt = toDSE.op inside {st_ops};
      toEIF.issueFMsg = toDSE.op == op_fmrf;
      toEIF.issueTMsg = toDSE.op == op_tmrf;
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
    `ip4_info("ise_tr", $psprintf("Get ife Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    end_tr(req);
    if(cancel[req.tid][0] || thread[req.tid].cancel)
      `ip4_info("ise_tr", $psprintf("canceling tid %0d", req.tid), OVM_MEDIUM)
    else
      vn.fmIFE = req;
    rsp = req;
    return 1;
  endfunction : nb_transport_ife

  function bit nb_transport_spu(input tr_spu2ise req, output tr_spu2ise rsp);
    `ip4_info("ise_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2ise req, output tr_spa2ise rsp);
    `ip4_info("ise_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa
  
  function bit nb_transport_rfm(input tr_rfm2ise req, output tr_rfm2ise rsp);
    `ip4_info("ise_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_dse(input tr_dse2ise req, output tr_dse2ise rsp);
    `ip4_info("ise_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_eif(input tr_eif2ise req, output tr_eif2ise rsp);
    `ip4_info("ise_tr", $psprintf("Get eif Transaction:\n%s", req.sprint()), OVM_FULL)
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
       `ip4_info("sync", $psprintf("sync already called. stamp is %0t", stamp), OVM_DEBUG)
       return;
     end
    stamp = $time;
    `ip4_info("sync", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_DEBUG)
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

    foreach(thread[i])
      thread[i] = new($psprintf("thread%0d", i), this);
                
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();
    stamp = 0ns;
   
    cntVrfBusy = 0;
    cntSrfBusy = 0;
    cntDSEBusy = 0;
    cntPRWr = 0;
    cntSrfWr = '{default: 0};
    cntVrfWr = '{default: 0};
    srExpBase = CFG_START_ADR;
    
    ldq = NUM_LDQUE;
    stq = NUM_STQUE;
    
    printer = new();
    printer.knobs.depth = 1;
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
