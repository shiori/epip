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
  exp_decode_err,   exp_dse_err,    exp_priv_err,     exp_msc_err
}ise_exp_t;

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fmSPU;
  tr_rfm2ise fmRFM;
  tr_ife2ise fmIFE;
  tr_spa2ise fmSPA;
  tr_dse2ise fmDSE[STAGE_ISE_VWBP:STAGE_ISE_DEM];
  
  tr_ise2rfm rfm[STAGE_ISE:1];
  tr_ise2spa spa[STAGE_ISE:1];
  tr_ise2spu spu[STAGE_ISE:1];
  tr_ise2dse dse[STAGE_ISE:1];
  
  uchar TIdIssueLast, TIdFetchLast;
  bit cancel[NUM_THREAD];
  uint pcStages[STAGE_ISE_VWB:1];
    
  `ovm_component_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_int(TIdIssueLast, OVM_ALL_ON)
    `ovm_field_int(TIdFetchLast, OVM_ALL_ON)
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
    `ovm_field_sarray_int(pcStages, OVM_ALL_ON)
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
  uchar IGrpBytes, adrPkgBytes, numImms,
        cntSrfRd, cntVrfRd, cntDSERd;
  word co[NUM_BP_CO];
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
  bit pendLoad, pendStore, loopRandMemMode;
  uchar pendIFetch, pendMemAcc, pendBr;
  
  inst_c iSPU, iDSE, iFu[NUM_FU];
  uint pc, pcBr, pcEret;
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
    `ovm_field_int(IGrpBytes, OVM_ALL_ON)
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
    `ovm_field_int(pcBr, OVM_ALL_ON)
    `ovm_field_int(pcEret, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_int(pendLoad, OVM_ALL_ON)
    `ovm_field_int(pendStore, OVM_ALL_ON)
    `ovm_field_int(loopRandMemMode, OVM_ALL_ON)
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
    pcBr = CFG_START_ADR;
    vecMode = CYC_VEC - 1;
    decoded = 0;
    decodeErr = 0;
    print_enabled = 0;
  endfunction : new
 
  function void map_iadr(input bit v, uchar orgAdr, output uchar grp, adr);
    uchar adrBits =  v ? (BITS_PRF_P_GRP - BITS_VRF_BKS) : (BITS_PRF_P_GRP - BITS_SRF_BKS);
    adr = orgAdr & ('1 << adrBits);
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
      pcBr = pc;
      brPred = 1;
      threadState = ts_b_self;
    end
    else begin
      pcBr = pc + iSPU.offSet;
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
      IGrpBytes = 1 + adrPkgBytes + numImms * NUM_WORD_BYTES + NUM_INST_BYTES;
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
      IGrpBytes = 2 + adrPkgBytes + numImms * NUM_WORD_BYTES + tmp * NUM_INST_BYTES;
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
                   IGrpBytes, enSPU, enDSE, enFuTmp, dseVec, wCntSel, adrPkgBytes, numImms),
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
      co[i] = {iBuf[i + 3], iBuf[i + 2], iBuf[i + 1], iBuf[i]};
      offSet += NUM_WORD_BYTES;
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
    IGrpBytes = 0;
    decoded = 0;
    decodeErr = 0;
    pendIFetch = 0;
    threadState = ts_rdy;
    cancel = 1;
  endfunction : flush
  
  function void retrieve_pc(uint adr);
    flush();
    pc = adr;
   endfunction : retrieve_pc

  function void msg_wait();
  endfunction : msg_wait
  
  function bit br_pred_miss(input bit br);
    if(threadState inside {ts_w_b, ts_b_pred, ts_b_self}) begin
      if(pendBr > 0) pendBr--;
      if(pendBr == 0)
        threadState = ts_rdy;
      if(br != brPred) begin
        flush();
        pc = pcBr;
        return 1;
      end
    end
    return 0;
  endfunction : br_pred_miss

  function bit can_req_ifetch();
    ovm_report_info("can_req_ifetch", $psprintf("threadState:%s, iBuf lv:%0d, pd:%0d", threadState.name, iBuf.size(), pendIFetch), OVM_HIGH);
    if(threadState == ts_disabled)
      return 0;
    if(iBuf.size() + pendIFetch * NUM_IFET_BYTES >=  NUM_IBUF_BYTES)
      return 0;
    if(IGrpBytes == 0)
      return 1;
    if(iBuf.size() < IGrpBytes)
      return 1;
    return 0;
  endfunction : can_req_ifetch
      
  function void update_inst(input inst_fg_c fetchGrp);
    uchar offSet = 0, LvlLast = iBuf.size();
    if(LvlLast  >= NUM_MAX_IGRP_BYTES)
      ovm_report_warning("ise", "iBuf overflow!");
    if(LvlLast == 0) ///only calculate offSet when iBuf size is reset to 0
      offSet = pc & ~{'1 << BITS_IFET};

    if(pendIFetch > 0)
      pendIFetch--;
    
    if(cancel) begin
      ovm_report_info("update_inst", $psprintf("cancel, pc:0x%0h, offSet:%0h, pd:%0d", pc, offSet, pendIFetch), OVM_HIGH);
      return;
    end
      
    foreach(fetchGrp.data[i])
      if(i >= offSet)
        iBuf.push_back(fetchGrp.data[i]);

    ovm_report_info("update_inst", $psprintf("pc:0x%0h, offSet:%0h, pd:%0d, iBuf lv %0d->%0d", pc, offSet, pendIFetch, LvlLast, iBuf.size()), OVM_HIGH);
  endfunction : update_inst

  function void fill_ife(input tr_ise2ife ife);
    ife.fetchReq = 1;
    ife.pc = (pc + NUM_IFET_BYTES * pendIFetch) & ~{'1 << BITS_IFET};
    pendIFetch++;
  endfunction : fill_ife
  
  function void fill_issue(ref tr_ise2rfm ciRFM[CYC_VEC], tr_ise2spa ciSPA[CYC_VEC], 
                               tr_ise2spu ciSPU[CYC_VEC], tr_ise2dse ciDSE[CYC_VEC],
                           input uchar tid, pbId);
    
    if(ciRFM[0] == null) ciRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    if(ciRFM[cntVrfRd] == null) ciRFM[cntVrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    ciRFM[0].start = 1;
    ciRFM[cntVrfRd].vecEnd = 1;

    foreach(iFu[i])
      iFu[i].map_wr_grp(vrfMap, srfMap);

    /// spu or scalar dse issue
    if(enSPU) begin
      bit brDepDSE = 0, brDepSPA = 0;
      iSPU.map_wr_grp(vrfMap, srfMap);
      if(ciRFM[0] == null) ciRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciRFM[cntSrfRd] == null) ciRFM[cntSrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciSPU[cntSrfRd] == null) ciSPU[cntSrfRd] = tr_ise2spu::type_id::create("toSPU", get_parent());
      ciRFM[0].start = 1;
      ciRFM[cntSrfRd].sclEnd = 1;
      ciSPU[cntSrfRd].start = 1;
      if(iSPU.is_br() && !iSPU.is_unc_br()) begin ///a conditional branch need vecMode cycles
        cntSrfRd = vecMode;
        ///find what br is depend on
        foreach(enFu[fid])
          if(enFu[fid] && iFu[fid].op inside {op_cmp, op_ucmp})
            brDepSPA = 1;
        if(enDSE)
          brDepDSE = !brDepSPA;
      end
      ciSPU[cntSrfRd].brEnd = iSPU.is_br();
      for(int i = 0; i <= cntSrfRd; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(ciSPA[i] == null) ciSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
        iSPU.fill_rfm(ciRFM[i], i);
        iSPU.fill_spu(ciSPU[i]);
        ciSPU[i].brDepSPA = brDepSPA;
        ciSPU[i].brDepDSE = brDepDSE;
        ciSPU[i].tid = pbId;
      end
    end
    
    if(enDSE) begin
      iDSE.map_wr_grp(vrfMap, srfMap);
      for(int i = 0; i <= cntDSERd; i++) begin
        if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
        if(ciDSE[i] == null) ciDSE[i] = tr_ise2dse::type_id::create("toDSE", get_parent());
        iDSE.fill_dse(ciDSE[i]);
        iDSE.fill_rfm(ciRFM[i], i);
        iDSE.fill_spu(ciSPU[i]);
        ciDSE[i].subVec = i;
        ciDSE[i].vecMode = vecMode;
        ciDSE[i].nonBlock = loopRandMemMode;
        ciDSE[i].tid = tid;
        ciDSE[i].pbId = pbId;
      end
    end
          
    for(int i = 0; i <= cntSrfRd || i <= cntVrfRd; i++) begin
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      ciRFM[i].bpCo = co;
      ciRFM[i].vrfRdGrp = vrfGrp[i];
      ciRFM[i].vrfRdAdr = vrfAdr[i];
      ciRFM[i].srfRdGrp = srfGrp[i];
      ciRFM[i].srfRdAdr = srfAdr[i];
    end
    
    for(int i = 0; i <= cntVrfRd; i++) begin
      if(ciSPA[i] == null) ciSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
      if(ciRFM[i] == null) ciRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(ciSPU[i] == null) ciSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
      if(enDSE)
        iDSE.fill_spa(ciSPA[i]);
      foreach(iFu[fid])
        if(enFu[fid]) begin
          iFu[fid].fill_rfm(ciRFM[i], i);
          iFu[fid].fill_spa(ciSPA[i]);
          iFu[fid].fill_spu(ciSPU[i]);
        end
      ciSPA[i].vecMode = vecMode;
      ciSPU[i].vecMode = vecMode;
      ciSPA[i].tid = tid;
      ciRFM[i].tid = tid;
      ciRFM[i].cyc = i;
      ciSPA[i].subVec = i;
      ciSPU[i].subVec = i;
    end
  endfunction : fill_issue

endclass : ise_thread_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;

  local uchar cntVrfRd, cntSrfRd, cntDSERd, cntVecProc,
              cntPRWr, cntSrfWr[NUM_SRF_BKS], cntVrfWr[NUM_VRF_BKS];
        
  local bit noLd, noSt, noSMsg, noRMsg, noFu[NUM_FU];
  
  local tr_ise2rfm ciRFM[CYC_VEC];
  local tr_ise2spa ciSPA[CYC_VEC];
  local tr_ise2spu ciSPU[CYC_VEC];
  local tr_ise2dse ciDSE[CYC_VEC];
  
  ovm_nonblocking_transport_imp_spu #(tr_spu2ise, tr_spu2ise, ip4_tlm_ise) spu_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2ise, tr_spa2ise, ip4_tlm_ise) spa_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2ise, tr_rfm2ise, ip4_tlm_ise) rfm_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2ise, tr_ife2ise, ip4_tlm_ise) ife_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2ise, tr_dse2ise, ip4_tlm_ise) dse_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_ise2rfm, tr_ise2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spu, tr_ise2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spa, tr_ise2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2ife, tr_ise2ife) ife_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2dse, tr_ise2dse) dse_tr_port;

  local ip4_tlm_ise_vars v, vn;
  local ise_thread_inf thread[NUM_THREAD];
  local ip4_printer printer;
  local uchar srPBId;
  local uint srExpBase;
  
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
  `ovm_component_utils_end

  function void enter_exp_pc(input uchar tid, bit ejtag = 0);
    ise_thread_inf tInf = thread[tid];
    tInf.privMode = 1;
    if(tInf.ejtagMode || ejtag) begin
      tInf.pc = IP4_BASE + VADR_EJTAGS + EJTG_SIZE * srPBId;
      tInf.ejtagMode = 1;
    end
    else begin
      tInf.pcEret = tInf.pc;
      tInf.pc = srExpBase;
    end
  endfunction
  
  function void enter_exp(input uchar tid, ise_exp_t Err);
    ise_thread_inf tInf = thread[tid];
    enter_exp_pc(tid); 
    case(Err)
    exp_decode_err  : begin end
    exp_dse_err     : begin end
    exp_priv_err    : begin end
    exp_msc_err     : begin end
    endcase
    tInf.flush();
  endfunction : enter_exp

  function void exe_ise(input uchar tid);
    ise_thread_inf tInf = thread[tid];
    case(tInf.iSPU.op)
    op_exit,
    op_sys: 
    begin
      enter_exp_pc(tid); 
      tInf.flush();     
    end
    op_brk  :
    begin
      enter_exp_pc(tid, 1); 
      tInf.flush();     
    end
    op_wait:
    begin
    end
    op_eret:
    begin
      tInf.pc = tInf.pcEret;
      tInf.privMode = 0;
      tInf.flush();
    end
    op_tsync:
    begin
    end
    op_msync:
    begin
    end
    op_alloc:
    begin
    end
    op_pint:
    begin
    end
    op_gp2s:
    begin
    end
    op_s2gp:
    begin
    end
    endcase
  endfunction : exe_ise

  function bit can_issue(input uchar tid);
    /// the vec value indicate 4 cyc issue style is needed
    ise_thread_inf tInf = thread[tid];
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [NUM_FU-1:0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = tInf.enFu[i];
        
      ovm_report_info("can_issue",
        $psprintf("threadState:%s, decoded:%0d, Err:%0d, wCnt:%0d, pc:%0h spu:%0b, dse:%0b, fu:%b. dv:%0b, wCntSel:%0b", 
                   tInf.threadState.name, tInf.decoded, tInf.decodeErr, tInf.wCnt[tInf.wCntSel], tInf.pc, tInf.enSPU, tInf.enDSE,
                   enFuTmp, tInf.dseVec, tInf.wCntSel),
        OVM_HIGH);
    end
    
    if(tInf.enVec && cntVecProc >= tInf.vecMode)
      return 0;
      
    ///issue disable check
    if(tInf.iDSE.dse_block(noLd, noSt, noSMsg, noRMsg))
      return 0;
    
    foreach(noFu[i])
      if(noFu[i] && tInf.enFu[i])
        return 0;
    
    ///read cyc check
    if(cntSrfRd > 0 && tInf.cntSrfRd > 0)
      return 0;

    if(cntVrfRd > 0 && tInf.cntVrfRd > 0)
      return 0;

    if(cntDSERd > 0 && tInf.cntDSERd > 0)
      return 0;

    /// write buf overflow check
    if(cntPRWr + tInf.cntPRWr > CYC_VEC)
      return 0;
    
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] + tInf.cntVrfWr[i] > CYC_VEC)
        return 0;      

    foreach(cntSrfWr[i])
      if(cntSrfWr[i] + tInf.cntSrfWr[i] > CYC_VEC)
        return 0;
    
    if(!tInf.loopRandMemMode && tInf.pendMemAcc > 0)
      return 0;
      
    return tInf.decoded && (tInf.threadState inside {ts_rdy, ts_w_b, ts_b_self} && tInf.wCnt[tInf.wCntSel] == 0);
  endfunction : can_issue

  function void issue(input uchar tid, pbId);
    ise_thread_inf tInf = thread[tid];
    vn.pcStages[1] = tInf.pc;
    if(tInf.decodeErr) begin
      if(tInf.threadState == ts_rdy)
        enter_exp(tid, exp_decode_err);
      return;
    end
      
    if(tInf.enSPU) begin
      if(tInf.iSPU.is_unc_br()) begin
        tInf.pc = tInf.pc + tInf.iSPU.offSet;
        tInf.brPred = 1;
      end
      else if(tInf.iSPU.is_br())
        tInf.br_pred();
      
    /// spu or scalar dse issue
      if(tInf.iSPU.is_priv()) begin
        if(tInf.privMode)
          exe_ise(tid);
        else
          enter_exp(tid, exp_priv_err);
      end
      else if(tInf.iSPU.is_ise_inst())
        exe_ise(tid);
    end

    ///branch taken
    if(tInf.enSPU && tInf.iSPU.is_br() && tInf.brPred) begin
      ///jmp to current?
      if(tInf.iSPU.offSet == 0)
        tInf.decoded = 1;
      else begin
        tInf.flush();
        tInf.decoded = 0;
      end
    end
    ///not branch or branch not taken
    else begin
      tInf.pc += tInf.IGrpBytes;
      tInf.decoded = 0;
      tInf.iBuf = tInf.iBuf[tInf.IGrpBytes:$];
      tInf.decodeErr = 0;
    end
            
    if(tInf.enDSE) begin
      cntDSERd = tInf.cntDSERd;
      tInf.pendMemAcc++;
      if(tInf.iDSE.op inside {op_lw, op_lh, op_lhu, op_lb, op_lbu})
        tInf.pendLoad = 1;
      else if(tInf.iDSE.op inside {op_sw, op_sh, op_sb})
        tInf.pendStore = 1;
      tInf.loopRandMemMode =  (tInf.iDSE.mT == 1 && tInf.enSPU && tInf.iSPU.is_br()
        && tInf.threadState == ts_b_self && tInf.iSPU.prWrAdr[0] == tInf.iDSE.prRdAdr
        && tInf.iDSE.prRdAdr != 0 && tInf.brPred);
    end

    if(tInf.wCntNext > tInf.wCnt[tInf.wCntSel])
      tInf.wCnt[tInf.wCntSel] = tInf.wCntNext;
          
    cntSrfRd = tInf.cntSrfRd;
    cntVrfRd = tInf.cntVrfRd;
    cntDSERd = tInf.cntDSERd;
    cntPRWr += tInf.cntPRWr;
  
    foreach(cntVrfWr[i])
      cntVrfWr[i] += tInf.cntVrfWr[i];

    foreach(cntSrfWr[i])
      cntSrfWr[i] += tInf.cntSrfWr[i];
    
    if(tInf.enVec)
      cntVecProc = tInf.vecMode;

    ///rdy to issue the ig 
    tInf.fill_issue(ciRFM, ciSPA, ciSPU, ciDSE, tid, pbId); 
  endfunction : issue
      
  function void comb_proc();
    ovm_report_info("ise", "comb_proc procing...", OVM_FULL); 
    
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmRFM != null) end_tr(v.fmRFM);
    if(v.fmIFE != null) end_tr(v.fmIFE);
    if(v.fmDSE[STAGE_ISE_DEM] != null) end_tr(v.fmDSE[STAGE_ISE_DEM]);
    
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmRFM = null;
    vn.fmIFE = null;
    vn.fmDSE[STAGE_ISE_DEM] = null;
    
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
    
    for(int i = STAGE_ISE_VWB; i > 1; i--)
      vn.pcStages[i] = v.pcStages[i - 1];
      
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
    noLd = 0;
    noSt = 0;
    noSMsg = 0;
    noRMsg = 0;

    for(int i = STAGE_ISE_VWBP; i > STAGE_ISE_DEM; i--)
      vn.fmDSE[i] = v.fmDSE[i-1];  
          
    ///cancel condition 1 branch mispredication, msc overflow
    if(v.fmSPU != null && v.fmSPU.brRsp) begin
      bit cancel;
      cancel = thread[v.fmSPU.tid].br_pred_miss(v.fmSPU.brTaken);
      if(v.fmSPU.mscTopChg) begin
        cancel = 1;
        enter_exp(v.fmIFE.tid, exp_msc_err);
      end
      if(v.fmIFE != null && cancel && v.fmIFE.tid == v.fmSPU.tid)
        v.fmIFE.instEn = 0;
    end
    
    ///cancel condition 2, spa exp
    if(v.fmSPA != null && v.fmSPA.exp) begin
      thread[v.fmSPA.tid].retrieve_pc(v.pcStages[STAGE_ISE_VWB]);
    end
    
    ///cancel condition 2 dse exp or cache miss
    if(v.fmDSE[STAGE_ISE_DEM] != null) begin
      tr_dse2ise dse = v.fmDSE[STAGE_ISE_DEM];
      if(dse.rdy) begin
        if(thread[dse.tid].pendMemAcc > 0)
          thread[dse.tid].pendMemAcc--;
        if(thread[dse.tid].pendMemAcc == 0) begin
          thread[dse.tid].pendLoad = 0;
          thread[dse.tid].pendStore = 0;
        end
      end
      else if(dse.cancel) begin
        thread[dse.tid].retrieve_pc(v.pcStages[STAGE_ISE_VWB]);
        if(dse.exp)
          enter_exp(dse.tid, exp_dse_err);
      end
      if(dse.msgWait) begin
        thread[dse.tid].msg_wait();
      end
    end
    
    ///update no_* for issue & check
    if(v.fmSPA != null)
      noFu = v.fmSPA.noFu;
      
    if(v.fmDSE[STAGE_ISE_DEM] != null) begin
      tr_dse2ise dse = v.fmDSE[STAGE_ISE_DEM];
      noLd = dse.noLd;
      noSt = dse.noSt;
      noSMsg = dse.noSMsg;
      noRMsg = dse.noRMsg;
    end
    
    ///check & issue, cancel condition 3, ise decode Err, priv enter, uncond branch
    ovm_report_info("iinf", $psprintf("\n%s", sprint(printer)), OVM_HIGH);
    for(int i = 1; i <= NUM_THREAD; i++) begin
      uchar tid = i + v.TIdIssueLast;
      tid = tid & ~('1 << BITS_TID);
      
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
        if(thread[i].iBuf.size() >= thread[i].IGrpBytes) begin
          thread[i].decode_igrp();
          break;
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
      tid = tid & ~('1 << BITS_TID);
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
    vn.fmDSE[STAGE_ISE_DEM] = req;
    return 1;
  endfunction : nb_transport_dse
    
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
        
    ife_tr_port = new("ife_tr_port", this);
    rfm_tr_port = new("rfm_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
        
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
