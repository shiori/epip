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

class ip4_tlm_ise_sr extends ovm_object;
  uint EBase;   ///SR[4]
  
  `ovm_object_utils_begin(ip4_tlm_ise_sr)
    `ovm_field_int(EBase, OVM_ALL_ON)
  `ovm_object_utils_end

  function new(string name = "ise_sr");
   super.new(name);
   EBase = CFG_START_ADR;
  endfunction
endclass

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fmSPU;
  tr_rfm2ise fmRFM;
  tr_ife2ise fmIFE;
  tr_spa2ise fmSPA;
  tr_dse2ise fmDSE[stage_ise_vwbp:stage_ise_dc];
  
  tr_ise2rfm rfm[stage_ise:1];
  tr_ise2spa spa[stage_ise:1];
  tr_ise2spu spu[stage_ise:1];
  tr_ise2dse dse[stage_ise:1];
  
  uchar TIdIssueLast, TIdFetchLast;
  bit cancel[NUM_THREAD];
  ip4_tlm_ise_sr SR;
  uint PCStages[stage_ise_vwb:1];
    
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
    `ovm_field_object(SR, OVM_ALL_ON)
    `ovm_field_sarray_int(PCStages, OVM_ALL_ON)
  `ovm_component_utils_end

  function new(string name, ovm_component parent);
    super.new(name, parent);
    TIdFetchLast = 0;
    TIdIssueLast = 0;
    SR = new(); 
    print_enabled = 0;
  endfunction : new
endclass : ip4_tlm_ise_vars

class ise_thread_inf extends ovm_component;
  ise_thread_state threadState;
  uchar IBuf[$];
  bit DSEVec;
  uchar IGrpBytes, adrPkgBytes, numImms,
        cntSrfRd, cntVrfRd, cntDSERd;
  word imms[NUM_BP_IMM];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS], vrfGrp[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_SRF_BKS], srfGrp[CYC_VEC][NUM_SRF_BKS];
  bit vrfRdEn[CYC_VEC][NUM_VRF_BKS], srfRdEn[CYC_VEC][NUM_SRF_BKS];
  uchar cntPRWr, cntVrfWr[NUM_VRF_BKS], cntSrfWr[NUM_SRF_BKS];
  
  bit enSPU, enDSE, enVec, enFu[NUM_FU];
  bit privMode,  ///privilege running status
      decoded,
      decodeErr,
      cancel;
  uchar WCnt[NUM_W_CNT], WCntNext, WCntSel, vecMode;
  
  uchar vrfMap[NUM_INST_VRF/NUM_PRF_P_GRP], 
        srfMap[NUM_INST_SRF/NUM_PRF_P_GRP];
  uchar pendIFetch;
  
  inst_c ISPU, IDSE, IFu[NUM_FU];
  uint PC, PCBr;
  bit brPred;
    
  `ovm_component_utils_begin(ise_thread_inf)
    `ovm_field_enum(ise_thread_state, threadState, OVM_ALL_ON)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decodeErr, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
    `ovm_field_int(privMode, OVM_ALL_ON)
    `ovm_field_queue_int(IBuf, OVM_ALL_ON)
    `ovm_field_int(WCntSel, OVM_ALL_ON)
    `ovm_field_int(IGrpBytes, OVM_ALL_ON)
    `ovm_field_int(adrPkgBytes, OVM_ALL_ON)
    `ovm_field_int(numImms, OVM_ALL_ON)
    `ovm_field_int(cntSrfRd, OVM_ALL_ON)
    `ovm_field_int(cntVrfRd, OVM_ALL_ON)
    `ovm_field_int(cntDSERd, OVM_ALL_ON)
    `ovm_field_sarray_int(imms, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_sarray_int(enFu, OVM_ALL_ON)
    `ovm_field_int(enVec, OVM_ALL_ON)
    `ovm_field_sarray_int(WCnt, OVM_ALL_ON)
    `ovm_field_int(WCntNext, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(pendIFetch, OVM_ALL_ON)
    `ovm_field_int(PC, OVM_ALL_ON)
    `ovm_field_int(PCBr, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_sarray_int(vrfMap, OVM_ALL_ON)
    `ovm_field_sarray_int(srfMap, OVM_ALL_ON)
    `ovm_field_object(ISPU, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_object(IDSE, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_object(IFu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end

	virtual function void do_print(ovm_printer printer);
		super.do_print(printer);
	  if(get_report_verbosity_level() >= OVM_HIGH) begin
  		if(enSPU)
  		  printer.print_object("spu", ISPU);
  		if(enDSE)
  		  printer.print_object("dse", IDSE);
  		foreach(enFu[i])
  		  if(enFu[i])
  		    printer.print_object($psprintf("fu%0d", i), IFu[i]);
    end
	    
    `PAF2(vrfAdr, OVM_DEC)
    `PAF2(srfAdr, OVM_DEC)
    `PAF2(vrfGrp, OVM_DEC)
    `PAF2(srfGrp, OVM_DEC)
	endfunction : do_print
	
  function new(string name, ovm_component parent);
    super.new(name, parent);
    ISPU = new();
    IDSE = new();
    foreach(IFu[i])
      IFu[i] = new();
    threadState = ts_disabled;
    privMode = 0;
    PC = CFG_START_ADR;
    PCBr = CFG_START_ADR;
    vecMode = CYC_VEC - 1;
    decoded = 0;
    decodeErr = 0;
    print_enabled = 0;
  endfunction : new
 
  function void mapIAdr(input bit v, uchar orgAdr, output uchar grp, adr);
    uchar adrBits =  v ? (BITS_PRF_P_GRP - BITS_VRF_BKS) : (BITS_PRF_P_GRP - BITS_SRF_BKS);
    adr = orgAdr & ('1 << adrBits);
    grp = orgAdr >> adrBits;
    grp = v ? vrfMap[grp] : srfMap[grp];
  endfunction : mapIAdr

  function void cycNew();
    cancel = 0;
    foreach(WCnt[i])
      if(WCnt[i] != 0) WCnt[i]--;
  endfunction : cycNew

  function void exePriv();
  endfunction : exePriv
  
  function void decodeIGrpStart();
    i_gs0_t grpStart = IBuf[0];
    enSPU = 0;
    enDSE = 0;
    enVec = 0;
    enFu = '{default : 0};

    if(!grpStart.t) begin
      WCntSel = grpStart.chkGrp;
      adrPkgBytes = grpStart.adrPkgB;
      numImms = grpStart.immPkgW;
      DSEVec = grpStart.unitEn;
      IGrpBytes = 1 + adrPkgBytes + numImms * NUM_WORD_BYTES + NUM_INST_BYTES;
    end
    else begin
      i_gs1_u grpStart;
      uchar tmp = 0;
      foreach(grpStart.b[i])
        grpStart.b[i] = IBuf[i];
      foreach(grpStart.i.unitEn[i])
        tmp += grpStart.i.unitEn[i];
      if(tmp == 0) begin
        ovm_report_warning("decodeIGrpStart", "igs decode error, unitEn not valid");
        decodeErr = 1;
      end
      WCntSel = grpStart.i.chkGrp;
      adrPkgBytes = grpStart.i.adrPkgB;
      numImms = grpStart.i.immPkgW;
      IGrpBytes = 2 + adrPkgBytes + numImms * NUM_WORD_BYTES + tmp * NUM_INST_BYTES;
      enSPU = grpStart.i.unitEn[0];
      enDSE = grpStart.i.unitEn[1];
      DSEVec = grpStart.i.dv;
      foreach(enFu[i])
        enFu[i] = grpStart.i.unitEn[2+i];
    end
        
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [NUM_FU-1:0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = enFu[i];
        
      ovm_report_info("decodeIGrpStart",
        $psprintf("inst grp len %0d bytes includes: spu:%0b, dse:%0b, fu:%b. dv:%0b, WCntSel:%0b, adrPkgB:%0d, immPkgW:%0d", 
                   IGrpBytes, enSPU, enDSE, enFuTmp, DSEVec, WCntSel, adrPkgBytes, numImms),
        OVM_HIGH);
    end
  endfunction : decodeIGrpStart
    
  function void decodeIGrp();
    uchar tmp = 0;
    iga_t adrs[12];
    uchar offSet;
    i_gs0_t grpStart = IBuf[0];
    
    vrfRdEn = '{default : 0};
    srfRdEn = '{default : 0};
    cntVrfRd = 0;
    cntSrfRd = 0;
    cntDSERd = 0;
    cntVrfWr = '{default : 0};
    cntSrfWr = '{default : 0};
    cntPRWr = 0;
    WCntNext = 0;
        
    if(!grpStart.t) begin
      tmp = 1;
      offSet = 1;
      if(adrPkgBytes != 0) adrPkgBytes --;
      ISPU.setData(IBuf, offSet, 0, DSEVec);
      IDSE.setData(IBuf, offSet, 0, DSEVec);
      foreach(IFu[i])
        IFu[i].setData(IBuf, offSet, i, 1);
        
      offSet += NUM_INST_BYTES;
      ISPU.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
      ISPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
      ISPU.analyze_fu(enSPU, enDSE, enFu);
      adrs[0] = grpStart.a;
      if(adrPkgBytes) begin
        i_ap0_t AdrPkg = IBuf[offSet];
        foreach(adrs[i])
          adrs[i] = AdrPkg.a[i];
        offSet ++;
      end
    end
    else begin
      i_gs1_u grpStart;
      foreach(grpStart.b[i])
        grpStart.b[i] = IBuf[i];
      offSet = 2;
      tmp = 1;
      if(adrPkgBytes != 0) adrPkgBytes --;
      
      if(enSPU) begin
        ISPU.setData(IBuf, offSet, 0, 0);
        ISPU.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
        ISPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
        offSet += NUM_INST_BYTES;
      end
      
      if(enDSE) begin
        IDSE.setData(IBuf, offSet, 0, DSEVec);
        IDSE.analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
        IDSE.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
        offSet += NUM_INST_BYTES;
      end
      
      foreach(IFu[i])
        if(enFu[i]) begin
          IFu[i].setData(IBuf, offSet, i, 1);
          IFu[i].analyze_rs(vecMode, vrfRdEn, srfRdEn, cntVrfRd, cntSrfRd, cntDSERd);
          ISPU.analyze_rd(cntVrfWr, cntSrfWr, cntPRWr);
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
          AdrPkg.b[i] = IBuf[offSet];
          offSet++;
        end
        foreach(AdrPkg.i.a[i])
          adrs[tmp++] = AdrPkg.i.a[i];
        adrPkgBytes -= 3;
      end
      else if(adrPkgBytes >= 2) begin
        i_ap1_u AdrPkg;
        foreach(AdrPkg.b[i]) begin
          AdrPkg.b[i] = IBuf[offSet];
          offSet++;
        end
        foreach(AdrPkg.i.a[i])
          adrs[tmp++] = AdrPkg.i.a[i];
        adrPkgBytes -= 2;
      end
      else if(adrPkgBytes >= 1) begin
        i_ap0_t AdrPkg;
        AdrPkg = IBuf[offSet];
        offSet++;
        foreach(AdrPkg.a[i])
          adrs[tmp++] = AdrPkg.a[i];
        adrPkgBytes -= 1;
      end
    end
      
    for(int i = 0; i < numImms; i++) begin
      imms[i] = {IBuf[i+3], IBuf[i+2], IBuf[i+1], IBuf[i]};
      offSet += NUM_WORD_BYTES;
    end
      
    ///allocate reg read address
    tmp = 0;
    
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfRdEn[i][j]) begin
          mapIAdr(1, adrs[tmp], vrfGrp[i][j], vrfAdr[i][j]);
          tmp++;
        end

      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfRdEn[i][j]) begin
          mapIAdr(0, adrs[tmp], srfGrp[i][j], srfAdr[i][j]);
          tmp++;
        end
    end
    
    foreach(IFu[fid]) begin
      IFu[fid].set_wcnt(WCntNext);
      decodeErr |= IFu[fid].decodeErr;
    end
    ISPU.set_wcnt(WCntNext);
    IDSE.set_wcnt(WCntNext);
    decodeErr |= ISPU.decodeErr;
    decodeErr |= IDSE.decodeErr;
    
    decoded = 1;
    ovm_report_info("decodeIGrp", {"\n", sprint()}, OVM_HIGH);
  endfunction : decodeIGrp

  function void flush();
    IBuf = {};
    IGrpBytes = 0;
    decoded = 0;
    decodeErr = 0;
    pendIFetch = 0;
    threadState = ts_rdy;
  endfunction : flush
  
  function void retrievePC(uint adr);
    flush();
    PC = adr;
    cancel = 1;
   endfunction : retrievePC

  function void msgWait();
  endfunction : msgWait
  
  function bit brPreMiss(input bit Br);
    if(threadState == ts_w_b)
      threadState = ts_rdy;
    
    if(Br != brPred) begin
      flush();
      PC = PCBr;
      cancel = 1;
      return 1;
    end
    else
      return 0;
  endfunction : brPreMiss

  function bit canReqIFetch();
    ovm_report_info("canReqIFetch", $psprintf("threadState:%s, IBuf lv:%0d, pd:%0d", threadState.name, IBuf.size(), pendIFetch), OVM_HIGH);
    if(threadState == ts_disabled)
      return 0;
    if(IBuf.size() + pendIFetch * NUM_IFET_BYTES >=  NUM_IBUF_BYTES)
      return 0;
    if(IGrpBytes == 0)
      return 1;
    if(IBuf.size() < IGrpBytes)
      return 1;
    return 0;
  endfunction : canReqIFetch
      
  function void updateInst(input inst_fg_c fetchGrp);
    uchar offSet = 0, LvlLast = IBuf.size();
    if(LvlLast  >= NUM_MAX_IGRP_BYTES)
      ovm_report_warning("ise", "IBuf overflow!");
    if(LvlLast == 0) ///only calculate offSet when IBuf size is reset to 0
      offSet = PC & ~{'1 << BITS_IFET};

    if(pendIFetch > 0)
      pendIFetch--;
    
    if(cancel) begin
      ovm_report_info("updateInst", $psprintf("cancel, PC:0x%0h, offSet:%0h, pd:%0d", PC, offSet, pendIFetch), OVM_HIGH);
      return;
    end
      
    foreach(fetchGrp.data[i])
      if(i >= offSet)
        IBuf.push_back(fetchGrp.data[i]);

    ovm_report_info("updateInst", $psprintf("PC:0x%0h, offSet:%0h, pd:%0d, IBuf lv %0d->%0d", PC, offSet, pendIFetch, LvlLast, IBuf.size()), OVM_HIGH);
  endfunction : updateInst

  function void fillIFE(input tr_ise2ife ife);
    ife.FetchReq = 1;
    ife.PC = (PC + NUM_IFET_BYTES * pendIFetch) & ~{'1 << BITS_IFET};
    pendIFetch++;
  endfunction : fillIFE
  
  function void fillIssue(ref tr_ise2rfm CiRFM[CYC_VEC], tr_ise2spa CiSPA[CYC_VEC], 
                               tr_ise2spu CiSPU[CYC_VEC], tr_ise2dse CiDSE[CYC_VEC]);
    
    if(CiRFM[0] == null) CiRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    if(CiRFM[cntVrfRd] == null) CiRFM[cntVrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
    CiRFM[0].start = 1;
    CiRFM[cntVrfRd].vec_end = 1;

    foreach(IFu[i])
      IFu[i].map_wr_grp(vrfMap, srfMap);

    /// spu or scalar dse issue
    if(enSPU) begin
      bit br_dep_dse = 0, br_dep_spa = 0;
      ISPU.map_wr_grp(vrfMap, srfMap);
      if(CiRFM[0] == null) CiRFM[0] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(CiRFM[cntSrfRd] == null) CiRFM[cntSrfRd] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(CiSPU[cntSrfRd] == null) CiSPU[cntSrfRd] = tr_ise2spu::type_id::create("toSPU", get_parent());
      CiRFM[0].start = 1;
      CiRFM[cntSrfRd].scl_end = 1;
      CiSPU[cntSrfRd].start = 1;
      if(ISPU.is_br() && !ISPU.is_unc_br()) begin ///a conditional branch need vecMode cycles
        cntSrfRd = vecMode;
        ///find what br is depend on
        foreach(enFu[fid])
          if(enFu[fid] && IFu[fid].op inside {op_cmp, op_ucmp})
            br_dep_spa = 1;
        if(enDSE)
          br_dep_dse = !br_dep_spa;
      end
      CiSPU[cntSrfRd].br_end = ISPU.is_br();
      for(int i = 0; i <= cntSrfRd; i++) begin
        if(CiRFM[i] == null) CiRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(CiSPA[i] == null) CiSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
        ISPU.fill_rfm(CiRFM[i], i);
        ISPU.fill_spu(CiSPU[i]);
        CiSPU[i].br_dep_spa = br_dep_spa;
        CiSPU[i].br_dep_dse = br_dep_dse;
      end
    end
    
    if(enDSE) begin
      IDSE.map_wr_grp(vrfMap, srfMap);
      CiDSE[0] = tr_ise2dse::type_id::create("toDSE", get_parent());
      IDSE.fill_dse(CiDSE[0]);      
      for(int i = 0; i <= cntDSERd; i++) begin
        if(CiRFM[i] == null) CiRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
        if(CiSPU[i] == null) CiSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
        IDSE.fill_rfm(CiRFM[i], i);
        IDSE.fill_spu(CiSPU[i]);
      end
    end
          
    for(int i = 0; i <= cntSrfRd || i <= cntVrfRd; i++) begin
      if(CiRFM[i] == null) CiRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      CiRFM[i].bpImm = imms;
      CiRFM[i].VrfRdGrp = vrfGrp[i];
      CiRFM[i].VrfRdAdr = vrfAdr[i];
      CiRFM[i].SrfRdGrp = srfGrp[i];
      CiRFM[i].SrfRdAdr = srfAdr[i];
    end
    
    for(int i = 0; i <= cntVrfRd; i++) begin
      if(CiSPA[i] == null) CiSPA[i] = tr_ise2spa::type_id::create("toSPA", get_parent());
      if(CiRFM[i] == null) CiRFM[i] = tr_ise2rfm::type_id::create("toRFM", get_parent());
      if(CiSPU[i] == null) CiSPU[i] = tr_ise2spu::type_id::create("toSPU", get_parent());
      if(enDSE)
        IDSE.fill_spa(CiSPA[i]);
      foreach(IFu[fid])
        if(enFu[fid]) begin
          IFu[fid].fill_rfm(CiRFM[i], i);
          IFu[fid].fill_spa(CiSPA[i]);
          IFu[fid].fill_spu(CiSPU[i]);
        end
      CiSPA[i].vecMode = vecMode;
      CiSPU[i].vecMode = vecMode;
      CiRFM[i].Cyc = i;
      CiSPA[i].subVec = i;
      CiSPU[i].subVec = i;
    end
  endfunction : fillIssue

endclass : ise_thread_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;

  local uchar cntVrfRd, cntSrfRd, cntDSERd, cnt_vec_proc,
              cntPRWr, cntSrfWr[NUM_SRF_BKS], cntVrfWr[NUM_VRF_BKS];
        
  local bit NoLd, NoSt, NoSMsg, NoRMsg, NoFu[NUM_FU];
  
  local tr_ise2rfm CiRFM[CYC_VEC];
  local tr_ise2spa CiSPA[CYC_VEC];
  local tr_ise2spu CiSPU[CYC_VEC];
  local tr_ise2dse CiDSE[CYC_VEC];
  
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
  local ise_thread_inf tinf[NUM_THREAD];
  local ip4_printer printer;
  
  `ovm_component_utils_begin(ip4_tlm_ise)
    `ovm_field_int(cnt_vec_proc, OVM_ALL_ON)
    `ovm_field_int(cntVrfRd, OVM_ALL_ON)
    `ovm_field_int(cntSrfRd, OVM_ALL_ON)
    `ovm_field_int(cntDSERd, OVM_ALL_ON)
    `ovm_field_int(NoLd, OVM_ALL_ON)
    `ovm_field_int(NoSt, OVM_ALL_ON)
    `ovm_field_int(NoSMsg, OVM_ALL_ON)
    `ovm_field_int(NoRMsg, OVM_ALL_ON)
    `ovm_field_sarray_int(NoFu, OVM_ALL_ON)
    `ovm_field_int(cntPRWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntSrfWr, OVM_ALL_ON)
    `ovm_field_sarray_int(cntVrfWr, OVM_ALL_ON)
  `ovm_component_utils_end
  
  function void enterExp(input uchar TId, ise_exp_t Err);
    ise_thread_inf TInf = tinf[TId];
    TInf.privMode = 1;
    TInf.cancel = 1;
    TInf.PC = v.SR.EBase;
    case(Err)
    exp_decode_err  : begin end
    exp_dse_err     : begin end
    exp_priv_err    : begin end
    exp_msc_err     : begin end
    endcase
    TInf.flush();
  endfunction : enterExp
  
  function void exePriv(input inst_c I);
  endfunction : exePriv

  function bit canIssue(input uchar TId);
    /// the vec value indicate 4 Cyc issue style is needed
///    vec = TInf.DSEVec;
    ise_thread_inf TInf = tinf[TId];
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [NUM_FU-1:0] enFuTmp;
      foreach(enFuTmp[i])
        enFuTmp[i] = TInf.enFu[i];
        
      ovm_report_info("canIssue",
        $psprintf("threadState:%s, decoded:%0d, Err:%0d, WCnt:%0d, PC:%0h spu:%0b, dse:%0b, fu:%b. dv:%0b, WCntSel:%0b", 
                   TInf.threadState.name, TInf.decoded, TInf.decodeErr, TInf.WCnt[TInf.WCntSel], TInf.PC, TInf.enSPU, TInf.enDSE,
                   enFuTmp, TInf.DSEVec, TInf.WCntSel),
        OVM_HIGH);
    end
    
    if(TInf.enVec && cnt_vec_proc >= TInf.vecMode)
      return 0;
      
    ///issue disable check
    if(TInf.IDSE.dse_block(NoLd, NoSt, NoSMsg, NoRMsg))
      return 0;
    
    foreach(NoFu[i])
      if(NoFu[i] && TInf.enFu[i])
        return 0;
    
    ///read Cyc check
    if(cntSrfRd > 0 && TInf.cntSrfRd > 0)
      return 0;

    if(cntVrfRd > 0 && TInf.cntVrfRd > 0)
      return 0;

    if(cntDSERd > 0 && TInf.cntDSERd > 0)
      return 0;

    /// write buf overflow check
    if(cntPRWr + TInf.cntPRWr > CYC_VEC)
      return 0;
    
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] + TInf.cntVrfWr[i] > CYC_VEC)
        return 0;      

    foreach(cntSrfWr[i])
      if(cntSrfWr[i] + TInf.cntSrfWr[i] > CYC_VEC)
        return 0;
    return TInf.decoded && (TInf.threadState == ts_rdy && TInf.WCnt[TInf.WCntSel] == 0);
  endfunction : canIssue

  function void issue(input uchar TId);
    ise_thread_inf TInf = tinf[TId];
    vn.PCStages[1] = TInf.PC;
    if(TInf.decodeErr) begin
      enterExp(TId, exp_decode_err);
      return;
    end
    
    TInf.IBuf = TInf.IBuf[TInf.IGrpBytes:$];
    TInf.PC += TInf.IGrpBytes;
    TInf.fillIssue(CiRFM, CiSPA, CiSPU, CiDSE);
    TInf.decoded = 0;
    TInf.decodeErr = 0;
    
    if(TInf.WCntNext > TInf.WCnt[TInf.WCntSel])
      TInf.WCnt[TInf.WCntSel] = TInf.WCntNext;
      
    if(TInf.enSPU) 
      if(TInf.ISPU.is_unc_br()) begin
        TInf.PC = TInf.PC + TInf.ISPU.offSet;
        TInf.flush();
        TInf.cancel = 1;
      end
      else if(TInf.ISPU.is_br()) begin
        TInf.threadState = ts_w_b;
        TInf.PCBr = TInf.PC + TInf.ISPU.offSet;
        TInf.brPred = 0;
      end
    
    /// spu or scalar dse issue
    if(TInf.enSPU) begin
      if(TInf.ISPU.is_priv()) begin
        if(TInf.privMode) begin
          exePriv(TInf.ISPU);
          TInf.exePriv();
        end
        else
          enterExp(TId, exp_priv_err);
      end
    end
    
    if(TInf.enDSE) begin
      cntDSERd = TInf.cntDSERd;
    end
    
    cntSrfRd = TInf.cntSrfRd;
    cntVrfRd = TInf.cntVrfRd;
    cntDSERd = TInf.cntDSERd;
    cntPRWr += TInf.cntPRWr;
  
    foreach(cntVrfWr[i])
      cntVrfWr[i] += TInf.cntVrfWr[i];

    foreach(cntSrfWr[i])
      cntSrfWr[i] += TInf.cntSrfWr[i];
    
    if(TInf.enVec)
      cnt_vec_proc = TInf.vecMode;
  endfunction : issue
      
  function void combProc();
    ovm_report_info("ise", "combProc procing...", OVM_FULL); 
    
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmRFM != null) end_tr(v.fmRFM);
    if(v.fmIFE != null) end_tr(v.fmIFE);
    if(v.fmDSE[stage_ise_dc] != null) end_tr(v.fmDSE[stage_ise_dc]);
    
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmRFM = null;
    vn.fmIFE = null;
    vn.fmDSE[stage_ise_dc] = null;
    
    for(int i = stage_ise; i > 1; i--) begin
      vn.rfm[i] = v.rfm[i-1];  
      vn.spa[i] = v.spa[i-1];
      vn.spu[i] = v.spu[i-1];
      vn.dse[i] = v.dse[i-1];
    end
    vn.rfm[1] = null; 
    vn.spa[1] = null;
    vn.spu[1] = null;
    vn.dse[1] = null;
    
    for(int i = stage_ise_vwb; i > 1; i--)
      vn.PCStages[i] = v.PCStages[i-1];
      
    foreach(tinf[i])
      tinf[i].cycNew();
    
    vn.cancel = '{default : 0};
    
    for(int i = 0; i < (CYC_VEC - 1); i++) begin
      CiRFM[i] = CiRFM[i+1];
      CiSPA[i] = CiSPA[i+1];
      CiSPU[i] = CiSPU[i+1];
      CiDSE[i] = CiDSE[i+1];
    end

    CiRFM[CYC_VEC-1] = null;
    CiSPA[CYC_VEC-1] = null;
    CiSPU[CYC_VEC-1] = null;
    CiDSE[CYC_VEC-1] = null;
    
    if(cntVrfRd != 0) cntVrfRd--;
    if(cntDSERd != 0) cntDSERd--;
    if(cntSrfRd != 0) cntSrfRd--;
    if(cntPRWr != 0) cntPRWr--;
    if(cnt_vec_proc != 0) cnt_vec_proc--;
    
    foreach(cntSrfWr[i])
      if(cntSrfWr[i] != 0) cntSrfWr[i]--;
    foreach(cntVrfWr[i])
      if(cntVrfWr[i] != 0) cntVrfWr[i]--;
    
    NoFu = '{default: 0};
    NoLd = 0;
    NoSt = 0;
    NoSMsg = 0;
    NoRMsg = 0;

    for(int i = stage_ise_vwbp; i > stage_ise_dc; i--)
      vn.fmDSE[i] = v.fmDSE[i-1];  
          
    ///cancel condition 1 branch mispredication, msc overflow
    if(v.fmSPU != null && v.fmSPU.br_rsp) begin
      bit cancel;
      cancel = tinf[v.fmSPU.TId].brPreMiss(v.fmSPU.br_taken);
      if(v.fmSPU.msc_top_chg) begin
        cancel = 1;
        enterExp(v.fmIFE.TId, exp_msc_err);
      end
      if(v.fmIFE != null && cancel && v.fmIFE.TId == v.fmSPU.TId)
        v.fmIFE.instEn = 0;
    end
    
    ///cancel condition 2, spa exp
    if(v.fmSPA != null && v.fmSPA.exp) begin
      tinf[v.fmSPA.TId].retrievePC(v.PCStages[stage_ise_vwb]);
    end
    
    ///cancel condition 2 dse exp or cache miss
    if(v.fmDSE[stage_ise_dc] != null) begin
      tr_dse2ise dse = v.fmDSE[stage_ise_dc];
      if(dse.cancel) begin
        tinf[dse.TId].retrievePC(v.PCStages[stage_ise_vwb]);
        if(dse.exp)
          enterExp(dse.TId, exp_dse_err);
      end
      if(dse.msgWait) begin
        tinf[dse.TId].msgWait();
      end
    end
    
    ///update no_* for issue & check
    if(v.fmSPA != null)
      NoFu = v.fmSPA.NoFu;
      
    if(v.fmDSE[stage_ise_dc] != null) begin
      tr_dse2ise dse = v.fmDSE[stage_ise_dc];
      NoLd = dse.NoLd;
      NoSt = dse.NoSt;
      NoSMsg = dse.NoSMsg;
      NoRMsg = dse.NoRMsg;
    end
    
    ///check & issue, cancel condition 3, ise decode Err, priv enter, uncond branch
    ovm_report_info("iinf", $psprintf("\n%s", sprint(printer)), OVM_HIGH);
    for(int i = 1; i <= NUM_THREAD; i++) begin
      uchar TId = i + v.TIdIssueLast;
      TId = TId & ~('1 << bits_tid);
      
      ovm_report_info("issue", $psprintf("checking thread %0d", TId), OVM_HIGH);
      if(canIssue(TId)) begin
        ovm_report_info("issue", $psprintf("issuing thread %0d", TId), OVM_HIGH);
        issue(TId);
        vn.TIdIssueLast = TId;
        break;
      end
    end

    ///cancel from one cycle delayed
    if(v.fmIFE != null && v.cancel[v.fmIFE.TId])
      v.fmIFE = null;
      
    ///update ife data into tinf
    if(v.fmIFE != null && v.fmIFE.instEn)
      tinf[v.fmIFE.TId].updateInst(v.fmIFE.fetchGrp);
    
    ///try to decode one inst grp
    foreach(tinf[i])
      if(tinf[i].threadState != ts_disabled && tinf[i].IBuf.size() > 1 && !tinf[i].decoded) begin
        tinf[i].decodeIGrpStart();
        if(tinf[i].IBuf.size() >= tinf[i].IGrpBytes) begin
          tinf[i].decodeIGrp();
          break;
        end
      end
  endfunction
  
  function void reqProc();
    tr_ise2rfm toRFM;
    tr_ise2spu toSPU;
    tr_ise2spa toSPA;
    tr_ise2ife toIFE;
    tr_ise2dse toDSE;
    
    ovm_report_info("ise", "reqProc procing...", OVM_FULL); 
    
    vn.rfm[1] = CiRFM[0];
    vn.spa[1] = CiSPA[0];
    vn.spu[1] = CiSPU[0];
    vn.dse[1] = CiDSE[0];  
        
    toRFM = v.rfm[stage_ise];
    toSPA = v.spa[stage_ise];
    toSPU = v.spu[stage_ise];
    toDSE = v.dse[stage_ise];
    
    ///ife req search
    for(int i = 1; i <= NUM_THREAD; i++) begin
      uchar TId = i + v.TIdFetchLast;
      TId = TId & ~('1 << bits_tid);
      if(tinf[TId].canReqIFetch()) begin
        toIFE = tr_ise2ife::type_id::create("toIFE", this);
        tinf[TId].fillIFE(toIFE);
        toIFE.TId = TId;
        vn.TIdFetchLast = TId;
        break;
      end
    end
    
    ///delay cancel one cycle
    foreach(tinf[i])
      if(tinf[i].cancel) begin
        if(toIFE == null) toIFE = tr_ise2ife::type_id::create("toIFE", this);
        toIFE.cancel[i] = 1;
        vn.cancel[i] = 1;
      end
    
    ///send dse cancel to spa
    if(v.fmDSE[stage_ise_vwbp] != null && v.fmDSE[stage_ise_vwbp].cancel) begin
      tr_dse2ise dse = v.fmDSE[stage_ise_vwbp];
      if(toSPA == null) toSPA = tr_ise2spa::type_id::create("toSPA", this);
      toSPA.cancel[dse.TId] = 1;
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
    ovm_report_info("ISE_TR", $psprintf("Get ife Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    end_tr(req);
    rsp = req;
    vn.fmIFE = req;
    return 1;
  endfunction : nb_transport_ife

  function bit nb_transport_spu(input tr_spu2ise req, output tr_spu2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2ise req, output tr_spa2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa
  
  function bit nb_transport_rfm(input tr_rfm2ise req, output tr_rfm2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_dse(input tr_dse2ise req, output tr_dse2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE[stage_ise_dc] = req;
    return 1;
  endfunction : nb_transport_dse
    
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time==stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
    ///--------------------synchronizing-------------------
    v.copy(vn);
    combProc();
  endfunction : sync

  task run();
    forever begin
      @(posedge sysif.clk);
      sync();
      reqProc();
    end
  endtask : run

  function new(string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
    
  virtual function void build();
    ovm_object tmp;
    tlm_vif_object vif_cfg;
    
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
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
    
    foreach(tinf[i])
      tinf[i] = new($psprintf("tinf%0d", i), this);
    tinf[0].threadState = ts_rdy;
    tinf[0].privMode = 1;
    
    cntVrfRd = 0;
    cntSrfRd = 0;
    cntDSERd = 0;
    cntPRWr = 0;
    cntSrfWr = '{default: 0};
    cntVrfWr = '{default: 0};
    
    printer = new();
    printer.knobs.depth = 1;
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
