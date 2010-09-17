/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_spu.sv
/// Title            : ip4 special processing unit
/// Version          : 0.1
/// Last modified    : Apr 9 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Apr 9 2010
  
class ip4_tlm_spu_vars extends ovm_component;
  tr_ise2spu fmISE[STAGE_RRF_VWB_END:0];
  tr_rfm2spu fmRFM[STAGE_RRF_VWB:STAGE_RRF_EXS0];
  tr_spa2spu fmSPA[STAGE_RRF_VWB:STAGE_RRF_CEM];
  tr_dse2spu fmDSE[STAGE_RRF_VWB:STAGE_RRF_DEM];
  tr_tlb2spu fmTLB;
  tr_eif2spu fmEIF;
  tr_spu2rfm rfm[STAGE_RRF_SWBP:STAGE_RRF_EXS1];
  
  `ovm_component_utils_begin(ip4_tlm_spu_vars)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_spu_vars

///---------------------------------------main component----------------------------------------

class ip4_tlm_spu extends ovm_component;
    
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local ip4_tlm_spu_vars v, vn;
  local bit ilm[NUM_THREAD][CYC_VEC][NUM_SP];
  local bit cm[NUM_THREAD][CYC_VEC][NUM_SP];
  local word msc[NUM_THREAD][CYC_VEC][NUM_SP];
  local bit mscGuard[NUM_THREAD][CYC_VEC][NUM_SP];
  local bit pr[NUM_THREAD][NUM_PR:1][CYC_VEC][NUM_SP];
  local bit prSPU[STAGE_RRF_SWBP:STAGE_RRF_EXS1];
  local bit[STAGE_RRF_WSR:0] cancel[NUM_THREAD];
  local bit expFu, expMSC, brCancel, emskAllZero;
  local bit cmNext[CYC_VEC:0][NUM_SP],
            ilmNext[CYC_VEC:0][NUM_SP],
            mscOF[CYC_VEC:0][NUM_SP],
            mscUF[CYC_VEC:0][NUM_SP],
            stkWEn[CYC_VEC:0],
            mskWEn[CYC_VEC:0];
  local word mscNext[CYC_VEC][NUM_SP];
  
  `ovm_component_utils_begin(ip4_tlm_spu)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2spu, tr_ise2spu, ip4_tlm_spu) ise_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2spu, tr_spa2spu, ip4_tlm_spu) spa_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2spu, tr_rfm2spu, ip4_tlm_spu) rfm_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2spu, tr_dse2spu, ip4_tlm_spu) dse_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2spu, tr_tlb2spu, ip4_tlm_spu) tlb_tr_imp;
  ovm_nonblocking_transport_imp_eif #(tr_eif2spu, tr_eif2spu, ip4_tlm_spu) eif_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_spu2rfm, tr_spu2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2ise, tr_spu2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2spa, tr_spu2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2dse, tr_spu2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2tlb, tr_spu2tlb) tlb_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2eif, tr_spu2eif) eif_tr_port;
  
  function void comb_proc();
    ovm_report_info("spu", "comb_proc procing...", OVM_FULL); 
    foreach(cancel[i])
      cancel[i] = cancel[i] << 1;
    
    ///self cancel
    if(v.fmISE[STAGE_RRF_EPS] != null && expFu)
      cancel[v.fmISE[STAGE_RRF_EPS].tid] |= `GML(STAGE_RRF_EPS);
    if(v.fmISE[STAGE_RRF_CBR] != null && brCancel)
      cancel[v.fmISE[STAGE_RRF_CBR].tid] |= `GML(STAGE_RRF_CBR);
    brCancel = 0;
    expFu = 0;
    
    ///spa request canceling
    if(v.fmSPA[STAGE_RRF_CEM] != null && v.fmSPA[STAGE_RRF_CEM].cancel)
      cancel[v.fmSPA[STAGE_RRF_CEM].tid] |= `GML(STAGE_RRF_WSR);
    
    ///dse request canceling
    if(v.fmDSE[STAGE_RRF_DEM] != null && v.fmDSE[STAGE_RRF_DEM].cancel)
      cancel[v.fmDSE[STAGE_RRF_DEM].tidCancel] |= `GML(STAGE_RRF_WSR);    

    for(int i = CYC_VEC; i > 0; i--) begin
      cmNext[i] = cmNext[i - 1];
      ilmNext[i] = ilmNext[i - 1];
      mscNext[i] = mscNext[i - 1];
      mscOF[i] = mscOF[i- 1];
      mscOF[i] = mscOF[i - 1];
      stkWEn[i] = stkWEn[i - 1];
      mskWEn[i] = mskWEn[i - 1];
    end
    stkWEn[0] = 0;
    mskWEn[0] = 0;
    mscOF[0] = '{default : 0};
    mscOF[0] = '{default : 0};
    
    for(int i = STAGE_RRF_VWB_END; i > 0; i--)
      vn.fmISE[i] = v.fmISE[i - 1];
      
    for(int i = STAGE_RRF_VWB; i > STAGE_RRF_EXS0; i--)
      vn.fmRFM[i] = v.fmRFM[i - 1];

    for(int i = STAGE_RRF_VWB; i > STAGE_RRF_CEM; i--)
      vn.fmSPA[i] = v.fmSPA[i - 1];

    for(int i = STAGE_RRF_VWB; i > STAGE_RRF_DEM; i--)
      vn.fmDSE[i] = v.fmDSE[i - 1];
            
    for(int i = STAGE_RRF_SWBP; i > STAGE_RRF_EXS1; i--)
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_EXS1] = null;

    for(int i = STAGE_RRF_SWBP; i > STAGE_RRF_EXS1; i--)
      prSPU[i] = prSPU[i - 1];
    prSPU[STAGE_RRF_EXS1] = 0;
          
    if(v.fmISE[0] != null) end_tr(v.fmISE[0]);
    if(v.fmRFM[STAGE_RRF_EXS0] != null) end_tr(v.fmRFM[STAGE_RRF_EXS0]);
    if(v.fmSPA[STAGE_RRF_CEM] != null) end_tr(v.fmSPA[STAGE_RRF_CEM]);
    if(v.fmDSE[STAGE_RRF_DEM] != null) end_tr(v.fmDSE[STAGE_RRF_DEM]);
    
    vn.fmISE[0] = null;
    vn.fmRFM[STAGE_RRF_EXS0] = null;
    vn.fmSPA[STAGE_RRF_CEM] = null;
    vn.fmDSE[STAGE_RRF_DEM] = null;
  endfunction
  
  function void req_proc();
    tr_spu2rfm toRFM;
    tr_spu2ise toISE;
    tr_spu2spa toSPA;
    tr_spu2dse toDSE;
    tr_spu2tlb toTLB;
    tr_spu2eif toEIF;
    
    ovm_report_info("spu", "req_proc procing...", OVM_FULL); 
    
    ///--------------prepare---------------------------------
    toRFM = v.rfm[STAGE_RRF_SWBP];
    
    ///----------process data---------------------
    ///update msc & msk
    if(v.fmISE[STAGE_RRF_CEM + CYC_VEC] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM + CYC_VEC];
      if(mskWEn[CYC_VEC]) begin
        ovm_report_info("update mask", $psprintf("tid: %0d, subVec: %0d, ", ise.tid, ise.subVecSPU), OVM_HIGH);
        ilm[ise.tid][ise.subVecSPU] = ilmNext[CYC_VEC];
        cm[ise.tid][ise.subVecSPU] = cmNext[CYC_VEC];
        for(int i = 0; i < NUM_SP; i++)
          ovm_report_info("update mask", $psprintf("ilm: %0d, cm: %0d", ilmNext[CYC_VEC][i], cmNext[CYC_VEC][i]), OVM_HIGH);
      end
      if(stkWEn[CYC_VEC]) begin
        ovm_report_info("update msc", $psprintf("tid: %0d, subVec: %0d, ", ise.tid, ise.subVecSPU), OVM_HIGH);
        msc[ise.tid][ise.subVecSPU] = mscNext[CYC_VEC];
        for(int i = 0; i < NUM_SP; i++)
          ovm_report_info("update msc", $psprintf("msc: %0d", mscNext[CYC_VEC][i]), OVM_HIGH);
      end
    end
          
    ///write back cmp predication register results
    if(v.fmSPA[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_spa2spu spa = v.fmSPA[STAGE_RRF_CEM];
      ovm_report_info("spu", "write back spa pres", OVM_FULL);
      pr[ise.tid][ise.prWrAdr0][ise.subVecFu] = spa.presCmp0;
      pr[ise.tid][ise.prWrAdr1][ise.subVecFu] = spa.presCmp1;
    end
    
    ///write back dse predication register results
    if(v.fmDSE[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_dse2spu dse = v.fmDSE[STAGE_RRF_CEM];
      ovm_report_info("spu", "write back dse pres", OVM_FULL);
      if(dse.wrEn)
        pr[ise.tid][ise.prWrAdr2][ise.subVecDSE] = dse.pres;
    end
        
    ///predication register read
    if(v.fmISE[STAGE_RRF_RRC] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC];
      foreach(toSPA.fu[fid]) begin
        if(!ise.enFu[fid]) continue;
        if(toSPA == null) toSPA = tr_spu2spa::type_id::create("toSPA", this);
        toSPA.fu[fid].emsk = ise.prRdAdr[fid] == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdr[fid]][ise.subVecFu];
        if(ise.prInv[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = !toSPA.fu[fid].emsk[i];
        if(!ise.prNMsk[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = toSPA.fu[fid].emsk[i] && ilm[ise.tid][ise.subVecFu][i] && cm[ise.tid][ise.subVecFu][i];
      end
    end
    
    if(v.fmISE[STAGE_RRF_RRC0] != null && v.fmISE[STAGE_RRF_RRC0].enDSE) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC0];
      toDSE = tr_spu2dse::type_id::create("toDSE", this);
      
      if(ise.sclDSE && ise.subVecDSE == 0)
        toDSE.emsk[0] = 1;
      else
        toDSE.emsk = ise.prRdAdrDSE == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdrDSE][ise.subVecDSE];
        
      foreach(toDSE.emsk[i]) begin
        if(ise.prInvDSE)
          toDSE.emsk[i] = !toDSE.emsk[i];
        if(!ise.prNMskDSE)
          toDSE.emsk[i] = toDSE.emsk[i] && ilm[ise.tid][ise.subVecDSE][i] && cm[ise.tid][ise.subVecDSE][i];
      end
      toDSE.emsk = ise.prRdAdrDSE == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdrDSE][ise.subVecDSE];
    end
    
    ///scalar dse enable
    if(v.fmISE[STAGE_RRF_RRC] != null && v.fmISE[STAGE_RRF_RRC].enDSE) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC];
      bit res = 0;
      if(toDSE) toDSE = tr_spu2dse::type_id::create("toDSE", this);
      if(ise.sclDSE) begin
        for(int subVec = 0; subVec <= ise.vecModeDSE; subVec++) begin
          bit tmp[NUM_SP];
          tmp = ise.prRdAdrDSE == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdrDSE][subVec];
          foreach(tmp[i]) begin
            if(ise.prInvDSE)
              res |= !tmp[i];
            if(!ise.prNMskDSE)
              toDSE.emsk[i] = tmp[i] && ilm[ise.tid][subVec][i] && cm[ise.tid][subVec][i];
          end
        end
      end
      toDSE.sclEn = res;
    end
        
    ///processing normal spu instructions
    if(v.fmISE[STAGE_RRF_EXS0] != null && v.fmISE[STAGE_RRF_EXS0].enSPU) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_EXS0];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_EXS0];
      bit[WORD_BITS:0] op0, op1, r0;
      word o0, o1;
      bit prTmp[CYC_VEC][NUM_SP];
      bit exp;
      
      if(rfm != null) begin
        o0 = rfm.op0;
        o1 = rfm.op1;
      end
        
      ovm_report_info("spu", "process spu inst", OVM_HIGH);
      foreach(prTmp[i,j]) begin
        prTmp[i][j] = ise.prRdAdrSPU == 0 ? 1 : pr[ise.tid][ise.prRdAdrSPU][i][j];
        if(ise.prInvSPU)
          prTmp[i][j] = !prTmp[i][j];
        if(!ise.prNMskSPU)
         prTmp[i][j] = prTmp[i][j] && ilm[ise.tid][i][j] && cm[ise.tid][i][j];
        prSPU[STAGE_RRF_EXS1] |= prTmp[i][j];
      end

      op0 = {o0[WORD_BITS-1], o0};
      op1 = {o1[WORD_BITS-1], o1};
      
      case(ise.op)
      op_nop,
      op_s2gp,
      op_bp0:    r0 = op0;
      op_bp1:    r0 = op1;
      
      op_and:    r0 = op0 & op1;
      op_or:     r0 = op0 | op1;
      op_xor:    r0 = op0 ^ op1;
      op_nor:    r0 = ~(op0 | op1);
      op_add:    r0 = signed'(op0) + signed'(op1);
      op_uadd:   r0 = op0 + op1;
      op_sub:    r0 = signed'(op0) - signed'(op1);
      op_usub:   r0 = op0 - op1;
      op_srl:    r0 = op0 >> op1;
      op_sra:    r0 = op0 >>> op1;
      op_sll:    r0 = op0 << op1;
      op_ror:    r0 = {o0, o0} >> o1;
///      op_umul:   r0 = unsigned'(o0) * unsigned'(o1);
///      op_smul:   r0 = signed'(op0) * signed'(op1);
      op_clo:   ovm_report_warning("SPU_UNIMP", "clo is not implemented yet");
      op_clz:   ovm_report_warning("SPU_UNIMP", "clz is not implemented yet");
      op_ext:   ovm_report_warning("SPU_UNIMP", "ext is not implemented yet");
      op_ins:   ovm_report_warning("SPU_UNIMP", "ins is not implemented yet");
      op_seb:   ovm_report_warning("SPU_UNIMP", "seb is not implemented yet");
      op_she:   ovm_report_warning("SPU_UNIMP", "she is not implemented yet");
      op_wsbh:  ovm_report_warning("SPU_UNIMP", "wsbh is not implemented yet");
      op_gp2s:
      begin
///        if(ise.srAdr == SR_MSCG)
///          for(int i = 0; i < CYC_VEC; i++)
///            for(int j = 0; j < NUM_SP; j++)
///              msc[ise.tid][i][j][WORD_BITS - 1] = vn.rfm[STAGE_RRF_EXS1].res[i * NUM_SP + j];
      end
      endcase
      vn.rfm[STAGE_RRF_EXS1] = tr_spu2rfm::type_id::create("toRFM", this);
      vn.rfm[STAGE_RRF_EXS1].res = r0[WORD_BITS-1:0];
      vn.rfm[STAGE_RRF_EXS1].wrEn = prSPU[STAGE_RRF_EXS1] && !exp && ise.wrEn;
      vn.rfm[STAGE_RRF_EXS1].expFu = exp;
      vn.rfm[STAGE_RRF_EXS1].tid = ise.tid;
      vn.rfm[STAGE_RRF_EXS1].vecMode = ise.vecModeFu;
      vn.rfm[STAGE_RRF_EXS1].srfWrBk   = ise.srfWrBk;
      vn.rfm[STAGE_RRF_EXS1].srfWrGrp  = ise.srfWrGrp;
      vn.rfm[STAGE_RRF_EXS1].srfWrAdr  = ise.srfWrAdr;
      expFu = exp;
      if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
      toISE.sclExp = exp;
      toISE.vecModeSclExp = ise.vecModeFu;
      toISE.tidSclExp = ise.tid;
    end

    if(v.fmISE[STAGE_RRF_RSRB] != null && v.fmISE[STAGE_RRF_RSRB] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RSRB];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_RSRB];    
      
      ///redirect sr reqs
      if(prSPU[STAGE_RRF_RSRB] && ise.op == op_gp2s) begin
        if(ise.srAdr inside {tlb_sr}) begin
          toTLB = tr_spu2tlb::type_id::create("toTLB", this);
          toTLB.op0 = rfm.op0;
          toTLB.s2gp = 1;
          toTLB.req = 1;
          toTLB.tid = ise.tid;
          toTLB.srAdr = ise.srAdr;
        end
        else if(ise.srAdr inside {SR_OCMC, SR_MBASE}) begin
          if(toDSE == null) toDSE = tr_spu2dse::type_id::create("toDSE", this);
          toDSE.srReq = 1;
          toDSE.s2gp = 1;
          toDSE.op = ise.op;
          toDSE.tid = ise.tid;
          toDSE.srAdr = ise.srAdr;
        end
        else begin
          if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
          toISE.srReq = 1;
          toISE.s2gp = 1;
          toISE.op = ise.op;
          toISE.tid = ise.tid;
          toISE.srAdr = ise.srAdr;
        end
      end
    end

    if(v.fmISE[STAGE_RRF_WSRB] != null && v.fmISE[STAGE_RRF_WSRB] != null
        && !cancel[v.fmISE[STAGE_RRF_WSRB].tid][STAGE_RRF_WSRB]) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_WSRB];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_WSRB];
      bit prPass = prSPU[STAGE_RRF_WSRB] || ise.op == op_tsync;
      
      if(prPass && ise.op inside {op_s2gp, tlb_ops, op_smsg, op_rmsg}) begin
        if(ise.srAdr inside {tlb_sr} && ise.op inside {tlb_ops}) begin
          toTLB = tr_spu2tlb::type_id::create("toTLB", this);
          toTLB.op0 = rfm.op0;
          toTLB.op = ise.op;
          toTLB.req = 1;
          toTLB.tid = ise.tid;
          toTLB.srAdr = ise.srAdr;
        end
        else if(ise.srAdr inside {[SR_MD0:SR_MD7], [SR_MCS0:SR_MCS2], SR_FFS, SR_SUPM0, SR_SUPM1}
                && ise.op inside {op_smsg, op_rmsg}) begin
          if(toEIF == null) toEIF = tr_spu2eif::type_id::create("toEIF", this);
          toEIF.srReq = 1;
          toEIF.tid = ise.tid;
          toEIF.srAdr = ise.srAdr;
          toEIF.op = ise.op;
          toEIF.rt = ise.rt;
          toEIF.ss = ise.ss;
          toEIF.vs = ise.vs;
        end
        else begin
          if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
          toISE.srReq = 1;
          toISE.op0 = rfm.op0;
          toISE.op = ise.op;
          toISE.tid = ise.tid;
          toISE.srAdr = ise.srAdr;
        end
        
        if(ise.srAdr inside {SR_OCMC, SR_MBASE} && ise.op inside {op_s2gp, tlb_ops}) begin
          if(toDSE == null) toDSE = tr_spu2dse::type_id::create("toDSE", this);
          toDSE.srReq = 1;
          toDSE.op0 = rfm.op0;
          toDSE.op = ise.op;
          toDSE.tid = ise.tid;
          toDSE.srAdr = ise.srAdr;
        end
      end
    end
            
    ///collect sr from dse & tlb
    if(v.fmISE[STAGE_RRF_RSR] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RSR];
      if(v.fmTLB != null && v.fmTLB.rsp) begin
        if(vn.rfm[STAGE_RRF_SWBP] == null)
          vn.rfm[STAGE_RRF_SWBP] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[STAGE_RRF_SWBP].res = v.fmTLB.res;
      end
      else if(v.fmDSE[STAGE_RRF_DEM] != null && v.fmDSE[STAGE_RRF_DEM].rsp) begin
        if(vn.rfm[STAGE_RRF_SWBP] == null)
          vn.rfm[STAGE_RRF_SWBP] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[STAGE_RRF_SWBP].res = v.fmDSE[STAGE_RRF_DEM].srRes;
      end
      else if(v.fmISE[0] != null && v.fmISE[0].srRsp) begin
        if(vn.rfm[STAGE_RRF_SWBP] == null)
          vn.rfm[STAGE_RRF_SWBP] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[STAGE_RRF_SWBP].res = v.fmISE[0].srRes;
      end
      else if(v.fmEIF != null && v.fmEIF.srRsp) begin
        if(vn.rfm[STAGE_RRF_SWBP] == null)
          vn.rfm[STAGE_RRF_SWBP] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[STAGE_RRF_SWBP].res = v.fmEIF.srRes;
      end
    end
    
    ///check for valid branch
    if(v.fmISE[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM].op inside {op_br, op_fcr}) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_CEM];
      uchar tid = ise.tid, subVec = ise.subVecSPU;
      ushort popcnt;
      bit b_inv = ise.prInvSPU, b_nmsk = ise.prNMskSPU;
      bit is_nop = ise.mop == mop_nop;
      bit emsk[NUM_SP];
      if(rfm != null)
        popcnt = rfm.op0;
        
      emsk = ise.prRdAdrSPU == 0 ? '{default:1} : pr[tid][ise.prRdAdrSPU][subVec];
      
      for(int i = 0; i < NUM_SP; i++) begin
        if(b_inv)
          emsk[i] = !emsk[i];
        if(ise.mop == mop_else)
          emsk[i] = emsk[i] && (!(msc[tid][subVec][i] > 1));
        else if(!b_nmsk) begin
          if(ise.mop == mop_loop)
            emsk[i] = emsk[i] && ilm[tid][subVec][i];
          else
            emsk[i] = emsk[i] && ilm[tid][subVec][i] && cm[tid][subVec][i];
        end
        if(emsk[i] == 1) 
          emskAllZero = 0;
      end
      ovm_report_info("spu", $psprintf("process branch for thread %0d, subVec %0d, az: %0d, %s, %s, %s",
                                        tid, ise.subVecSPU, emskAllZero, ise.bop.name, ise.mop.name, ise.sop.name), OVM_HIGH);      
      
      case(ise.mop)
      mop_guard :
        mscGuard[tid][subVec] = emsk;
      mop_rstor :
      begin
        foreach(emsk[i]) begin
          if(msc[tid][subVec][i] > 0)
            cmNext[0][i] = 0;
          else
            cmNext[0][i] = 0;
          if(msc[tid][subVec][i] > 1)
            ilmNext[0][i] = 0;
          else
            cmNext[0][i] = 1;
        end
      end
      mop_if,
      mop_loop,
///      mop_brk,
      mop_else  :
      begin
        ilmNext[0] = emsk;
        cmNext[0] = emsk;
      end
      mop_cont  :
      begin
        cmNext[0] = emsk;
        ilmNext[0] = ilm[tid][subVec];
      end
      endcase
        
      case(ise.sop)
      sop_p2n,
      sop_p2nc :
        foreach(emsk[i]) begin
          if(msc[tid][subVec][i] > (2*popcnt))
              mscNext[0][i] -= (2*popcnt);
          else begin
            mscNext[0][i] = 0;
            if(mscGuard[tid][subVec][i]) begin
              mscUF[0][i] = 1;
              mscNext[0][i] += CFG_MAX_MSC / 2;
              expMSC = 1;
            end
          end
        end
      sop_store :
        foreach(emsk[i]) begin
          mscNext[0][i] += !ilm[tid][subVec][i];
          mscNext[0][i] += !cm[tid][subVec][i];
          if(mscNext[0][i] > CFG_MAX_MSC) begin
            mscOF[0][i] = 1;
            mscNext[0][i] -= CFG_MAX_MSC / 2;
            expMSC = 1;
          end
        end
      sop_zero  :
        mscNext[0] = '{default : 0};
      endcase
      
      if(ise.subVecSPU == ise.vecModeSPU) begin
        bit updateMSC = 0, updateMSK = 0, missBr = 0, brTaken = 0;
        
        if(is_nop)
          brTaken = 0;
        else
          case(ise.bop)
          bop_naz  :  brTaken = !emskAllZero;
          bop_az   :  brTaken = emskAllZero;
          endcase
             
        case(ise.mop)
        mop_bc,
        mop_nop   : updateMSK = 0;
        mop_guard,
        mop_rstor : updateMSK = 1;
        mop_loop,
        mop_if,
        mop_else,
///        mop_brk,
        mop_cont  : updateMSK = !emskAllZero;
        endcase

        if(ise.sop inside {sop_p2n, sop_store, sop_zero})
          updateMSC = 1;
        else/// if(ise.sop == sop_p2nc)
          updateMSC = emskAllZero;

        updateMSK &= !cancel[tid][STAGE_RRF_CEM];
        updateMSC &= !cancel[tid][STAGE_RRF_CEM];
                      
        for(int i = 0; i <= ise.vecModeSPU; i++) begin
          stkWEn[i] = updateMSC;
          mskWEn[i] = updateMSK;
        end
        
        missBr = ise.brPred != brTaken;
        if(brTaken && ise.brSrf && rfm != null && ise.predPc != rfm.op0)
          missBr = 1;
        expMSC = expMSC && updateMSC;
        
        if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
        toISE.brRsp = 1;
        toISE.tid = tid;
        toISE.mscExp = 0;
        toISE.vecMode = ise.vecModeSPU;
        toISE.mscExp = expMSC;
        toISE.brTaken = brTaken;
        toISE.missBr = missBr;
        if(toRFM == null) toRFM = tr_spu2rfm::type_id::create("toRFM", this);
        toRFM.missBr = missBr;
        toRFM.expMSC = expMSC;
        if(toDSE == null) toDSE = tr_spu2dse::type_id::create("toDSE", this);
        toDSE.missBr = missBr;
        toDSE.expMSC = expMSC;
        toDSE.tidExpMSC = ise.tid;
        emskAllZero = 1;
        brCancel = 1;
        expMSC = 0;
      end
    end
    
    ///send msc exp info to rfm
    if(v.fmISE[STAGE_RRF_CEM + CYC_VEC] != null && stkWEn[CYC_VEC]) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM + CYC_VEC];
      if(toRFM == null) toRFM = tr_spu2rfm::type_id::create("toRFM", this);
      toRFM.wrSrMSC = 1;
      toRFM.msco = mscOF[CYC_VEC];
      toRFM.mscu = mscUF[CYC_VEC];
      toRFM.subVec = ise.subVecSPU;
    end
    
    ///send bpc to ise
    if(v.fmISE[STAGE_RRF_EXS0] != null && v.fmISE[STAGE_RRF_EXS0].brSrf && v.fmRFM[STAGE_RRF_EXS0] != null) begin
      if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
      toISE.bpc = v.fmRFM[STAGE_RRF_EXS0].op0;
    end
    
    ///------------req to other module----------------
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toTLB != null) void'(tlb_tr_port.nb_transport(toTLB, toTLB));
    if(toEIF != null) void'(eif_tr_port.nb_transport(toEIF, toEIF));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2spu req, output tr_ise2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spu req, output tr_rfm2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM[STAGE_RRF_EXS0] = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spa(input tr_spa2spu req, output tr_spa2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA[STAGE_RRF_CEM] = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_dse(input tr_dse2spu req, output tr_dse2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE[STAGE_RRF_DEM] = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_tlb(input tr_tlb2spu req, output tr_tlb2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get tlb Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmTLB = req;
    return 1;
  endfunction : nb_transport_tlb

  function bit nb_transport_eif(input tr_eif2spu req, output tr_eif2spu rsp);
    ovm_report_info("spu_tr", $psprintf("Get eif Transaction:\n%s", req.sprint()), OVM_HIGH);
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
    spa_tr_imp = new("spa_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    tlb_tr_imp = new("tlb_tr_imp", this);
    eif_tr_imp = new("eif_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    eif_tr_port = new("eif_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    ilm = '{default : 1};
    cm = '{default : 1};
  endfunction : build
endclass : ip4_tlm_spu

///-------------------------------------other functions-----------------------------------------
