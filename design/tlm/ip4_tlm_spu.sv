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
  tr_ise2spu fmISE[STAGE_RRF_VWB:0];
  tr_rfm2spu fmRFM[STAGE_RRF_VWB:STAGE_RRF_EXS0];
  tr_spa2spu fmSPA[STAGE_RRF_VWB:STAGE_RRF_CEM];
  tr_dse2spu fmDSE[STAGE_RRF_VWB:STAGE_RRF_DEM];
  tr_tlb2spu fmTLB;
  tr_spu2rfm rfm[STAGE_RRF_SWBP:STAGE_RRF_EXS1];
  
  `ovm_component_utils_begin(ip4_tlm_spu_vars)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)
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
  local bit pr[NUM_THREAD][NUM_PR:1][CYC_VEC][NUM_SP];
  local bit[NUM_VEC - 1 : 0] srMSCO[NUM_THREAD],  srMSCU[NUM_THREAD];
  local bit prSPU[STAGE_RRF_SWBP:STAGE_RRF_EXS1];
  local bit[STAGE_RRF_WSR:0] cancel[NUM_THREAD];
  local bit expFu, expMSC, missBr;
  
  `ovm_component_utils_begin(ip4_tlm_spu)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2spu, tr_ise2spu, ip4_tlm_spu) ise_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2spu, tr_spa2spu, ip4_tlm_spu) spa_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2spu, tr_rfm2spu, ip4_tlm_spu) rfm_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2spu, tr_dse2spu, ip4_tlm_spu) dse_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2spu, tr_tlb2spu, ip4_tlm_spu) tlb_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_spu2rfm, tr_spu2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2ise, tr_spu2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2spa, tr_spu2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2dse, tr_spu2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_spu2tlb, tr_spu2tlb) tlb_tr_port;
  
  function void comb_proc();
    ovm_report_info("spu", "comb_proc procing...", OVM_FULL); 
    foreach(cancel[i])
      cancel[i] = cancel[i] << 1;
    
    ///self cancel
    if(v.fmISE[STAGE_RRF_EPS] != null && expFu)
      cancel[v.fmISE[STAGE_RRF_EPS].tid] |= `GML(STAGE_RRF_EPS);
    if(v.fmISE[STAGE_RRF_CBR] != null && (expMSC || missBr))
      cancel[v.fmISE[STAGE_RRF_EPS].tid] |= `GML(STAGE_RRF_CBR);
            
    expFu = 0;
    missBr = 0;
    expMSC = 0;
    
    ///spa request canceling
    if(v.fmSPA[STAGE_RRF_CEM] != null && v.fmSPA[STAGE_RRF_CEM].cancel)
      cancel[v.fmSPA[STAGE_RRF_CEM].tid] |= `GML(STAGE_RRF_WSR);
    
    ///dse request canceling
    if(v.fmDSE[STAGE_RRF_DEM] != null && v.fmDSE[STAGE_RRF_DEM].cancel)
      cancel[v.fmDSE[STAGE_RRF_DEM].tidCancel] |= `GML(STAGE_RRF_WSR);    
    
    for(int i = STAGE_RRF_VWB; i > 0; i--)
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
    
    ovm_report_info("spu", "req_proc procing...", OVM_FULL); 
    
    ///--------------prepare---------------------------------
    toRFM = v.rfm[STAGE_RRF_SWBP];
    
    ///----------process data---------------------
      
    ///write back cmp predication register results
    if(v.fmSPA[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_spa2spu spa = v.fmSPA[STAGE_RRF_CEM];
      ovm_report_info("spu", "write back spa pres", OVM_FULL);
      pr[ise.tid][ise.prWrAdr0][ise.subVec] = spa.presCmp0;
      pr[ise.tid][ise.prWrAdr1][ise.subVec] = spa.presCmp1;
    end
    
    ///write back dse predication register results
    if(v.fmDSE[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_dse2spu dse = v.fmDSE[STAGE_RRF_CEM];
      ovm_report_info("spu", "write back dse pres", OVM_FULL);
      if(dse.wrEn)
        pr[ise.tid][ise.prWrAdr2][ise.subVec] = dse.pres;
    end
        
    ///predication register read
    if(v.fmISE[STAGE_RRF_RRC] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC];
      foreach(toSPA.fu[fid]) begin
        if(!ise.enFu[fid]) continue;
        if(toSPA == null) toSPA = tr_spu2spa::type_id::create("toSPA", this);
        toSPA.fu[fid].emsk = ise.prRdAdr[fid] == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdr[fid]][ise.subVec];
        if(ise.prInv[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = !toSPA.fu[fid].emsk[i];
        if(!ise.prNMsk[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = toSPA.fu[fid].emsk[i] && ilm[ise.tid][ise.subVec][i] && cm[ise.tid][ise.subVec][i];
      end
    end
    
    if(v.fmISE[STAGE_RRF_RRC0] != null && v.fmISE[STAGE_RRF_RRC0].enDSE) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC0];
      toDSE = tr_spu2dse::type_id::create("toDSE", this);
      
      if(ise.sclDSE && ise.subVec == 0)
        toDSE.emsk[0] = 1;
      else
        toDSE.emsk = ise.prRdAdrDSE == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdrDSE][ise.subVec];
        
      foreach(toDSE.emsk[i]) begin
        if(ise.prInvDSE)
          toDSE.emsk[i] = !toDSE.emsk[i];
        if(!ise.prNMskDSE)
          toDSE.emsk[i] = toDSE.emsk[i] && ilm[ise.tid][ise.subVec][i] && cm[ise.tid][ise.subVec][i];
      end
      toDSE.emsk = ise.prRdAdrDSE == 0 ? '{default:1} : pr[ise.tid][ise.prRdAdrDSE][ise.subVec];
    end
    
    ///scalar dse enable
    if(v.fmISE[STAGE_RRF_RRC] != null && v.fmISE[STAGE_RRF_RRC].enDSE) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC];
      bit res = 0;
      if(toDSE) toDSE = tr_spu2dse::type_id::create("toDSE", this);
      if(ise.sclDSE) begin
        for(int subVec = 0; subVec <= ise.vecMode; subVec++) begin
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
      
      if(rfm != null) begin
        o0 = rfm.op0;
        o1 = rfm.op1;
      end
      
      if(ise.start) begin
        bit exp;
        ovm_report_info("spu", "process spu inst", OVM_FULL);
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
///        op_umul:   r0 = unsigned'(o0) * unsigned'(o1);
///        op_smul:   r0 = signed'(op0) * signed'(op1);
        op_clo:   ovm_report_warning("SPU_UNIMP", "clo is not implemented yet");
        op_clz:   ovm_report_warning("SPU_UNIMP", "clz is not implemented yet");
        op_ext:   ovm_report_warning("SPU_UNIMP", "ext is not implemented yet");
        op_ins:   ovm_report_warning("SPU_UNIMP", "ins is not implemented yet");
        op_seb:   ovm_report_warning("SPU_UNIMP", "seb is not implemented yet");
        op_she:   ovm_report_warning("SPU_UNIMP", "she is not implemented yet");
        op_wsbh:  ovm_report_warning("SPU_UNIMP", "wsbh is not implemented yet");
        op_gp2s:
        begin
          if(ise.srAdr == SR_MSCT)
            for(int i = 0; i < CYC_VEC; i++)
              for(int j = 0; j < NUM_SP; j++)
                msc[ise.tid][i][j][WORD_BITS - 1] = vn.rfm[STAGE_RRF_EXS1].res[i * NUM_SP + j];
        end
        endcase
        vn.rfm[STAGE_RRF_EXS1] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[STAGE_RRF_EXS1].res = r0[WORD_BITS-1:0];
        vn.rfm[STAGE_RRF_EXS1].wrEn = prSPU[STAGE_RRF_EXS1] && !exp;
        vn.rfm[STAGE_RRF_EXS1].expFu = exp;
        vn.rfm[STAGE_RRF_EXS1].tid = ise.tid;
        vn.rfm[STAGE_RRF_EXS1].vecMode = ise.vecMode;
        vn.rfm[STAGE_RRF_EXS1].srfWrBk   = ise.srfWrBk;
        vn.rfm[STAGE_RRF_EXS1].srfWrGrp  = ise.srfWrGrp;
        vn.rfm[STAGE_RRF_EXS1].srfWrAdr  = ise.srfWrAdr;
        expFu = exp;
        if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
        toISE.sclExp = exp;
        toISE.vecModeSclExp = ise.vecMode;
        toISE.tidSclExp = ise.tid;
      end
    end

    if(v.fmISE[STAGE_RRF_DSR] != null && v.fmISE[STAGE_RRF_DSR] != null) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_DSR];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_DSR];    
      
      if(ise.op == op_s2gp && ise.srAdr inside {SR_MSCT, SR_MSCO, SR_MSCU}) begin
        if(vn.rfm[STAGE_RRF_DSR] == null) vn.rfm[STAGE_RRF_DSR] = tr_spu2rfm::type_id::create("toRFM", this);
        case(ise.srAdr)
        SR_MSCT:
          for(int i = 0; i < CYC_VEC; i++)
            for(int j = 0; j < NUM_SP; j++)
              vn.rfm[STAGE_RRF_DSR].res[i * NUM_SP + j] = msc[ise.tid][i][j][WORD_BITS - 1];
        SR_MSCO: vn.rfm[STAGE_RRF_DSR].res = srMSCO[ise.tid];
        SR_MSCU: vn.rfm[STAGE_RRF_DSR].res = srMSCU[ise.tid];
        endcase
      end
      
      ///redirect sr reqs
      if(prSPU[STAGE_RRF_DSR] && ise.op == op_gp2s) begin
        if(ise.srAdr inside {tlbsr}) begin
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

    if(v.fmISE[STAGE_RRF_WSR] != null && v.fmISE[STAGE_RRF_WSR] != null
        && !cancel[v.fmISE[STAGE_RRF_WSR].tid][STAGE_RRF_WSR]) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_WSR];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_WSR];
      
      if(prSPU[STAGE_RRF_DSR] && ise.op inside {op_s2gp, tlb_ops}) begin
        if(ise.srAdr inside {tlbsr} && ise.op inside {tlb_ops}) begin
          toTLB = tr_spu2tlb::type_id::create("toTLB", this);
          toTLB.op0 = rfm.op0;
          toTLB.op = ise.op;
          toTLB.req = 1;
          toTLB.tid = ise.tid;
          toTLB.srAdr = ise.srAdr;
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
    end
    
    ///check for valid branch
    if(v.fmISE[STAGE_RRF_CEM] != null && v.fmSPA[STAGE_RRF_CEM] != null && v.fmISE[STAGE_RRF_CEM].brEnd)begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_CEM];
      tr_rfm2spu rfm = v.fmRFM[STAGE_RRF_CEM];
      uchar tid = ise.tid;
      ushort popcnt = rfm == null ? 0 : rfm.op0;
      bit b_inv = ise.prInvSPU, b_nmsk = ise.prNMskSPU;
      bit is_nop = ise.mop == mop_nop, emsk_az = 1, update_msc = 0;
      bit emsk[CYC_VEC][NUM_SP] = ise.prRdAdrSPU == 0 ? '{default:1} : pr[tid][ise.prRdAdrSPU];
      
      ovm_report_info("spu", $psprintf("process branch for thread %0d", tid), OVM_HIGH);
      
      if(b_inv)
        foreach(emsk[j,k])
          emsk[j][k] = !emsk[j][k];
      
      if(ise.mop == mop_else) begin
        foreach(emsk[j,k])
          emsk[j][k] = emsk[j][k] && (!(msc[tid][j][k] > 1));
      end
      else if(!b_nmsk) begin
        if(ise.mop == mop_loop)
          foreach(emsk[j,k])
            emsk[j][k] = emsk[j][k] && ilm[tid][j][k];
        else
          foreach(emsk[j,k])
            emsk[j][k] = emsk[j][k] && ilm[tid][j][k] && cm[tid][j][k];
      end
      
      if(toISE == null) toISE = tr_spu2ise::type_id::create("toISE", this);
      toISE.tid = tid;
      toISE.mscExp = 0;
      toISE.vecMode = ise.vecMode;
      
      foreach(emsk[j,k]) 
        if(emsk[j][k] == 1 && j <= ise.vecMode) begin
           emsk_az = 0;
           break;
        end

      if(is_nop)
        toISE.brTaken = 0;
      else
        case(ise.bop)
        bop_naz  :  toISE.brTaken = !emsk_az;
        bop_az   :  toISE.brTaken = emsk_az;
        endcase
             
      case(ise.mop)
      mop_nop   : update_msc = 1;
      mop_rstor :
        begin
          foreach(emsk[j,k]) begin
            if(msc[tid][j][k] > 0) cm[tid][j][k] = 0;
            else cm[tid][j][k] = 0;
            if(msc[tid][j][k] > 1) ilm[tid][j][k] = 0;
            else ilm[tid][j][k] = 1;
          end
        end
      mop_if,
      mop_else  :
        if(!emsk_az) begin
          ilm[tid] = emsk;
          cm[tid] = emsk;
        end
      mop_loop  :
        if(!emsk_az) begin
          ilm[tid] = emsk;
          cm[tid] = emsk;
        end
      mop_cont  :
        if(!emsk_az)
          cm[tid] = emsk;
        else
          update_msc = 1;
      mop_brk:
        if(!emsk_az) begin
          ilm[tid] = emsk;
          cm[tid] = emsk;
        end
        else
          update_msc = 1;
      endcase
        
      case(ise.sop)
      sop_pop2n :
        if(update_msc)
          foreach(emsk[j,k]) begin
            bit top = msc[tid][j][k][WORD_BITS - 1];
            if(j > ise.vecMode) break;
            if(msc[tid][j][k] > (2*popcnt))
              msc[tid][j][k] -= (2*popcnt);
            else
              msc[tid][j][k] = 0;
            if(top == 1 && msc[tid][j][k][WORD_BITS - 1] == 0) begin
              srMSCU[tid][j * NUM_SP + k] = 1;
              expMSC = 1;
            end
            else
              srMSCU[tid][j * NUM_SP + k] = 0;
          end
      sop_store :
        foreach(emsk[j,k]) begin
          bit top = msc[tid][j][k][WORD_BITS - 1];
          if(j > ise.vecMode) break;
          msc[tid][j][k] += !ilm[tid][j][k];
          msc[tid][j][k] += !cm[tid][j][k];
          if(top == 1 && msc[tid][j][k][WORD_BITS - 1] == 0) begin
            srMSCO[tid][j * NUM_SP + k] = 1;
            msc[tid][j][k][WORD_BITS - 1] = 1;
            expMSC = 1;
          end
          else
            srMSCO[tid][j * NUM_SP + k] = 0;
        end
      endcase
      
      missBr = ise.brPred != toISE.brTaken;
      toISE.mscExp = expMSC;
      if(toRFM == null) toRFM = tr_spu2rfm::type_id::create("toRFM", this);
      toRFM.missBr = ise.brPred != toISE.brTaken;
      toRFM.expMSC = expMSC;
      if(toDSE == null) toDSE = tr_spu2dse::type_id::create("toDSE", this);
      toDSE.missBr = missBr;
      toDSE.expMSC = toISE.mscExp;
      toDSE.tidExpMSC = ise.tid;
    end

    ///spu exp cancel
///    if(toRFM != null) begin
///      if(toRFM.missBr || toRFM.expMSC)
///        selfCancel = STAGE_RRF_WSR;
///      else if(toRFM.expFu)
///        selfCancel = STAGE_RRF_EXS1;
///    end
    
    ///------------req to other module----------------
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toTLB != null) void'(tlb_tr_port.nb_transport(toTLB, toTLB));
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
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    
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
