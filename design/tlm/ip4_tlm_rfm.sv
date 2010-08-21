/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_rfm.sv
/// Title            : ip4 register file manager
/// Version          : 0.1
/// Last modified    : Mar 7 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Mar 7 2010

class ip4_tlm_rfm_vars extends ovm_component;
  tr_spu2rfm fmSPU;
  tr_dse2rfm fmDSE;
  tr_spa2rfm fmSPA;
  
  tr_ise2rfm fmISE[STAGE_EEX_VWB0:0];
  tr_rfm2spa spa[CYC_VEC];
  tr_rfm2spu spu;
  word dseOp1[CYC_VEC];
  
  `ovm_component_utils_begin(ip4_tlm_rfm_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(dseOp1, OVM_ALL_ON)
    `ovm_field_object(spu, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new

endclass : ip4_tlm_rfm_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_rfm extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local word vrf[NUM_PHY_VRF_GRP][NUM_PRF_P_GRP/NUM_VRF_BKS][NUM_VRF_BKS][CYC_VEC][NUM_SP];
  local word srf[NUM_PHY_SRF_GRP][NUM_PRF_P_GRP/NUM_VRF_BKS][NUM_SRF_BKS];
    
  local ip4_tlm_rfm_vars v, vn;
  local word bpImmLast[NUM_BP_IMM];
  local tr_rfm2spa toSPA;
  local tr_rfm2spu toSPU;
  local tr_rfm2dse toDSE;
  local uchar srExpFlag[NUM_THREAD][CYC_VEC][NUM_SP];
  
  `ovm_component_utils_begin(ip4_tlm_rfm)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2rfm, tr_ise2rfm, ip4_tlm_rfm) ise_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2rfm, tr_spu2rfm, ip4_tlm_rfm) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2rfm, tr_dse2rfm, ip4_tlm_rfm) dse_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2rfm, tr_spa2rfm, ip4_tlm_rfm) spa_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_rfm2spa, tr_rfm2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_rfm2dse, tr_rfm2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_rfm2spu, tr_rfm2spu) spu_tr_port;
  
  extern function void read_rf(inout word, input rbk_sel_e, uchar, const ref word cvrf[NUM_VRF_BKS][NUM_SP],
                                csrf[NUM_SRF_BKS], bpImm[NUM_BP_IMM], input word imm);
  //endfunction
  
  function void comb_proc();
    ovm_report_info("rfm", "comb_proc procing...", OVM_FULL); 
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmDSE != null) end_tr(v.fmDSE);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmISE[0] != null) end_tr(v.fmISE[0]);
    
    vn.fmSPU = null;
    vn.fmDSE = null;
    vn.fmSPA = null;
    vn.fmISE[0] = null;

    toSPA = null;
    toSPU = null;
    toDSE = null;
  
    for(int i = STAGE_EEX_VWB0; i > 0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    for(int i = CYC_VEC - 1; i > 0; i--) 
      vn.dseOp1[i] = v.dseOp1[i - 1];
  endfunction
  
  function void req_proc();
    word cvrf[NUM_VRF_BKS][NUM_SP];
    word csrf[NUM_SRF_BKS];
      
    ovm_report_info("rfm", "req_proc procing...", OVM_FULL); 
   
    ///----------------------write back results---------------------
    if(v.fmSPA != null) begin
      tr_spa2rfm spa = v.fmSPA;
      uchar bk0, bk1;
      
      foreach(spa.fu[fid]) begin
        ovm_report_info("RFM_WR", $psprintf("Write Back FU%0d : %s...", fid, fu_cfg[fid].name), OVM_HIGH);
        srExpFlag[spa.fu[fid].tid][spa.fu[fid].subVec] = spa.fu[fid].expFlag;
        bk0 = spa.fu[fid].vrfWrBk & ('1 - 'b01);
        bk1 = spa.fu[fid].vrfWrBk & ('1 - 'b01) + 'b01;
        foreach(spa.fu[0].wrEn[sp])
          if(spa.fu[fid].wrEn[sp]) begin
            if(spa.fu[fid].dw) begin
              vrf[spa.fu[fid].vrfWrGrp][spa.fu[fid].vrfWrAdr][bk0][spa.fu[fid].subVec][sp] = spa.fu[fid].res0[sp];
              vrf[spa.fu[fid].vrfWrGrp][spa.fu[fid].vrfWrAdr][bk1][spa.fu[fid].subVec][sp] = spa.fu[fid].res1[sp];
            end
            else
              vrf[spa.fu[fid].vrfWrGrp][spa.fu[fid].vrfWrAdr][spa.fu[fid].vrfWrBk][spa.fu[fid].subVec][sp] = spa.fu[fid].res0[sp];
          end
      end
    end
    
    if(v.fmDSE != null) begin
      tr_dse2rfm dse = v.fmDSE;
      ovm_report_info("RFM_WR", "Write Back dse...", OVM_HIGH);
      if(dse.srfWr)
        srf[dse.wrGrp][dse.wrAdr][dse.wrBk] = v.fmDSE.res[0];
      else foreach(dse.wrEn[sp])
        if(dse.wrEn[sp])
          vrf[dse.wrGrp][dse.wrAdr][dse.wrBk][dse.subVec][sp] = dse.res[sp];
      if(!dse.srfWr && dse.updateAdrWr)
        foreach(dse.wrEn[sp])
          if(dse.wrEn[sp])
            vrf[dse.updateAdrWrGrp][dse.updateAdrWrAdr][dse.updateAdrWrBk][dse.subVec][sp] = dse.updateAdrRes[sp];
    end
    
    if(v.fmSPU != null && v.fmSPU.wrEn) begin
      word res;
      tr_spu2rfm spu = v.fmSPU;
      tr_spa2rfm spa = v.fmSPA;
      tr_dse2rfm dse = v.fmDSE;
      bit dw = 0;
      ovm_report_info("RFM_WR", "Write Back spu...", OVM_HIGH);
      res = spu.res;
      srf[spu.srfWrGrp][spu.srfWrAdr][spu.srfWrBk] = res;
    end
         
    ///----------read registers---------------------
   
    for(int subVec = 0; subVec < CYC_VEC; subVec++) begin
      tr_ise2rfm ise = v.fmISE[STAGE_RRF_RRC0 + subVec];
      
      if(ise == null) continue;
      foreach(cvrf[bk,sp])
        cvrf[bk][sp] = vrf[ise.vrfRdGrp[bk]][ise.vrfRdAdr[bk]][bk][subVec][sp];

      foreach(csrf[bk])
        csrf[bk] = srf[ise.srfRdGrp[bk]][ise.srfRdAdr[bk]][bk];  
                
      if(ise.start) begin
        bpImmLast = ise.bpImm;
      end
      
      foreach(ise.fu[fid]) begin
        if(!ise.fu[fid].en) continue;
        ovm_report_info("RFM_RD", $psprintf("Read for spa subVec %0d, FU%0d : %s ...", subVec, fid, fu_cfg[fid].name), OVM_HIGH);
        if(vn.spa[subVec] == null) vn.spa[subVec] = tr_rfm2spa::type_id::create("toSPA", this);
        vn.spa[subVec].fu[fid].en = 1;
        foreach(vn.spa[subVec].fu[fid].rp[rp])
          foreach(vn.spa[subVec].fu[fid].rp[rp].op[sp])
            read_rf(vn.spa[subVec].fu[fid].rp[rp].op[sp], ise.fu[fid].rdBkSel[rp], sp, cvrf, csrf, bpImmLast, ise.fu[fid].imm);
      end
      
      if(ise.dseEn && subVec == 0) begin
        ovm_report_info("RFM_RD", $psprintf("Read for dse subVec %0d ...", subVec), OVM_HIGH);
        if(toDSE == null) toDSE = tr_rfm2dse::type_id::create("toDSE", this);
        foreach(toDSE.base[sp]) begin
          read_rf(toDSE.base[sp], ise.dseRdBk[0], sp, cvrf, csrf, ise.bpImm, ise.dseImm);
          read_rf(vn.dseOp1[ise.cyc][sp], ise.dseRdBk[1], sp, cvrf, csrf, ise.bpImm, ise.dseImm);
        end
        read_rf(toDSE.op2, ise.dseRdBk[2], 0, cvrf, csrf, ise.bpImm, ise.dseImm);
      end
            
      if(ise.spuEn && subVec == 0) begin
        ovm_report_info("RFM_RD", $psprintf("Read for spu subs %0d ...", subVec), OVM_HIGH);
        if(vn.spu == null) vn.spu = tr_rfm2spu::type_id::create("toSPU", this);
        read_rf(vn.spu.op0, ise.spuRdBk[0], 0, cvrf, csrf, ise.bpImm, ise.spuImm);
        read_rf(vn.spu.op1, ise.spuRdBk[1], 0, cvrf, csrf, ise.bpImm, ise.spuImm);          
      end
    end
    
    for(int subVec = 0; subVec < CYC_VEC; subVec++)
      if(v.fmISE[STAGE_RRF_RRC0 + subVec] != null && v.fmISE[STAGE_RRF_RRC0 + subVec].vecEnd) begin
        toSPA = vn.spa[subVec];
        vn.spa[subVec] = null;
        break;
      end
    
    if(v.fmISE[STAGE_RRF_RRC0] != null && v.fmISE[STAGE_RRF_RRC0].sclEnd) begin
      toSPU = vn.spu;
      vn.spu = null;
    end

    ///------------req to other module----------------
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2rfm req, output tr_ise2rfm rsp);
    ovm_report_info("RFM_TR", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_spu(input tr_spu2rfm req, output tr_spu2rfm rsp);
    ovm_report_info("RFM_TR", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2rfm req, output tr_dse2rfm rsp);
    ovm_report_info("RFM_TR", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_spa(input tr_spa2rfm req, output tr_spa2rfm rsp);
    ovm_report_info("RFM_TR", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa
  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time == stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
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
    spu_tr_imp = new("spu_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);

    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_rfm

///-------------------------------------other functions-----------------------------------------
  
  function void ip4_tlm_rfm::read_rf(inout word res, input rbk_sel_e s, uchar i, const ref word cvrf[NUM_VRF_BKS][NUM_SP], csrf[NUM_SRF_BKS], 
                                      bpImm[NUM_BP_IMM], input word imm);
    case(s)
    selv0:    res = cvrf[0][i];
    selv1:    res = cvrf[1][i];
    selv2:    res = cvrf[2][i];
    selv3:    res = cvrf[3][i];
    sels0:    res = csrf[0];
    sels1:    res = csrf[1];
    selz:     res = 0;
    seli0:    res = bpImm[0];
    selii:    res = imm;
    endcase
  endfunction : read_rf
