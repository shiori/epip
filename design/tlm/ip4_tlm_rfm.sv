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
  
  tr_ise2rfm fmISE[STAGE_EEX_VWB:0];
  
  `ovm_component_utils_begin(ip4_tlm_rfm_vars)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
///    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE)
///    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new

endclass : ip4_tlm_rfm_vars

///---------------------------------------main component----------------------------------------
class ip4_tlm_rfm extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local word vrf[NUM_PHY_VRF_GRP][NUM_PRF_P_GRP / NUM_VRF_BKS][NUM_VRF_BKS][CYC_VEC][NUM_SP];
  local word srf[NUM_PHY_SRF_GRP][NUM_PRF_P_GRP / NUM_SRF_BKS][NUM_SRF_BKS];
  local word srIIDx[NUM_THREAD][CYC_VEC][NUM_SP],
             srIIDy[NUM_THREAD][CYC_VEC][NUM_SP],
             srIIDz[NUM_THREAD][CYC_VEC][NUM_SP];
  
  local ip4_tlm_rfm_vars v, vn;
  local word bpCoFu[NUM_BP_CO],
             bpCoDSE[NUM_BP_CO],
             bpCoSPU[NUM_BP_CO];
  local tr_rfm2spa toSPA;
  local tr_rfm2spu toSPU;
  local tr_rfm2dse toDSE;
  local word srExpFlag[NUM_THREAD][CYC_VEC][NUM_SP];
  local bit srMSCO[NUM_THREAD][CYC_VEC][NUM_SP],
            srMSCU[NUM_THREAD][CYC_VEC][NUM_SP],
            srDSEExp[NUM_THREAD][CYC_VEC][NUM_SP];
  
  local word dseSt[2][CYC_VEC][NUM_SP];
  local bit[STAGE_RRF_VWB:1] cancel[NUM_THREAD];
  local tr_rfm2spa spa[CYC_VEC][CYC_VEC];
  local tr_rfm2spu spu[2];
  local word cvrf[NUM_VRF_BKS][NUM_SP];
  local word csrf[NUM_SRF_BKS];
      
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
                                csrf[NUM_SRF_BKS], bpCo[NUM_BP_CO], input word imm);
  //endfunction
  
  function void comb_proc();
    `ip4_info("rfm", "comb_proc procing...", OVM_DEBUG) 
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
  
    for(int i = STAGE_EEX_VWB; i > 0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    dseSt[1] = dseSt[0];
    
    spu[1] = spu[0];
    spu[0] = null;

    for(int i = CYC_VEC - 1; i > 0; i--) 
      spa[i] = spa[i - 1];
    spa[0] = '{default : null};
    
    foreach(cancel[i])
      cancel[i] = cancel[i] << 1;
      
    if(v.fmSPA != null && v.fmSPA.cancel)
      cancel[v.fmSPA.tidCancel] |= '1;

    if(v.fmSPU != null) begin
      tr_spu2rfm spu = v.fmSPU;
      if(spu.expFu)
        cancel[spu.tid] |= `GML(STAGE_RRF_VWB - spu.vecMode);
      if(spu.missBr || spu.expMSC)
        cancel[spu.tid] |=  `GML(STAGE_RRF_CEM);
    end
    
    ///dse self cancel, rfm only cancel next group
    if(v.fmDSE != null && v.fmDSE.exp)
      cancel[v.fmDSE.tidExp] |= `GML(STAGE_RRF_DEM);
  endfunction
  
  function void req_proc();
      
    `ip4_info("rfm", "req_proc procing...", OVM_DEBUG) 
   
    ///----------------------write back results---------------------
    if(v.fmSPA != null) begin /// && !cancel[v.fmSPA.tid][STAGE_RRF_VWB]
      tr_spa2rfm spa = v.fmSPA;
      uchar bk0, bk1;
      
      foreach(spa.fu[fid]) begin
        uchar tid = spa.fu[fid].tid,
              subVec = spa.fu[fid].subVec;
        if(cancel[tid][STAGE_RRF_VWB]) begin
          `ip4_info("rfm_wr", $psprintf("FU%0d write back canceled...", fid), OVM_HIGH) 
          continue;
        end
        if(spa.fu[fid].wr[0] || spa.fu[fid].wr[1])
          `ip4_info("rfm_wr", $psprintf("Write Back FU%0d : %s, vec %0d, grp: %0d, adr %0d, bk %0d ...",
            fid, fu_cfg[fid].name, spa.fu[fid].vec, spa.fu[fid].wrGrp, spa.fu[fid].wrAdr, spa.fu[fid].wrBk), OVM_HIGH)
        bk0 = spa.fu[fid].wr[1] ? (spa.fu[fid].wrBk & `GMH(1)) : spa.fu[fid].wrBk;
        bk1 = bk0 + 1;
        if(!spa.fu[fid].vec) begin
          if(spa.fu[fid].wrEn[0] && spa.fu[fid].subVec == 0) begin
            if(spa.fu[fid].wr[1])
              srf[spa.fu[fid].wrGrp][spa.fu[fid].wrAdr][bk1] = spa.fu[fid].res1[0];
            if(spa.fu[fid].wr[0]) begin
              srf[spa.fu[fid].wrGrp][spa.fu[fid].wrAdr][bk0] = spa.fu[fid].res0[0];
///              if((spa.fu[fid].wrGrp == 3 && spa.fu[fid].wrAdr == 1 && bk0 == 1))begin
///                $display($psprintf("%t write t1 s11: %0d", $time, spa.fu[fid].res0[0]));
//////                $stop;
///              end
              
///              if((spa.fu[fid].wrGrp == 2 && spa.fu[fid].wrAdr == 2 && bk0 == 0))begin
///                $display($psprintf("%t write t1 s4: %0d", $time, spa.fu[fid].res0[0]));
//////                $stop;
///              end
              
               if((spa.fu[fid].wrGrp == 0 && spa.fu[fid].wrAdr == 2 && bk0 == 0))begin
                $display($psprintf("%t write t0 s4: %0d", $time, spa.fu[fid].res0[0]));
///                $stop;
              end
              
///               if((spa.fu[fid].wrGrp == 1 && spa.fu[fid].wrAdr == 3 && bk0 == 1))begin
///                $display($psprintf("%t write s15: %0d", $time, spa.fu[fid].res0[0]));
//////                $stop;
///              end
              
            end
          end
        end
        else foreach(spa.fu[0].wrEn[sp])
          if(spa.fu[fid].wrEn[sp]) begin
            word res0 = spa.fu[fid].res0[sp];
            srExpFlag[tid][subVec][sp] |= spa.fu[fid].expFlag[sp] << (fid * 8);
            if(spa.fu[fid].s2gp)
              case(spa.fu[fid].srAdr)
              SR_IIDX:  res0 = srIIDx[tid][subVec][sp];
              SR_IIDY:  res0 = srIIDy[tid][subVec][sp];
              SR_IIDZ:  res0 = srIIDz[tid][subVec][sp];
              SR_EXPFV: res0 = srExpFlag[tid][subVec][sp];
              SR_DSEEV: res0 = srDSEExp[tid][subVec][sp];
              SR_MSCO:  res0 = srMSCO[tid][subVec][sp];
              SR_MSCU:  res0 = srMSCU[tid][subVec][sp];
              endcase
            if(spa.fu[fid].gp2s)
              case(spa.fu[fid].srAdr)
              SR_IIDX:  srIIDx[tid][subVec][sp] = res0;
              SR_IIDY:  srIIDy[tid][subVec][sp] = res0;
              SR_IIDZ:  srIIDz[tid][subVec][sp] = res0;
              SR_EXPFV: srExpFlag[tid][subVec][sp] = res0;
              SR_DSEEV: srDSEExp[tid][subVec][sp] = res0;
              endcase
                                
            if(spa.fu[fid].wr[1])
              vrf[spa.fu[fid].wrGrp][spa.fu[fid].wrAdr][bk1][spa.fu[fid].subVec][sp] = spa.fu[fid].res1[sp];
            if(spa.fu[fid].wr[0]) begin
              vrf[spa.fu[fid].wrGrp][spa.fu[fid].wrAdr][bk0][spa.fu[fid].subVec][sp] = res0;
///              if(spa.fu[fid].wrGrp ==1 && spa.fu[fid].wrAdr == 1 && bk0 == 3)
///                $display($psprintf("v15[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));
//////              
///              if(spa.fu[fid].wrGrp ==1 && spa.fu[fid].wrAdr == 1 && bk0 == 2)
///                $display($psprintf("v14[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));
//////              
///              if(spa.fu[fid].wrGrp ==0 && spa.fu[fid].wrAdr == 0 && bk0 == 2)
///                $display($psprintf("v2[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));
///                
///              if(spa.fu[fid].wrGrp ==0 && spa.fu[fid].wrAdr == 0 && bk0 == 3)
///                $display($psprintf("v3[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));  
///                
///              if(spa.fu[fid].wrGrp ==3 && spa.fu[fid].wrAdr == 0 && bk0 == 3)
///                $display($psprintf("v27[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));  
//////                
///              if(spa.fu[fid].wrGrp ==2 && spa.fu[fid].wrAdr == 0 && bk0 == 0)
///                $display($psprintf("v16[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));
///              
///               if(spa.fu[fid].wrGrp ==2 && spa.fu[fid].wrAdr == 0 && bk0 == 1)
///                $display($psprintf("v17[%0d] = %0d", spa.fu[fid].subVec * 8 + sp, res0));
            end
          end
      end
    end
    
    if(v.fmDSE != null) begin
      tr_dse2rfm dse = v.fmDSE;
      if(dse.wr)
        `ip4_info("rfm_wr", $psprintf("Write Back DSE: vec %0d, grp: %0d, adr %0d, bk %0d ...",
            dse.vec, dse.wrGrp, dse.wrAdr, dse.wrBk), OVM_HIGH)
      srDSEExp[dse.tid][dse.subVec] = dse.expVec;
      if(dse.vec && dse.wr) begin
        if(cancel[dse.tid][STAGE_RRF_VWB])
          `ip4_info("rfm_wr", "dse vec write back canceled...", OVM_HIGH) 
        else
          foreach(dse.wrEn[sp]) begin
            if(dse.wrEn[sp]) begin
              vrf[dse.wrGrp][dse.wrAdr][dse.wrBk][dse.subVec][sp] = dse.res[sp];
              if(dse.uaWrEn)
                vrf[dse.uaWrGrp][dse.uaWrAdr][dse.uaWrBk][dse.subVec][sp] = dse.uaRes[sp];
            end
          end
      end
      else if(dse.wr) begin
        if(cancel[dse.tid][STAGE_RRF_VWB])
          `ip4_info("rfm_wr", "dse scl write back canceled...", OVM_HIGH) 
        else if(dse.wrEn[0])
          srf[dse.wrGrp][dse.wrAdr][dse.wrBk] = v.fmDSE.res[0];
      end
    end
    
    if(v.fmSPU != null && v.fmSPU.wrEn && !cancel[v.fmSPU.tid][STAGE_RRF_VWB]) begin
      tr_spu2rfm spu = v.fmSPU;
      if(cancel[v.fmSPU.tid][STAGE_RRF_VWB])
        `ip4_info("rfm_wr", "spu write back canceled...", OVM_HIGH) 
      else begin
        `ip4_info("rfm_wr", $psprintf("Write Back SPU: grp: %0d, adr %0d, bk %0d ...",
            spu.srfWrGrp, spu.srfWrAdr, spu.srfWrBk), OVM_HIGH)
        if(spu.wrSrMSC) begin
          srMSCU[spu.tid][spu.subVec] = spu.mscu;
          srMSCO[spu.tid][spu.subVec] = spu.msco;
        end
        if(spu.wrEn)
          srf[spu.srfWrGrp][spu.srfWrAdr][spu.srfWrBk] = spu.res;
      end
    end
         
    ///----------read registers---------------------
   
    for(int subVec = 0; subVec < CYC_VEC; subVec++) begin
      tr_ise2rfm ise = v.fmISE[STAGE_RRF_RRC0 + subVec];
      
      if(ise == null) continue;
                        
      foreach(cvrf[bk, sp])
        cvrf[bk][sp] = vrf[ise.vrfRdGrp[bk]][ise.vrfRdAdr[bk]][bk][subVec][sp];
      
      foreach(csrf[bk])
        csrf[bk] = srf[ise.srfRdGrp[bk]][ise.srfRdAdr[bk]][bk];  
                
      if(ise.cycFu == 0)
        bpCoFu = ise.bpCo;
      if(ise.dseEn && ise.cycDSE == 0)
        bpCoDSE = ise.bpCo;
      if(ise.spuEn && ise.cycSPU == 0)
        bpCoSPU = ise.bpCo;
      
      foreach(ise.fu[fid]) begin
        if(!ise.fu[fid].en) continue;
        `ip4_info("rfm_rd", $psprintf("Read for spa subVec %0d, cyc %0d, Fu%0d : %s ...",
                        subVec, ise.cycFu, fid, fu_cfg[fid].name), OVM_FULL)
        if(spa[ise.cycFu][subVec] == null) spa[ise.cycFu][subVec] = tr_rfm2spa::type_id::create("toSPA", this);
        spa[ise.cycFu][subVec].fu[fid].en = 1;
        foreach(spa[ise.cycFu][subVec].fu[fid].rp[rp])
          foreach(spa[ise.cycFu][subVec].fu[fid].rp[rp].op[sp])
            read_rf(spa[ise.cycFu][subVec].fu[fid].rp[rp].op[sp], ise.fu[fid].rdBkSel[rp],
                    sp, cvrf, csrf, bpCoFu, ise.fu[fid].imm);
      end
      
      if(ise.dseEn && ise.cycDSE < 2) begin  /// && subVec == 0
        `ip4_info("rfm_rd", $psprintf("Read for dse subVec %0d, cyc %0d ...",
                        subVec, ise.cycDSE), OVM_FULL)
        if(toDSE == null) toDSE = tr_rfm2dse::type_id::create("toDSE", this);
        foreach(toDSE.base[sp])
          read_rf(dseSt[ise.cycDSE][subVec][sp], ise.dseRdBk[1], sp, cvrf, csrf, bpCoDSE, ise.dseImm);
        if(ise.cycDSE < 1) begin
          read_rf(toDSE.os, ise.dseRdBk[2], 0, cvrf, csrf, bpCoDSE, ise.dseImm);
          foreach(toDSE.base[sp])
            read_rf(toDSE.base[sp], ise.dseRdBk[0], sp, cvrf, csrf, bpCoDSE, ise.dseImm);
        end
      end
            
      if(ise.spuEn && ise.cycSPU < 2) begin
        `ip4_info("rfm_rd", $psprintf("Read for spu cyc %0d ...", ise.cycSPU), OVM_FULL)
        if(spu[ise.cycSPU] == null) spu[ise.cycSPU] = tr_rfm2spu::type_id::create("toSPU", this);
        read_rf(spu[ise.cycSPU].op0, ise.spuRdBk[0], 0, cvrf, csrf, bpCoSPU, ise.spuImm);
        read_rf(spu[ise.cycSPU].op1, ise.spuRdBk[1], 0, cvrf, csrf, bpCoSPU, ise.spuImm);          
      end
    end
    
    if(v.fmISE[STAGE_RRF_RRC] != null) begin
      tr_ise2rfm ise = v.fmISE[STAGE_RRF_RRC];
      toSPA = spa[CYC_VEC - 1][ise.cycFu];
    end
    
    if(v.fmISE[STAGE_RRF_RRC1] != null && v.fmISE[STAGE_RRF_RRC1].dseEn) begin
      if(toDSE == null) toDSE = tr_rfm2dse::type_id::create("toDSE", this);
      toDSE.st = dseSt[1][v.fmISE[STAGE_RRF_RRC1].cycDSE];
    end
    
    if(v.fmISE[STAGE_RRF_RRC1] != null && v.fmISE[STAGE_RRF_RRC1].spuEn) begin
      toSPU = spu[1];
      spu[1] = null;
    end
  
    ///------------req to other module----------------
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2rfm req, output tr_ise2rfm rsp);
    `ip4_info("rfm_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_spu(input tr_spu2rfm req, output tr_spu2rfm rsp);
    `ip4_info("rfm_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2rfm req, output tr_dse2rfm rsp);
    `ip4_info("rfm_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_spa(input tr_spa2rfm req, output tr_spa2rfm rsp);
    `ip4_info("rfm_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH)
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
  
  function void ip4_tlm_rfm::read_rf(inout word res, input rbk_sel_e s, uchar i, const ref word cvrf[NUM_VRF_BKS][NUM_SP], 
                                    csrf[NUM_SRF_BKS], bpCo[NUM_BP_CO], input word imm);
    case(s)
    selv0:    res = cvrf[0][i];
    selv1:    res = cvrf[1][i];
    selv2:    res = cvrf[2][i];
    selv3:    res = cvrf[3][i];
    sels0:    res = csrf[0];
    sels1:    res = csrf[1];
    selz:     res = 0;
    selc0:    res = bpCo[0];
    selc1:    res = bpCo[1];
    selc2:    res = bpCo[2];
    selc3:    res = bpCo[3];
///    selb0:    res = i;
///    selb1:    res = i << 1;
///    selb2:    res = i << 2;
    selii:    res = imm;
    endcase
  endfunction : read_rf
