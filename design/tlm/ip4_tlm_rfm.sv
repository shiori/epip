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

class ip4_tlm_rfm_vars extends ovm_object;
  tr_spu2rfm fm_spu;
  tr_dse2rfm fm_dse;
  tr_spa2rfm fm_spa;
  
  tr_ise2rfm fm_ise[stage_eex_vwb0:0];
  tr_rfm2spa spa[cyc_vec];
///  tr_rfm2dse dse[cyc_vec];
  tr_rfm2spu spu;
  
  `ovm_object_utils_begin(ip4_tlm_rfm_vars)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE)
///    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(spu, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_object_utils_end
  
  function new (string name = "rfm_vars");
    super.new(name);
  endfunction : new
  
  function void gen(input ip4_tlm_rfm_vars o);
    this.copy(o);
  endfunction
endclass : ip4_tlm_rfm_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_rfm extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local word vrf[num_phy_vrf_grp][num_prf_p_grp/num_vrf_bks][num_vrf_bks][cyc_vec][num_sp];
  local word srf[num_phy_srf_grp][num_prf_p_grp/num_vrf_bks][num_srf_bks];
    
  local ip4_tlm_rfm_vars v, vn;
  local word csrf_l[num_srf_bks];
  local word bp_imm_l[num_bp_imm];
  
  `ovm_component_utils_begin(ip4_tlm_rfm)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2rfm, tr_ise2rfm, ip4_tlm_rfm) ise_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2rfm, tr_spu2rfm, ip4_tlm_rfm) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2rfm, tr_dse2rfm, ip4_tlm_rfm) dse_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2rfm, tr_spa2rfm, ip4_tlm_rfm) spa_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_rfm2spa, tr_rfm2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_rfm2dse, tr_rfm2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_rfm2spu, tr_rfm2spu) spu_tr_port;
  
  extern function word read_rf(input rbk_sel_e, uchar, const ref word cvrf[num_vrf_bks][num_sp], csrf[num_srf_bks], 
                                bp_imm[num_bp_imm], input word imm);
  //endfunction
  
  function void comb_proc();
    ovm_report_info("RFM", "comb_proc procing...", OVM_HIGH); 
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_dse != null) end_tr(v.fm_dse);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_ise[0] != null) end_tr(v.fm_ise[0]);
    
    vn.fm_spu = null;
    vn.fm_dse = null;
    vn.fm_spa = null;
    vn.fm_ise[0] = null;

    for(int i=stage_eex_vwb0; i > 0; i--) 
      vn.fm_ise[i] = v.fm_ise[i-1];
  endfunction
  
  function void req_proc();
    tr_rfm2spa to_spa;
    tr_rfm2spu to_spu;
    tr_rfm2dse to_dse;
    word cvrf[num_vrf_bks][num_sp];
    word csrf[num_srf_bks];
      
    ovm_report_info("RFM", "req_proc procing...", OVM_HIGH); 
   
    ///--------------prepare---------------------------------

///    to_spa = tr_rfm2spa::type_id::create("to_spa", this);
///    to_spu = tr_rfm2spu::type_id::create("to_spu", this);
///    to_dse = tr_rfm2dse::type_id::create("to_dse", this);
    
    ///----------------------write back results---------------------
    if(v.fm_spa != null) begin
///      bit cancel = 0;
///      tr_ise2rfm ise = v.fm_ise[stage_rrf_wb0];
      tr_spa2rfm spa = v.fm_spa;
      uchar bk0, bk1;
      
///      if(v.fm_ise[0] != null && v.fm_ise[0].cancel)
///        cancel = 1;
        
///      if(!cancel) begin
      foreach(spa.fu[fid]) begin
        ovm_report_info("RFM_WR", $psprintf("Write Back FU%0d : %s...", fid, fu_cfg[fid].name), OVM_HIGH);
        bk0 = spa.fu[fid].vrf_wr_bk & ('1 - 'b01);
        bk1 = spa.fu[fid].vrf_wr_bk & ('1 - 'b01) + 'b01;
        foreach(spa.fu[0].wen[sp])
          if(spa.fu[fid].wen[sp]) begin
            if(spa.fu[fid].dw) begin
              vrf[spa.fu[fid].vrf_wr_grp][spa.fu[fid].vrf_wr_adr][bk0][spa.fu[fid].subv][sp] = spa.fu[fid].res0[sp];
              vrf[spa.fu[fid].vrf_wr_grp][spa.fu[fid].vrf_wr_adr][bk1][spa.fu[fid].subv][sp] = spa.fu[fid].res1[sp];
            end
            else
              vrf[spa.fu[fid].vrf_wr_grp][spa.fu[fid].vrf_wr_adr][spa.fu[fid].vrf_wr_bk][spa.fu[fid].subv][sp] = spa.fu[fid].res0[sp];
          end
      end
///      end
      
    end
    
    if(v.fm_dse != null) begin
      tr_dse2rfm dse = v.fm_dse;
      ovm_report_info("RFM_WR", "Write Back DSE...", OVM_HIGH);
      if(dse.srf_wr)
        srf[dse.wr_grp][dse.wr_adr][dse.wr_bk] = v.fm_dse.res[0];
      else foreach(dse.wen[sp])
        if(dse.wen[sp])
          vrf[dse.wr_grp][dse.wr_adr][dse.wr_bk][dse.subv][sp] = dse.res[sp];
      if(!dse.srf_wr && dse.br_wr)
        foreach(dse.wen[sp])
          if(dse.wen[sp])
            vrf[dse.br_wr_grp][dse.br_wr_adr][dse.br_wr_bk][dse.subv][sp] = dse.br_res[sp];
    end
    
    if(v.fm_spu != null && v.fm_spu.wen) begin
      word res;
      tr_spu2rfm spu = v.fm_spu;
      tr_spa2rfm spa = v.fm_spa;
      tr_dse2rfm dse = v.fm_dse;
      bit dw = 0;
      ovm_report_info("RFM_WR", "Write Back SPU...", OVM_HIGH);
      res = spu.res;
///      if(spu.sel_dwbp && dse != null)
///        res = dse.res[0];
      srf[spu.srf_wr_grp][spu.srf_wr_adr][spu.srf_wr_bk] = res;
    end
         
    ///----------read registers---------------------
   
    for(int cyc = 0; cyc < cyc_vec; cyc++) begin
      tr_ise2rfm ise = v.fm_ise[stage_rrf_rrc0+cyc];
      
      if(ise == null) continue;
      foreach(cvrf[bk,sp])
        cvrf[bk][sp] = vrf[ise.vrf_rd_grp[bk]][ise.vrf_rd_adr[bk]][bk][cyc][sp];

      foreach(csrf[bk])
        csrf[bk] = srf[ise.srf_rd_grp[bk]][ise.srf_rd_adr[bk]][bk];  
                
      if(ise.start) begin
        csrf_l = csrf;
        bp_imm_l = ise.bp_imm;
      end
      
      foreach(ise.en[fid]) begin
        if(!ise.en[fid]) continue;
        ovm_report_info("RFM_RD", $psprintf("Read for SPA cyc %0d, FU%0d : %s ...", cyc, fid, fu_cfg[fid].name), OVM_HIGH);

        if(vn.spa[cyc] == null) vn.spa[cyc] = tr_rfm2spa::type_id::create("to_spa", this);
        foreach(vn.spa[cyc].fu[fid].rp[rp])
          foreach(vn.spa[cyc].fu[fid].rp[rp].op[sp])
            vn.spa[cyc].fu[fid].rp[rp].op[sp] = read_rf(ise.fu[fid].rd_bk[rp], sp, cvrf, csrf_l, bp_imm_l, ise.fu[fid].imm);
      end
      
      if(ise.dse_en) begin
        ovm_report_info("RFM_RD", $psprintf("Read for DSE cyc %0d ...", cyc), OVM_HIGH);
        if(to_dse == null) to_dse = tr_rfm2dse::type_id::create("to_dse", this);
        foreach(to_dse.base[sp]) begin
          to_dse.base[sp] = read_rf(ise.dse_rd_bk[0], sp, cvrf, csrf, ise.bp_imm, ise.dse_imm);
          to_dse.op1[sp] = read_rf(ise.dse_rd_bk[1], sp, cvrf, csrf, ise.bp_imm, ise.dse_imm);
        end
        if(cyc == 0) begin
///          to_dse.op1 = read_rf(ise.dse_rd_bk[1], 0, cvrf, csrf, ise.bp_imm, ise.dse_imm);
          to_dse.op2 = read_rf(ise.dse_rd_bk[2], 0, cvrf, csrf, ise.bp_imm, ise.dse_imm);
        end
      end
            
      if(ise.spu_en && cyc == 0) begin
        ovm_report_info("RFM_RD", $psprintf("Read for SPU subs %0d ...", cyc), OVM_HIGH);
        if(vn.spu == null) vn.spu = tr_rfm2spu::type_id::create("to_spu", this);
        vn.spu.op0 = read_rf(ise.spu_rd_bk[0], 0, cvrf, csrf, ise.bp_imm, ise.spu_imm);
        vn.spu.op1 = read_rf(ise.spu_rd_bk[1], 0, cvrf, csrf, ise.bp_imm, ise.spu_imm);          
      end
    end
    
    for(int cyc = 0; cyc < cyc_vec; cyc++)
      if(v.fm_ise[cyc] != null && v.fm_ise[cyc].vec_end) begin
        tr_ise2rfm ise = v.fm_ise[cyc];
        to_spa = vn.spa[cyc];
        vn.spa[cyc] = null;
        break;
      end
    
    if(v.fm_ise[stage_rrf_rrc0] != null && v.fm_ise[stage_rrf_rrc0].scl_end) begin
      tr_ise2rfm ise = v.fm_ise[stage_rrf_rrc0];
      to_spu = vn.spu;
      vn.spu = null;
    end
       
    ///------------req to other module----------------
    if(to_spa != null) void'(spa_tr_port.nb_transport(to_spa, to_spa));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2rfm req, output tr_ise2rfm rsp);
    ovm_report_info("RFM_TR", "Get ISE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_ise[0] = req;
///    rsp.set_id_info(req);
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_spu(input tr_spu2rfm req, output tr_spu2rfm rsp);
    ovm_report_info("RFM_TR", "Get SPU Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2rfm req, output tr_dse2rfm rsp);
    ovm_report_info("RFM_TR", "Get DSE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_spa(input tr_spa2rfm req, output tr_spa2rfm rsp);
    ovm_report_info("RFM_TR", "Get SPA Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spa = req;
    return 1;
  endfunction : nb_transport_spa
  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_rfm_vars t;
    if($time == stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_HIGH);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_HIGH);
    ///--------------------synchronizing-------------------
    t = v;
    v = vn;
    vn = t;
    vn.gen(v);
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
    tlm_vif_object vif_cfg;
    
    super.build();
    ise_tr_imp = new("ise_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_rfm

///-------------------------------------other functions-----------------------------------------
///  function void ip4_tlm_rfm::read_crf(input tr_ise2rfm t, uchar vec, ref word cvrf[num_vrf_bks][num_sp], csrf[num_srf_bks]);
///    if(t == null)
///      return;
///  endfunction
  
  function word ip4_tlm_rfm::read_rf(input rbk_sel_e s, uchar i, const ref word cvrf[num_vrf_bks][num_sp], csrf[num_srf_bks], 
                                      bp_imm[num_bp_imm], input word imm);
    case(s)
    selv0:    return cvrf[0][i];
    selv1:    return cvrf[1][i];
    selv2:    return cvrf[2][i];
    selv3:    return cvrf[3][i];
    selv4:    return cvrf[4][i];
    selv5:    return cvrf[5][i];
    selv6:    return cvrf[6][i];
    selv7:    return cvrf[7][i];
    selv8:    return cvrf[8][i];
    selv9:    return cvrf[9][i];
    selv10:   return cvrf[10][i];
    selv11:   return cvrf[11][i];
    selv12:   return cvrf[12][i];
    selv13:   return cvrf[13][i];
    selv14:   return cvrf[14][i];
    selv15:   return cvrf[15][i];
    sels0:    return csrf[0];
    sels1:    return csrf[1];
    sels2:    return csrf[2];
    sels3:    return csrf[3];
    sels4:    return csrf[4];
    sels5:    return csrf[5];
    sels6:    return csrf[6];
    sels7:    return csrf[7];
    sels8:    return csrf[8];
    selz:     return 0;
    seli0:    return bp_imm[0];
    selii:    return imm;
///    seli1:    return bp_imm[1];
///    seli2:    return bp_imm[2];
///    seli3:    return bp_imm[3];
///    seli4:    return bp_imm[4];
    endcase
  endfunction : read_rf
