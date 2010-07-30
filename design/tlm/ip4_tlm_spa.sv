/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_spa.sv
/// Title            : ip4 stream processor array
/// Version          : 0.1
/// Last modified    : Apr 9 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Apr 7 2010

class ip4_tlm_sfu_stages extends ovm_object;
  bit en[num_fu];
  word res0[num_fu][num_sp], res1[num_fu][num_sp];
  bit emsk[num_fu][num_sp];
  uchar subv, tid, vrf_wr_bk[num_fu], vrf_wr_adr[num_fu], vrf_wr_grp[num_fu];
  
  `ovm_object_utils(ip4_tlm_sfu_stages)
  
  function new (string name = "sfu_stages");
    super.new(name);
    subv = 0;
    en = '{default:0};
  endfunction : new  
 
endclass

class ip4_tlm_spa_vars extends ovm_object;
  tr_ise2spa fm_ise[stage_exe_vwbp:0];
  tr_rfm2spa fm_rfm[stage_exe_vwbp:0];
  tr_spu2spa fm_spu[stage_exe_vwbp:0];
  tr_dse2spa fm_dse[stage_exe_vwbp:stage_exe_dwb];
  
  ip4_tlm_sfu_stages sfu[stage_eex_vwbp:1];
  
  `ovm_object_utils_begin(ip4_tlm_spa_vars)
    `ovm_field_sarray_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(sfu, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_object_utils_end
  
  function new (string name = "spa_vars");
    super.new(name);
  endfunction : new
  
  function void gen(input ip4_tlm_spa_vars o);
    this.copy(o);
  endfunction
endclass : ip4_tlm_spa_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_spa extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local ip4_tlm_spa_vars v, vn;
///  local word grag_base;
///  local word op0_vec[num_fu][cyc_vec][num_sp];
///  local word op1_vec[num_fu][cyc_vec][num_sp];
///  local bit emsk_vec[num_fu][cyc_vec][num_sp];
  local word spu_res_l;
  
  `ovm_component_utils_begin(ip4_tlm_spa)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2spa, tr_ise2spa, ip4_tlm_spa) ise_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2spa, tr_spu2spa, ip4_tlm_spa) spu_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2spa, tr_rfm2spa, ip4_tlm_spa) rfm_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2spa, tr_dse2spa, ip4_tlm_spa) dse_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_spa2rfm, tr_spa2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_spa2ise, tr_spa2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_spa2spu, tr_spa2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_spa2dse, tr_spa2dse) dse_tr_port;
  
  extern function void proc_data(input opcode_e, cmp_opcode_e, pr_merge_e, uchar, 
                                      const ref bit emsk[num_sp], word o[num_fu_rp][num_sp],
                                      ref bit pres0[num_sp], pres1[num_sp], /// pres2[num_sp], pres3[num_sp],
                                      word res0[num_sp], r1[num_sp]);
  // endfunction

///  extern function void proc_data_vec(input uchar, tr_ise2spa, tr_rfm2spa, tr_spu2spa, tr_spa2rfm, tr_spa2spu);
///  // endfunction
    
  function void comb_proc();
    ovm_report_info("SPA", "comb_proc procing...", OVM_FULL); 
    if(v.fm_ise[0] != null) end_tr(v.fm_ise[0]);
    if(v.fm_spu[0] != null) end_tr(v.fm_spu[0]);
    if(v.fm_rfm[0] != null) end_tr(v.fm_rfm[0]);
    if(v.fm_dse[stage_exe_dwb] != null) end_tr(v.fm_dse[stage_exe_dwb]);
    
    vn.fm_ise[0] = null;
    vn.fm_spu[0] = null;
    vn.fm_rfm[0] = null;
    vn.fm_dse[stage_exe_dwb] = null;
     
    for(int i = stage_exe_vwbp; i > 0; i--) begin
      vn.fm_ise[i] = v.fm_ise[i-1];
      vn.fm_rfm[i] = v.fm_rfm[i-1];
      vn.fm_spu[i] = v.fm_spu[i-1];
    end
    
    for(int i = stage_exe_vwbp; i > stage_exe_dwb; i--)
      vn.fm_dse[i] = v.fm_dse[i-1];

    for(int fid = 0; fid < num_fu; fid++) begin
      uchar cnt = 0;
      for(int sg = stage_eex_vwbp; sg > 0; sg--) begin
        if(v.sfu[sg] == null || !v.sfu[sg].en[fid])
          continue;
        cnt++;
        
        if(cnt >= cyc_sfu_busy) begin
          ovm_report_warning("SPA", $psprintf("too many sfu request for fu%0d at stage %0d", fid, sg));
          cnt = 0;
        end
      end
    end
    
    for(int sg = stage_eex; sg > 1; sg--)
      vn.sfu[sg] = v.sfu[sg-1];
    vn.sfu[1] = null;
  endfunction
  
  function void req_proc();
    tr_spa2rfm to_rfm;
    tr_spa2ise to_ise;
    tr_spa2spu to_spu;
    tr_spa2dse to_dse;
///    bit pr2dse[num_sp];
    uchar tmp[cyc_vec][num_sp];
    
    ovm_report_info("SPA", "req_proc procing...", OVM_FULL); 
    
    ///--------------prepare---------------------------------
///    to_rfm = tr_spa2rfm::type_id::create("to_spa", this);
///    to_spu = tr_spa2spu::type_id::create("to_spu", this);
///    to_ise = tr_spa2ise::type_id::create("to_ise", this);
///    to_rfm = v.fm_rfm[lat_mac-1];
///    to_spu = v.fm_spu[lat_mac-1];
       
    ///----------process data---------------------
    if(v.fm_spu[stage_exe_vwbp] != null && v.fm_ise[stage_exe_vwbp] != null && v.fm_rfm[stage_exe_vwbp] != null) begin
      
      word op[num_fu_rp][num_sp];
      tr_ise2spa ise = v.fm_ise[stage_exe_vwbp];
      tr_spu2spa spu = v.fm_spu[stage_exe_vwbp];
      tr_rfm2spa rfm = v.fm_rfm[stage_exe_vwbp];
      tr_dse2spa dse = v.fm_dse[stage_exe_vwbp];
      
      to_spu = tr_spa2spu::type_id::create("to_spu", this);
      to_rfm = tr_spa2rfm::type_id::create("to_rfm", this);
      
      foreach(ise.fu[fid]) begin
        ise2spa_fu fu = ise.fu[fid];
        if(!fu.en) continue;
        ovm_report_info("SPA", $psprintf("Process FU%0d : %s ...", fid, fu_cfg[fid].name), OVM_HIGH); 
      
        to_spu.tid[fid] = ise.tid;
        to_spu.subv[fid] = ise.subv;
      
        to_rfm.fu[fid].vrf_wr_grp = fu.vrf_wr_grp;
        to_rfm.fu[fid].vrf_wr_adr = fu.vrf_wr_adr;
        to_rfm.fu[fid].vrf_wr_bk  = fu.vrf_wr_bk;       
        to_rfm.fu[fid].wen = spu.fu[fid].emsk;
        
        if(fu.op inside {bp_ops}) begin
          word spu_res;
          if(ise.subv == 0)
            spu_res_l = spu.res;
          spu_res = spu_res_l;
          
          foreach(fu_cfg[i])
            foreach(op[rp])
              if(i > fid && fu.bp_sel[rp] == rbk_sel_e'(selfu0 + i))
                op[rp] = to_rfm.fu[i].res0;
                
          if(dse != null)
            foreach(op[rp])
              op[rp] = fu.bp_sel[rp] == seldse ? dse.res : rfm.fu[fid].rp[rp].op;
          
          if(spu != null)
            foreach(op[rp])
              op[rp] = fu.bp_sel[rp] == selspu ? '{default : spu_res} : rfm.fu[fid].rp[rp].op;
        end
        
///        op0_vec[fid][ise.subv] = rfm.fu[fid].rp[0].op;        
///        op1_vec[fid][ise.subv] = rfm.fu[fid].rp[1].op;        
///        emsk_vec[fid][ise.subv] = spu.fu[fid].emsk;
///        
///        if(fu.op inside {op_gglw, op_gglb, op_gglh, op_ggsw, op_ggsh, op_ggsb}) begin
///          foreach(tmp[i,j])
///            if(emsk_vec[fid][i][j]) begin
///              grag_base = op0_vec[fid][i][j];
///              break;
///            end
///        end
        proc_data(fu.op, fu.cop, ise.fmerge, ise.subv, spu.fu[fid].emsk, op,
                  to_spu.pres_cmp0, to_spu.pres_cmp1,/// to_spu.pres_update, pr2dse, 
                  to_rfm.fu[fid].res0, to_rfm.fu[fid].res1);
        
///        proc_data_vec(fid, ise, rfm, spu, to_rfm, to_spu);
        
      end
    end
 
    if(v.fm_rfm[0] != null && v.fm_spu[0] != null && v.fm_ise[0] != null) begin
      tr_ise2spa ise = v.fm_ise[0];
      tr_spu2spa spu = v.fm_spu[0];
      tr_rfm2spa rfm = v.fm_rfm[0];
      bit pres[num_sp];
      word op[num_fu_rp][num_sp];
      foreach(ise.fu[fid]) begin
        ise2spa_fu fu = ise.fu[fid];
        if(!(fu.op inside {spu_only_ops})) continue;
        if (vn.sfu[1] == null) vn.sfu[1] = new();
        vn.sfu[1].emsk[fid] = spu.fu[fid].emsk;
        vn.sfu[1].en[fid] = fu.en;
        vn.sfu[1].vrf_wr_bk[fid] = fu.vrf_wr_bk;
        vn.sfu[1].vrf_wr_adr[fid] = fu.vrf_wr_adr;
        vn.sfu[1].vrf_wr_grp[fid] = fu.vrf_wr_grp;
        vn.sfu[1].tid = ise.tid;
        vn.sfu[1].subv = ise.subv;
        foreach(op[i])
          op[i] = rfm.fu[fid].rp[i].op;
          
        proc_data(fu.op, fu.cop, ise.fmerge, ise.subv, spu.fu[fid].emsk, op,
                  pres, pres, vn.sfu[1].res0[fid], vn.sfu[1].res1[fid]);
      end
    end
   
    if(v.sfu[stage_eex] != null) begin
      ip4_tlm_sfu_stages sfu = v.sfu[stage_eex];
      foreach(sfu.en[fid]) begin
        if(!sfu.en[fid]) continue;
        if(to_spu == null) to_spu = tr_spa2spu::type_id::create("to_spu", this);
        if(to_rfm == null) to_rfm = tr_spa2rfm::type_id::create("to_rfm", this);
        to_spu.subv[fid] = v.sfu[stage_eex].subv;
        to_spu.tid[fid] = v.sfu[stage_eex].tid;
        to_rfm.fu[fid].res0 = v.sfu[stage_eex].res0[fid];
        to_rfm.fu[fid].res1 = v.sfu[stage_eex].res1[fid];
        to_rfm.fu[fid].wen = v.sfu[stage_eex].emsk[fid];
        if(v.fm_ise[stage_exe_vwbp] != null && v.fm_ise[stage_exe_vwbp].fu[fid].en 
          && !(v.fm_ise[stage_exe_vwbp].fu[fid].op inside {spu_only_ops}))
          ovm_report_warning("SPA", "sfu writeback conflict");
      end
    end
    
    if(v.fm_ise[stage_exe_vwbp] != null && v.fm_ise[stage_exe_vwbp].bp_rf_dse inside {[selfu0:selfu0+num_fu]}) begin
      tr_ise2spa ise = v.fm_ise[stage_exe_vwbp];
      if(to_dse == null) to_dse = tr_spa2dse::type_id::create("to_dse", this);
      if(to_rfm == null) begin
        to_dse.res = '{default:0};
///        to_dse.emsk = '{default:0};
      end
      else begin
        uchar fu_sel = uchar'(ise.bp_rf_dse) - uchar'(selfu0);
        assert(fu_sel < num_fu);
        if(ise.bp_rf_dse_wp == 0)
          to_dse.res = to_rfm.fu[fu_sel].res0;
        else
          to_dse.res = to_rfm.fu[fu_sel].res1;
///        to_dse.emsk = pr2dse;
      end
    end
    
    if(v.fm_ise[0] != null && v.fm_ise[0].cancel) begin
      ovm_report_info("SPA", "Canceling FU...", OVM_HIGH);
      for(int i = 0; i < cyc_vec; i++) begin
        if(vn.sfu[stage_exe_vwb0-i] != null && vn.sfu[stage_exe_vwb0-i].tid == v.fm_ise[0].tid_cancel)
          vn.sfu[stage_exe_vwb0] = null;
        
        if(v.fm_ise[stage_exe_vwb0-i] != null && v.fm_ise[stage_exe_vwb0-i].tid == v.fm_ise[0].tid_cancel && v.fm_spu[stage_exe_vwb0-i] != null)
          foreach(v.fm_spu[stage_exe_vwb0-i].fu[fid])
            v.fm_spu[stage_exe_vwb0-i].fu[fid].emsk = '{default : 0};
      end
    end
    
    for(int i = ck_stage_sfu0; i <= ck_stage_sfu1; i++)
      foreach(v.sfu[0].en[fid])
        if(v.sfu[i] != null && v.sfu[i].en[fid]) begin
          if(to_ise == null) to_ise = tr_spa2ise::type_id::create("to_ise", this);
          to_ise.no_fu[fid] = 1;
        end
          
    ///------------req to other module----------------
    if(to_rfm != null) void'(rfm_tr_port.nb_transport(to_rfm, to_rfm));
    if(to_ise != null) void'(ise_tr_port.nb_transport(to_ise, to_ise));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2spa req, output tr_ise2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get ISE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_ise[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spa req, output tr_rfm2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get RFM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm[0] = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2spa req, output tr_spu2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get SPU Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu[0] = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2spa req, output tr_dse2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get DSE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse[stage_exe_dwb] = req;
    return 1;
  endfunction : nb_transport_dse
  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_spa_vars t;
    if($time == stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
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
    rfm_tr_imp = new("rfm_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_spa

///-------------------------------------other functions-----------------------------------------
  
function void ip4_tlm_spa::proc_data(input opcode_e op, cmp_opcode_e cop, pr_merge_e fmerge, 
                                    uchar subv, const ref bit emsk[num_sp], word o[num_fu_rp][num_sp],
                                    ref bit pres0[num_sp], pres1[num_sp], ///pres2[num_sp], pres3[num_sp],
                                    word res0[num_sp], r1[num_sp]);
  bit pres[num_sp];
  bit[word_width:0] op0[num_sp], op1[num_sp], op2[num_sp], op3[num_sp], r0[num_sp] = '{default:0};
  
  foreach(op0[i]) begin
    op0[i] = {o[0][i][word_width-1], o[0][i]};
    op1[i] = {o[1][i][word_width-1], o[1][i]};
    op2[i] = {o[2][i][word_width-1], o[2][i]};
    op3[i] = {o[3][i][word_width-1], o[3][i]};
  end
  
  case(op)
  op_nop,   
  op_bp0:   foreach(r0[i]) r0[i] = op0[i];
  op_bp1:   foreach(r0[i]) r0[i] = op1[i];
  op_bp2:   foreach(r0[i]) r0[i] = op2[i];
  op_bp3:   foreach(r0[i]) r0[i] = op3[i];
  
  op_umul:  foreach(r0[i]) r0[i] = unsigned'(o[0][i]) * unsigned'(o[1][i]);
  op_smul:  foreach(r0[i]) r0[i] = signed'(op0[i]) * signed'(op1[i]);
  op_umad:  foreach(r0[i]) r0[i] = unsigned'(o[0][i]) * unsigned'(o[1][i]) + unsigned'(o[2][i]);
  op_smad:  foreach(r0[i]) r0[i] = signed'(op0[i]) * signed'(op1[i]) + signed'(op2[i]);
  op_umsu:  foreach(r0[i]) r0[i] = unsigned'(o[2][i]) - unsigned'(o[0][i]) * unsigned'(o[1][i]);
  op_smsu:  foreach(r0[i]) r0[i] = -signed'(op0[i]) * signed'(op1[i]) + signed'(op2[i]);
  op_udmul: foreach(r0[i]) {r0[i], r1[i]} = unsigned'(o[0][i]) * unsigned'(o[1][i]);
  op_sdmul: foreach(r0[i]) {r0[i], r1[i]} = signed'(op0[i]) * signed'(op1[i]);
  op_udmad: foreach(r0[i]) {r0[i], r1[i]} = unsigned'(o[0][i]) * unsigned'(o[1][i]) + {1'b0, o[3][i], o[2][i]};
  op_sdmad: foreach(r0[i]) {r0[i], r1[i]} = signed'(op0[i]) * signed'(op1[i]) + signed'({op3[i], o[2][i]});
  op_udmsu: foreach(r0[i]) {r0[i], r1[i]} = {1'b0, o[3][i], o[2][i]} - unsigned'(o[0][i]) * unsigned'(o[0][i]);
  op_sdmsu: foreach(r0[i]) {r0[i], r1[i]} = -signed'(op0[i]) * signed'(op1[i]) + signed'({op3[i], o[2][i]});
  op_add3:  foreach(r0[i]) {r0[i], r1[i]} = signed'(op0[i]) + signed'(op1[i]) + signed'(op2[i]);
  op_uadd3: foreach(r0[i]) {r0[i], r1[i]} = unsigned'(o[0][i]) + unsigned'(o[1][i]) + unsigned'(o[2][i]);

  op_and:   foreach(r0[i]) r0[i] = o[0][i] & o[1][i];
  op_or:    foreach(r0[i]) r0[i] = o[0][i] | o[1][i];
  op_xor:   foreach(r0[i]) r0[i] = o[0][i] ^ o[1][i];
  op_nor:   foreach(r0[i]) r0[i] = ~(o[0][i] | o[1][i]);
  op_add:   foreach(r0[i]) r0[i] = signed'(op0[i]) + signed'(op1[i]);
  op_uadd:  foreach(r0[i]) r0[i] = op0[i] + op1[i];
  op_sub:   foreach(r0[i]) r0[i] = signed'(op0[i]) - signed'(op1[i]);
  op_usub:  foreach(r0[i]) r0[i] = unsigned'(o[0][i]) - unsigned'(o[1][i]);
  op_srl:   foreach(r0[i]) r0[i] = o[0][i] >> o[1][i];
  op_sra:   foreach(r0[i]) r0[i] = o[0][i] >>> o[1][i];
  op_sll:   foreach(r0[i]) r0[i] = o[0][i] << o[1][i];
  op_ror:   foreach(r0[i]) {r1[i], r0[i]} = {o[0][i], o[0][i]} >> o[1][i];
  op_lid:   foreach(r0[i]) r0[i] = o[0][i] + i + subv;
  op_shf4:  foreach(r0[i]) r0[i] = op0[op1[i][2:0]];
  op_max:   foreach(r0[i]) r0[i] = op0[i] > op1[i] ? op0[i] : op1[i];
  op_min:   foreach(r0[i]) r0[i] = op0[i] > op1[i] ? op1[i] : op0[i];
  op_umax:  foreach(r0[i]) r0[i] = o[0][i] > o[1][i] ? o[0][i] : o[1][i];
  op_umin:  foreach(r0[i]) r0[i] = o[0][i] > o[1][i] ? o[1][i] : o[0][i];
  op_clo:   
    foreach(r0[i])
      for(int j=word_width-1; j>=0; j--)
        if(o[0][i][j])
          r0[i]++;
        else
          break;
  op_clz:
    foreach(r0[i])
      for(int j=word_width-1; j>=0; j--)
        if(!o[0][i][j])
          r0[i]++;
        else
          break;
  op_ext:   ovm_report_warning("SPA_UNIMP", "ext is not implemented yet");
  op_ins:   ovm_report_warning("SPA_UNIMP", "ins is not implemented yet");
  op_seb:   ovm_report_warning("SPA_UNIMP", "seb is not implemented yet");
  op_she:   ovm_report_warning("SPA_UNIMP", "she is not implemented yet");
  op_wsbh:  ovm_report_warning("SPA_UNIMP", "wsbh is not implemented yet");
  
  op_cmp,
  op_ucmp,  
///  op_ggsw,
///  op_ggsh,
///  op_ggsb,
///  op_pera,
///  op_perb:  begin end /// those are not processed by this case statement
///  op_vsl,
///  op_vslu,
///  op_vror,
///  op_vroru,
///  op_vsr,
///  op_vsru:
///    begin
///    end  /// those are not processed by this case statement
///  op_gglw:
///    begin
///      word base = grag_base >> 7;
///      pres2 = emsk;
///      foreach(op0[sp])
///        if(base == (o[0][sp] >> 7)) begin
///          r0[sp] = (o[0][sp] >> 2) & 'b011111;
///          pres2[sp] = 0;
///          pres0[sp] = 1;
///        end
///    end
///  op_gglb:
///    begin
///      word base = grag_base >> 6;
///      pres2 = emsk;
///      foreach(op0[sp])
///        if(base == (o[0][sp] >> 6)) begin
///          r0[sp] = (o[0][sp] >> 1) & 'b011111;
///          pres2[sp] = 0;
///          pres0[sp] = 1;
///        end
///    end
///  op_gglh:
///    begin
///      word base = grag_base >> 5;
///      pres2 = emsk;
///      foreach(op0[sp])
///        if(base == (o[0][sp] >> 5)) begin
///          r0[sp] = o[0][sp] & 'b011111;
///          pres2[sp] = 0;
///          pres0[sp] = 1;
///        end
///    end
  op_div:   foreach(r0[i]) begin r0[i] = op0[i] / op1[i]; r1[i] = op0[i] % op1[i]; end
  op_udiv:  foreach(r0[i]) begin r0[i] = o[0][i] / o[1][i]; r1[i] = o[0][i] % o[1][i]; end
  op_quo:   foreach(r0[i]) r0[i] = op0[i] / op1[i];
  op_uquo:  foreach(r0[i]) r0[i] = o[0][i] / o[1][i];
  op_res:   foreach(r0[i]) r0[i] = op0[i] % op1[i];
  op_ures:  foreach(r0[i]) r0[i] = o[0][i] % o[1][i];
  
  default:  ovm_report_warning("SPA_ILLEGAL", "Illegal instruction opcode!!!");
  endcase
  
  foreach(res0[i])
    res0[i] = r0[i][word_width-1:0];
    
  if(op == op_cmp)
    case(cop)
    cop_e:    foreach(pres[i]) pres[i] = op0[i] == op1[i];
    cop_g:    foreach(pres[i]) pres[i] = signed'(op0[i]) > signed'(op1[i]);
    cop_l:    foreach(pres[i]) pres[i] = signed'(op0[i]) < signed'(op1[i]);
    cop_ge:   foreach(pres[i]) pres[i] = signed'(op0[i]) >= signed'(op1[i]);
    cop_le:   foreach(pres[i]) pres[i] = signed'(op0[i]) <= signed'(op1[i]);
    default:  ovm_report_warning("SPA_ILLEGAL", "Illegal cop!!!");
    endcase    
  else if(op == op_ucmp)
    case(cop)
    cop_e:    foreach(pres[i]) pres[i] = op0[i] == op1[i];
    cop_g:    foreach(pres[i]) pres[i] = unsigned'(o[0][i]) > unsigned'(o[1][i]);
    cop_l:    foreach(pres[i]) pres[i] = unsigned'(o[0][i]) < unsigned'(o[1][i]);
    cop_ge:   foreach(pres[i]) pres[i] = unsigned'(o[0][i]) >= unsigned'(o[1][i]);
    cop_le:   foreach(pres[i]) pres[i] = unsigned'(o[0][i]) <= unsigned'(o[1][i]);
    default:  ovm_report_warning("SPA_ILLEGAL", "Illegal cop!!!");
    endcase  
  
  case(fmerge)
  pm_nop:
    foreach(pres[i]) begin
      pres0[i] = pres[i];
      pres1[i] = pres[i];
    end
  pm_unc:
    foreach(pres[i]) 
      if(emsk[i]) begin
        pres0[i] = pres[i];
        pres1[i] = pres[i];
      end
      else begin
        pres0[i] = 0;
        pres1[i] = 0;          
      end
  pm_and:
    foreach(pres[i]) 
      if(emsk[i] && !pres[i]) begin
        pres0[i] = 0;
        pres1[i] = 0;
      end
  pm_andcm:
    foreach(pres[i]) 
      if(emsk[i] && pres[i]) begin
        pres0[i] = 0;
        pres1[i] = 0;
      end    
  pm_or:
    foreach(pres[i])
      if(emsk[i] && !pres[i]) begin
        pres0[i] = 1;
        pres1[i] = 1;
      end 
  pm_orcm:
    foreach(pres[i]) 
      if(emsk[i] && pres[i]) begin
        pres0[i] = 1;
        pres1[i] = 1;
      end
  pm_and_orcm:
    foreach(pres[i]) 
      if(emsk[i] && !pres[i]) begin
        pres0[i] = 0;
        pres1[i] = 1;
      end
  pm_or_andcm:
    foreach(pres[i]) 
      if(emsk[i] && pres[i]) begin
        pres0[i] = 1;
        pres1[i] = 0;
      end
  default:  ovm_report_warning("SPA_ILLEGAL", "Illegal fmerge!!!");
  endcase
endfunction : proc_data
        
///function void ip4_tlm_spa::proc_data_vec(input uchar fid, tr_ise2spa ise, tr_rfm2spa rfm, tr_spu2spa spu,
///                                         tr_spa2rfm to_rfm, tr_spa2spu to_spu);
///  uchar tmp[cyc_vec][num_sp];
///  ise2spa_fu fu = ise.fu[fid];
///  
///  case(fu.op)
///  op_ggsw:
///    begin
///      to_spu.pres_update = spu.fu[fid].emsk;
///      foreach(to_rfm.fu[0].res0[k])
///        foreach(tmp[i,j]) begin
///          uchar vid = (op0_vec[fid][i][j] >> 2) & 'b011111;
///          if((vid == (ise.subv * num_sp + k)) && spu.fu[fid].emsk[k]
///              && ((grag_base >> 7) == (op0_vec[fid][i][j] >> 7))) begin
///            to_spu.pres_cmp0[k] = 1;
///            to_spu.pres_update[k] = 0;
///            to_rfm.fu[fid].res0[k] = i * num_sp + j; 
///          end
///        end
///    end
///  op_ggsh:
///    begin
///      to_spu.pres_update = spu.fu[fid].emsk;
///      foreach(to_rfm.fu[0].res0[k])
///        foreach(tmp[i,j]) begin
///          uchar vid = (op0_vec[fid][i][j] >> 1) & 'b011111;
///          if((vid == (ise.subv * num_sp + k)) && spu.fu[fid].emsk[k]
///              && ((grag_base >> 6) == (op0_vec[fid][i][j] >> 6))) begin
///            to_spu.pres_cmp0[k] = 1;
///            to_spu.pres_update[k] = 0;
///            to_rfm.fu[fid].res0[k] = i * num_sp + j; 
///          end
///        end
///    end
///  op_ggsb:
///    begin
///      to_spu.pres_update = spu.fu[fid].emsk;
///      foreach(to_rfm.fu[0].res0[k])
///        foreach(tmp[i,j]) begin
///          uchar vid = op0_vec[fid][i][j] & 'b011111;
///          if((vid == (ise.subv * num_sp + k)) && spu.fu[fid].emsk[k]
///              && ((grag_base >> 5) == (op0_vec[fid][i][j] >> 5))) begin
///            to_spu.pres_cmp0[k] = 1;
///            to_spu.pres_update[k] = 0;
///            to_rfm.fu[fid].res0[k] = i * num_sp + j; 
///          end
///        end
///    end              
///    op_pera:
///    ///wen is emsk after permute
///      begin
///        foreach(rfm.fu[0].rp[2].op[sp]) begin
///          uchar subv = rfm.fu[fid].rp[2].op[sp] >> 3,
///                sel = rfm.fu[fid].rp[2].op[sp] & 'b0111;
///          if(rfm.fu[fid].rp[2].op[sp] < 32) begin
///            to_rfm.fu[fid].res0[sp] = op0_vec[fid][subv][sel];
///            to_rfm.fu[fid].wen[sp] = emsk_vec[fid][subv][sel];
///          end
///          else if(rfm.fu[fid].rp[2].op[sp] < 64) begin
///            to_rfm.fu[fid].res0[sp] = op1_vec[fid][subv][sel];
///            to_rfm.fu[fid].wen[sp] = emsk_vec[fid][subv][sel];
///          end
///          else
///            to_rfm.fu[fid].wen[sp] = 0;
///        end
///      end
///    op_perb:
///    ///wen is emsk before permute
///      begin
///        to_rfm.fu[fid].wen = spu.fu[fid].emsk;
///        foreach(rfm.fu[0].rp[2].op[sp]) begin
///          uchar subv = rfm.fu[fid].rp[2].op[sp] >> 3,
///                sel = rfm.fu[fid].rp[2].op[sp] & 'b0111;
///          if(rfm.fu[fid].rp[2].op[sp] < 32)
///            to_rfm.fu[fid].res0[sp] = op0_vec[fid][subv][sel];
///          else if(rfm.fu[fid].rp[2].op[sp] < 64)
///            to_rfm.fu[fid].res0[sp] = op1_vec[fid][subv][sel];
///          else
///            to_rfm.fu[fid].wen[sp] = 0;
///        end
///      end
///  op_vsl,
///  op_vslu,
///  op_vror,
///  op_vroru,
///  op_vsr,
///  op_vsru:
///    begin
///    end  /// those are not processed by this case statement            
///  endcase
///endfunction : proc_data_vec