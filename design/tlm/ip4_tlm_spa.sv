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
  
  function new (string name = "sfu_vars");
    super.new(name);
    subv = 0;
    en = '{default:0};
  endfunction : new  
 
endclass

class ip4_tlm_spa_vars extends ovm_component;
  tr_ise2spa fm_ise[stage_rrf_exe0:0];
  tr_rfm2spa fm_rfm;
  tr_spu2spa fm_spu;
///  tr_dse2spa fm_dse[stage_exe_vwbp:stage_exe_dwb];

  tr_spa2rfm rfm[stage_exe_vwbp:1];
  tr_spa2ise ise[stage_exe_vwbp:1];
  tr_spa2spu spu[stage_exe_cmp:1];
  tr_spa2dse dse[stage_exe_vwbp:1];
    
  ip4_tlm_sfu_stages sfu[stage_eex_vwbp:1];
  
  bit cancel[num_thread];
  
  `ovm_component_utils_begin(ip4_tlm_spa_vars)
///    `ovm_field_sarray_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(sfu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
    cancel = '{default : 0};
  endfunction : new
endclass : ip4_tlm_spa_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_spa extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local ip4_tlm_spa_vars v, vn;
///  local word spu_res_l;
  local bit exe_exp;
  
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
  
  extern function void proc_data(input opcode_e, cmp_opcode_e, pr_merge_e, uchar, uchar,
                                      const ref bit emsk[num_sp], word o[num_fu_rp][num_sp],
                                      ref bit pres0[num_sp], pres1[num_sp], word res0[num_sp], r1[num_sp],
                                      inout uchar exp_flag[num_sp], bit);
  // endfunction

  function void comb_proc();
    ovm_report_info("SPA", "comb_proc procing...", OVM_FULL); 
    if(v.fm_ise[stage_rrf_exe0] != null) end_tr(v.fm_ise[stage_rrf_exe0]);
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_rfm != null) end_tr(v.fm_rfm);

    for(int i = stage_rrf_exe0; i > 0; i--)
      vn.fm_ise[i] = vn.fm_ise[i-1];
    vn.fm_ise[0] = null;
    vn.fm_spu = null;
    vn.fm_rfm = null;

    vn.cancel = '{default : 0};
    for(int i = stage_exe_vwbp; i > 1; i--) begin
      vn.ise[i] = v.ise[i-1];
      vn.rfm[i] = v.rfm[i-1];
      vn.dse[i] = v.dse[i-1];
    end
    vn.ise[1] = null;
    vn.rfm[1] = null;
    vn.dse[1] = null;
          
    for(int i = stage_exe_cmp; i > 1; i--)
      vn.spu[i] = v.spu[i-1];
    vn.spu[1] = null;
    
    ///check for sfu write back conflict
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

    ///----------process data---------------------
    if(v.fm_spu != null && v.fm_ise[stage_rrf_exe0] != null && v.fm_rfm != null) begin
      word op[num_fu_rp][num_sp];
      bit pres_cmp0[num_sp], pres_cmp1[num_sp];
      tr_ise2spa ise = v.fm_ise[stage_rrf_exe0];
      tr_spu2spa spu = v.fm_spu;
      tr_rfm2spa rfm = v.fm_rfm;

      foreach(ise.fu[fid]) begin
        ise2spa_fu fu = ise.fu[fid];
        if(!fu.en) continue;
        ovm_report_info("SPA", $psprintf("Process FU%0d : %s ...", fid, fu_cfg[fid].name), OVM_HIGH); 
        
        if(fu.op inside {spu_only_ops}) begin
          ///long operations
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
            
          proc_data(fu.op, fu.cop, ise.fmerge, ise.subv, spu.exe_mode, spu.fu[fid].emsk, op,
                    pres_cmp0, pres_cmp1, vn.sfu[1].res0[fid], vn.sfu[1].res1[fid],
                    vn.rfm[1].fu[fid].exp_flag, exe_exp);
        end
        else begin
          ///normal operations
          if(vn.rfm[1] == null) vn.rfm[1] = tr_spa2rfm::type_id::create("to_rfm", this);
          vn.rfm[1].fu[fid].vrf_wr_grp = fu.vrf_wr_grp;
          vn.rfm[1].fu[fid].vrf_wr_adr = fu.vrf_wr_adr;
          vn.rfm[1].fu[fid].vrf_wr_bk  = fu.vrf_wr_bk;       
          vn.rfm[1].fu[fid].wen = spu.fu[fid].emsk;
          vn.rfm[1].fu[fid].tid = ise.tid;
          vn.rfm[1].fu[fid].subv = ise.subv;
          vn.rfm[1].fu[fid].en = 1;

          foreach(op[i])
            op[i] = rfm.fu[fid].rp[i].op;
                      
          ///bypass op
          if(fu.op inside {bp_ops}) begin
            foreach(fu_cfg[i])
              foreach(op[rp])
                if(i < fid && fu.bp_sel[rp] == rbk_sel_e'(selfu0 + i))
                  op[rp] = vn.rfm[1].fu[i].res0;
          end
          
          proc_data(fu.op, fu.cop, ise.fmerge, ise.subv, spu.exe_mode, spu.fu[fid].emsk, op,
                    pres_cmp0, pres_cmp1, vn.rfm[1].fu[fid].res0, vn.rfm[1].fu[fid].res1,
                    vn.rfm[1].fu[fid].exp_flag, exe_exp);
        end
        if(fu.op inside {op_cmp, op_ucmp}) begin
          if(vn.spu[1] == null) vn.spu[1] = tr_spa2spu::type_id::create("to_spu", this);
          vn.spu[1].pres_cmp0 = pres_cmp0;
          vn.spu[1].pres_cmp1 = pres_cmp1;
        end
      end
      
      if(ise.subv == ise.vec_mode) begin
        if(vn.ise[1] == null) vn.ise[1] = tr_spa2ise::type_id::create("to_ise", this);
        vn.ise[1].exp = exe_exp;
        vn.ise[1].tid = ise.tid;
        exe_exp = 0;
      end
    end
    
    ///log spa exp to cancel
    if(v.ise[stage_exe_vwbp] != null) begin
      tr_spa2ise ise = v.ise[stage_exe_vwbp];
      vn.cancel[ise.tid] = ise.exp;
    end
        
    ///canceling from ise
    if(v.fm_ise[0] != null) begin
      foreach(vn.cancel[i])
        vn.cancel[i] |= v.fm_ise[0].cancel[i];
    end
    
    foreach(vn.rfm[i])
      if(vn.rfm[i] != null && v.cancel[vn.rfm[i].fu[0].tid]) begin
        vn.rfm[i] = null;
        ovm_report_info("SPA", $psprintf("canceling tid:%0d, at stage %0d", vn.rfm[i].fu[0].tid, i), OVM_HIGH);
      end
  endfunction
  
  function void req_proc();
    tr_spa2rfm to_rfm;
    tr_spa2ise to_ise;
    tr_spa2spu to_spu;
    tr_spa2dse to_dse;
    ovm_report_info("SPA", "req_proc procing...", OVM_FULL); 
        
    to_rfm = v.rfm[stage_exe_vwbp];
    to_ise = v.ise[stage_exe_vwbp];
    to_dse = v.dse[stage_exe_vwbp];
    to_spu = v.spu[stage_exe_cmp];
        
    ///long operations write back
    if(v.sfu[stage_eex] != null) begin
      ip4_tlm_sfu_stages sfu = v.sfu[stage_eex];
      foreach(sfu.en[fid]) begin
        if(!sfu.en[fid]) continue;
        ovm_report_info("sfu", $psprintf("write back tid:%0d", sfu.tid), OVM_HIGH);
        if(to_rfm == null) to_rfm = tr_spa2rfm::type_id::create("to_rfm", this);
        to_rfm.fu[fid].vrf_wr_grp = sfu.vrf_wr_grp[fid];
        to_rfm.fu[fid].vrf_wr_adr = sfu.vrf_wr_adr[fid];
        to_rfm.fu[fid].vrf_wr_bk  = sfu.vrf_wr_bk[fid]; 
        to_rfm.fu[fid].res0 = sfu.res0[fid];
        to_rfm.fu[fid].res1 = sfu.res1[fid];
        to_rfm.fu[fid].wen = sfu.emsk[fid];
        to_rfm.fu[fid].tid = sfu.tid;
        to_rfm.fu[fid].subv = sfu.subv;
        to_rfm.fu[fid].en = 1;
///        if(v.fm_ise[stage_exe_vwbp] != null && v.fm_ise[stage_exe_vwbp].fu[fid].en 
///          && !(v.fm_ise[stage_exe_vwbp].fu[fid].op inside {spu_only_ops}))
///          ovm_report_warning("SPA", "sfu writeback conflict");
      end
    end    

    ///no_fu generation
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
    if(v.cancel[req.tid])
      ovm_report_info("SPA_TR", $psprintf("canceling tid:%0d", req.tid), OVM_HIGH);
    else
      vn.fm_ise[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spa req, output tr_rfm2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get RFM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2spa req, output tr_spu2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get SPU Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2spa req, output tr_dse2spa rsp);
    ovm_report_info("SPA_TR", $psprintf("Get DSE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fm_dse[stage_exe_dwb] = req;
    return 1;
  endfunction : nb_transport_dse
  
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
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
    
  endfunction : build
endclass : ip4_tlm_spa

///-------------------------------------other functions-----------------------------------------
  
function void ip4_tlm_spa::proc_data(input opcode_e op, cmp_opcode_e cop, pr_merge_e fmerge, 
                                    uchar subv, exe_mode, const ref bit emsk[num_sp], word o[num_fu_rp][num_sp],
                                    ref bit pres0[num_sp], pres1[num_sp], word res0[num_sp], r1[num_sp],
                                    inout uchar exp_flag[num_sp], bit exp);
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
  
///  ovm_report_info("SPA_PROC_DATA", $psprintf("subv:%0d, op:%s, res0:%0h, op1:%0h, o1:%0h", subv, op.name, res0[0], op1[0], o[1][0]), OVM_HIGH);
endfunction : proc_data