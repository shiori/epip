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
  bit vec[NUM_FU], en[NUM_FU], wr[NUM_FU][2];
  word res0[NUM_FU][NUM_SP], res1[NUM_FU][NUM_SP];
  bit emsk[NUM_FU][NUM_SP];
  uchar subVec, tid, wrBk[NUM_FU], wrAdr[NUM_FU], wrGrp[NUM_FU];
  
  `ovm_object_utils(ip4_tlm_sfu_stages)
  
  function new (string name = "sfu_vars");
    super.new(name);
    subVec = 0;
    vec = '{default : 1};
    en = '{default:0};
  endfunction : new  
 
endclass

class ip4_tlm_spa_vars extends ovm_component;
  tr_ise2spa fmISE[STAGE_RRF_EXE0:0];
  tr_rfm2spa fmRFM;
  tr_spu2spa fmSPU;

  tr_spa2rfm rfm[STAGE_EXE_VWBP:1];
  tr_spa2ise ise[STAGE_EXE_VWBP:1];
  tr_spa2spu spu[STAGE_EXE_CMP:1];
  tr_spa2dse dse[STAGE_EXE_VWBP:1];
    
  ip4_tlm_sfu_stages sfu[STAGE_EEX_VWBP:1];
  
///  bit cancel[NUM_THREAD];
  
  `ovm_component_utils_begin(ip4_tlm_spa_vars)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(sfu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE)
///    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
///    cancel = '{default : 0};
  endfunction : new
endclass : ip4_tlm_spa_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_spa extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
    
  local ip4_tlm_spa_vars v, vn;
  local bit exeExp;
  
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
                                      const ref bit emsk[NUM_SP], word o[NUM_FU_RP][NUM_SP],
                                      ref bit pres0[NUM_SP], pres1[NUM_SP], word res0[NUM_SP], r1[NUM_SP],
                                      inout uint expFlag[NUM_SP]);
  // endfunction

  function void comb_proc();
    `ip4_info("spa", "comb_proc procing...", OVM_DEBUG) 
    if(v.fmISE[STAGE_RRF_EXE0] != null) end_tr(v.fmISE[STAGE_RRF_EXE0]);
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmRFM != null) end_tr(v.fmRFM);

    for(int i = STAGE_RRF_EXE0; i > 0; i--)
      vn.fmISE[i] = vn.fmISE[i - 1];
    vn.fmISE[0] = null;
    vn.fmSPU = null;
    vn.fmRFM = null;

///    vn.cancel = '{default : 0};
    for(int i = STAGE_EXE_VWBP; i > 1; i--) begin
      vn.rfm[i] = v.rfm[i - 1];
      vn.dse[i] = v.dse[i - 1];
    end
    vn.rfm[1] = null;
    vn.dse[1] = null;
        
    for(int i = STAGE_EXE_VWBP; i > 1; i--)
      vn.ise[i] = v.ise[i - 1];
    vn.ise[1] = null;
          
    for(int i = STAGE_EXE_CMP; i > 1; i--)
      vn.spu[i] = v.spu[i - 1];
    vn.spu[1] = null;
    
    ///check for sfu write back conflict
    for(int fid = 0; fid < NUM_FU; fid++) begin
      uchar cnt = 0, st = 0;
      for(int sg = 1; sg <= CYC_SFU_BUSY; sg++) begin
        if(v.sfu[sg] == null || !v.sfu[sg].en[fid])
          continue;
        cnt++;
        
        if(cnt > CYC_VEC) begin
          ovm_report_warning("spa", $psprintf("too many sfu request for fu%0d at stage %0d", fid, sg));
          cnt = 0;
        end
      end
    end
    
    for(int sg = STAGE_EEX; sg > 1; sg--)
      vn.sfu[sg] = v.sfu[sg-1];
    vn.sfu[1] = null;

    ///----------process data---------------------
    if(v.fmSPU != null && v.fmISE[STAGE_RRF_EXE0] != null && v.fmRFM != null) begin
      word op[NUM_FU_RP][NUM_SP];
      bit presCmp0[NUM_SP], presCmp1[NUM_SP];
      tr_ise2spa ise = v.fmISE[STAGE_RRF_EXE0];
      tr_spu2spa spu = v.fmSPU;
      tr_rfm2spa rfm = v.fmRFM;

      foreach(ise.fu[fid]) begin
        ise2spa_fu fu = ise.fu[fid];
        if(!fu.en) continue;
        `ip4_info("spa", $psprintf("Process FU%0d : %s ...", fid, fu_cfg[fid].name), OVM_FULL) 
        
        if(fu.op inside {sfu_only_ops}) begin
          uint expFlag[NUM_SP];
          ///long operations
          if (vn.sfu[1] == null) vn.sfu[1] = new();
          vn.sfu[1].emsk[fid] = spu.fu[fid].emsk;
          vn.sfu[1].en[fid] = fu.en;
          vn.sfu[1].wrBk[fid] = fu.wrBk;
          vn.sfu[1].wrAdr[fid] = fu.wrAdr;
          vn.sfu[1].wrGrp[fid] = fu.wrGrp;
          vn.sfu[1].wr[fid] = fu.wrEn;
          vn.sfu[1].vec[fid] = fu.vec;
          vn.sfu[1].tid = ise.tid;
          vn.sfu[1].subVec = ise.subVec;
          foreach(op[i])
            op[i] = rfm.fu[fid].rp[i].op;
          
          ///exp check is disabled for long ops
          proc_data(fu.op, fu.cop, ise.prMerge, ise.subVec, ise.rndMode, spu.fu[fid].emsk, op,
                    presCmp0, presCmp1, vn.sfu[1].res0[fid], vn.sfu[1].res1[fid], expFlag);
        end
        else begin
          ///normal operations
          if(vn.rfm[1] == null) vn.rfm[1] = tr_spa2rfm::type_id::create("toRFM", this);
          vn.rfm[1].fu[fid].wrGrp = fu.wrGrp;
          vn.rfm[1].fu[fid].wrAdr = fu.wrAdr;
          vn.rfm[1].fu[fid].wrBk  = fu.wrBk;
          vn.rfm[1].fu[fid].vec  = fu.vec;
          vn.rfm[1].fu[fid].wrEn = spu.fu[fid].emsk;
          vn.rfm[1].fu[fid].wr = ise.fu[fid].wrEn;
          vn.rfm[1].fu[fid].tid = ise.tid;
          vn.rfm[1].fu[fid].subVec = ise.subVec;
          vn.rfm[1].fu[fid].en = 1;

          foreach(op[i])
            op[i] = rfm.fu[fid].rp[i].op;
                      
          ///bypass op
          if(fu.op inside {bp_ops}) begin
            foreach(fu_cfg[i])
              foreach(op[rp])
                if(i < fid && fu.bpSel[rp] == rbk_sel_e'(selfu0 + i))
                  op[rp] = vn.rfm[1].fu[i].res0;
          end
          
          proc_data(fu.op, fu.cop, ise.prMerge, ise.subVec, ise.rndMode, spu.fu[fid].emsk, op,
                    presCmp0, presCmp1, vn.rfm[1].fu[fid].res0, vn.rfm[1].fu[fid].res1,
                    vn.rfm[1].fu[fid].expFlag);
          foreach(vn.rfm[1].fu[0].expFlag[sp])
            if(vn.rfm[1].fu[fid].expFlag[sp] != 0)
              exeExp = 1;
        end
        if(fu.op inside {op_cmp, op_ucmp}) begin
          if(vn.spu[1] == null) vn.spu[1] = tr_spa2spu::type_id::create("toSPU", this);
          vn.spu[1].presCmp0 = presCmp0;
          vn.spu[1].presCmp1 = presCmp1;
        end
        
        if(fu.op inside {op_gp2s, op_s2gp}) begin
          if(vn.rfm[1] == null) vn.rfm[1] = tr_spa2rfm::type_id::create("toRFM", this);
          vn.rfm[1].fu[fid].gp2s = fu.op == op_gp2s;
          vn.rfm[1].fu[fid].s2gp = fu.op == op_s2gp;
        end
      end
      
      ///signal exp when whole request finished
      if(ise.subVec == ise.vecMode) begin
        if(vn.ise[1] == null) vn.ise[1] = tr_spa2ise::type_id::create("toISE", this);
        if(vn.rfm[1] == null) vn.rfm[1] = tr_spa2rfm::type_id::create("toRFM", this);
        vn.ise[1].exp = exeExp && !ise.noExp;
        vn.ise[1].tid = ise.tid;
        vn.rfm[1].tidCancel = ise.tid;
        exeExp = 0;
      end
    end
  endfunction
  
  function void req_proc();
    tr_spa2rfm toRFM;
    tr_spa2ise toISE;
    tr_spa2spu toSPU;
    tr_spa2dse toDSE;
    `ip4_info("spa", "req_proc procing...", OVM_DEBUG) 
        
    toRFM = v.rfm[STAGE_EXE_VWBP];
    toISE = v.ise[STAGE_EXE_VWBP];
    toDSE = v.dse[STAGE_EXE_VWBP];
    toSPU = v.spu[STAGE_EXE_CMP];
        
    ///long operations write back
    if(v.sfu[STAGE_EEX_VWBP] != null) begin
      ip4_tlm_sfu_stages sfu = v.sfu[STAGE_EEX_VWBP];
      foreach(sfu.en[fid]) begin
        if(!sfu.en[fid]) continue;
        `ip4_info("sfu", $psprintf("write back tid:%0d", sfu.tid), OVM_FULL)
        if(toRFM == null) toRFM = tr_spa2rfm::type_id::create("toRFM", this);
        toRFM.fu[fid].wrGrp = sfu.wrGrp[fid];
        toRFM.fu[fid].wrAdr = sfu.wrAdr[fid];
        toRFM.fu[fid].wrBk  = sfu.wrBk[fid]; 
        toRFM.fu[fid].res0 = sfu.res0[fid];
        toRFM.fu[fid].res1 = sfu.res1[fid];
        toRFM.fu[fid].wrEn = sfu.emsk[fid];
        toRFM.fu[fid].vec = sfu.vec[fid];
        toRFM.fu[fid].wr = sfu.wr[fid];
        toRFM.fu[fid].tid = sfu.tid;
        toRFM.fu[fid].subVec = sfu.subVec;
        toRFM.fu[fid].en = 1;
        if(v.fmISE[STAGE_EXE_VWBP] != null && v.fmISE[STAGE_EXE_VWBP].fu[fid].en 
          && !(v.fmISE[STAGE_EXE_VWBP].fu[fid].op inside {sfu_only_ops}))
          ovm_report_warning("spa", "sfu writeback conflict");
      end
    end    

    ///noFu generation
    for(int i = CK_STAGE_SFU0; i <= CK_STAGE_SFU1; i++)
      foreach(v.sfu[0].en[fid])
        if(v.sfu[i] != null && v.sfu[i].en[fid]) begin
          if(toISE == null) toISE = tr_spa2ise::type_id::create("toISE", this);
          toISE.noFu[fid] = 1;
        end    
                  
    ///------------req to other module----------------
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2spa req, output tr_ise2spa rsp);
    `ip4_info("spa_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    if(v.cancel[req.tid])
///      `ip4_info("spa_tr", $psprintf("canceling tid:%0d", req.tid), OVM_FULL)
///    else
    vn.fmISE[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spa req, output tr_rfm2spa rsp);
    `ip4_info("spa_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2spa req, output tr_spu2spa rsp);
    `ip4_info("spa_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_dse(input tr_dse2spa req, output tr_dse2spa rsp);
    `ip4_info("spa_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmDSE[stage_exe_dwb] = req;
    return 1;
  endfunction : nb_transport_dse
  
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
    rfm_tr_imp = new("rfm_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    
  endfunction : build
endclass : ip4_tlm_spa

///-------------------------------------other functions-----------------------------------------
  
function void ip4_tlm_spa::proc_data(input opcode_e op, cmp_opcode_e cop, pr_merge_e prMerge, 
                                    uchar subVec, exeMode, const ref bit emsk[NUM_SP], word o[NUM_FU_RP][NUM_SP],
                                    ref bit pres0[NUM_SP], pres1[NUM_SP], word res0[NUM_SP], r1[NUM_SP],
                                    inout uint expFlag[NUM_SP]);
  bit pres[NUM_SP];
  bit[WORD_BITS:0] op0[NUM_SP], op1[NUM_SP], op2[NUM_SP], op3[NUM_SP], r0[NUM_SP] = '{default:0};
  
  foreach(op0[i]) begin
    op0[i] = {o[0][i][WORD_BITS-1], o[0][i]};
    op1[i] = {o[1][i][WORD_BITS-1], o[1][i]};
    op2[i] = {o[2][i][WORD_BITS-1], o[2][i]};
    op3[i] = {o[3][i][WORD_BITS-1], o[3][i]};
  end
  
  case(op)
  op_nop,
  op_s2gp,
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
  op_lid:   foreach(r0[i]) r0[i] = o[0][i] + i + subVec;
  op_shf4a:  foreach(r0[i]) r0[i] = op0[op1[i][2:0]];
  op_max:   foreach(r0[i]) r0[i] = op0[i] > op1[i] ? op0[i] : op1[i];
  op_min:   foreach(r0[i]) r0[i] = op0[i] > op1[i] ? op1[i] : op0[i];
  op_umax:  foreach(r0[i]) r0[i] = o[0][i] > o[1][i] ? o[0][i] : o[1][i];
  op_umin:  foreach(r0[i]) r0[i] = o[0][i] > o[1][i] ? o[1][i] : o[0][i];
  op_clo:   
    foreach(r0[i])
      for(int j=WORD_BITS-1; j>=0; j--)
        if(o[0][i][j])
          r0[i]++;
        else
          break;
  op_clz:
    foreach(r0[i])
      for(int j=WORD_BITS-1; j>=0; j--)
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
  op_div:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = op0[i] / op1[i];
      r1[i] = op0[i] % op1[i];
    end
  op_udiv:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = o[0][i] / o[1][i];
      r1[i] = o[0][i] % o[1][i];
    end
  op_quo:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = op0[i] / op1[i];
    end
  op_uquo:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = o[0][i] / o[1][i];
    end
  op_res:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = op0[i] % op1[i];
    end
  op_ures:
    foreach(r0[i]) begin
      if(op1[i] == 0) continue;
      r0[i] = o[0][i] % o[1][i];
    end
  op_gp2s:
  begin
    foreach(expFlag[i]) expFlag[i] = op0[i];
    r0 = op0;
  end
  op_vid:   foreach(r0[i]) r0[i] = i + subVec * NUM_SP;
  default:  ovm_report_warning("SPA_ILLEGAL", "Illegal instruction opcode!!!");
  endcase
  
  foreach(res0[i])
    res0[i] = r0[i][WORD_BITS-1:0];
    
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
  
  case(prMerge)
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
  default:  ovm_report_warning("SPA_ILLEGAL", "Illegal prMerge!!!");
  endcase
  
///  `ip4_info("SPA_PROC_DATA", $psprintf("subVec:%0d, op:%s, res0:%0h, op1:%0h, o1:%0h", subVec, op.name, res0[0], op1[0], o[1][0]), OVM_FULL)
endfunction : proc_data