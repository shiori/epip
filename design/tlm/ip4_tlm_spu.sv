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
  tr_ise2spu fmISE[stage_rrf_vwb0:0];
  tr_rfm2spu fmRFM[stage_rrf_vwb0:stage_rrf_exs0];
  tr_spa2spu fmSPA;
  tr_dse2spu fmDSE;
  tr_spu2rfm rfm[stage_rrf_swbp:stage_rrf_exs1];
    
  `ovm_component_utils_begin(ip4_tlm_spu_vars)
    `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)
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
  local bit pr[NUM_THREAD][num_pr:1][CYC_VEC][NUM_SP];
  local uchar sr_exe_mode[NUM_THREAD];
  
  ///buffer for branch infos
///  local bit b_pd[NUM_THREAD], b_nmsk[NUM_THREAD], b_inv[NUM_THREAD];
///  local uchar b_rdy[NUM_THREAD], b_adr[NUM_THREAD];
///  local br_opcode_e bop[NUM_THREAD];
///  local msc_opcode_e sop[NUM_THREAD];
///  local msk_opcode_e mop[NUM_THREAD];
///  local ushort popcnt[NUM_THREAD];
  
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
  
  function void combProc();
    ovm_report_info("spu", "combProc procing...", OVM_FULL); 
    for(int i = stage_rrf_vwb0; i > 0; i--)
      vn.fmISE[i] = v.fmISE[i-1];
      
    for(int i = stage_rrf_vwb0; i > stage_rrf_exs0; i--)
      vn.fmRFM[i] = v.fmRFM[i-1];

    for(int i = stage_rrf_swbp; i > stage_rrf_exs1; i--)
      vn.rfm[i] = v.rfm[i-1];
    vn.rfm[stage_rrf_exs1] = null;
    
    if(v.fmISE[0] != null) end_tr(v.fmISE[0]);
    if(v.fmRFM[stage_rrf_exs0] != null) end_tr(v.fmRFM[stage_rrf_exs0]);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmDSE != null) end_tr(v.fmDSE);
    
    vn.fmISE[0] = null;
    vn.fmRFM[stage_rrf_exs0] = null;
    vn.fmSPA = null;
    vn.fmDSE = null;
  endfunction
  
  function void reqProc();
    tr_spu2rfm toRFM;
    tr_spu2ise to_ise;
    tr_spu2spa toSPA;
    tr_spu2dse toDSE;
    
    ovm_report_info("spu", "reqProc procing...", OVM_FULL); 
    
    ///--------------prepare---------------------------------
    toRFM = v.rfm[stage_rrf_swbp];
    
    ///----------process data---------------------
    ///write back cmp predication register results
    if(v.fmSPA != null && v.fmISE[stage_rrf_cem0] != null) begin
      tr_ise2spu ise = v.fmISE[stage_rrf_cem0];
      tr_spa2spu spa = v.fmSPA;
      ovm_report_info("spu", "write back spa pres", OVM_FULL);
      pr[ise.TId][ise.pr_wr_adr0][ise.subVec] = spa.pres_cmp0;
      pr[ise.TId][ise.pr_wr_adr1][ise.subVec] = spa.pres_cmp1;
      if(v.fmDSE != null)
        pr[ise.TId][ise.pr_wr_adr2][ise.subVec] = v.fmDSE.pres;
///      if(ise.op inside {op_br, op_fcr} && b_pd[ise.TId] && b_rdy[ise.TId] > 0)
///        b_rdy[ise.TId]--;
    end
    
    ///write back dse predication register results
    if(v.fmDSE != null && v.fmISE[stage_rrf_dem0] != null) begin
      tr_ise2spu ise = v.fmISE[stage_rrf_cem0];
      tr_dse2spu dse = v.fmDSE;
      ovm_report_info("spu", "write back dse pres", OVM_FULL);
      if(!dse.exp)
        pr[ise.TId][ise.pr_wr_adr2][ise.subVec] = dse.pres;
    end
        
    ///predication register read
    if(v.fmISE[stage_rrf_rrc] != null) begin
      tr_ise2spu ise = v.fmISE[stage_rrf_rrc];
      foreach(toSPA.fu[fid]) begin
        if(!ise.enFu[fid]) continue;
        if(toSPA == null) toSPA = tr_spu2spa::type_id::create("toSPA", this);
        toSPA.fu[fid].emsk = ise.pr_rd_adr[fid] == 0 ? '{default:1} : pr[ise.TId][ise.pr_rd_adr[fid]][ise.subVec];
        if(ise.pr_inv[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = !toSPA.fu[fid].emsk[i];
        if(!ise.pr_nmsk[fid])
          foreach(toSPA.fu[fid].emsk[i])
            toSPA.fu[fid].emsk[i] = toSPA.fu[fid].emsk[i] && ilm[ise.TId][ise.subVec][i] && cm[ise.TId][ise.subVec][i];
      end
      if(toSPA != null) toSPA.exe_mode = sr_exe_mode[ise.TId];
    end
    
    if(v.fmISE[STAGE_RRF_RRC0] != null && v.fmISE[STAGE_RRF_RRC0].enDSE) begin
      tr_ise2spu ise = v.fmISE[STAGE_RRF_RRC0];
      toDSE = tr_spu2dse::type_id::create("toDSE", this);
      foreach(toDSE.emsk[i]) begin
        if(ise.pr_inv_dse)
          toDSE.emsk[i] = !toDSE.emsk[i];
        if(!ise.pr_nmsk_dse)
          toDSE.emsk[i] = toDSE.emsk[i] && ilm[ise.TId][ise.subVec][i] && cm[ise.TId][ise.subVec][i];
      end
      toDSE.emsk = ise.pr_rd_adr_dse == 0 ? '{default:1} : pr[ise.TId][ise.pr_rd_adr_dse][ise.subVec];
    end
    
    ///processing normal spu instructions
    if(v.fmISE[stage_rrf_exs0] != null && v.fmRFM[stage_rrf_exs0] != null) begin
      tr_ise2spu ise = v.fmISE[stage_rrf_exs0];
      tr_rfm2spu rfm = v.fmRFM[stage_rrf_exs0];
      bit[word_width:0] op0, op1, r0;
      bit pr_spu = 0, pr_tmp[CYC_VEC][NUM_SP];
      
      if(ise.start) begin
        ovm_report_info("spu", "process spu inst", OVM_FULL);
        foreach(pr_tmp[i,j]) begin
          pr_tmp[i][j] = ise.pr_rd_adr_spu == 0 ? 1 : pr[ise.TId][ise.pr_rd_adr_spu][i][j];
          if(ise.pr_inv_spu)
            pr_tmp[i][j] = !pr_tmp[i][j];
          if(!ise.pr_nmsk_spu)
           pr_tmp[i][j] = pr_tmp[i][j] && ilm[ise.TId][i][j] && cm[ise.TId][i][j];
          pr_spu |= pr_tmp[i][j];
        end

        op0 = {rfm.op0[word_width-1], rfm.op0};
        op1 = {rfm.op1[word_width-1], rfm.op1};
        
        case(ise.op)
        op_nop,   
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
        op_ror:    r0 = {rfm.op0, rfm.op0} >> rfm.op1;
///        op_umul:   r0 = unsigned'(rfm.op0) * unsigned'(rfm.op1);
///        op_smul:   r0 = signed'(op0) * signed'(op1);
        op_clo:   ovm_report_warning("SPU_UNIMP", "clo is not implemented yet");
        op_clz:   ovm_report_warning("SPU_UNIMP", "clz is not implemented yet");
        op_ext:   ovm_report_warning("SPU_UNIMP", "ext is not implemented yet");
        op_ins:   ovm_report_warning("SPU_UNIMP", "ins is not implemented yet");
        op_seb:   ovm_report_warning("SPU_UNIMP", "seb is not implemented yet");
        op_she:   ovm_report_warning("SPU_UNIMP", "she is not implemented yet");
        op_wsbh:  ovm_report_warning("SPU_UNIMP", "wsbh is not implemented yet");
        endcase
        vn.rfm[stage_rrf_exs1] = tr_spu2rfm::type_id::create("toRFM", this);
        vn.rfm[stage_rrf_exs1].res = r0[word_width-1:0];
        vn.rfm[stage_rrf_exs1].wrEn = pr_spu;
        vn.rfm[stage_rrf_exs1].srf_wr_dsel = ise.srf_wr_dsel;
        vn.rfm[stage_rrf_exs1].srfWrBk   = ise.srfWrBk;
        vn.rfm[stage_rrf_exs1].srfWrGrp  = ise.srfWrGrp;
        vn.rfm[stage_rrf_exs1].srfWrAdr  = ise.srfWrAdr;
      end
    end
    
///    ///bypass to spa
///    if(toRFM != null) begin
///      if(toSPA == null) toSPA = tr_spu2spa::type_id::create("toSPA", this);
///      toSPA.res = toRFM.res;
///    end
    
///    ///log branch info to buf
///    if(v.fmISE[stage_rrf_exs0] != null) begin
///      tr_ise2spu ise = v.fmISE[stage_rrf_exs0];
///      if(ise.op inside {op_br, op_fcr}) begin
///        mop[ise.TId] = ise.mop;
///        sop[ise.TId] = ise.sop;
///        bop[ise.TId] = ise.bop;
///        popcnt[ise.TId] = v.fmRFM[stage_rrf_exs0] == null ? 0 : v.fmRFM[stage_rrf_exs0].op0;
///        b_pd[ise.TId] = 1;
///        b_adr[ise.TId] = ise.pr_rd_adr_spu;
///        b_nmsk[ise.TId] = ise.pr_nmsk_spu;
///        b_inv[ise.TId] = ise.pr_inv_spu;
///        if(ise.pr_br_dep) begin
///          b_rdy[ise.TId] = ise.vecMode + 1;
///        end
///        else begin
///          b_rdy[ise.TId] = 0;
///        end
///      end
///    end
    
    ///check for valid branch
///    foreach(b_pd[TId])
    begin
      bit found = 1;
      tr_ise2spu ise;
      tr_rfm2spu rfm;
      
      if(v.fmISE[stage_rrf_cem0] != null && v.fmSPA != null
          && v.fmISE[stage_rrf_cem0].br_dep_spa && v.fmISE[stage_rrf_cem0].br_end) begin
        ise = v.fmISE[stage_rrf_cem0];
        rfm = v.fmRFM[stage_rrf_cem0];
      end
      else if(v.fmISE[stage_rrf_dem0] != null && v.fmDSE != null
               && v.fmISE[stage_rrf_dem0].br_dep_dse && v.fmISE[stage_rrf_dem0].br_end
               && !v.fmDSE.exp) begin
        ise = v.fmISE[stage_rrf_dem0];
        rfm = v.fmRFM[stage_rrf_dem0];
      end
      else if(v.fmISE[stage_rrf_exs0] != null && v.fmISE[stage_rrf_exs0].op inside {op_br, op_fcr}
               && v.fmISE[stage_rrf_exs0].br_end && !v.fmISE[stage_rrf_exs0].br_dep) begin
        ise = v.fmISE[stage_rrf_exs0];
        rfm = v.fmRFM[stage_rrf_exs0];        
      end
      else
        found = 0;
      
      if(found) begin ///b_pd[TId] && b_rdy[TId] == 0
        uchar TId = ise.TId;
        ushort popcnt = rfm == null ? 0 : rfm.op0;
        bit b_inv = ise.pr_inv_spu, b_nmsk = ise.pr_nmsk_spu;
        bit is_nop = ise.mop == mop_nop, emsk_az = 1, update_msc = 0;
        bit emsk[CYC_VEC][NUM_SP] = ise.pr_rd_adr_spu == 0 ? '{default:1} : pr[TId][ise.pr_rd_adr_spu];
        
        ovm_report_info("spu", $psprintf("process branch for thread %0d", TId), OVM_HIGH);
///        b_pd[TId] = 0;
        
        if(b_inv)
          foreach(emsk[j,k])
            emsk[j][k] = !emsk[j][k];
        
        if(ise.mop == mop_else) begin
          foreach(emsk[j,k])
            emsk[j][k] = emsk[j][k] && (!(msc[TId][j][k] > 1));
        end
        else if(!b_nmsk) begin
          if(ise.mop == mop_loop)
            foreach(emsk[j,k])
              emsk[j][k] = emsk[j][k] && ilm[TId][j][k];
          else
            foreach(emsk[j,k])
              emsk[j][k] = emsk[j][k] && ilm[TId][j][k] && cm[TId][j][k];
        end
        
///        if(toRFM == null) toRFM = tr_spu2rfm::type_id::create("toRFM", this);
        if(to_ise == null) to_ise = tr_spu2ise::type_id::create("to_ise", this);
        to_ise.TId = TId;

        foreach(emsk[j,k]) 
          if(emsk[j][k] == 1) begin
             emsk_az = 0;
             break;
          end

        if(is_nop)
          to_ise.br_taken = 0;
        else
          case(ise.bop)
          bop_naz  :  to_ise.br_taken = !emsk_az;
          bop_az   :  to_ise.br_taken = emsk_az;
          endcase
       
        case(ise.mop)
        mop_nop   : update_msc = 1;
        mop_rstor :
          begin
            foreach(emsk[j,k]) begin
              if(msc[TId][j][k] > 0) cm[TId][j][k] = 0;
              else cm[TId][j][k] = 0;
              if(msc[TId][j][k] > 1) ilm[TId][j][k] = 0;
              else ilm[TId][j][k] = 1;
            end
          end
        mop_if,
        mop_else  :
          if(!emsk_az) begin
            ilm[TId] = emsk;
            cm[TId] = emsk;
          end
        mop_loop  :
          if(!emsk_az) begin
            ilm[TId] = emsk;
            cm[TId] = emsk;
          end
        mop_cont  :
          if(!emsk_az)
            cm[TId] = emsk;
          else
            update_msc = 1;
        mop_brk:
          if(!emsk_az) begin
            ilm[TId] = emsk;
            cm[TId] = emsk;
          end
          else
            update_msc = 1;
        endcase
          
        case(ise.sop)
        sop_pop2n :
          if(update_msc)
            foreach(emsk[j,k])
              if(msc[TId][j][k] > (2*popcnt))
                msc[TId][j][k] -= (2*popcnt);
              else
                msc[TId][j][k] = 0;
        sop_store :
          foreach(emsk[j,k]) begin
            msc[TId][j][k] += !ilm[TId][j][k];
            msc[TId][j][k] += !cm[TId][j][k];
          end
          endcase
///        break;
      end
    end
    
    ///------------req to other module----------------
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
    if(to_ise != null) void'(ise_tr_port.nb_transport(to_ise, to_ise));
    if(toSPA != null) void'(spa_tr_port.nb_transport(toSPA, toSPA));
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2spu req, output tr_ise2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spu req, output tr_rfm2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM[stage_rrf_exs0] = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spa(input tr_spa2spu req, output tr_spa2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_dse(input tr_dse2spu req, output tr_dse2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_tlb(input tr_tlb2spu req, output tr_tlb2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get TLB Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_tlb
    
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
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
///    b_rdy = '{default: CYC_VEC};
///    b_pd = '{default: 0};
    ilm = '{default : 1};
    cm = '{default : 1};
  endfunction : build
endclass : ip4_tlm_spu

///-------------------------------------other functions-----------------------------------------
