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
  tr_ise2spu fm_ise[stage_rrf_vwb0:0];
  tr_rfm2spu fm_rfm[stage_rrf_vwb0:stage_rrf_rrc1];
  tr_spa2spu fm_spa;
  tr_dse2spu fm_dse;
  tr_spu2rfm rfm[stage_rrf_swbp:stage_rrf_rrc1];
    
  `ovm_component_utils_begin(ip4_tlm_spu_vars)
    `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE)
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
  local bit ilm[num_thread][cyc_vec][num_sp];
  local bit cm[num_thread][cyc_vec][num_sp];
  local word msc[num_thread][cyc_vec][num_sp];
  local bit pr[num_thread][num_pr:1][cyc_vec][num_sp];
  local uchar exe_mode[num_thread];
  
  ///buffer for branch infos
  local bit b_pd[num_thread], b_nmsk[num_thread], b_inv[num_thread];
  local uchar b_rdy[num_thread], b_adr[num_thread];
  local br_opcode_e bop[num_thread];
  local msc_opcode_e sop[num_thread];
  local msk_opcode_e mop[num_thread];
  local ushort popcnt[num_thread];
  
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
    ovm_report_info("SPU", "comb_proc procing...", OVM_FULL); 
    for(int i = stage_rrf_vwb0; i > 0; i--)
      vn.fm_ise[i] = v.fm_ise[i-1];
      
    for(int i = stage_rrf_vwb0; i > stage_rrf_rrc1; i--)
      vn.fm_rfm[i] = v.fm_rfm[i-1];

    for(int i = stage_rrf_swbp; i > stage_rrf_rrc1; i--)
      vn.rfm[i] = v.rfm[i-1];
    vn.rfm[stage_rrf_swbp] = null;
    
    if(v.fm_ise[stage_rrf_rrc1] != null) end_tr(v.fm_ise[stage_rrf_rrc1]);
    if(v.fm_rfm[stage_rrf_rrc1] != null) end_tr(v.fm_rfm[stage_rrf_rrc1]);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_dse != null) end_tr(v.fm_dse);
    
    vn.fm_ise[stage_rrf_rrc1] = null;
    vn.fm_rfm[stage_rrf_rrc1] = null;
    vn.fm_spa = null;
    vn.fm_dse = null;
  endfunction
  
  function void req_proc();
    tr_spu2rfm to_rfm;
    tr_spu2ise to_ise;
    tr_spu2spa to_spa;
    tr_spu2dse to_dse;
    
    ovm_report_info("SPU", "req_proc procing...", OVM_FULL); 
    
    ///--------------prepare---------------------------------
    to_rfm = v.rfm[stage_rrf_swbp];
    
    ///----------process data---------------------
    ///write back predication register results
    if(v.fm_spa != null && v.fm_ise[stage_rrf_vwb0] != null) begin
      tr_ise2spu ise = v.fm_ise[stage_rrf_vwb0];
      tr_spa2spu spa = v.fm_spa;
      ovm_report_info("SPU", "write back SPA pres", OVM_FULL);
      pr[ise.tid][ise.pr_wr_adr0][ise.subv] = spa.pres_cmp0;
      pr[ise.tid][ise.pr_wr_adr1][ise.subv] = spa.pres_cmp1;
      if(v.fm_dse != null)
        pr[ise.tid][ise.pr_wr_adr2][ise.subv] = v.fm_dse.pres;
      if(ise.op inside {op_br, op_fcr} && b_pd[ise.tid] && b_rdy[ise.tid] > 0)
        b_rdy[ise.tid]--;
    end
    
    ///predication register read
    if(v.fm_ise[stage_rrf_rrc] != null) begin
      tr_ise2spu ise = v.fm_ise[stage_rrf_rrc];
      to_spa = tr_spu2spa::type_id::create("to_spa", this);
      to_dse = tr_spu2dse::type_id::create("to_dse", this);
      to_spa.exe_mode = exe_mode[ise.tid];
      foreach(to_spa.fu[fid]) begin
        to_spa.fu[fid].emsk = ise.pr_rd_adr == 0 ? '{default:1} : pr[ise.tid][ise.pr_rd_adr][ise.subv];
        if(ise.pr_inv[fid])
          foreach(to_spa.fu[fid].emsk[i])
            to_spa.fu[fid].emsk[i] = !to_spa.fu[fid].emsk[i];
        if(!ise.pr_nmsk[fid])
          foreach(to_spa.fu[fid].emsk[i])
            to_spa.fu[fid].emsk[i] = to_spa.fu[fid].emsk[i] && ilm[ise.tid][ise.subv][i] && cm[ise.tid][ise.subv][i];
      end
      to_dse.emsk = ise.pr_rd_adr_dse == 0 ? '{default:1} : pr[ise.tid][ise.pr_rd_adr_dse][ise.subv];
      
      foreach(to_dse.emsk[i]) begin
        if(ise.pr_inv_dse)
          to_dse.emsk[i] = !to_dse.emsk[i];
        if(!ise.pr_nmsk_dse)
          to_dse.emsk[i] = to_dse.emsk[i] && ilm[ise.tid][ise.subv][i] && cm[ise.tid][ise.subv][i];
      end
    end
    
    ///processing normal spu instructions
    if(v.fm_ise[stage_rrf_rrc1] != null && v.fm_rfm[stage_rrf_rrc1] != null) begin
      tr_ise2spu ise = v.fm_ise[stage_rrf_rrc1];
      tr_rfm2spu rfm = v.fm_rfm[stage_rrf_rrc1];
      bit[word_width:0] op0, op1, r0;
      bit pr_spu = 0, pr_tmp[cyc_vec][num_sp];
      
      if(ise.spu_start) begin
        ovm_report_info("SPU", "process SPU inst", OVM_FULL);
        foreach(pr_tmp[i,j]) begin
          pr_tmp[i][j] = ise.pr_rd_adr_spu == 0 ? 1 : pr[ise.tid][ise.pr_rd_adr_spu][i][j];
          if(ise.pr_inv_spu)
            pr_tmp[i][j] = !pr_tmp[i][j];
          if(!ise.pr_nmsk_spu)
           pr_tmp[i][j] = pr_tmp[i][j] && ilm[ise.tid][i][j] && cm[ise.tid][i][j];
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
        vn.rfm[stage_rrf_rrc1] = tr_spu2rfm::type_id::create("to_rfm", this);
        vn.rfm[stage_rrf_rrc1].res = r0[word_width-1:0];
        vn.rfm[stage_rrf_rrc1].wen = pr_spu;
        vn.rfm[stage_rrf_rrc1].srf_wr_dsel = ise.srf_wr_dsel;
        vn.rfm[stage_rrf_rrc1].srf_wr_bk   = ise.srf_wr_bk;
        vn.rfm[stage_rrf_rrc1].srf_wr_grp  = ise.srf_wr_grp;
        vn.rfm[stage_rrf_rrc1].srf_wr_adr  = ise.srf_wr_adr;
      end
    end
    
    ///bypass to spa
    if(to_rfm != null) begin
      if(to_spa == null) to_spa = tr_spu2spa::type_id::create("to_spa", this);
      to_spa.res = to_rfm.res;
    end
    
    ///log branch info to buf
    if(v.fm_ise[stage_rrf_rrc1] != null) begin
      tr_ise2spu ise = v.fm_ise[stage_rrf_rrc1];
      if(ise.op inside {op_br, op_fcr}) begin
        mop[ise.tid] = ise.mop;
        sop[ise.tid] = ise.sop;
        bop[ise.tid] = ise.bop;
        popcnt[ise.tid] = v.fm_rfm[stage_rrf_rrc1] == null ? 0 : v.fm_rfm[stage_rrf_rrc1].op0;
        b_pd[ise.tid] = 1;
        b_adr[ise.tid] = ise.pr_rd_adr_spu;
        b_nmsk[ise.tid] = ise.pr_nmsk_spu;
        b_inv[ise.tid] = ise.pr_inv_spu;
        if(ise.pr_br_dep) begin
          b_rdy[ise.tid] = ise.vec_mode;
        end
        else begin
          b_rdy[ise.tid] = 0;
        end
      end
    end
    
    ///check for valid branch
    foreach(b_pd[tid])
      if(b_pd[tid] && b_rdy[tid] == 0) begin
        uchar adr = b_adr[tid];
        bit is_nop = mop[adr] == mop_nop, emsk_az = 1, update_msc = 0;
        bit emsk[cyc_vec][num_sp] = adr == 0 ? '{default:1} : pr[tid][adr];
        
        ovm_report_info("SPU", $psprintf("process branch for thread %0d", tid), OVM_HIGH);
        b_pd[tid] = 0;

        if(b_inv[tid])
          foreach(emsk[j,k])
            emsk[j][k] = !emsk[j][k];
        
        if(mop[adr] == mop_else) begin
          foreach(emsk[j,k])
            emsk[j][k] = emsk[j][k] && (!(msc[tid][j][k] > 1));
        end
        else if(!b_nmsk[tid]) begin
          if(mop[adr] == mop_loop)
            foreach(emsk[j,k])
              emsk[j][k] = emsk[j][k] && ilm[tid][j][k];
          else
            foreach(emsk[j,k])
              emsk[j][k] = emsk[j][k] && ilm[tid][j][k] && cm[tid][j][k];
        end
        
        if(to_rfm == null) to_rfm = tr_spu2rfm::type_id::create("to_rfm", this);
        to_ise = tr_spu2ise::type_id::create("to_ise", this);
        to_ise.tid = tid;

        foreach(emsk[j,k]) 
          if(emsk[j][k] == 1) begin
             emsk_az = 0;
             break;
          end

        if(is_nop)
          to_ise.br_taken = 0;
        else
          case(bop[adr])
          bop_naz  :  to_ise.br_taken = !emsk_az;
          bop_az   :  to_ise.br_taken = emsk_az;
          endcase
       
        case(mop[adr])
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
          
        case(mop[tid])
        sop_pop2n :
          if(update_msc)
            foreach(emsk[j,k])
              if(msc[tid][j][k] > (2*popcnt[tid]))
                msc[tid][j][k] -= (2*popcnt[tid]);
              else
                msc[tid][j][k] = 0;
        sop_store :
          foreach(emsk[j,k]) begin
            msc[tid][j][k] += !ilm[tid][j][k];
            msc[tid][j][k] += !cm[tid][j][k];
          end
          endcase
        break;
      end
    
    ///------------req to other module----------------
    if(to_rfm != null) void'(rfm_tr_port.nb_transport(to_rfm, to_rfm));
    if(to_ise != null) void'(ise_tr_port.nb_transport(to_ise, to_ise));
    if(to_spa != null) void'(spa_tr_port.nb_transport(to_spa, to_spa));
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ise(input tr_ise2spu req, output tr_ise2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get ISE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_ise[0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2spu req, output tr_rfm2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get RFM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm[stage_rrf_rrc1] = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spa(input tr_spa2spu req, output tr_spa2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get SPA Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spa = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_dse(input tr_dse2spu req, output tr_dse2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get DSE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_tlb(input tr_tlb2spu req, output tr_tlb2spu rsp);
    ovm_report_info("SPU_TR", $psprintf("Get TLB Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fm_dse = req;
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
    b_rdy = '{default: cyc_vec};
    b_pd = '{default: 0};
  endfunction : build
endclass : ip4_tlm_spu

///-------------------------------------other functions-----------------------------------------
