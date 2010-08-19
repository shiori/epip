/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_dse.sv
/// Title            : ip4 data stream engine
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

parameter QUEUE_SIZE = 256*2;  /// 32*8*2

class ip4_tlm_dse_vars extends ovm_component;
  
  tr_ise2dse fm_ise[stage_rrf_vwb0:stage_rrf_rrc0];
  tr_spu2dse fm_spu;
  tr_rfm2dse fm_rfm;
  tr_spa2dse fm_spa;
  tr_tlb2dse fm_tlb;
  tr_shm2dse fm_shm;   /// shared memory
  tr_ci2dse  fm_ci;    /// communication interfaces
  
  tr_dse2ise ise;
  tr_dse2spu spu;
  tr_dse2rfm rfm;
  tr_dse2spa spa;
  tr_dse2tlb tlb;
  tr_dse2shm shm[stage_exe_vwbp:0];
  tr_dse2ci  ci;
    
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fm_tlb, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fm_shm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_ci, OVM_ALL_ON + OVM_REFERENCE)  
     
     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_sarray_object(shm, OVM_ALL_ON + OVM_REFERENCE)     
     `ovm_field_object(ci, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_dse_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_dse extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_dse_vars v, vn;  
  
  `ovm_component_utils_begin(ip4_tlm_dse)
    
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2dse, tr_ise2dse, ip4_tlm_dse) ise_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2dse, tr_rfm2dse, ip4_tlm_dse) rfm_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2dse, tr_spu2dse, ip4_tlm_dse) spu_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2dse, tr_spa2dse, ip4_tlm_dse) spa_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2dse, tr_tlb2dse, ip4_tlm_dse) tlb_tr_imp;
  ovm_nonblocking_transport_imp_shm #(tr_shm2dse, tr_shm2dse, ip4_tlm_dse) shm_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_dse2ise, tr_dse2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2rfm, tr_dse2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spu, tr_dse2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spa, tr_dse2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2tlb, tr_dse2tlb) tlb_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2shm, tr_dse2shm) shm_tr_port;
        
  function void comb_proc();
    int k = 0;
    word var_vadr[num_sp];
    word valva_adr;
    bit var_emsk[num_sp];
    bit [PHY_width-1:0] phy_adr[num_sp];
    bit [2:0] bank_check;
    bit [PHY_width-1:0] smadr_start;   /// pb_id owns shared memory start address  
    bit [PHY_width-1:0] smadr_end;     /// pb_id owns shared memory end address
    
    bit [PHY_width-1:0] que_ld_adr[QUEUE_SIZE];
    uchar que_ld_tid[QUEUE_SIZE];
    ushort qld_adr_ptr = 0;
    
    bit [PHY_width-1:0] que_st_adr[QUEUE_SIZE];
    ushort qst_adr_ptr = 0;
    word que_stdat[QUEUE_SIZE];
    uchar que_st_tid[QUEUE_SIZE];
    
    ovm_report_info("DSE", "comb_proc procing...", OVM_FULL); 
    
    if(v.fm_ise[stage_rrf_rrc0] != null) end_tr(v.fm_ise[stage_rrf_rrc0]);
    if(v.fm_rfm != null) end_tr(v.fm_rfm); 
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_tlb != null) end_tr(v.fm_tlb);
    if(v.fm_shm != null) end_tr(v.fm_shm);
    if(v.fm_ci  != null) end_tr(v.fm_ci);
    
    vn.fm_ise[stage_rrf_rrc0] = null;
    vn.fm_rfm = null;
    vn.fm_spu = null;
    vn.fm_spa = null;
    vn.fm_tlb = null;
    vn.fm_shm = null;
    vn.fm_ci  = null;
    
    for (int i = stage_rrf_vwb0; i > stage_rrf_rrc0; i--) 
      vn.fm_ise[i] = v.fm_ise[i-1];
      
    for (int i = stage_exe_vwbp; i > 0; i--)
      vn.shm[i] = v.shm[i-1];
    vn.shm[stage_exe_vwbp] = null;
    
    /// calculating the virtual address  ag stage
    if(v.fm_spu != null)
      var_emsk = v.fm_spu.emsk;
    
    if(v.fm_rfm != null && v.fm_ise[stage_rrf_ag] != null && v.fm_spu != null) begin
      if(v.fm_ise[stage_rrf_ag].en) begin
        /// virtual address gen and selected send to tlb
        for (int i = 0; i < num_sp; i++)
          var_vadr[i] = v.fm_rfm.base[i] + v.fm_rfm.op2[i];
        
        for (int i = 0; (i < num_sp)&&(v.fm_spu.emsk[i]==1); i++) begin
          valva_adr = var_vadr[i];
          break;
        end
        
        for (int i = 0; (i < num_sp)&&(v.fm_spu.emsk[i]==1); i++) begin
          if((var_vadr[i][31:VADD_START] != valva_adr[31:VADD_START]))
            var_emsk[i] = 0;           /// the first step emask modification
        end
    
        if(v.fm_ise[stage_rrf_ag] != null) begin
          vn.tlb = tr_dse2tlb::type_id::create("to_tlb", this);
          vn.tlb.v_adrh[31:VADD_START-1]= valva_adr[31:VADD_START-1];  /// only the high phase sent to tlb for translation + evenoddbit
        end
    
        /// check the physical address in sel stage
        if(v.fm_ise[stage_rrf_sel] != null && v.fm_tlb != null) begin
          vn.ise = tr_dse2ise::type_id::create("to_ise", this);
          vn.shm[0] = tr_dse2shm::type_id::create("to_shm", this);
          /// use the pb_id to compute the pb self shared memory address mapped to the address space 
          smadr_start = v.fm_ise[stage_rrf_sel].pb_id * SM_SIZE;
          smadr_end = smadr_start + SM_SIZE - 1;
          if(v.fm_tlb.hit) begin
            /// gen the complete phy_adr
            for(int i = 0; (i < num_sp)&&(var_emsk[i]==1); i++) begin
              for(int j = 0; j < v.fm_tlb.eobit; j++)
                phy_adr[i][j] = valva_adr[j];
              for(int m = v.fm_tlb.eobit; m < PHY_width; m++)
                phy_adr[i][m] = v.fm_tlb.phy_adr[m];
            end
            
            for(int i = 0; (i < num_sp)&&(var_emsk[i]==1); i++) begin
              bank_check = phy_adr[i][4:2];
              k = i;
            end
                
            /// check the physical address whether match the op_code or not
            for(int i = 0; (i < num_sp)&&(var_emsk[i]==1); i++) begin
              if((((v.fm_ise[stage_rrf_sel].op == op_lw) || (v.fm_ise[stage_rrf_sel].op == op_sw)) && (phy_adr[i][1:0] != 2'b00)) 
                  ||(((v.fm_ise[stage_rrf_sel].op == op_lh) || (v.fm_ise[stage_rrf_sel].op == op_sh)) && (phy_adr[i][0] != 1'b0))) begin
                vn.ise.exp = 1;
                ovm_report_warning("DSE_ILLEGAL0", "Phy ADR does not match with the op_code type!!!");
              end
            
              /// the shared memory or other PB shared memory will be access, so must judge which memory
              /// if the address is pb_self shared memory
              if(smadr_start <= phy_adr[i] && phy_adr[i] <= smadr_end) begin
                if((v.fm_ise[stage_rrf_sel].op == op_lw) || (v.fm_ise[stage_rrf_sel].op == op_lh) || (v.fm_ise[stage_rrf_sel].op == op_lb)) begin  
                  /// the sm access will be execution, so must check if the sm bank conflict
                  vn.shm[0].ld_adr[i] = phy_adr[i]- SM_BASE; /// real memory address
                  if((phy_adr[i][4:2] == bank_check)&&(k!=i))
                    var_emsk[i] = 0;             /// the second step emask modification
                end
                if((v.fm_ise[stage_rrf_sel].op == op_sw) || (v.fm_ise[stage_rrf_sel].op == op_sh) || (v.fm_ise[stage_rrf_sel].op == op_sb)) begin
                  if((phy_adr[i][4:2] == bank_check)&&(k!=i))
                    var_emsk[i] = 0;             /// the second step emask modification
                  if(v.fm_rfm != null) begin
                    vn.shm[0].st_adr[i] = phy_adr[i]- SM_BASE; /// real memory address
                    vn.shm[0].st_dat[i] = v.fm_rfm.op1[i];
                  end
                end
              end
              else begin          /// access other pb share memory
                if((v.fm_ise[stage_rrf_sel].op == op_lw) || (v.fm_ise[stage_rrf_sel].op == op_lh) || (v.fm_ise[stage_rrf_sel].op == op_lb)) begin  
                  que_ld_adr[qld_adr_ptr] = phy_adr[i];
                  que_ld_tid[qld_adr_ptr] = v.fm_ise[stage_rrf_sel].tid;
                  qld_adr_ptr = qld_adr_ptr + 1;
                end
                
                if((v.fm_ise[stage_rrf_sel].op == op_sw) || (v.fm_ise[stage_rrf_sel].op == op_sh) || (v.fm_ise[stage_rrf_sel].op == op_sb)) begin
                  
                end
              end
            end  /// end of for  
         
          end   /// end of hit
          else vn.ise.cancel = 1;  /// no find the match entry in tlb,  miss 
        end
      end
    end
    /// send signals to rfm
    if(v.fm_ise[stage_rrf_swb] != null) begin
      vn.rfm = tr_dse2rfm::type_id::create("to_rfm", this);
      if(v.fm_ise[stage_rrf_swb].en) begin
        vn.rfm.wr_grp = v.fm_ise[stage_rrf_swb].wr_grp;
        vn.rfm.wr_adr = v.fm_ise[stage_rrf_swb].wr_adr;
        vn.rfm.wr_bk  = v.fm_ise[stage_rrf_swb].wr_bk;
        vn.rfm.ua_wr_grp = v.fm_ise[stage_rrf_swb].ua_wr_grp;
        vn.rfm.ua_wr_adr = v.fm_ise[stage_rrf_swb].ua_wr_adr;
        vn.rfm.ua_wr_bk = v.fm_ise[stage_rrf_swb].ua_wr_bk;
      end
    end
  endfunction
  
  function void req_proc();
    tr_dse2rfm res;
    
    ovm_report_info("DSE", "req_proc procing...", OVM_FULL); 
    
    ///send write back control signal to rfm
    if(v.fm_ise[stage_rrf_swb] != null) begin
      res = tr_dse2rfm::type_id::create("to_rfm", this);
      if(v.fm_ise[stage_rrf_swb].en) begin
        res.wr_grp = v.fm_ise[stage_rrf_swb].wr_grp;
        res.wr_adr = v.fm_ise[stage_rrf_swb].wr_adr;
        res.wr_bk  = v.fm_ise[stage_rrf_swb].wr_bk;
      end
    end
    
///    if(v.fm_ise != null) begin
///      if(v.fm_ise[stage_rrf_ag].en) begin
///        if(v.fm_ise[stage_rrf_ag].op == op_) begin
///        end
///      end
///    end
    
    
///    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2dse req, output tr_ise2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get ISE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_ise[stage_rrf_rrc0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2dse req, output tr_rfm2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get RFM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2dse req, output tr_spu2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get SPU Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2dse req, output tr_spa2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get SPA Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spa = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_tlb(input tr_tlb2dse req, output tr_tlb2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get TLB Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_tlb = req;
    return 1;
  endfunction : nb_transport_tlb

  function bit nb_transport_shm(input tr_shm2dse req, output tr_shm2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get SHM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_shm = req;
    return 1;
  endfunction : nb_transport_shm
          
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
    spa_tr_imp = new("spa_tr_imp", this);
    tlb_tr_imp = new("tlb_tr_imp", this);
    shm_tr_imp = new("shm_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    shm_tr_port = new("shm_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
