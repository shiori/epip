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

parameter bit[1:0] Cbyte_offset = 2'b11;

class ip4_tlm_dse_vars extends ovm_object;
  
  tr_ise2dse fm_ise[stage_rrf_dwb:stage_rrf_rrc0];
  tr_spu2dse fm_spu;
  tr_rfm2dse fm_rfm;
  tr_spa2dse fm_spa;
  tr_tlb2dse fm_tlb;
  
  tr_dse2ise ise;
  tr_dse2spu spu[];
  tr_dse2rfm rfm[stage_ag_dwb:1];
  tr_dse2spa spa[];
  tr_dse2tlb tlb;
    
  `ovm_object_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fm_tlb, OVM_ALL_ON + OVM_REFERENCE)  
     
     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE)        
  `ovm_object_utils_end
  
  function new (string name = "dse_vars");
    super.new(name);
  endfunction : new
  
  function void gen(input ip4_tlm_dse_vars o);
    this.copy(o);
  endfunction  
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
    
  ovm_nonblocking_transport_port #(tr_dse2ise, tr_dse2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2rfm, tr_dse2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spu, tr_dse2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spa, tr_dse2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2tlb, tr_dse2tlb) tlb_tr_port;
        
  function void comb_proc();
    ovm_report_info("DSE", "comb_proc procing...", OVM_HIGH); 
    
    if(v.fm_ise[stage_rrf_ag] != null) end_tr(v.fm_ise[stage_rrf_ag]);
    if(v.fm_rfm != null) end_tr(v.fm_rfm); 
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_tlb != null) end_tr(v.fm_tlb);
    
    
    vn.fm_ise[stage_rrf_dwb] = null;
    vn.fm_rfm = null;
    vn.fm_spu = null;
    vn.fm_spa = null;
    vn.fm_tlb = null;
    
    for (int i = stage_rrf_dwb; i > 1; i--)
      vn.fm_ise[i] = v.fm_ise[i-1];
    
    for (int i = stage_ag_dwb; i > 1; i--)
      vn.
    
    /// calculating the virtual address  ag stage
    if(v.fm_rfm ! = null)begin
      if(v.fm_ise[stage_rrf_ag].en)begin
        
        vn.tlb.v_addr = v.fm_rfm.base + v.fm_rfm.op2;
      end
    end
    
    /// check the physical address in sel stage
    if((v.fm_ise[stage_rrf_sel].op == op_lw) || (v.fm_ise[stage_rrf_sel].op == op_sw))begin
      if((v.fm_tlb.phy_addr[1:0] && Cbyte_offset) == 2'b00)begin
        
      end
        
    end
    
     
  endfunction
  
  function void req_proc();
    ovm_report_info("DSE", "req_proc procing...", OVM_HIGH); 
    
    tr_dse2rfm res;
    
    ///send write back control signal to rfm
    if(v.fm_ise != null)begin
      if(v.fm_ise[stage_rrf_ag].en)begin
        res.wr_grp = v.fm_ise[stage_rrf_dwb].wr_grp;
        res.wr_adr = v.fm_ise[stage_rrf_dwb].wr_adr;
        res.wr_bk  = v.fm_ise[stage_rrf_dwb].wr_bk;
      end
    end
    
    if(v.fm_ise != null)begin
      if(v.fm_ise[stage_rrf_ag].en)begin
        if(v.fm_ise[stage_rrf_ag].op == op_)begin
          vn.rfm.
        end
      end
    end
    
    
///    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
///  function bit nb_transport_dse(input tr_dse2spa req, output tr_dse2spa rsp);
///    ovm_report_info("SPA_TR", "Get DSE Transaction...", OVM_HIGH);
///    sync();
///    assert(req != null);
///    void'(begin_tr(req));
///    rsp = req;
///    vn.fm_dse[stage_exe_dwb] = req;
///    return 1;
///  endfunction : nb_transport_dse
  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_dse_vars t;
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
///    ise_tr_imp = new("ise_tr_imp", this);
///    dse_tr_port = new("dse_tr_port", this);
    
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
