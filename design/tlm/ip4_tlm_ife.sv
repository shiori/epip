/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Newton Chen
/// File             : ip4_tlm_ife.sv
/// Title            : ip4 instruction fetch engine
/// Version          : 0.1
/// Last modified    : July 28 2010
/// =============================================================================
///Log:
///Created by Newton Chen on July 28 2010

class ip4_tlm_ife_vars extends ovm_object;
  tr_ise2ife fm_ise;
  tr_tlb2ife fm_tlb;
  
  tr_ife2ise ise[stage_ife:1];
  
  `ovm_object_utils_begin(ip4_tlm_ife_vars)
    `ovm_field_object(fm_ise, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_tlb, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_object_utils_end
  
  function new (string name = "ife_vars");
    super.new(name);
  endfunction : new
  
  function void gen(input ip4_tlm_ife_vars o);
    this.copy(o);
  endfunction  
endclass : ip4_tlm_ife_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_ife extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_ife_vars v, vn;  
  local uchar im[];
  local string im_file_path;
  local uint im_base, im_size;
  
  `ovm_component_utils_begin(ip4_tlm_ife)
    `ovm_field_string(im_file_path, OVM_ALL_ON)
    `ovm_field_int(im_size, OVM_ALL_ON)
    `ovm_field_int(im_base, OVM_ALL_ON)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2ife, tr_ise2ife, ip4_tlm_ife) ise_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2ife, tr_tlb2ife, ip4_tlm_ife) tlb_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_ife2ise, tr_ife2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_ife2tlb, tr_ife2tlb) tlb_tr_port;
        
  function void comb_proc();
    ovm_report_info("IFE", "comb_proc procing...", OVM_HIGH); 
    for(int i = stage_ife; i > 1; i--)
      vn.ise[i] = v.ise[i-1];
    vn.ise[1] = null;
    if(v.fm_ise != null) end_tr(v.fm_ise);
    vn.fm_ise = null;
    
    if(v.fm_ise != null && v.fm_ise.fetch_req) begin
      tr_ise2ife ise = v.fm_ise;
      uchar data[num_ifet_bytes];
      if(ise.pc inside {[im_base:im_base+im_size]}) begin
        uint adr = ise.pc - im_base;
        foreach(data[i])
          data[i] = im[adr+i];
      end
        
      if(vn.ise[1] == null) vn.ise[1] = tr_ife2ise::type_id::create("to_ise", this);
      vn.ise[1].inst_en = 1;
      vn.ise[1].tid = ise.tid;
      vn.ise[1].fg.fill(data);
    end
  endfunction
  
  function void req_proc();
    tr_ife2ise to_ise;
    ovm_report_info("IFE", "req_proc procing...", OVM_HIGH); 
    
    to_ise = v.ise[stage_ife];
    if(to_ise != null) void'(ise_tr_port.nb_transport(to_ise, to_ise));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2ife req, output tr_ise2ife rsp);
    ovm_report_info("IFE_TR", "Get ISE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_ise = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_tlb(input tr_tlb2ife req, output tr_tlb2ife rsp);
    ovm_report_info("IFE_TR", "Get TLB Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_tlb = req;
    return 1;
  endfunction : nb_transport_tlb
  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_ife_vars t;
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
    tlb_tr_imp = new("tlb_tr_imp", this);
    ise_tr_port = new("ise_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
    
    im = new[im_size];
    $readmemh(im_file_path, im);
  endfunction : build
endclass : ip4_tlm_ife

///-------------------------------------other functions-----------------------------------------
  
