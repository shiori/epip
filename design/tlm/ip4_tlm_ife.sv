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

class ip4_tlm_ife_vars extends ovm_component;
  tr_ise2ife fmISE;
  tr_tlb2ife fmTLB;
  
  tr_ife2ise ise[STAGE_IFE:1];
  
  `ovm_component_utils_begin(ip4_tlm_ife_vars)
    `ovm_field_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(ise, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_ife_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_ife extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_ife_vars v, vn;  
  local uchar im[];
  local string imFilePath;
  local uint imBase, imSize;
  
  `ovm_component_utils_begin(ip4_tlm_ife)
    `ovm_field_string(imFilePath, OVM_ALL_ON)
    `ovm_field_int(imSize, OVM_ALL_ON)
    `ovm_field_int(imBase, OVM_ALL_ON)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_ise #(tr_ise2ife, tr_ise2ife, ip4_tlm_ife) ise_tr_imp;
  ovm_nonblocking_transport_imp_tlb #(tr_tlb2ife, tr_tlb2ife, ip4_tlm_ife) tlb_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_ife2ise, tr_ife2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_ife2tlb, tr_ife2tlb) tlb_tr_port;
        
  function void comb_proc();
    `ip4_info("ife", "comb_proc procing...", OVM_DEBUG) 
    for(int i = STAGE_IFE; i > 1; i--)
      vn.ise[i] = v.ise[i - 1];
    vn.ise[1] = null;
    if(v.fmISE != null) end_tr(v.fmISE);
    vn.fmISE = null;
    
    if(v.fmISE != null) begin
      tr_ise2ife ise = v.fmISE;
      foreach(vn.ise[i])
        if(vn.ise[i] != null && ise.cancel[vn.ise[i].tid])
          vn.ise[i] = null;
    end
    
    if(v.fmISE != null && v.fmISE.fetchReq) begin
      tr_ise2ife ise = v.fmISE;
      uchar data[NUM_IFET_BYTES];
         
      if(ise.pc inside {[imBase:imBase+imSize]}) begin
        uint adr = ise.pc - imBase;
        foreach(data[i])
          data[i] = im[adr + i];
      end
        
      if(vn.ise[1] == null) vn.ise[1] = tr_ife2ise::type_id::create("toISE", this);
      vn.ise[1].instEn = 1;
      vn.ise[1].tid = ise.tid;
      vn.ise[1].fetchGrp.ex = 1;
      vn.ise[1].fetchGrp.pc = ise.pc;
      vn.ise[1].fetchGrp.fill(data);
    end
  endfunction
  
  function void req_proc();
    tr_ife2ise toISE;
    `ip4_info("ife", "req_proc procing...", OVM_DEBUG) 
    
    toISE = v.ise[STAGE_IFE];
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2ife req, output tr_ise2ife rsp);
    `ip4_info("ife_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_tlb(input tr_tlb2ife req, output tr_tlb2ife rsp);
    `ip4_info("ife_tr", $psprintf("Get tlb Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmTLB = req;
    return 1;
  endfunction : nb_transport_tlb
  
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
    tlb_tr_imp = new("tlb_tr_imp", this);
    ise_tr_port = new("ise_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    
  endfunction : build
  
  virtual function void start_of_simulation();
    ip4_assembler asm;
    if(imFilePath != "") begin
      asm = new();
      asm.i = imFilePath;
      asm.o = "code.asm";
      asm.verb = ovm_verbosity'(get_report_verbosity_level());///OVM_FULL;
      assert(asm.translate());
      im = new[imSize];
      $readmemb("code.asm", im);
    end    
  endfunction
endclass : ip4_tlm_ife

///-------------------------------------other functions-----------------------------------------
  
