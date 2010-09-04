/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_eif.sv
/// Title            : ip4 external interface
/// Version          : 0.1
/// Last modified    : July 19 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 19 2010

class ip4_tlm_eif_vars extends ovm_component;
  tr_dse2eif fmDSE[LAT_EXM - 1 : 0];
  tr_ise2eif fmISE;
  tr_eif2dse dse[CYC_EIF_ISE_DSE:0];

  `ovm_component_utils_begin(ip4_tlm_eif_vars)
    `ovm_field_sarray_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
endclass : ip4_tlm_eif_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_eif extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_eif_vars v, vn;  
  local uchar dm[];
  local string dmFilePath;
  local uint dmBase, dmSize;
  local tr_dse2eif dseReq[$];
  local tr_eif2dse dseRsp[$];
  local uchar waitISE;
  
///  local tr_eif2dse resDSE;
///  local uchar id;
  
  `ovm_component_utils_begin(ip4_tlm_eif)
    `ovm_field_string(dmFilePath, OVM_ALL_ON)
    `ovm_field_int(dmSize, OVM_ALL_ON)
    `ovm_field_int(dmBase, OVM_ALL_ON)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_dse #(tr_dse2eif, tr_dse2eif, ip4_tlm_eif) dse_tr_imp;
  ovm_nonblocking_transport_imp_ise #(tr_ise2eif, tr_ise2eif, ip4_tlm_eif) ise_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_eif2dse, tr_eif2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_eif2ise, tr_eif2ise) ise_tr_port;
  
  function void comb_proc();
    
    ovm_report_info("EIF", "comb_proc procing...", OVM_FULL); 
    for(int i = LAT_EXM - 1; i > 0; i++)
      vn.fmDSE[i] = v.fmDSE[i - 1];
    for(int i = CYC_EIF_ISE_DSE - 1; i > 0; i++)
      vn.dse[i] = v.dse[i - 1];
    vn.dse[0] = null;
            
    if(v.fmDSE[0] != null) end_tr(v.fmDSE[0]);
    vn.fmDSE[0] = null;
    
  endfunction
  
  function void req_proc();
    tr_eif2dse toDSE;
    tr_eif2ise toISE;
    
    ovm_report_info("EIF", "req_proc procing...", OVM_FULL); 
    
    begin
      tr_dse2eif dse = v.fmDSE[LAT_EXM - 1];
      if(dse != null && dse.req && (dse.cacheFill || dse.op inside {ld_ops})) begin
        uint adr;
        tr_eif2dse toDSE;
        if(dse.pAdr >= dmBase && dse.pAdr < (dmBase + dmSize))
          adr = dse.pAdr - dmBase;
        else
          ovm_report_warning("dse", "Physical adr out of bound");
        toDSE = tr_eif2dse::type_id::create("toDSE", this);
        case(dse.op)
        op_lw: begin end
        endcase
        toDSE.id = dse.id;
        dseReq.push_back(dse);
        dseRsp.push_back(toDSE);
      end
    end
    
    if(dseReq.size() > 0 && waitISE < 2) begin
      tr_dse2eif dse = dseReq.pop_front();
      waitISE++;
      toISE = tr_eif2ise::type_id::create("toISE", this);
      toISE.noLd = 0;
      toISE.noSt = 0; 
      toISE.noSMsg = 0;
      toISE.noRMsg = 0; 
      toISE.vecCnt = 0; 
      toISE.sclCnt = 0;
    end
    
    if(v.fmISE != null && v.fmISE.rsp) begin
      vn.dse[0] = dseRsp.pop_front();
      if(waitISE > 0) waitISE--;
    end
    
    toDSE = v.dse[CYC_EIF_ISE_DSE - 1];
    
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 

  function bit nb_transport_dse(input tr_dse2eif req, output tr_dse2eif rsp);
    ovm_report_info("eif_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE[0] = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_ise(input tr_ise2eif req, output tr_ise2eif rsp);
    ovm_report_info("eif_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE = req;
    return 1;
  endfunction : nb_transport_ise
            
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    if($time == stamp) begin
       ovm_report_info("sync", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("sync", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
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
    dse_tr_imp = new("dse_tr_imp", this);
    ise_tr_imp = new("ise_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    
    dm = new[dmSize];
    $readmemb(dmFilePath, dm);
  endfunction : build
endclass : ip4_tlm_eif

///-------------------------------------other functions-----------------------------------------
  
