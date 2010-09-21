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
  tr_eif2dse dse[STAGE_ISE_DC:0];

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
  local tr_dse2eif reqBuf[$];
  local uint reqAdr[$];
  local uchar iseReq[$], iseReqBuf[$], dseLdCacheFillCntRsp;
  local tr_eif2dse dseLdCacheFillRsp[$], dseStRsp[$];
  local uchar pbId, waitISE;
  
///  local tr_eif2dse resDSE;
///  local uchar id;
  
  `ovm_component_utils_begin(ip4_tlm_eif)
    `ovm_field_string(dmFilePath, OVM_ALL_ON)
    `ovm_field_int(dmSize, OVM_ALL_ON)
    `ovm_field_int(dmBase, OVM_ALL_ON)
    `ovm_field_int(pbId, OVM_ALL_ON)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_dse #(tr_dse2eif, tr_dse2eif, ip4_tlm_eif) dse_tr_imp;
  ovm_nonblocking_transport_imp_ise #(tr_ise2eif, tr_ise2eif, ip4_tlm_eif) ise_tr_imp;
  ovm_nonblocking_transport_imp_spu #(tr_spu2eif, tr_spu2eif, ip4_tlm_eif) spu_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_eif2dse, tr_eif2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_eif2ise, tr_eif2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_eif2spu, tr_eif2spu) spu_tr_port;
  
  function void comb_proc();
    
    `ip4_info("EIF", "comb_proc procing...", OVM_DEBUG) 
    for(int i = LAT_EXM - 1; i > 0; i--)
      vn.fmDSE[i] = v.fmDSE[i - 1];
    for(int i = STAGE_ISE_DC; i > 0; i--)
      vn.dse[i] = v.dse[i - 1];
    vn.dse[0] = null;
            
    if(v.fmDSE[0] != null) end_tr(v.fmDSE[0]);
    vn.fmDSE[0] = null;
    
  endfunction
  
  function void req_proc();
    tr_eif2dse toDSE;
    tr_eif2ise toISE;
    tr_eif2spu toEIF;
    
    `ip4_info("EIF", "req_proc procing...", OVM_DEBUG) 
    
    begin
      tr_dse2eif dse = v.fmDSE[LAT_EXM - 1];
      if(dse != null && dse.req) begin
        uint adr;
        if(dse.exAdr >= (dmBase >> (WID_WORD + WID_SMEM_BK))
            && dse.exAdr < ((dmBase + dmSize) >> (WID_WORD + WID_SMEM_BK)))
          adr = dse.exAdr - (dmBase >> (WID_WORD + WID_SMEM_BK));
        else
          ovm_report_warning("dse", "Physical adr out of bound");
        
        if(!dse.sgl) begin
          adr = adr & `GMH(1);
        end if(!dse.last)
          ovm_report_warning("eif", "dse req sgl without last!");
        if(dse.last)
          reqAdr.push_back(adr);
        reqBuf.push_back(dse);
      end
    end
    
    if(reqAdr.size() > 0) begin
      uint adr = reqAdr.pop_front();
      uchar cnt = 0, typ = 0;
      while(reqBuf.size() > 0) begin
        tr_eif2dse toDSE;
        tr_dse2eif dse = reqBuf.pop_front();
        adr = adr + dse.cyc;
        toDSE = tr_eif2dse::type_id::create("toDSE", this);
        toDSE.endian = dse.endian;
        toDSE.exAdr = dse.exAdr;
        cnt++;
        if(dse.op inside {st_ops} || dse.cacheFlush) begin
          if(dse.cacheFlush)
            adr = adr ^ 'b01;
          foreach(dse.data[bk]) begin
            wordu res = dse.data[bk];
            for(int os = 0; os < WORD_BYTES; os++) begin
              uint adrb = adr << (WID_WORD + WID_SMEM_BK) + bk << WID_WORD + os;
              if(dse.byteEn[bk][os])
                dm[adrb] = res.b[os];
            end
          end
        end
        if(dse.cacheFill || dse.op inside {ld_ops}) begin
          toDSE.cyc = dse.cyc;
          toDSE.last = dse.last;
          foreach(dse.data[bk]) begin
            wordu res;
            for(int os = 0; os < WORD_BYTES; os++) begin
              uint adrb = adr << (WID_WORD + WID_SMEM_BK) + bk << WID_WORD + os;
              res.b[os] = dm[adrb];
            end
            toDSE.data[bk] = res;
          end
          toDSE.alloc = dse.cacheFill;
          toDSE.loadRsp = dse.op inside {ld_ops};
          toDSE.storeRsp = dse.op inside {st_ops};
          if(cnt != 0 && typ != 2)
            ovm_report_warning("eif", "inconsistent access typ");
          typ = 2;
          dseLdCacheFillRsp.push_back(toDSE);
          if(dse.last) begin
            typ = 0;
            iseReq.push_back(cnt);
          end
        end
        else if(dse.last) begin
          toDSE.storeRsp = 1;
          toDSE.last = 1;
          toDSE.alloc = 0;
          if(cnt != 0 && typ != 1)
            ovm_report_warning("eif", "inconsistent access typ");
          typ = 1; 
          dseStRsp.push_back(toDSE);
        end
      end
    end
    
    if(iseReq.size() > 0 && waitISE < 2) begin
      uchar cnt = iseReq.pop_front();
      waitISE++;
      toISE = tr_eif2ise::type_id::create("toISE", this);
      toISE.noLd = cnt;
      toISE.noSt = cnt; 
      toISE.noTMsg = cnt;
      toISE.noFMsg = cnt; 
      toISE.vecCnt = cnt; 
      toISE.sclCnt = cnt;
      iseReqBuf.push_back(cnt);
    end
    
    if(v.fmISE != null && v.fmISE.rsp) begin
      uchar cnt = iseReqBuf.pop_front();
      dseLdCacheFillCntRsp += cnt;
      if(waitISE > 0) waitISE--;
    end
    
    if(dseLdCacheFillCntRsp > 0) begin
      dseLdCacheFillCntRsp--;
      if(dseLdCacheFillRsp.size() == 0)
        ovm_report_warning("eif", "dseLdCacheFillRsp empty while cnt > 0!");
      vn.dse[0] = dseLdCacheFillRsp.pop_front();
    end
    
    if(v.dse[STAGE_ISE_RRC0] != null) begin
      toDSE = tr_eif2dse::type_id::create("toDSE", this);
      toDSE.copy(v.dse[STAGE_ISE_DC - 1]);
    end
    else if(dseStRsp.size() > 0)
      toDSE = dseStRsp.pop_front();
      
    if(v.dse[STAGE_ISE_DC - 1] != null) begin
      if(toDSE == null) toDSE = tr_eif2dse::type_id::create("toDSE", this);
      toDSE.data = v.dse[STAGE_ISE_DC - 1].data;
    end
    
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toISE != null) void'(ise_tr_port.nb_transport(toISE, toISE));
    if(toEIF != null) void'(spu_tr_port.nb_transport(toEIF, toEIF));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 

  function bit nb_transport_dse(input tr_dse2eif req, output tr_dse2eif rsp);
    `ip4_info("eif_tr", $psprintf("Get dse Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE[0] = req;
    return 1;
  endfunction : nb_transport_dse

  function bit nb_transport_ise(input tr_ise2eif req, output tr_ise2eif rsp);
    `ip4_info("eif_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_spu(input tr_spu2eif req, output tr_spu2eif rsp);
    `ip4_info("eif_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_FULL)
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    return 1;
  endfunction : nb_transport_spu
              
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
    dse_tr_imp = new("dse_tr_imp", this);
    ise_tr_imp = new("ise_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
    
  endfunction : build
  
  virtual function void start_of_simulation();
    if(dmFilePath != "") begin
      dm = new[dmSize];
      $readmemb(dmFilePath, dm);
    end
  endfunction
endclass : ip4_tlm_eif

///-------------------------------------other functions-----------------------------------------
  
