/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : yajing yuan
/// File             : ip4_tlm_tlb.sv
/// Title            : ip4 data stream engine
/// Version          : 0.1
/// Last modified    : July 20 2010
/// =============================================================================
///Log:
///Created by yajing yuan on July 20 2010

typedef enum uchar {
  page_8K   = 0,
  page_64K  = 1,
  page_512K = 2,
  page_4M   = 3,
  page_16M  = 4,
  page_64M  = 5,
  page_256M = 6
} page_typ_e;

parameter ushort page_mask_table[] = {
  15'b000000000000000,   /// 8K
  15'b000000000000111,   /// 64K
  15'b000000000111111,   /// 512k
  15'b000000111111111,   /// 4M
  15'b000011111111111,   /// 16M
  15'b001111111111111,   /// 64M
  15'b111111111111111    /// 256M
};

parameter uchar even_odd_bit_table[] = {
  0,
  3,
  6,
  9,
  11,
  13,
  15
};

class ip4_tlm_tlb_vars extends ovm_component;
  tr_dse2tlb fmDSE;
  tr_spu2tlb fmSPU;
  tr_ife2tlb fmIFE[IFE_REQ_BUF];
  uchar ifeBufPtr;

  tr_tlb2spu spu[STAG_TLB_SPU:1];
  
  uint vpn2[NUM_TLB_E];
  uchar pageTyp[NUM_TLB_E];
  uchar asid[NUM_TLB_E];
  
  uint  pfn2e[NUM_TLB_E], pfn2o[NUM_TLB_E];
  bit[NUM_TLB_E-1:0] ex[2], k[2], e[2], d[2], v[2], g[2];
  bit[NUM_TLB_E-1:0][2:0] c[2];
  
  word srIndex;
  word srRandom;
  word srEntryLo0;
  word srEntryLo1;
  word srEntryHi;
  word srContent[NUM_SP];
  uchar srASID[NUM_SP];

  `ovm_component_utils_begin(ip4_tlm_tlb_vars)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(vpn2, OVM_ALL_ON)
    `ovm_field_sarray_int(pageTyp, OVM_ALL_ON)
    `ovm_field_sarray_int(asid, OVM_ALL_ON)
    `ovm_field_sarray_int(g, OVM_ALL_ON)
    `ovm_field_sarray_int(pfn2e, OVM_ALL_ON)
    `ovm_field_sarray_int(ex, OVM_ALL_ON)
    `ovm_field_sarray_int(c, OVM_ALL_ON)
    `ovm_field_sarray_int(k, OVM_ALL_ON)
    `ovm_field_sarray_int(e, OVM_ALL_ON)
    `ovm_field_sarray_int(d, OVM_ALL_ON)
    `ovm_field_sarray_int(v, OVM_ALL_ON)
    `ovm_field_sarray_int(pfn2o, OVM_ALL_ON)
    `ovm_field_int(srIndex, OVM_ALL_ON)
    `ovm_field_int(srRandom, OVM_ALL_ON)
    `ovm_field_int(srEntryLo0, OVM_ALL_ON)
    `ovm_field_int(srEntryLo1, OVM_ALL_ON)
    `ovm_field_int(srEntryHi, OVM_ALL_ON)
    `ovm_field_sarray_int(srContent, OVM_ALL_ON)
    `ovm_field_sarray_int(srASID, OVM_ALL_ON)
    `ovm_field_int(ifeBufPtr, OVM_ALL_ON)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
    vpn2 = '{default : 0};
    pageTyp = '{default : 0};
    asid = '{default : 0};
    g    = '{default : 0};
    e = '{default : 0};
    c = '{default : 0};
    k = '{default : 0};
    e = '{default : 0};
    d = '{default : 0};
    v = '{default : 0};
    srIndex = 0;
    srRandom = 0;
    srEntryLo0 = 0;
    srEntryLo1 = 0;
    srEntryHi = 0;
    srContent = '{default : 0};
    ifeBufPtr = 0;
  endfunction : new
endclass : ip4_tlm_tlb_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_tlb extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_tlb_vars v, vn;
  local tr_tlb2ife toIFE;
  local tr_tlb2dse toDSE;
  
  `ovm_component_utils_begin(ip4_tlm_tlb)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_spu #(tr_spu2tlb, tr_spu2tlb, ip4_tlm_tlb) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2tlb, tr_dse2tlb, ip4_tlm_tlb) dse_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2tlb, tr_ife2tlb, ip4_tlm_tlb) ife_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_tlb2spu, tr_tlb2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2dse, tr_tlb2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2ife, tr_tlb2ife) ife_tr_port;
    
  function void comb_proc();
    uchar evenOddBit; 
    ushort varMask; ///[NUM_TLB_E];
    uint varPFN;
    bit varEx = 0, varK = 0, varE = 0, varD = 0, varV = 0, varG = 0;
    bit [2:0] varC = 0;
    bit find = 0;
    uchar varTid;
    word virAdr;
    bit rspDSE = 0, rspIFE = 0, exp = 0;
    
    ovm_report_info("tlb", "comb_proc procing...", OVM_FULL);
     
    if(v.fmDSE != null) end_tr(v.fmDSE);
    if(v.fmSPU != null) end_tr(v.fmSPU);
    vn.fmDSE = null;
    vn.fmSPU = null;
    
    ///serve dse or ife?
    if(v.fmDSE == null || (v.fmDSE != null && !v.fmDSE.req)) begin
      rspIFE = v.fmIFE[0] != null && v.fmIFE[0].req;
      if(v.fmIFE[0] != null) end_tr(v.fmIFE[0]);
      for(int i = 1; i < v.ifeBufPtr; i++)
        vn.fmIFE[i - 1] = v.fmIFE[i];
      if(v.ifeBufPtr > 0) begin
        vn.fmIFE[v.ifeBufPtr - 1] = null;
        vn.ifeBufPtr = v.ifeBufPtr - 1;
      end
      else
       vn.fmIFE[0] = null;
    end
    else if(v.fmDSE != null)
      rspDSE = 1;
     
    for (int i = STAG_TLB_SPU; i > 1; i--)
      vn.spu[i] = v.spu[i-1];
    vn.spu[1] = null;
     
    ///tlb translation
    if(rspDSE || rspIFE) begin
      if(rspIFE && v.fmIFE[0] != null) begin
        virAdr = v.fmIFE[0].vAdr;
        varTid = v.fmIFE[0].tid;
      end
      else if(v.fmDSE != null) begin
        virAdr = v.fmDSE.vAdr;
        varTid = v.fmDSE.tid;
      end
      
      ///search for match
      for (int i = 0; i < NUM_TLB_E; i++) begin
        evenOddBit = even_odd_bit_table[v.pageTyp[i]];
        varMask = page_mask_table[v.pageTyp[i]];
        if(((v.vpn2[i] & ~varMask) == (virAdr & ~varMask)) 
              && (v.g[i] || (v.asid[i] == v.srASID[varTid]))) begin
            ///match found, select even/odd page
            bit eosel = virAdr[evenOddBit];
            varPFN = eosel ? v.pfn2o[i] : v.pfn2e[i];
            varV   = v.v[eosel];
            varC   = v.c[eosel];
            varEx  = v.ex[eosel];
            varD   = v.d[eosel];

            if(varV == 0) begin
              ovm_report_info("TLB_Invalid", "tlb Invalid exception!!!", OVM_HIGH); 
              vn.srContent[varTid][4:0] = 0;
              vn.srContent[varTid][22:5] = v.vpn2[i];
              exp = 1;
              break;
            end
            
            if(rspDSE && varD == 0 && ((v.fmDSE.op == op_sw) || (v.fmDSE.op == op_sh) || (v.fmDSE.op == op_sb))) begin
              ovm_report_info("TLB_Modified", "tlb Modified exception!!!", OVM_HIGH); 
              vn.srContent[varTid][4:0] = 0;
              vn.srContent[varTid][22:5] = v.vpn2[i];
              exp = 1;
              break;
            end

            if(!varEx && rspIFE) begin
              ovm_report_info("TLB_EX", "tlb NON_EXECUTION exception!!!", OVM_HIGH); 
              vn.srContent[varTid][4:0] = 0;
              vn.srContent[varTid][22:5] = v.vpn2[i];
              exp = 1;
              break;
            end
            find = 1;
            break;
        end  
      end
      
      if(rspIFE && v.fmIFE[0] != null && v.fmIFE[0].req) begin
        if(toIFE == null) toIFE = tr_tlb2ife::type_id::create("toIFE", this);
        toIFE.pfn = varPFN;///varPAdr;
        toIFE.tid = v.fmIFE[0].tid;
        toIFE.rsp = 1;
        toIFE.hit = find;
        toIFE.exp = exp;
        toIFE.eobit = evenOddBit;
      end
      
      if(rspDSE && v.fmDSE != null && v.fmDSE.req) begin
        if(toDSE == null) toDSE = tr_tlb2dse::type_id::create("toDSE", this);
        toDSE.pfn = varPFN;///varPAdr;
        toDSE.eobit = evenOddBit;
        toDSE.hit = find;
        toDSE.exp = exp;
        toDSE.c = varC;
        toDSE.k = varK;
        toDSE.e = varE;
      end   
    end   
    
    /// tlb support instruction  spu -> tlb
    /// dse:    | rrf | rrc0 |  ag  |  tag |  sel |  dc  | dwbp |  dwb |
    /// spu:    | rrf | rrc0 | exs0 | exs1 | exs2 | exs3 | swbp |  swb |
    ///                             |      |
    ///                          request  respond 
   
    if(v.fmSPU != null && v.fmSPU.req) begin
      case(v.fmSPU.op)
      /// TLBP
      op_tlbp:
        for (int i = 0; i < NUM_TLB_E; i++) begin
          varMask = page_mask_table[v.pageTyp[i]];
          if((v.vpn2[i] & ~varMask) == (v.srEntryHi[WORD_WIDTH - 1 : WORD_WIDTH - VPN2_WIDTH] & ~varMask)
              && ((v.g[i] == 1) || (v.asid[i] == v.srEntryHi[ASID_WIDTH-1:0])))
            vn.srIndex = i;
        end
      
      /// TLBPR
      op_tlbr:
      begin
        int i = v.srIndex;
        if(i < NUM_TLB_E) begin
          varMask = page_mask_table[v.pageTyp[i]];
          vn.srEntryHi[TYPE_WIDTH + ASID_WIDTH - 1: ASID_WIDTH] = v.pageTyp[i];
          vn.srEntryHi[WORD_WIDTH - 1 : WORD_WIDTH - VPN2_WIDTH] = v.vpn2[i] & ~varMask;
          vn.srEntryHi[ASID_WIDTH-1:0] = v.asid[i];
          
          vn.srEntryLo1[0] = v.g[i];
          vn.srEntryLo1[WORD_WIDTH - 1 : WORD_WIDTH - PFN_WIDTH] = v.pfn2o[i];
          vn.srEntryLo1[8] = v.ex[1][i];
          vn.srEntryLo1[7:5] = v.c[1][i];
          vn.srEntryLo1[4] = v.k[1][i];
          vn.srEntryLo1[3] = v.e[1][i];
          vn.srEntryLo1[2] = v.d[1][i];
          vn.srEntryLo1[1] = v.v[1][i];
          
          vn.srEntryLo0[0] = v.g[i];
          vn.srEntryLo0[WORD_WIDTH - 1 : WORD_WIDTH - PFN_WIDTH] = v.pfn2e[i];
          vn.srEntryLo0[8] = v.ex[0][i];
          vn.srEntryLo0[7:5] = v.c[0][i];
          vn.srEntryLo0[4] = v.k[0][i];
          vn.srEntryLo0[3] = v.e[0][i];
          vn.srEntryLo0[2] = v.d[0][i];
          vn.srEntryLo0[1] = v.v[0][i];
        end
     end   
     /// TLBWI
      op_tlbwi:
      begin
        int i = v.srIndex;
        varMask = page_mask_table[v.pageTyp[i]];
        vn.pageTyp[i] = v.srEntryHi[TYPE_WIDTH + ASID_WIDTH - 1: ASID_WIDTH];
        vn.vpn2[i] = v.srEntryHi[WORD_WIDTH - 1 : WORD_WIDTH - VPN2_WIDTH] & ~varMask;
        vn.asid[i] = v.srEntryHi[ASID_WIDTH-1:0];
        vn.g[i] = v.srEntryLo1[0] && v.srEntryLo0[0];
        
        vn.pfn2o[i] = v.srEntryLo1[WORD_WIDTH - 1 : WORD_WIDTH - PFN_WIDTH] & ~varMask;
        vn.ex[1][i] = v.srEntryLo1[8];
        vn.c[1][i] = v.srEntryLo1[7:5];
        vn.k[1][i] = v.srEntryLo1[4];
        vn.e[1][i] = v.srEntryLo1[3]; vn.d[1][i] = v.srEntryLo1[2];
        vn.v[1][i] = v.srEntryLo1[1];
        
        vn.pfn2e[i] = v.srEntryLo0[WORD_WIDTH - 1 : WORD_WIDTH - PFN_WIDTH] & ~varMask;
        vn.ex[0][i] = v.srEntryLo0[8];
        vn.c[0][i] = v.srEntryLo0[7:5];
        vn.k[0][i] = v.srEntryLo0[4];
        vn.e[0][i] = v.srEntryLo0[3]; vn.d[0][i] = v.srEntryLo0[2];
        vn.v[0][i] = v.srEntryLo0[1];
      end
      /// TLBWR
      op_tlbwr:
      begin
        int i = v.srIndex;
        varMask = page_mask_table[v.pageTyp[i]];
        vn.pageTyp[i] = v.srEntryHi[TYPE_WIDTH + ASID_WIDTH - 1: ASID_WIDTH];
        vn.vpn2[i] = v.srEntryHi[WORD_WIDTH - 1 : WORD_WIDTH - VPN2_WIDTH] & ~varMask;
        vn.asid[i] = v.srEntryHi[ASID_WIDTH - 1 : 0];
        vn.g[i] = v.srEntryLo1[0] && v.srEntryLo0[0];
        vn.pfn2o[i] = v.srEntryLo1[WORD_WIDTH - 1 : 9] & ~varMask;
        vn.ex[1][i] = v.srEntryLo1[8]; vn.c[1][i] = v.srEntryLo1[7:5]; vn.k[1][i] = v.srEntryLo1[4];
        vn.e[1][i] = v.srEntryLo1[3]; vn.d[1][i] = v.srEntryLo1[2]; vn.v[1][i] = v.srEntryLo1[1];
        vn.pfn2e[i] = v.srEntryLo0[WORD_WIDTH - 1 : 9] & ~varMask;
        vn.ex[0][i] = v.srEntryLo0[8]; vn.c[0][i] = v.srEntryLo0[7:5]; vn.k[0][i] = v.srEntryLo0[4];
        vn.e[0][i] = v.srEntryLo0[3]; vn.d[0][i] = v.srEntryLo0[2]; vn.v[0][i] = v.srEntryLo0[1];
      end
      /// GP2S
      op_gp2s:
        case(v.fmSPU.srAdr)
        SR_CONTENT:   vn.srContent[v.fmSPU.tid]  = v.fmSPU.op0;
        SR_INDEX:     vn.srIndex    = v.fmSPU.op0;
        SR_RANDOM:    vn.srRandom   = v.fmSPU.op0;
        SR_ENTRY_L0:  vn.srEntryLo0 = v.fmSPU.op0;
        SR_ENTRY_L1:  vn.srEntryLo1 = v.fmSPU.op0;
        SR_ENTRY_HI:  vn.srEntryHi  = v.fmSPU.op0;
        SR_ASID:      vn.srASID[v.fmSPU.tid] = v.fmSPU.op0 & `GML(ASID_WIDTH);
        default: ovm_report_warning("SPU_SRAD", "spu WRITE SR_ADDR IS ERROR!!!");
        endcase 
        
    /// S2GP
      op_s2gp:
      begin
        if(vn.spu[1] == null)
          vn.spu[1] = tr_tlb2spu::type_id::create("toSPU", this);
        case(v.fmSPU.srAdr)
        SR_CONTENT:   vn.spu[1].res = v.srContent[v.fmSPU.tid];
        SR_INDEX:     vn.spu[1].res = v.srIndex;
        SR_RANDOM:    vn.spu[1].res = v.srRandom;
        SR_ENTRY_L0:  vn.spu[1].res = v.srEntryLo0;
        SR_ENTRY_L1:  vn.spu[1].res = v.srEntryLo1;
        SR_ENTRY_HI:  vn.spu[1].res = v.srEntryHi;
        SR_ASID:      vn.spu[1].res = v.srASID[v.fmSPU.tid];
        default: ovm_report_warning("SPU_SRAD", "spu READ SR_ADDR IS ERROR!!!");
        endcase 
      end
      endcase
    end
   
  endfunction
  
  function void req_proc();
    tr_tlb2spu toSPU;
    
    ovm_report_info("tlb", "req_proc procing...", OVM_FULL); 
    
    /// send to spu
    toSPU = v.spu[STAG_TLB_SPU];
    
    /// req to other module
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toIFE != null) void'(ife_tr_port.nb_transport(toIFE, toIFE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_dse(input tr_dse2tlb req, output tr_dse2tlb rsp);
    ovm_report_info("tlb_tr", "Get dse Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse
  
  function bit nb_transport_spu(input tr_spu2tlb req, output tr_spu2tlb rsp);
    ovm_report_info("tlb_tr", "Get spu Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_ife(input tr_ife2tlb req, output tr_ife2tlb rsp);
    ovm_report_info("tlb_tr", "Get ife Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    
    if(vn.ifeBufPtr == IFE_REQ_BUF)
      ovm_report_warning("TLB_BUF_OVERFLOW", "tlb fmIFE OVERFLOW");
    else begin
      vn.fmIFE[v.ifeBufPtr] = req;
      vn.ifeBufPtr = v.ifeBufPtr + 1;
    end
    return 1;
  endfunction : nb_transport_ife  
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
    ife_tr_imp = new("ife_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ife_tr_port = new("ife_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
   
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_tlb

///-------------------------------------other functions-----------------------------------------
  
