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

parameter uchar INDEX_ENT    = 7 , /// entry bits
                NUM_TLB_E    = 1 << INDEX_ENT,  ///128
                VPN2_WIDTH   = 18,
                MASK_WIDTH   = 15,
                TYPE_WIDTH   = 3,  /// Page Size Type bit width
                ASID_WIDTH   = 8,
                IFE_REQ_BUF  = 2,
                SR_CONTENT   = 6,
                SR_INDEX     = 7,
                SR_RANDOM    = 8,
                SR_ENTRY_L0  = 9,
                SR_ENTRY_L1  = 10,
                SR_ENTRY_HI  = 11,
                SR_PAGE_TYP  = 12,
                STAG_TLB_SPU = STAGE_RRF_SWBP - STAGE_RRF_EXS1 - 1 -1,
                VADD_START   = 14,  /// 8K 14BIT START for tlb and dse
                PFN_WIDTH    = 23,
                PHY_WIDTH    = VADD_START + PFN_WIDTH;

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

class ip4_tlm_tlb_vars extends ovm_component;
  tr_dse2tlb fmDSE;
  tr_spu2tlb fmSPU;
  tr_ife2tlb fmIFE[IFE_REQ_BUF];
  uchar ifeBufPtr;
  
  tr_tlb2dse dse;
  tr_tlb2ife ife;
  tr_tlb2spu spu[STAG_TLB_SPU:1];
  
  uint vpn2[NUM_TLB_E];
  uchar ptyp[NUM_TLB_E];
  uchar asid[NUM_TLB_E];
  bit g[NUM_TLB_E]; 
  
  uint  pfn2e[NUM_TLB_E], pfn2o[NUM_TLB_E];
  bit[NUM_TLB_E-1:0] ex[2], c[2], k[2], e[2], d[2], v[2];
  
  word srIndex;
  word srRandom;
  word srEntryLo0;
  word srEntryLo1;
  word srEntryHi;
  word srPageType;
  word srContent;  

  `ovm_component_utils_begin(ip4_tlm_tlb_vars)
    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(vpn2, OVM_ALL_ON)
    `ovm_field_sarray_int(ptyp, OVM_ALL_ON)
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
    `ovm_field_int(srPageType, OVM_ALL_ON)
    `ovm_field_int(srContent, OVM_ALL_ON)
    `ovm_field_int(ifeBufPtr, OVM_ALL_ON)
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
    vpn2 = '{default : 0};
    ptyp = '{default : 0};
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
    srPageType = 0;
    srContent = 0;
    ifeBufPtr = 0;
  endfunction : new
endclass : ip4_tlm_tlb_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_tlb extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_tlb_vars v, vn;
  
/*  
  local bit find;
  word vir_adr; 
  bit [PHY_width-1:0] var_padr;
  */
  `ovm_component_utils_begin(ip4_tlm_tlb)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_spu #(tr_spu2tlb, tr_spu2tlb, ip4_tlm_tlb) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2tlb, tr_dse2tlb, ip4_tlm_tlb) dse_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2tlb, tr_ife2tlb, ip4_tlm_tlb) ife_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_tlb2spu, tr_tlb2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2dse, tr_tlb2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2ife, tr_tlb2ife) ife_tr_port;
    
  function void comb_proc();
/*    uchar EvenOddBit; 
    bit[MASK_WIDTH-1:0] var_mask[NUM_TLB_E];
    bit[PFN_WIDTH-1:0] var_pfn;
    bit var_ex = 0, var_c = 0, var_k = 0;
    bit var_e = 0,  var_d = 0, var_v = 0, var_g = 0;  
    word i0, i1, i2;
    uchar var_tid;
    bit rsp_dse = 0, rsp_ife = 0, exp = 0;
    
    ovm_report_info("TLB", "comb_proc procing...", OVM_FULL);
     
    if(v.fmDSE != null) end_tr(v.fmDSE);
    if(v.fmSPU != null) end_tr(v.fmSPU);
    vn.fmDSE = null;
    vn.fmSPU = null;
    
    if(v.fmDSE != null && !v.fmDSE.req) begin
      rsp_ife = 1;
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
      rsp_dse = 1;
      
    for (int i = STAG_TLB_SPU; i > 1; i--)
      vn.spu[i] = v.spu[i-1];
    vn.spu[1] = null;
    
      for (int i = 0; i < NUM_TLB_E; i++)begin
            /// the page type ---> mask
        case(v.ptyp[i])
          pagetype0: var_mask[i] = pagemask0;
          pagetype1: var_mask[i] = pagemask1;
          pagetype2: var_mask[i] = pagemask2;
          pagetype3: var_mask[i] = pagemask3;
          pagetype4: var_mask[i] = pagemask4;
          pagetype5: var_mask[i] = pagemask5;
          pagetype6: var_mask[i] = pagemask6;
          default: ovm_report_warning("TLBPSize0_ILLEGAL", "No this type page size, and no mask!!!");
        endcase   
      end
      
    find = 0;
    ///tlb basic function
    if(rsp_dse || rsp_ife)begin
      if(rsp_ife && v.fmIFE[0] != null) begin
        vir_adr = v.fmIFE[0].vAdr;
        var_tid = v.fmIFE[0].tid;
      end
      else if(v.fmDSE != null) begin
        vir_adr = v.fmDSE.vAdrHi;
        var_tid = v.fmDSE.tid;
      end
      
      for (int i = 0; i < NUM_TLB_E; i++)begin
            case(v.ptyp[i])
              pagetype0: EvenOddBit = 13;
              pagetype1: EvenOddBit = 16;
              pagetype2: EvenOddBit = 19;
              pagetype3: EvenOddBit = 22;
              pagetype4: EvenOddBit = 24;
              pagetype5: EvenOddBit = 26;
              pagetype6: EvenOddBit = 28;
              default:  ovm_report_warning("TLBPSize1_ILLEGAL", "No this type page size, and no evenoddbit!!!");              
            endcase
            if(((v.vpn2[i] && (!var_mask[i])) == (vir_adr[31:VADD_START] && (!var_mask[i]))) 
                  && (v.g[i] || (v.asid[i][var_tid] == v.srEntryHi[ASID_WIDTH-1:0])))begin
                if(vir_adr[EvenOddBit] == 0)begin
                  var_pfn = v.pfn2Evn[i];
                  var_v   = v.vEvn[i];
                  var_c   = v.cEvn[i];
                  var_ex  = v.exEvn[i];
                  var_d   = v.dEvn[i];
                end
                else begin
                  var_pfn = v.pfn2Odd[i];
                  var_v   = v.vOdd[i];
                  var_c   = v.cOdd[i];
                  var_ex  = v.exOdd[i];
                  var_d   = v.dOdd[i];
                end
                if(var_v == 0)begin
                  ovm_report_info("TLB_Invalid", "TLB Invalid exception!!!", OVM_HIGH); 
                  vn.srContent[4:0] = 0;
                  vn.srContent[22:5] = v.vpn2[i];
                  exp = 1;
                  break;
                end
                
                if(rsp_dse && (var_d == 0) && ((v.fmDSE.op == op_sw) || (v.fmDSE.op == op_sh) || (v.fmDSE.op == op_sb)))begin
                  ovm_report_info("TLB_Modified", "TLB Modified exception!!!", OVM_HIGH); 
                  vn.srContent[4:0] = 0;
                  vn.srContent[22:5] = v.vpn2[i];
                  exp = 1;
                  break;
                end

                if((!var_ex) && rsp_ife)begin
                  ovm_report_info("TLB_EX", "TLB NON_EXECUTION exception!!!", OVM_HIGH); 
                  vn.srContent[4:0] = 0;
                  vn.srContent[22:5] = v.vpn2[i];
                  exp = 1;
                  break;
                end
                                
                for (int n = EvenOddBit; n < PHY_width; n++)
                  var_padr[n] = var_pfn[n-EvenOddBit];      
                find = 1;
                break;
            end  
      end    
      if(rsp_ife) begin
        if(vn.ife == null) vn.ife = tr_tlb2ife::type_id::create("toIFE", this);
        vn.ife.pAdr = var_padr;
        vn.ife.tid = v.fmIFE[0].tid;
        vn.ife.rsp = 1;
        vn.ife.hit = find;
        vn.ife.exp = exp;
      end    
      if(rsp_dse) begin
        if(vn.dse == null) vn.dse = tr_tlb2dse::type_id::create("toDSE", this);
        vn.dse.phy_adr = var_padr;
        vn.dse.eobit = EvenOddBit;
        vn.dse.hit = find;
        vn.dse.exp = exp;
      end   
    end   
    /// tlb support instruction  spu -> tlb
    /// dse:    | rrf | rrc0 |  ag  |  tag |  sel |  dc  | dwbp |  dwb |
    /// spu:    | rrf | rrc0 | exs0 | exs1 | exs2 | exs3 | swbp |  swb |
    ///                             |      |
    ///                          request  respond 
    
    if(v.fmSPU != null && v.fmSPU.req)begin
      case(v.fmSPU.op)
      /// TLBP
      op_tlbp:
        for (int i = 0; i < NUM_TLB_E; i++)begin
          if((v.vpn2[i] && (!var_mask[i])) == (v.srEntryHi[WORD_WIDTH-1:WORD_WIDTH-VPN2_WIDTH] && (!var_mask[i]))
              && ((v.g[i] == 1) || (v.asid[i][v.fmDSE.tid] == v.srEntryHi[ASID_WIDTH-1:0])))
              vn.srIndex = i;
        end
      
      /// TLBPR
      op_tlbr:
      begin
        i0 = v.srIndex;
        if(i0 < NUM_TLB_E) begin
          vn.srPageType[TYPE_WIDTH-1:0] = v.ptyp[i0];
          vn.srEntryHi = {(v.vpn2[i0] && (!var_mask[i0])), 6'b0, v.asid[i0][v.fmDSE.tid]};
          vn.srEntryLo1 = {(v.pfn2Odd[i0] && (!var_mask[i0])), v.exOdd[i0], 
                           v.cOdd[i0], v.kOdd[i0], v.eOdd[i0], v.dOdd[i0],
                           v.vOdd[i0], v.gOdd[i0]};
          vn.srEntryLo0 = {(v.pfn2Evn[i0] && (!var_mask[i0])), v.exEvn[i0], 
                           v.cEvn[i0], v.kEvn[i0], v.eEvn[i0], v.dEvn[i0],
                           v.vEvn[i0], v.gEvn[i0]}; 
        end
     end   
     /// TLBWI
      op_tlbwi:
      begin
        i1 = v.srIndex;
        vn.ptyp[i1] = v.srPageType[TYPE_WIDTH-1:0];
        vn.vpn2[i1] = v.srEntryHi[WORD_WIDTH-1:WORD_WIDTH-VPN2_WIDTH] && (!var_mask[i1]);
        vn.asid[i1][v.fmDSE.tid] = v.srEntryHi[ASID_WIDTH-1:0];
        vn.g[i1] = v.srEntryLo1[0] && v.srEntryLo0[0];
        vn.pfn2Odd[i1] = v.srEntryLo1[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.exOdd[i1] = v.srEntryLo1[8]; vn.cOdd[i1] = v.srEntryLo1[7:5]; vn.kOdd[i1] = v.srEntryLo1[4];
        vn.eOdd[i1] = v.srEntryLo1[3]; vn.dOdd[i1] = v.srEntryLo1[2]; vn.vOdd[i1] = v.srEntryLo1[1];
        vn.pfn2Evn[i1] = v.srEntryLo0[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.exEvn[i1] = v.srEntryLo0[8]; vn.cEvn[i1] = v.srEntryLo0[7:5]; vn.kEvn[i1] = v.srEntryLo0[4];
        vn.eEvn[i1] = v.srEntryLo0[3]; vn.dEvn[i1] = v.srEntryLo0[2]; vn.vEvn[i1] = v.srEntryLo0[1];
      end
      /// TLBWR
      op_tlbwr:
      begin
        i1 = v.srIndex;
        vn.ptyp[i2] = v.srPageType[TYPE_WIDTH-1:0];
        vn.vpn2[i2] = v.srEntryHi[WORD_WIDTH-1:WORD_WIDTH-VPN2_WIDTH] && (!var_mask[i1]);
        vn.asid[i2][v.fmDSE.tid] = v.srEntryHi[ASID_WIDTH-1:0];
        vn.g[i2] = v.srEntryLo1[0] && v.srEntryLo0[0];
        vn.pfn2Odd[i2] = v.srEntryLo1[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.exOdd[i2] = v.srEntryLo1[8]; vn.cOdd[i2] = v.srEntryLo1[7:5]; vn.kOdd[i2] = v.srEntryLo1[4];
        vn.eOdd[i2] = v.srEntryLo1[3]; vn.dOdd[i2] = v.srEntryLo1[2]; vn.vOdd[i2] = v.srEntryLo1[1];
        vn.pfn2Evn[i2] = v.srEntryLo0[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.exEvn[i2] = v.srEntryLo0[8]; vn.cEvn[i2] = v.srEntryLo0[7:5]; vn.kEvn[i2] = v.srEntryLo0[4];
        vn.eEvn[i2] = v.srEntryLo0[3]; vn.dEvn[i2] = v.srEntryLo0[2]; vn.vEvn[i2] = v.srEntryLo0[1];
      end
      /// GP2S
      op_gp2s:
        case(v.fmSPU.srAdr)
        SR_CONTENT: vn.srContent = v.fmSPU.op0;
        SR_INDEX: vn.srIndex = v.fmSPU.op0;
        SR_RANDOM: vn.srRandom = v.fmSPU.op0;
        SR_ENTRY_L0: vn.srEntryLo0 = v.fmSPU.op0;
        SR_ENTRY_L1: vn.srEntryLo1 = v.fmSPU.op0;
        SR_ENTRY_HI: vn.srEntryHi = v.fmSPU.op0;
        SR_PAGE_TYP: vn.srPageType = v.fmSPU.op0;
        default: ovm_report_warning("SPU_SRAD", "spu WRITE SR_ADDR IS ERROR!!!");
        endcase 
        
    /// S2GP
      op_s2gp:
      begin
        if(vn.spu[1] == null)
          vn.spu[1] = tr_tlb2spu::type_id::create("toSPU", this);
        case(v.fmSPU.srAdr)
        SR_CONTENT: vn.spu[1].res = v.srContent;
        SR_INDEX: vn.spu[1].res = v.srIndex;
        SR_RANDOM: vn.spu[1].res = v.srRandom;
        SR_ENTRY_L0: vn.spu[1].res = v.srEntryLo0;
        SR_ENTRY_L1: vn.spu[1].res = v.srEntryLo1;
        SR_ENTRY_HI: vn.spu[1].res = v.srEntryHi;
        SR_PAGE_TYP: vn.spu[1].res = v.srPageType;
        default: ovm_report_warning("SPU_SRAD", "spu READ SR_ADDR IS ERROR!!!");
        endcase 
      end
      endcase
    end
*/    
  endfunction
  
  function void req_proc();
    tr_tlb2dse toDSE;
    tr_tlb2spu toSPU;
    tr_tlb2ife toIFE;
    
    ovm_report_info("TLB", "req_proc procing...", OVM_FULL); 
   
    /// send to dse
    toDSE = v.dse;
    
    /// send to spu
    toSPU = v.spu[STAG_TLB_SPU];     ///
   
    /// send to ife
    toIFE = v.ife;    
    
    /// req to other module
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toIFE != null) void'(ife_tr_port.nb_transport(toIFE, toIFE));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_dse(input tr_dse2tlb req, output tr_dse2tlb rsp);
    ovm_report_info("DSE2TLB_TR", "Get dse Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse
  
  function bit nb_transport_spu(input tr_spu2tlb req, output tr_spu2tlb rsp);
    ovm_report_info("SPU2TLB_TR", "Get spu Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_ife(input tr_ife2tlb req, output tr_ife2tlb rsp);
    ovm_report_info("IFE2TLB_TR", "Get ife Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    
    if(vn.ifeBufPtr == IFE_REQ_BUF)
      ovm_report_warning("TLB_BUF_OVERFLOW", "TLB fmIFE OVERFLOW");
    else begin
      vn.fmIFE[v.ifeBufPtr] = req;
      vn.ifeBufPtr = v.ifeBufPtr + 1;
    end
    return 1;
  endfunction : nb_transport_ife  
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
  
