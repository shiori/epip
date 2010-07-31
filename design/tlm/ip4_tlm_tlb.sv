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

parameter uchar Index_EBit  = 7 , /// entry bits
                Entry_NUM  = 1 << Index_EBit,  ///128
                VPN2_width = 18,
                MASK_width = 15,
                TYPE_width = 3,  /// Page Size Type bit width
                ASID_width = 8;
                

parameter uchar   pagetype0 = 0,   /// 8K
                  pagetype1 = 1,   /// 64K
                  pagetype2 = 2,   /// 512k
                  pagetype3 = 3,   /// 4M
                  pagetype4 = 4,   /// 16M
                  pagetype5 = 5,   /// 64M
                  pagetype6 = 6;   /// 256M 
                  
parameter word  pagemask0 = 15'b000000000000000,   /// 8K
                pagemask1 = 15'b000000000000111,   /// 64K
                pagemask2 = 15'b000000000111111,   /// 512k
                pagemask3 = 15'b000000111111111,   /// 4M
                pagemask4 = 15'b000011111111111,   /// 16M
                pagemask5 = 15'b001111111111111,   /// 64M
                pagemask6 = 15'b111111111111111;   /// 256M

parameter uchar IFE_REQ_BUF = 2;
                
parameter uchar RContent_NO = 6,
                RIndex_NO = 7,
                RRandom_NO = 8,
                REntryLo0_NO = 9,
                REntryLo1_NO = 10,
                REntryHi_NO = 11,
                RPageType_NO = 12;

parameter uchar num_sstage = 2,
                sstage_max = num_sstage - 1; /// spu pipeline in the tlb

class ip4_tlm_tlb_vars extends ovm_object;
  
  tr_dse2tlb fm_dse;
  tr_spu2tlb fm_spu;
  tr_ife2tlb fm_ife[IFE_REQ_BUF];
  uchar ife_buf_ptr;
  
  tr_tlb2dse dse;
  tr_tlb2ife ife;
  tr_tlb2spu spu[sstage_max:1];
  
  bit[VPN2_width-1:0] tlb_vpn2[Entry_NUM-1:0];
///  bit[MASK_width-1:0] tlb_mask[Entry_NUM-1:0];
  bit[TYPE_width-1:0] tlb_ptype[Entry_NUM-1:0];
  bit[num_thread-1:0][ASID_width-1:0] tlb_asid[Entry_NUM-1:0];
  bit                 tlb_G[Entry_NUM-1:0]; 
  
  bit[PFN_width-1:0]  tlb_pfn20[Entry_NUM-1:0];
  bit tlb_EX0[Entry_NUM-1:0], tlb_C0[Entry_NUM-1:0], tlb_K0[Entry_NUM-1:0];
  bit tlb_E0[Entry_NUM-1:0], tlb_D0[Entry_NUM-1:0], tlb_V0[Entry_NUM-1:0];
  bit                 tlb_G0[Entry_NUM-1:0]; 
  
  bit[PFN_width-1:0]  tlb_pfn21[Entry_NUM-1:0];
  bit tlb_EX1[Entry_NUM-1:0], tlb_C1[Entry_NUM-1:0], tlb_K1[Entry_NUM-1:0];
  bit tlb_E1[Entry_NUM-1:0], tlb_D1[Entry_NUM-1:0], tlb_V1[Entry_NUM-1:0];
  bit                 tlb_G1[Entry_NUM-1:0]; 
  
  word RIndex;
  word RRandom;
  word REntryLo0;
  word REntryLo1;
  word REntryHi;
  word RPageType;
  word RContent;  
    
  `ovm_object_utils_begin(ip4_tlm_tlb_vars)
    `ovm_field_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(tlb_vpn2, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_ptype, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_asid, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_G, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_pfn20, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_EX0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_C0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_K0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_E0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_D0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_V0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_G0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_pfn21, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_EX1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_C1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_K1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_E1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_D1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_V1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_G1, OVM_ALL_ON)
    `ovm_field_int(RIndex, OVM_ALL_ON)
    `ovm_field_int(RRandom, OVM_ALL_ON)
    `ovm_field_int(REntryLo0, OVM_ALL_ON)
    `ovm_field_int(REntryLo1, OVM_ALL_ON)
    `ovm_field_int(REntryHi, OVM_ALL_ON)
    `ovm_field_int(RPageType, OVM_ALL_ON)
    `ovm_field_int(RContent, OVM_ALL_ON)
    `ovm_field_int(ife_buf_ptr, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function new (string name = "tlb_vars");
    super.new(name);
    tlb_vpn2 = '{default : 0};
    tlb_ptype = '{default : 0};
    tlb_asid = '{default : 0};
    tlb_G    = '{default : 0};
    tlb_EX0 = '{default : 0};
    tlb_C0 = '{default : 0};
    tlb_K0 = '{default : 0};
    tlb_E0 = '{default : 0};
    tlb_D0 = '{default : 0};
    tlb_V0 = '{default : 0};
    tlb_EX1 = '{default : 0};
    tlb_C1 = '{default : 0};
    tlb_K1 = '{default : 0};
    tlb_E1 = '{default : 0};
    tlb_D1 = '{default : 0};
    tlb_V1 = '{default : 0};
    RIndex = 0;
    RRandom = 0;
    REntryLo0 = 0;
    REntryLo1 = 0;
    REntryHi = 0;
    RPageType = 0;
    RContent = 0;
    ife_buf_ptr = 0;
  endfunction : new
  
  function void gen(input ip4_tlm_tlb_vars o);
    this.copy(o);
  endfunction  
endclass : ip4_tlm_tlb_vars


///---------------------------------------main component----------------------------------------
class ip4_tlm_tlb extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  local ip4_tlm_tlb_vars v, vn;
  
  
  local bit find;
  word vir_adr; 
  word var_padr;
  
  `ovm_component_utils_begin(ip4_tlm_tlb)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_spu #(tr_spu2tlb, tr_spu2tlb, ip4_tlm_tlb) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2tlb, tr_dse2tlb, ip4_tlm_tlb) dse_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2tlb, tr_ife2tlb, ip4_tlm_tlb) ife_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_tlb2spu, tr_tlb2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2dse, tr_tlb2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2ife, tr_tlb2ife) ife_tr_port;
    
  function void comb_proc();
    uchar EvenOddBit; 
    bit[MASK_width-1:0] var_mask[Entry_NUM];
    bit[PFN_width-1:0] var_pfn;
    bit var_ex = 0, var_c = 0, var_k = 0;
    bit var_e = 0,  var_d = 0, var_v = 0, var_g = 0;  
    word i0, i1, i2;
    uchar var_tid;
    bit rsp_dse = 0, rsp_ife = 0, exp = 0;
    
    ovm_report_info("TLB", "comb_proc procing...", OVM_HIGH);
     
    if(v.fm_dse != null) end_tr(v.fm_dse);
    if(v.fm_spu != null) end_tr(v.fm_spu);
    vn.fm_dse = null;
    vn.fm_spu = null;
    
    if(v.fm_dse != null && !v.fm_dse.req) begin
      rsp_ife = 1;
      if(v.fm_ife[0] != null) end_tr(v.fm_ife[0]);
      for(int i = 1; i < v.ife_buf_ptr; i++)
        vn.fm_ife[i-1] = v.fm_ife[i];
      if(v.ife_buf_ptr > 0) begin
        vn.fm_ife[v.ife_buf_ptr-1] = null;
        vn.ife_buf_ptr = v.ife_buf_ptr - 1;
      end
      else
       vn.fm_ife[0] = null;
    end
    else if(v.fm_dse != null)
      rsp_dse = 1;
      
    for (int i = sstage_max; i > 1; i--)
      vn.spu[i] = v.spu[i-1];
    vn.spu[1] = null;
    
      for (int i = 0; i < Entry_NUM; i++)begin
            /// the page type ---> mask
        case(v.tlb_ptype[i])
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
      if(rsp_ife && v.fm_ife[0] != null) begin
        vir_adr = v.fm_ife[0].v_adr;
        var_tid = v.fm_ife[0].tid;
      end
      else if(v.fm_dse != null) begin
        vir_adr = v.fm_dse.v_adrh;
        var_tid = v.fm_dse.tid;
      end
      
      for (int i = 0; i < Entry_NUM; i++)begin
            case(v.tlb_ptype[i])
              pagetype0: EvenOddBit = 13;
              pagetype1: EvenOddBit = 16;
              pagetype2: EvenOddBit = 19;
              pagetype3: EvenOddBit = 22;
              pagetype4: EvenOddBit = 24;
              pagetype5: EvenOddBit = 26;
              pagetype6: EvenOddBit = 28;
              default:  ovm_report_warning("TLBPSize1_ILLEGAL", "No this type page size, and no evenoddbit!!!");              
            endcase
            if(((v.tlb_vpn2[i] && (!var_mask[i])) == (vir_adr[31:VADD_START] && (!var_mask[i]))) 
                  && (v.tlb_G[i] || (v.tlb_asid[i][var_tid] == v.REntryHi[ASID_width-1:0])))begin
                if(vir_adr[EvenOddBit] == 0)begin
                  var_pfn = v.tlb_pfn20[i];
                  var_v   = v.tlb_V0[i];
                  var_c   = v.tlb_C0[i];
                  var_ex  = v.tlb_EX0[i];
                  var_d   = v.tlb_D0[i];
                end
                else begin
                  var_pfn = v.tlb_pfn21[i];
                  var_v   = v.tlb_V1[i];
                  var_c   = v.tlb_C1[i];
                  var_ex  = v.tlb_EX1[i];
                  var_d   = v.tlb_D1[i];
                end
                if(var_v == 0)begin
                  ovm_report_info("TLB_Invalid", "TLB Invalid exception!!!", OVM_HIGH); 
                  vn.RContent[4:0] = 0;
                  vn.RContent[22:5] = v.tlb_vpn2[i];
                  exp = 1;
                  break;
                end
                
                if(rsp_dse && (var_d == 0) && ((v.fm_dse.op == op_sw) || (v.fm_dse.op == op_sh) || (v.fm_dse.op == op_sb)))begin
                  ovm_report_info("TLB_Modified", "TLB Modified exception!!!", OVM_HIGH); 
                  vn.RContent[4:0] = 0;
                  vn.RContent[22:5] = v.tlb_vpn2[i];
                  exp = 1;
                  break;
                end

                if((!var_ex) && rsp_ife)begin
                  ovm_report_info("TLB_EX", "TLB NON_EXECUTION exception!!!", OVM_HIGH); 
                  vn.RContent[4:0] = 0;
                  vn.RContent[22:5] = v.tlb_vpn2[i];
                  exp = 1;
                  break;
                end
                                
                for (int n = EvenOddBit; n < PFN_width+EvenOddBit; n++)
                 var_padr[n] = var_pfn[n-EvenOddBit];      /// 23 
                find = 1;
                break;
            end  
      end    
      if(rsp_ife) begin
        if(vn.ife == null) vn.ife = tr_tlb2ife::type_id::create("to_ife", this);
        vn.ife.p_adr = var_padr;
        vn.ife.tid = v.fm_ife[0].tid;
        vn.ife.rsp = 1;
        vn.ife.hit = find;
        vn.ife.exp = exp;
      end    
      if(rsp_dse) begin
        if(vn.dse == null) vn.dse = tr_tlb2dse::type_id::create("to_dse", this);
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
    
    if(v.fm_spu != null && v.fm_spu.req)begin
      case(v.fm_spu.op)
      /// TLBP
      op_tlbp:
        for (int i = 0; i < Entry_NUM; i++)begin
          if((v.tlb_vpn2[i] && (!var_mask[i])) == (v.REntryHi[word_width-1:word_width-VPN2_width] && (!var_mask[i]))
              && ((v.tlb_G[i] == 1) || (v.tlb_asid[i][v.fm_dse.tid] == v.REntryHi[ASID_width-1:0])))
              vn.RIndex = i;
        end
      
      /// TLBPR
      op_tlbr:
      begin
        i0 = v.RIndex;
        if(i0 < Entry_NUM) begin
          vn.RPageType[TYPE_width-1:0] = v.tlb_ptype[i0];
          vn.REntryHi = {(v.tlb_vpn2[i0] && (!var_mask[i0])), 6'b0, v.tlb_asid[i0][v.fm_dse.tid]};
          vn.REntryLo1 = {(v.tlb_pfn21[i0] && (!var_mask[i0])), v.tlb_EX1[i0], 
                           v.tlb_C1[i0], v.tlb_K1[i0], v.tlb_E1[i0], v.tlb_D1[i0],
                           v.tlb_V1[i0], v.tlb_G1[i0]};
          vn.REntryLo0 = {(v.tlb_pfn20[i0] && (!var_mask[i0])), v.tlb_EX0[i0], 
                           v.tlb_C0[i0], v.tlb_K0[i0], v.tlb_E0[i0], v.tlb_D0[i0],
                           v.tlb_V0[i0], v.tlb_G0[i0]}; 
        end
     end   
     /// TLBWI
      op_tlbwi:
      begin
        i1 = v.RIndex;
        vn.tlb_ptype[i1] = v.RPageType[TYPE_width-1:0];
        vn.tlb_vpn2[i1] = v.REntryHi[word_width-1:word_width-VPN2_width] && (!var_mask[i1]);
        vn.tlb_asid[i1][v.fm_dse.tid] = v.REntryHi[ASID_width-1:0];
        vn.tlb_G[i1] = v.REntryLo1[0] && v.REntryLo0[0];
        vn.tlb_pfn21[i1] = v.REntryLo1[word_width-1:9] && (!var_mask[i1]);
        vn.tlb_EX1[i1] = v.REntryLo1[8]; vn.tlb_C1[i1] = v.REntryLo1[7:5]; vn.tlb_K1[i1] = v.REntryLo1[4];
        vn.tlb_E1[i1] = v.REntryLo1[3]; vn.tlb_D1[i1] = v.REntryLo1[2]; vn.tlb_V1[i1] = v.REntryLo1[1];
        vn.tlb_pfn20[i1] = v.REntryLo0[word_width-1:9] && (!var_mask[i1]);
        vn.tlb_EX0[i1] = v.REntryLo0[8]; vn.tlb_C0[i1] = v.REntryLo0[7:5]; vn.tlb_K0[i1] = v.REntryLo0[4];
        vn.tlb_E0[i1] = v.REntryLo0[3]; vn.tlb_D0[i1] = v.REntryLo0[2]; vn.tlb_V0[i1] = v.REntryLo0[1];
      end
      /// TLBWR
      op_tlbwr:
      begin
        i1 = v.RIndex;
        vn.tlb_ptype[i2] = v.RPageType[TYPE_width-1:0];
        vn.tlb_vpn2[i2] = v.REntryHi[word_width-1:word_width-VPN2_width] && (!var_mask[i1]);
        vn.tlb_asid[i2][v.fm_dse.tid] = v.REntryHi[ASID_width-1:0];
        vn.tlb_G[i2] = v.REntryLo1[0] && v.REntryLo0[0];
        vn.tlb_pfn21[i2] = v.REntryLo1[word_width-1:9] && (!var_mask[i1]);
        vn.tlb_EX1[i2] = v.REntryLo1[8]; vn.tlb_C1[i2] = v.REntryLo1[7:5]; vn.tlb_K1[i2] = v.REntryLo1[4];
        vn.tlb_E1[i2] = v.REntryLo1[3]; vn.tlb_D1[i2] = v.REntryLo1[2]; vn.tlb_V1[i2] = v.REntryLo1[1];
        vn.tlb_pfn20[i2] = v.REntryLo0[word_width-1:9] && (!var_mask[i1]);
        vn.tlb_EX0[i2] = v.REntryLo0[8]; vn.tlb_C0[i2] = v.REntryLo0[7:5]; vn.tlb_K0[i2] = v.REntryLo0[4];
        vn.tlb_E0[i2] = v.REntryLo0[3]; vn.tlb_D0[i2] = v.REntryLo0[2]; vn.tlb_V0[i2] = v.REntryLo0[1];
      end
      /// GP2S
      op_gp2s:
        case(v.fm_spu.sr_adr)
        RContent_NO: vn.RContent = v.fm_spu.op0;
        RIndex_NO: vn.RIndex = v.fm_spu.op0;
        RRandom_NO: vn.RRandom = v.fm_spu.op0;
        REntryLo0_NO: vn.REntryLo0 = v.fm_spu.op0;
        REntryLo1_NO: vn.REntryLo1 = v.fm_spu.op0;
        REntryHi_NO: vn.REntryHi = v.fm_spu.op0;
        RPageType_NO: vn.RPageType = v.fm_spu.op0;
        default: ovm_report_warning("SPU_SRAD", "SPU WRITE SR_ADDR IS ERROR!!!");
        endcase 
        
    /// S2GP
      op_s2gp:
      begin
        if(vn.spu[1] == null)
          vn.spu[1] = tr_tlb2spu::type_id::create("to_spu", this);
        case(v.fm_spu.sr_adr)
        RContent_NO: vn.spu[1].res = v.RContent;
        RIndex_NO: vn.spu[1].res = v.RIndex;
        RRandom_NO: vn.spu[1].res = v.RRandom;
        REntryLo0_NO: vn.spu[1].res = v.REntryLo0;
        REntryLo1_NO: vn.spu[1].res = v.REntryLo1;
        REntryHi_NO: vn.spu[1].res = v.REntryHi;
        RPageType_NO: vn.spu[1].res = v.RPageType;
        default: ovm_report_warning("SPU_SRAD", "SPU READ SR_ADDR IS ERROR!!!");
        endcase 
      end
      endcase
    end
    
  endfunction
  
  function void req_proc();
    tr_tlb2dse to_dse;
    tr_tlb2spu to_spu;
    tr_tlb2ife to_ife;
    
    ovm_report_info("TLB", "req_proc procing...", OVM_HIGH); 
   
    /// send to dse
    to_dse = v.dse;
    
    /// send to spu
    to_spu = v.spu[sstage_max];     ///
   
    /// send to ife
    to_ife = v.ife;    
    
    /// req to other module
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
    if(to_ife != null) void'(ife_tr_port.nb_transport(to_ife, to_ife));
    
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_dse(input tr_dse2tlb req, output tr_dse2tlb rsp);
    ovm_report_info("DSE2TLB_TR", "Get DSE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse = req;
    return 1;
  endfunction : nb_transport_dse
  
  function bit nb_transport_spu(input tr_spu2tlb req, output tr_spu2tlb rsp);
    ovm_report_info("SPU2TLB_TR", "Get SPU Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_ife(input tr_ife2tlb req, output tr_ife2tlb rsp);
    ovm_report_info("IFE2TLB_TR", "Get IFE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    
    if(vn.ife_buf_ptr == IFE_REQ_BUF)
      ovm_report_warning("TLB_BUF_OVERFLOW", "TLB fm_ife OVERFLOW");
    else begin
      vn.fm_ife[v.ife_buf_ptr] = req;
      vn.ife_buf_ptr = v.ife_buf_ptr + 1;
    end
    
    return 1;
  endfunction : nb_transport_ife  
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_tlb_vars t;
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
    dse_tr_imp = new("dse_tr_imp", this);
    ife_tr_imp = new("ife_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ife_tr_port = new("ife_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
   
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_tlb

///-------------------------------------other functions-----------------------------------------
  
