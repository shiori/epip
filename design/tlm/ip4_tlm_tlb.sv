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
/*
parameter uchar index_ent   = 7 , /// entry bits
                num_tlb_e   = 1 << index_ent,  ///128
                vpn2_width  = 18,
                mask_width  = 15,
                type_width  = 3,  /// Page Size Type bit width
                asid_width  = 8,
                IFE_REQ_BUF = 2,
                RContent_NO = 6,
                RIndex_NO = 7,
                RRandom_NO = 8,
                REntryLo0_NO = 9,
                REntryLo1_NO = 10,
                REntryHi_NO = 11,
                RPageType_NO = 12;

parameter uchar   pagetype0 = 0,   /// 8K
                  pagetype1 = 1,   /// 64K
                  pagetype2 = 2,   /// 512k
                  pagetype3 = 3,   /// 4M
                  pagetype4 = 4,   /// 16M
                  pagetype5 = 5,   /// 64M
                  pagetype6 = 6;   /// 256M 
                  


typedef enum ushort {
  pagemask0 = 15'b000000000000000,   /// 8K
  pagemask1 = 15'b000000000000111,   /// 64K
  pagemask2 = 15'b000000000111111,   /// 512k
  pagemask3 = 15'b000000111111111,   /// 4M
  pagemask4 = 15'b000011111111111,   /// 16M
  pagemask5 = 15'b001111111111111,   /// 64M
  pagemask6 = 15'b111111111111111;   /// 256M
} page_mask_e;

parameter page_mask_e page_mask_table[] = {
  pagemask0,
  pagemask1,
  pagemask2,
  pagemask3,
  pagemask4,
  pagemask5,
  pagemask6,
};


parameter uchar num_sstage = 2,
                sstage_max = num_sstage - 1; /// spu pipeline in the tlb
*/
class ip4_tlm_tlb_vars extends ovm_component;
/*  
  tr_dse2tlb fmDSE;
  tr_spu2tlb fmSPU;
  tr_ife2tlb fmIFE[IFE_REQ_BUF];
  uchar ife_buf_ptr;
  
  tr_tlb2dse dse;
  tr_tlb2ife ife;
  tr_tlb2spu spu[sstage_max:1];
  
  bit[vpn2_width-1:0] tlb_vpn2[num_tlb_e-1:0];
  bit[type_width-1:0] tlb_ptype[num_tlb_e-1:0];
  bit[NUM_THREAD-1:0][asid_width-1:0] tlb_asid[num_tlb_e-1:0];
  bit                 tlb_G[num_tlb_e-1:0]; 
  
  bit[PFN_width-1:0]  tlb_pfn20[num_tlb_e-1:0];
  bit tlb_EX0[num_tlb_e-1:0], tlb_C0[num_tlb_e-1:0], tlb_K0[num_tlb_e-1:0];
  bit tlb_E0[num_tlb_e-1:0], tlb_D0[num_tlb_e-1:0], tlb_V0[num_tlb_e-1:0];
  bit                 tlb_G0[num_tlb_e-1:0]; 
  
  bit[PFN_width-1:0]  tlb_pfn21[num_tlb_e-1:0];
  bit tlb_EX1[num_tlb_e-1:0], tlb_C1[num_tlb_e-1:0], tlb_K1[num_tlb_e-1:0];
  bit tlb_E1[num_tlb_e-1:0], tlb_D1[num_tlb_e-1:0], tlb_V1[num_tlb_e-1:0];
  bit                 tlb_G1[num_tlb_e-1:0]; 
  
  word RIndex;
  word RRandom;
  word REntryLo0;
  word REntryLo1;
  word REntryHi;
  word RPageType;
  word RContent;  
*/    
  `ovm_component_utils_begin(ip4_tlm_tlb_vars)
/*    `ovm_field_object(fmDSE, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fmIFE, OVM_ALL_ON + OVM_REFERENCE)
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
    `ovm_field_int(ife_buf_ptr, OVM_ALL_ON)*/
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
/*    tlb_vpn2 = '{default : 0};
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
    ife_buf_ptr = 0;*/
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
    bit[mask_width-1:0] var_mask[num_tlb_e];
    bit[PFN_width-1:0] var_pfn;
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
      for(int i = 1; i < v.ife_buf_ptr; i++)
        vn.fmIFE[i-1] = v.fmIFE[i];
      if(v.ife_buf_ptr > 0) begin
        vn.fmIFE[v.ife_buf_ptr-1] = null;
        vn.ife_buf_ptr = v.ife_buf_ptr - 1;
      end
      else
       vn.fmIFE[0] = null;
    end
    else if(v.fmDSE != null)
      rsp_dse = 1;
      
    for (int i = sstage_max; i > 1; i--)
      vn.spu[i] = v.spu[i-1];
    vn.spu[1] = null;
    
      for (int i = 0; i < num_tlb_e; i++)begin
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
      if(rsp_ife && v.fmIFE[0] != null) begin
        vir_adr = v.fmIFE[0].vAdr;
        var_tid = v.fmIFE[0].tid;
      end
      else if(v.fmDSE != null) begin
        vir_adr = v.fmDSE.vAdrHi;
        var_tid = v.fmDSE.tid;
      end
      
      for (int i = 0; i < num_tlb_e; i++)begin
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
                  && (v.tlb_G[i] || (v.tlb_asid[i][var_tid] == v.REntryHi[asid_width-1:0])))begin
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
                
                if(rsp_dse && (var_d == 0) && ((v.fmDSE.op == op_sw) || (v.fmDSE.op == op_sh) || (v.fmDSE.op == op_sb)))begin
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
        for (int i = 0; i < num_tlb_e; i++)begin
          if((v.tlb_vpn2[i] && (!var_mask[i])) == (v.REntryHi[WORD_WIDTH-1:WORD_WIDTH-vpn2_width] && (!var_mask[i]))
              && ((v.tlb_G[i] == 1) || (v.tlb_asid[i][v.fmDSE.tid] == v.REntryHi[asid_width-1:0])))
              vn.RIndex = i;
        end
      
      /// TLBPR
      op_tlbr:
      begin
        i0 = v.RIndex;
        if(i0 < num_tlb_e) begin
          vn.RPageType[type_width-1:0] = v.tlb_ptype[i0];
          vn.REntryHi = {(v.tlb_vpn2[i0] && (!var_mask[i0])), 6'b0, v.tlb_asid[i0][v.fmDSE.tid]};
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
        vn.tlb_ptype[i1] = v.RPageType[type_width-1:0];
        vn.tlb_vpn2[i1] = v.REntryHi[WORD_WIDTH-1:WORD_WIDTH-vpn2_width] && (!var_mask[i1]);
        vn.tlb_asid[i1][v.fmDSE.tid] = v.REntryHi[asid_width-1:0];
        vn.tlb_G[i1] = v.REntryLo1[0] && v.REntryLo0[0];
        vn.tlb_pfn21[i1] = v.REntryLo1[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.tlb_EX1[i1] = v.REntryLo1[8]; vn.tlb_C1[i1] = v.REntryLo1[7:5]; vn.tlb_K1[i1] = v.REntryLo1[4];
        vn.tlb_E1[i1] = v.REntryLo1[3]; vn.tlb_D1[i1] = v.REntryLo1[2]; vn.tlb_V1[i1] = v.REntryLo1[1];
        vn.tlb_pfn20[i1] = v.REntryLo0[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.tlb_EX0[i1] = v.REntryLo0[8]; vn.tlb_C0[i1] = v.REntryLo0[7:5]; vn.tlb_K0[i1] = v.REntryLo0[4];
        vn.tlb_E0[i1] = v.REntryLo0[3]; vn.tlb_D0[i1] = v.REntryLo0[2]; vn.tlb_V0[i1] = v.REntryLo0[1];
      end
      /// TLBWR
      op_tlbwr:
      begin
        i1 = v.RIndex;
        vn.tlb_ptype[i2] = v.RPageType[type_width-1:0];
        vn.tlb_vpn2[i2] = v.REntryHi[WORD_WIDTH-1:WORD_WIDTH-vpn2_width] && (!var_mask[i1]);
        vn.tlb_asid[i2][v.fmDSE.tid] = v.REntryHi[asid_width-1:0];
        vn.tlb_G[i2] = v.REntryLo1[0] && v.REntryLo0[0];
        vn.tlb_pfn21[i2] = v.REntryLo1[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.tlb_EX1[i2] = v.REntryLo1[8]; vn.tlb_C1[i2] = v.REntryLo1[7:5]; vn.tlb_K1[i2] = v.REntryLo1[4];
        vn.tlb_E1[i2] = v.REntryLo1[3]; vn.tlb_D1[i2] = v.REntryLo1[2]; vn.tlb_V1[i2] = v.REntryLo1[1];
        vn.tlb_pfn20[i2] = v.REntryLo0[WORD_WIDTH-1:9] && (!var_mask[i1]);
        vn.tlb_EX0[i2] = v.REntryLo0[8]; vn.tlb_C0[i2] = v.REntryLo0[7:5]; vn.tlb_K0[i2] = v.REntryLo0[4];
        vn.tlb_E0[i2] = v.REntryLo0[3]; vn.tlb_D0[i2] = v.REntryLo0[2]; vn.tlb_V0[i2] = v.REntryLo0[1];
      end
      /// GP2S
      op_gp2s:
        case(v.fmSPU.srAdr)
        RContent_NO: vn.RContent = v.fmSPU.op0;
        RIndex_NO: vn.RIndex = v.fmSPU.op0;
        RRandom_NO: vn.RRandom = v.fmSPU.op0;
        REntryLo0_NO: vn.REntryLo0 = v.fmSPU.op0;
        REntryLo1_NO: vn.REntryLo1 = v.fmSPU.op0;
        REntryHi_NO: vn.REntryHi = v.fmSPU.op0;
        RPageType_NO: vn.RPageType = v.fmSPU.op0;
        default: ovm_report_warning("SPU_SRAD", "spu WRITE SR_ADDR IS ERROR!!!");
        endcase 
        
    /// S2GP
      op_s2gp:
      begin
        if(vn.spu[1] == null)
          vn.spu[1] = tr_tlb2spu::type_id::create("toSPU", this);
        case(v.fmSPU.srAdr)
        RContent_NO: vn.spu[1].res = v.RContent;
        RIndex_NO: vn.spu[1].res = v.RIndex;
        RRandom_NO: vn.spu[1].res = v.RRandom;
        REntryLo0_NO: vn.spu[1].res = v.REntryLo0;
        REntryLo1_NO: vn.spu[1].res = v.REntryLo1;
        REntryHi_NO: vn.spu[1].res = v.REntryHi;
        RPageType_NO: vn.spu[1].res = v.RPageType;
        default: ovm_report_warning("SPU_SRAD", "spu READ SR_ADDR IS ERROR!!!");
        endcase 
      end
      endcase
    end
*/    
  endfunction
  
  function void req_proc();
/*    tr_tlb2dse toDSE;
    tr_tlb2spu toSPU;
    tr_tlb2ife toIFE;
    
    ovm_report_info("TLB", "req_proc procing...", OVM_FULL); 
   
    /// send to dse
    toDSE = v.dse;
    
    /// send to spu
    toSPU = v.spu[sstage_max];     ///
   
    /// send to ife
    toIFE = v.ife;    
    
    /// req to other module
    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
    if(toSPU != null) void'(spu_tr_port.nb_transport(toSPU, toSPU));
    if(toIFE != null) void'(ife_tr_port.nb_transport(toIFE, toIFE));
*/    
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_dse(input tr_dse2tlb req, output tr_dse2tlb rsp);
    ovm_report_info("DSE2TLB_TR", "Get dse Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmDSE = req;
    return 1;
  endfunction : nb_transport_dse
  
  function bit nb_transport_spu(input tr_spu2tlb req, output tr_spu2tlb rsp);
    ovm_report_info("SPU2TLB_TR", "Get spu Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_ife(input tr_ife2tlb req, output tr_ife2tlb rsp);
    ovm_report_info("IFE2TLB_TR", "Get ife Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
///    rsp = req;
///    
///    if(vn.ife_buf_ptr == IFE_REQ_BUF)
///      ovm_report_warning("TLB_BUF_OVERFLOW", "TLB fmIFE OVERFLOW");
///    else begin
///      vn.fmIFE[v.ife_buf_ptr] = req;
///      vn.ife_buf_ptr = v.ife_buf_ptr + 1;
///    end
    
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
  
