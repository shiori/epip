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
          Entry_NUM  = 2<<Index_EBit,  ///128
          VPN2_width = 18,
          MASK_width = 15,
          ASID_width = 8,
          VADD_START = 14;  /// 8K 14BIT START

parameter pagemask0 = 15'b000000000000000,   /// 8K
          pagemask1 = 15'b000000000000111,   /// 64K
          pagemask2 = 15'b000000000111xxx,   /// 512k
          pagemask3 = 15'b000000111xxxxxx,   /// 4M
          pagemask4 = 15'b000011xxxxxxxxx,   /// 16M
          pagemask5 = 15'b0011xxxxxxxxxxx,   /// 64M
          pagemask6 = 15'b11xxxxxxxxxxxxx;   /// 256M 

parameter uchar PFN_width = 23;

parameter uchar RContent_NO = 6,
          RIndex_NO = 7,
          RRandom_NO = 8,
          REntryLo0_NO = 9,
          REntryLo1_NO = 10,
          REntryHi_NO = 11,
          RPageMask_NO = 12;

parameter uchar num_sstage = 2,
                sstage_max = num_sstage - 1; /// spu pipeline in the tlb

class ip4_tlm_tlb_vars extends ovm_object;
  
  tr_dse2tlb fm_dse;
  tr_spu2tlb fm_spu;
  tr_ife2tlb fm_ife;
  
  tr_tlb2dse dse;
  tr_tlb2spu spu[sstage_max:1];
  tr_tlb2ife ife;
  
  rand bit[VPN2_width-1:0] tlb_vpn2[Entry_NUM-1:0];
  rand bit[MASK_width-1:0] tlb_mask[Entry_NUM-1:0];
  rand bit[ASID_width-1:0] tlb_asid[Entry_NUM-1:0];
  rand bit                 tlb_G[Entry_NUM-1:0]; 
  
  rand bit[PFN_width-1:0]  tlb_pfn20[Entry_NUM-1:0];
  rand bit tlb_EX0[Entry_NUM-1:0], tlb_C0[Entry_NUM-1:0], tlb_K0[Entry_NUM-1:0];
  rand bit tlb_E0[Entry_NUM-1:0], tlb_D0[Entry_NUM-1:0], tlb_V0[Entry_NUM-1:0];
  
  rand bit[PFN_width-1:0]  tlb_pfn21[Entry_NUM-1:0];
  rand bit tlb_EX1[Entry_NUM-1:0], tlb_C1[Entry_NUM-1:0], tlb_K1[Entry_NUM-1:0];
  rand bit tlb_E1[Entry_NUM-1:0], tlb_D1[Entry_NUM-1:0], tlb_V1[Entry_NUM-1:0];
    
  rand word RIndex;
  rand word RRandom;
  rand word REntryLo0;
  rand word REntryLo1;
  rand word REntryHi;
  rand word RPageMask;
  rand word RContent;  
    
  `ovm_object_utils_begin(ip4_tlm_tlb_vars)
    `ovm_field_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_int(tlb_vpn2, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_mask, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_asid, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_pfn2, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_G, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_EX0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_C0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_K0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_E0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_D0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_V0, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_EX1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_C1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_K1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_E1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_D1, OVM_ALL_ON)
    `ovm_field_sarray_int(tlb_V1, OVM_ALL_ON)
    `ovm_field_int(RIndex, OVM_ALL_ON)
    `ovm_field_int(RRandom, OVM_ALL_ON)
    `ovm_field_int(REntryLo0, OVM_ALL_ON)
    `ovm_field_int(REntryLo1, OVM_ALL_ON)
    `ovm_field_int(REntryHi, OVM_ALL_ON)
    `ovm_field_int(RPageMask, OVM_ALL_ON)
    `ovm_field_int(RContent, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function new (string name = "tlb_vars");
    super.new(name);
    for (int i = 0; i < Entry_NUM; i++)begin
      tlb_vpn2[i] = 0;
      tlb_mask[i] = 0;
      tlb_asid[i] = 0;
      tlb_G       = 0; tlb_EX = 0;tlb_C = 0;tlb_K = 0;
      tlb_E = 0;tlb_D = 0;tlb_V = 0;      
    end    
    RIndex = 0;
    RRandom = 0;
    REntryLo0 = 0;
    REntryLo1 = 0;
    REntryHi = 0;
    RPageMask = 0;
    RContent = 0;
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
  
  local uchar EvenOddBit; 
  local bit[PFN_width-1:0] var_pfn;
  local bit var_ex, var_c, var_k;
  local bit var_e,  var_d, var_v, var_g;
  local bit find; 
  
  bit Miss_exp;  
  
  rand bit var_emsk[num_sp];
  rand word valva_addr[num_sp]
  rand uchar var_cnt;
  
    
  `ovm_component_utils_begin(ip4_tlm_tlb)
  `ovm_component_utils_end
      
  ovm_nonblocking_transport_imp_spu #(tr_spu2tlb, tr_spu2tlb, ip4_tlm_tlb) spu_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2tlb, tr_dse2tlb, ip4_tlm_tlb) dse_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2tlb, tr_ife2tlb, ip4_tlm_tlb) ife_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_tlb2spu, tr_tlb2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2dse, tr_tlb2dse) dse_tr_port;
  ovm_nonblocking_transport_port #(tr_tlb2ife, tr_tlb2ife) ife_tr_port;  
    
  function void comb_proc();
    ovm_report_info("TLB", "comb_proc procing...", OVM_HIGH);
     
    if(v.fm_dse != null) end_tr(v.fm_dse);
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_ife != null) end_tr(v.fm_ife);
    
    
    /// virtual address select
    find = 0;
    var_emsk = v.fm_dse.emsk;
    int k = 0;
    for (int j = 0; (j < num_sp)&&(v.fm_dse.emsk[j]==1); j++) begin
        if(k == 0)begin   
          valva_addr[k] = v.fm_dse.v_addr[j];   /// valid virtual address to send into tlb for translation
          k++;
        end
        else if(v.fm_dse.v_addr[j][31:VADD_START] == valva_addr[0][31:VADD_START])begin
                valva_addr[k] = v.fm_dse.v_addr[j];
                k++;
             end
             else var_emsk[j] = 0;
   end  
    var_cnt = k-1;
    
    ///tlb basic function
      for (int i = 0; i < Entry_NUM; i++)begin
        if(((v.tlb_vpn2[i] && (!v.tlb_mask[i])) == (valva_addr[0][31:VADD_START] && (!v.tlb_mask[i]))) 
          && (v.tlb_G[i] || (v.tlb_asid[i] == v.REntryHi[ASID_width-1:0])))begin
            case(v.tlb_mask[i])
              pagemask0: EvenOddBit = 13;   
              pagemask1: EvenOddBit = 16;
              pagemask2: EvenOddBit = 19;
              pagemask3: EvenOddBit = 22;
              pagemask4: EvenOddBit = 24;
              pagemask5: EvenOddBit = 26;
              pagemask6: EvenOddBit = 28;
              default:  ovm_report_warning("TLBPSize_ILLEGAL", "No this type page size!!!");              
            endcase
            if(valva_addr[0][EvenOddBit] == 0)begin
              var_pfn = v.tlb_pfn20[i];
              var_v   = v.tlb_V0[i];
              var_c   = v.tlb_C0[i];
              var_d   = v.tlb_D0[i];
            end
            else begin
              var_pfn = v.tlb_pfn21[i];
              var_v   = v.tlb_V1[i];
              var_c   = v.tlb_C1[i];
              var_d   = v.tlb_D1[i];
            end
            if(var_v == 0)begin
              ovm_report_warning("TLB_Invalid", "TLB Invalid exception!!!"); 
              vn.RContent[4:0] = 0;
              vn.RContent[22:5] = v.tlb_vpn2[i];
        ///    vn.RContent[31:23] = ;
            end
            if((var_d == 0) && ((v.fm_dse.op == op_sw) || (v.fm_dse.op == op_sh) || (v.fm_dse.op == op_sb)))begin
              ovm_report_warning("TLB_Modified", "TLB Modified exception!!!"); 
              vn.RContent[4:0] = 0;
              vn.RContent[22:5] = v.tlb_vpn2[i];
            end
            for (int j = 0; j < var_cnt; j++)begin
              vn.dse.phy_addr[j][EvenOddBit-1:0] = valva_addr[j][EvenOddBit-1:0];   /// 13
              vn.dse.phy_addr[PFN_width+EvenOddBit-1:EvenOddBit] = var_pfn;      /// 23
            end
            vn.dse.vaddr_cnt = var_cnt;    /// the number of the physical addresses translated 
            find = 1;
            break;
      end  
    end
       
     local word i0, i1, i2;
    /// tlb support instruction  spu -> tlb
    /// dse:    | rrf | rrc0 |  ag  |  tag |  sel |  dc  | dwbp |  dwb |
    /// spu:    | rrf | rrc0 | exs0 | exs1 | exs2 | exs3 | swbp |  swb |
    ///                             |      |
    ///                          request  respond
   
    /// delay one pipelinestage
    for (int i = sstage_max; i > 1; i--)
      vn.spu[i] = v.spu[i-1];   
    
    if(v.fm_spu.req)begin
      case(v.fm_spu.op)
      /// TLBP
      op_tlbp:
        for (int i = 0; i < Entry_NUM; i++)begin
          if((v.tlb_vpn2[i] && (!v.tlb_mask[i])) == (v.REntryHi[word_width-1:word_width-VPN2_width] && (!v.tlb_mask[i]))
              && ((v.tlb_G[i] == 1) || (v.tlb_asid[i] == v.REntryHi[ASID_width-1:0])))
              vn.RIndex = i;
        end
      
      /// TLBPR
      op_tlbr:
        i0 = v.RIndex;
        if(i0 < Entry_NUM)begin
          vn.RPageMask[MASK_width-1:0] = v.tlb_mask[i0];
          vn.REntryHi = {(v.tlb_vpn2[i0] && (!v.tlb_mask[i0]), 6'b0, v.tlb_asid[i0]};
          vn.REntryLo1 = {(v.tlb_pfn21[i0] && (!v.tlb_mask[i0]), v.tlb_EX1[i0], 
                           v.tlb_C1[i0], v.tlb_K1[i0], v.tlb_E1[i0], v.tlb_D1[i0],
                           v.tlb_V1[i0], v.tlb_G1[i0]};
          vn.REntryLo0 = {(v.tlb_pfn20[i0] && (!v.tlb_mask[i0]), v.tlb_EX0[i0], 
                           v.tlb_C0[i0], v.tlb_K0[i0], v.tlb_E0[i0], v.tlb_D0[i0],
                           v.tlb_V0[i0], v.tlb_G0[i0]}; 
        end
        
     /// TLBWI
     op_tlbwi:
        i1 = v.RIndex;
        vn.tlb_mask[i1] = v.RPageMask[MASK_width-1:0];
        vn.tlb_vpn2[i1] = v.REntryHi[word_width-1:word_width-VPN2_width] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_asid[i1] = v.REntryHi[ASID_width-1:0];
        vn.tlb_G[i1] = v.REntryLo1[0] && v.REntryLo[0];
        vn.tlb_pfn1[i1] = v.REntryLo1[word_width-1:9] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_EX1 = v.REntryLo1[8]; vn.tlb_C1 = v.REntryLo1[7:5]; vn.tlb_K1 = v.REntryLo1[4];
        vn.tlb_E1 = v.REntryLo1[3]; vn.tlb_D1 = v.REntryLo1[2]; vn.tlb_V1 = v.REntryLo1[1];
        vn.tlb_pfn0[i1] = v.REntryLo0[word_width-1:9] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_EX0 = v.REntryLo0[8]; vn.tlb_C0 = v.REntryLo0[7:5]; vn.tlb_K0 = v.REntryLo0[4];
        vn.tlb_E0 = v.REntryLo0[3]; vn.tlb_D0 = v.REntryLo0[2]; vn.tlb_V0 = v.REntryLo0[1];
      
     /// TLBWR
     op_tlbwr:
        i1 = v.RIndex;
        vn.tlb_mask[i2] = v.RPageMask[MASK_width-1:0];
        vn.tlb_vpn2[i2] = v.REntryHi[word_width-1:word_width-VPN2_width] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_asid[i2] = v.REntryHi[ASID_width-1:0];
        vn.tlb_G[i2] = v.REntryLo1[0] && v.REntryLo[0];
        vn.tlb_pfn1[i2] = v.REntryLo1[word_width-1:9] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_EX1 = v.REntryLo1[8]; vn.tlb_C1 = v.REntryLo1[7:5]; vn.tlb_K1 = v.REntryLo1[4];
        vn.tlb_E1 = v.REntryLo1[3]; vn.tlb_D1 = v.REntryLo1[2]; vn.tlb_V1 = v.REntryLo1[1];
        vn.tlb_pfn0[i2] = v.REntryLo0[word_width-1:9] && (!v.RPageMask[MASK_width-1:0]);
        vn.tlb_EX0 = v.REntryLo0[8]; vn.tlb_C0 = v.REntryLo0[7:5]; vn.tlb_K0 = v.REntryLo0[4];
        vn.tlb_E0 = v.REntryLo0[3]; vn.tlb_D0 = v.REntryLo0[2]; vn.tlb_V0 = v.REntryLo0[1];
    
     /// GP2S
     op_gp2s:
        case(v.fm_spu.sr_addr)
          RContent_NO: vn.RContent = v.fm_spu.op0;
          RIndex_NO: vn.RIndex = v.fm_spu.op0;
          RRandom_NO: vn.RRandom = v.fm_spu.op0;
          REntryLo0_NO: vn.REntryLo0 = v.fm_spu.op0;
          REntryLo1_NO: vn.REntryLo1 = v.fm_spu.op0;
          REntryHi_NO: vn.REntryHi = v.fm_spu.op0;
          RPageMask_NO: vn.RPageMask = v.fm_spu.op0;
          default: ovm_report_warning("SPU_SRAD", "SPU WRITE SR_ADDR IS ERROR!!!");
        endcase 
        
    /// S2GP
    op_s2gp:
        case(v.fm_spu.sr_addr)
          RContent_NO: vn.spu.res = v.RContent;
          RIndex_NO: vn.spu.res = v.RIndex;
          RRandom_NO: vn.spu.res = v.RRandom;
          REntryLo0_NO: vn.spu.res = v.REntryLo0;
          REntryLo1_NO: vn.spu.res = v.REntryLo1;
          REntryHi_NO: vn.spu.res = v.REntryHi;
          RPageMask_NO: vn.spu.res = v.RPageMask;
          default: ovm_report_warning("SPU_SRAD", "SPU READ SR_ADDR IS ERROR!!!");
        endcase 
     endcase
    end
    
  endfunction
  
  function void req_proc();
    ovm_report_info("TLB", "req_proc procing...", OVM_HIGH); 
    
    tr_tlb2dse to_dse;
    tr_tlb2spu to_spu;
    
    /// send to dse
    if(find == 1)
      to_dse = v.dse;
    else begin
      Miss_exp = 1;
      ovm_report_warning("TLB_Missed", "TLB Missed exception!!!"); 
    end
    
    /// send to spu
    
    to_spu = v.spu;     ///
   
    /// req to other module
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
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
    ise_tr_imp = new("ise_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    
    dse_tr_port = new("dse_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
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
  
