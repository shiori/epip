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
/*
parameter uchar VADD_START = 14,  /// 8K 14BIT START for tlb and dse
                PFN_width = 23,
                PHY_width = VADD_START + PFN_width;
parameter uchar rf_bank0 = 0,      /// NUM_SP register file bank in code  3bit
                rf_bank1 = 1,      ///  unit : word 32bit
                rf_bank2 = 2,      /// 
                rf_bank3 = 3,
                rf_bank4 = 4,
                rf_bank5 = 5,
                rf_bank6 = 6,
                rf_bank7 = 7;
parameter bit [PHY_width-1:0] SM_BASE   = 'h00_0000_0000,
                              TFIF_BASE = 'h00_0020_0000,
                              CTLR_BASE = 'h00_0024_0000,
                              EJTG_BASE = 'h00_0024_1000,
                              EBUS_BASE = 'h00_0024_2000;
               
parameter uint SM_SIZE   = 2^16,/// shared memory size 64Kbyte
               MSG_SIZE  = 256, /// 256BYTE
               CTLR_SIZE = 128, /// each control register of pb 128byte
               EJTG_SIZE = 128; /// each ejtag of pb 128byte
parameter QUEUE_SIZE = 256*2;  /// 32*8*2
*/
class ip4_tlm_dse_vars extends ovm_component;
/*  
  tr_ise2dse fmISE[STAGE_RRF_VWB0:STAGE_RRF_RRC0];
  tr_spu2dse fmSPU;
  tr_rfm2dse fmRFM;
  tr_spa2dse fmSPA;
  tr_tlb2dse fmTLB;
  tr_eif2dse fm_eif;    /// communication interfaces
  
  tr_dse2ise ise;
  tr_dse2spu spu;
  tr_dse2rfm rfm;
  tr_dse2spa spa;
  tr_dse2tlb tlb;
  tr_dse2eif eif;
*/    
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
/*     `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fm_eif, OVM_ALL_ON + OVM_REFERENCE)  
     
     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(eif, OVM_ALL_ON + OVM_REFERENCE)*/
  `ovm_component_utils_end
  
  function new (string name, ovm_component parent);
    super.new(name, parent);
  endfunction : new
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
  ovm_nonblocking_transport_imp_eif #(tr_eif2dse, tr_eif2dse, ip4_tlm_dse) eif_tr_imp;
    
  ovm_nonblocking_transport_port #(tr_dse2ise, tr_dse2ise) ise_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2rfm, tr_dse2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spu, tr_dse2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2spa, tr_dse2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2tlb, tr_dse2tlb) tlb_tr_port;
  ovm_nonblocking_transport_port #(tr_dse2eif, tr_dse2eif) eif_tr_port;
        
  function void comb_proc();
/*    int k = 0;
    word var_vadr[NUM_SP];
    word valva_adr;
    bit var_emsk[NUM_SP];
    bit [PHY_width-1:0] phy_adr[NUM_SP];
    bit [2:0] bank_check;
    bit [PHY_width-1:0] smadr_start;   /// pbId owns shared memory start address  
    bit [PHY_width-1:0] smadr_end;     /// pbId owns shared memory end address
    
    bit [PHY_width-1:0] que_ld_adr[QUEUE_SIZE];
    uchar que_ld_tid[QUEUE_SIZE];
    ushort qld_adr_ptr = 0;
    
    bit [PHY_width-1:0] que_st_adr[QUEUE_SIZE];
    ushort qst_adr_ptr = 0;
    word que_stdat[QUEUE_SIZE];
    uchar que_st_tid[QUEUE_SIZE];
    
    ovm_report_info("dse", "comb_proc procing...", OVM_FULL); 
    
    if(v.fmISE[STAGE_RRF_RRC0] != null) end_tr(v.fmISE[STAGE_RRF_RRC0]);
    if(v.fmRFM != null) end_tr(v.fmRFM); 
    if(v.fmSPU != null) end_tr(v.fmSPU);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmTLB != null) end_tr(v.fmTLB);
    if(v.fm_eif != null) end_tr(v.fm_eif);
    if(v.fm_eif  != null) end_tr(v.fm_eif);
    
    vn.fmISE[STAGE_RRF_RRC0] = null;
    vn.fmRFM = null;
    vn.fmSPU = null;
    vn.fmSPA = null;
    vn.fmTLB = null;
    vn.fm_eif = null;
    vn.fm_eif  = null;
    
    for (int i = STAGE_RRF_VWB0; i > STAGE_RRF_RRC0; i--) 
      vn.fmISE[i] = v.fmISE[i-1];
      
    for (int i = STAGE_EXE_VWBP; i > 0; i--)
      vn.eif[i] = v.eif[i-1];
    vn.eif[STAGE_EXE_VWBP] = null;
    
    /// calculating the virtual address  ag stage
    if(v.fmSPU != null)
      var_emsk = v.fmSPU.emsk;
    
    if(v.fmRFM != null && v.fmISE[STAGE_RRF_AG] != null && v.fmSPU != null) begin
      if(v.fmISE[STAGE_RRF_AG].en) begin
        /// virtual address gen and selected send to tlb
        for (int i = 0; i < NUM_SP; i++)
          var_vadr[i] = v.fmRFM.base[i] + v.fmRFM.op2[i];
        
        for (int i = 0; (i < NUM_SP)&&(v.fmSPU.emsk[i]==1); i++) begin
          valva_adr = var_vadr[i];
          break;
        end
        
        for (int i = 0; (i < NUM_SP)&&(v.fmSPU.emsk[i]==1); i++) begin
          if((var_vadr[i][31:VADD_START] != valva_adr[31:VADD_START]))
            var_emsk[i] = 0;           /// the first step emask modification
        end
    
        if(v.fmISE[STAGE_RRF_AG] != null) begin
          vn.tlb = tr_dse2tlb::type_id::create("to_tlb", this);
          vn.tlb.vAdrHi[31:VADD_START-1]= valva_adr[31:VADD_START-1];  /// only the high phase sent to tlb for translation + evenoddbit
        end
    
        /// check the physical address in sel stage
        if(v.fmISE[STAGE_RRF_SEL] != null && v.fmTLB != null) begin
          vn.ise = tr_dse2ise::type_id::create("toISE", this);
          vn.eif[0] = tr_dse2eif::type_id::create("to_eif", this);
          /// use the pbId to compute the pb self shared memory address mapped to the address space 
          smadr_start = v.fmISE[STAGE_RRF_SEL].pbId * SM_SIZE;
          smadr_end = smadr_start + SM_SIZE - 1;
          if(v.fmTLB.hit) begin
            /// gen the complete phy_adr
            for(int i = 0; (i < NUM_SP)&&(var_emsk[i]==1); i++) begin
              for(int j = 0; j < v.fmTLB.eobit; j++)
                phy_adr[i][j] = valva_adr[j];
              for(int m = v.fmTLB.eobit; m < PHY_width; m++)
                phy_adr[i][m] = v.fmTLB.phy_adr[m];
            end
            
            for(int i = 0; (i < NUM_SP)&&(var_emsk[i]==1); i++) begin
              bank_check = phy_adr[i][4:2];
              k = i;
            end
                
            /// check the physical address whether match the op_code or not
            for(int i = 0; (i < NUM_SP)&&(var_emsk[i]==1); i++) begin
              if((((v.fmISE[STAGE_RRF_SEL].op == op_lw) || (v.fmISE[STAGE_RRF_SEL].op == op_sw)) && (phy_adr[i][1:0] != 2'b00)) 
                  ||(((v.fmISE[STAGE_RRF_SEL].op == op_lh) || (v.fmISE[STAGE_RRF_SEL].op == op_sh)) && (phy_adr[i][0] != 1'b0))) begin
                vn.ise.exp = 1;
                ovm_report_warning("DSE_ILLEGAL0", "Phy ADR does not match with the op_code type!!!");
              end
            
              /// the shared memory or other PB shared memory will be access, so must judge which memory
              /// if the address is pb_self shared memory
              if(smadr_start <= phy_adr[i] && phy_adr[i] <= smadr_end) begin
                if((v.fmISE[STAGE_RRF_SEL].op == op_lw) || (v.fmISE[STAGE_RRF_SEL].op == op_lh) || (v.fmISE[STAGE_RRF_SEL].op == op_lb)) begin  
                  /// the sm access will be execution, so must check if the sm bank conflict
                  vn.eif[0].ld_adr[i] = phy_adr[i]- SM_BASE; /// real memory address
                  if((phy_adr[i][4:2] == bank_check)&&(k!=i))
                    var_emsk[i] = 0;             /// the second step emask modification
                end
                if((v.fmISE[STAGE_RRF_SEL].op == op_sw) || (v.fmISE[STAGE_RRF_SEL].op == op_sh) || (v.fmISE[STAGE_RRF_SEL].op == op_sb)) begin
                  if((phy_adr[i][4:2] == bank_check)&&(k!=i))
                    var_emsk[i] = 0;             /// the second step emask modification
                  if(v.fmRFM != null) begin
                    vn.eif[0].st_adr[i] = phy_adr[i]- SM_BASE; /// real memory address
                    vn.eif[0].st_dat[i] = v.fmRFM.op1[i];
                  end
                end
              end
              else begin          /// access other pb share memory
                if((v.fmISE[STAGE_RRF_SEL].op == op_lw) || (v.fmISE[STAGE_RRF_SEL].op == op_lh) || (v.fmISE[STAGE_RRF_SEL].op == op_lb)) begin  
                  que_ld_adr[qld_adr_ptr] = phy_adr[i];
                  que_ld_tid[qld_adr_ptr] = v.fmISE[STAGE_RRF_SEL].tid;
                  qld_adr_ptr = qld_adr_ptr + 1;
                end
                
                if((v.fmISE[STAGE_RRF_SEL].op == op_sw) || (v.fmISE[STAGE_RRF_SEL].op == op_sh) || (v.fmISE[STAGE_RRF_SEL].op == op_sb)) begin
                  
                end
              end
            end  /// end of for  
         
          end   /// end of hit
          else vn.ise.cancel = 1;  /// no find the match entry in tlb,  miss 
        end
      end
    end
    /// send signals to rfm
    if(v.fmISE[STAGE_RRF_SWB] != null) begin
      vn.rfm = tr_dse2rfm::type_id::create("toRFM", this);
      if(v.fmISE[STAGE_RRF_SWB].en) begin
        vn.rfm.wrGrp = v.fmISE[STAGE_RRF_SWB].wrGrp;
        vn.rfm.wrAdr = v.fmISE[STAGE_RRF_SWB].wrAdr;
        vn.rfm.wrBk  = v.fmISE[STAGE_RRF_SWB].wrBk;
        vn.rfm.updateAdrWrGrp = v.fmISE[STAGE_RRF_SWB].updateAdrWrGrp;
        vn.rfm.updateAdrWrAdr = v.fmISE[STAGE_RRF_SWB].updateAdrWrAdr;
        vn.rfm.updateAdrWrBk = v.fmISE[STAGE_RRF_SWB].updateAdrWrBk;
      end
    end
    */
  endfunction
  
  function void req_proc();
/*    tr_dse2rfm res;
    
    ovm_report_info("dse", "req_proc procing...", OVM_FULL); 
    
    ///send write back control signal to rfm
    if(v.fmISE[STAGE_RRF_SWB] != null) begin
      res = tr_dse2rfm::type_id::create("toRFM", this);
      if(v.fmISE[STAGE_RRF_SWB].en) begin
        res.wrGrp = v.fmISE[STAGE_RRF_SWB].wrGrp;
        res.wrAdr = v.fmISE[STAGE_RRF_SWB].wrAdr;
        res.wrBk  = v.fmISE[STAGE_RRF_SWB].wrBk;
      end
    end
    
///    if(v.fmISE != null) begin
///      if(v.fmISE[STAGE_RRF_AG].en) begin
///        if(v.fmISE[STAGE_RRF_AG].op == op_) begin
///        end
///      end
///    end
    
    
///    if(toDSE != null) void'(dse_tr_port.nb_transport(toDSE, toDSE));
*/
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2dse req, output tr_ise2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmISE[STAGE_RRF_RRC0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2dse req, output tr_rfm2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmRFM = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2dse req, output tr_spu2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmSPU = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2dse req, output tr_spa2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_tlb(input tr_tlb2dse req, output tr_tlb2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get TLB Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fmTLB = req;
    return 1;
  endfunction : nb_transport_tlb

  function bit nb_transport_eif(input tr_eif2dse req, output tr_eif2dse rsp);
    ovm_report_info("DSE_TR", $psprintf("Get EIF Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
///    vn.fm_eif = req;
    return 1;
  endfunction : nb_transport_eif
          
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
    ise_tr_imp = new("ise_tr_imp", this);
    rfm_tr_imp = new("rfm_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    tlb_tr_imp = new("tlb_tr_imp", this);
    eif_tr_imp = new("eif_tr_imp", this);
    
    rfm_tr_port = new("rfm_tr_port", this);
    ise_tr_port = new("ise_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    tlb_tr_port = new("tlb_tr_port", this);
    eif_tr_port = new("eif_tr_port", this);
    
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vifCfg", tmp));
    failed_convert_interface: assert($cast(vifCfg, tmp));
    sysif = vifCfg.get_vif();  
    stamp = 0ns;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
