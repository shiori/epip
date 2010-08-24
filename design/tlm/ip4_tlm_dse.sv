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

parameter ulong IP4_BASE    = 'h00_0000_0000;

parameter uint VADR_MAPPED = 'h0000_0000,
               VADR_NMAPNC = 'hF000_0000,
               VADR_EJTAGS = 'hF7F0_0000,
               VADR_NMAPCH = 'hF800_0000,
               SMEM_OFFSET = 'h00_0000,
               TFIF_OFFSET = 'h20_0000,
               CTLR_OFFSET = 'h24_0000,
               EJTG_OFFSET = 'h24_1000,
               EBUS_OFFSET = 'h24_2000;
               
parameter uint SGRP_SIZE = NUM_SMEM_BK * NUM_SMEM_GRP_W * 4,
               SMEM_SIZE = SGRP_SIZE * NUM_SMEM_GRP,
               MSGE_SIZE = 256, /// 256BYTE
               CTLR_SIZE = 128, /// each control register of pb 128byte
               EJTG_SIZE = 128; /// each ejtag of pb 128byte

class ip4_tlm_dse_vars extends ovm_component;
  tr_ise2dse fmISE[STAGE_RRF_VWB0 : STAGE_RRF_RRC0];
  tr_spu2dse fmSPU[STAGE_RRF_SEL : STAGE_RRF_AG];
  tr_rfm2dse fmRFM[STAGE_RRF_SEL : STAGE_RRF_AG];
  tr_spa2dse fmSPA;
  tr_tlb2dse fmTLB;
  tr_eif2dse fmEIF;    /// external interfaces
  
  tr_dse2ise ise;
  tr_dse2spu spu;
  tr_dse2rfm rfm[STAGE_RRF_VWBP : STAGE_RRF_DEM0];
  tr_dse2spa spa;
  tr_dse2tlb tlb;
  tr_dse2eif eif;
  
  `ovm_component_utils_begin(ip4_tlm_dse_vars)
     `ovm_field_sarray_object(fmISE, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmSPU, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(fmRFM, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(fmSPA, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmTLB, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(fmEIF, OVM_ALL_ON + OVM_REFERENCE)  
     `ovm_field_object(ise, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spu, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
     `ovm_field_object(spa, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(tlb, OVM_ALL_ON + OVM_REFERENCE) 
     `ovm_field_object(eif, OVM_ALL_ON + OVM_REFERENCE)
///     `ovm_field_int(tlbValidReq, OVM_ALL_ON)
///     `ovm_field_int(seltlbExp, OVM_ALL_ON)
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
  local word sharedMem[NUM_SMEM_GRP][NUM_SMEM_GRP_W][NUM_SMEM_BK];
  local uchar cacheGrp;
  local uint cacheTag[NUM_SMEM_GRP_W / NUM_DCHE_CL][NUM_DCHE_ASS];
  local bit cacheTagV[NUM_SMEM_GRP_W / NUM_DCHE_CL][NUM_DCHE_ASS];
  local bit cacheDirty[NUM_SMEM_GRP_W / NUM_DCHE_CL][NUM_DCHE_ASS];
  local word tlbReqVAdr[STAGE_RRF_SEL : STAGE_RRF_TAG];
  local bit seltlbExp;
  local bit selSMemBk[2][NUM_SMEM_BK], selCacheBk[2][NUM_SMEM_BK];
  local uint selSMemAdr[2][NUM_SMEM_BK];
  local bit selAlignExp, selSMemBoundExp;
  local bit[WORD_WIDTH / 8 - 1 : 0][7:0] exStoreBuf[NUM_BURST_LEN];
  local padr_t exAdr;
  local bit exRdy, exByte[NUM_BURST_LEN][WORD_WIDTH / 8];
  
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
    padr_t selPAdr[NUM_SP];
    bit selEMsk[NUM_SP];
    bit selNoCache[NUM_SP];
    
    ovm_report_info("dse", "comb_proc procing...", OVM_FULL); 
   
    if(v.fmISE[STAGE_RRF_RRC0] != null) end_tr(v.fmISE[STAGE_RRF_RRC0]);
    if(v.fmRFM[STAGE_RRF_AG] != null) end_tr(v.fmRFM[STAGE_RRF_AG]); 
    if(v.fmSPU[STAGE_RRF_AG] != null) end_tr(v.fmSPU[STAGE_RRF_AG]);
    if(v.fmSPA != null) end_tr(v.fmSPA);
    if(v.fmTLB != null) end_tr(v.fmTLB);
    if(v.fmEIF != null) end_tr(v.fmEIF);
    
    vn.fmISE[STAGE_RRF_RRC0] = null;
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    vn.fmSPA = null;
    vn.fmTLB = null;
    vn.fmEIF = null;
    vn.fmEIF = null;
    
    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_TAG; i--) 
      tlbReqVAdr[i] = tlbReqVAdr[i - 1];
      
    for (int i = STAGE_RRF_VWB0; i > STAGE_RRF_RRC0; i--) 
      vn.fmISE[i] = v.fmISE[i - 1];
    vn.fmISE[STAGE_RRF_RRC0] = null;

    for (int i = STAGE_RRF_SEL; i > STAGE_RRF_AG; i--) begin
      vn.fmSPU[i] = v.fmSPU[i - 1];
      vn.fmRFM[i] = v.fmRFM[i - 1];
    end
    vn.fmRFM[STAGE_RRF_AG] = null;
    vn.fmSPU[STAGE_RRF_AG] = null;
    
    for (int i = STAGE_RRF_VWBP; i > STAGE_RRF_DEM0; i--) 
      vn.rfm[i] = v.rfm[i - 1];
    vn.rfm[STAGE_RRF_DEM0] = null;

    selEMsk = '{default : 0};
    
    if(v.fmSPU[STAGE_RRF_SEL] != null
      && v.fmRFM[STAGE_RRF_SEL] != null && v.fmISE[STAGE_RRF_SEL]) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_SEL];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_SEL];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_SEL];
      tr_tlb2dse tlb = v.fmTLB;
      uchar pbId = ise.pbId;
      uint vAdrHi;
      uint dcIdx, dcRdy = 0;
      bit ed = 0;
      uchar clBits = n2w(NUM_DCHE_CL + cacheGrp - 1);

      if(ise.subVec & 2'b01 == 0) begin
        selSMemBk = '{default : 0};
        selCacheBk = '{default : 0};
        selSMemAdr = '{default : 0};
      end  

      ///vadr to padr translation stage
      if(tlb != null) begin
        vAdrHi = tlbReqVAdr[STAGE_RRF_SEL] >> tlb.eobit;
        ed = tlb.e;
      end
      foreach(rfm.base[i]) begin
        if(rfm.base[i] >= VADR_NMAPNC) begin
          if(rfm.base[i] >= VADR_NMAPCH)
            selPAdr[i] = rfm.base[i];
          else begin
            selPAdr[i] = IP4_BASE + SMEM_OFFSET + pbId * SMEM_SIZE + selPAdr[i] - VADR_EJTAGS;
            if(rfm.base[i] < VADR_EJTAGS)
              selNoCache[i] = 1;
          end
        end
        else if(tlb != null && tlb.hit && !tlb.exp
            && (rfm.base[i] >> (VADR_START + tlb.eobit)) == vAdrHi) begin
          selEMsk[i] = spu.emsk[i];
          selPAdr[i] = rfm.base[i];
          if(!selEMsk[i]) continue;
          selNoCache[i] = tlb.c == 0;
          for(int j = (VADR_START + tlb.eobit); j < PADR_WIDTH; j++)
            selPAdr[i][j] = tlb.pfn[j - VADR_START];
        end
      end
      seltlbExp |= tlb.exp;
      
      ///access data
      foreach(selPAdr[i]) begin
        padr_t smStart = IP4_BASE + SMEM_OFFSET + pbId * SMEM_SIZE,
               smEnd   = IP4_BASE + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_BK - cacheGrp) * SGRP_SIZE,
               smEnd2  = IP4_BASE + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;
        uchar bk, grp, adr;
        word res;
        bit oc = 0, ex = 0;
        
        if(!selEMsk[i]) continue;
        
        ///align exp
        if((ise.op inside {op_lw, op_sw} && selPAdr[i][1:0] != 2'b0)
          || (ise.op inside {op_lh, op_sh, op_lhu} && selPAdr[0] != 1'b0))
          selAlignExp = 1;
        
        ///external mem
        if(selPAdr[i] < smStart && selPAdr[i] >= smEnd2) begin
          bit hit = 0;
          uint idx;
          ///chk cache for match
          if(!selNoCache[i] && cacheGrp > 0) begin
            uint tag = selPAdr[i] >> (BITS_WORD + BITS_SMEM_BK + clBits + BITS_DCH_IDX);
            idx = selPAdr[i] >> (BITS_WORD + BITS_SMEM_BK + clBits) & ~(-1 << BITS_DCH_IDX);
            for(int asid = 0; asid < NUM_DCHE_ASS; asid++)
              if(!dcRdy && cacheTagV[idx][asid] && cacheTag[idx][asid] == tag) begin
                ///cache hit
                hit = 1;
                dcRdy = 1;
                dcIdx = idx;
              end
              else if(dcRdy && dcIdx == idx && cacheTagV[idx][asid] && cacheTag[idx][asid] == tag)
                hit = 1;
          end
          
          ///cache hit
          if(hit) begin
            bk = selPAdr[i] & ~(-1 << BITS_SMEM_BK);
            grp = (selPAdr[i] >> (BITS_WORD + BITS_SMEM_BK)) & ~(-1 << clBits);
            foreach(selSMemBk[j])
              if(!selSMemBk[j][bk] && !selCacheBk[j][bk]) begin
                selCacheBk[j][bk] = 1;
                oc = 1;
                break;
              end
            adr = idx << clBits;
          end
          ///external access
          else begin
            bk = selPAdr[i] & ~(-1 << BITS_BURST);
            adr = selPAdr[i] & ~(-1 << BITS_WORD);
            if(!exRdy) begin
              exRdy = 1;
              ex = 1;
              exAdr = selPAdr[i] >> (BITS_BURST + BITS_WORD);
            end
            else if(exAdr == (selPAdr[i] >> BITS_BURST))
              ex = 1;
          end
        end
        ///shared mem
        else begin
          uint adr1;
          if(selPAdr[i] >= smEnd) begin
            selSMemBoundExp = 1;
            continue;
          end
          
          ///load req  
          bk = selPAdr[i] & ~(-1 << BITS_SMEM_BK);
          adr1 = (selPAdr[i] >> BITS_SMEM_BK) & ~(-1 << BITS_SMEM_ADR + BITS_SMEM_GRP);
          adr = adr1 & ~(-1 << BITS_SMEM_ADR);
          grp = adr1 >> BITS_SMEM_ADR;
          foreach(selSMemBk[j])
            if((!selSMemBk[j][bk] && !selCacheBk[j][bk]) 
                || (selSMemAdr[j][bk] == adr1 && selSMemBk[j][bk])) begin
              selSMemBk[j][bk] = 1;
              selSMemAdr[j][bk] = adr1;
              oc = 1;
              break;
            end
        end
        
        selEMsk[i] = oc || ex;
        if(vn.rfm[STAGE_RRF_DEM0] == null) vn.rfm[STAGE_RRF_DEM0] = tr_dse2rfm::type_id::create("toRFM", this);
        
        if(oc) begin
          res = sharedMem[grp][adr][bk];
          case(ise.op)
          op_lbu  : res = {'0, res[7:0]};
          op_lb   : res = {{WORD_WIDTH{res[7]}}, res[7 : 0]};
          op_lhu  : res = {'0, res[WORD_WIDTH / 2 - 1 : 0]};
          op_lh   : res = {{WORD_WIDTH{res[WORD_WIDTH / 2 - 1]}}, res[WORD_WIDTH / 2 - 1 : 0]};
          endcase
          vn.rfm[STAGE_RRF_DEM0].res[i] = res;
        end
        
        if(ex) begin
          if(ise.op inside {op_lbu, op_lb, op_sb})
            exByte[bk][ed ? adr : (WORD_WIDTH / 8 - adr)] = 1;
          else if(ise.op inside {op_lhu, op_lh, op_sh})
            for(int j = adr; j < adr + WORD_WIDTH / 16; j++)
              exByte[bk][ed ? j : (adr + WORD_WIDTH / 16 - j)] = 1;
          else if(ise.op inside {op_lw, op_sw})
            for(int j = 0; j < WORD_WIDTH / 8; j++)
              exByte[bk][ed ? j : (WORD_WIDTH / 8 - j)] = 1;
              
          case(ise.op)
          op_sw   : 
            for(int j = 0; j < WORD_WIDTH / 8; j++)
              exStoreBuf[bk][ed ? j : (WORD_WIDTH / 8 - j)] = rfm.st[i] >> j;
          op_sh   :
            for(int j = adr; j < adr + WORD_WIDTH / 16; j++)
              exStoreBuf[bk][ed ? j : (adr + WORD_WIDTH / 16 - j)] = rfm.st[i] >> j;
          op_sb   :
            exStoreBuf[bk][ed ? adr : (WORD_WIDTH / 8 - adr)] = rfm.st[i][7:0];
          endcase
        end
        
        vn.rfm[STAGE_RRF_DEM0].wrEn[i] = selEMsk[i];
        vn.rfm[STAGE_RRF_DEM0].updateAdrRes[i] = rfm.base[i];
      end
      
      if(ise.subVec == ise.vecMode) begin
        seltlbExp = 0;
        selSMemBk = '{default : 0};
        selCacheBk = '{default : 0};
        selSMemAdr = '{default : 0};
        selAlignExp = 0;
        selSMemBoundExp = 0;
        exRdy = 0;
        exByte = '{default : 0};
      end
    end
    
    
  endfunction
  
  function void req_proc();
    tr_dse2rfm toRFM;
    tr_dse2tlb toTLB;
    
    ovm_report_info("dse", "req_proc procing...", OVM_FULL); 
    
    if(v.fmISE[STAGE_RRF_AG] != null && v.fmRFM[STAGE_RRF_AG] != null
       && v.fmSPU[STAGE_RRF_AG] != null) begin
      tr_ise2dse ise = v.fmISE[STAGE_RRF_AG];
      tr_rfm2dse rfm = v.fmRFM[STAGE_RRF_AG];
      tr_spu2dse spu = v.fmSPU[STAGE_RRF_AG];
      int vadr = 0;
      bit found = 0;
      
      foreach(spu.emsk[i])
        if(spu.emsk[i]) begin
          rfm.base[i] = rfm.base[i] + rfm.os[i];
          if(rfm.base[i] >= VADR_MAPPED && rfm.base[i] < VADR_NMAPNC) begin
           found = 1;
           vadr = rfm.base[i];
          end
        end
        
      if(found) begin
        toTLB = tr_dse2tlb::type_id::create("toTLB", this);
        toTLB.vAdr = vadr >> VADR_START;
        toTLB.op = ise.op;
        toTLB.tid = ise.tid;
        toTLB.req = 1;
        tlbReqVAdr[STAGE_RRF_TAG] = toTLB.vAdr;
      end
    end
    
    if(toRFM != null) void'(rfm_tr_port.nb_transport(toRFM, toRFM));
  endfunction

///------------------------------nb_transport functions---------------------------------------
 
  function bit nb_transport_ise(input tr_ise2dse req, output tr_ise2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get ise Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmISE[STAGE_RRF_RRC0] = req;
    return 1;
  endfunction : nb_transport_ise

  function bit nb_transport_rfm(input tr_rfm2dse req, output tr_rfm2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get rfm Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmRFM[STAGE_RRF_AG] = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_spu(input tr_spu2dse req, output tr_spu2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get spu Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPU[STAGE_RRF_AG] = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2dse req, output tr_spa2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get spa Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmSPA = req;
    return 1;
  endfunction : nb_transport_spa

  function bit nb_transport_tlb(input tr_tlb2dse req, output tr_tlb2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get tlb Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmTLB = req;
    return 1;
  endfunction : nb_transport_tlb

  function bit nb_transport_eif(input tr_eif2dse req, output tr_eif2dse rsp);
    ovm_report_info("dse_tr", $psprintf("Get EIF Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fmEIF = req;
    return 1;
  endfunction : nb_transport_eif
          
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
    cacheGrp = 0;
  endfunction : build
endclass : ip4_tlm_dse

///-------------------------------------other functions-----------------------------------------
  
