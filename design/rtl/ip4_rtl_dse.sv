/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_rtl_dse.sv
/// Title            : ip4 stream processor array
/// Version          : 0.1
/// Last modified    : Feb 23 2011
/// =============================================================================
///Log:
///Created by Andy Chen on Feb 23 2011

`include "ip4_rtl.svh"

module ip4_rtl_dse(
  input logic clk, rst_n,
  ip4_int_if.dse inf
);
  `include "ip4_tlm_ts.svh"
  import ip4_rtl_pkg::*;
  `IP4_DEF_PARAM
  
  typedef struct {
    ///store xchg stage data struct
    bit sMemOpy[NUM_SP],   ///occupy for onchip shared mem
        exMemOpy[NUM_SP], ///cl info for ex store
        exEn[NUM_SP][WORD_BYTES],  ///ext enabled
        exLxgEn[NUM_SP][WORD_BYTES],
        exSxgEn[NUM_SP][WORD_BYTES],
        sMemWEn[NUM_SP][WORD_BYTES];
    smadr_t sMemAdr[NUM_SP];
    uint sMemGrp[NUM_SP];  ///on chip adr grp
    wordu stData[NUM_SP]; ///store exchange buffer
    
    bit exp[NUM_SP], oc[NUM_SP], ex[NUM_SP], re[NUM_SP];
    
    bit[WID_DCHE_CL + WID_SMEM_BK + WID_WORD - 1:0] ladr[NUM_SP];
    uchar sl[NUM_SP][WORD_BYTES],
          bk[NUM_SP][WORD_BYTES],
          os[NUM_SP][WORD_BYTES];
    opcode_e op;
    uchar tid;
  }sxg_t;
    
  typedef struct { 
    ///load xchg data struct
    wordu data[NUM_SP];
    bit vrfWEn[NUM_SP];
  }lxg_t;

  typedef struct{
    exadr_t adr;
    uchar tid[NUM_DCHE_CL][NUM_SP];
    bit en[NUM_DCHE_CL][NUM_SP];
  }ll_ck_t;
  
  typedef struct {
    bit is_ld,
        is_st,
        is_word,
        is_half,
        is_byte,
        exReq,
        expReq,
        endian,
        last,
        xhgEnd;
    word tlbReqVAdr,
         cacheIdx;
    bit is_nmapch[NUM_SP],
        is_ejtags[NUM_SP],
        is_nmapnc[NUM_SP],
        is_mapch[NUM_SP];
    uchar cacheGrp,
          tid,
          subVec;
    cause_dse_t expCause;
    opcode_e opcode;
    sxg_t sxg[CYC_HVEC];
  } dvars;

  typedef struct {
    tlb2dse_s tlbCached;
///    word srMapBase;
    uchar srCacheGrp,
          sendExpTid,
          selCacheAso,
          llNext;
///cacheGrpEn[NUM_SMEM_GRP],
    bit sendExp,
        tlbRdy,
        selExRdy,
        selEndian,
        selWriteAlloc,
        selCoherency,
        selNoCache,
        selLock2CL,
        selExpReq,
        selValidReq,
        needExOc,
        selNeedLock2CL,
        selExp;
    cause_dse_t selCause;
    bit[WID_SMEM_GRP - 1:0] grpMsk;
    exadr_t selExAdr;
    bit asohit[NUM_DCHE_ASO];
    sxg_t sxgSel[LAT_SEL][CYC_HVEC], sxgDC[CYC_HVEC], sxgLxg[CYC_HVEC];
    ll_ck_t llCk[NUM_LLCK];
  } svars;
      
  typedef struct {
    ise2dse_s fmISE[STAGE_RRF_VWB:0];
    spu2dse_s fmSPU[STAGE_RRF_LXG:STAGE_RRF_AG];
    rfm2dse_s fmRFM[STAGE_RRF_LXG:STAGE_RRF_AG];
    eif2dse_s fmEIF[STAGE_RRF_LXG:STAGE_RRF_AG];
    spa2dse_s fmSPA;
    tlb2dse_s fmTLB;
  
    dse2rfm_s rfm[STAGE_RRF_VWBP:STAGE_RRF_SXG0];
    dse2eif_s eif[STAGE_RRF_LXG:STAGE_RRF_SXG0];
    dse2spu_s spu[STAGE_RRF_DPRB:STAGE_RRF_DEM];
    
    dvars d[STAGE_RRF_VWB:0];
    bit[STAGE_RRF_LXG:0] cancel[NUM_THREAD];
  } vars;
  
  vars v, vn;
  svars s, sn;
    
  wordu dmi[NUM_SP], dmo[NUM_SP], dms[NUM_SP];
  smadr_t dmRAdr[NUM_SP], dmWAdr[NUM_SP];
  logic dmWr[NUM_SP];

  bit tmWrTag, tmWrCnt, tmWrSt;
  bit[WID_DCHE_IDX - 1:0] tmAdr0, tmAdr1, tmWAdr;
  bit[WID_SMEM_GRP - 1:0] tmGrp0, tmGrp1, tmWGrp;
  cache_t tmi, tmo0, tmo1, tms0, tms1;
    
  always_ff @(posedge clk or negedge rst_n)
    if(!rst_n) begin
      v <= '{default : '0};
      s <= '{default : '0};
      dms <= '{default : '0};
      tms0 <= '{default : '0};
      tms1 <= '{default : '0};
    end
    else begin
      v <= vn;
      s <= sn;
      tms0 <= tmo0;
      tms1 <= tmo1;
      if(!sn.selLock2CL)
        dms <= dmo;
    end
  
  always_comb
  begin : comb_proc
    automatic sxg_t sxgBuf[CYC_HVEC] = s.sxgSel[0];
    
    begin : pip_init
      vn = '{default : '0};
      sn = s;
      
      inf.dse2eif = '{default : '0};
      inf.dse2rfm = '{default : '0};
      inf.dse2spu = '{default : '0};
      inf.dse2ise = '{default : '0};
      
      vn.fmSPA = inf.spa2dse;
      vn.fmTLB = inf.tlb2dse;
      vn.fmISE[0] = inf.ise2dse;
      vn.fmRFM[STAGE_RRF_TAG] = inf.rfm2dse;
      vn.fmSPU[STAGE_RRF_AG] = inf.spu2dse;
      vn.fmEIF[STAGE_RRF_AG] = inf.eif2dse;
      
      for (int i = STAGE_RRF_VWB; i > 0; i--) begin
        vn.fmISE[i] = v.fmISE[i - 1];
        vn.d[i] = v.d[i - 1];
      end
  
      for (int i = STAGE_RRF_LXG; i > STAGE_RRF_AG; i--) begin
        vn.fmEIF[i] = v.fmEIF[i - 1];
        vn.fmSPU[i] = v.fmSPU[i - 1];
        vn.fmRFM[i] = v.fmRFM[i - 1];
      end
      
      for(int i = STAGE_RRF_VWBP; i > STAGE_RRF_SXG0; i--) 
        vn.rfm[i] = v.rfm[i - 1];
      vn.rfm[STAGE_RRF_SXG0] = '{default : '0};
  
      for(int i = STAGE_RRF_DPRB; i > STAGE_RRF_DEM; i--)
        vn.spu[i] = v.spu[i - 1];
      vn.spu[STAGE_RRF_DEM] = '{default : '0};
      
      for(int i = STAGE_RRF_LXG; i > STAGE_RRF_SXG0; i--)
        vn.eif[i] = v.eif[i - 1];
      vn.eif[STAGE_RRF_SXG0] = '{default : '0};

      for(int i = 0; i < NUM_THREAD; i++)
        vn.cancel[i] = v.cancel[i] << 1;
        
      ///cancel from spa
      if(v.fmSPA != null && v.fmSPA.cancel)
        vn.cancel[v.fmSPA.tid] |= `GML(STAGE_RRF_DC);
  
      ///cancel from spu
      if(v.fmSPU[0].en) begin
        if(v.fmSPU[0].missBr || v.fmSPU[0].expMSC)
          vn.cancel[v.fmSPU[0].tidExpMSC] |= `GML(STAGE_RRF_DC);
        if(v.fmSPU[0].expFu)
          vn.cancel[v.fmSPU[0].tidExpFu] |= `GML(STAGE_RRF_EPS - v.fmSPU[0].vecModeExpFu);
      end
      
      ///cancel from self
      if(v.fmISE[STAGE_RRF_DPRB].en && s.sendExp)
        vn.cancel[s.sendExpTid] |= `GML(STAGE_RRF_DEM);
      sn.sendExp = 0;
      
      if(v.fmTLB.en)
        sn.tlbCached = v.fmTLB;
        
      dmWr = '{default : '0};
      dmWAdr = '{default : '0};
      dmRAdr = '{default : '0};
      dmi = '{default : '0};
      tmWrTag = '0;
      tmAdr0 = '0;
      tmAdr1 = '0;
      tmWAdr = '0;
      tmGrp0 = '0;
      tmGrp1 = '0;
      tmWGrp = '0;
      tmi = tms0;
    end : pip_init
    
    begin : dv_init
      if(vn.fmISE[0].en) begin
        automatic ise2dse_s ise = vn.fmISE[0];
        automatic dvars dn = '{default : '0};
        
        dn.is_ld = ise.op inside {ld_ops};
        dn.is_st = ise.op inside {st_ops};
        dn.is_word = ise.op inside {op_lw, op_ll, op_sw, op_sc};
        dn.is_half = ise.op inside {op_lh, op_lhu, op_sh};
        dn.is_byte = ise.op inside {op_lb, op_lbu, op_sb};
        vn.d[0] = dn;
      end
    end : dv_init
    
    begin : ag_stage
      automatic ise2dse_s ise = v.fmISE[STAGE_RRF_AG];
      automatic rfm2dse_s rfm = v.fmRFM[STAGE_RRF_AG], rfmn = vn.fmRFM[STAGE_RRF_TAG];
      automatic spu2dse_s spu = v.fmSPU[STAGE_RRF_AG];
      automatic dvars d = v.d[STAGE_RRF_AG], dn = vn.d[STAGE_RRF_TAG];
      
      automatic padr_t vadr = 0;
      automatic bit found = 0;

      ///AG stage: select vadr from ise req to tlb for translation
      if(v.fmISE[STAGE_RRF_AG].en && v.fmRFM[STAGE_RRF_AG].en 
          && v.fmSPU[STAGE_RRF_AG].en)
      begin
        if(d.is_ld || d.is_st || ise.op inside {op_cmpxchg, op_fetadd}) begin /// op_tmrf
          for(int i = 0; i < NUM_SP; i++) begin
            automatic uchar vecId = NUM_SP * ise.subVec + i;
            if(spu.emsk[i]) begin
              rfmn.base[i] = rfm.base[i] + rfm.os;
              if(ise.at == at_burst || ise.op == op_tmrf) begin
                if(d.is_word)
                  rfmn.base[i] += vecId << 2;
                else if(d.is_half)
                  rfmn.base[i] += vecId << 1;
                else
                  rfmn.base[i] += vecId;
              end
              if(rfmn.base[i] >= VADR_NMAPCH)
                dn.is_nmapch[i] = 1;
              else if(rfmn.base[i] >= VADR_EJTAGS)
                dn.is_ejtags[i] = 1;
              else if(rfmn.base[i] >= VADR_NMAPNC)
                dn.is_nmapnc[i] = 1;
              else if(rfmn.base[i] >= VADR_MAPPED)
                dn.is_mapch[i] = 1;
            end
          end
        end
        
        if(!ise.vec) begin
          for(int i = 1; i < NUM_SP; i++) begin
            dn.is_nmapnc[i] = 0;
            dn.is_ejtags[i] = 0;
            dn.is_nmapnc[i] = 0;
            dn.is_mapch[i] = 0;
          end
        end
        
        for(int i = 0; i < NUM_SP; i++) begin
          if(dn.is_mapch[i]) begin
            found = 1;
            vadr = rfmn.base[i];
            break;
          end
        end
              
        inf.dse2tlb.vAdr = vadr >> VADR_START;
        if(found && (!s.tlbRdy || (v.d[STAGE_RRF_TAG].tlbReqVAdr != inf.dse2tlb.vAdr))) begin
          sn.tlbRdy = 1;
          inf.dse2tlb.en = 1;
          inf.dse2tlb.op = ise.op;
          inf.dse2tlb.tid = ise.tid;
          inf.dse2tlb.k = ise.priv;
          dn.tlbReqVAdr = inf.dse2tlb.vAdr;
        end
        
        found = 0;
        for(int i = 0; i < NUM_SP; i++) begin
          if(dn.is_mapch[i] || dn.is_nmapch[i]) begin
            found = 1;
            vadr = rfmn.base[i];
            if(dn.is_mapch[i])
              break;
          end
        end
                
        if(found) begin
          tmWrTag = 0;
          tmAdr0 = vadr.ex.c.idx;
          tmGrp0 = vadr.ex.c.t.grp;
          dn.cacheIdx = tmAdr0;
          dn.cacheGrp = tmGrp0;
        end
        vn.fmRFM[STAGE_RRF_TAG] = rfmn;
        vn.d[STAGE_RRF_TAG] = dn;
      end
    end : ag_stage

    begin : spu_ops
      ///spu ops
      automatic spu2dse_s spu = v.fmSPU[`SG(STAGE_RRF_SRA, STAGE_RRF_SRA, STAGE_RRF_AG)];
      if(spu.en && spu.s2gp) begin
        inf.dse2spu.en = 1;
        case(spu.srAdr)
///        SR_MBASE: inf.dse2spu.srRes = v.srMapBase;
        SR_OCMC:  inf.dse2spu.srRes = s.srCacheGrp;
        endcase
      end
      if(spu.en && spu.op == op_gp2s && !v.cancel[STAGE_RRF_SRA][spu.tid]) begin
        case(spu.srAdr)
///        SR_MBASE: vn.srMapBase = spu.op0;
        SR_OCMC:
        begin
          if(spu.op0 > NUM_SMEM_GRP)
            sn.srCacheGrp = NUM_SMEM_GRP;
          else
            sn.srCacheGrp = spu.op0;
          sn.grpMsk = `GML(n2w(sn.srCacheGrp));
          sn.srCacheGrp = sn.srCacheGrp & (~sn.grpMsk);
///          for(int i = 0; i < NUM_SMEM_GRP; i++)
///            if(i >= (NUM_SMEM_GRP - sn.srCacheGrp))
///              sn.cacheGrpEn[i] = 1;
        end
        endcase
      end
      else begin
        sn.tlbRdy = 0;
        sn.tlbCached.en = 0;
      end
    end : spu_ops
        
    begin : sel_stage_ld_st
      automatic ise2dse_s ise = v.fmISE[STAGE_RRF_SEL];
      automatic rfm2dse_s rfm = v.fmRFM[STAGE_RRF_SEL];
      automatic spu2dse_s spu = v.fmSPU[STAGE_RRF_SEL];
      automatic eif2dse_s eif = v.fmEIF[STAGE_RRF_SEL];
      automatic tlb2dse_s tlb = v.fmTLB;
      automatic dvars d = v.d[STAGE_RRF_SEL],
                      dn = vn.d[STAGE_RRF_SXG0];
      automatic word tlbVAdr;
      
      if(spu.en && rfm.en && ise.en && (d.is_ld || d.is_st))
      begin : valid_en
        automatic padr_t smStart = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE,
                         smEnd   = srMapBase + SMEM_OFFSET + pbId * SMEM_SIZE + (NUM_SMEM_GRP - s.srCacheGrp) * SGRP_SIZE,
                         smEnd2  = srMapBase + SMEM_OFFSET + (pbId + 1) * SMEM_SIZE;  
        automatic uchar minSlot = ise.vec ? 0 : CYC_HVEC - 1,
                        cyc = ise.subVec & `GML(WID_XCHG);
        automatic bit last = ise.subVec == (CYC_VEC - 1) || !ise.vec,
                      xhgEnd = ise.subVec == (CYC_HVEC - 1) || last,
                      needExOc = 0,
                      exNeedSxg = 0;
        automatic exadr_t lladr;
        automatic bit llrdy = 0;
        automatic uchar llid;

        dn.exReq = 0;
        dn.subVec = ise.subVec;
        dn.last = last;
        dn.xhgEnd = xhgEnd;
        
        if(!tlb.en)
          tlb = s.tlbCached;
        if(tlb.en) begin
          tlbVAdr = d.tlbReqVAdr >> tlb.eobit;
          if(!s.selExRdy) begin
            sn.selEndian = tlb.endian;
            sn.selWriteAlloc = tlb.writeAlloc;
            sn.selCoherency = tlb.coherency;
            sn.selNoCache = !tlb.cached;
            sn.needExOc = tlb.writeThru && d.is_st;
            sn.selNeedLock2CL |= tlb.coherency && d.is_st;
          end
        end
        
        if(dn.subVec == 0)
          sxgBuf = '{default : 0};
          
        for(int sp = 0; sp < NUM_SP; sp++) begin : sp_iter
          automatic padr_t padr;
          automatic bit nc, exp;
          automatic wordu st = rfm.st[sp];
          automatic bit oc = spu.emsk[sp],
                        ex = spu.emsk[sp] && !ise.noExt,
                        ocWEn;
          automatic uchar grp = 0,
                          bk = 0,
                          os = 0,
                          slot = minSlot,
                          cl = 0,
                          clc = 0;
          automatic smadr_t adr;
///          wordu res;
          automatic tag_t tag;
          
          begin : init
          if(d.is_nmapnc[sp]) begin
            padr = rfm.base[sp];
            sn.selEndian = 1;
          end
          else if(d.is_ejtags[sp]) begin
            nc = 1;
            oc = 0;
            padr = srMapBase + EJTG_OFFSET + pbId * EJTG_SIZE + rfm.base[sp] - VADR_EJTAGS;
            sn.selEndian = 1;
          end
          else if(d.is_nmapnc[sp]) begin
            nc = 1;
            oc = 0;
            padr = srMapBase + rfm.base[sp] - VADR_NMAPNC;
            sn.selEndian = 1;
          end
          ///vadr to padr translation stage
          else if(d.is_mapch[sp] && tlb.en) begin
            padr = rfm.base[sp];
            if(!spu.emsk[sp]) begin
              ex = 0;
              oc = 0;
            end
            if(!s.selExp && tlb.exp) begin
              sn.selExp = 1;
              exp = 1;
              sn.selCause = tlb.cause;
            end
            if((rfm.base[sp] >> (VADR_START + tlb.eobit)) != tlbVAdr || tlb.exp) begin
              oc = 0;
              ex = 0;
            end
            nc = !tlb.cached;
            for(int j = 0; j < PADR_WIDTH - VADR_START; j++)
              if(j >= tlb.eobit)
                padr[j] = tlb.pfn[j];
          end
          else begin
            oc = 0;
            ex = 0;
          end
  
          if(!ise.vec && sp != 0) begin
            oc = 0;
            ex = 0;
            exp = 0;
          end
                
          if(padr >= smStart && padr < smEnd2)
            ex = 0;
          end : init
          
          if(oc || ex) begin : acc_data ///access data
            ///align exp
            if(((d.is_half || ise.op == op_cmpxchg) && padr[0] != 1'b0)
               || ((d.is_word || ise.op == op_fetadd) && padr[1:0] != 2'b0))
            begin
              if(!sn.selExp) begin
                sn.selCause = EC_ADRALG;
                exp = 1;
                sn.selExp = 1;
              end
              ex = 0;
              oc = 0;
            end
    
            os = padr.os;        
            bk = padr.bk;
            adr = padr.ex.s.a;
            grp = padr.ex.c.t.grp;
            cl = padr.ex.c.cl;
            clc = ise.vec ? cl & `GML(WID_XCHG) : 0;
            tag = padr.ex.c.t;
            exNeedSxg = (!((cl < CYC_HVEC) ^ (ise.subVec < CYC_HVEC))) || !ise.vec;
            
            ///----------------------start access----------------------------
            ///external mem
            ///**cache address:   | grp | aso | idx | cl | bk | offset |
            ///           |    tag      | grp |
            if(ex) begin : ex_cache
              automatic bit hit = 0;
              automatic uint idx = 0;
              
              ///chk cache for match
              if(!nc && oc && s.srCacheGrp != 0) begin ///when oc is possible
                automatic uint grp = (tag.grp & s.grpMsk) + (NUM_SMEM_GRP - s.srCacheGrp),
                               idx = padr.ex.c.idx;
                if(d.cacheIdx == idx && d.cacheGrp == grp) begin
                  ///only idx and grp matched are allowed
                  for(int asoIdx = 0; asoIdx < NUM_DCHE_ASO; asoIdx++) begin
                    if(!(sn.selLock2CL && sn.selCacheAso == asoIdx)) begin
                      sn.selCacheAso = asoIdx;
                      hit = tms0.tag[asoIdx] == tag;
                      if(tms0.state[asoIdx] == cs_inv)
                        hit = 0;
                      if(d.is_st && tms0.state[asoIdx] == cs_shared)
                        hit = 0;
                      if(hit) begin
                        adr.aso = asoIdx;
                        sn.asohit[asoIdx] = 1;
                        if(!sn.needExOc)
                          ex = 0;
                      end
                      if(hit) break;
                    end
                  end
                end
                else begin
                  ///can't access this time, unknown if onchip or not
                  oc = 0;
                  ex = 0;
                end                
              end
              else
                oc = 0;
  
              ///case when ex can fail, then bank select is not necessory
              if(sn.needExOc && sn.selExRdy && sn.selExAdr != padr.ex)
                oc = 0;
                
              ///cache hit, allocate exchange bank
              if(oc) begin
                automatic bit found = 0;
                for(int s = minSlot; s < CYC_HVEC; s++) begin
                  if(!sxgBuf[s].exMemOpy[bk] &&
                    ((sxgBuf[s].sMemAdr[bk] == adr && sxgBuf[s].sMemGrp[bk] == grp) || !sxgBuf[s].sMemOpy[bk]))
                  begin
                    sxgBuf[s].sMemOpy[bk] = 1;
                    slot = s;
                    found = 1;
                    break;
                  end
                end
                oc = found;
              end
              
              if(sn.needExOc && !oc)
                ex = 0;
                
              ///external access
              if(ex) begin : ex_acc
                automatic bit exhit = sn.selExAdr.c.t == padr.ex.c.t && sn.selExAdr.c.idx == padr.ex.c.idx,
                              found = 0;
                if(!sn.selExRdy || exhit) begin
                  sn.selExRdy = 1;
                  sn.selNoCache |= nc;
                  sn.selExAdr = padr.ex;
                  ///some ex stores needs sxg xchg network too
                  if(exNeedSxg && d.is_st) begin
                    for(int s = minSlot; s < CYC_HVEC; s++) begin
                      if(!sxgBuf[s].sMemOpy[bk] &&
                        (!sxgBuf[s].exMemOpy[bk] || sxgBuf[s].sMemAdr[bk] == cl))
                      begin
                        sxgBuf[s].exMemOpy[bk] = 1;
                        sxgBuf[s].sMemAdr[bk] = cl;
                        slot = s;
                        found = 1;
                        break;
                      end
                    end
                    if(!found)
                      ex = 0;
                  end
                end
                else begin
                  ex = 0;
                end
              end : ex_acc
              
              if(sn.needExOc && !ex)
                oc = 0;
                          
              ///lock to this cache line if accessed
              sn.selLock2CL |= sn.selNeedLock2CL && oc;
            end : ex_cache
            ///**shared mem
            else if(oc) begin : oc_acc
              if(padr >= smEnd) begin
                if(!sn.selExp) begin
                  sn.selCause = EC_SMBOND;
                  sn.selExp = 1;
                  exp = 1;
                end
                oc = 0;
              end
              
              begin
                automatic bit found = 0;
                for(int s = minSlot; s < CYC_HVEC; s++) begin
                  if((!sxgBuf[s].exMemOpy[bk]) &&
                    ((sxgBuf[s].sMemAdr[bk] == adr && sxgBuf[s].sMemGrp[bk] == grp) || !sxgBuf[s].sMemOpy[bk]))
                  begin
                    sxgBuf[s].sMemOpy[bk] = oc;
                    slot = s;
                    found = 1;
                    break;
                  end
                  if(found) break;
                end
                oc = oc && found;
              end
            end : oc_acc
           
            ///load link & store conditional
            if(ise.op inside {op_ll, op_sc} && (oc || ex))
            begin : ll_sc_proc
              automatic bit found = 0, failed = 1;
              automatic uint idx = padr.ex.c.idx;
              if(!llrdy) begin
                ///one cycle can only check one valid address in vector
                llrdy = 1;
                for(int i = 0; i < NUM_LLCK; i++) begin
                  if(sn.llCk[i].adr.c.t == tag && sn.llCk[i].adr.c.idx == idx) begin
                    found = 1;
                    llid = i;
                    lladr = padr.ex;
                  end
                end
                if(!found && ise.op == op_ll) begin
                  ///no entry found, if its a ll, allocate one
                  sn.llNext++;
                  if(sn.llNext >= NUM_LLCK)
                    sn.llNext = 0;
                  llid = sn.llNext;
                  found = 1;
                  sn.llCk[llid].adr.c.t = tag;
                  sn.llCk[llid].adr.c.idx = idx;
                  sn.llCk[llid].en = '{default : 0};
                end
              end
              else
                found = lladr.c.t == tag && lladr.c.idx == idx;
              
              if(found) begin
                if(ise.op == op_ll) begin
                  sn.llCk[llid].en[cl][bk] = 1;
                  sn.llCk[llid].tid[cl][bk] = ise.tid;
                end
                else begin
                  ///store conditional
                  failed = !(sn.llCk[llid].en[cl][bk] && sn.llCk[llid].tid[cl][bk] == ise.tid);
                  sn.llCk[llid].en[cl][bk] = 0;  ///so following sc failed
                end
              end
              
              if(failed && ise.op == op_sc) begin
                oc = 0;
                ex = 0;
              end
            end : ll_sc_proc
            
            ///sxg stage, filling sxgBuf, exchange data
            if(oc) begin
              sxgBuf[slot].sMemAdr[bk] = adr;
              sxgBuf[slot].sMemGrp[bk] = grp;
            end
            
            ocWEn = d.is_st && oc;
            
            if(oc || ex) begin : sxg_stage
              case(ise.op)
              op_sw,
              op_sc:
                for(int os2 = 0; os2 < WORD_BYTES; os2++) begin
                  automatic uchar os3 = sn.selEndian ? os2 : WORD_BYTES - os2;
                  if(exNeedSxg) begin
                    sxgBuf[slot].stData[bk].b[os3] = st.b[os2];
                    sxgBuf[slot].exSxgEn[bk][os3] = ex;
                  end
                  sxgBuf[slot].sMemWEn[bk][os2] = ocWEn;
                  sxgBuf[minSlot + clc].exEn[bk][os3] = ex;
                  sxgBuf[minSlot + clc].exLxgEn[bk][os3] = ex && !exNeedSxg;
                  sxgBuf[minSlot + clc].sl[bk][os3] = cyc;
                  sxgBuf[minSlot + clc].os[bk][os3] = os2;
                  sxgBuf[minSlot + clc].bk[bk][os3] = sp;
                end          
              op_lw,
              op_ll: 
                for(int os2 = 0; os2 < WORD_BYTES; os2++) begin
                  sxgBuf[minSlot + clc].exEn[bk][os2] =  ex;
                  sxgBuf[minSlot + cyc].os[sp][os2] = os2;
                  sxgBuf[minSlot + cyc].sl[sp][os2] = slot - minSlot;
                  sxgBuf[minSlot + cyc].bk[sp][os2] = bk;
                end
              op_sh:
              begin
                 automatic uchar adr2 = os & `GMH(WID_HALF);
                 for(int os2 = 0; os2 < HALF_BYTES; os2++) begin
                  automatic uchar os3 = sn.selEndian ? adr2 + os2 : WORD_BYTES - adr2 - os2;
                  if(exNeedSxg) begin
                    sxgBuf[slot].stData[bk].b[os3] = st.b[os2];
                    sxgBuf[slot].exSxgEn[bk][os3] = ex;
                  end
                  sxgBuf[slot].sMemWEn[bk][adr2 + os2] = ocWEn;
                  sxgBuf[minSlot + clc].exEn[bk][os3] = ex;
                  sxgBuf[minSlot + clc].exLxgEn[bk][os3] = ex && !exNeedSxg;
                  sxgBuf[minSlot + clc].os[bk][os3] = os2;
                  sxgBuf[minSlot + clc].sl[bk][os3] = cyc;
                  sxgBuf[minSlot + clc].bk[bk][os3] = sp;
                end
              end
              op_lh,
              op_lhu:
              begin
                automatic uchar adr2 = os & `GMH(WID_HALF);
                for(int os2 = 0; os2 < HALF_BYTES; os2++) begin
                  sxgBuf[minSlot + clc].exEn[bk][adr2 + os2] = ex;
                  sxgBuf[minSlot + cyc].os[sp][os2] = adr2 + os2;
                  sxgBuf[minSlot + cyc].sl[sp][os2] = slot - minSlot;
                  sxgBuf[minSlot + cyc].bk[sp][os2] = bk;
                end
              end
              op_sb:
              begin
                automatic uchar os3 = sn.selEndian ? os : WORD_BYTES - os;
                if(exNeedSxg) begin
                  sxgBuf[slot].stData[bk].b[os3] = st.b[0];
                  sxgBuf[slot].exSxgEn[bk][os3] = ex;
                end
                sxgBuf[slot].sMemWEn[bk][os] = ocWEn;
                sxgBuf[minSlot + clc].exEn[bk][os3] = ex;
                sxgBuf[minSlot + clc].exLxgEn[bk][os3] = ex && !exNeedSxg;
                sxgBuf[minSlot + clc].os[bk][os3] = 0;
                sxgBuf[minSlot + clc].sl[bk][os3] = cyc;
                sxgBuf[minSlot + clc].bk[bk][os3] = sp;
              end          
              op_lb,
              op_lbu:
              begin
                sxgBuf[minSlot + clc].exEn[bk][os] =  ex;
                sxgBuf[minSlot + cyc].os[sp][0] = os;
                sxgBuf[minSlot + cyc].sl[sp][0] = slot - minSlot;
                sxgBuf[minSlot + cyc].bk[sp][0] = bk;
              end
              endcase
            end : sxg_stage
          end : acc_data
          sxgBuf[minSlot + cyc].exp[sp] = exp;
          sxgBuf[minSlot + cyc].oc[sp] = oc;
          sxgBuf[minSlot + cyc].ex[sp] = ex;
          sxgBuf[minSlot + cyc].re[sp] = (ise.vec ? spu.emsk[sp] : sp == 0) && !oc && !ex;
          sxgBuf[minSlot + cyc].ladr[sp] = padr;/// & `GML(WID_DCHE_CL + WID_SMEM_BK + WID_WORD);
         
          sn.selValidReq |= oc || ex;
          dn.exReq |= ex;
        end : sp_iter
        dn.expCause = sn.selCause;
        dn.endian = sn.selEndian;
        
        sn.selExpReq |= sn.selExp;
  
        ///start resolve exp
        if(last) begin
          automatic bit res = sn.selExpReq;
          if(ise.nonBlock)
            res &= !sn.selValidReq;
          
          if(ise.vec)  
            for(int i = 0; i < CYC_VEC; i++)
              vn.d[STAGE_RRF_SXG0 + i].expReq = res;
          else
            dn.expReq = res;
          sn.sendExp = res;            
          sn.sendExpTid = ise.tid;
          sn.selExpReq = 0;
          sn.selValidReq = 0;
        end
        else
          dn.expReq = 1;

        dn.opcode = ise.op;
        dn.tid = ise.tid;
        
        ///finish one half wrap or whole request
        if(xhgEnd) begin
          sn.selExp = 0;
          sn.selNoCache = 0;
          sn.selEndian = 0;
          sn.selWriteAlloc = 0;
          sn.selCoherency = 0;
          sn.selNeedLock2CL = 0;
        end

        ///if write to owner without need2lockcl change it to dirty
        for(int i = 0; i < NUM_DCHE_ASO; i++)
          if(sn.asohit[i]) begin
            if(d.is_st && !sn.selNeedLock2CL && tms0.state[i] == cs_exclusive)
              tmi.state[i] = cs_dirty;
            if(tms0.cnt[i] >= 2)
              tmi.cnt[i] -= 2;
            else
              tmi.cnt[i] = 0;
          end
        
        if(last) begin
          ///cache state (silent) transitions
          if(sn.selLock2CL) begin
            tmWrSt = 1;
            case(tms0.state[sn.selCacheAso])
            cs_exclusive: tmi.state[sn.selCacheAso] = cs_modified;
            cs_owned,
            cs_shared:    tmi.state[sn.selCacheAso] = cs_inv;
            ///this case happens only when tlb cc bit changed
            cs_dirty:     tmi.state[sn.selCacheAso] = cs_modified;
            default :     tmWrSt = 0;
            endcase
          end
          sn.selExRdy = 0;
          sn.selLock2CL = 0;
        end
        sn.asohit = '{default : '0};
      end : valid_en
      dn.sxg = sxgBuf;
    end : sel_stage_ld_st
    
    begin : last_sxg
      automatic ise2dse_s ise = v.fmISE[STAGE_RRF_SXG];
      automatic rfm2dse_s rfm = v.fmRFM[STAGE_RRF_SXG];
      automatic spu2dse_s spu = v.fmSPU[STAGE_RRF_SXG];
      automatic eif2dse_s eif = v.fmEIF[STAGE_RRF_SXG];
      automatic dvars d = v.d[STAGE_RRF_SXG],
                      dn = vn.d[STAGE_RRF_DC];      
      
      for(int bk = 0; bk < NUM_SP; bk++) begin
        
      end
    end : last_sxg
    
    begin : last_proc
      sn.sxgSel[1] = sxgBuf;
      if(sn.selLock2CL) begin
        vn.d[STAGE_RRF_TAG].cacheIdx = v.d[STAGE_RRF_TAG].cacheIdx;
        vn.d[STAGE_RRF_TAG].cacheGrp = v.d[STAGE_RRF_TAG].cacheGrp;
      end
    end : last_proc
  end : comb_proc
  
  genvar i;
  
  for(i = 0; i < NUM_SP; i++) begin : mem
    ip4_sm_bk d(
      .clk,
      .wr     (dmWr[i]),
      .radr   (dmRAdr[i]),
      .wadr   (dmWAdr[i]),
      .datai  (dmi[i]),
      .datao  (dmo[i])
    );
    
  end
  
  ip4_tm mem_t (
    .clk, 
    .wrTag  (tmWrTag),
    .wrSt   (tmWrSt),
    .wrCnt  (tmWrCnt),
    .adr0   (tmAdr0),
    .adr1   (tmAdr1),
    .wadr   (tmWAdr),
    .grp0   (tmGrp0),
    .grp1   (tmGrp1),
    .wgrp   (tmWGrp),
    .datai  (tmi),
    .datao0 (tmo0),
    .datao1 (tmo1)
  );

endmodule : ip4_rtl_dse

