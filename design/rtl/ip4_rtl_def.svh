  typedef struct{
    bit en, vec, wrEn[2];
    opcode_e opcode;
    cmp_opcode_e cop;
    uchar wrBk, wrAdr, wrGrp;
    rbk_sel_e bpSel[NUM_FU_RP];
  }ise2spa_fu_s;

  typedef struct{
    ise2spa_fu_s fu[NUM_FU];
    pr_merge_e prMerge;
    uchar subVec, tid;  ///vecMode = 3
    uchar bpRfDSEwp;
    rbk_sel_e bpRfDSE;
    round_mode_t rndMode;
    uchar expMsk;
    bit noExp;
  }ise2spa_s;
    
  typedef struct{
    bit noFu[NUM_FU];
    bit exp;
    uchar tid;
  }spa2ise_s;
  
  typedef struct{ 
    bit emsk[NUM_SP];
  }spu2spa_fu_s;

  typedef struct{
    spu2spa_fu_s fu[NUM_FU];
  }spu2spa_s;
  
  typedef struct{
    bit presCmp0[NUM_SP], presCmp1[NUM_SP], cancel;
    uchar tid;
  }spa2spu_s;
  
  typedef struct{
    word op[NUM_SP];
  }rfm2spa_rp_s;

  typedef struct{
    rfm2spa_rp_s rp[NUM_FU_RP];
    bit en;
  }rfm2spa_fu_s;

  typedef struct{
	  rfm2spa_fu_s fu[NUM_FU];
  }rfm2spa_s;
  
  typedef struct{
    word res0[NUM_SP],	res1[NUM_SP];///, res_vsbp;
    bit wr[2], wrEn[NUM_SP], s2gp, gp2s, vec;
    uchar wrGrp, wrAdr, wrBk, subVec, tid;
    uint expFlag[NUM_SP];
    uchar srAdr;
    bit en;   ///used only for printing
  }spa2rfm_fu_s;
  
  typedef struct{
  	spa2rfm_fu_s fu[NUM_FU];
  	uchar tidCancel;
  	bit cancel;
  }spa2rfm_s;
  
///  typedef struct{
///  }dse2spa_s;
  
  typedef struct{
    bit en, cancel;
    uchar tid;
  }spa2dse_s;
  
  typedef struct{
    bit req, cacheFlush, cacheFill, vec,
        endian, queryNoHit,///allocFail
        coherency, priv, uncachable;
    opcode_e op;
    uchar id, vecMode, subVec, mrfAdr;
    exadr_t exAdr;
    word data[NUM_SP];
    bit[WORD_BYTES - 1:0] byteEn[NUM_SP];
    cache_state_t queryRes, state;
  }dse2eif_s;
  
  typedef struct{
    bit en, loadRsp, storeRsp, noVecSt, vec,
        rd, wr, alloc, noSglSt, noLd, endian,
        queryCacheState, queryAndUpdate;
    uchar id, subVec, vecMode;
    exadr_t exAdr;
    cache_state_t state;
    
    ///those 2 comes late
    bit[WORD_BYTES - 1:0] byteEn[NUM_SP];
    word data[NUM_SP];
  }eif2dse_s;
  
  typedef struct{
    bit en;
	  word base[NUM_SP], st[NUM_SP], os;
 }rfm2dse_s;
 
  typedef struct{
	  word res[NUM_SP], uaRes[NUM_SP];
	  bit wrEn[NUM_SP], wr, uaWrEn, exp;
	  uchar tid, tidExp;
	  bit expVec[NUM_SP], vec;
	  uchar wrGrp, wrAdr, wrBk, 
          uaWrGrp, uaWrAdr, uaWrBk, 
          subVec, vecMode, vecModeExp;
  }dse2rfm_s;
  
  typedef struct{
    bit emsk[NUM_SP];
    word op0;
    bit en, srReq, expFu, missBr, expMSC, s2gp;
    opcode_e op;
    uchar tid, srAdr, vecMode, tidExpFu, tidExpMSC, vecModeExpFu;
  }spu2dse_s;
  
  typedef struct{
    uchar tid, tidCancel;
    bit en, pres[NUM_SP], wrEn, rsp, cancel;
    word srRes;  
  }dse2spu_s;
  
  typedef struct{
    uchar wrGrp, wrAdr, wrBk,
          uaWrGrp, uaWrAdr, uaWrBk, tid;
    bit en, priv, vec, wr, uaWrEn, nonBlock, noExt, sendRotRight;
    opcode_e op;
    uchar vecMode, subVec, mrfAdr;
    update_adr_t ua;
    access_typ_t at;
  }ise2dse_s;
  
  typedef struct{
  /// sync to dem0 stage
    bit en,
        rsp,     ///respond
        extLd,     ///this req generate a external load transaction
        exp,     ///the whole req has exception
        scl,
        noExtLd, noExtSt,
        rlsPipLd, rlsPipSt;
    uchar tid, tidNoExt, vecMode, pendExLoad, pendExStore, pendSMsg, ldq, stq;
    cause_dse_t cause;
  }dse2ise_s;
  
  typedef struct{
    word pfn;
    bit en, endian, exp;
    bit writeAlloc, writeThru, coherency, cached;
    uchar eobit;  /// evenoddbit
    cause_dse_t cause;
  }tlb2dse_s;
  
  typedef struct{  
    word vAdr;
    opcode_e op;
    uchar tid;
    bit en, k;
  }dse2tlb_s;
  
  