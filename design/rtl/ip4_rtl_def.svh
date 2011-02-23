  typedef struct{
    bit en, vec, wrEn[2];
    opcode_e op;
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
    bit cancel;
    uchar tid;
  }spa2dse_s;
  