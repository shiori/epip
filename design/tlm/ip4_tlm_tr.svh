typedef class inst_fg_c;    // endclass

///---------------------------trsaction ise_rfm rfm_ise------------------------
class ise2rfm_fu extends ovm_object;
  rand rbk_sel_e rdBkSel[NUM_FU_RP];
  rand word imm;
  rand bit en;
  
  constraint valid_var {
    en dist {0:=1, 1:=9};
    foreach(rdBkSel[i])
      rdBkSel[i] inside {[selv0:selv_e], [sels0:sels_e], [selc0:selc_e], selz, selii};
  }
  `ovm_object_utils_begin(ise2rfm_fu)
    `ovm_field_sarray_enum(rbk_sel_e, rdBkSel, OVM_ALL_ON)
    `ovm_field_int(imm, OVM_ALL_ON)    
    `ovm_field_int(en, OVM_ALL_ON)    
  `ovm_object_utils_end

	function new (string name = "ise2rfm_fu");
  	super.new(name);
	endfunction : new  
endclass : ise2rfm_fu

class tr_ise2rfm extends ovm_sequence_item;
	rand uchar vrfRdGrp[NUM_VRF_BKS], srfRdGrp[NUM_SRF_BKS],
             vrfRdAdr[NUM_VRF_BKS], srfRdAdr[NUM_SRF_BKS];
  
	rand ise2rfm_fu fu[NUM_FU];
	rand rbk_sel_e dseRdBk[4], spuRdBk[2];
	rand word bpCo[NUM_BP_CO], dseImm, spuImm;
	rand bit dseEn, spuEn;
	rand uchar cycFu, cycDSE, cycSPU, vecModeFu;
  
	constraint valid_var {
    cycFu < CYC_VEC;
    cycDSE < CYC_VEC;
    cycSPU < CYC_VEC;
  	foreach(vrfRdGrp[i]) {
    	vrfRdGrp[i] inside {[0:NUM_PHY_VRF_GRP-1]};
    	vrfRdAdr[i] inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
    }
  	foreach(srfRdGrp[i]) {
    	srfRdGrp[i] inside {[0:NUM_PHY_SRF_GRP-1]};
    	srfRdAdr[i] inside {[0:NUM_PRF_P_GRP/NUM_SRF_BKS-1]};
    }
    dseRdBk[0] inside {[selv0:selv_e], [sels0:sels_e], [selc0:selc_e], selz};
    dseRdBk[1] inside {[selv0:selv_e], [sels0:sels_e], [selc0:selc_e], selz};
    dseRdBk[2] inside {selii, selz};
    dseRdBk[3] inside {[selv0:selv_e], [sels0:sels_e], [selc0:selc_e], selz};
    foreach(spuRdBk[i])
      spuRdBk[i] inside {[sels0:sels_e], [selc0:selc_e], selz};
  }
  
	constraint dist_var {
  	dseEn dist {0:=6, 1:=4};
  	spuEn dist {0:=6, 1:=4};
  }
    
	function void post_randomize();
  	foreach(fu[i])
      assert(fu[i].randomize());
  endfunction
  
  `ovm_object_utils_begin(tr_ise2rfm)
    `ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
///    `ovm_field_int(tidFu, OVM_ALL_ON)
    `ovm_field_int(spuEn, OVM_ALL_ON)
    `ovm_field_int(dseEn, OVM_ALL_ON)
    `ovm_field_int(cycFu, OVM_ALL_ON)
    `ovm_field_int(cycDSE, OVM_ALL_ON)
    `ovm_field_int(cycSPU, OVM_ALL_ON)
    `ovm_field_int(vecModeFu, OVM_ALL_ON)
    `ovm_field_sarray_int(vrfRdGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_sarray_int(vrfRdAdr, OVM_ALL_ON + OVM_DEC)
    `ovm_field_sarray_int(srfRdGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_sarray_int(srfRdAdr, OVM_ALL_ON + OVM_DEC)
    `ovm_field_sarray_enum(rbk_sel_e, dseRdBk, OVM_ALL_ON)
    `ovm_field_sarray_enum(rbk_sel_e, spuRdBk, OVM_ALL_ON)
    `ovm_field_sarray_int(bpCo, OVM_ALL_ON)
    `ovm_field_int(dseImm, OVM_ALL_ON)
    `ovm_field_int(spuImm, OVM_ALL_ON)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
  	foreach(fu[i])
      if(fu[i].en)
        printer.print_object($psprintf("fu%0d", i), fu[i]);
	endfunction : do_print
    
	function new (string name = "tr_ise2rfm");
  	super.new(name);
  	foreach(fu[i])
      fu[i] = new();
	endfunction : new
endclass : tr_ise2rfm

class tr_rfm2ise extends ovm_sequence_item;
        
  `ovm_object_utils_begin(tr_rfm2ise)
  `ovm_object_utils_end
  
	function new (string name = "tr_rfm2ise");
  	super.new(name);
	endfunction : new
  
endclass : tr_rfm2ise

///---------------------------trsaction spu_rfm rfm_spu------------------------

class tr_spu2rfm extends ovm_sequence_item;
	rand bit wrEn, wrSrMSC;///, sel_sfu;
	rand word res; ///BRU ScalarP use port4
	rand uchar tid, vecMode, srfWrBk, srfWrGrp, srfWrAdr, subVec;
	rand bit expFu, missBr, expMSC;
	rand bit msco[NUM_SP], mscu[NUM_SP];
  
  ///wrEn signal is given one cycle before writeback
  
	constraint valid_wen {
  	wrEn dist {0:=1, 1:=9};
  }
  
	constraint valid_spu {
  	srfWrBk inside {[0:NUM_VRF_BKS-1]};
  	srfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
  	srfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
  }
    
  `ovm_object_utils_begin(tr_spu2rfm)
    `ovm_field_int(wrEn, OVM_ALL_ON)
    `ovm_field_int(res, OVM_ALL_ON)
    `ovm_field_int(expFu, OVM_ALL_ON)
    `ovm_field_int(missBr, OVM_ALL_ON)
    `ovm_field_int(expMSC, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(wrSrMSC, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_sarray_int(msco, OVM_ALL_ON)
    `ovm_field_sarray_int(mscu, OVM_ALL_ON)
    `ovm_field_int(srfWrBk, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(srfWrGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(srfWrAdr, OVM_ALL_ON + OVM_DEC)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2rfm");
  	super.new(name);
	endfunction : new
  
endclass : tr_spu2rfm

class tr_rfm2spu extends ovm_sequence_item;
	rand word op0, op1; ///BRU ScalarP use port4

  `ovm_object_utils_begin(tr_rfm2spu)
    `ovm_field_int(op0, OVM_ALL_ON)
    `ovm_field_int(op1, OVM_ALL_ON)  
  `ovm_object_utils_end
  
	function new (string name = "tr_rfm2spu");
  	super.new(name);
	endfunction : new
  
endclass : tr_rfm2spu

///---------------------------trsaction spa_rfm rfm_spa------------------------
class rfm2spa_rp extends ovm_object;
  rand word op[NUM_SP];
  
  `ovm_object_utils_begin(rfm2spa_rp)
    `ovm_field_sarray_int(op, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "rfm2spa_rp");
  	super.new(name);
	endfunction : new
endclass : rfm2spa_rp

class rfm2spa_fu extends ovm_object;
  rfm2spa_rp rp[NUM_FU_RP];
  rand bit en;
  
  `ovm_object_utils_begin(rfm2spa_fu)
    `ovm_field_sarray_object(rp, OVM_ALL_ON)
    `ovm_field_int(en, OVM_ALL_ON + OVM_NOPRINT + OVM_NOCOMPARE)
  `ovm_object_utils_end
  
	function new (string name = "rfm2spa_fu");
  	super.new(name);
  	foreach(rp[i])
      rp[i] = new();
	endfunction : new
endclass : rfm2spa_fu

class tr_rfm2spa extends ovm_sequence_item;
	rfm2spa_fu fu[NUM_FU];
  
  `ovm_object_utils_begin(tr_rfm2spa)
    `ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
  	foreach(fu[i])
      if(fu[i].en)
        printer.print_object($psprintf("fu%0d", i), fu[i]);
	endfunction : do_print
    
	function new (string name = "tr_rfm2spa");
  	super.new(name);
    foreach(fu[i])
      fu[i] = new();
	endfunction : new
  
	function void post_randomize();
    foreach(fu[i])
      assert(fu[i].randomize());
	endfunction
endclass : tr_rfm2spa

class spa2rfm_fu extends ovm_object;
  rand word res0[NUM_SP],	res1[NUM_SP];///, res_vsbp;
  rand bit wr[2], wrEn[NUM_SP], s2gp, gp2s, vec;
  rand uchar wrGrp, wrAdr, wrBk, subVec, tid;
  rand uint expFlag[NUM_SP];
  rand uchar srAdr;
  rand bit en;   ///used only for printing
  
	constraint valid_vars{
  	wrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
  	wrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
  	wrBk inside {[0:NUM_VRF_BKS-1]};
  	subVec dist {0:=5, 1:=5};
  	tid < NUM_THREAD;
  	foreach(wrEn[i])
    	wrEn[i] dist {0:=1, 1:=9};        
  }
    
  `ovm_object_utils_begin(spa2rfm_fu)
    `ovm_field_sarray_int(res0, OVM_ALL_ON)
    `ovm_field_sarray_int(res1, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_sarray_int(wr, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(s2gp, OVM_ALL_ON)
    `ovm_field_int(gp2s, OVM_ALL_ON)
    `ovm_field_int(vec, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_int(wrGrp, OVM_ALL_ON)
    `ovm_field_int(wrAdr, OVM_ALL_ON)
    `ovm_field_int(wrBk, OVM_ALL_ON)
    `ovm_field_sarray_int(expFlag, OVM_ALL_ON)
    `ovm_field_int(en, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_object_utils_end
  
	function new (string name = "spa2rfm_fu");
  	super.new(name);
	endfunction : new
endclass : spa2rfm_fu

class tr_spa2rfm extends ovm_sequence_item;
	spa2rfm_fu fu[NUM_FU];
	rand uchar tidCancel;
	rand bit cancel;
  
	function void post_randomize();
    static uchar lastSubVec[NUM_FU] = '{default:0};
    foreach(fu[i]) begin
      assert(fu[i].randomize());
    	if(lastSubVec[i] == 0 || lastSubVec[i] == (CYC_VEC - 1)) begin
      	lastSubVec[i] = fu[i].subVec;
    	end
    	else begin
        lastSubVec[i]++;
      	fu[i].subVec = lastSubVec[i];
    	end	    
    end
	endfunction : post_randomize
  
  `ovm_object_utils_begin(tr_spa2rfm)
    `ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(tidCancel, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
  `ovm_object_utils_end
  
	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
  	foreach(fu[i])
      if(fu[i].en)
        printer.print_object($psprintf("fu%0d", i), fu[i]);
	endfunction : do_print
  
	function new (string name = "tr_spa2rfm");
  	super.new(name);
    foreach(fu[i])
      fu[i] = new();
	endfunction : new

endclass : tr_spa2rfm

///---------------------------trsaction dse_rfm rfm_dse------------------------

class tr_dse2rfm extends ovm_sequence_item;
	rand word res[NUM_SP], uaRes[NUM_SP];
	rand bit wrEn[NUM_SP], wr, uaWrEn, exp;
	rand uchar tid, tidExp;
	rand bit expVec[NUM_SP], vec;
	rand uchar wrGrp, wrAdr, wrBk, 
             uaWrGrp, uaWrAdr, uaWrBk, 
             subVec, vecMode, vecModeExp;
  
	constraint valid_dse{
  	foreach(wrEn[i])
    	wrEn[i] dist {0:=1, 1:=9};
  	wrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
  	wrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
  	wrBk inside {[0:NUM_VRF_BKS-1]};
  	subVec dist {0:=5, 1:=5};
  	wr dist {0:=9, 1:=1};
  }

	function void post_randomize();
  	static uchar lastSubVec = 0;
  	if(lastSubVec == 0 || lastSubVec == (CYC_VEC - 1)) begin
    	lastSubVec = subVec;
  	end
  	else begin
      lastSubVec++;
    	subVec = lastSubVec;
  	end
	endfunction : post_randomize
    
  `ovm_object_utils_begin(tr_dse2rfm)
    `ovm_field_sarray_int(res, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_sarray_int(uaRes, OVM_ALL_ON)
    `ovm_field_sarray_int(expVec, OVM_ALL_ON)
    `ovm_field_int(vec, OVM_ALL_ON)
    `ovm_field_int(wrGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(wrAdr, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(wrBk, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(uaWrGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(uaWrAdr, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(uaWrBk, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(tidExp, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(vecModeExp, OVM_ALL_ON)
    `ovm_field_int(wr, OVM_ALL_ON)
    `ovm_field_int(uaWrEn, OVM_ALL_ON)
    `ovm_field_int(exp, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2rfm");
  	super.new(name);
	endfunction : new
  
endclass : tr_dse2rfm

class tr_rfm2dse extends ovm_sequence_item;
	rand word base[NUM_SP], st[NUM_SP], os;
  
  `ovm_object_utils_begin(tr_rfm2dse)
    `ovm_field_sarray_int(base, OVM_ALL_ON)
    `ovm_field_sarray_int(st, OVM_ALL_ON)
    `ovm_field_int(os, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_rfm2dse");
  	super.new(name);
	endfunction : new

///	function void post_randomize();
///    static uchar lastSubVec = 0;
///  	if(lastSubVec == 0 || lastSubVec == (CYC_VEC - 1)) begin
///      	lastSubVec = subVec;
///    	end
///    	else begin
///        lastSubVec++;
///      	subVec = lastSubVec;
///    	end	    
///	endfunction : post_randomize	
endclass : tr_rfm2dse

///---------------------------trsaction spa_ise ise_spa------------------------

class ise2spa_fu extends ovm_object;
  rand bit en, vec, wrEn[2];
  rand opcode_e op;
  rand cmp_opcode_e cop;
  rand uchar wrBk, wrAdr, wrGrp;
  rand rbk_sel_e bpSel[NUM_FU_RP];
  uchar fuId;
  
  constraint valid_opcodes{
    fu_cfg[fuId] ==  mac -> op inside {mac_ops};
    fu_cfg[fuId] ==  alu -> op inside {alu_ops};
    fu_cfg[fuId] ==  sfu -> op inside {sfu_ops};
  }

  constraint valid_vars{
    en dist {0:=1, 1:=9};
  	wrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
  	wrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
  	wrBk inside {[0:NUM_VRF_BKS-1]};
  }
    
  `ovm_object_utils_begin(ise2spa_fu)
    `ovm_field_int(en, OVM_ALL_ON)
    `ovm_field_int(vec, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_enum(cmp_opcode_e, cop, OVM_ALL_ON)
    `ovm_field_int(wrBk, OVM_ALL_ON)
    `ovm_field_int(wrAdr, OVM_ALL_ON)
    `ovm_field_int(wrGrp, OVM_ALL_ON)
    `ovm_field_sarray_enum(rbk_sel_e, bpSel, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "ise2spa_fu");
  	super.new(name);
	endfunction : new
endclass : ise2spa_fu

class tr_ise2spa extends ovm_sequence_item;   ///syn to EXE0 stage
  ise2spa_fu fu[NUM_FU];
  rand pr_merge_e prMerge;
  rand uchar subVec, vecMode, tid;
///  rand bit cancel[NUM_THREAD]; /// cancel is sync to vwb0 stage to fu & sfu
  rand uchar bpRfDSEwp;
  rand rbk_sel_e bpRfDSE;
  rand round_mode_t rndMode;
  rand uchar expMsk;
  rand bit noExp;
  
  `ovm_object_utils_begin(tr_ise2spa)
    `ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_enum(pr_merge_e, prMerge, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
///    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
    `ovm_field_int(bpRfDSEwp, OVM_ALL_ON)
    `ovm_field_int(noExp, OVM_ALL_ON)
    `ovm_field_int(expMsk, OVM_ALL_ON)
    `ovm_field_enum(rbk_sel_e, bpRfDSE, OVM_ALL_ON)
    `ovm_field_enum(round_mode_t, rndMode, OVM_ALL_ON)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
  	foreach(fu[i])
      if(fu[i].en)
        printer.print_object($psprintf("fu%0d", i), fu[i]);
	endfunction : do_print
    
  constraint dist_vars{
    subVec dist {0:=5, 1:=5};
    vecMode < CYC_VEC; ///inside {[1:CYC_VEC]};
///    foreach(cancel[i]) cancel[i] dist {0:=19, 1:=1};
    bpRfDSEwp < 2;
    bpRfDSE dist {selnull:=9, [selfu0:selfu0+NUM_FU]:=1};
  }
  
	function new (string name = "tr_ise2spa");
  	super.new(name);
    foreach(fu[i])
      fu[i] = new();
	endfunction : new

	function void post_randomize();
  	static uchar lastSubVec = 0, last_tid;
  	if(lastSubVec == 0 || lastSubVec == (CYC_VEC - 1)) begin
    	lastSubVec = subVec;
    	last_tid = tid;
  	end
  	else begin
      lastSubVec++;
    	subVec = lastSubVec;
    	tid = last_tid;
    end
    
    foreach(fu[i]) begin
      unit_typ_e t = fu_cfg[i];
      fu[i].fuId = i;
      assert(fu[i].randomize());
    end
	endfunction : post_randomize
  
endclass : tr_ise2spa

class tr_spa2ise extends ovm_sequence_item;
  ///syn to vwb0
  rand bit noFu[NUM_FU];
  rand bit exp;
  rand uchar tid;
  
  `ovm_object_utils_begin(tr_spa2ise)
    `ovm_field_sarray_int(noFu, OVM_ALL_ON)
    `ovm_field_int(exp, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2ise");
  	super.new(name);
  	noFu = '{default : 0};
  	exp = 0;
	endfunction : new
  
endclass : tr_spa2ise

///---------------------------trsaction spa_spu spu_spa------------------------

class spu2spa_fu extends ovm_object;
  rand bit emsk[NUM_SP];

  constraint dist_emsk{
    foreach(emsk[i])
      emsk[i] dist {0:=1, 1:=9};
  }
    
  `ovm_object_utils_begin(spu2spa_fu)
    `ovm_field_sarray_int(emsk, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "spu2spa_fu");
  	super.new(name);
	endfunction : new
  
endclass : spu2spa_fu

class tr_spu2spa extends ovm_sequence_item;
  spu2spa_fu fu[NUM_FU];
  
  `ovm_object_utils_begin(tr_spu2spa)
    `ovm_field_sarray_object(fu, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2spa");
  	super.new(name);
    foreach(fu[i])
      fu[i] = new();    
	endfunction : new

	function void post_randomize();
    foreach(fu[i])
      assert(fu[i].randomize());      
	endfunction : post_randomize
    
endclass : tr_spu2spa

class tr_spa2spu extends ovm_sequence_item;
  ///syn to cem0
  rand bit presCmp0[NUM_SP], presCmp1[NUM_SP], cancel;
  rand uchar tid;
///  rand uchar tid[NUM_FU], subVec[NUM_FU];
  
///  constraint valid_vars {
///    foreach(tid[i]) {
///      tid[i] < NUM_THREAD;
///      subVec[i] dist {0:=5, 1:=5};
///    }
///  }
  
  `ovm_object_utils_begin(tr_spa2spu)
    `ovm_field_sarray_int(presCmp0, OVM_ALL_ON)
    `ovm_field_sarray_int(presCmp1, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2spu");
  	super.new(name);
	endfunction : new

///	function void post_randomize();
///    static uchar lastSubVec[NUM_FU] = '{default:0};
///    foreach(subVec[i]) begin
///    	if(lastSubVec[i] == 0 || lastSubVec[i] == (CYC_VEC - 1)) begin
///      	lastSubVec[i] = subVec[i];
///    	end
///    	else begin
///        lastSubVec[i]++;
///      	subVec[i] = lastSubVec[i];
///    	end	    
///    end
///	endfunction : post_randomize
endclass : tr_spa2spu

///---------------------------trsaction ise_spu spu_ise------------------------

class tr_spu2ise extends ovm_sequence_item;
  rand bit brRsp, brTaken, missBr, mscExp, sclExp;
  rand uchar tidBr, tidSPU, tidSclExp;
  rand word op0;
  rand bit srReq, s2gp;
  rand opcode_e op;
  rand uchar srAdr, vecMode, vecModeSclExp;
  rand uint bpc;
  
  `ovm_object_utils_begin(tr_spu2ise)
    `ovm_field_int(brRsp, OVM_ALL_ON)
    `ovm_field_int(brTaken, OVM_ALL_ON)
    `ovm_field_int(missBr, OVM_ALL_ON)
    `ovm_field_int(bpc, OVM_ALL_ON)
    `ovm_field_int(mscExp, OVM_ALL_ON)
    `ovm_field_int(sclExp, OVM_ALL_ON)
    `ovm_field_int(tidBr, OVM_ALL_ON)
    `ovm_field_int(tidSPU, OVM_ALL_ON)
    `ovm_field_int(tidSclExp, OVM_ALL_ON)
    `ovm_field_int(srReq, OVM_ALL_ON)
    `ovm_field_int(s2gp, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(vecModeSclExp, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2ise");
  	super.new(name);
	endfunction : new
  
endclass : tr_spu2ise

class tr_ise2spu extends ovm_sequence_item;
  rand msc_opcode_e sop;
  rand msk_opcode_e mop;
  rand br_opcode_e bop;
  rand opcode_e op;
  
  rand uchar srAdr, /// subVec, vecMode, 
             subVecFu, vecModeFu, tidFu,
             subVecDSE, vecModeDSE, tidDSE,
             subVecSPU, vecModeSPU, tidSPU;
  rand bit brDep, ///brEnd,  ///signal the last subVec of a br
           brDepDSE,
           brDepSPA,
           brSrf,
           enFu[NUM_FU],
           vecFu[NUM_FU],
           enDSE,
           vecDSE,
           enSPU,
           brPred,
           wrEn;
  
  rand uchar srfWrBk, srfWrGrp, srfWrAdr;
  rand uchar prWrAdr0, prWrAdr1, ///fu pr write adr
             prWrAdr2; ///dse pr write adr
  rand uchar prRdAdr[NUM_FU],   ///fu pr read adr
             prRdAdrSPU,  ///spu pr read adr
             prRdAdrDSE;  ///dse pr read adr
  rand bit prInv[NUM_FU], prInvDSE, prInvSPU,
           prNMsk[NUM_FU], prNMskDSE, prNMskSPU;
  
  rand bit srRsp;
  rand word srRes;
  rand uchar rt, ss, vs;
  rand uint predPc;
  
  constraint valid_data{
///    tid < NUM_THREAD;
    brDep dist {0:=6, 1:=4};
    brDep -> brDepDSE || brDepSPA;
///    subVec dist {0:=5, 1:=5};
///    vecMode < CYC_VEC; ///inside {[1:CYC_VEC]};
    prRdAdrSPU == 0 -> brDep == 0;
    op inside {spu_ops, spu_com_ops};
    op != op_br -> sop == sop_p2n && mop == mop_nop && bop == bop_az;
    foreach(prRdAdr[i])
      prRdAdr[i] <= NUM_PR;
    prWrAdr0 <= NUM_PR;
    prWrAdr1 <= NUM_PR;
    prWrAdr0 <= NUM_PR;
    prWrAdr1 <= NUM_PR;
    prRdAdrSPU <= NUM_PR;
    prRdAdrDSE <= NUM_PR;
  	srfWrBk inside {[0:NUM_VRF_BKS-1]};
  	srfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
  	srfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
    solve prRdAdrSPU before brDep;
    solve op before sop, mop, bop;
  }
  
///  function void post_randomize();
///    static uchar last_cycs = 0, last_subs=0, lastSubVec = 0;
///  	if(lastSubVec == 0 || lastSubVec == (CYC_VEC - 1)) begin
///    	lastSubVec = subVec;
///  	end
///  	else begin
///      lastSubVec++;
///    	subVec = lastSubVec;
///    end    
///  endfunction
  
  `ovm_object_utils_begin(tr_ise2spu)
    `ovm_field_int(tidSPU, OVM_ALL_ON)
    `ovm_field_int(tidDSE, OVM_ALL_ON)
    `ovm_field_int(tidFu, OVM_ALL_ON)
    `ovm_field_int(wrEn, OVM_ALL_ON)
    `ovm_field_int(brPred, OVM_ALL_ON)
    `ovm_field_int(brDep, OVM_ALL_ON)
    `ovm_field_int(brDepDSE, OVM_ALL_ON)
    `ovm_field_int(brDepSPA, OVM_ALL_ON)
    `ovm_field_int(brSrf, OVM_ALL_ON)
    `ovm_field_int(predPc, OVM_ALL_ON)
///    `ovm_field_int(start, OVM_ALL_ON)start,   ///signal spu normal op to start
///    `ovm_field_int(subVec, OVM_ALL_ON)
///    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(subVecFu, OVM_ALL_ON)
    `ovm_field_int(vecModeFu, OVM_ALL_ON)
    `ovm_field_int(subVecDSE, OVM_ALL_ON)
    `ovm_field_int(vecModeDSE, OVM_ALL_ON)
    `ovm_field_int(subVecSPU, OVM_ALL_ON)
    `ovm_field_int(vecModeSPU, OVM_ALL_ON)
    `ovm_field_int(prWrAdr0, OVM_ALL_ON)
    `ovm_field_int(prWrAdr1, OVM_ALL_ON)
    `ovm_field_int(prWrAdr2, OVM_ALL_ON)
    `ovm_field_sarray_int(prRdAdr, OVM_ALL_ON)
    `ovm_field_int(prRdAdrSPU, OVM_ALL_ON)
    `ovm_field_int(prRdAdrDSE, OVM_ALL_ON)
    `ovm_field_sarray_int(prInv, OVM_ALL_ON)
    `ovm_field_int(prInvDSE, OVM_ALL_ON)
    `ovm_field_int(prInvSPU, OVM_ALL_ON)
    `ovm_field_sarray_int(prNMsk, OVM_ALL_ON)
    `ovm_field_int(prNMskDSE, OVM_ALL_ON)
    `ovm_field_int(prNMskSPU, OVM_ALL_ON)
    `ovm_field_enum(msc_opcode_e, sop, OVM_ALL_ON)
    `ovm_field_enum(msk_opcode_e, mop, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_enum(br_opcode_e, bop, OVM_ALL_ON)
    `ovm_field_int(srfWrBk, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(srfWrGrp, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(srfWrAdr, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_int(vecDSE, OVM_ALL_ON)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_sarray_int(enFu, OVM_ALL_ON)
    `ovm_field_sarray_int(vecFu, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_int(srRsp, OVM_ALL_ON)
    `ovm_field_int(srRes, OVM_ALL_ON)
    `ovm_field_int(rt, OVM_ALL_ON)
    `ovm_field_int(ss, OVM_ALL_ON)
    `ovm_field_int(vs, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2spu");
  	super.new(name);
	endfunction : new
  
endclass : tr_ise2spu

///---------------------------trsaction dse_spu spu_dse------------------------

class tr_spu2dse extends ovm_sequence_item;
  rand bit emsk[NUM_SP];
  rand word op0;
  rand bit srReq, expFu, missBr, expMSC, s2gp;
  rand opcode_e op;
  rand uchar tid, srAdr, vecMode, tidExpFu, tidExpMSC, vecModeExpFu;
  
  `ovm_object_utils_begin(tr_spu2dse)
    `ovm_field_sarray_int(emsk, OVM_ALL_ON)
    `ovm_field_int(op0, OVM_ALL_ON)
    `ovm_field_int(srReq, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(tidExpFu, OVM_ALL_ON)
    `ovm_field_int(tidExpMSC, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_int(expFu, OVM_ALL_ON)
    `ovm_field_int(missBr, OVM_ALL_ON)
    `ovm_field_int(expMSC, OVM_ALL_ON)
    `ovm_field_int(s2gp, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(vecModeExpFu, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2dse");
  	super.new(name);
	endfunction : new
  
endclass : tr_spu2dse

class tr_dse2spu extends ovm_sequence_item;
  rand uchar tid, tidCancel;
  rand bit pres[NUM_SP], wrEn, rsp, cancel;
  rand word srRes;
  
  constraint valid_vars {
    tid < NUM_THREAD;
  }
  
  `ovm_object_utils_begin(tr_dse2spu)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(tidCancel, OVM_ALL_ON)
    `ovm_field_int(cancel, OVM_ALL_ON)
    `ovm_field_int(wrEn, OVM_ALL_ON)
    `ovm_field_sarray_int(pres, OVM_ALL_ON)
///    `ovm_field_sarray_int(tmrfRes, OVM_ALL_ON)
    `ovm_field_int(srRes, OVM_ALL_ON)
    `ovm_field_int(rsp, OVM_ALL_ON)
///    `ovm_field_int(tmrf, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2spu");
  	super.new(name);
	endfunction : new
  
endclass : tr_dse2spu

///---------------------------trsaction ife_ise ise_ife------------------------

class tr_ife2ise extends ovm_sequence_item;
  rand bit instEn;
  rand uchar tid;
  rand inst_fg_c fetchGrp;
  
  `ovm_object_utils_begin(tr_ife2ise)
    `ovm_field_int(instEn, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_object(fetchGrp, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_ife2ise");
  	super.new(name);
  	fetchGrp = new();
	endfunction : new
  
endclass : tr_ife2ise

class tr_ise2ife extends ovm_sequence_item;
  rand bit fetchReq;
  rand uchar tid;
  rand uint pc;
  rand bit cancel[NUM_THREAD];
  
  `ovm_object_utils_begin(tr_ise2ife)
    `ovm_field_int(fetchReq, OVM_ALL_ON)
    `ovm_field_sarray_int(cancel, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON + OVM_DEC)
    `ovm_field_int(pc, OVM_ALL_ON + OVM_HEX)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2ife");
  	super.new(name);
	endfunction : new
  
endclass : tr_ise2ife

///---------------------------trsaction dse_spa spa_dse------------------------

class tr_spa2dse extends ovm_sequence_item;
  rand bit cancel;
  rand uchar tid;
    
  `ovm_object_utils_begin(tr_spa2dse)
    `ovm_field_int(cancel, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2dse");
  	super.new(name);
	endfunction : new
  
endclass : tr_spa2dse

class tr_dse2spa extends ovm_sequence_item;
  
  `ovm_object_utils_begin(tr_dse2spa)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2spa");
  	super.new(name);
	endfunction : new
  
endclass : tr_dse2spa

///---------------------------trsaction dse_ise ise_dse------------------------

class tr_ise2dse extends ovm_sequence_item;
  rand uchar wrGrp, wrAdr, wrBk,
             uaWrGrp, uaWrAdr, uaWrBk, tid;
  rand bit priv, vec, en, wr, uaWrEn, nonBlock, noExt, sendRotRight;
  rand opcode_e op;
  rand uchar vecMode, subVec, mrfAdr;
  rand update_adr_t ua;
  rand access_typ_t at;
  
  `ovm_object_utils_begin(tr_ise2dse)
    `ovm_field_int(wr, OVM_ALL_ON)
    `ovm_field_int(wrBk, OVM_ALL_ON)
    `ovm_field_int(wrAdr, OVM_ALL_ON)
    `ovm_field_int(wrGrp, OVM_ALL_ON)
    `ovm_field_int(uaWrBk, OVM_ALL_ON)
    `ovm_field_int(uaWrAdr, OVM_ALL_ON)
    `ovm_field_int(uaWrGrp, OVM_ALL_ON)
    `ovm_field_int(mrfAdr, OVM_ALL_ON)
    `ovm_field_int(priv, OVM_ALL_ON)
    `ovm_field_int(en, OVM_ALL_ON)
    `ovm_field_int(sendRotRight, OVM_ALL_ON)
    `ovm_field_int(noExt, OVM_ALL_ON)
///    `ovm_field_int(burst, OVM_ALL_ON)
    `ovm_field_int(nonBlock, OVM_ALL_ON)
    `ovm_field_int(uaWrEn, OVM_ALL_ON)
///    `ovm_field_int(updatePr, OVM_ALL_ON)
    `ovm_field_int(vec, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_enum(update_adr_t, ua, OVM_ALL_ON)
    `ovm_field_enum(access_typ_t, at, OVM_ALL_ON)
  `ovm_object_utils_end
  
  constraint valid_vars{
    en dist {0:=4, 1:=6};
    op inside {dse_ops};
    vecMode < CYC_VEC;
  }
  
	function new (string name = "tr_ise2dse");
  	super.new(name);
	endfunction : new

endclass : tr_ise2dse

class tr_dse2ise extends ovm_sequence_item;
  /// sync to dem0 stage
  rand bit rsp,     ///respond
           extLd,     ///this req generate a external load transaction
           exp,     ///the whole req has exception
           scl,
           noExtLd, noExtSt,
           rlsPipLd, rlsPipSt;
  rand uchar tid, tidNoExt, vecMode, pendExLoad, pendExStore, pendSMsg, ldq, stq;
  rand cause_dse_t cause;
  
  constraint dist_var {
    extLd dist {0:=19, 1:=1};
  }
  
  constraint valid_var {
    exp -> rsp;
    extLd -> rsp;
  }
  
  `ovm_object_utils_begin(tr_dse2ise)
    `ovm_field_int(extLd, OVM_ALL_ON)
    `ovm_field_int(noExtLd, OVM_ALL_ON)
    `ovm_field_int(noExtSt, OVM_ALL_ON)
    `ovm_field_int(ldq, OVM_ALL_ON)
    `ovm_field_int(stq, OVM_ALL_ON)
    `ovm_field_int(rlsPipSt, OVM_ALL_ON)
    `ovm_field_int(rlsPipLd, OVM_ALL_ON)
    `ovm_field_int(tidNoExt, OVM_ALL_ON)
    `ovm_field_int(rsp, OVM_ALL_ON)
    `ovm_field_int(exp, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(scl, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
    `ovm_field_int(pendExLoad, OVM_ALL_ON)
    `ovm_field_int(pendExStore, OVM_ALL_ON)
    `ovm_field_int(pendSMsg, OVM_ALL_ON)
///    `ovm_field_int(pendMemAcc, OVM_ALL_ON)
    `ovm_field_enum(cause_dse_t, cause, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2ise");
  	super.new(name);
	endfunction : new
  
endclass : tr_dse2ise

///---------------------------trsaction spu_tlb tlb_spu------------------------

class tr_spu2tlb extends ovm_sequence_item;
  rand word op0;
  rand bit req, s2gp;
  rand opcode_e op;
  rand uchar tid, srAdr;
  
  constraint valid_var{
    op inside {tlb_ops, op_gp2s, op_s2gp};
  }
  
  `ovm_object_utils_begin(tr_spu2tlb)
    `ovm_field_int(op0, OVM_ALL_ON)
    `ovm_field_int(req, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(s2gp, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2tlb");
  	super.new(name);
	endfunction : new
  
endclass : tr_spu2tlb

class tr_tlb2spu extends ovm_sequence_item;
  rand bit rsp;
  rand word res;
  rand uchar tid;
  
  `ovm_object_utils_begin(tr_tlb2spu)
    `ovm_field_int(rsp, OVM_ALL_ON)
    `ovm_field_int(res, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_tlb2spu");
  	super.new(name);
	endfunction : new  
endclass : tr_tlb2spu

///---------------------------trsaction dse_tlb tlb_dse------------------------

class tr_dse2tlb extends ovm_sequence_item;
  rand word vAdr;
  rand opcode_e op;
  rand uchar tid;
  rand bit req, k;
  
  `ovm_object_utils_begin(tr_dse2tlb)
    `ovm_field_int(vAdr, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(req, OVM_ALL_ON)
    `ovm_field_int(k, OVM_ALL_ON)
  `ovm_object_utils_end  

	function new (string name = "tr_dse2tlb");
  	super.new(name);
	endfunction : new  
endclass : tr_dse2tlb

class tr_tlb2dse extends ovm_sequence_item;
  rand word pfn;
  rand bit endian, exp;
  rand bit writeAlloc, writeThru, coherency, cached;
  rand uchar eobit;  /// evenoddbit
  rand cause_dse_t cause;
  
  `ovm_object_utils_begin(tr_tlb2dse)
    `ovm_field_int(pfn, OVM_ALL_ON);
    `ovm_field_int(exp, OVM_ALL_ON);
    `ovm_field_int(endian, OVM_ALL_ON);
    `ovm_field_int(writeAlloc, OVM_ALL_ON);
    `ovm_field_int(writeThru, OVM_ALL_ON);
    `ovm_field_int(coherency, OVM_ALL_ON);
    `ovm_field_int(cached, OVM_ALL_ON);
    `ovm_field_int(eobit, OVM_ALL_ON);
    `ovm_field_enum(cause_dse_t, cause, OVM_ALL_ON)
  `ovm_object_utils_end  

	function new (string name = "tr_tlb2dse");
  	super.new(name);
	endfunction : new
endclass : tr_tlb2dse  

///---------------------------trsaction ife_tlb tlb_ife------------------------

class tr_ife2tlb extends ovm_sequence_item;
  rand word vAdr;
  rand bit req;
  rand uchar tid;
  
  `ovm_object_utils_begin(tr_ife2tlb)
    `ovm_field_int(vAdr, OVM_ALL_ON);
    `ovm_field_int(req, OVM_ALL_ON);
    `ovm_field_int(tid, OVM_ALL_ON);
  `ovm_object_utils_end  

	function new (string name = "tr_ife2tlb");
  	super.new(name);
	endfunction : new
endclass : tr_ife2tlb  

class tr_tlb2ife extends ovm_sequence_item;
  rand word pfn;
  rand bit rsp, exp, k, ex;
  rand uchar tid, eobit;
  
  `ovm_object_utils_begin(tr_tlb2ife)
    `ovm_field_int(pfn, OVM_ALL_ON)
    `ovm_field_int(rsp, OVM_ALL_ON)
    `ovm_field_int(exp, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(eobit, OVM_ALL_ON)
    `ovm_field_int(k, OVM_ALL_ON)
    `ovm_field_int(ex, OVM_ALL_ON)
  `ovm_object_utils_end  

	function new (string name = "tr_tlb2ife");
  	super.new(name);
	endfunction : new
endclass : tr_tlb2ife

///---------------------------trsaction dse_eif eif_dse------------------------
class tr_dse2eif extends ovm_sequence_item;
  rand bit req, cacheFlush, cacheFill, sgl,
           last, endian, allocFail, queryNoHit,
           coherency, priv, uncachable;
  rand opcode_e op;
  rand uchar id, subVec, mrfAdr;
  rand exadr_t exAdr;
  rand word data[NUM_SP];
  rand bit[WORD_BYTES - 1:0] byteEn[NUM_SP];
  rand cache_state_t queryRes, state;
  
  `ovm_object_utils_begin(tr_dse2eif)
    `ovm_field_int(req, OVM_ALL_ON)
    `ovm_field_int(sgl, OVM_ALL_ON)
    `ovm_field_int(last, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(id, OVM_ALL_ON)
    `ovm_field_int(mrfAdr, OVM_ALL_ON)
    `ovm_field_int(allocFail, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(cacheFlush, OVM_ALL_ON)
    `ovm_field_int(cacheFill, OVM_ALL_ON)
    `ovm_field_int(exAdr, OVM_ALL_ON)
    `ovm_field_int(coherency, OVM_ALL_ON)
    `ovm_field_int(priv, OVM_ALL_ON)
    `ovm_field_int(uncachable, OVM_ALL_ON)
    `ovm_field_sarray_int(data, OVM_ALL_ON)
    `ovm_field_sarray_int(byteEn, OVM_ALL_ON + OVM_BIN)
    `ovm_field_enum(cache_state_t, queryRes, OVM_ALL_ON)
    `ovm_field_enum(cache_state_t, state, OVM_ALL_ON)
    `ovm_field_int(queryNoHit, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_dse2eif");
  	super.new(name);
	endfunction : new
endclass : tr_dse2eif

class tr_eif2dse extends ovm_sequence_item;
  rand bit loadRsp, storeRsp, last, noVecSt,
           rd, wr, alloc, noSglSt, noLd, endian,
           queryCacheState, queryAndUpdate;
  rand uchar id, subVec;
  rand exadr_t exAdr;
  rand cache_state_t state;
  
  ///those 2 comes late
  rand bit[WORD_BYTES - 1:0] byteEn[NUM_SP];
  rand word data[NUM_SP];
  
  `ovm_object_utils_begin(tr_eif2dse)
    `ovm_field_int(loadRsp, OVM_ALL_ON)
    `ovm_field_int(storeRsp, OVM_ALL_ON)
    `ovm_field_int(last, OVM_ALL_ON)
    `ovm_field_int(rd, OVM_ALL_ON)
    `ovm_field_int(wr, OVM_ALL_ON)
    `ovm_field_int(alloc, OVM_ALL_ON)
    `ovm_field_int(noVecSt, OVM_ALL_ON)
    `ovm_field_int(noSglSt, OVM_ALL_ON)
    `ovm_field_int(noLd, OVM_ALL_ON)
    `ovm_field_int(endian, OVM_ALL_ON)
    `ovm_field_int(id, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(exAdr, OVM_ALL_ON)
    `ovm_field_sarray_int(data, OVM_ALL_ON)
    `ovm_field_enum(cache_state_t, state, OVM_ALL_ON)
    `ovm_field_sarray_int(byteEn, OVM_ALL_ON)
    `ovm_field_int(queryCacheState, OVM_ALL_ON)
    `ovm_field_int(queryAndUpdate, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_eif2dse");
  	super.new(name);
	endfunction : new
endclass : tr_eif2dse

///---------------------------trsaction ise_eif eif_ise------------------------
class tr_ise2eif extends ovm_sequence_item;
  rand bit rsp, supClear, issueLd, issueSt, issueFMsg, issueTMsg;
    
  `ovm_object_utils_begin(tr_ise2eif)
    `ovm_field_int(rsp, OVM_ALL_ON)
    `ovm_field_int(supClear, OVM_ALL_ON)
    `ovm_field_int(issueLd, OVM_ALL_ON)
    `ovm_field_int(issueSt, OVM_ALL_ON)
    `ovm_field_int(issueFMsg, OVM_ALL_ON)
    `ovm_field_int(issueTMsg, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_ise2eif");
  	super.new(name);
	endfunction : new
endclass : tr_ise2eif

class tr_eif2ise extends ovm_sequence_item;
  rand uchar noLd, noSt, noTMsg, noFMsg, vecCnt, sclCnt;
  rand bit reqCleanUp, reqNo, noSMsg, mrfLocked[NUM_THREAD];
  rand bit[NUM_FIFO - 1 : 0] msgRdy[NUM_THREAD];
  rand bit supRdy;
  
  constraint valid_var {
    noLd <= CYC_VEC;
    noSt <= CYC_VEC;
    noTMsg <= CYC_VEC;
    noFMsg <= CYC_VEC;
  }
    
  `ovm_object_utils_begin(tr_eif2ise)
    `ovm_field_int(noLd, OVM_ALL_ON)
    `ovm_field_int(noSt, OVM_ALL_ON)
    `ovm_field_int(noTMsg, OVM_ALL_ON)
    `ovm_field_int(noFMsg, OVM_ALL_ON)
    `ovm_field_int(vecCnt, OVM_ALL_ON)
    `ovm_field_int(sclCnt, OVM_ALL_ON)
    `ovm_field_int(reqCleanUp, OVM_ALL_ON)
    `ovm_field_int(reqNo, OVM_ALL_ON)
    `ovm_field_int(noSMsg, OVM_ALL_ON)
    `ovm_field_int(supRdy, OVM_ALL_ON)
    `ovm_field_sarray_int(msgRdy, OVM_ALL_ON)
    `ovm_field_sarray_int(mrfLocked, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_eif2ise");
  	super.new(name);
	endfunction : new
endclass : tr_eif2ise

///---------------------------trsaction spu_eif eif_spu------------------------
class tr_spu2eif extends ovm_sequence_item;
  rand bit srReq, s2gp;
  rand uchar srAdr, tid;
  rand opcode_e op;
  rand uchar rt, ss, vs;
  
  `ovm_object_utils_begin(tr_spu2eif)
    `ovm_field_int(srReq, OVM_ALL_ON)
    `ovm_field_int(s2gp, OVM_ALL_ON)
    `ovm_field_int(srAdr, OVM_ALL_ON)
    `ovm_field_int(tid, OVM_ALL_ON)
    `ovm_field_int(rt, OVM_ALL_ON)
    `ovm_field_int(ss, OVM_ALL_ON)
    `ovm_field_int(vs, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_spu2eif");
  	super.new(name);
	endfunction : new
endclass : tr_spu2eif

class tr_eif2spu extends ovm_sequence_item;
  rand word srRes;
  rand bit srRsp;
  
  `ovm_object_utils_begin(tr_eif2spu)
    `ovm_field_int(srRes, OVM_ALL_ON)
    `ovm_field_int(srRsp, OVM_ALL_ON)
  `ovm_object_utils_end

	function new (string name = "tr_eif2spu");
  	super.new(name);
	endfunction : new
endclass : tr_eif2spu