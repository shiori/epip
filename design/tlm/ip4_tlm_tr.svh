typedef class inst_fg_c;    // endclass

///---------------------------trsaction ise_rfm rfm_ise------------------------
class ise2rfm_fu extends ovm_object;
  rand rbk_sel_e rd_bk[num_fu_rp];
  rand word imm;
  rand bit en;
  
  constraint valid_var {
    en dist {0:=1, 1:=9};
	  foreach(rd_bk[i])
	    rd_bk[i] inside {[selv0:selv_e], [sels0:sels_e], [seli0:seli_e], selz, selii};
  }
  `ovm_object_utils_begin(ise2rfm_fu)
    `ovm_field_sarray_enum(rbk_sel_e, rd_bk, OVM_ALL_ON)
	  `ovm_field_int(imm, OVM_ALL_ON)    
	  `ovm_field_int(en, OVM_ALL_ON)    
  `ovm_object_utils_end

	function new (string name = "ise2rfm_fu");
		super.new(name);
	endfunction : new  
endclass : ise2rfm_fu

class tr_ise2rfm extends ovm_sequence_item;
	rand uchar VrfRdGrp[NUM_VRF_BKS], SrfRdGrp[NUM_SRF_BKS],
	           VrfRdAdr[NUM_VRF_BKS], SrfRdAdr[NUM_SRF_BKS];
	
	rand ise2rfm_fu fu[NUM_FU];
	rand rbk_sel_e dse_rd_bk[3], spu_rd_bk[2];
	rand bit vec_end, scl_end, start;
	rand word bpImm[NUM_BP_IMM], dse_imm, spu_imm;
	rand bit dse_en, spu_en;
	rand uchar Cyc;
	
	constraint valid_var {
	  Cyc < CYC_VEC;
		foreach(VrfRdGrp[i]) {
			VrfRdGrp[i] inside {[0:NUM_PHY_VRF_GRP-1]};
			VrfRdAdr[i] inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
    }
		foreach(SrfRdGrp[i]) {
			SrfRdGrp[i] inside {[0:NUM_PHY_SRF_GRP-1]};
			SrfRdAdr[i] inside {[0:NUM_PRF_P_GRP/NUM_SRF_BKS-1]};
    }
    dse_rd_bk[0] inside {[selv0:selv_e], [sels0:sels_e], [seli0:seli_e], selz};
    dse_rd_bk[1] inside {[sels0:sels_e], [seli0:seli_e], selz};
    dse_rd_bk[2] inside {[seli0:seli_e], selz};
	  foreach(spu_rd_bk[i])
	    spu_rd_bk[i] inside {[sels0:sels_e], [seli0:seli_e], selz};
	}
	
	constraint dist_var {
		dse_en dist {0:=6, 1:=4};
		spu_en dist {0:=6, 1:=4};
	}
    
	function void post_randomize();
		foreach(fu[i])
		  assert(fu[i].randomize());
  endfunction
	
	`ovm_object_utils_begin(tr_ise2rfm)
		`ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
		`ovm_field_int(spu_en, OVM_ALL_ON)
		`ovm_field_int(dse_en, OVM_ALL_ON)
	  `ovm_field_int(vec_end, OVM_ALL_ON)
		`ovm_field_int(scl_end, OVM_ALL_ON)
		`ovm_field_int(start, OVM_ALL_ON)
		`ovm_field_int(Cyc, OVM_ALL_ON)
		`ovm_field_sarray_int(VrfRdGrp, OVM_ALL_ON + OVM_DEC)
		`ovm_field_sarray_int(VrfRdAdr, OVM_ALL_ON + OVM_DEC)
		`ovm_field_sarray_int(SrfRdGrp, OVM_ALL_ON + OVM_DEC)
		`ovm_field_sarray_int(SrfRdAdr, OVM_ALL_ON + OVM_DEC)
		`ovm_field_sarray_enum(rbk_sel_e, dse_rd_bk, OVM_ALL_ON)
		`ovm_field_sarray_enum(rbk_sel_e, spu_rd_bk, OVM_ALL_ON)
		`ovm_field_sarray_int(bpImm, OVM_ALL_ON)
		`ovm_field_int(dse_imm, OVM_ALL_ON)
		`ovm_field_int(spu_imm, OVM_ALL_ON)
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
	rand bit wrEn;///, sel_sfu;
	rand word res; ///BRU ScalarP use port4
	rand uchar srfWrBk, srfWrGrp, srfWrAdr, 
	           srf_wr_dsel; ///select which res from dual res
///	rand bit sel_dwbp; ///unit_inst_e
	
	///wrEn signal is given one cycle before writeback
	
	constraint valid_wen {
		wrEn dist {0:=1, 1:=9};
///		sel_vsbp dist {0:=9, 1:=1};
///		sel_dwbp dist {0:=9, 1:=1};
	}
	
	constraint valid_spu {
///		sel_vsbp dist {spu0:=20, mac0:=2, alu0:=2, dse0:=5, sfu0:=1};
		srfWrBk inside {[0:NUM_VRF_BKS-1]};
		srfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
		srfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
		srf_wr_dsel < 2;
	}
		
	`ovm_object_utils_begin(tr_spu2rfm)
		`ovm_field_int(wrEn, OVM_ALL_ON)
		`ovm_field_int(res, OVM_ALL_ON)
///		`ovm_field_int(sel_dwbp, OVM_ALL_ON)
///		`ovm_field_int(sel_vsbp, OVM_ALL_ON)
///		`ovm_field_enum(unit_inst_e, sel_vsbp, OVM_ALL_ON)
		`ovm_field_int(srf_wr_dsel, OVM_ALL_ON)
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
  rfm2spa_rp rp[num_fu_rp];
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
  rand bit dw, wrEn[NUM_SP];
  rand uchar vrfWrGrp, vrfWrAdr, vrfWrBk, subVec, TId;
  rand uchar expFlag[NUM_SP];
  rand bit en;   ///used only for printing
  
	constraint valid_vars{
		vrfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
		vrfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
		vrfWrBk inside {[0:NUM_VRF_BKS-1]};
		subVec dist {0:=5, 1:=5};
		TId < NUM_THREAD;
		foreach(wrEn[i])
			wrEn[i] dist {0:=1, 1:=9};				
	}
	  
  `ovm_object_utils_begin(spa2rfm_fu)
    `ovm_field_sarray_int(res0, OVM_ALL_ON)
    `ovm_field_sarray_int(res1, OVM_ALL_ON)
///    `ovm_field_int(res_vsbp, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_int(dw, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(vrfWrGrp, OVM_ALL_ON)
    `ovm_field_int(vrfWrAdr, OVM_ALL_ON)
    `ovm_field_int(vrfWrBk, OVM_ALL_ON)
    `ovm_field_sarray_int(expFlag, OVM_ALL_ON)
    `ovm_field_int(en, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_object_utils_end
  
	function new (string name = "spa2rfm_fu");
		super.new(name);
	endfunction : new
endclass : spa2rfm_fu

class tr_spa2rfm extends ovm_sequence_item;
	spa2rfm_fu fu[NUM_FU];
	
	function void post_randomize();
	  static uchar last_subv[NUM_FU] = '{default:0};
	  foreach(fu[i]) begin
	    assert(fu[i].randomize());
  		if(last_subv[i] == 0 || last_subv[i] == (CYC_VEC - 1)) begin
  			last_subv[i] = fu[i].subVec;
  		end
  		else begin
  		  last_subv[i]++;
  			fu[i].subVec = last_subv[i];
  		end	    
	  end
	endfunction : post_randomize
	
	`ovm_object_utils_begin(tr_spa2rfm)
		`ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
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
	rand word res[NUM_SP], UARes[NUM_SP];
	rand bit wrEn[NUM_SP], srfWr, ua_wr;
	rand uchar wrGrp, wrAdr, wrBk, 
	           UAWrGrp, UAWrAdr, UAWrBk, 
	           subVec;
	
	constraint valid_dse{
		foreach(wrEn[i])
			wrEn[i] dist {0:=1, 1:=9};
		wrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
		wrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
		wrBk inside {[0:NUM_VRF_BKS-1]};
		subVec dist {0:=5, 1:=5};
		srfWr dist {0:=9, 1:=1};
	}

	function void post_randomize();
		static uchar last_subv = 0;
		if(last_subv == 0 || last_subv == (CYC_VEC - 1)) begin
			last_subv = subVec;
		end
		else begin
		  last_subv++;
			subVec = last_subv;
		end
	endfunction : post_randomize
		
	`ovm_object_utils_begin(tr_dse2rfm)
		`ovm_field_sarray_int(res, OVM_ALL_ON)
		`ovm_field_sarray_int(wrEn, OVM_ALL_ON)
		`ovm_field_sarray_int(UARes, OVM_ALL_ON)
		`ovm_field_int(wrGrp, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(wrAdr, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(wrBk, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(UAWrGrp, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(UAWrAdr, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(UAWrBk, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(subVec, OVM_ALL_ON)
		`ovm_field_int(srfWr, OVM_ALL_ON)
		`ovm_field_int(ua_wr, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2rfm");
		super.new(name);
	endfunction : new
	
endclass : tr_dse2rfm

class tr_rfm2dse extends ovm_sequence_item;
	rand word base[NUM_SP], op1[NUM_SP], op2;
	rand uchar subVec;
	
	constraint valid_vars{
	  subVec dist {0:=5, 1:=5};
	}
	
	`ovm_object_utils_begin(tr_rfm2dse)
		`ovm_field_sarray_int(base, OVM_ALL_ON)
		`ovm_field_sarray_int(op1, OVM_ALL_ON)
		`ovm_field_int(op2, OVM_ALL_ON)
		`ovm_field_int(subVec, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_rfm2dse");
		super.new(name);
	endfunction : new

	function void post_randomize();
	  static uchar last_subv = 0;
		if(last_subv == 0 || last_subv == (CYC_VEC - 1)) begin
  			last_subv = subVec;
  		end
  		else begin
  		  last_subv++;
  			subVec = last_subv;
  		end	    
	endfunction : post_randomize	
endclass : tr_rfm2dse

///---------------------------trsaction spa_ise ise_spa------------------------

class ise2spa_fu extends ovm_object;
  rand bit en;
  rand opcode_e op;
  rand cmp_opcode_e cop;
  rand uchar vrfWrBk, vrfWrAdr, vrfWrGrp;
  rand rbk_sel_e bp_sel[num_fu_rp];
  uchar fu_id;
  
  constraint valid_opcodes{
    fu_cfg[fu_id] ==  mac -> op inside {mac_ops};
    fu_cfg[fu_id] ==  alu -> op inside {alu_ops};
    fu_cfg[fu_id] ==  sfu -> op inside {sfu_ops};
  }

  constraint valid_vars{
    en dist {0:=1, 1:=9};
		vrfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
		vrfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
		vrfWrBk inside {[0:NUM_VRF_BKS-1]};
  }
    
  `ovm_object_utils_begin(ise2spa_fu)
    `ovm_field_int(en, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_enum(cmp_opcode_e, cop, OVM_ALL_ON)
	  `ovm_field_int(vrfWrBk, OVM_ALL_ON)
	  `ovm_field_int(vrfWrAdr, OVM_ALL_ON)
	  `ovm_field_int(vrfWrGrp, OVM_ALL_ON)
	  `ovm_field_sarray_enum(rbk_sel_e, bp_sel, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "ise2spa_fu");
		super.new(name);
	endfunction : new
	
///	function void pre_randomize();
///	  op.rand_mode(0);
///  endfunction
endclass : ise2spa_fu

class tr_ise2spa extends ovm_sequence_item;   ///syn to EXE0 stage
  ise2spa_fu fu[NUM_FU];
  rand pr_merge_e fmerge;
  rand uchar subVec, vecMode, TId;
  rand bit cancel[NUM_THREAD]; /// cancel is sync to vwb0 stage to fu & sfu
  rand uchar bp_rf_dse_wp;
  rand rbk_sel_e bp_rf_dse;
  
	`ovm_object_utils_begin(tr_ise2spa)
	  `ovm_field_sarray_object(fu, OVM_ALL_ON + OVM_NOPRINT)
	  `ovm_field_enum(pr_merge_e, fmerge, OVM_ALL_ON)
	  `ovm_field_int(subVec, OVM_ALL_ON)
	  `ovm_field_int(vecMode, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_sarray_int(cancel, OVM_ALL_ON)
	  `ovm_field_int(bp_rf_dse_wp, OVM_ALL_ON)
	  `ovm_field_enum(rbk_sel_e, bp_rf_dse, OVM_ALL_ON)
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
    foreach(cancel[i]) cancel[i] dist {0:=19, 1:=1};
    bp_rf_dse_wp < 2;
    bp_rf_dse dist {selnull:=9, [selfu0:selfu0+NUM_FU]:=1};
  }
  
	function new (string name = "tr_ise2spa");
		super.new(name);
	  foreach(fu[i])
	    fu[i] = new();
	endfunction : new

	function void post_randomize();
		static uchar last_subv = 0, last_tid;
		if(last_subv == 0 || last_subv == (CYC_VEC - 1)) begin
			last_subv = subVec;
			last_tid = TId;
		end
		else begin
		  last_subv++;
			subVec = last_subv;
			TId = last_tid;
	  end
	  
	  foreach(fu[i]) begin
	    unit_typ_e t = fu_cfg[i];
	    fu[i].fu_id = i;
	    assert(fu[i].randomize());
	  end
	endfunction : post_randomize
	
endclass : tr_ise2spa

class tr_spa2ise extends ovm_sequence_item;
  ///syn to vwb0
  rand bit NoFu[NUM_FU];
  rand bit exp;
  rand uchar TId;
  
	`ovm_object_utils_begin(tr_spa2ise)
	  `ovm_field_sarray_int(NoFu, OVM_ALL_ON)
	  `ovm_field_int(exp, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2ise");
		super.new(name);
		NoFu = '{default : 0};
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
///  rand word res;     ///spu res bypass
  rand uchar exe_mode;
  
	`ovm_object_utils_begin(tr_spu2spa)
	  `ovm_field_sarray_object(fu, OVM_ALL_ON)
///	  `ovm_field_int(res, OVM_ALL_ON)
	  `ovm_field_int(exe_mode, OVM_ALL_ON)
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
  rand bit pres_cmp0[NUM_SP], pres_cmp1[NUM_SP];
///  rand uchar TId[NUM_FU], subVec[NUM_FU];
  
///  constraint valid_vars {
///    foreach(TId[i]) {
///      TId[i] < NUM_THREAD;
///      subVec[i] dist {0:=5, 1:=5};
///    }
///  }
  
	`ovm_object_utils_begin(tr_spa2spu)
	  `ovm_field_sarray_int(pres_cmp0, OVM_ALL_ON)
	  `ovm_field_sarray_int(pres_cmp1, OVM_ALL_ON)
///	  `ovm_field_sarray_int(TId, OVM_ALL_ON)
///	  `ovm_field_sarray_int(subVec, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2spu");
		super.new(name);
	endfunction : new

///	function void post_randomize();
///	  static uchar last_subv[NUM_FU] = '{default:0};
///	  foreach(subVec[i]) begin
///  		if(last_subv[i] == 0 || last_subv[i] == (CYC_VEC - 1)) begin
///  			last_subv[i] = subVec[i];
///  		end
///  		else begin
///  		  last_subv[i]++;
///  			subVec[i] = last_subv[i];
///  		end	    
///	  end
///	endfunction : post_randomize
endclass : tr_spa2spu

///---------------------------trsaction ise_spu spu_ise------------------------

class tr_spu2ise extends ovm_sequence_item;
  rand bit br_rsp, br_taken, msc_top_chg;
  rand uchar TId;
  
	`ovm_object_utils_begin(tr_spu2ise)
	  `ovm_field_int(br_rsp, OVM_ALL_ON)
	  `ovm_field_int(br_taken, OVM_ALL_ON)
	  `ovm_field_int(msc_top_chg, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2ise");
		super.new(name);
	endfunction : new
	
endclass : tr_spu2ise

class tr_ise2spu extends ovm_sequence_item;
  rand msc_opcode_e sop;
  rand msk_opcode_e mop;
  rand br_opcode_e bop;
///  rand cmp_opcode_e cop;
///  rand pr_merge_e fmerge;  
  rand opcode_e op;
  
  rand uchar TId, subVec, vecMode;
  rand bit start,   ///signal spu normal op to start
           br_end,  ///signal the last subVec of a br
           br_dep,
           br_dep_dse,
           br_dep_spa,
           enFu[NUM_FU],
           enDSE;
///  rand uchar pr_br_adr;
  
  rand uchar srfWrBk, srfWrGrp, srfWrAdr, srf_wr_dsel;
  rand uchar pr_wr_adr0, pr_wr_adr1, ///fu pr write adr
             pr_wr_adr2; ///dse pr write adr
  rand uchar pr_rd_adr[NUM_FU],   ///fu pr read adr
             pr_rd_adr_spu,  ///spu pr read adr
             pr_rd_adr_dse;  ///dse pr read adr
  rand bit pr_inv[NUM_FU], pr_inv_dse, pr_inv_spu,
           pr_nmsk[NUM_FU], pr_nmsk_dse, pr_nmsk_spu;
  
///  rand bit pr_up_en_rot, pr_up_val_rot, pr_up_fnaz_rot;
  
  constraint valid_data{
    TId < NUM_THREAD;
    br_dep dist {0:=6, 1:=4};
    br_dep -> br_dep_dse || br_dep_spa;
///    pr_br_adr <= num_pr;
    subVec dist {0:=5, 1:=5};
    vecMode < CYC_VEC; ///inside {[1:CYC_VEC]};
///    subs dist {0:=5, 1:=5};
///    cycs inside {[1:CYC_VEC]};
    pr_rd_adr_spu == 0 -> br_dep == 0;
    op inside {spu_ops, spu_com_ops};
    op != op_br -> sop == sop_nop && mop == mop_nop && bop == bop_az;
    foreach(pr_rd_adr[i])
      pr_rd_adr[i] <= num_pr;
    pr_wr_adr0 <= num_pr;
    pr_wr_adr1 <= num_pr;
    pr_wr_adr0 <= num_pr;
    pr_wr_adr1 <= num_pr;
///    pr_up_adr <= num_pr;
///    pr_up_adr_rot <= num_pr;
    pr_rd_adr_spu <= num_pr;
    pr_rd_adr_dse <= num_pr;
		srfWrBk inside {[0:NUM_VRF_BKS-1]};
		srfWrGrp inside {[0:NUM_PHY_VRF_GRP-1]};
		srfWrAdr inside {[0:NUM_PRF_P_GRP/NUM_VRF_BKS-1]};
		srf_wr_dsel < 2;
    solve pr_rd_adr_spu before br_dep;
    solve op before sop, mop, bop;
  }
  
  function void post_randomize();
    static uchar last_cycs = 0, last_subs=0, last_subv = 0;
///    static opcode_e last_op;
///    if(last_subs == 0 || last_subs == (last_cycs-1)) begin
///      last_cycs = cycs;
///      last_subs = subs;
///      last_op = op;
///    end
///    else begin
///      last_subs++;
///      cycs = last_cycs;
///      subs = last_subs;
///      op = last_op;
///    end
		if(last_subv == 0 || last_subv == (CYC_VEC - 1)) begin
			last_subv = subVec;
		end
		else begin
		  last_subv++;
			subVec = last_subv;
	  end    
  endfunction
  
	`ovm_object_utils_begin(tr_ise2spu)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_int(br_dep, OVM_ALL_ON)
	  `ovm_field_int(br_dep_dse, OVM_ALL_ON)
	  `ovm_field_int(br_dep_spa, OVM_ALL_ON)
	  `ovm_field_int(start, OVM_ALL_ON)
///	  `ovm_field_int(pr_br_adr, OVM_ALL_ON)
///	  `ovm_field_int(cycs, OVM_ALL_ON)
///	  `ovm_field_int(subs, OVM_ALL_ON)
    `ovm_field_int(subVec, OVM_ALL_ON)
    `ovm_field_int(vecMode, OVM_ALL_ON)
	  `ovm_field_int(pr_wr_adr0, OVM_ALL_ON)
	  `ovm_field_int(pr_wr_adr1, OVM_ALL_ON)
	  `ovm_field_int(pr_wr_adr2, OVM_ALL_ON)
///	  `ovm_field_int(pr_up_adr, OVM_ALL_ON)
///	  `ovm_field_int(pr_up_adr_rot, OVM_ALL_ON)
///	  `ovm_field_int(pr_up_en_rot, OVM_ALL_ON)
///	  `ovm_field_int(pr_up_fnaz_rot, OVM_ALL_ON)
///	  `ovm_field_int(pr_up_val_rot, OVM_ALL_ON)
    `ovm_field_sarray_int(pr_rd_adr, OVM_ALL_ON)
	  `ovm_field_int(pr_rd_adr_spu, OVM_ALL_ON)
	  `ovm_field_int(pr_rd_adr_dse, OVM_ALL_ON)
	  `ovm_field_sarray_int(pr_inv, OVM_ALL_ON)
	  `ovm_field_int(pr_inv_dse, OVM_ALL_ON)
	  `ovm_field_int(pr_inv_spu, OVM_ALL_ON)
	  `ovm_field_sarray_int(pr_nmsk, OVM_ALL_ON)
	  `ovm_field_int(pr_nmsk_dse, OVM_ALL_ON)
	  `ovm_field_int(pr_nmsk_spu, OVM_ALL_ON)
	  `ovm_field_enum(msc_opcode_e, sop, OVM_ALL_ON)
	  `ovm_field_enum(msk_opcode_e, mop, OVM_ALL_ON)
	  `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
///	  `ovm_field_enum(cmp_opcode_e, cop, OVM_ALL_ON)
///	  `ovm_field_enum(pr_merge_e, fmerge, OVM_ALL_ON)
	  `ovm_field_enum(br_opcode_e, bop, OVM_ALL_ON)
		`ovm_field_int(srf_wr_dsel, OVM_ALL_ON)
		`ovm_field_int(srfWrBk, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(srfWrGrp, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(srfWrAdr, OVM_ALL_ON + OVM_DEC)
		`ovm_field_int(enDSE, OVM_ALL_ON + OVM_NOPRINT)
		`ovm_field_sarray_int(enFu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2spu");
		super.new(name);
	endfunction : new
	
endclass : tr_ise2spu

///---------------------------trsaction dse_spu spu_dse------------------------

class tr_spu2dse extends ovm_sequence_item;
   rand bit emsk[NUM_SP];
   
	`ovm_object_utils_begin(tr_spu2dse)
	  `ovm_field_sarray_int(emsk, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spu2dse");
		super.new(name);
	endfunction : new
	
endclass : tr_spu2dse

class tr_dse2spu extends ovm_sequence_item;
  rand uchar TId;
  rand bit pres[NUM_SP], exp;
  
  constraint valid_vars {
    TId < NUM_THREAD;
  }
  
	`ovm_object_utils_begin(tr_dse2spu)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_int(exp, OVM_ALL_ON)
	  `ovm_field_sarray_int(pres, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2spu");
		super.new(name);
	endfunction : new
	
endclass : tr_dse2spu

///---------------------------trsaction ife_ise ise_ife------------------------

class tr_ife2ise extends ovm_sequence_item;
  rand bit instEn;
  rand uchar TId;
  rand inst_fg_c fetchGrp;
  
	`ovm_object_utils_begin(tr_ife2ise)
	  `ovm_field_int(instEn, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_object(fetchGrp, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_ife2ise");
		super.new(name);
		fetchGrp = new();
	endfunction : new
	
endclass : tr_ife2ise

class tr_ise2ife extends ovm_sequence_item;
  rand bit FetchReq;
  rand uchar TId;
  rand uint PC;
  rand bit cancel[NUM_THREAD];
  
	`ovm_object_utils_begin(tr_ise2ife)
	  `ovm_field_int(FetchReq, OVM_ALL_ON)
	  `ovm_field_sarray_int(cancel, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON + OVM_DEC)
	  `ovm_field_int(PC, OVM_ALL_ON + OVM_HEX)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2ife");
		super.new(name);
	endfunction : new
	
endclass : tr_ise2ife

///---------------------------trsaction dse_spa spa_dse------------------------

class tr_spa2dse extends ovm_sequence_item;
///  rand word res[NUM_SP];    ///sync to vwb0
  rand bit cancel[NUM_THREAD];
    
	`ovm_object_utils_begin(tr_spa2dse)
///	  `ovm_field_sarray_int(res, OVM_ALL_ON)
	  `ovm_field_sarray_int(cancel, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_spa2dse");
		super.new(name);
	endfunction : new
	
endclass : tr_spa2dse

class tr_dse2spa extends ovm_sequence_item;
///  rand word res[NUM_SP];
  
	`ovm_object_utils_begin(tr_dse2spa)
///	  `ovm_field_sarray_int(res, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2spa");
		super.new(name);
	endfunction : new
	
endclass : tr_dse2spa

///---------------------------trsaction dse_ise ise_dse------------------------

class tr_ise2dse extends ovm_sequence_item;
  rand uchar wrGrp, wrAdr, wrBk,
             UAWrGrp, UAWrAdr, UAWrBk, TId;
  rand bit vec, en, ua_wr;  ///bp_data
  rand opcode_e op;
  rand uchar vecMode, subVec;
  rand uchar pb_id;
  
	`ovm_object_utils_begin(tr_ise2dse)
	  `ovm_field_int(wrBk, OVM_ALL_ON)
	  `ovm_field_int(wrAdr, OVM_ALL_ON)
	  `ovm_field_int(wrGrp, OVM_ALL_ON)
	  `ovm_field_int(UAWrBk, OVM_ALL_ON)
	  `ovm_field_int(UAWrAdr, OVM_ALL_ON)
	  `ovm_field_int(UAWrGrp, OVM_ALL_ON)
	  `ovm_field_int(en, OVM_ALL_ON)
	  `ovm_field_int(ua_wr, OVM_ALL_ON)
	  `ovm_field_int(vec, OVM_ALL_ON)
	  `ovm_field_int(vecMode, OVM_ALL_ON)
	  `ovm_field_int(subVec, OVM_ALL_ON)
///	  `ovm_field_int(bp_data, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_int(pb_id, OVM_ALL_ON)
	  `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end
  
  constraint valid_vars{
    en dist {0:=4, 1:=6};
///    bp_data dist {0:=4, 1:=6};
    op inside {dse_ops};
///    bp_data -> vec;
    vecMode < CYC_VEC; ///inside {[1:CYC_VEC]};
///    solve vec before bp_data;
  }
  
	function new (string name = "tr_ise2dse");
		super.new(name);
	endfunction : new

endclass : tr_ise2dse

class tr_dse2ise extends ovm_sequence_item;
  rand bit NoLd, NoSt, NoSMsg, NoRMsg,
           cancel, exp, msgWait;   /// sync to dc stage
  rand uchar TId;
  
  constraint dist_var {
    cancel dist {0:=19, 1:=1};
  }
  
  constraint valid_var {
    exp -> cancel;
  }
  
	`ovm_object_utils_begin(tr_dse2ise)
	  `ovm_field_int(NoLd, OVM_ALL_ON)
	  `ovm_field_int(NoSt, OVM_ALL_ON)
	  `ovm_field_int(NoSMsg, OVM_ALL_ON)
	  `ovm_field_int(NoRMsg, OVM_ALL_ON)
	  `ovm_field_int(cancel, OVM_ALL_ON)
	  `ovm_field_int(exp, OVM_ALL_ON)
	  `ovm_field_int(msgWait, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_dse2ise");
		super.new(name);
	endfunction : new
	
endclass : tr_dse2ise

///---------------------------trsaction spu_tlb tlb_spu------------------------

class tr_spu2tlb extends ovm_sequence_item;
  rand word op0;
  rand bit req;
  rand opcode_e op;
  rand uchar TId, sr_adr;
  
  constraint valid_var{
    op inside {tlb_ops};
  }
  
	`ovm_object_utils_begin(tr_spu2tlb)
	  `ovm_field_int(op0, OVM_ALL_ON)
	  `ovm_field_int(req, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
	  `ovm_field_int(sr_adr, OVM_ALL_ON)
	  `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new (string name = "tr_ise2tlb");
		super.new(name);
	endfunction : new
	
endclass : tr_spu2tlb

class tr_tlb2spu extends ovm_sequence_item;
  rand bit rsp;
  rand word res;
  rand uchar TId;
  
	`ovm_object_utils_begin(tr_tlb2spu)
	  `ovm_field_int(rsp, OVM_ALL_ON)
	  `ovm_field_int(res, OVM_ALL_ON)
	  `ovm_field_int(TId, OVM_ALL_ON)
  `ovm_object_utils_end
  
endclass : tr_tlb2spu

///---------------------------trsaction dse_tlb tlb_dse------------------------

class tr_dse2tlb extends ovm_sequence_item;
  rand word v_adrh;      /// vitrual address high phase
  rand opcode_e op;
  rand uchar TId;
  rand bit req;
  
  `ovm_object_utils_begin(tr_dse2tlb)
    `ovm_field_int(v_adrh, OVM_ALL_ON);
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(TId, OVM_ALL_ON);
    `ovm_field_int(req, OVM_ALL_ON);
  `ovm_object_utils_end  
  
endclass : tr_dse2tlb

class tr_tlb2dse extends ovm_sequence_item;
///  rand bit[PHY_width-1:0] phy_adr;
///  rand bit hit, exp;
///  rand uchar eobit;  /// evenoddbit
///  
///  `ovm_object_utils_begin(tr_tlb2dse)
///    `ovm_field_int(phy_adr, OVM_ALL_ON);
///    `ovm_field_int(hit, OVM_ALL_ON);
///    `ovm_field_int(exp, OVM_ALL_ON);
///  `ovm_object_utils_end  

endclass : tr_tlb2dse  

///---------------------------trsaction ife_tlb tlb_ife------------------------

class tr_ife2tlb extends ovm_sequence_item;
  rand word v_adr;
  rand bit req;
  rand uchar TId;
  
  `ovm_object_utils_begin(tr_ife2tlb)
    `ovm_field_int(v_adr, OVM_ALL_ON);
    `ovm_field_int(req, OVM_ALL_ON);
    `ovm_field_int(TId, OVM_ALL_ON);
  `ovm_object_utils_end  

endclass : tr_ife2tlb  

class tr_tlb2ife extends ovm_sequence_item;
  rand word p_adr;
  rand bit rsp, hit, exp;
  rand uchar TId;
  
  `ovm_object_utils_begin(tr_tlb2ife)
    `ovm_field_int(p_adr, OVM_ALL_ON);
    `ovm_field_int(rsp, OVM_ALL_ON);
    `ovm_field_int(hit, OVM_ALL_ON);
    `ovm_field_int(exp, OVM_ALL_ON);
    `ovm_field_int(TId, OVM_ALL_ON);
  `ovm_object_utils_end  

endclass : tr_tlb2ife

///---------------------------trsaction dse_eif eif_dse------------------------
class tr_dse2eif extends ovm_sequence_item;
///  rand bit req;
///  rand bit[PHY_width-1:0] ld_adr[NUM_SP];
///  rand bit[PHY_width-1:0] st_adr[NUM_SP];
///  rand word st_dat[NUM_SP];
///  rand uchar TId;
///  
///  `ovm_object_utils_begin(tr_dse2eif)
///    `ovm_field_int(req, OVM_ALL_ON);
///    `ovm_field_sarray_int(ld_adr, OVM_ALL_ON);
///    `ovm_field_sarray_int(st_adr, OVM_ALL_ON);
///    `ovm_field_sarray_int(st_dat, OVM_ALL_ON);
///    `ovm_field_int(TId, OVM_ALL_ON);
///  `ovm_object_utils_end

endclass : tr_dse2eif

class tr_eif2dse extends ovm_sequence_item;
  rand bit rsp;
  
  `ovm_object_utils_begin(tr_eif2dse)
    `ovm_field_int(rsp, OVM_ALL_ON);
  `ovm_object_utils_end

endclass : tr_eif2dse