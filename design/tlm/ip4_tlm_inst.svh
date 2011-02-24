

class inst_c extends ovm_object;
  inst_u inst;
  bit decoded, decodeErr, vecRd, priv, isVec, needWrAdr;
  opcode_e op;
  rbk_sel_e rdBkSel[NUM_FU_RP];
  uchar CntVrfRd, CntSrfRd, prRdAdr, prWrAdr[2], fuid,
        grpWr[2], adrWr[2], bkWr[2];
  uint imm, offSet;
  bit vrfEn[CYC_VEC][NUM_VRF_BKS], srfEn[CYC_VEC][NUM_SRF_BKS], 
      wrEn[2], prRdEn, prWrEn[2], brDep, fcRet, noExp;
  cmp_opcode_e cmpOp;
  pr_merge_e mergeOp;
  msc_opcode_e mscOp;
  msk_opcode_e mskOp;
  br_opcode_e brOp;
  uchar mST, mSs, mVs, mFun, mRt, mrfAdr, srAdr;
  bit[NUM_FIFO - 1 : 0] mFifos;
  bit enSPU, enDSE, enFu;
  update_adr_t mua;
  access_typ_t mat;
  
  `ovm_object_utils_begin(inst_c)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decodeErr, OVM_ALL_ON)
    `ovm_field_int(isVec, OVM_ALL_ON)
    `ovm_field_int(noExp, OVM_ALL_ON)
    `ovm_field_int(fuid, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(inst, OVM_ALL_ON)
    `ovm_field_enum(opcode_e, op, OVM_ALL_ON)
    `ovm_field_int(enSPU, OVM_ALL_ON)
    `ovm_field_int(enDSE, OVM_ALL_ON)
    `ovm_field_int(enFu, OVM_ALL_ON)
///    `ovm_field_int(priv, OVM_ALL_ON)
    `ovm_field_sarray_enum(rbk_sel_e, rdBkSel, OVM_ALL_ON)
    `ovm_field_int(vecRd, OVM_ALL_ON)
    `ovm_field_int(CntVrfRd, OVM_ALL_ON)
    `ovm_field_int(CntSrfRd, OVM_ALL_ON)
    `ovm_field_int(prRdAdr, OVM_ALL_ON)
    `ovm_field_int(prRdEn, OVM_ALL_ON)
    `ovm_field_sarray_int(prWrAdr, OVM_ALL_ON)
    `ovm_field_sarray_int(prWrEn, OVM_ALL_ON)
///    `ovm_field_int(brDep, OVM_ALL_ON)
///    `ovm_field_sarray_int(grpWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(adrWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(bkWr, OVM_ALL_ON)
///    `ovm_field_sarray_int(grpRMsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(adrRMsg, OVM_ALL_ON)
///    `ovm_field_sarray_int(bkRMsg, OVM_ALL_ON)
    `ovm_field_int(imm, OVM_ALL_ON)
    `ovm_field_int(offSet, OVM_ALL_ON)
    `ovm_field_sarray_int(wrEn, OVM_ALL_ON)
    `ovm_field_sarray_int(bkWr, OVM_ALL_ON)
    `ovm_field_sarray_int(adrWr, OVM_ALL_ON)
    `ovm_field_sarray_int(grpWr, OVM_ALL_ON)
    `ovm_field_enum(cmp_opcode_e, cmpOp, OVM_ALL_ON)
    `ovm_field_enum(pr_merge_e, mergeOp, OVM_ALL_ON)
///    `ovm_field_enum(msc_opcode_e, mscOp, OVM_ALL_ON)
///    `ovm_field_enum(msk_opcode_e, mskOp, OVM_ALL_ON)
///    `ovm_field_enum(br_opcode_e, brOp, OVM_ALL_ON)
///    `ovm_field_int(mST, OVM_ALL_ON)
///    `ovm_field_int(mUpdateAdr, OVM_ALL_ON)
///    `ovm_field_int(mFun, OVM_ALL_ON)
///    `ovm_field_int(mS, OVM_ALL_ON)
///    `ovm_field_int(mRt, OVM_ALL_ON)
///    `ovm_field_int(mT, OVM_ALL_ON)
///    `ovm_field_int(mrfAdr, OVM_ALL_ON)
///    `ovm_field_int(mFifos, OVM_ALL_ON)
  `ovm_object_utils_end

	virtual function void do_print(ovm_printer printer);
  	super.do_print(printer);
  	if(enDSE) begin
      `PF(mST, OVM_BIN)
      `PF(mSs, OVM_BIN)
      `PF(mVs, OVM_BIN)
///      `PF(mUpdateAdr, OVM_BIN)
      `PF(mFun, OVM_BIN)
///      `PF(mS, OVM_BIN)
      `PF(mRt, OVM_BIN)
///      `PF(mT, OVM_BIN)
      `PF(mrfAdr, OVM_BIN)
      `PF(mFifos, OVM_BIN)
      `PE(mua)
      `PE(mat)
    end
    if(enSPU) begin
      `PF(srAdr, OVM_DEC)
      `PF(priv, OVM_BIN)
      `PF(brDep, OVM_BIN)
      `PE(mscOp)
      `PE(mskOp)
      `PE(brOp)
    end
	endfunction : do_print
      
	function new (string name = "inst_c");
  	super.new(name);
	endfunction : new

  function void set_rf_en(input uchar adr, inout rbk_sel_e sel, bit hasVec, 
                          ref bit vrfEn[CYC_VEC][NUM_VRF_BKS], srfEn[CYC_VEC][NUM_SRF_BKS],
                          inout uchar vrf, srf);
    uchar cyc, bk;
    if(adr >= irs_srf0 && adr <= irs_srf_e) begin
      adr -= irs_srf0;
      cyc = adr >> WID_SRF_BKS;
      bk = adr & `GML(WID_SRF_BKS);
      srf = (srf > (cyc + 1)) ? srf : (cyc + 1);
      srfEn[cyc][bk] = 1;
      sel = rbk_sel_e'(sels0 + cyc * NUM_SRF_BKS + bk);
    end
    else if(adr >= irs_vrf0 && adr <= irs_vrf_e) begin
      adr -= irs_vrf0;
      cyc = adr >> WID_VRF_BKS;
      bk = adr & `GML(WID_VRF_BKS);
      vrf = (vrf > (cyc + 1)) ? vrf : (cyc + 1);
      vrfEn[cyc][bk] = 1;
      hasVec = 1;
      sel = rbk_sel_e'(selv0 + cyc * NUM_VRF_BKS + bk);
    end
    else if(adr == irs_z)
      sel = selz;
    else if(adr >= irs_bp0 && adr <= irs_bp_e)
      sel = rbk_sel_e'(selfu0 + adr - irs_bp0);
    else if(adr >= irs_co0 && adr <= irs_co_e)
      sel = rbk_sel_e'(selc0 + adr - irs_co0);
    else if(adr == irs_pc)
      sel = selpc;
  endfunction : set_rf_en
  
	function void decode();
	  bit noRfWr = 0;
	  uchar ibk;
    decoded = 1;
    rdBkSel = '{default : selnull};
    prRdAdr = inst.i.p;
    prRdEn = prRdAdr != 0;
    noExp = 1;
    
    if(inst.i.b.i28.rd == ird_zs) begin
      noRfWr = 1;
      isVec = 0;
      ibk = 0;
    end
    else if(inst.i.b.i28.rd == ird_zv) begin
      noRfWr = 1;
      isVec = 1;
      ibk = 0;
    end
    else if(inst.i.b.i28.rd >= ird_vrfb0 && inst.i.b.i28.rd <= ird_vrfb_e) begin
      isVec = 1;
      ibk = inst.i.b.i28.rd - ird_vrfb0;
    end
    else if(inst.i.b.i28.rd >= ird_srfb0 && inst.i.b.i28.rd <= ird_srfb_e) begin
      ibk = inst.i.b.i28.rd - ird_srfb0;
      isVec = 0;
    end
    
    needWrAdr = !noRfWr;
    
    if(inst.i.op inside {iop_i28}) begin
      imm = {inst.i.b.i28.imm1, inst.i.b.i28.imm0};
      bkWr[0] = ibk;
      wrEn[0] = 1;
      rdBkSel[1] = selii;
      case(inst.i.op)
      iop_lu    : begin op = op_bp1; imm = imm << (WORD_BITS / 2); end
      iop_li    : begin op = op_bp1; end
      endcase
    end
    else if(inst.i.op inside {iop_r1w1i}) begin
      uint imms = {{WORD_BITS{inst.i.b.ir1w1.imm1[$bits(inst.i.b.ir1w1.imm1) - 1]}}, inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      imm = {inst.i.b.ir1w1.imm1, inst.i.b.ir1w1.imm0};
      bkWr[0] = ibk;
      wrEn[0] = 1;
      set_rf_en(inst.i.b.ir1w1.rs, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      rdBkSel[1] = selii;
      case(inst.i.op)
      iop_addi  : begin op = op_uadd; end
      iop_andi  : begin op = op_and; end
      iop_ori   : begin op = op_or; end
      iop_xori  : begin op = op_xor; end
      iop_addsi : begin op = op_uadd; imm = imms; end
      iop_andsi : begin op = op_and; imm = imms; end
      iop_orsi  : begin op = op_or; imm = imms; end
      iop_xorsi : begin op = op_xor; imm = imms; end
      endcase
    end
    else if(inst.i.op == iop_r3w1) begin
      set_rf_en(inst.i.b.ir3w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      set_rf_en(inst.i.b.ir3w1.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      if(inst.i.b.ir3w1.fun inside {iop31_mul, iop31_fmul})
        rdBkSel[2] = selnull;
      else
        set_rf_en(inst.i.b.ir3w1.rs2, rdBkSel[2], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      
      if(inst.i.b.ir3w1.d) begin
        wrEn = '{default : 1};
        bkWr[0] = ibk & `GMH(1);
        bkWr[1] = bkWr[0] + 1;
        rdBkSel[3] = rbk_sel_e'(rdBkSel[2] | 'b01);
      end
      else begin
        wrEn[0] = 1;
        bkWr[0] = ibk;
      end
      
      case(inst.i.b.ir3w1.fun)
      iop31_mul   : begin op = inst.i.b.ir3w1.s ? op_smul : op_umul; end
      iop31_mad   : begin op = inst.i.b.ir3w1.s ? op_smad : op_umad; end
      iop31_msu   : begin op = inst.i.b.ir3w1.s ? op_smsu : op_umsu; end
      iop31_add3  : begin op = inst.i.b.ir3w1.s ? op_add3 : op_uadd3; end
      iop31_fmul  : begin op = op_fmul; end
      iop32_fmad  : begin op = op_fmad; end
      iop31_fmsu  : begin op = op_fmsu; end
      endcase
    end
    else if(inst.i.op == iop_r2w1) begin
      if(inst.i.op != iop21_vid)
        set_rf_en(inst.i.b.ir2w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      if(!(inst.i.op == iop21_vid || inst.i.b.ir2w1.fun inside {iop11_ops}))
        set_rf_en(inst.i.b.ir2w1.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      if(inst.i.b.ir2w1.fun inside {iop21_div, iop21_udiv}) begin
        wrEn = '{default : 1};
        bkWr[0] = ibk & `GMH(1);
        bkWr[1] = bkWr[0] + 1;
        rdBkSel[3] = rbk_sel_e'(rdBkSel[2] + 1);
      end
      else begin
        wrEn[0] = 1;
        bkWr[0] = ibk;
      end
      imm = inst.i.b.ir2w1.imm;
      
      case(inst.i.b.ir2w1.fun)
      iop21_div   : begin op = op_div; end
      iop21_quo   : begin op = op_quo; end
      iop21_res   : begin op = op_res; end
      iop21_udiv  : begin op = op_udiv; end
      iop21_uquo  : begin op = op_uquo; end
      iop21_ures  : begin op = op_ures; end
      iop21_uadd  : begin op = op_uadd; end
      iop21_usub  : begin op = op_usub; end
      iop21_srl   : begin op = op_srl; rdBkSel[1] = selii; end
      iop21_srlv  : begin op = op_srl; end
      iop21_or    : begin op = op_or; end
      iop21_clo   : begin op = op_clo; end
      iop21_ext   : begin op = op_ext; rdBkSel[1] = selii; end
      iop21_sll   : begin op = op_sll; rdBkSel[1] = selii; end
      iop21_rot   : begin op = op_ror; rdBkSel[1] = selii; end
      iop21_and   : begin op = op_and; end
      iop21_seb   : begin op = op_seb; rdBkSel[1] = selii; end
      iop21_wsbh  : begin op = op_wsbh; rdBkSel[1] = selii; end
      iop21_max   : begin op = op_max; end
      iop21_min   : begin op = op_min; end
      iop21_add   : begin op = op_add; end
      iop21_sub   : begin op = op_sub; end
      iop21_sra   : begin op = op_sra; rdBkSel[1] = selii; end
      iop21_srav  : begin op = op_sra; end
      iop21_nor   : begin op = op_nor; end
      iop21_clz   : begin op = op_clz; end
      iop21_ins   : begin op = op_ins; end
      iop21_sllv  : begin op = op_sll; end
      iop21_rotv  : begin op = op_ror; end
      iop21_xor   : begin op = op_xor; end
      iop21_she   : begin op = op_she; end
      ///mvs is special, a spu inst require vec reg
      iop21_mv2s  : begin op = op_mvs; rdBkSel[1] = selii; if(CntVrfRd > CntSrfRd) CntSrfRd = CntVrfRd; end
      iop21_umax  : begin op = op_umax; end
      iop21_umin  : begin op = op_umin; end
      iop21_vid   : begin op = op_vid; rdBkSel[0] = selnull; rdBkSel[1] = selnull;end
      iop21_fadd  : begin op = op_fadd; end 
      iop21_fmax  : begin op = op_fmax; end 
      iop21_fdiv  : begin op = op_fdiv; end 
      iop21_fexp2 : begin op = op_fexp2; end 
      iop21_frootn: begin op = op_frootn; end 
      iop21_fpown : begin op = op_fpown; end 
      iop21_fround: begin op = op_fround; end 
      iop21_ftrunc: begin op = op_ftrunc; end 
      iop21_fsqrt : begin op = op_fsqrt; end 
      iop21_fabs  : begin op = op_fabs; end 
      iop21_fhypot: begin op = op_fhypot; end 
      iop21_fsin  : begin op = op_fsin; end 
      iop21_ftan  : begin op = op_ftan; end 
      iop21_fsinh : begin op = op_fsinh; end 
      iop21_ftanh : begin op = op_ftanh; end 
      
      iop21_fsub  : begin op = op_fsub; end 
      iop21_fmin  : begin op = op_fmin; end 
      iop21_flog2 : begin op = op_flog2; end 
      iop21_fpow  : begin op = op_fpow; end 
      iop21_fpowr : begin op = op_fpowr; end 
      iop21_ffloor: begin op = op_ffloor; end 
      iop21_fceil : begin op = op_fceil; end 
      iop21_frsqrt: begin op = op_frsqrt; end 
      iop21_fdim  : begin op = op_fdim; end 
      iop21_cos   : begin op = op_fcos; end 
      iop21_fatan : begin op = op_fatan; end 
      iop21_fcosh : begin op = op_fcosh; end 
      iop21_fatanh: begin op = op_fatanh; end 
      endcase
    end
    else if(inst.i.op inside {iop_fcrs}) begin
      noExp = 0;
      offSet = {{WORD_BITS{inst.i.b.fcr.os2[$bits(inst.i.b.fcr.os2)-1]}}, inst.i.b.fcr.os2, inst.i.b.fcr.os1, inst.i.b.fcr.os0};
      imm = 0;
      set_rf_en(inst.i.b.fcr.ja, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      fcRet = inst.i.b.fcr.ja == 0;
      op = op_fcr;
      case(inst.i.op)
      iop_fcr   : begin brDep = 0; brOp = bop_az; end
      iop_fcrn  : begin brDep = 0; brOp = bop_naz; end
      iop_fcrp  : begin brDep = 1; brOp = bop_az; end
      iop_fcrpn : begin brDep = 1; brOp = bop_naz; end
      endcase
      bkWr[0] = 0;
      adrWr[0] = 0;
      grpWr[0] = 0;
      wrEn[0] = inst.i.b.fcr.l;
      noRfWr = 0;
      needWrAdr = 0;
      mskOp = inst.i.b.fcr.mu ? (inst.i.b.fcr.l ? mop_if : mop_rstor) : mop_nop;
      mscOp = inst.i.b.fcr.su ? (inst.i.b.fcr.l ? sop_store : sop_p2nc) : sop_p2n;
    end
    else if(inst.i.op inside {iop_bs}) begin
      noExp = 0;
      noRfWr = 1;
      needWrAdr = 0;
      imm = inst.i.b.b.sc;
      rdBkSel[0] = selii;
      offSet = {{WORD_BITS{inst.i.b.b.offSet[$bits(inst.i.b.b.offSet)-1]}}, inst.i.b.b.offSet};
      op = op_br;
      case(inst.i.op)
      iop_b   : begin brDep = 0; brOp = bop_az; end
      iop_bn  : begin brDep = 0; brOp = bop_naz; end
      iop_bp  : begin brDep = 1; brOp = bop_az; end
      iop_bpn : begin brDep = 1; brOp = bop_naz; end
      endcase
      case(inst.i.b.b.sop)
      2'b00 : mscOp = sop_p2n;
      2'b01 : mscOp = sop_p2nc;
      2'b10 : mscOp = sop_store;
      2'b11 : begin mscOp = sop_zero; priv = 1; end
      endcase
      case(inst.i.b.b.mop)
      3'b000 : mskOp = mop_nop;
      3'b001 : mskOp = mop_bc;
      3'b010 : mskOp = mop_rstor;
      3'b011 : mskOp = mop_loop;
      3'b100 : mskOp = mop_else;
      3'b101 : mskOp = mop_cont;
      3'b110 : mskOp = mop_if;
      3'b111 : begin mskOp = mop_guard; priv = 1; end
      endcase
    end
    else if(inst.i.op inside {iop_cmps}) begin
      isVec = 1;
      needWrAdr = 0;
      noRfWr = 1;
      imm = {inst.i.b.cmpi.imm1, inst.i.b.cmpi.imm0};
      set_rf_en(inst.i.b.cmpi.rs, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      case(inst.i.op)
      iop_cmp   :
      begin
        op = inst.i.b.cmp.f ? op_fcmp : op_cmp;
        set_rf_en(inst.i.b.cmp.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      iop_cmpu  :
      begin
        op = op_ucmp;
        set_rf_en(inst.i.b.cmp.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      iop_cmpi  : begin op = op_cmp; rdBkSel[1] = selii; end
      iop_cmpiu : begin op = op_ucmp; rdBkSel[1] = selii; end
      endcase
      
      case(inst.i.b.cmp.ctyp)
      3'b000 : cmpOp = cop_e;
      3'b001 : cmpOp = cop_g;
      3'b010 : cmpOp = cop_ge;
      3'b011 : cmpOp = cop_l;
      3'b100 : cmpOp = cop_le;
      endcase
      
      case(inst.i.b.cmp.mtyp)
      4'b0000 : mergeOp = pm_nop;
      4'b0001 : mergeOp = pm_unc;
      4'b0010 : mergeOp = pm_and;
      4'b0011 : mergeOp = pm_andcm;
      4'b0100 : mergeOp = pm_or;
      4'b0101 : mergeOp = pm_orcm;
      4'b0111 : mergeOp = pm_or_andcm;
      4'b0111 : mergeOp = pm_and_orcm;
      endcase
      prWrAdr[0] = inst.i.b.cmp.pr0;
      prWrAdr[1] = inst.i.b.cmp.pr1;
      prWrEn[0] = prWrAdr[0] != 0;
      prWrEn[1] = prWrAdr[1] != 0;
    end
    else if(inst.i.op inside {iop_sp_dse, iop_ls_dse}) begin
      noExp = 0;
      rdBkSel[2] = selii;
      mat = access_typ_t'(inst.i.b.ld.t);
      mua = update_adr_t'(inst.i.b.ld.ua);
      wrEn[1] = mua != ua_no ? 1 : 0;
      bkWr[0] = ibk;
      prWrAdr[0] = inst.i.p;
      prWrEn[0] = (mat == at_rand && prWrAdr[0] != 0);
      if(!prWrEn[0]) prWrAdr[0] = 0;
      
      if(inst.i.op inside {iop_lw, iop_lh, iop_lb, iop_ll, iop_lhu, iop_lbu}) begin
        imm = {inst.i.b.ld.os1, inst.i.b.ld.os0};
        wrEn[0] = 1;
        set_rf_en(inst.i.b.ld.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        case(inst.i.op)
        iop_lw    : op = op_lw;
        iop_lh    : op = op_lh;
        iop_lb    : op = op_lb;
        iop_ll    : op = op_ll;
        iop_lhu   : op = op_lhu;
        iop_lbu   : op = op_lbu;
        endcase
      end
      else if(inst.i.op inside {iop_sw, iop_sh, iop_sb, iop_sc}) begin
        noRfWr = 1;
        needWrAdr = 0;
        imm = {inst.i.b.st.os2, inst.i.b.st.os1, inst.i.b.st.os0};
        set_rf_en(inst.i.b.st.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.st.rs, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        case(inst.i.op)
        iop_sw    : op = op_sw;
        iop_sh    : op = op_sh;
        iop_sb    : op = op_sb;
        iop_sc    : op = op_sc;
        endcase
      end
      else if(inst.i.op == iop_cmpxchg) begin
        op = op_cmpxchg;
        wrEn[0] = 1;
        imm = {inst.i.b.cmpxchg.os2, inst.i.b.cmpxchg.os1, inst.i.b.cmpxchg.os0};
        set_rf_en(inst.i.b.cmpxchg.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.cmpxchg.rs, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      else if(inst.i.op == iop_fetadd) begin
        op = op_fetadd;
        wrEn[0] = 1;
        imm = {inst.i.b.fetadd.os1, inst.i.b.fetadd.os0};
        set_rf_en(inst.i.b.fetadd.rb, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.fetadd.rs, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      else if(inst.i.op == iop_mctl) begin
        noRfWr = 1;
        needWrAdr = 0;
        mFun = inst.i.b.mctl.fun;
        imm = {inst.i.b.mctl.os1, inst.i.b.mctl.os0};
        if(inst.i.b.mctl.c)
          op = op_cache;
        if(mFun < 7)
          op = op_pref;
        else if(mFun < 9)
          op = op_syna;
        else if(mFun < 11)
          op = op_synld;
        else if(mFun < 13)
          op = op_synst;
        else if(mFun == 13)
          op = op_synci;
      end
    end
    else if(inst.i.op == iop_mrfa) begin
      op = inst.i.b.mrfa.ft ? op_fmrf : op_tmrf;
      bkWr[0] = ibk;
      if(op == op_tmrf) begin
        wrEn[0] = 1;
      end
      else begin
        wrEn[0] = 0;
        needWrAdr = 0;
        noRfWr = 1;
      end
      set_rf_en(inst.i.b.mrfa.rs, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      imm = inst.i.b.mrfa.s;
      mST = inst.i.b.mrfa.st;
      mrfAdr = inst.i.b.mrfa.mrfa;
      prWrAdr[0] = inst.i.p;
      prWrEn[0] = prWrAdr[0] != 0;
    end
    else if(inst.i.op == iop_cmsg) begin
      op = inst.i.b.cmsg.sr ? op_smsg : op_rmsg;
      noRfWr = 1;
      needWrAdr = 0;
      mFifos = inst.i.b.cmsg.fifos;
      mSs = inst.i.b.cmsg.ss;
      mVs = inst.i.b.cmsg.vs;
      mRt = inst.i.b.cmsg.mrt;
    end    
    else if(inst.i.op == iop_vxchg) begin
      if(inst.i.b.vxchg.t) begin
        op = inst.i.b.vxchg.s ? op_pera : op_perb;
        set_rf_en(inst.i.b.vxchg.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
        set_rf_en(inst.i.b.vxchg.rs1, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      else begin
        op = inst.i.b.vxchg.s ? op_shf4a : op_shf4b;
        imm = inst.i.b.vxchg.fun;
        rdBkSel[0] = selz;
        rdBkSel[2] = selii;
        set_rf_en(inst.i.b.vxchg.rs0, rdBkSel[1], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
      end
      bkWr[0] = ibk;
      wrEn[0] = 1;
      prWrAdr[0] = inst.i.p;
      prWrEn[0] = prWrAdr[0] != 0 && inst.i.b.vxchg.up;
    end 
    else if(inst.i.op == iop_cop) begin
      imm = inst.i.b.cop.code;
      
      if(inst.i.b.cop.fun != icop_asr) begin
        needWrAdr = 0;
        noRfWr = 1;
      end
      
      case(inst.i.b.cop.fun)
      icop_sysc  : op = op_sys;
      icop_wait  : op = op_wait;
      icop_exit  : op = op_exit;
      icop_brk   : op = op_brk;
      icop_tsync : op = op_tsync;
      icop_msync : op = op_msync;
      icop_alloc :
      begin
        op = op_alloc;
        priv = 1;
      end
      icop_tlbp  :
      begin
        op = op_tlbp;
        priv = 1;
      end
      icop_tlbr  :
      begin
        op = op_tlbr;
        priv = 1;
      end
      icop_tlbwi  :
      begin
        op = op_tlbwi;
        priv = 1;
      end
      icop_tlbwr  :
      begin
        op = op_tlbwr;
        priv = 1;
      end
      icop_asr  :
      begin
        wrEn[0] = inst.i.b.cop.code[0];
        op = wrEn[0] ? op_s2gp : op_gp2s;
        bkWr[0] = ibk;
        srAdr = inst.i.b.cop.code[17:10];
        if(!wrEn[0]) begin
          set_rf_en(inst.i.b.ir2w1.rs0, rdBkSel[0], vecRd, vrfEn, srfEn, CntVrfRd, CntSrfRd);
          needWrAdr = 0;
          noRfWr = 1;
        end
        else
          rdBkSel[0] = selii;
        imm = inst.i.b.cop.code[9:1];
        if(!wrEn[0] && !(srAdr inside {non_kernel_sr}))
          priv = 1;
      end
      icop_eret :
      begin
        op = op_eret;
        priv = 1;
      end      
      endcase
    end
    
///    if(isVec)
///      foreach(adrWr[i]) begin
///        bkWr[i] = adrWr[i] & `GML(WID_VRF_BKS);
///        grpWr[i] = adrWr[i] >> WID_PRF_P_GRP;
///        adrWr[i] = (adrWr[i] >> WID_VRF_BKS) & `GML(WID_PRF_P_GRP - WID_VRF_BKS);
///      end
///  	else
///      foreach(adrWr[i]) begin
///        bkWr[i] = adrWr[i] & `GML(WID_SRF_BKS);
///        grpWr[i] = adrWr[i] >> WID_PRF_P_GRP;
///        adrWr[i] = (adrWr[i] >> WID_SRF_BKS) & `GML(WID_PRF_P_GRP - WID_SRF_BKS);
///      end
      
    if(noRfWr)
      wrEn = '{default : 0};
	endfunction : decode
  
	function bit is_unc_br();
    if(!decoded) decode();
    return (op inside {op_br, op_fcr}) && (brOp == bop_naz && !prRdEn);
	endfunction : is_unc_br

	function bit is_br();
    if(!decoded) decode();
    return op inside {op_br, op_fcr};
	endfunction : is_br
    
	function bit is_priv();
    if(!decoded) decode();
    return priv;
	endfunction : is_priv
    
///	function void set_wcnt(inout uchar wCnt[7], input uchar vm, bit nb = 0);///, ld = 0, st = 0, vec = 1);
///    uchar tCnt[6] = '{default : 0};
///    if(!decoded) decode();
///    ///long cyc instructions
///    if(op inside {sfu_only_ops}) begin
///      if(isVec)
///        tCnt[gprv_styp] = STAGE_RRF_RRC + STAGE_EEX_VWB - 1;
///      else
///        tCnt[gprs_styp] = STAGE_RRF_RRC + STAGE_EEX_VWB - 1;
///    end
///    else if(op inside {op_gp2s, op_alloc, tlb_ops}) begin
///      ///sr write back
///      tCnt[sr_styp] = STAGE_RRF_RSRB;
///    end
///    ///zero wait instructions for ise only, those inst only change threadstate
///    else if(op inside {ise_zw_ops}) begin
///    end
///    ///branchs are predicted, no need to wait
///    else if(op inside {op_fcr, op_br}) begin
///      ///need to resolve br, only change sr, gen exp
//////      if(!is_unc_br()) begin
///      tCnt[sr_styp] = STAGE_RRF_CBR - 1;
///      tCnt[br_styp] = STAGE_RRF_CBR + vm - 1;
//////      end
///    end
///    else if(op inside {ld_ops}) begin
///      if(isVec)
///        tCnt[gprv_styp] = STAGE_RRF_VWB - 1;
///      else
///        tCnt[gprs_styp] = STAGE_RRF_SWB - 1;
///      ///load write pr
///      if(mat != at_randnu)
///       tCnt[pr_styp] = STAGE_RRF_DEM;
///      ///load generate exp
///      tCnt[br_styp] = STAGE_RRF_SEL + vm;
///      if(nb) begin
///        ///in nb mode, only care about e
///        tCnt[gprs_styp] = 0;
///        tCnt[gprv_styp] = 0;
///      end
///    end
///    ///store are non blocking
///    else if(op inside {st_ops}) begin
///      ///store write dc
///      if(!nb)
///        tCnt[mem_styp] = STAGE_RRF_LXG0 + vm;
///      ///store write pr
///     if(mat != at_randnu)
///       tCnt[pr_styp] = STAGE_RRF_DEM;
///      ///store generate exp
///      tCnt[br_styp] = STAGE_RRF_SEL + vm;
///    end
///    else if(op inside {op_cmp, op_ucmp}) begin
///      ///cmp write pr
///      tCnt[pr_styp] = STAGE_RRF_CMP;
///    end
///    else begin
///      if(isVec)
///        tCnt[gprv_styp] = STAGE_RRF_VWB - 1;
///      else
///        tCnt[gprs_styp] = STAGE_RRF_SWB - 1;
///      tCnt[br_styp] = noExp ? 0 : (isVec ? STAGE_RRF_VWB : STAGE_RRF_SWB);
///    end
///    
///    foreach(tCnt[i])
///      if(tCnt[i] > wCnt[i])
///        wCnt[i] = tCnt[i];
///    
///    ///min_styp don't need br_styp
///    foreach(tCnt[i])
///      if(((wCnt[i] != 0 && tCnt[i] < wCnt[min_styp]) || wCnt[min_styp] == 0) && i != br_styp)
///        wCnt[min_styp] = wCnt[i];
///	endfunction : set_wcnt
  
  function void set_wcnt(inout uchar brcnt, bit dep, input uchar vm, bit nb = 0);
    if(op inside {ld_ops})
      dep = 1;
    else if(op inside {st_ops})
      brcnt = vm + 1 + STAGE_EXE_VWBP - STAGE_RRF_DC;
  endfunction
  
	function void set_data(const ref uchar data[$], input uchar start, id = 0);
    fuid = id;
    decoded = 0;
    priv = 0;
    enDSE = 0;
    enSPU = 0;
    enFu = 0;
    prWrAdr = '{default : 0};
    vrfEn = '{default : 0};
    srfEn = '{default : 0};
    wrEn = '{default : 0};
    prRdEn = 0;
    prWrEn = '{default : 0};
    brDep = 0;    
    op = op_nop;
    CntSrfRd = 0;
    CntVrfRd = 0;
    vecRd = 0;
    fcRet = 0;
    isVec = 1;
    
    foreach(inst.b[i])
      inst.b[i] = data[start+i];
      
    decode();
///    if(op inside {dse_ops})
///      enDSE = 1;
///    else if(vec)  
///      enFu = 1;
///    else if(op inside {spu_ops, spu_com_ops})
///      enSPU = 1;    
	endfunction : set_data
  
  function void analyze(input uchar vmode, ref bit v_en[CYC_VEC][NUM_VRF_BKS], 
                            s_en[CYC_VEC][NUM_SRF_BKS], inout uchar vrfBusy, srfBusy, dseBusy, spuBusy, fuBusy,
                            ref uchar vrf[NUM_VRF_BKS], srf[NUM_SRF_BKS], inout uchar pr);
    if(!decoded) decode();
    foreach(vrfEn[i,j])
      v_en[i][j] = v_en[i][j] | vrfEn[i][j];

    foreach(srfEn[i,j])
      s_en[i][j] = s_en[i][j] | srfEn[i][j];
    
    if(CntSrfRd > srfBusy)
      srfBusy = CntSrfRd;
    if(CntVrfRd > vrfBusy)
      vrfBusy = CntVrfRd;
    
    if(enSPU) begin
      if(is_br() && !is_unc_br())
        spuBusy = vmode + 1;
      else
        spuBusy = 1;
    end
    
    if(enFu) begin
      if(isVec)
        fuBusy = vmode + 1;
      else if(fuBusy== 0)
        fuBusy = 1;
    end
  
    if(enDSE) begin
      if(isVec)
        dseBusy = vmode + 1;
      else
        dseBusy = 1;
    end
    
    foreach(wrEn[i])
      if(wrEn[i]) begin
        if(isVec)
          vrf[bkWr[i]]++;
        else
          srf[bkWr[i]]++;
      end
      
    foreach(prWrEn[i])
      pr += prWrEn[i];
    
///    if(enFu) begin
///      wCntDep[gprv_styp] = 1;
///      wCntDep[gprs_styp] = 1;
///    end
///    
///    if(enSPU) begin
///      wCntDep[gprs_styp] = 1;
///      if(op inside {op_s2gp})
///        wCntDep[sr_styp] = 1;
///    end
///    
///    if(enDSE) begin
///      wCntDep[gprv_styp] = 1;
///      wCntDep[gprs_styp] = 1;
///      if(op inside {ld_ops})/// && mat != at_rand)
///        wCntDep[mem_styp] = 1;
///    end
///    
///    if(prRdEn) wCntDep[pr_styp] = 1;
  endfunction : analyze
  
  ///reallocate en set by set_data
  function void analyze_fu(inout bit spu, dse, ref bit fu[NUM_FU]);
    enDSE = 0;
    enSPU = 0;
    enFu = 0;
    if(!decoded) decode();
    if(op inside {dse_ops})
      enDSE = 1;
    else if(op inside {spu_ops})
      enSPU = 1;
    else begin ///if(isVec) 
      enFu = 1;
      if(op inside {sfu_only_ops}) begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == sfu) begin
            fu[i] = 1;
            break;
          end
      end
      else begin
        foreach(fu_cfg[i])
          if(fu_cfg[i] == alu) begin
            fu[i] = 1;
            break;
          end
      end
    end
///    else if(op inside {spu_com_ops})
///      enSPU = 1;
      
    spu = enSPU;
    dse = enDSE;
  endfunction : analyze_fu
  
  function rbk_sel_e cvt_sel(input rbk_sel_e s, uchar i);
    cvt_sel = s;
    if(s inside {[selv0:selv_e], [sels0:sels_e]})
      cvt_sel = selnull;
    if(s inside {[(selv0 + i * NUM_VRF_BKS) : (selv0 + (i + 1) * NUM_VRF_BKS - 1)]})
      cvt_sel = rbk_sel_e'(s - i * NUM_VRF_BKS);
    if(s inside {[(sels0 + i * NUM_SRF_BKS) : (sels0 + (i + 1) * NUM_SRF_BKS - 1)]})
      cvt_sel = rbk_sel_e'(s - i * NUM_SRF_BKS);
///    $display($psprintf("%s, %s, %0d", s.name, cvt_sel.name, i));
  endfunction
  
  function void fill_rfm(input tr_ise2rfm rfm, uchar subVec);
    if(!decoded) decode();
    if(enSPU) begin
      rfm.spuEn = 1;
      rfm.spuImm = imm;
      foreach(rfm.spuRdBk[i])
        rfm.spuRdBk[i] = cvt_sel(rdBkSel[i], subVec);
    end
    else if(enDSE) begin
      rfm.dseEn = 1;
      rfm.dseImm = imm;
      foreach(rfm.dseRdBk[i])
        rfm.dseRdBk[i] = cvt_sel(rdBkSel[i], subVec);
    end
    else begin
      rfm.fu[fuid].en = 1;
      rfm.fu[fuid].imm = imm;
      foreach(rfm.fu[0].rdBkSel[i])
        rfm.fu[fuid].rdBkSel[i] = cvt_sel(rdBkSel[i], subVec);
    end
  endfunction : fill_rfm

  function void fill_spu(input tr_ise2spu spu);
    if(!decoded) decode();
    
    spu.prNMsk = '{default : 0};
    if(enSPU) begin
      spu.sop = mscOp;
      spu.mop = mskOp;
      spu.bop = brOp;
      ///unconditional branch is not issued
      if(is_unc_br())
        spu.op = op_bp1;
      else
        spu.op = op;
      spu.srfWrAdr = adrWr[0];
      spu.srfWrBk = bkWr[0];
      spu.srfWrGrp = grpWr[0];
      spu.prRdAdrSPU = prRdAdr;
      spu.prInvSPU = 0;
      spu.prNMskSPU = 0;
      spu.brDep = brDep;
      spu.srAdr = srAdr;
      spu.rt = mRt;
      spu.ss = mSs;
      spu.vs = mVs;
      spu.enSPU = 1;
      spu.wrEn = wrEn[0];
    end
    else if(enDSE) begin
      spu.prRdAdrDSE = prRdAdr;
      spu.prNMskDSE = 0;
      spu.prInvDSE = 0;
      spu.prWrAdr2 = prWrAdr[0];
      spu.enDSE = 1;
      spu.vecDSE = isVec;
    end
    else begin
      spu.enFu[fuid] = 1;
      spu.vecFu[fuid] = isVec;
      spu.prInv[fuid] = 0;
      spu.prNMsk[fuid] = 0;
      spu.prRdAdr[fuid] = prRdAdr;
      if(op inside {op_cmp, op_ucmp}) begin
        spu.prWrAdr0 = prWrAdr[0];
        spu.prWrAdr1 = prWrAdr[1];
      end
    end
  endfunction : fill_spu

  function void fill_dse(input tr_ise2dse dse);
    if(!decoded) decode();
    if(enDSE) begin
      dse.en = 1;
      dse.wrAdr = adrWr[0];
      dse.wrBk = bkWr[0];
      dse.wrGrp = grpWr[0];
      dse.uaWrBk = (rdBkSel[0] >= selv0 && rdBkSel[0] <= selv_e) ? rdBkSel[0] - selv0 : 0;
      dse.uaWrGrp = 0;
      dse.uaWrAdr = 0; ///todo no way to know those yet
      dse.uaWrEn = wrEn[1];
      dse.op = op;
      dse.vec = isVec;
      dse.wr = wrEn[0];
      dse.ua = mua;
      dse.at = mat;
      dse.sendRotRight = mST;
      dse.mrfAdr = mrfAdr;
    end
  endfunction : fill_dse

  function void fill_spa(input tr_ise2spa spa);
    if(!decoded) decode();
    if(op inside {op_cmp, op_ucmp}) begin
      spa.prMerge = mergeOp;
    end
    
    if(op inside {dse_ops}) begin
      spa.bpRfDSE = rdBkSel[1];
      spa.bpRfDSEwp = 0;
    end
    else begin
      spa.fu[fuid].en = 1;
      spa.fu[fuid].op = op;
      spa.fu[fuid].cop = cmpOp;
      spa.fu[fuid].wrEn = wrEn;
      foreach(spa.fu[0].bpSel[i])
        spa.fu[fuid].bpSel[i] = rdBkSel[i];
      spa.fu[fuid].wrBk = bkWr[0];
      spa.fu[fuid].wrAdr = adrWr[0];
      spa.fu[fuid].wrGrp = grpWr[0];
      spa.fu[fuid].vec = isVec;
    end
  endfunction : fill_spa

  function bit dse_block(input uchar noLd, noSt, noTMsg, noFMsg);
    if(!decoded) decode();
    if(enSPU) begin
      if(noLd > 0 && op inside {ld_ops})
        return 1;
      if(noSt > 0 && op inside {st_ops})
        return 1;
      if(noTMsg > 0 && op == op_tmrf)
        return 1;
      if(noFMsg > 0 && op == op_fmrf)
        return 1;
    end
    return 0;
  endfunction : dse_block

endclass

class inst_fg_c extends ovm_object;
  uchar data[NUM_IFET_BYTES];
  bit k, ex, exp, accErr;
  uint pc;
  
  `ovm_object_utils_begin(inst_fg_c)
    `ovm_field_sarray_int(data, OVM_ALL_ON + OVM_BIN)
    `ovm_field_int(k, OVM_ALL_ON)
    `ovm_field_int(ex, OVM_ALL_ON)
    `ovm_field_int(exp, OVM_ALL_ON)
    `ovm_field_int(accErr, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
  `ovm_object_utils_end
  
	function new(string name = "inst_fg_c");
  	super.new(name);
	endfunction : new

	function void fill(const ref uchar i[NUM_IFET_BYTES]);
    data = i;
	endfunction : fill
endclass
