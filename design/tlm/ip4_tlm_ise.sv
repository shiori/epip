/// =============================================================================
///                         FILE DETAILS
/// Project          : IP4
/// Author           : Andy Chen
/// File             : ip4_tlm_ise.sv
/// Title            : ip4 instruction stream engine
/// Version          : 0.1
/// Last modified    : Mar 16 2010
/// =============================================================================
///Log:
///Created by Andy Chen on Mar 16 2010

typedef enum uchar {
  exp_decode_err,   exp_dse_err,    exp_priv_err
}ise_exp_t;

class ip4_tlm_ise_sr extends ovm_object;
  uint ebase;   ///SR[4]
  
  `ovm_object_utils_begin(ip4_tlm_ise_sr)
    `ovm_field_int(ebase, OVM_ALL_ON)
  `ovm_object_utils_end

  function new(string name = "ise_sr");
   super.new(name);
   ebase = cfg_start_adr;
  endfunction
  
endclass

class ip4_tlm_ise_vars extends ovm_component;
  tr_spu2ise fm_spu;
  tr_rfm2ise fm_rfm;
  tr_ife2ise fm_ife;
  tr_spa2ise fm_spa;
  tr_dse2ise fm_dse[stage_exe_vwbp:stage_exe_dwbp];
  
  tr_ise2rfm rfm[stage_ise:1];
  tr_ise2spa spa[stage_ise:1];
  tr_ise2spu spu[stage_ise:1];
  tr_ise2dse dse[stage_ise:1];
  
  uchar tid_iss_l, tid_fet_l, tid_ife_cancel;
  bit ife_cancel;
  ip4_tlm_ise_sr sr;
    
  `ovm_component_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_object(fm_ife, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE + OVM_NOPRINT)
    `ovm_field_int(tid_iss_l, OVM_ALL_ON)
    `ovm_field_int(tid_fet_l, OVM_ALL_ON)
    `ovm_field_int(tid_ife_cancel, OVM_ALL_ON)
    `ovm_field_int(ife_cancel, OVM_ALL_ON)
    `ovm_field_object(sr, OVM_ALL_ON)
  `ovm_component_utils_end

  function new(string name, ovm_component parent);
    super.new(name, parent);
    tid_fet_l = 0;
    tid_iss_l = 0;
    sr = new(); 
    print_enabled = 0;
  endfunction : new
endclass : ip4_tlm_ise_vars

class ise_thread_inf extends ovm_component;
  ise_thread_state ts;
  uchar ibuf[num_ibuf_bytes];
  bit nc, dse_vec;
  uchar ibuf_level, igrp_bytes, ap_bytes, num_imms,
        cnt_srf_rd, cnt_vrf_rd, cnt_dse_rd;
  word imms[num_bp_imm];
  uchar vrf_adr[cyc_vec][num_vrf_bks], vrf_grp[cyc_vec][num_vrf_bks],
        srf_adr[cyc_vec][num_srf_bks], srf_grp[cyc_vec][num_srf_bks];
  bit vrf_rd_en[cyc_vec][num_vrf_bks], srf_rd_en[cyc_vec][num_srf_bks];
  uchar cnt_pr_wr, cnt_vrf_wr[num_vrf_bks], cnt_srf_wr[num_srf_bks];
  
  bit en_spu, en_dse, en_vec, en_fu[num_fu];
  bit priv_mode,  ///privilege running status
      decoded,
      decode_error;
  uchar wcnt, vec_mode;
  
  uchar vrf_map[num_inst_vrf/num_prf_p_grp], 
        srf_map[num_inst_srf/num_prf_p_grp];
  uchar pd_ifet;
  
  inst_c i_spu, i_dse, i_fu[num_fu];
  uint pc, pc_l;  ///pc_br
  bit br_pred;
    
  `ovm_component_utils_begin(ise_thread_inf)
    `ovm_field_enum(ise_thread_state, ts, OVM_ALL_ON)
    `ovm_field_int(decoded, OVM_ALL_ON)
    `ovm_field_int(decode_error, OVM_ALL_ON)
    `ovm_field_int(priv_mode, OVM_ALL_ON)
    `ovm_field_sarray_int(ibuf, OVM_ALL_ON)
    `ovm_field_int(nc, OVM_ALL_ON)
    `ovm_field_int(ibuf_level, OVM_ALL_ON)
    `ovm_field_int(igrp_bytes, OVM_ALL_ON)
    `ovm_field_int(ap_bytes, OVM_ALL_ON)
    `ovm_field_int(num_imms, OVM_ALL_ON)
    `ovm_field_int(cnt_srf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_vrf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_dse_rd, OVM_ALL_ON)
    `ovm_field_sarray_int(imms, OVM_ALL_ON)
    `ovm_field_int(cnt_pr_wr, OVM_ALL_ON)
    `ovm_field_sarray_int(cnt_vrf_wr, OVM_ALL_ON)
    `ovm_field_sarray_int(cnt_srf_wr, OVM_ALL_ON)
    `ovm_field_int(en_spu, OVM_ALL_ON)
    `ovm_field_int(en_dse, OVM_ALL_ON)
    `ovm_field_sarray_int(en_fu, OVM_ALL_ON)
    `ovm_field_int(en_vec, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_int(wcnt, OVM_ALL_ON)
    `ovm_field_int(vec_mode, OVM_ALL_ON)
    `ovm_field_int(pd_ifet, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(pc_l, OVM_ALL_ON)
    `ovm_field_int(br_pred, OVM_ALL_ON)
    `ovm_field_sarray_int(vrf_map, OVM_ALL_ON)
    `ovm_field_sarray_int(srf_map, OVM_ALL_ON)
    `ovm_field_object(i_spu, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_object(i_dse, OVM_ALL_ON + OVM_NOPRINT)
    `ovm_field_sarray_object(i_fu, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end

	virtual function void do_print(ovm_printer printer);
		super.do_print(printer);
	  if(get_report_verbosity_level() >= OVM_HIGH) begin
  		if(en_spu)
  		  printer.print_object("spu", i_spu);
  		if(en_dse)
  		  printer.print_object("dse", i_dse);
  		foreach(en_fu[i])
  		  if(en_fu[i])
  		    printer.print_object($psprintf("fu%0d", i), i_fu[i]);
    end
	    
    `PAF2(vrf_adr, OVM_DEC)
    `PAF2(srf_adr, OVM_DEC)
    `PAF2(vrf_grp, OVM_DEC)
    `PAF2(srf_grp, OVM_DEC)
	endfunction : do_print
	
  function new(string name, ovm_component parent);
    super.new(name, parent);
    i_spu = new();
    i_dse = new();
    foreach(i_fu[i])
      i_fu[i] = new();
    ts = ts_disabled;
    priv_mode = 0;
    pc = cfg_start_adr;
    pc_l = cfg_start_adr;
    vec_mode = cyc_vec;
    decoded = 0;
    decode_error = 0;
    print_enabled = 0;
  endfunction : new
 
  function void map_iaddr(input bit v, uchar oadr, output uchar grp, adr);
    uchar adr_bits =  v ? (bits_prf_p_grp - bits_vrf_bks) : (bits_prf_p_grp - bits_srf_bks);
    adr = oadr & ('1 << adr_bits);
    grp = oadr >> adr_bits;
    grp = v ? vrf_map[grp] : srf_map[grp];
  endfunction : map_iaddr

  function void cyc_new();
    if(wcnt != 0) wcnt--;
    if(ts == ts_w_pip && wcnt == 0)
      ts = ts_rdy;
  endfunction : cyc_new

  function void exe_priv();
  endfunction : exe_priv
  
  function void decode_igs();
    i_gs0_t gs0 = ibuf[0];
    en_spu = 0;
    en_dse = 0;
    en_vec = 0;
    en_fu = '{default : 0};

    if(!gs0.t) begin
      nc = gs0.nc;
      ap_bytes = gs0.apb;
      num_imms = gs0.ipw;
      dse_vec = gs0.fua;
      igrp_bytes = 1 + ap_bytes + num_imms * num_word_bytes + num_inst_bytes;
    end
    else begin
      i_gs1_u gs;
      uchar tmp = 0;
      foreach(gs.b[i])
        gs.b[i] = ibuf[i];
      foreach(gs.i.fua[i])
        tmp += gs.i.fua[i];
      if(tmp == 0) begin
        ovm_report_warning("decode_igs", "igs decode error, fua not valid");
        decode_error = 1;
      end
      nc = gs.i.nc;
      ap_bytes = gs.i.apb;
      num_imms = gs.i.ipw;
      igrp_bytes = 2 + ap_bytes + num_imms * num_word_bytes + tmp * num_inst_bytes;
      en_spu = gs.i.fua[0];
      en_dse = gs.i.fua[1];
      dse_vec = gs.i.dv;
      foreach(en_fu[i])
        en_fu[i] = gs.i.fua[2+i];
    end
        
    foreach(en_fu[i])
      en_vec |= en_fu[i];
      
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [num_fu-1:0] en_fu_t;
      foreach(en_fu_t[i])
        en_fu_t[i] = en_fu[i];
        
      ovm_report_info("decode_igs",
        $psprintf("inst grp len %0d bytes includes: spu:%0b, dse:%0b, fu:%b. dv:%0b, nc:%0b, apb:%0d, ipw:%0d", 
                   igrp_bytes, en_spu, en_dse, en_fu_t, dse_vec, nc, ap_bytes, num_imms),
        OVM_HIGH);
    end
  endfunction : decode_igs
    
  function void decode_ig();
///    decode_igs();  /// no need
    uchar tmp = 0;
    iga_t a[12];
    uchar os;
    i_gs0_t gs0 = ibuf[0];
    
    vrf_rd_en = '{default : 0};
    srf_rd_en = '{default : 0};
    cnt_vrf_rd = 0;
    cnt_srf_rd = 0;
    cnt_dse_rd = 0;
    cnt_vrf_wr = '{default : 0};
    cnt_srf_wr = '{default : 0};
    cnt_pr_wr = 0;
    
    if(!gs0.t) begin
      tmp = 1;
      os = 1;
      if(ap_bytes != 0) ap_bytes --;
      i_spu.set_data(ibuf, os, 0, dse_vec);
      i_dse.set_data(ibuf, os, 0, dse_vec);
      foreach(i_fu[i])
        i_fu[i].set_data(ibuf, os, i, 1);
        
      os += num_inst_bytes;
      i_spu.analyze_rs(vec_mode, vrf_rd_en, srf_rd_en, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
      i_spu.analyze_rd(cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
      i_spu.analyze_fu(en_spu, en_dse, en_fu);
      a[0] = gs0.a;
      if(ap_bytes) begin
        i_ap0_t ap = ibuf[os];
        foreach(a[i])
          a[i] = ap.a[i];
        os ++;
      end
    end
    else begin
      i_gs1_u gs;
      gs.b[0] = ibuf[0];
      gs.b[1] = ibuf[1];
      os = 2;
      tmp = 1;
      if(ap_bytes != 0) ap_bytes --;
      
      if(en_spu) begin
        i_spu.set_data(ibuf, os, 0, 0);
        i_spu.analyze_rs(vec_mode, vrf_rd_en, srf_rd_en, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
        i_spu.analyze_rd(cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
        os += num_inst_bytes;
      end
      
      if(en_dse) begin
        i_dse.set_data(ibuf, os, 0, dse_vec);
        i_dse.analyze_rs(vec_mode, vrf_rd_en, srf_rd_en, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
        i_dse.analyze_rd(cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
        os += num_inst_bytes;
      end
      
      foreach(i_fu[i])
        if(en_fu[i]) begin
          i_fu[i].set_data(ibuf, os, i, 1);
          i_fu[i].analyze_rs(vec_mode, vrf_rd_en, srf_rd_en, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
          i_spu.analyze_rd(cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
          os += num_inst_bytes;          
        end

      a[0] = gs.i.a;
    end
    
    ///fill in rf address
    while(ap_bytes != 0) begin
      if(ap_bytes >= 3) begin
        i_ap2_u ap;
        foreach(ap.b[i]) begin
          ap.b[i] = ibuf[os];
          os++;
        end
        foreach(ap.i.a[i])
          a[tmp++] = ap.i.a[i];
        ap_bytes -= 3;
      end
      else if(ap_bytes >= 2) begin
        i_ap1_u ap;
        foreach(ap.b[i]) begin
          ap.b[i] = ibuf[os];
          os++;
        end
        foreach(ap.i.a[i])
          a[tmp++] = ap.i.a[i];
        ap_bytes -= 2;
      end
      else if(ap_bytes >= 1) begin
        i_ap0_t ap;
        ap = ibuf[os];
        os++;
        foreach(ap.a[i])
          a[tmp++] = ap.a[i];
        ap_bytes -= 1;
      end
    end
      
    for(int i = 0; i < num_imms; i++) begin
      imms[i] = {ibuf[i+3], ibuf[i+2], ibuf[i+1], ibuf[i]};
      os += num_word_bytes;
    end
      
    ///allocate reg read address
    tmp = 0;
    
    for(int i = 0; i < cyc_vec; i++) begin
      for(int j = 0; j < num_vrf_bks; j++)
        if(vrf_rd_en[i][j]) begin
          map_iaddr(1, a[tmp], vrf_grp[i][j], vrf_adr[i][j]);
          tmp++;
        end

      for(int j = 0; j < num_srf_bks; j++)
        if(srf_rd_en[i][j]) begin
          map_iaddr(0, a[tmp], srf_grp[i][j], srf_adr[i][j]);
          tmp++;
        end
    end
    
    foreach(i_fu[fid]) begin
      i_fu[fid].set_wcnt(wcnt);
      decode_error |= i_fu[fid].decode_error;
    end
    i_spu.set_wcnt(wcnt);
    i_dse.set_wcnt(wcnt);
    decode_error |= i_spu.decode_error;
    decode_error |= i_dse.decode_error;
    
    decoded = 1;
    ovm_report_info("decode_ig", {"\n", sprint()}, OVM_HIGH);
  endfunction : decode_ig

  function void flush();
    ibuf_level = 0;
    pd_ifet = 0;
    igrp_bytes = 0;
    decoded = 0;
    decode_error = 0;
    if(pd_ifet != 0)
      ts = ts_w_ife;
  endfunction : flush
  
  function void retrieve_pc_l();
    flush();
    pc = pc_l;
   endfunction : retrieve_pc_l

  function void msg_wait();
  endfunction : msg_wait
  
  function bit br_pre_miss(input bit br);
    if(ts == ts_w_b)
      ts = ts_rdy;
    if(br == br_pred)
      return 0;
    return 1;
  endfunction : br_pre_miss

  function bit can_req_ifet();
    ovm_report_info("can_req_ifet", $psprintf("ts:%s, ibuf lv:%0d, pd:%0d", ts.name, ibuf_level, pd_ifet), OVM_HIGH);
    if(ts == ts_disabled)
      return 0;
    if(ibuf_level + pd_ifet * num_ifet_bytes >=  num_ibuf_bytes)
      return 0;
    if(igrp_bytes == 0)
      return 1;
    if(ibuf_level < igrp_bytes)
      return 1;
    return 0;
  endfunction : can_req_ifet
      
  function void update_inst(input inst_fg_c fg);
    uchar os = 0, level_l = ibuf_level;
    if(ibuf_level  >= num_max_igrp_bytes)
      ovm_report_warning("ISE", "ibuf overflow!");
    if(ibuf_level == 0)
      os = pc & ~{'1 << bits_ifet};

    if(pd_ifet > 0)
      pd_ifet--;
    
    if(ts == ts_w_ife) begin
      ovm_report_info("update_inst", $psprintf("ts_w_ife, pc:0x%0h, os:%0h, pd:%0d", pc, os, pd_ifet), OVM_HIGH);
      if(pd_ifet == 0)  ts = ts_rdy;
    end
    else
      ovm_report_info("update_inst", $psprintf("pc:0x%0h, os:%0h, pd:%0d, ibuf lv %0d->%0d", pc, os, pd_ifet, level_l, ibuf_level), OVM_HIGH);
      
    foreach(fg.data[i])
      if(i >= os)
        ibuf[ibuf_level++] = fg.data[i];
              
    if(ibuf_level > 1 && !decoded) begin
      decode_igs();
      if(ibuf_level >= igrp_bytes)
        decode_ig();
    end
  endfunction : update_inst

  function void fill_ife(input tr_ise2ife ife);
    ife.fetch_req = 1;
    ife.pc = (pc + num_ifet_bytes * pd_ifet) & ~{'1 << bits_ifet};
    pd_ifet++;
  endfunction : fill_ife
  
  function void fill_iss(ref tr_ise2rfm ci_rfm[cyc_vec], tr_ise2spa ci_spa[cyc_vec], 
                               tr_ise2spu ci_spu[cyc_vec], tr_ise2dse ci_dse[cyc_vec]);
    
    if(ci_rfm[0] == null) ci_rfm[0] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
    if(ci_rfm[cnt_vrf_rd] == null) ci_rfm[cnt_srf_rd] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
    ci_rfm[0].start = 1;
    ci_rfm[cnt_vrf_rd].vec_end = 1;

    foreach(i_fu[i])
      i_fu[i].map_wr_grp(vrf_map, srf_map);

    /// spu or scalar dse issue
    if(en_spu) begin
      i_spu.map_wr_grp(vrf_map, srf_map);
      if(ci_rfm[0] == null) ci_rfm[0] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
      if(ci_rfm[cnt_srf_rd] == null) ci_rfm[cnt_srf_rd] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
      ci_rfm[0].start = 1;
      ci_rfm[cnt_srf_rd].scl_end = 1;
    end
    
    if(en_dse) begin
      i_dse.map_wr_grp(vrf_map, srf_map);
      ci_dse[0] = tr_ise2dse::type_id::create("to_dse", get_parent());
      i_dse.fill_dse(ci_dse[0]);      
      for(int i = 0; i < cnt_dse_rd; i++) begin
        if(ci_rfm[i] == null) ci_rfm[i] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
        i_dse.fill_rfm(ci_rfm[i]);
      end
    end
          
    for(int i = 0; i < cnt_srf_rd && i < cnt_vrf_rd; i++) begin
      if(ci_rfm[i] == null) ci_rfm[i] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
      ci_rfm[i].bp_imm = imms;
      ci_rfm[i].vrf_rd_grp = vrf_grp[i];
      ci_rfm[i].vrf_rd_adr = vrf_adr[i];
      ci_rfm[i].srf_rd_grp = srf_grp[i];
      ci_rfm[i].srf_rd_adr = srf_adr[i];
    end
    
    for(int i = 0; i < cnt_srf_rd; i++) begin
      if(ci_rfm[i] == null) ci_rfm[i] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
      if(ci_spa[i] == null) ci_spa[i] = tr_ise2spa::type_id::create("to_spa", get_parent());
      i_spu.fill_rfm(ci_rfm[i]);
      i_spu.fill_spa(ci_spa[i]);
    end
    
    for(int i = 0; i < cnt_vrf_rd; i++) begin
      if(ci_spa[i] == null) ci_spa[i] = tr_ise2spa::type_id::create("to_spa", get_parent());
      if(ci_rfm[i] == null) ci_rfm[i] = tr_ise2rfm::type_id::create("to_rfm", get_parent());
      i_dse.fill_spa(ci_spa[i]);
      foreach(i_fu[fid]) begin
        i_fu[fid].fill_rfm(ci_rfm[i]);
        i_fu[fid].fill_spa(ci_spa[i]);
      end
      ci_spa[i].subv = i;
    end
  endfunction : fill_iss

endclass : ise_thread_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;

  local uchar cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd, cnt_vec_proc,
              cnt_pr_wr, cnt_srf_wr[num_srf_bks], cnt_vrf_wr[num_vrf_bks];
        
  local bit no_ld, no_st, no_smsg, no_rmsg, no_fu[num_fu];
  
  local tr_ise2rfm ci_rfm[cyc_vec];
  local tr_ise2spa ci_spa[cyc_vec];
  local tr_ise2spu ci_spu[cyc_vec];
  local tr_ise2dse ci_dse[cyc_vec];
  
  ovm_nonblocking_transport_imp_spu #(tr_spu2ise, tr_spu2ise, ip4_tlm_ise) spu_tr_imp;
  ovm_nonblocking_transport_imp_spa #(tr_spa2ise, tr_spa2ise, ip4_tlm_ise) spa_tr_imp;
  ovm_nonblocking_transport_imp_rfm #(tr_rfm2ise, tr_rfm2ise, ip4_tlm_ise) rfm_tr_imp;
  ovm_nonblocking_transport_imp_ife #(tr_ife2ise, tr_ife2ise, ip4_tlm_ise) ife_tr_imp;
  ovm_nonblocking_transport_imp_dse #(tr_dse2ise, tr_dse2ise, ip4_tlm_ise) dse_tr_imp;
  
  ovm_nonblocking_transport_port #(tr_ise2rfm, tr_ise2rfm) rfm_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spu, tr_ise2spu) spu_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2spa, tr_ise2spa) spa_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2ife, tr_ise2ife) ife_tr_port;
  ovm_nonblocking_transport_port #(tr_ise2dse, tr_ise2dse) dse_tr_port;

  local ip4_tlm_ise_vars v, vn;
  local ise_thread_inf tinf[num_thread];
  local ip4_printer printer;
  
  `ovm_component_utils_begin(ip4_tlm_ise)
    `ovm_field_int(cnt_vec_proc, OVM_ALL_ON)
    `ovm_field_int(cnt_vrf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_srf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_dse_rd, OVM_ALL_ON)
    `ovm_field_int(no_ld, OVM_ALL_ON)
    `ovm_field_int(no_st, OVM_ALL_ON)
    `ovm_field_int(no_smsg, OVM_ALL_ON)
    `ovm_field_int(no_rmsg, OVM_ALL_ON)
    `ovm_field_sarray_int(no_fu, OVM_ALL_ON)
    `ovm_field_int(cnt_pr_wr, OVM_ALL_ON)
    `ovm_field_sarray_int(cnt_srf_wr, OVM_ALL_ON)
    `ovm_field_sarray_int(cnt_vrf_wr, OVM_ALL_ON)
///    `ovm_field_object(v, OVM_ALL_ON + OVM_NOPRINT)
///    `ovm_field_object(vn, OVM_ALL_ON + OVM_NOPRINT)
///    `ovm_field_sarray_object(tinf, OVM_ALL_ON + OVM_NOPRINT)
  `ovm_component_utils_end
  
  function void enter_exp(input uchar tid, ise_exp_t err);
    ise_thread_inf tif = tinf[tid];
    tif.priv_mode = 1;
    tif.pc = v.sr.ebase;
    case(err)
    exp_decode_err  : begin end
    exp_dse_err     : begin end
    exp_priv_err    : begin end
    endcase
    tif.flush();
  endfunction : enter_exp
  
  function void exe_priv(input inst_c i);
  endfunction : exe_priv
  
  function void update_block(input tr_spa2ise spa, tr_dse2ise dse);
    if(spa != null)
      no_fu = spa.no_fu;
    if(dse == null)
      return;
    no_ld = dse.no_ld;
    no_st = dse.no_st;
    no_smsg = dse.no_smsg;
    no_rmsg = dse.no_rmsg;
  endfunction : update_block

  function bit can_iss(input uchar tid);
    /// the vec value indicate 4 cyc issue style is needed
///    vec = tif.dse_vec;
    ise_thread_inf tif = tinf[tid];
    if(get_report_verbosity_level() >= OVM_HIGH) begin
      bit [num_fu-1:0] en_fu_t;
      foreach(en_fu_t[i])
        en_fu_t[i] = tif.en_fu[i];
        
      ovm_report_info("can_iss",
        $psprintf("ts:%s, decoded:%0d, err:%0d, wcnt:%0d, pc:%0h spu:%0b, dse:%0b, fu:%b. dv:%0b, nc:%0b", 
                   tif.ts.name, tif.decoded, tif.decode_error, tif.wcnt, tif.pc, tif.en_spu, tif.en_dse,
                   en_fu_t, tif.dse_vec, tif.nc),
        OVM_HIGH);
    end
    
    if(tif.en_vec && cnt_vec_proc >= tif.vec_mode)
      return 0;
      
    ///issue disable check
    if(tif.i_dse.dse_block(no_ld, no_st, no_smsg, no_rmsg))
      return 0;
    
    foreach(no_fu[i])
      if(no_fu[i] && tif.en_fu[i])
        return 0;
    
    ///read cyc check
    if(cnt_srf_rd > 0 && tif.cnt_srf_rd > 0)
      return 0;

    if(cnt_vrf_rd > 0 && tif.cnt_vrf_rd > 0)
      return 0;

    if(cnt_dse_rd > 0 && tif.cnt_dse_rd > 0)
      return 0;

    /// write buf overflow check
    if(cnt_pr_wr + tif.cnt_pr_wr > cyc_vec)
      return 0;
    
    foreach(cnt_vrf_wr[i])
      if(cnt_vrf_wr[i] + tif.cnt_vrf_wr[i] > cyc_vec)
        return 0;      

    foreach(cnt_srf_wr[i])
      if(cnt_srf_wr[i] + tif.cnt_srf_wr[i] > cyc_vec)
        return 0;
    return tif.decoded && ((tif.ts == ts_rdy) || (tif.ts == ts_w_pip && tif.nc));
  endfunction : can_iss

  function void issue(input uchar tid);
    ise_thread_inf tif = tinf[tid];
    if(tif.decode_error) begin
      enter_exp(tid, exp_decode_err);
      return;
    end
    
    tif.pc += tif.igrp_bytes;
    tif.ibuf_level -= tif.igrp_bytes;
    tif.fill_iss(ci_rfm, ci_spa, ci_spu, ci_dse);
    tif.decoded = 0;
    tif.decode_error = 0;
    
    if(tif.wcnt > 0)
      tif.ts = ts_w_pip;
    if(tif.en_spu && tif.i_spu.is_pd_br())
      tif.ts = ts_w_b;
              
    /// spu or scalar dse issue
    if(tif.en_spu) begin
      if(tif.i_spu.is_priv()) begin
        if(tif.priv_mode) begin
          exe_priv(tif.i_spu);
          tif.exe_priv();
        end
        else
          enter_exp(tid, exp_priv_err);
      end
    end
    
    if(tif.en_dse) begin
      cnt_dse_rd = tif.cnt_dse_rd;
    end
    
    cnt_srf_rd = tif.cnt_srf_rd;
    cnt_vrf_rd = tif.cnt_vrf_rd;
    cnt_dse_rd = tif.cnt_dse_rd;
    cnt_pr_wr += tif.cnt_pr_wr;
  
    foreach(cnt_vrf_wr[i])
      cnt_vrf_wr[i] += tif.cnt_vrf_wr[i];

    foreach(cnt_srf_wr[i])
      cnt_srf_wr[i] += tif.cnt_srf_wr[i];
    
    if(tif.en_vec)
      cnt_vec_proc = tif.vec_mode;
  endfunction : issue
      
  function void comb_proc();
///    bit spu_from_vec = 0;
    
    ovm_report_info("ISE", "comb_proc procing...", OVM_FULL); 
    
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_rfm != null) end_tr(v.fm_rfm);
    if(v.fm_ife != null) end_tr(v.fm_ife);
    if(v.fm_dse[stage_exe_dwbp] != null) end_tr(v.fm_dse[stage_exe_dwbp]);
    
    vn.fm_spu = null;
    vn.fm_spa = null;
    vn.fm_rfm = null;
    vn.fm_ife = null;
    vn.fm_dse[stage_exe_dwbp] = null;
    
    for(int i = stage_ise; i > 1; i--) begin
      vn.rfm[i] = v.rfm[i-1];  
      vn.spa[i] = v.spa[i-1];
      vn.spu[i] = v.spu[i-1];
      vn.dse[i] = v.dse[i-1];
    end
    vn.rfm[1] = null; 
    vn.spa[1] = null;
    vn.spu[1] = null;
    vn.dse[1] = null;
          
    foreach(tinf[i])
      tinf[i].cyc_new();
    
    vn.ife_cancel = 0;
    
    for(int i = 0; i < (cyc_vec - 1); i++) begin
      ci_rfm[i] = ci_rfm[i+1];
      ci_spa[i] = ci_spa[i+1];
      ci_spu[i] = ci_spu[i+1];
      ci_dse[i] = ci_dse[i+1];
    end

    ci_rfm[cyc_vec-1] = null;
    ci_spa[cyc_vec-1] = null;
    ci_spu[cyc_vec-1] = null;
    ci_dse[cyc_vec-1] = null;
    
    if(cnt_vrf_rd != 0) cnt_vrf_rd--;
    if(cnt_dse_rd != 0) cnt_dse_rd--;
    if(cnt_srf_rd != 0) cnt_srf_rd--;
    if(cnt_pr_wr != 0) cnt_pr_wr--;
    if(cnt_vec_proc != 0) cnt_vec_proc--;
    
    foreach(cnt_srf_wr[i])
      if(cnt_srf_wr[i] != 0) cnt_srf_wr[i]--;
    foreach(cnt_vrf_wr[i])
      if(cnt_vrf_wr[i] != 0) cnt_vrf_wr[i]--;
    
    no_fu = '{default: 0};
    no_ld = 0;
    no_st = 0;
    no_smsg = 0;
    no_rmsg = 0;
    
    if(v.fm_spu != null && v.fm_spu.br_rsp && tinf[v.fm_spu.tid].br_pre_miss(v.fm_spu.br_taken)) begin
      tinf[v.fm_spu.tid].retrieve_pc_l();
      if(v.fm_ife != null && v.fm_ife.tid == v.fm_spu.tid)
        v.fm_ife.inst_en = 0;
    end

    if(v.fm_ife != null && v.fm_ife.tid == v.tid_ife_cancel && v.ife_cancel)
      v.fm_ife.inst_en = 0;
                
    for(int i = stage_exe_vwbp; i > stage_exe_dwbp; i--)
      vn.fm_dse[i] = v.fm_dse[i-1];  
    
    update_block(v.fm_spa, v.fm_dse[stage_exe_dwbp]);
    
    if(v.fm_ife != null && v.fm_ife.inst_en)
      tinf[v.fm_ife.tid].update_inst(v.fm_ife.fg);
    if(v.fm_dse[stage_exe_dwbp] != null) begin
      tr_dse2ise dse = v.fm_dse[stage_exe_dwbp];
      if(dse.cancel) begin
        tinf[dse.tid].retrieve_pc_l();
        if(dse.exp)
          enter_exp(dse.tid, exp_dse_err);
      end
      if(dse.msg_wait) begin
        tinf[dse.tid].msg_wait();
      end
    end
    
    ovm_report_info("iinf", $psprintf("\n%s", sprint(printer)), OVM_HIGH);
    for(int i = 1; i <= num_thread; i++) begin
      uchar tid = i + v.tid_iss_l;
      tid = tid & ~('1 << bits_tid);
      
      ovm_report_info("issue", $psprintf("checking thread %0d", tid), OVM_HIGH);
      if(can_iss(tid)) begin
        ovm_report_info("issue", $psprintf("issuing thread %0d", tid), OVM_HIGH);
        issue(tid);
        vn.tid_iss_l = tid;
        break;
      end
    end
    
  endfunction
  
  function void req_proc();
    tr_ise2rfm to_rfm;
    tr_ise2spu to_spu;
    tr_ise2spa to_spa;
    tr_ise2ife to_ife;
    tr_ise2dse to_dse;
    
    ovm_report_info("ISE", "req_proc procing...", OVM_FULL); 
    
    vn.rfm[1] = ci_rfm[0];
    vn.spa[1] = ci_spa[0];
    vn.spu[1] = ci_spu[0];
    vn.dse[1] = ci_dse[0];  
        
    to_rfm = v.rfm[stage_ise];
    to_spa = v.spa[stage_ise];
    to_spu = v.spu[stage_ise];
    to_dse = v.dse[stage_ise];
    
    for(int i = 1; i <= num_thread; i++) begin
      uchar tid = i + v.tid_fet_l;
      tid = tid & ~('1 << bits_tid);
      if(tinf[tid].can_req_ifet()) begin
        to_ife = tr_ise2ife::type_id::create("to_ife", this);
        tinf[tid].fill_ife(to_ife);
        to_ife.tid = tid;
        vn.tid_fet_l = tid;
        break;
      end
    end
    
    foreach(tinf[i])
      if(tinf[i].ts == ts_w_ife) begin
        if(to_ife == null) to_ife = tr_ise2ife::type_id::create("to_ife", this);
        to_ife.cancel = 1;
        to_ife.tid_cancel = i;
        vn.ife_cancel = 1;
        vn.tid_ife_cancel = i;
        tinf[i].ts = ts_rdy;
      end
    
    if(v.fm_dse[stage_exe_vwbp] != null && v.fm_dse[stage_exe_vwbp].cancel) begin
      tr_dse2ise dse = v.fm_dse[stage_exe_vwbp];
      if(to_spa != null) to_spa = tr_ise2spa::type_id::create("to_spa", this);
      to_spa.tid_cancel = dse.tid;
      to_spa.cancel = 1;
    end
      
///    iinf.cyc_new();
    
    ///------------req to other module----------------
    if(to_rfm != null) void'(rfm_tr_port.nb_transport(to_rfm, to_rfm));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
    if(to_spa != null) void'(spa_tr_port.nb_transport(to_spa, to_spa));
    if(to_ife != null) void'(ife_tr_port.nb_transport(to_ife, to_ife));
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ife(input tr_ife2ise req, output tr_ife2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get IFE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    end_tr(req);
    rsp = req;
    vn.fm_ife = req;
    return 1;
  endfunction : nb_transport_ife

  function bit nb_transport_spu(input tr_spu2ise req, output tr_spu2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get SPU Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2ise req, output tr_spa2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get SPA Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spa = req;
    return 1;
  endfunction : nb_transport_spa
  
  function bit nb_transport_rfm(input tr_rfm2ise req, output tr_rfm2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get RFM Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_dse(input tr_dse2ise req, output tr_dse2ise rsp);
    ovm_report_info("ISE_TR", $psprintf("Get DSE Transaction:\n%s", req.sprint()), OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse[stage_exe_dwbp] = req;
    return 1;
  endfunction : nb_transport_dse
    
///-------------------------------------common functions-----------------------------------------    
  function void sync();
///    ip4_tlm_ise_vars t;
    if($time==stamp) begin
       ovm_report_info("SYNC", $psprintf("sync already called. stamp is %0t", stamp), OVM_FULL);
       return;
     end
    stamp = $time;
    ovm_report_info("SYNC", $psprintf("synchronizing... stamp set to %0t", stamp), OVM_FULL);
    ///--------------------synchronizing-------------------
///    t = v;
///    v = vn;
///    vn = t;
///    vn.gen(v);
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
    tlm_vif_object vif_cfg;
    
    super.build();
    ife_tr_imp = new("ife_tr_imp", this);
    spu_tr_imp = new("spu_tr_imp", this);
    spa_tr_imp = new("spa_tr_imp", this);
    rfm_tr_imp = new("rfm_tr_imp", this);
    dse_tr_imp = new("dse_tr_imp", this);
        
    ife_tr_port = new("ife_tr_port", this);
    rfm_tr_port = new("rfm_tr_port", this);
    spu_tr_port = new("spu_tr_port", this);
    spa_tr_port = new("spa_tr_port", this);
    dse_tr_port = new("dse_tr_port", this);

///    ife_tr_imp.print_enabled = 0;
///    spu_tr_imp.print_enabled = 0;
///    spa_tr_imp.print_enabled = 0;
///    rfm_tr_imp.print_enabled = 0;
///    dse_tr_imp.print_enabled = 0;
///        
///    ife_tr_port.print_enabled = 0;
///    rfm_tr_port.print_enabled = 0;
///    spu_tr_port.print_enabled = 0;
///    spa_tr_port.print_enabled = 0;
///    dse_tr_port.print_enabled = 0;
        
    v = new("v", this);
    vn = new("vn", this);
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
    
    foreach(tinf[i])
      tinf[i] = new($psprintf("tinf%0d", i), this);
    tinf[0].ts = ts_rdy;
    tinf[0].priv_mode = 1;
    
    cnt_vrf_rd = 0;
    cnt_srf_rd = 0;
    cnt_dse_rd = 0;
    cnt_pr_wr = 0;
    cnt_srf_wr[num_srf_bks] = '{default: 0};
    cnt_vrf_wr[num_vrf_bks] = '{default: 0};
    
    printer = new();
    printer.knobs.depth = 1;
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
