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
  ts_disabled,    ts_rdy,     ts_w_sfu,     ts_w_ls,
  ts_w_msg,       ts_w_spu,   ts_w_b,       ts_w_pip
}ise_thread_state;

class ip4_tlm_ise_vars extends ovm_object;
  tr_spu2ise fm_spu;
  tr_rfm2ise fm_rfm;
  tr_ife2ise fm_ife;
  tr_spa2ise fm_spa;
  tr_dse2ise fm_dse[stage_exe_vwbp:stage_exe_dwb];
  
  tr_ise2rfm rfm[stage_ise_rrf:1];
  tr_ise2spa spa[stage_ise_rrf:1];
  tr_ise2spu spu[stage_ise_rrf:1];
  tr_ise2dse dse[stage_ise_rrf:1];
  
  uchar tid_iss_l, tid_fet_l;
    
  `ovm_object_utils_begin(ip4_tlm_ise_vars)
    `ovm_field_object(fm_spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_spa, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_object(fm_ife, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(fm_dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(rfm, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spa, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(spu, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_sarray_object(dse, OVM_ALL_ON + OVM_REFERENCE)
    `ovm_field_int(tid_iss_l, OVM_ALL_ON)
    `ovm_field_int(tid_fet_l, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function new (string name = "ise_vars");
    super.new(name);
    tid_fet_l = 0;
    tid_iss_l = 0;    
  endfunction : new
  
  function void gen(input ip4_tlm_ise_vars o);
    this.copy(o);
  endfunction
endclass : ip4_tlm_ise_vars

class ise_thread_inf extends ovm_object;
  ise_thread_state ts;
  uchar ibuf[num_ibuf_bytes];
  bit nck, dse_vec, no_sp_rot;
  uchar ibuf_level, igrp_bytes, ap_bytes, num_imms,
        cnt_srf_rd, cnt_vrf_rd, cnt_dse_rd;
  word imms[num_bp_imm];
  uchar vrf_adr[cyc_vec][num_vrf_bks], vrf_grp[cyc_vec][num_srf_bks],
        srf_adr[cyc_vec][num_vrf_bks], srf_grp[cyc_vec][num_srf_bks];
  bit vrf_rd_en[cyc_vec][num_vrf_bks], srf_rd_en[cyc_vec][num_srf_bks];
  uchar cnt_pr_wr, cnt_vrf_wr[num_vrf_bks], cnt_srf_wr[num_srf_bks];
  
  bit en_spu, en_dse, en_fu[num_fu];
///  bit sgl_mode;
  bit pri_state;  ///privilege running status
  uchar wcnt, vec_mode;
  
  uchar vrf_map[num_inst_vrf/num_prf_p_grp], 
        srf_map[num_inst_srf/num_prf_p_grp];
  uchar pd_ifet;
  
  inst_c i_spu, i_dse, i_fu[num_fu];
  uint pc, pc_l;  ///pc_br
  bit br_pred;
  uchar ife_pend;
    
  `ovm_object_utils_begin(ise_thread_inf)
    `ovm_field_enum(ise_thread_state, ts, OVM_ALL_ON)
    `ovm_field_int(pri_state, OVM_ALL_ON)
    `ovm_field_sarray_int(ibuf, OVM_ALL_ON)
    `ovm_field_int(nck, OVM_ALL_ON)
    `ovm_field_int(no_sp_rot, OVM_ALL_ON)
    `ovm_field_int(ibuf_level, OVM_ALL_ON)
///    `ovm_field_int(sgl_mode, OVM_ALL_ON)
    `ovm_field_int(pc, OVM_ALL_ON)
    `ovm_field_int(pc_l, OVM_ALL_ON)
    `ovm_field_int(br_pred, OVM_ALL_ON)
    `ovm_field_int(ife_pend, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function new (string name = "ise_thread_inf");
    super.new(name);
    ts = ts_disabled;
    pri_state = 0;
    pc = cfg_start_adr;
    pc_l = cfg_start_adr;
  endfunction : new
 
  function void map_vreg(input uchar oadr, output uchar grp, adr);
    uchar adr_bits =  bits_prf_p_grp - bits_vrf_bks;
    adr = oadr & ('1 << adr_bits);
    grp = oadr >> adr_bits;
    adr = vrf_map[adr];
  endfunction : map_vreg

  function void map_sreg(input uchar oadr, output uchar grp, adr);
    uchar adr_bits =  bits_prf_p_grp - bits_srf_bks;
    adr = oadr & ('1 << adr_bits);
    grp = oadr >> adr_bits;
    adr = srf_map[adr];    
  endfunction : map_sreg
  
  function void cyc_new();
    if(wcnt != 0) wcnt--;
    if(ts == ts_w_pip)
      ts = ts_rdy;
  endfunction
  
  function void decode_igs();
    i_gs1_t gs1 = ibuf[0];
    en_spu = 0;
    en_dse = 0;
    en_fu = '{default : 0};
    
    if(gs1.t) begin
///      sgl_mode = 1;
      nck = gs1.nc;
      ap_bytes = gs1.apb;
      num_imms = gs1.ipw;
      dse_vec = gs1.fua;
      igrp_bytes = 1 + ap_bytes + num_imms * num_word_bytes;
    end
    else begin
      i_gs0_u gs;
      uchar tmp = 0;
///      sgl_mode = 0;
      foreach(gs.b[i])
        gs.b[i] = ibuf[i];
///      sgl_mode = 0;
      foreach(gs.i.fua[i])
        tmp += gs.i.fua[i];
      if(tmp == 0)
        ovm_report_warning("ISE", "igs decode error, fua not valid");
      nck = gs1.nc;
      ap_bytes = gs.i.apb;
      num_imms = gs.i.ipw;
      igrp_bytes = 2 + ap_bytes + num_imms * num_word_bytes;
      en_spu = gs.i.fua[0];
      en_dse = gs.i.fua[1];
      dse_vec = gs.i.dv;
      foreach(en_fu[i])
        en_fu[i] = gs.i.fua[2+i];
    end
  endfunction : decode_igs
    
  function void decode_ig();
///    decode_igs();  /// no need
    uchar tmp = 0;
    iga_t a[12];
    uchar os;
    i_gs1_t gs1 = ibuf[0];
    
    vrf_rd_en = '{default : 0};
    srf_rd_en = '{default : 0};
    dse_vec = 0;
    cnt_vrf_rd = 0;
    cnt_srf_rd = 0;
    cnt_dse_rd = 0;
    cnt_vrf_wr = '{default : 0};
    cnt_srf_wr = '{default : 0};
    cnt_pr_wr = 0;
    
    if(gs1.t) begin
      tmp = 1;
      if(ap_bytes != 0) ap_bytes --;
      i_spu.set_data(ibuf, os);
      i_dse.set_data(ibuf, os);
      foreach(i_fu[i])
        i_fu[i].set_data(ibuf, os, i);
        
      os = 1 + num_inst_bytes;
      i_spu.analyze_rs(dse_vec, vrf_rd_en, srf_rd_en, dse_vec, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
      i_spu.analyze_rd(dse_vec, cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
      i_spu.analyze_fu(dse_vec, en_spu, en_dse, en_fu);
      a[0] = gs1.a;
      if(ap_bytes) begin
        i_ap0_t ap = ibuf[os];
        foreach(a[i])
          a[i] = ap.a[i];
        os ++;
      end
    end
    else begin
      bit tmp_en_spu = en_spu, tmp_en_dse = en_dse, tmp_en_fu[num_fu] = en_fu;
      i_gs0_u gs;
      gs.b[0] = ibuf[0];
      gs.b[1] = ibuf[1];
      os = 2;
      tmp = 1;
      if(ap_bytes != 0) ap_bytes --;
      
      if(en_spu) begin
        i_spu.set_data(ibuf, os);
///        i_spu.analyze_fu(0, 0, tmp_en_spu, tmp_en_dse, tmp_en_fu);
        i_spu.analyze_rs(0, vrf_rd_en, srf_rd_en, dse_vec, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
        i_spu.analyze_rd(0, cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
        os += num_inst_bytes;
      end
      
      if(en_dse) begin
        i_dse.set_data(ibuf, os);
///        i_dse.analyze_fu(dse_vec, 0, tmp_en_spu, tmp_en_dse, tmp_en_fu);
        i_dse.analyze_rs(dse_vec, vrf_rd_en, srf_rd_en, dse_vec, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
        i_dse.analyze_rd(dse_vec, cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
        os += num_inst_bytes;
      end
      
      foreach(i_fu[i])
        if(en_fu[i]) begin
          i_fu[i].set_data(ibuf, os, i);
///          i_spu.analyze_fu(1, 0, tmp_en_spu, tmp_en_dse, tmp_en_fu);
          i_fu[i].analyze_rs(1, vrf_rd_en, srf_rd_en, dse_vec, cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd);
          i_spu.analyze_rd(1, cnt_vrf_wr, cnt_srf_wr, cnt_pr_wr);
          os += num_inst_bytes;          
        end
      
      en_fu = tmp_en_fu;
      en_dse = tmp_en_dse;
      en_spu = tmp_en_spu;
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
          map_vreg(a[tmp], vrf_grp[i][j], vrf_adr[i][j]);
          tmp++;
        end

      for(int j = 0; j < num_srf_bks; j++)
        if(srf_rd_en[i][j]) begin
          map_sreg(a[tmp], vrf_grp[i][j], vrf_adr[i][j]);
          tmp++;
        end
    end
  endfunction : decode_ig

  function void dse_cancel();
    ibuf_level = 0;
    pd_ifet = 0;
    igrp_bytes = 0;
    pc = pc_l;
  endfunction : dse_cancel

  function bit br_pre_miss(input bit br);
    if(ts == ts_w_b)
      ts = ts_rdy;
    if(br == br_pred)
      return 0;
    dse_cancel();
    return 1;
  endfunction : br_pre_miss

  function bit can_req_ifet();
    if(igrp_bytes == 0 && ts != ts_disabled)
      return 1;
    if(ibuf_level < igrp_bytes)
      return 1;
    return 0;
  endfunction : can_req_ifet
      
  function void fill_ife_req(input tr_ise2ife ife);
    ife.fetch_req = 1;
    pd_ifet++;
    ife.pc = (pc + num_ifet_bytes * pd_ifet) & {'1 << bits_ifet};
  endfunction : fill_ife_req

  function void update_inst(input inst_fg_c fg);
    uchar os = 0;
    if(ibuf_level  >= num_max_igrp_bytes)
      ovm_report_warning("ISE", "ibuf overflow!");
    if(ibuf_level == 0)
      os = pc & ~{'1 << bits_ifet};
    foreach(fg.data[i])
      if(i > os)
        ibuf[ibuf_level++] = fg.data[i];
    decode_igs();
    if(ibuf_level >= igrp_bytes)
      decode_ig();
    if(pd_ifet > 0)
      pd_ifet--;
  endfunction : update_inst
endclass : ise_thread_inf

class ise_iss_inf extends ovm_object;
  ovm_component pa;
  uchar cnt_vrf_rd, cnt_srf_rd, cnt_dse_rd, cnt_vec_proc,
        cnt_pr_wr,  cnt_srf_wr[num_srf_bks], cnt_vrf_wr[num_vrf_bks];
        
  bit no_ld, no_st, no_smsg, no_rmsg, no_fu[num_fu];
  
  tr_ise2rfm ci_rfm[cyc_vec];
  tr_ise2spa ci_spa[cyc_vec];
  tr_ise2spu ci_spu[cyc_vec];
  tr_ise2dse ci_dse[cyc_vec];
    
  `ovm_object_utils_begin(ise_iss_inf)
    `ovm_field_int(cnt_vrf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_srf_rd, OVM_ALL_ON)
    `ovm_field_int(cnt_dse_rd, OVM_ALL_ON)
    `ovm_field_sarray_object(ci_rfm, OVM_ALL_ON)
    `ovm_field_sarray_object(ci_spa, OVM_ALL_ON)
    `ovm_field_sarray_object(ci_spu, OVM_ALL_ON)
    `ovm_field_sarray_object(ci_dse, OVM_ALL_ON)
  `ovm_object_utils_end
  
  function new (string name = "ise_iss_inf", ovm_component p = null);
    super.new(name);
    pa = p;
    foreach(ci_spa[i]) begin
      ci_rfm[i] = tr_ise2rfm::type_id::create("to_rfm", pa);
      ci_dse[i] = tr_ise2dse::type_id::create("to_dse", pa);
      ci_spa[i] = tr_ise2spa::type_id::create("to_spa", pa);
      ci_spu[i] = tr_ise2spu::type_id::create("to_spu", pa);
    end
    cnt_vrf_rd = 0;
    cnt_srf_rd = 0;
    cnt_dse_rd = 0;
    cnt_pr_wr = 0;
    cnt_srf_wr[num_srf_bks] = '{default: 0};
    cnt_vrf_wr[num_vrf_bks] = '{default: 0};
  endfunction : new

  function void cyc_new();
    for(int i = 0; i < (cyc_vec - 1); i++) begin
      ci_rfm[i] = ci_rfm[i+1];
      ci_spa[i] = ci_spa[i+1];
      ci_spu[i] = ci_spu[i+1];
      ci_dse[i] = ci_dse[i+1];
    end
    
    ci_rfm[cyc_vec-1] = tr_ise2rfm::type_id::create("to_rfm", pa);
    ci_spa[cyc_vec-1] = tr_ise2spa::type_id::create("to_spa", pa);
    ci_spu[cyc_vec-1] = tr_ise2spu::type_id::create("to_spu", pa);
    ci_dse[cyc_vec-1] = tr_ise2dse::type_id::create("to_dse", pa);
    
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
  endfunction : cyc_new

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
  
  function void get_tr(input tr_ise2rfm rfm, tr_ise2spa spa, tr_ise2spu spu, tr_ise2dse dse);
    rfm = ci_rfm[0];
    spa = ci_spa[0];
    spu = ci_spu[0];
    dse = ci_dse[0];  
  endfunction : get_tr

  function bit can_iss(input ise_thread_inf tif, output bit vec);
    /// the vec value indicate 4 cyc issue style is needed
///    vec = tif.dse_vec;
    foreach(tif.en_fu[i])
      vec |= tif.en_fu[i];
    
    if(vec && cnt_vec_proc >= tif.vec_mode)
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
    return 1;
  endfunction : can_iss

  function void iss_scl(input ise_thread_inf tif);
  /// spu or scalar dse issue
    if(tif.en_spu) begin
      for(int i = 0; i < tif.cnt_srf_rd; i++) begin
        tif.i_dse.fill_rfm(ci_rfm[i]);
        tif.i_dse.fill_spa(ci_spa[i]);
      end
      ci_rfm[0].start = 1;
      ci_rfm[tif.cnt_srf_rd].scl_end = 1;
    end
    
    if(tif.en_dse) begin
      tif.i_dse.fill_dse(ci_dse[0]);
      for(int i = 0; i < tif.cnt_dse_rd; i++)
        tif.i_dse.fill_rfm(ci_rfm[i]);
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
  endfunction : iss_scl
      
  function void iss_vec(input ise_thread_inf tif);
    for(int i = 0; i < tif.cnt_vrf_rd; i++) begin
      foreach(tif.i_fu[fid]) begin
        tif.i_fu[fid].fill_rfm(ci_rfm[i]);
        tif.i_fu[fid].fill_spa(ci_spa[i]);
      end
      ci_spa[i].subv = i;
    end
    ci_rfm[0].start = 1;
    ci_rfm[tif.cnt_vrf_rd].vec_end = 1;
    cnt_vec_proc = tif.vec_mode;
    
    iss_scl(tif);
  endfunction : iss_vec

endclass : ise_iss_inf

///---------------------------------------main component----------------------------------------
class ip4_tlm_ise extends ovm_component;
  
  virtual tlm_sys_if.mods sysif;
  local time stamp;
  
  `ovm_component_utils_begin(ip4_tlm_ise)
  `ovm_component_utils_end
      
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
  local ise_iss_inf iinf;
    
  function void comb_proc();
///    bit spu_from_vec = 0;
    
    ovm_report_info("ISE", "comb_proc procing...", OVM_HIGH); 
    
    if(v.fm_spu != null) end_tr(v.fm_spu);
    if(v.fm_spa != null) end_tr(v.fm_spa);
    if(v.fm_rfm != null) end_tr(v.fm_rfm);
    if(v.fm_ife != null) end_tr(v.fm_ife);
    if(v.fm_dse[stage_exe_dwb] != null) end_tr(v.fm_dse[stage_exe_dwb]);
    
    vn.fm_spu = null;
    vn.fm_spa = null;
    vn.fm_rfm = null;
    vn.fm_ife = null;
    vn.fm_dse[stage_exe_dwb] = null;
    
    for(int i = stage_ise_rrf; i > 1; i--) begin
      vn.rfm[i] = v.rfm[i];  
      vn.spa[i] = v.spa[i];
      vn.spu[i] = v.spu[i];
    end
    
    foreach(tinf[i])
      tinf[i].cyc_new();
      
    if(v.fm_spu != null && v.fm_spu.br_rsp)
      void'(tinf[v.fm_spu.tid].br_pre_miss(v.fm_spu.br_taken));
      
    for(int i = stage_exe_dwb; i > stage_exe_vwbp; i--)
      vn.fm_dse[i] = v.fm_dse[i];  
    
    iinf.update_block(v.fm_spa, v.fm_dse[stage_exe_dwb]);
    
    if(v.fm_ife != null && v.fm_ife.inst_en)
      tinf[v.fm_ife.tid].update_inst(v.fm_ife.fg);
    if(v.fm_dse[stage_exe_dwb] != null && v.fm_dse[stage_exe_dwb].cancel)
      tinf[v.fm_dse[stage_exe_dwb].tid].dse_cancel();
            
    for(int i = 1; i <= num_thread; i++) begin
        uchar tid = i + v.tid_iss_l;
        bit vec;
        if(iinf.can_iss(tinf[tid], vec)) begin
          if(vec)
            iinf.iss_vec(tinf[tid]);
          else
            iinf.iss_scl(tinf[tid]);
            
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
    
    ovm_report_info("ISE", "req_proc procing...", OVM_HIGH); 
    
    iinf.get_tr(vn.rfm[1], vn.spa[1], vn.spu[1], vn.dse[1]);
    
    to_rfm = vn.rfm[stage_ise_rrf];
    to_spa = vn.spa[stage_ise_rrf];
    to_spu = vn.spu[stage_ise_rrf];
    to_dse = vn.dse[stage_ise_rrf];
    
    for(int i = 1; i <= num_thread; i++) begin
      uchar tid = i + v.tid_fet_l;
      if(tinf[tid].can_req_ifet()) begin
        to_ife = tr_ise2ife::type_id::create("to_ife", this);
        tinf[tid].fill_ife_req(to_ife);
        to_ife.tid = tid;
        vn.tid_fet_l = tid;
        break;
      end
    end
    
    if(v.fm_dse[stage_exe_vwbp] != null && v.fm_dse[stage_exe_vwbp].cancel) begin
      tr_dse2ise dse = v.fm_dse[stage_exe_vwbp];
      if(to_spa != null) to_spa = tr_ise2spa::type_id::create("to_spa", this);
      to_spa.tid_cancel = dse.tid;
      to_spa.cancel = 1;
    end
      
    iinf.cyc_new();
    
    ///------------req to other module----------------
    if(to_rfm != null) void'(rfm_tr_port.nb_transport(to_rfm, to_rfm));
    if(to_spu != null) void'(spu_tr_port.nb_transport(to_spu, to_spu));
    if(to_spa != null) void'(spa_tr_port.nb_transport(to_spa, to_spa));
    if(to_ife != null) void'(ife_tr_port.nb_transport(to_ife, to_ife));
    if(to_dse != null) void'(dse_tr_port.nb_transport(to_dse, to_dse));
  endfunction

///------------------------------nb_transport functions---------------------------------------
  function bit nb_transport_ife(input tr_ife2ise req, output tr_ife2ise rsp);
    ovm_report_info("ISE_TR", "Get IFE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    end_tr(req);
    rsp = req;
    vn.fm_ife = req;
    return 1;
  endfunction : nb_transport_ife

  function bit nb_transport_spu(input tr_spu2ise req, output tr_spu2ise rsp);
    ovm_report_info("ISE_TR", "Get SPU Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spu = req;
    return 1;
  endfunction : nb_transport_spu

  function bit nb_transport_spa(input tr_spa2ise req, output tr_spa2ise rsp);
    ovm_report_info("ISE_TR", "Get SPA Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_spa = req;
    return 1;
  endfunction : nb_transport_spa
  
  function bit nb_transport_rfm(input tr_rfm2ise req, output tr_rfm2ise rsp);
    ovm_report_info("ISE_TR", "Get RFM Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_rfm = req;
    return 1;
  endfunction : nb_transport_rfm

  function bit nb_transport_dse(input tr_dse2ise req, output tr_dse2ise rsp);
    ovm_report_info("ISE_TR", "Get DSE Transaction...", OVM_HIGH);
    sync();
    assert(req != null);
    void'(begin_tr(req));
    rsp = req;
    vn.fm_dse[stage_exe_dwb] = req;
    return 1;
  endfunction : nb_transport_dse
    
///-------------------------------------common functions-----------------------------------------    
  function void sync();
    ip4_tlm_ise_vars t;
    if($time==stamp) begin
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
    
    v = new();
    vn = new();
    
    no_virtual_interface: assert(get_config_object("vif_cfg", tmp));
    failed_convert_interface: assert($cast(vif_cfg, tmp));
    sysif = vif_cfg.get_vif();  
    stamp = 0ns;
    
    foreach(tinf[i])
      tinf[i] = new();
    tinf[0].ts = ts_rdy;
    tinf[0].pri_state = 1;
    
    iinf = new(, this);
  endfunction : build
endclass : ip4_tlm_ise

///-------------------------------------other functions-----------------------------------------
