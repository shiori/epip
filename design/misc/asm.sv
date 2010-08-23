///assembler for ip4
`define asm_msg(s, i = OVM_LOW, d = display)\
if(verb >= i)\
$d(s);

`define asm_err(s) $display({"Err: ", string'(s)});

function automatic int get_imm(string tk);
  string tk0 = tk.substr(0, 0);
  string tk1 = tk.substr(1, 1);
  string tk1n = tk.substr(1, tk.len() - 1);
  string tk2n = tk.substr(2, tk.len() - 1);
  if(tk0.tolower() == "o")
    get_imm = tk1n.atooct();
  else if(tk0.tolower() == "h")
    get_imm = tk1n.atohex();
  else if(tk0 == "0" && tk1.tolower() == "x")
    get_imm = tk2n.atohex();
  else if(tk0.tolower() == "b")
    get_imm = tk1n.atobin();
  else if(tk0.tolower() == "d")
    get_imm = tk1n.atoi();
  else
    get_imm = tk.atoi();
endfunction

function automatic void brk_token(string s, string sp[$], ref string tokens[$]);
  int cnt = 0, found = 1;
  tokens = {};

  ///break string into tokens
  for(int i = cnt; i < s.len(); i++)
    if(s[i] inside {sp}) begin
      if(!found) begin
        ///found a token end
        found = 1;
        tokens.push_back(s.substr(cnt, i - 1));
        cnt = i + 1;
      end
      else begin
        ///found, eating chars after
        cnt = i + 1;
      end
    end
    else if(i == (s.len() - 1))
      tokens.push_back(s.substr(cnt, i));
    else begin
      if(found) begin
        ///found next token start
        cnt = i;
        found = 0;
      end
    end
endfunction

class asmig;
  bit[4:0][3:0] vecOp, immOp, zeroOp, enOp;
  uchar adr[5][4], padr[5];
  int imm[5][4];
  string op[5];
  bit[4:0] en, s, si;
  uchar chkGrp;
  uchar grpsize;
  uint pc;
  uchar allAdr[64];
  bit isVec[5]; 
  uchar adrcnt;
  i_gs0_t gs0;
  i_gs1_u gs1;
  inst_u inst[5];
  bit vrfEn[CYC_VEC][NUM_VRF_BKS],
      srfEn[CYC_VEC][NUM_SRF_BKS];
  uchar vrfAdr[CYC_VEC][NUM_VRF_BKS],
        srfAdr[CYC_VEC][NUM_VRF_BKS];
          
  function new();
    vecOp = 0;
    immOp = 0;
    zeroOp = 0;
    enOp = 0;
    en = 0;
    s = 0;
    si = 0;
    isVec = '{default : 0};
    chkGrp = 0;
    adrcnt = 0;
  endfunction

  function bit pack_grp(ovm_verbosity verb);
    ///assemble each inst
    foreach(inst[i]) begin
      uchar adru[3], bk[3];
      uchar bksel[3] = '{default : 15};
      bit dual = 0, three = 0;
      
      if(!en[i]) break;
      `asm_msg($psprintf("assemble inst %0d", i), OVM_HIGH);
      isVec[i] = vecOp[i][0];
      inst[i].i.p = padr[i];
      inst[i].i.b.ir3w1.rd = adr[i][0];
      
      ///set rs0 rs1
      foreach(bk[j]) begin
        if(!enOp[i][1 + j]) break;
        if(vecOp[i][1 + j]) begin
          if(adr[1 + j] > 31) begin
            `asm_err("vec reg out of bound!");
            return 0;
          end
          adru[j] = adr[i][1 + j] >> BITS_VRF_BKS;
          bk[j] = adr[i][1 + j] & ~{'1 << BITS_VRF_BKS};
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!vrfEn[k][bk[j]]) begin
                vrfEn[k][bk[j]] = 1;
                vrfAdr[k][bk[j]] = adru[j];
                failed = 0;
                bksel[j] = 16 + k * NUM_VRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_err("vec reg alloc failed!");
              return 0;
            end
          end
        end
        else begin
          if(adr[1 + j] > 15) begin
            `asm_err("scl reg out of bound!");
            return 0;
          end
          adru[j] = adr[i][1 + j] >> BITS_SRF_BKS;
          bk[j] = adr[i][1 + j] & ~{'1 << BITS_SRF_BKS};
          if(j < 2) begin
            bit failed = 1;
            for(int k = 0; k < CYC_VEC; k++)
              if(!srfEn[k][bk[j]]) begin
                srfEn[k][bk[j]] = 1;
                srfAdr[k][bk[j]] = adru[j];
                failed = 0;
                bksel[j] = k * NUM_SRF_BKS + bk[j];
                break;
              end
            if(failed) begin
              `asm_msg("vec reg alloc failed!");
              return 0;
            end
          end
        end
      end

      inst[i].i.b.ir3w1.rs0 = bksel[0];
      inst[i].i.b.ir3w1.rs1 = bksel[1];
      
      case(op[i])
      "li"      :
        begin
          inst[i].i.op = iop_li;
          {inst[i].i.b.i26.imm1, inst[i].i.b.i26.imm0} = imm[i][1];
        end
        "add"   :
          begin
            if(immOp[i][2]) begin
              inst[i].i.op = si[i] ? iop_addsi : iop_addi;
              {inst[i].i.b.ir1w1.imm1, inst[i].i.b.ir1w1.imm0} = imm[i][2];
            end
            else if(enOp[i][3]) begin
              inst[i].i.op = iop_r3w1;
              inst[i].i.b.ir3w1.fun = iop31_add3;
              inst[i].i.b.ir3w1.s = s[i];
              three = 1;
            end
            else begin
              inst[i].i.op = iop_r2w1;
              inst[i].i.b.ir2w1.fun = s[i] ? iop21_add : iop21_uadd;
            end
          end
      default: begin `asm_err("op not understood!"); return 0; end
      endcase
      
      ///alloc 3rd rs, should be vec
      if(three && dual) begin
        bit failed = 1;
        bk[3] = bk[3] & ('1 << 1);
        for(int k = 0; k < CYC_VEC; k ++)
          if(!vrfEn[k][bk[3]] && !vrfEn[k][bk[3] + 1]) begin
            vrfEn[k][bk[3]] = 1;
            vrfEn[k][bk[3] + 1] = 1;
            vrfAdr[k][bk[3]] = adru[3];
            vrfAdr[k][bk[3] + 1] = 255;
            failed = 0;
            bksel[3] = 16 + k * NUM_VRF_BKS + bk[3];
            break;
          end
        if(failed) begin
          `asm_err("vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[3];
      end
      else if(three) begin
        bit failed = 1;
        for(int k = 0; k < CYC_VEC; k++)
          if(!vrfEn[k][bk[3]]) begin
            vrfEn[k][bk[3]] = 1;
            vrfAdr[k][bk[3]] = adru[3];
            failed = 0;
            bksel[3] = 16 + k * NUM_VRF_BKS + bk[3];
            break;
          end
        if(failed) begin
          `asm_err("Err: vec reg alloc failed!");
          return 0;
        end
        inst[i].i.b.ir3w1.rs2 = bksel[3];
      end
    end
    
    ///collect all address
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfEn[i][j] && vrfAdr[i][j] < 32) begin
          allAdr[adrcnt] = vrfAdr[i][j];
          adrcnt++;
        end
         
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfEn[i][j]) begin
          allAdr[adrcnt] = srfAdr[i][j];
          adrcnt++;
        end
    end
    
    `asm_msg("--------------------------------", OVM_HIGH);
    return 1;
  endfunction
  
  function void wirte_out(int fo, ovm_verbosity verb);
    if(en == 'b01) begin
      gs0.t = 0;
      gs0.chkGrp = chkGrp;
      gs0.unitEn = isVec[0];
      gs0.a = allAdr[0];
      gs0.adrPkgB = (adrcnt - 1) * 3 / 8;
      $fwrite(fo, "%8b\n", gs0);
      
      if(gs0.adrPkgB > 0) begin
        i_ap0_t AdrPkg;
        foreach(AdrPkg.a[i])
          AdrPkg.a[i] = allAdr[1 + i];
        $fwrite(fo, "%8b\n", AdrPkg);
      end
      
      for(int i = 0; i < 5; i++)
        $fwrite(fo, "%8b\n", inst[0].b[i]);
    end
    else begin
    end
    $fwrite(fo, "%s", "//--------------------------------\n");
  endfunction
endclass

class ip4_assembler;
  string i, o;
  int fi, fo;
  uint pc;
  ovm_verbosity verb;
  asmig tag2ig[string];
  asmig cur;
  asmig igs[$];
    
  function bit translate();
    string s;
    uchar icnt = 0;
    fi = $fopen(i, "r");
    fo = $fopen(o, "w");
        
    if(fi == 0 || fo == 0) begin
      `asm_err("Open file failed.");
      return 0;
    end
    
    `asm_msg("IP4 assembler translating...", OVM_LOW);
    
    ///first pass, translate lines
    while($fgets(s, fi)) begin
      string tokens[$];
      int state = 0, opcnt = 0;
      bit isInst = 0, hasTag = 0;
      if(cur == null) cur  = new();
      `asm_msg("@@Asm code as follows:", OVM_HIGH);
      `asm_msg(s, OVM_HIGH, write);
      brk_token(s, '{" ", "\t", "\n"}, tokens);
      `asm_msg("@@Tokens:", OVM_MEDIUM);
      foreach(tokens[i])
        `asm_msg({tokens[i], "||"}, OVM_MEDIUM, write);
      `asm_msg("\n", OVM_MEDIUM, write);
      
      for(int tid = 0; tokens.size() != 0; tid++) begin
        string tk = tokens.pop_front();
        string tk0 = tk.substr(0, 0);
        string tk1 = tk.substr(1, 1);
        string tk1n = tk.substr(1, tk.len() - 1);
        string tk2n = tk.substr(2, tk.len() - 1);
        `asm_msg({"@@read token ", tk}, OVM_HIGH);
        if(tk0 == "/") begin
          `asm_msg("it's a comment.", OVM_HIGH);
          if(tid != 0) begin
            `asm_err("comment not at begining");
            return 0;
          end
          $fwrite(fo, "%s", s);
          break;
        end
        else if(tk0 == ";") begin
          `asm_msg("it's a group end.", OVM_HIGH);
          if(!cur.pack_grp(verb)) begin
            `asm_err("pack instruction grp failed");
            return 0;
          end
          icnt = 0;
          isInst = 0;
          cur.pc = pc;
          pc += cur.grpsize;
          igs.push_back(cur);
          cur = null;
          break;
        end
        else begin
          if(icnt >= 5) begin
            `asm_err("more than 5 inst in a group");
            return 0;
          end
          
          if(tk0 == "$") begin
            `asm_msg($psprintf("it's a tag: %s.", tk1n), OVM_HIGH);
            if(tid != 0 || icnt != 0) begin
              `asm_err("tag not at begining");
              return 0;
            end
            if(tag2ig.exists(tk1n)) begin
              `asm_err("tag exists.");
              return 0;
            end
              tag2ig[tk1n] = cur;
            hasTag = 1;
          end
          else if(tk0 == "(") begin
            if((tid - hasTag) != 0) begin
              `asm_err("predication not at begining");
              return 0;
            end
            cur.padr[icnt] = (tk1.tolower() == "p") ? tk2n.atoi() : tk1n.atoi();
            `asm_msg($psprintf("it's a predication reg :%0d", cur.padr[icnt]), OVM_HIGH);
          end
          else if(state == 0) begin
            string opts[$];
            `asm_msg($psprintf("trying to get a op for inst%0d", icnt), OVM_HIGH);
            brk_token(tk, {" ", ".", "\t", "\n"}, opts);
            cur.op[icnt] = opts.pop_front();
            cur.op[icnt] = cur.op[icnt].tolower();
            if(cur.op[icnt].tolower() != "options") begin
              cur.en[icnt] = 1;
              state ++;
              isInst = 1;
              `asm_msg($psprintf("opcode set to %s", cur.op[icnt]), OVM_HIGH);
            end
            
            while(opts.size() > 0) begin
              string opt = opts.pop_front();
              `asm_msg($psprintf("get option: %s", opt), OVM_HIGH);
              case(opt.tolower())
              "s"   : cur.s[icnt] = 1;
              "u"   : cur.s[icnt] = 0;
              "si"  : cur.si[icnt] = 1;
              "i"   : cur.si[icnt] = 0;
              "g0"  : cur.chkGrp = 0;
              "g1"  : cur.chkGrp = 1;
              default : begin `asm_err("unkonwn options."); return 0; end
              endcase
            end
          end        
          else if(state == 1) begin
            if(opcnt >= 4)
              continue;
            `asm_msg($psprintf("trying to get a reg adr or imm for op%0d", opcnt), OVM_HIGH);
            cur.enOp[icnt][opcnt] = 1;
            cur.vecOp[icnt][opcnt] = tk0.tolower() == "v";
            cur.zeroOp[icnt][opcnt] = tk.tolower() == "zero";
            cur.immOp[icnt][opcnt] = tk0.tolower() != "s" && !cur.vecOp[icnt][opcnt] && !cur.zeroOp[icnt][opcnt];
            if(cur.immOp[icnt][opcnt])
              cur.imm[icnt][opcnt] = get_imm(tk);
            else if(!cur.zeroOp[icnt][opcnt])
              cur.adr[icnt][opcnt] = tk1n.atoi();
            `asm_msg($psprintf("vecOp:%0d, zeroOp:%0d, immOp:%0d, adr:%0d, imm:%0d", cur.vecOp[icnt][opcnt],
                      cur.zeroOp[icnt][opcnt], cur.immOp[icnt][opcnt], cur.adr[icnt][opcnt],
                      cur.imm[icnt][opcnt]), OVM_HIGH);
            opcnt++;
          end
        end   
      end
      icnt += isInst;
    end
    
    ///second pass
    for(int i = 0; i < igs.size(); i++)
      igs[i].wirte_out(fo, verb);
      
    $fclose(fi);
    $fclose(fo);
    fi = 0;
    fo = 0;
    `asm_msg("Translate complete!", OVM_LOW);
    return 1;
  endfunction

  function new();
    verb = OVM_LOW;
  endfunction
endclass