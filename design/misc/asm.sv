///assembler for ip4

class ip4_assembler;
  string i, o, tokens[$], opts[$], tag;
  bit[4:0][3:0] vecOp, immOp, zeroOp, enOp;
  uchar adr[5][4], padr[5];
  int imm[5][4];
  string op[5];
  bit[4:0] en, s, si;
  uchar chkGrp;
  int fi, fo;
  
  function void get_token(string s);
    int cnt = 0, found = 1;
    tokens = {};

    ///break string into tokens
    for(int i = cnt; i < s.len(); i++)
      if(s[i] == " " || s[i] == "\t" || s[i] == "\n") begin
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

  function void get_opts(string s);
    int cnt = 0, found = 1;
    opts = {};

    ///break string into opts
    for(int i = cnt; i < s.len(); i++)
      if(s[i] == "." || s[i] == " " || s[i] == "\n") begin
        if(!found) begin
          ///found a token end
          found = 1;
          opts.push_back(s.substr(cnt, i - 1));
          cnt = i + 1;
        end
        else begin
          ///found, eating chars after
          cnt = i + 1;
        end
      end
      else if(i == (s.len() - 1))
        opts.push_back(s.substr(cnt, i));
      else begin
        if(found) begin
          ///found next token start
          cnt = i;
          found = 0;
        end
      end
  endfunction
  
  function void finish_grp();
    inst_u inst[5];
    i_gs0_t gs0;
    i_gs1_u gs1;
    bit isVec[5];
    bit vrfEn[CYC_VEC][NUM_VRF_BKS],
        srfEn[CYC_VEC][NUM_SRF_BKS];
    uchar vrfAdr[CYC_VEC][NUM_VRF_BKS],
          srfAdr[CYC_VEC][NUM_VRF_BKS];
    uchar allAdr[64], tmp = 0;
    
    ///assemble each inst
    foreach(inst[i]) begin
      uchar adru[3], bk[3];
      uchar bksel[3] = '{default : 15};
      bit dual = 0, three = 0;
      
      if(!en[i]) break;
      $display($psprintf("assemble inst %0d", i));
      isVec[i] = vecOp[i][0];
      inst[i].i.p = padr[i];
      inst[i].i.b.ir3w1.rd = adr[i][0];
      
      ///set rs0 rs1
      foreach(bk[j]) begin
        if(!enOp[i][1 + j]) continue;
        if(vecOp[i][1 + j]) begin
          if(adr[1 + j] > 31) begin
            $display("Err: vec reg out of bound!");
            continue;
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
            if(failed)
              $display("Err: vec reg alloc failed!");
          end
        end
        else begin
          if(adr[1 + j] > 15) begin
            $display("Err: scl reg out of bound!");
            continue;
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
            if(failed)
              $display("Err: vec reg alloc failed!");
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
      default: $display("Err: op not understood!");
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
        if(failed)
          $display("Err: vec reg alloc failed!");
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
        if(failed)
          $display("Err: vec reg alloc failed!");
        inst[i].i.b.ir3w1.rs2 = bksel[3];
      end
    end
    
    ///collect all address
    for(int i = 0; i < CYC_VEC; i++) begin
      for(int j = 0; j < NUM_VRF_BKS; j++)
        if(vrfEn[i][j] && vrfAdr[i][j] < 32) begin
          allAdr[tmp] = vrfAdr[i][j];
          tmp++;
        end
         
      for(int j = 0; j < NUM_SRF_BKS; j++)
        if(srfEn[i][j]) begin
          allAdr[tmp] = srfAdr[i][j];
          tmp++;
        end
    end
    
    if(en == 'b01) begin
      gs0.t = 0;
      gs0.chkGrp = chkGrp;
      gs0.unitEn = isVec[0];
      gs0.a = allAdr[0];
      gs0.adrPkgB = (tmp - 1) * 3 / 8;
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
    
    ///clean up
    $display("--------------------------------");
    $fwrite(fo, "%s", "//--------------------------------\n");
    vecOp = 0;
    immOp = 0;
    zeroOp = 0;
    enOp = 0;
    en = 0;
    s = 0;
    si = 0;
    chkGrp = 0;
  endfunction
    
  function bit translate();
    string s;
    uchar icnt = 0;
    fi = $fopen(i, "r");
    fo = $fopen(o, "w");
        
    if(fi == 0 || fo == 0) begin
      $display("Open file failed.");
      return 0;
    end
    
    $display("IP4 assembler translating...\nAsm code as follows:");
    
    ///first pass, translate lines
    while($fgets(s, fi)) begin
      int state = 0, opcnt = 0;
      bit isInst = 0;
      $write(s);
      get_token(s);
      $display("Tokens:");
      foreach(tokens[i])
        $write({tokens[i], "||"});
      $write("\n");
      
      for(int tid = 0; tokens.size() != 0; tid++) begin
        string tk = tokens.pop_front();
        string tk0 = tk.substr(0, 0);
        string tk1 = tk.substr(1, 1);
        string tk1n = tk.substr(1, tk.len() - 1);
        string tk2n = tk.substr(2, tk.len() - 1);
        $display({"read token ", tk});
        if(tk0 == "/") begin
          $display("it's a comment.");
          if(tid != 0) begin
            $display("ERR: comment not at begining");
            return 0;
          end
          $fwrite(fo, "%s", s);
          break;
        end
        else if(tk0 == ";") begin
          $display("it's a group end.");
          finish_grp();
          icnt = 0;
          isInst = 0;
          break;
        end
        else begin
          if(icnt >= 5) begin
            $display("ERR: more than 5 inst in a group");
            return 0;
          end
          
          if(tk0 == "$") begin
            $display($psprintf("it's a tag: %s.", tk1n));
            if(tid != 0 || icnt != 0) begin
              $display("ERR: tag not at begining");
              return 0;
            end
            tag = tk1n;
          end
          else if(tk0 == "(") begin
            string t;
            if(tid != 0) begin
              $display("ERR: predication not at begining");
              return 0;
            end
            padr[icnt] = (tk1.tolower() == "p") ? tk2n.atoi() : tk1n.atoi();
            $display($psprintf("it's a predication reg :%0d", padr[icnt]));
          end
          else if(state == 0) begin
            $display($psprintf("trying to get a op for inst%0d", icnt));
            get_opts(tk);
            op[icnt] = opts.pop_front();
            if(op[icnt] != "options") begin
              en[icnt] = 1;
              state ++;
              isInst = 1;
              $display($psprintf("opcode set to %s", op[icnt]));
            end
            
            while(opts.size() > 0) begin
              string opt = opts.pop_front();
              $display($psprintf("get option: %s", opt));
              case(opt)
              "s"   : s[icnt] = 1;
              "u"   : s[icnt] = 0;
              "si"  : si[icnt] = 1;
              "i"   : si[icnt] = 0;
              "g0"  : chkGrp = 0;
              "g1"  : chkGrp = 1;
              default : $display("Err, unkonwn options.");
              endcase
            end
          end        
          else if(state == 1) begin
            if(opcnt >= 4)
              continue;
            $display($psprintf("trying to get a reg adr or imm for op%0d", opcnt));
            enOp[icnt][opcnt] = 1;
            vecOp[icnt][opcnt] = tk0.tolower() == "v";
            zeroOp[icnt][opcnt] = tk.tolower() == "zero";
            immOp[icnt][opcnt] = tk0.tolower() != "s" && !vecOp[icnt][opcnt] && !zeroOp[icnt][opcnt];
            if(immOp[icnt][opcnt]) begin
              if(tk0.tolower() == "o")
                imm[icnt][opcnt] = tk1n.atooct();
              else if(tk0.tolower() == "h")
                imm[icnt][opcnt] = tk1n.atohex();
              else if(tk0 == "0" && tk1.tolower() == "x")
                imm[icnt][opcnt] = tk2n.atohex();
              else if(tk0.tolower() == "b")
                imm[icnt][opcnt] = tk1n.atobin();
              else if(tk0.tolower() == "d")
                imm[icnt][opcnt] = tk1n.atoi();
              else
                imm[icnt][opcnt] = tk.atoi();
            end
            else if(!zeroOp[icnt][opcnt])
              adr[icnt][opcnt] = tk1n.atoi();
            $display($psprintf("vecOp:%0d, zeroOp:%0d, immOp:%0d, adr:%0d, imm:%0d", vecOp[icnt][opcnt],
                      zeroOp[icnt][opcnt], immOp[icnt][opcnt], adr[icnt][opcnt], imm[icnt][opcnt]));
            opcnt++;
          end
        end   
      end
      icnt += isInst;
    end
    
    $fclose(fi);
    $fclose(fo);
    fi = 0;
    fo = 0;
    $display("Translate complete!");
    return 1;
  endfunction

  function new();
  endfunction
endclass