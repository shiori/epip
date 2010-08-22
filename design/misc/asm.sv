///assembler for ip4

class ip4_assembler;
  string i, o;
  
  function void translate();
    int fi = $fopen(i, "r");
    int fo = $fopen(o, "w");
    string s;
    
    if(fi == 0 || fo == 0) begin
      $display("Open file failed.");
      return;
    end
    
    $display("IP4 assembler translating...\nAsm code as follows:");
    while($fgets(s, fi)) begin
      $write(s);
      $fwrite(fo, s);
    end
    $fclose(fi);
    $fclose(fo);
    $display("Translate complete!");
  endfunction

  function new();
  endfunction
endclass