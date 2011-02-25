// =============================================================================
//                         FILE DETAILS
// Project          : IM2
// Author           : Andy Chen
// File             : ram.sv
// Title            : Single port sram wrapper
// Version          : 0.1
// Last modified		: Jun 16 2008
// =============================================================================
//Log:
//Created by Andy Chen on Jun 16 2008

//synopsys translate_off
/////////////////////////////////////////////////////
// Module interface
/////////////////////////////////////////////////////

module ip4_ram(clk, radr, wadr, wr, be, datai,
               datao, datao_d);
                 
/////////////////////////////////////////////////////
// Parameters
/////////////////////////////////////////////////////
  parameter addr_width = 10,
            num_words  = 'b01 << addr_width,
            word_width = 32,
            be_width   = (word_width - 1) / 8 + 1,
            two_port   = 0;
  
/////////////////////////////////////////////////////
// Internal nets and registers 
/////////////////////////////////////////////////////
  input logic clk;
  input logic [addr_width-1:0] radr, wadr;
  input logic wr;
  input logic [be_width-1:0] be;
  input logic [word_width-1:0] datai;
  output logic [word_width-1:0] datao, datao_d;	
  
  logic [word_width-1:0] ram[num_words-1:0];
  logic [addr_width-1:0] addr_d;
  logic wr_d;
  
  genvar i;
  
  assign datao = (wr_d && !two_port) ? 'x : ram[addr_d];
  
/////////////////////////////////////////////////////
// Functions and tasks
/////////////////////////////////////////////////////
//optional
	
/////////////////////////////////////////////////////
// Instances
/////////////////////////////////////////////////////
//optional

/////////////////////////////////////////////////////
// Sequential logic
/////////////////////////////////////////////////////
  always_ff@(posedge clk)
  begin
  	addr_d <= radr;
  	wr_d <= wr;
  	datao_d <= datao;
  end

  for(i = 0; i < be_width; i++)
  begin
   	always_ff@(posedge clk)
   	  if(be[i] && wr) begin
   	    if(i == (be_width-1))
   	      ram[wadr][word_width-1:i*8] <= datai[word_width-1:i*8];
   	    else
   	      ram[wadr][i*8+7:i*8] <= datai[i*8+7:i*8];
   	  end
  end

/////////////////////////////////////////////////////
// Combinational logic
/////////////////////////////////////////////////////
//required

/////////////////////////////////////////////////////
// Initials
/////////////////////////////////////////////////////

endmodule
//synopsys translate_on