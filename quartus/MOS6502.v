// Quartus Prime Verilog Template
// Single port RAM with single read/write address 
module single_port_ram 
#(parameter DATA_WIDTH=8, parameter ADDR_WIDTH=16)
(
	input [(DATA_WIDTH-1):0] data,
	input [(ADDR_WIDTH-1):0] addr,
	input we, clk,
	output [(DATA_WIDTH-1):0] q
);

	// Declare the RAM variable
	reg [DATA_WIDTH-1:0] ram[2**ADDR_WIDTH-1:0];

	// Variable to hold the registered read address
	reg [ADDR_WIDTH-1:0] addr_reg;

	always @ (posedge clk)
	begin
		// Write
		if (we)
			ram[addr] <= data;

		addr_reg <= addr;
	end

	// Continuous assignment implies read returns NEW data.
	// This is the natural behavior of the TriMatrix memory
	// blocks in Single Port mode.  
	assign q = ram[addr_reg];

endmodule

module SevenSegmentDriver(input [3:0] number, output reg [6:0] data);
    always @* begin
        case(number)
            4'h0: data = ~7'b0111111;
            4'h1: data = ~7'b0000110;
            4'h2: data = ~7'b1011011;
            4'h3: data = ~7'b1001111;
            4'h4: data = ~7'b1100110;
            4'h5: data = ~7'b1101101;
            4'h6: data = ~7'b1111101;
            4'h7: data = ~7'b0000111;
            4'h8: data = ~7'b1111111;
            4'h9: data = ~7'b1101111;
            4'ha: data = ~7'b1110111;
            4'hb: data = ~7'b1111100;
            4'hc: data = ~7'b0111001;
            4'hd: data = ~7'b1011110;
            4'he: data = ~7'b1111001;
            4'hf: data = ~7'b1110001;
        endcase
    end
endmodule

module MOS6502(

	//////////// CLOCK //////////
	input 		          		CLOCK2_50,
	input 		          		CLOCK3_50,
	input 		          		CLOCK4_50,
	input 		          		CLOCK_50,

	//////////// SEG7 //////////
	output		     [6:0]		HEX0,
	output		     [6:0]		HEX1,
	output		     [6:0]		HEX2,
	output		     [6:0]		HEX3,
	output		     [6:0]		HEX4,
	output		     [6:0]		HEX5,

	//////////// KEY //////////
	input 		     [3:0]		KEY,

	//////////// LED //////////
	output		     [9:0]		LEDR,

	//////////// SW //////////
	input 		     [9:0]		SW,

	//////////// VGA //////////
	output		          		VGA_BLANK_N,
	output		     [7:0]		VGA_B,
	output		          		VGA_CLK,
	output		     [7:0]		VGA_G,
	output		          		VGA_HS,
	output		     [7:0]		VGA_R,
	output		          		VGA_SYNC_N,
	output		          		VGA_VS
);



//=======================================================
//  REG/WIRE declarations
//=======================================================

	wire clk = CLOCK_50;
	wire rst = KEY[0];
	wire enable = KEY[1];

	wire [15:0] addr_out;
	wire write;
	wire [7:0] data_out;

	wire [7:0] data_in;

//=======================================================
//  Structural coding
//=======================================================

	single_port_ram ram(
		.data(data_out),
		.addr(addr_out),
		.we(write),
		.clk(clk),
		.q(data_in)
	);

	topEntity cpu0(
		.CLK(clk),
		.RESET(rst),
		.ENABLE(enable),
		.MEM_DATA_IN(data_in | SW[7:0]),

		.MEM_ADDR(addr_out),
		.MEM_W(write),
		.MEM_W_DATA(data_out)
	);

	SevenSegmentDriver bus_low0(.number(addr_out[3:0]), .data(HEX0));
	SevenSegmentDriver bus_low1(.number(addr_out[7:4]), .data(HEX1));
	SevenSegmentDriver bus_low2(.number(addr_out[11:8]), .data(HEX2));
	SevenSegmentDriver bus_low3(.number(addr_out[15:12]), .data(HEX3));
	
	assign LEDR[7:0] = data_in;
	
	assign VGA_BLANK_N = 0;
	assign VGA_B = 0;
	assign VGA_CLK = 0;
	assign VGA_G = 0;
	assign VGA_HS = 0;
	assign VGA_R = 0;
	assign VGA_SYNC_N = 0;
	assign VGA_VS = 0;
endmodule
