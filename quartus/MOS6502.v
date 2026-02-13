
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
	wire enable = 1;

//=======================================================
//  Structural coding
//=======================================================



	topEntity cpu0(
		.CLK(clk),
		.RESET(rst),
		.ENABLE(enable),
		.SWITCHES(SW[7:0]),

		.VGA_R(VGA_R),
		.VGA_G(VGA_G),
		.VGA_B(VGA_B),
		.VGA_HSYNC(VGA_HS),
		.VGA_VSYNC(VGA_VS),
		.VGA_BLANK_N(VGA_BLANK_N),
		.LEDS(LEDR[7:0])
	);

/*
	topEntity cpu0(
		.CLK(clk),
		.RESET(rst),
		.ENABLE(enable),
		.SWITCHES(SW[7:0]),
		
	);
*/

	assign VGA_SYNC_N = 1;
	assign VGA_CLK = clk;
	
	assign LEDR[8] = rst;
	assign LEDR[9] = enable;
endmodule
