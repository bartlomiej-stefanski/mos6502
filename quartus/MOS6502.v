
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
	wire rst = !KEY[0];
	wire enable = 1;

//=================================z======================
//  Structural coding
//=======================================================



	topEntity cpu0(
		.CLK(clk),
		.RESET(rst),
		.ENABLE(enable),
		.SWITCHES(SW[7:0]),
		.BUTTON(!KEY[3]),

		.VGA_R(VGA_R),
		.VGA_G(VGA_G),
		.VGA_B(VGA_B),
		.VGA_HSYNC(VGA_HS),
		.VGA_VSYNC(VGA_VS),
		.VGA_BLANK_N(VGA_BLANK_N),
		.LEDS(LEDR[7:0]),
		.SEG0(HEX0),
		.SEG1(HEX1)
	);

	assign VGA_SYNC_N = 1;
	assign VGA_CLK = clk;
	
	assign LEDR[8] = rst;
	assign LEDR[9] = enable;
endmodule
