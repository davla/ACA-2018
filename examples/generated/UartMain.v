module Tx(input clk, input reset,
    output io_txd,
    input [7:0] io_channel_data,
    output io_channel_ready,
    input  io_channel_valid
);

  reg [11:0] shiftReg;
  wire[11:0] T44;
  wire[11:0] T0;
  wire[11:0] T1;
  wire[11:0] T2;
  wire[11:0] T3;
  wire[11:0] T4;
  wire[11:0] T45;
  wire[10:0] T5;
  wire[9:0] T6;
  wire[10:0] T7;
  wire T8;
  wire T9;
  reg [3:0] bitsReg;
  wire[3:0] T46;
  wire[3:0] T10;
  wire[3:0] T11;
  wire[3:0] T12;
  wire T13;
  reg [19:0] cntReg;
  wire[19:0] T47;
  wire[19:0] T14;
  wire[19:0] T15;
  wire[19:0] T16;
  wire T17;
  wire[11:0] T18;
  wire[11:0] T48;
  wire[1:0] T19;
  wire[1:0] T20;
  wire[1:0] T49;
  wire T21;
  wire T22;
  wire[9:0] T50;
  wire T51;
  wire[11:0] T23;
  wire[11:0] T52;
  wire[1:0] T24;
  wire[9:0] T53;
  wire T54;
  wire[11:0] T25;
  wire[11:0] T55;
  wire[8:0] T26;
  wire[7:0] T27;
  wire[11:0] T28;
  wire[11:0] T56;
  wire[9:0] T29;
  wire[9:0] T30;
  wire[1:0] T57;
  wire T58;
  wire[11:0] T31;
  wire[11:0] T32;
  wire[11:0] T33;
  wire[11:0] T34;
  wire T35;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  wire T41;
  wire T42;
  wire T43;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    shiftReg = {1{$random}};
    bitsReg = {1{$random}};
    cntReg = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T44 = reset ? 12'h7ff : T0;
  assign T0 = T38 ? 12'h7ff : T1;
  assign T1 = T35 ? T31 : T2;
  assign T2 = T35 ? T25 : T3;
  assign T3 = T35 ? T18 : T4;
  assign T4 = T8 ? T45 : shiftReg;
  assign T45 = {1'h0, T5};
  assign T5 = {1'h1, T6};
  assign T6 = T7[9:0];
  assign T7 = shiftReg >> 1'h1;
  assign T8 = T13 & T9;
  assign T9 = bitsReg != 4'h0;
  assign T46 = reset ? 4'h0 : T10;
  assign T10 = T35 ? 4'hb : T11;
  assign T11 = T8 ? T12 : bitsReg;
  assign T12 = bitsReg - 4'h1;
  assign T13 = cntReg == 20'h0;
  assign T47 = reset ? 20'h0 : T14;
  assign T14 = T17 ? T16 : T15;
  assign T15 = T13 ? 20'h1b1 : cntReg;
  assign T16 = cntReg - 20'h1;
  assign T17 = T13 ^ 1'h1;
  assign T18 = T23 | T48;
  assign T48 = {T50, T19};
  assign T19 = T49 & T20;
  assign T20 = 2'h1;
  assign T49 = T21 ? 2'h3 : 2'h0;
  assign T21 = T22;
  assign T22 = 1'h0;
  assign T50 = T51 ? 10'h3ff : 10'h0;
  assign T51 = T19[1];
  assign T23 = T4 & T52;
  assign T52 = {T53, T24};
  assign T24 = ~ T20;
  assign T53 = T54 ? 10'h3ff : 10'h0;
  assign T54 = T24[1];
  assign T25 = T28 | T55;
  assign T55 = {3'h0, T26};
  assign T26 = T27 << 1'h1;
  assign T27 = io_channel_data & 8'hff;
  assign T28 = T3 & T56;
  assign T56 = {T57, T29};
  assign T29 = ~ T30;
  assign T30 = 10'h1fe;
  assign T57 = T58 ? 2'h3 : 2'h0;
  assign T58 = T29[9];
  assign T31 = T32 | 12'h600;
  assign T32 = T2 & T33;
  assign T33 = ~ T34;
  assign T34 = 12'h600;
  assign T35 = T36 & io_channel_valid;
  assign T36 = T13 & T37;
  assign T37 = T9 ^ 1'h1;
  assign T38 = T36 & T39;
  assign T39 = io_channel_valid ^ 1'h1;
  assign io_channel_ready = T40;
  assign T40 = T42 & T41;
  assign T41 = bitsReg == 4'h0;
  assign T42 = cntReg == 20'h0;
  assign io_txd = T43;
  assign T43 = shiftReg[0];

  always @(posedge clk) begin
    if(reset) begin
      shiftReg <= 12'h7ff;
    end else if(T38) begin
      shiftReg <= 12'h7ff;
    end else if(T35) begin
      shiftReg <= T31;
    end else if(T35) begin
      shiftReg <= T25;
    end else if(T35) begin
      shiftReg <= T18;
    end else if(T8) begin
      shiftReg <= T45;
    end
    if(reset) begin
      bitsReg <= 4'h0;
    end else if(T35) begin
      bitsReg <= 4'hb;
    end else if(T8) begin
      bitsReg <= T12;
    end
    if(reset) begin
      cntReg <= 20'h0;
    end else if(T17) begin
      cntReg <= T16;
    end else if(T13) begin
      cntReg <= 20'h1b1;
    end
  end
endmodule

module Buffer(input clk, input reset,
    input [7:0] io_in_data,
    output io_in_ready,
    input  io_in_valid,
    output[7:0] io_out_data,
    input  io_out_ready,
    output io_out_valid
);

  wire T0;
  reg  stateReg;
  wire T9;
  wire T1;
  wire T2;
  wire T3;
  wire T4;
  wire T5;
  wire T6;
  reg [7:0] dataReg;
  wire[7:0] T10;
  wire[7:0] T7;
  wire T8;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    stateReg = {1{$random}};
    dataReg = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_out_valid = T0;
  assign T0 = stateReg == 1'h1;
  assign T9 = reset ? 1'h0 : T1;
  assign T1 = T5 ? 1'h0 : T2;
  assign T2 = T3 ? 1'h1 : stateReg;
  assign T3 = T4 & io_in_valid;
  assign T4 = stateReg == 1'h0;
  assign T5 = T6 & io_out_ready;
  assign T6 = T4 ^ 1'h1;
  assign io_out_data = dataReg;
  assign T10 = reset ? 8'h0 : T7;
  assign T7 = T3 ? io_in_data : dataReg;
  assign io_in_ready = T8;
  assign T8 = stateReg == 1'h0;

  always @(posedge clk) begin
    if(reset) begin
      stateReg <= 1'h0;
    end else if(T5) begin
      stateReg <= 1'h0;
    end else if(T3) begin
      stateReg <= 1'h1;
    end
    if(reset) begin
      dataReg <= 8'h0;
    end else if(T3) begin
      dataReg <= io_in_data;
    end
  end
endmodule

module BufferedTx(input clk, input reset,
    output io_txd,
    input [7:0] io_channel_data,
    output io_channel_ready,
    input  io_channel_valid
);

  wire tx_io_txd;
  wire tx_io_channel_ready;
  wire buf__io_in_ready;
  wire[7:0] buf__io_out_data;
  wire buf__io_out_valid;


  assign io_channel_ready = buf__io_in_ready;
  assign io_txd = tx_io_txd;
  Tx tx(.clk(clk), .reset(reset),
       .io_txd( tx_io_txd ),
       .io_channel_data( buf__io_out_data ),
       .io_channel_ready( tx_io_channel_ready ),
       .io_channel_valid( buf__io_out_valid )
  );
  Buffer buf_(.clk(clk), .reset(reset),
       .io_in_data( io_channel_data ),
       .io_in_ready( buf__io_in_ready ),
       .io_in_valid( io_channel_valid ),
       .io_out_data( buf__io_out_data ),
       .io_out_ready( tx_io_channel_ready ),
       .io_out_valid( buf__io_out_valid )
  );
endmodule

module Sender2(input clk, input reset,
    output io_txd,
    output io_led
);

  wire T0;
  reg  validReg;
  wire T14;
  wire T1;
  wire T2;
  wire T3;
  wire T4;
  reg [3:0] cntReg;
  wire[3:0] T15;
  wire[3:0] T5;
  wire[3:0] T6;
  wire[3:0] T7;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire[7:0] T16;
  reg [5:0] T12;
  reg  blkReg;
  wire T17;
  wire tx_io_txd;
  wire tx_io_channel_ready;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    validReg = {1{$random}};
    cntReg = {1{$random}};
    blkReg = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T0 = validReg != 1'h1;
  assign T14 = reset ? 1'h0 : T1;
  assign T1 = T8 ? 1'h1 : T2;
  assign T2 = T3 ? 1'h0 : validReg;
  assign T3 = tx_io_channel_ready & T4;
  assign T4 = cntReg != 4'h0;
  assign T15 = reset ? 4'ha : T5;
  assign T5 = T8 ? 4'ha : T6;
  assign T6 = T3 ? T7 : cntReg;
  assign T7 = cntReg - 4'h1;
  assign T8 = T11 & T9;
  assign T9 = tx_io_channel_ready & T10;
  assign T10 = cntReg == 4'h0;
  assign T11 = T3 ^ 1'h1;
  assign T16 = {2'h0, T12};
  always @(*) case (cntReg)
    0: T12 = 6'h39;
    1: T12 = 6'h38;
    2: T12 = 6'h37;
    3: T12 = 6'h36;
    4: T12 = 6'h35;
    5: T12 = 6'h34;
    6: T12 = 6'h33;
    7: T12 = 6'h32;
    8: T12 = 6'h31;
    9: T12 = 6'h30;
    default: begin
      T12 = 6'bx;
`ifndef SYNTHESIS
// synthesis translate_off
      T12 = {1{$random}};
// synthesis translate_on
`endif
    end
  endcase
  assign io_led = blkReg;
  assign T17 = reset ? 1'h0 : blkReg;
  assign io_txd = tx_io_txd;
  BufferedTx tx(.clk(clk), .reset(reset),
       .io_txd( tx_io_txd ),
       .io_channel_data( T16 ),
       .io_channel_ready( tx_io_channel_ready ),
       .io_channel_valid( T0 )
  );

  always @(posedge clk) begin
    if(reset) begin
      validReg <= 1'h0;
    end else if(T8) begin
      validReg <= 1'h1;
    end else if(T3) begin
      validReg <= 1'h0;
    end
    if(reset) begin
      cntReg <= 4'ha;
    end else if(T8) begin
      cntReg <= 4'ha;
    end else if(T3) begin
      cntReg <= T7;
    end
    if(reset) begin
      blkReg <= 1'h0;
    end
  end
endmodule

module Led(input clk, input reset,
    output io_led
);

  reg  blkReg;
  wire T5;
  wire T0;
  wire T1;
  wire T2;
  reg [31:0] cntReg;
  wire[31:0] T6;
  wire[31:0] T3;
  wire[31:0] T4;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    blkReg = {1{$random}};
    cntReg = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_led = blkReg;
  assign T5 = reset ? 1'h0 : T0;
  assign T0 = T2 ? T1 : blkReg;
  assign T1 = ~ blkReg;
  assign T2 = cntReg == 32'h17d783f;
  assign T6 = reset ? 32'h0 : T3;
  assign T3 = T2 ? 32'h0 : T4;
  assign T4 = cntReg + 32'h1;

  always @(posedge clk) begin
    if(reset) begin
      blkReg <= 1'h0;
    end else if(T2) begin
      blkReg <= T1;
    end
    if(reset) begin
      cntReg <= 32'h0;
    end else if(T2) begin
      cntReg <= 32'h0;
    end else begin
      cntReg <= T4;
    end
  end
endmodule

module UartMain(input clk, input reset,
    input  io_rxd,
    output io_txd,
    output io_ledG,
    output io_ledR
);

  wire l_io_led;
  wire u_io_txd;
  wire u_io_led;


  assign io_ledR = u_io_led;
  assign io_ledG = l_io_led;
  assign io_txd = u_io_txd;
  Sender2 u(.clk(clk), .reset(reset),
       .io_txd( u_io_txd ),
       .io_led( u_io_led )
  );
  Led l(.clk(clk), .reset(reset),
       .io_led( l_io_led )
  );
endmodule

