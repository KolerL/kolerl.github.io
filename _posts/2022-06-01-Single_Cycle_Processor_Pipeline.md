---
title: 'Single Cycle Processor & Pipeline'
date: 2022-06-01
permalink: /posts/2022/06/Single_Cycle_Processor_Pipeline/
tags:
  - Pipeline
  - Single Cycle
  - Verilog
  - RISC-V
---

这篇博客以中国科学技术大学《计算机组成原理（COD2022）》课程为依托，针对 RISC-V CPU 进行设计，仅做 Verilog 代码展示。
该课程是面向计算机科学与技术学院本科生的一门专业核心基础课。 本课程介绍了运算器、控制器、存储器的结构、工作原理、设计方法及互连构成整机的有关技术， 有助于学生深刻理解现代数字计算机系统的工作原理及软硬件设计中的折中思想， 引导学生体验完整的设计过程，解决工程设计面临的实际问题。
<br/>
<br/>
<br/>
<br/>
<br/>
# 关于单周期CPU

### 数据通路：
![cpu1.png](/images/cpu1.png)

要求实现下列十条指令：
```
    add rd, rs1, rs2        # x[rd] = x[rs1] + x[rs2]
    addi rd, rs1, imm 	    # x[rd] = x[rs1] + sext(imm)
    sub rd, rs1, rs2        # x[rd] = x[rs1] - x[rs2]
    auipc rd, imm           # x[rd] = pc + sext(imm[31:12] << 12)
    lw rd, offset(rs1)      # x[rd] = M[x[rs1] + sext(offset)]
    sw rs2, offset(rs1)     # M[x[rs1]+sext(offset)=x[rs2]
    beq rs1, rs2, offset    # if (rs1 == rs2) pc += sext(offset)
    blt rs1, rs2, offset    # if (rs1 < s rs2) pc += sext(offset)
    jal rd, offset 	    # x[rd] = pc+4; pc += sext(offset) 
    jalr rd, offset(rs1)    # t =pc+4; pc=(x[rs1]+sext(offset))&~1; x[rd]=t 
```
&nbsp;
&nbsp;
### 代码展示
```
module reg_file(
input clk,
input [4:0]a1,a2,a3,
input [31:0]wd3,
input we3,
output [31:0]rd1,rd2,x18,x19,x20,x21,x22,x23
);
reg [31:0] reg_file[0:31];
integer i;
initial
begin
    for(i=0;i<32;i=i+1) reg_file[i]=0;
end
always@(posedge clk)
begin
    if(we3&&(a3))
        reg_file[a3]<=wd3;
end
assign rd1 = a1 ? reg_file[a1] : 32'h0;
assign rd2 = a2 ? reg_file[a2] : 32'h0;
assign x18 = reg_file[18];
assign x19 = reg_file[19];
assign x20 = reg_file[20];
assign x21 = reg_file[21];
assign x22 = reg_file[22];
assign x23 = reg_file[23];
endmodule
//
module alu(
input [31:0]a,b,
input [1:0] alu_ctrl,         //0是清零，1是加，2是减
output reg [31:0] alu_out
);
/*wire signed [31:0] signed_a ;
wire signed [31:0] signed_b ;*/ //用不到，这里计算不用补码

always@(*)
begin
    case(alu_ctrl)
    2'h0: alu_out=32'h0;
    2'h1: alu_out=a+b;
    2'h2: alu_out=a-b;
    default: alu_out =32'h0;
    endcase
end
endmodule
//
module pc_calc(
input clk,
input rst,
input branch,
input [31:0]alu_out,
output reg [31:0]pc,
output [31:0] pc_plus4
    );
always@(posedge clk or posedge rst)
begin
    if(rst)
        pc<=32'h00003000;
    else if(branch)
        pc<=alu_out;
    else
        pc<=pc_plus4;
end
assign pc_plus4=pc+32'h4;   
endmodule
//
module bran(
input [31:0] a,
input [31:0] b,
input [2:0]comp_ctrl,        //0是beq，4是blt
input do_branch,
input do_jump,
output branch
);
wire signed [31:0]signed_a;
wire signed [31:0]signed_b;
reg taken;                 //是1且do_branch则跳转

assign signed_a=a;
assign signed_b=b;

always@(*)
begin
    case(comp_ctrl)
    3'h0: taken = (signed_a==signed_b);
    3'h4: taken = (signed_a<signed_b);
    default: taken = 0;
    endcase
end

assign branch = (taken&&do_branch) || do_jump;//b型或jalr，jal都需跳转
endmodule
//
module controller(
input [31:0]inst,
output rf_wr_en,
output alu_a_sel,
output alu_b_sel,
output reg [1:0]alu_ctrl,
output reg [2:0]dm_rd_ctrl,
output reg [1:0]dm_wr_ctrl,
output reg [1:0]rf_wr_sel,
output [2:0] comp_ctrl,
output do_branch,
output do_jump
);
wire [6:0] opcode;
wire [2:0] funct3;
wire [6:0] funct7;
assign opcode = inst[6:0];
assign funct3 = inst[14:12];
assign funct7 = inst[31:25];

wire is_add;
wire is_addi;
wire is_sub;
wire is_auipc;
wire is_lw;
wire is_sw;
wire is_beq;
wire is_blt;
wire is_jal;
wire is_jalr;

assign is_add = (opcode==7'h33)&&(funct3==3'h0)&&(funct7==7'h00);
assign is_addi = (opcode==7'h13)&&(funct3==3'h0);
assign is_sub = (opcode==7'h33)&&(funct3==3'h0)&&(funct7==7'h20);
assign is_auipc = (opcode==7'h17);
assign is_lw = (opcode==7'h03)&&(funct3==3'h2);
assign is_sw = (opcode==7'h23)&&(funct3==3'h2);
assign is_beq = (opcode==7'h63)&&(funct3==3'h0);
assign is_blt = (opcode==7'h63)&&(funct3==3'h4);
assign is_jal = (opcode==7'h6f);
assign is_jalr = (opcode==7'h67)&&(funct3==3'h0);


wire is_add_type;
wire is_u_type;
wire is_jump_type;
wire is_b_type;
wire is_r_type;
wire is_i_type;
wire is_s_type;

assign is_b_type = is_beq | is_blt;
assign is_r_type = is_add | is_sub;
assign is_i_type = is_jalr | is_lw | is_addi;
assign is_s_type = is_sw;
assign is_u_type = is_auipc;
assign is_jump_type = is_jal;
assign is_add_type = is_auipc | is_add | is_addi | is_jal | is_jalr | is_b_type | is_s_type | is_lw;    //除了sub指令alu全用加

always@(*)
begin
    if(is_add_type) alu_ctrl=2'h1;
    else if(is_sub) alu_ctrl=2'h2;
    else alu_ctrl=2'h0;
end

assign rf_wr_en = is_u_type | is_jump_type | is_r_type | is_i_type;
assign alu_a_sel = is_r_type | is_i_type | is_s_type;
assign alu_b_sel = ~ is_r_type;

always@(*)
begin
    if(is_lw) dm_rd_ctrl=3'h5;
    else dm_rd_ctrl=3'h0;
end

always@(*)
begin
    if(is_sw) dm_wr_ctrl=2'h3;
    else dm_wr_ctrl=2'h0;
end

always@(*)
begin
    if(is_lw) rf_wr_sel=2'h3;
    else if(((~is_jalr)&is_i_type) | is_u_type | is_r_type) rf_wr_sel=2'h2;
    else if(is_jalr | is_jal) rf_wr_sel=2'h1;
    else  rf_wr_sel=2'h0;
end
assign comp_ctrl = funct3;
assign do_branch = is_b_type;
assign do_jump = is_jal | is_jalr;
endmodule
//
module imm(
input [31:0] inst,
output reg [31:0] imm_out
);
wire [6:0] inst_type;
assign inst_type = inst[6:0];

always@(*)
begin
    case(inst_type)
    7'h13 : imm_out={{21{inst[31]}},inst[30:20]};//addi
    7'h17 : imm_out={inst[31:12],12'h0};        //auipc
    7'h03 : imm_out={{21{inst[31]}},inst[30:20]};//lw
    7'h23 : imm_out={{21{inst[31]}},inst[30:25],inst[11:7]};//sw
    7'h63 : imm_out={{20{inst[31]}},inst[7],inst[30:25],inst[11:8],1'b0};//beq,blt
    7'h6f : imm_out={{12{inst[31]}},inst[19:12],inst[20],inst[30:21],1'b0};//jal
    7'h67 : imm_out={{21{inst[31]}},inst[30:20]};//jalr
    default : imm_out = 32'h0;
    endcase
end

endmodule
//
module mem(
input clk,
input [31:0] im_addr,//就是pc
output [31:0] im_dout, //就是inst
input [2:0] dm_rd_ctrl,
input [1:0] dm_wr_ctrl,
input [31:0] dm_addr,//就是alu_out,比如sw计算要修改的数据位置
input [31:0] dm_din,//就是rf_rd2
input [7:0] m_rf_addr,
output reg [31:0] dm_dout,//eg，读取数据段给寄存器 
output [31:0] m_data
);
wire [31:0]im_addr_use;
assign im_addr_use=(im_addr-32'h00003000)/4;//就比如把3000的指令位置变成0，方便下面调用ram
dist_mem_inst  mem_inst(im_addr_use[7:0],32'h0,clk,1'b0,im_dout);//得到im_dout，且im_mem不会改，所以we=0;

wire [31:0]dm_addr_use;
wire [31:0]dm_out;

assign dm_addr_use=dm_addr/4;
dist_mem_data  mem_data(dm_addr_use[7:0],dm_din,m_rf_addr,clk,dm_wr_ctrl,dm_out,m_data);

always@(*)
begin
    case(dm_rd_ctrl)//对于sw
    3'h5: dm_dout<=dm_out;  
    default: dm_dout<=32'h0;
    endcase
end
endmodule
//
module  cpu (
  input clk, 
  input rst,

  //IO_BUS
/*  output [7:0] io_addr,      //led和seg的地址
  output [31:0] io_dout,     //输出led和seg的数据
  output io_we,                 //输出led和seg数据时的使能信号
  input [31:0] io_din,          //来自sw的输入数据*/

  //Debug_BUS
  input [7:0] m_rf_addr,   //存储器(MEM)或寄存器堆(RF)的调试读口地址
  output [31:0] m_data,    //从MEM读取的数据
  output [31:0] x18,x19,x20,x21,x22,x23,
  output [31:0] pc             //PC的内容
  
);
wire branch;
wire [31:0] pc_plus4;
wire [31:0] inst;
wire [31:0] imm_out;
wire rf_wr_en;
wire alu_a_sel;
wire alu_b_sel;
wire [1:0] alu_ctrl;
wire [2:0] dm_rd_ctrl;
wire [1:0] dm_wr_ctrl;
wire [1:0] rf_wr_sel;
wire [2:0] comp_ctrl;
wire do_branch;
wire do_jump;

reg [31:0] rf_wd3;
wire [31:0] rf_rd1,rf_rd2;
wire [31:0] alu_a,alu_b,alu_out;
wire [31:0] dm_dout;

pc_calc pc_calc(clk,rst,branch,alu_out,pc,pc_plus4);

mem mem(clk,pc,inst,dm_rd_ctrl,dm_wr_ctrl,alu_out,rf_rd2,m_rf_addr,dm_dout,m_data);

imm imm(inst,imm_out);

controller controller(inst,rf_wr_en,alu_a_sel,alu_b_sel,alu_ctrl,dm_rd_ctrl,dm_wr_ctrl,rf_wr_sel,comp_ctrl,do_branch,do_jump);

always@(*)
begin
    case(rf_wr_sel)
    2'b00: rf_wd3 = 32'h0;
    2'b01: rf_wd3 = pc_plus4;
    2'b10: rf_wd3 = alu_out;//eg，sw要修改的数据位置
    2'b11: rf_wd3 = dm_dout;//eg，读取数据段给寄存器
    default: rf_wd3 = 32'h0;
    endcase
end

reg_file reg_file(clk,inst[19:15],inst[24:20],inst[11:7],rf_wd3,rf_wr_en,rf_rd1,rf_rd2,x18,x19,x20,x21,x22,x23);

assign alu_a = alu_a_sel ? rf_rd1 : pc;
assign alu_b = alu_b_sel ? imm_out : rf_rd2;

alu alu(alu_a,alu_b,alu_ctrl,alu_out);

bran bran(rf_rd1,rf_rd2,comp_ctrl,do_branch,do_jump,branch);
endmodule
```
&nbsp;
&nbsp;
&nbsp;
# 关于流水线CPU

### 数据通路：
![cpu2.png](/images/cpu2.png)

要求实现下列十条指令：
```
    add rd, rs1, rs2        # x[rd] = x[rs1] + x[rs2]
    addi rd, rs1, imm 	    # x[rd] = x[rs1] + sext(imm)
    sub rd, rs1, rs2        # x[rd] = x[rs1] - x[rs2]
    auipc rd, imm           # x[rd] = pc + sext(imm[31:12] << 12)
    lw rd, offset(rs1)      # x[rd] = M[x[rs1] + sext(offset)]
    sw rs2, offset(rs1)     # M[x[rs1]+sext(offset)=x[rs2]
    beq rs1, rs2, offset    # if (rs1 == rs2) pc += sext(offset)
    blt rs1, rs2, offset    # if (rs1 < s rs2) pc += sext(offset)
    jal rd, offset 	    # x[rd] = pc+4; pc += sext(offset) 
    jalr rd, offset(rs1)    # t =pc+4; pc=(x[rs1]+sext(offset))&~1; x[rd]=t
```
&nbsp;
&nbsp;
### 代码展示
```
module alu
#(parameter WIDTH = 4) 	//数据宽度
(output [WIDTH-1:0] y, 		//运算结果
output zf, 					//零标志
output cf, 					//进位/借位标志
output of, 					//溢出标志
input [WIDTH-1:0] a, b,		//两操作数
input [2:0]m						//操作类型
);
reg cf_r,of_r;
reg [WIDTH-1:0]y_r;
assign  zf=~|y;
assign  y=y_r;
assign  cf=cf_r;
assign  of=of_r;

localparam ADD=3'b000;
localparam SUB=3'b001;
localparam AND=3'b010;
localparam  OR=3'b011;
localparam XOR=3'b100;

always@(*) begin
    case (m)
        ADD: begin 
            {cf_r, y_r} = a + b;
            of_r = (~a[WIDTH-1] & ~b[WIDTH-1] & y[WIDTH-1]) | (a[WIDTH-1] & b[WIDTH-1] & ~y[WIDTH-1]) ;
        end
        SUB: begin 
            {cf_r, y_r} = a - b;
            of_r = (~a[WIDTH-1] & b[WIDTH-1] & y[WIDTH-1]) | (a[WIDTH-1] & ~b[WIDTH-1] & ~y[WIDTH-1]) ;
        end
        AND: begin 
            y_r=a&b;
        end
         OR: begin 
            y_r=a|b;
        end
        XOR: begin 
            y_r=a^b;
        end
        default: begin
        end
    endcase
end
endmodule

module mux_1
#(parameter N = 32)
(
    input i_sel,
    input [N-1:0]num0,num1,
    output [N-1:0]o_m
);
reg [N-1:0]m_r;
assign o_m=m_r;
always@(*)begin
    case (i_sel)
        1'd0: m_r=num0;
        1'd1: m_r=num1;
        default: begin end
    endcase
end
endmodule 

module mux_2
#(parameter N = 32)
(
    input [1:0]i_sel,
    input [N-1:0]num0,num1,num2,num3,
    output [N-1:0]o_m
);
reg [N-1:0]m_r;
assign o_m=m_r;
always@(*)begin
    case (i_sel)
        2'd0: m_r=num0;
        2'd1: m_r=num1;
        2'd2: m_r=num2;
        2'd3: m_r=num3;
        default: begin end
    endcase
end
endmodule

module register_file				//32 x WIDTH寄存器堆
#(parameter WIDTH = 32) 	//数据宽度
(
input clk,						//时钟（上升沿有效）
input [4:0] ra0,				//读端口0地址
output [WIDTH-1:0] rd0, 	//读端口0数据
input [4:0] ra1, 				//读端口1地址
output [WIDTH-1:0] rd1, 	//读端口1数据
input [4:0] wa, 				//写端口地址
input we,					//写使能，高电平有效
input [WIDTH-1:0] wd,		//写端口数据
input [4:0] dbgra,
output [WIDTH-1:0] dbgrd
);
reg [WIDTH-1:0] reg_storage [0:31];
reg [WIDTH-1:0] reg_rd0, reg_rd1, reg_dbg;
assign rd0=reg_rd0;
assign rd1=reg_rd1;
assign dbgrd=reg_dbg;

initial begin
$readmemh("register_file.vec", reg_storage);
end
always @(*) begin
    reg_rd0=reg_storage[ra0];
    reg_rd1=reg_storage[ra1];
    reg_dbg=reg_storage[dbgra];
    if(we&&wa!=5'd0) begin
        if(ra0==wa)
            reg_rd0=wd;
        if(ra1==wa)
            reg_rd1=wd;
        if(dbgra==wa)
            reg_dbg=wd;
    end
end
always @(posedge clk ) begin
    if(we) begin
        if(wa!=5'd0)
            reg_storage[wa]<=wd;
    end
end
endmodule

module cpu_multi_cycle	//单周期CPU
(input clk,			//时钟（上升沿有效）
input rst,				//异步复位，高电平有效
output [15:0]status,
output [31:0]m_data,rf_data,
input [15:0]m_rf_addr,
input [2:0]i_sel,
output reg [31:0]o_sel_data
);

//pipeline regs
reg [31:0]PC=32'd0;
reg [31:0]IF_ID_NPC=32'd0,IF_ID_IR=32'd0;
reg [31:0]ID_EX_NPC=32'd0,ID_EX_IR=32'd0,ID_EX_A=32'd0,ID_EX_B=32'd0,ID_EX_IMMI=32'd0;
reg [31:0]EX_MEM_NPC=32'd0,EX_MEM_Y=32'd0,EX_MEM_B=32'd0;
reg [31:0]MEM_WB_MDR=32'd0,MEM_WB_Y=32'd0;
reg EX_MEM_ZF=1'd0;
reg [4:0]EX_MEM_WA=5'd0,MEM_WB_WA=5'd0;


//control regs
reg [1:0]ID_EX_WB=2'd0,EX_MEM_WB=2'd0,MEM_WB_WB=2'd0;
reg [2:0]ID_EX_M=3'd0,EX_MEM_M=3'd0;
reg [5:0]ID_EX_EX=6'd0;
reg EX_MEM_J=1'd0;

reg [1:0]wb_ctrl;
reg [2:0]m_ctrl;
reg [5:0]ex_ctrl;

//
wire [31:0]alu_result;
wire [31:0]write_reg_data,read_reg_data_1,read_reg_data_2;
wire [31:0]read_mem_data;
wire [31:0]alu_in_2;
wire [4:0]write_reg_addr;


wire RegWrite,MemtoReg;
wire Jump,Branch,MemRead,MemWrite;
wire PCSrc;
wire RegDst,ALUSrc;
wire [2:0]ALUOp;
reg [2:0]ALUm;
wire Zero;
wire [31:0]instruction;
wire [31:0]pc_plus,pc_next;
wire [31:0]br_pc,j_pc,n_pc;

//dbg TODO
/*
assign status={PCSrc,PCwe,IorD,MemWrite,IRWrite,RegDst,MemtoReg,RegWrite,ALUm,ALUSrcA,ALUSrcB,Zero};
always @(*) begin
    case (i_sel)
        3'd1: o_sel_data=PC;
        3'd2: o_sel_data=IR;
        3'd3: o_sel_data=MemoryDataRegister;
        3'd4: o_sel_data=A;
        3'd5: o_sel_data=B;
        3'd6: o_sel_data=ALUOut;
        3'd7: o_sel_data=read_mem_data;//?
        default: begin end
    endcase
end*/

//instruction rom
dist_rom ins_rom(.a(PC[9:2]), .spo(instruction));

//reg file
register_file #(32) my_rf(.clk(clk), .ra0(IF_ID_IR[25:21]), .rd0(read_reg_data_1), .ra1(IF_ID_IR[20:16]), .rd1(read_reg_data_2), .wa(MEM_WB_WA), .we(RegWrite), .wd(write_reg_data),.dbgra(m_rf_addr[4:0]),.dbgrd(rf_data));

mux_1 #(5) write_register_addr_mux(.i_sel(RegDst),.num0(ID_EX_IR[20:16]),.num1(ID_EX_IR[15:11]),.o_m(write_reg_addr));
mux_1 #(32) write_register_data_mux(.i_sel(MemtoReg),.num0(MEM_WB_Y),.num1(MEM_WB_MDR),.o_m(write_reg_data));

//control
//opcode
localparam LW   =   6'b100011;
localparam SW   =   6'b101011;
localparam ADD  =   6'b000000;
localparam ADDI =   6'b001000;
localparam BEQ  =   6'b000100;
localparam J    =   6'b000010;

//ALUOp
localparam ALUOP_ADD      =3'd0;
localparam ALUOP_SUB      =3'd1;
localparam ALUOP_FUNCT    =3'd2;
localparam ALUOP_NOP      =3'd3;

always @(*) begin
    {wb_ctrl,m_ctrl,ex_ctrl}=11'd0;
    if(!rst)
        case (IF_ID_IR[31:26])
            LW  :begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b1,1'b1,1'b0,1'b1,1'b0,1'b0,1'b0,ALUOP_ADD,1'b1};
            end
            SW  :begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b0,1'b0,1'b0,1'b0,1'b1,1'b0,1'b0,ALUOP_ADD,1'b1};
            end
            ADD :begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b1,1'b0,1'b0,1'b0,1'b0,1'b0,1'b1,ALUOP_FUNCT,1'b0};
            end
            ADDI:begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b1,1'b0,1'b0,1'b0,1'b0,1'b0,1'b0,ALUOP_ADD,1'b1};
            end
            BEQ :begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b0,1'b0,1'b1,1'b0,1'b0,1'b0,1'b0,ALUOP_NOP,1'b0};
            end
            J   :begin
                {wb_ctrl,m_ctrl,ex_ctrl}={1'b0,1'b0,1'b1,1'b0,1'b0,1'b1,1'b0,ALUOP_ADD,1'b1};
            end
            default: begin
            
            end
        endcase
end

assign {Jump,RegDst,ALUOp,ALUSrc}=ID_EX_EX;
assign {Branch,MemRead,MemWrite}=EX_MEM_M;
assign {RegWrite,MemtoReg}=MEM_WB_WB;

//alu
mux_1 #(32) alu_in_2_mux(.i_sel(ALUSrc),.num0(ID_EX_B),.num1(ID_EX_IMMI),.o_m(alu_in_2));

alu #(32) arith_ALU(.y(alu_result),.zf(Zero),.a(ID_EX_A),.b(alu_in_2),.m(ALUm));

//alu control
//TODO
localparam m_ADD=3'b000;
localparam m_SUB=3'b001;
localparam m_AND=3'b010;
localparam m_OR =3'b011;
localparam m_XOR=3'b100;
localparam m_NOP=3'b101;

always @(*) begin
    case (ALUOp)
        ALUOP_ADD   :   ALUm=m_ADD;
        ALUOP_SUB   :   ALUm=m_SUB;
        ALUOP_FUNCT :   begin
            case (ID_EX_IR[5:0])
                6'b100000:ALUm=m_ADD; 
                default: begin
                    ALUm=m_NOP;
                end
            endcase
        end
        default: begin
            ALUm=m_NOP;
        end
    endcase
end

//TODO MemRead
//data memory
dist_ram data_ram(.clk(clk), .we(MemWrite), .d(EX_MEM_B), .a(EX_MEM_Y[9:2]), .spo(read_mem_data), .dpra(m_rf_addr[9:2]), .dpo(m_data));

//pc mux
mux_1 #(32) pc_mux(.i_sel(PCSrc),.num0(pc_plus),.num1(EX_MEM_NPC),.o_m(pc_next));
assign pc_plus=PC+32'd4;

//pc
always @(posedge clk or posedge rst) begin
    if(rst) begin
        PC<=32'd0;
    end 
    else begin
        PC<=pc_next;
    end
end

//IF-ID
always @(posedge clk or posedge rst) begin
    if(rst) begin
        IF_ID_NPC<=32'd0;
        IF_ID_IR<=32'd0;
    end
    else begin
        IF_ID_NPC<=pc_plus;
        IF_ID_IR<=instruction;
    end
end

//ID-EX
always @(posedge clk or posedge rst) begin
    if(rst) begin
        ID_EX_NPC<=32'd0;
        ID_EX_IR<=32'd0;
        ID_EX_A<=32'd0;
        ID_EX_B<=32'd0;
        ID_EX_IMMI<=32'd0;
        ID_EX_WB<=2'd0;
        ID_EX_M<=3'd0;
        ID_EX_EX<=6'd0;
    end
    else begin
        ID_EX_NPC<=IF_ID_NPC;
        ID_EX_IR<=IF_ID_IR;
        ID_EX_A<= read_reg_data_1;
        ID_EX_B<= read_reg_data_2;
        ID_EX_IMMI<={{16{IF_ID_IR[15]}},{IF_ID_IR[15:0]}};
        ID_EX_WB<=wb_ctrl;
        ID_EX_M<=m_ctrl;
        ID_EX_EX<=ex_ctrl;
    end
end

//EX-MEM
mux_1 #(32) npc_mux(.i_sel(Jump),.num0(br_pc),.num1(j_pc),.o_m(n_pc));
assign br_pc=ID_EX_NPC+{ID_EX_IMMI[29:0],2'd0};
assign j_pc ={ID_EX_NPC[31:28],{ID_EX_IR[25:2]},{2'd0}};
always @(posedge clk or posedge rst) begin
    if(rst) begin
        EX_MEM_NPC<=32'd0;
        EX_MEM_ZF<=1'd0;
        EX_MEM_Y<=32'd0;
        EX_MEM_B<=32'd0;
        EX_MEM_WA<=5'd0;
        EX_MEM_WB<=2'd0;
        EX_MEM_M<=3'd0;
        EX_MEM_J<=1'd0;
    end
    else begin
        EX_MEM_NPC<=n_pc;
        EX_MEM_ZF<=Zero;
        EX_MEM_Y<=alu_result;
        EX_MEM_B<=ID_EX_B;
        EX_MEM_WA<=write_reg_addr;
        EX_MEM_WB<=ID_EX_WB;
        EX_MEM_M<=ID_EX_M;
        EX_MEM_J<=Jump;
    end
end
assign PCSrc=(Branch&EX_MEM_ZF)|EX_MEM_J;

//MEM-WB
always @(posedge clk or posedge rst) begin
    if(rst) begin
        MEM_WB_MDR<=32'd0;
        MEM_WB_Y<=32'd0;
        MEM_WB_WA<=5'd0;
        MEM_WB_WB<=2'd0;
    end
    else begin
        MEM_WB_MDR<=read_mem_data;
        MEM_WB_Y<=EX_MEM_Y;
        MEM_WB_WA<=EX_MEM_WA;
        MEM_WB_WB<=EX_MEM_WB;
    end
end
endmodule
```
