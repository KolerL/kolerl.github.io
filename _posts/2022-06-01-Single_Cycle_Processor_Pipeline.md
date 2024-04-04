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

这篇博客以中国科学技术大学计算机组成原理（COD2022）课程为依托，针对RISC-V CPU 进行设计，仅做 Verilog 代码展示。
&nbsp;
&nbsp;
&nbsp;
# 关于单周期CPU

### 数据通路：
![cpu1.png](/images/cpu1.png)

要求实现下列十条指令：
```
    add rd, rs1, rs2		    # x[rd] = x[rs1] + x[rs2]
    addi rd, rs1, imm 	    # x[rd] = x[rs1] + sext(imm)
    sub rd, rs1, rs2		    # x[rd] = x[rs1] - x[rs2]
    auipc rd, imm           # x[rd] = pc + sext(imm[31:12] << 12)
    lw rd, offset(rs1)      # x[rd] = M[x[rs1] + sext(offset)]
    sw rs2, offset(rs1)     # M[x[rs1]+sext(offset)=x[rs2]
    beq rs1, rs2, offset    # if (rs1 == rs2) pc += sext(offset)
    blt rs1, rs2, offset    # if (rs1 < s rs2) pc += sext(offset)
    jal rd, offset 	        # x[rd] = pc+4; pc += sext(offset) 
    jalr rd, offset(rs1)    # t =pc+4; pc=(x[rs1]+sext(offset))&~1; x[rd]=t 
```

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
    add rd, rs1, rs2		    # x[rd] = x[rs1] + x[rs2]
    addi rd, rs1, imm 	    # x[rd] = x[rs1] + sext(imm)
    sub rd, rs1, rs2		    # x[rd] = x[rs1] - x[rs2]
    auipc rd, imm           # x[rd] = pc + sext(imm[31:12] << 12)
    lw rd, offset(rs1)      # x[rd] = M[x[rs1] + sext(offset)]
    sw rs2, offset(rs1)     # M[x[rs1]+sext(offset)=x[rs2]
    beq rs1, rs2, offset    # if (rs1 == rs2) pc += sext(offset)
    blt rs1, rs2, offset    # if (rs1 < s rs2) pc += sext(offset)
    jal rd, offset 	        # x[rd] = pc+4; pc += sext(offset) 
    jalr rd, offset(rs1)    # t =pc+4; pc=(x[rs1]+sext(offset))&~1; x[rd]=t 
```

### 代码展示
