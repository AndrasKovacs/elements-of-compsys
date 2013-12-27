//  SP = 256
@256
D=A
@SP
M=D

//  goto entry point
@__end_of_init
0;JMP

(__eq)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@__0
M;JEQ
@SP
A=M
M=1
@__1
0;JMP
(__0)
@SP
A=M
M=0
(__1)
@SP
M=M+1
@13
A=M
0;JMP

(__gt)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@__2
M;JGT
@SP
A=M
M=1
@__3
0;JMP
(__2)
@SP
A=M
M=0
(__3)
@SP
M=M+1
@13
A=M
0;JMP

(__lt)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@__4
M;JLT
@SP
A=M
M=1
@__5
0;JMP
(__4)
@SP
A=M
M=0
(__5)
@SP
M=M+1
@13
A=M
0;JMP

(__add)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M+D
@SP
M=M+1
@13
A=M
0;JMP

(__sub)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M-D
@SP
M=M+1
@13
A=M
0;JMP

(__or)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M|D
@SP
M=M+1
@13
A=M
0;JMP

(__and)
@SP
M=M-1
A=M
D=M
@SP
M=M-1
A=M
M=M&D
@SP
M=M+1
@13
A=M
0;JMP

(__neg)
@SP
M=M-1
A=M
M=-M
@SP
M=M+1
@13
A=M
0;JMP

(__not)
@SP
M=M-1
A=M
M=!M
@SP
M=M+1
@13
A=M
0;JMP

(__push_w_offset)
@15
D=M
@14
A=D+M
D=M
@SP
A=M
M=D
@SP
M=M+1
@13
A=M
0;JMP

(__push_static)
@14
A=M
D=M
@SP
A=M
M=D
@SP
M=M+1
@13
A=M
0;JMP

(__pop_static)
@SP
M=M-1
A=M
D=M
@14
A=M
M=D
@13
A=M
0;JMP

(__pop_w_offset)
@15
D=M
@14
M=D+M
@SP
M=M-1
A=M
D=M
@14
A=M
M=D
@13
A=M
0;JMP


(__end_of_init)







@7
D=A
@SP
A=M
M=D
@SP
M=M+1

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

@__6
D=A
@13
M=D
@__add
0;JMP
(__6)


