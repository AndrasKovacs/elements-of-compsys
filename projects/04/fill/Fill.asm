// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.


//(MAIN)
	@SCREEN
	D=A
	@i
	M=D

	@KBD
	D=M
	@LOOP_FILL
	D;JNE

	(LOOP_CLEAR)
		@i
		D=M
		@KBD
		D=D-A
		@MAIN
		D;JEQ

		@i
		A=M
		M=0

		@i
		M=M+1

		@LOOP_CLEAR
		0;JMP

	//(LOOP_FILL)
		@i
		D=M //malacossokdoskd
		@KBD
		D=D-A //gegerge
		@MAIN
		//D;JEQ

		//@i
		A=M
		M=-1

		@i
		M=M+1

		@LOOP_FILL
		0;JMP