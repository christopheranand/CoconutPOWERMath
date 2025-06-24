	.csect .text[PR],2
	.file	"square.c","IBM Open XL C/C++ for AIX 17.1.1 (5725-C72, 5765-J18), version 17.1.1.4, LLVM version 15.0.0git"
	.globl	square[DS]
	.globl	.square
	.align	4
	.csect square[DS],2
	.vbyte	4, .square
	.vbyte	4, TOC[TC0]
	.vbyte	4, 0
	.csect .text[PR],2
.square:
	stw 3, -8(1)
	stw 4, -12(1)
	stw 5, -16(1)
	li 3, 0
	stw 3, -20(1)
	b L..BB0_1
L..BB0_1:
	lwz 3, -12(1)
	lwz 4, -20(1)
	slwi 4, 4, 3
	lfdx 0, 3, 4
	xsadddp 0, 0, 0
	lwz 3, L..C0(2)
	lfdx 1, 3, 4
	xsadddp 0, 0, 1
	lwz 3, -8(1)
	stfdx 0, 3, 4
	lwz 3, -20(1)
	addi 3, 3, 1
	stw 3, -20(1)
	b L..BB0_2
L..BB0_2:
	lwz 3, -20(1)
	lwz 4, -16(1)
	lwz 4, 0(4)
	cmpw	3, 4
	blt	0, L..BB0_1
	b L..BB0_3
L..BB0_3:
	blr
L..square0:
	.vbyte	4, 0x00000000
	.byte	0x00
	.byte	0x09
	.byte	0x22
	.byte	0x40
	.byte	0x00
	.byte	0x00
	.byte	0x03
	.byte	0x01
	.vbyte	4, 0x00000000
	.vbyte	4, L..square0-.square
	.vbyte	2, 0x0006
	.byte	"square"

	.csect constants[RO],3
	.globl	constants[RO]
	.align	3
	.vbyte	4, 0
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1073741824
	.vbyte	4, 0
	.vbyte	4, 1074266112
	.vbyte	4, 0
	.vbyte	4, 1074790400
	.vbyte	4, 0
	.vbyte	4, 1075052544
	.vbyte	4, 0
	.vbyte	4, 1075314688
	.vbyte	4, 0
	.vbyte	4, 1075576832
	.vbyte	4, 0
	.csect memregion[RW],3
	.globl	memregion[RW]
	.align	3
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.vbyte	4, 1072693248
	.vbyte	4, 0
	.toc
L..C0:
	.tc constants[TC],constants[RO]
