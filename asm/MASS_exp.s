.set r0,0; .set SP,1; .set RTOC,2; .set r3,3; .set r4,4
.set r5,5; .set r6,6; .set r7,7; .set r8,8; .set r9,9
.set r10,10; .set r11,11; .set r12,12; .set r13,13; .set r14,14
.set r15,15; .set r16,16; .set r17,17; .set r18,18; .set r19,19
.set r20,20; .set r21,21; .set r22,22; .set r23,23; .set r24,24
.set r25,25; .set r26,26; .set r27,27; .set r28,28; .set r29,29
.set r30,30; .set r31,31
.set fp0,0; .set fp1,1; .set fp2,2; .set fp3,3; .set fp4,4
.set fp5,5; .set fp6,6; .set fp7,7; .set fp8,8; .set fp9,9
.set fp10,10; .set fp11,11; .set fp12,12; .set fp13,13; .set fp14,14
.set fp15,15; .set fp16,16; .set fp17,17; .set fp18,18; .set fp19,19
.set fp20,20; .set fp21,21; .set fp22,22; .set fp23,23; .set fp24,24
.set fp25,25; .set fp26,26; .set fp27,27; .set fp28,28; .set fp29,29
.set fp30,30; .set fp31,31
.set v0,0; .set v1,1; .set v2,2; .set v3,3; .set v4,4
.set v5,5; .set v6,6; .set v7,7; .set v8,8; .set v9,9
.set v10,10; .set v11,11; .set v12,12; .set v13,13; .set v14,14
.set v15,15; .set v16,16; .set v17,17; .set v18,18; .set v19,19
.set v20,20; .set v21,21; .set v22,22; .set v23,23; .set v24,24
.set v25,25; .set v26,26; .set v27,27; .set v28,28; .set v29,29
.set v30,30; .set v31,31
.set x0,0; .set x1,1; .set x2,2; .set x3,3; .set x4,4
.set x5,5; .set x6,6; .set x7,7; .set x8,8; .set x9,9
.set x10,10; .set x11,11; .set x12,12; .set x13,13; .set x14,14
.set x15,15; .set x16,16; .set x17,17; .set x18,18; .set x19,19
.set x20,20; .set x21,21; .set x22,22; .set x23,23; .set x24,24
.set x25,25; .set x26,26; .set x27,27; .set x28,28; .set x29,29
.set x30,30; .set x31,31; .set x32,32; .set x33,33; .set x34,34
.set x35,35; .set x36,36; .set x37,37; .set x38,38; .set x39,39
.set x40,40; .set x41,41; .set x42,42; .set x43,43; .set x44,44
.set x45,45; .set x46,46; .set x47,47; .set x48,48; .set x49,49
.set x50,50; .set x51,51; .set x52,52; .set x53,53; .set x54,54
.set x55,55; .set x56,56; .set x57,57; .set x58,58; .set x59,59
.set x60,60; .set x61,61; .set x62,62; .set x63,63
.set q0,0; .set q1,1; .set q2,2; .set q3,3; .set q4,4
.set q5,5; .set q6,6; .set q7,7; .set q8,8; .set q9,9
.set q10,10; .set q11,11; .set q12,12; .set q13,13; .set q14,14
.set q15,15; .set q16,16; .set q17,17; .set q18,18; .set q19,19
.set q20,20; .set q21,21; .set q22,22; .set q23,23; .set q24,24
.set q25,25; .set q26,26; .set q27,27; .set q28,28; .set q29,29
.set q30,30; .set q31,31
.set MQ,0; .set XER,1; .set DSCR,3; .set FROM_RTCU,4; .set FROM_RTCL,5
.set FROM_DEC,6; .set LR,8; .set CTR,9; .set AMR,13; .set TID,17; .set DSISR,18
.set DAR,19; .set TO_RTCU,20; .set TO_RTCL,21; .set TO_DEC,22; .set SDR_0,24
.set SDR_1,25; .set SRR_0,26; .set SRR_1,27
.set BO_dCTR_NZERO_AND_NOT,0; .set BO_dCTR_NZERO_AND_NOT_1,1
.set BO_dCTR_ZERO_AND_NOT,2; .set BO_dCTR_ZERO_AND_NOT_1,3
.set BO_IF_NOT,4; .set BO_IF_NOT_1,5; .set BO_IF_NOT_2,6
.set BO_IF_NOT_3,7; .set BO_dCTR_NZERO_AND,8; .set BO_dCTR_NZERO_AND_1,9
.set BO_dCTR_ZERO_AND,10; .set BO_dCTR_ZERO_AND_1,11; .set BO_IF,12
.set BO_IF_1,13; .set BO_IF_2,14; .set BO_IF_3,15; .set BO_dCTR_NZERO,16
.set BO_dCTR_NZERO_1,17; .set BO_dCTR_ZERO,18; .set BO_dCTR_ZERO_1,19
.set BO_ALWAYS,20; .set BO_ALWAYS_1,21; .set BO_ALWAYS_2,22
.set BO_ALWAYS_3,23; .set BO_dCTR_NZERO_8,24; .set BO_dCTR_NZERO_9,25
.set BO_dCTR_ZERO_8,26; .set BO_dCTR_ZERO_9,27; .set BO_ALWAYS_8,28
.set BO_ALWAYS_9,29; .set BO_ALWAYS_10,30; .set BO_ALWAYS_11,31
.set CR0_LT,0; .set CR0_GT,1; .set CR0_EQ,2; .set CR0_SO,3
.set CR1_FX,4; .set CR1_FEX,5; .set CR1_VX,6; .set CR1_OX,7
.set CR2_LT,8; .set CR2_GT,9; .set CR2_EQ,10; .set CR2_SO,11
.set CR3_LT,12; .set CR3_GT,13; .set CR3_EQ,14; .set CR3_SO,15
.set CR4_LT,16; .set CR4_GT,17; .set CR4_EQ,18; .set CR4_SO,19
.set CR5_LT,20; .set CR5_GT,21; .set CR5_EQ,22; .set CR5_SO,23
.set CR6_LT,24; .set CR6_GT,25; .set CR6_EQ,26; .set CR6_SO,27
.set CR7_LT,28; .set CR7_GT,29; .set CR7_EQ,30; .set CR7_SO,31
.set TO_LT,16; .set TO_GT,8; .set TO_EQ,4; .set TO_LLT,2; .set TO_LGT,1

	.rename	H.10.NO_SYMBOL{PR},""
	.rename	H.18.NO_SYMBOL{TC},""
	.rename	H.20.NO_SYMBOL{RO},""
	.rename	H.26.vsexp_{TC},"vsexp_"
	.rename	H.28.vsexp{TC},"vsexp"

	.lglobl	H.10.NO_SYMBOL{PR}      
	.globl	.vsexp,hidden           
	.globl	.vsexp_                 
	.lglobl	H.20.NO_SYMBOL{RO}      
	.globl	vsexp{DS},hidden        
	.globl	vsexp_                  


# .text section
	.file	"/buildbot_worker/official-worker/mass-aix-prod1711/mass/mass/source/loopgen/expf_v_p7.cpp","Tue Nov  8 17:59:59 2022 ","IBM XL C/C++ for AIX, Version 16.1.1.0"
	.machine	"pwr8"


	.csect	H.10.NO_SYMBOL{PR}, 5   
.vsexp:                                 # 0x00000000 (H.10.NO_SYMBOL)
.vsexp_:                                # 0x00000000 (H.10.NO_SYMBOL)
	stwu       SP,-624(SP)
	addi       r10,r0,0
	lwz        r0,0(r5)
	cmpi       0,0,r0,0
	bc         BO_IF_NOT,CR0_GT,__L21e8
	cmpi       0,0,r0,4
	bc         BO_IF,CR0_LT,__L181c
	vspltisb   v0,7
	rlwinm     r5,r0,30,2,31
	cmpi       0,0,r5,10
	bc         BO_IF,CR0_LT,__Le40
	addi       r6,r0,384
	stvx       v31,SP,r6
	addis      r8,r0,26214
	vspltisb   v1,-1
	lxvd2x     x6,r0,r4
	lwz        r7,T.18.NO_SYMBOL(RTOC)
	addi       r6,r0,368
	stvx       v30,SP,r6
	addi       r8,r8,26215
	vspltisw   v2,12
	addi       r12,r0,128
	addi       r10,r0,80
	addi       r6,r0,352
	stvx       v29,SP,r6
	addi       r6,r0,336
	stvx       v28,SP,r6
	addi       r6,r0,320
	stvx       v27,SP,r6
	addi       r6,r0,304
	stvx       v26,SP,r6
	addi       r6,r0,288
	stvx       v25,SP,r6
	addi       r6,r0,272
	stvx       v24,SP,r6
	addi       r6,r0,256
	stvx       v23,SP,r6
	addi       r6,r0,240
	stvx       v22,SP,r6
	addi       r6,r0,224
	stvx       v21,SP,r6
	addi       r9,r0,400
	lxvdsx     x0,r7,r9
	xxlor      x42,x0,x0
	addi       r9,r0,64
	addi       r6,r0,208
	stvx       v20,SP,r6
	stfd       fp31,616(SP)
	stfd       fp30,608(SP)
	addi       r6,r5,-10
	stfd       fp29,600(SP)
	stfd       fp28,592(SP)
	stfd       fp27,584(SP)
	stfd       fp26,576(SP)
	stfd       fp25,568(SP)
	stfd       fp24,560(SP)
	stfd       fp23,552(SP)
	stfd       fp22,544(SP)
	stfd       fp21,536(SP)
	stfd       fp20,528(SP)
	stfd       fp19,520(SP)
	stfd       fp18,512(SP)
	stfd       fp17,504(SP)
	stfd       fp16,496(SP)
	stfd       fp15,488(SP)
	stfd       fp14,480(SP)
	stw        r31,476(SP)
	stw        r30,472(SP)
	rlwinm     r30,r6,1,31,31
	addi       r11,r0,408
	lxvdsx     x1,r7,r11
	stw        r29,468(SP)
	stw        r28,464(SP)
	mulhw      r6,r6,r8
	stw        r27,460(SP)
	addi       r8,r0,576
	lxvdsx     x2,r7,r8
	stw        r26,456(SP)
	addi       r31,r0,96
	stw        r25,452(SP)
	srawi      r6,r6,2
	addi       r8,r0,584
	lxvdsx     x3,r7,r8
	stw        r24,448(SP)
	add        r6,r6,r30
	stw        r23,444(SP)
	addi       r30,r0,144
	stw        r22,440(SP)
	stw        r21,436(SP)
	stw        r20,432(SP)
	stw        r19,428(SP)
	stw        r18,424(SP)
	addi       r11,r0,112
	stw        r17,420(SP)
	stw        r16,416(SP)
	stw        r15,412(SP)
	stw        r14,408(SP)
	ori        r0,r0,0x0000
	addi       r8,r6,1
	xvmaddasp  x42,x6,x1
	addi       r28,r0,16
	lxvd2x     x27,r4,r28
	mtspr      CTR,r8
	xxlor      x44,x0,x0
	addi       r8,r0,480
	lxvd2x     x35,r7,r8
	xxlor      x45,x0,x0
	xxlor      x49,x0,x0
	xvmaddasp  x44,x27,x1
	xxland     x4,x2,x42
	vperm      v9,v12,v12,v3
	vperm      v4,v10,v10,v3
	lxvd2x     x25,r4,r9
	addi       r29,r0,-160
	xxland     x5,x2,x44
	xvsubsp    x26,x4,x3
	addi       r27,r0,-144
	mulli      r6,r6,10
	addi       r26,r0,-112
	addi       r8,r0,496
	lxvdsx     x4,r7,r8
	xvsubsp    x28,x5,x3
	addi       r24,r0,-80
	addi       r22,r0,-96
	addi       r23,r0,-208
	xvmsubasp  x26,x6,x1
	addi       r25,r0,-128
	xxsel      x46,x4,x36,x32
	addi       r21,r0,-64
	addi       r8,r0,544
	lxvd2x     x36,r7,r8
	xxsel      x41,x4,x41,x32
	xvmsubasp  x28,x27,x1
	addi       r8,r0,560
	lxvd2x     x37,r7,r8
	addi       r17,r0,-16
	addi       r20,r0,-192
	lxvd2x     x23,r4,r10
	addi       r18,r0,-176
	addi       r8,r0,592
	lxvdsx     x5,r7,r8
	vperm      v11,v4,v5,v14
	xvmaddasp  x26,x6,x5
	addi       r8,r0,416
	lxvdsx     x6,r7,r8
	xvsubsp    x14,x26,x43
	xvmaddasp  x28,x27,x5
	vperm      v11,v4,v5,v9
	addi       r8,r0,424
	lxvdsx     x7,r7,r8
	xvsubsp    x28,x28,x43
	addi       r19,r0,32
	lxvd2x     x29,r4,r19
	xvmaddasp  x49,x29,x1
	addi       r8,r0,432
	lxvdsx     x8,r7,r8
	xxland     x48,x2,x49
	vperm      v11,v17,v17,v3
	xvsubsp    x27,x48,x3
	addi       r8,r0,440
	lxvdsx     x38,r7,r8
	xxsel      x43,x4,x43,x32
	xvmsubasp  x27,x29,x1
	addi       r8,r0,448
	lxvdsx     x9,r7,r8
	xxlor      x48,x0,x0
	xvcmpgtsp  x51,x8,x42
	xvcmpgtsp  x24,x8,x44
	addi       r8,r0,456
	lxvdsx     x10,r7,r8
	xvmaddasp  x48,x25,x1
	xvmaddasp  x27,x29,x5
	addi       r16,r0,48
	lxvd2x     x22,r4,r16
	xxland     x26,x2,x48
	xvmaddasp  x45,x22,x1
	addi       r8,r0,464
	lxvdsx     x11,r7,r8
	xxland     x47,x2,x45
	xvsubsp    x50,x47,x3
	vslw       v15,v10,v2
	addi       r8,r0,512
	lxvd2x     x39,r7,r8
	xxsel      x20,x47,x10,x51
	addi       r8,r0,528
	lxvd2x     x40,r7,r8
	xxland     x58,x11,x20
	vperm      v31,v7,v8,v14
	addi       r8,r0,600
	lxvdsx     x12,r7,r8
	vperm      v14,v4,v5,v11
	xxlor      x21,x12,x12
	xxlor      x17,x12,x12
	addi       r8,r0,608
	lxvdsx     x13,r7,r8
	xvmaddasp  x21,x14,x13
	addi       r8,r0,616
	lxvdsx     x31,r7,r8
	addi       r8,r0,624
	lxvdsx     x30,r7,r8
	addi       r8,r0,80
	xvmaddasp  x17,x28,x13
	xvmaddmsp  x21,x14,x31
	xvmsubasp  x50,x22,x1
	xvmaddmsp  x21,x14,x30
	vperm      v20,v7,v8,v9
	xxlor      x62,x0,x0
	xvsubsp    x26,x26,x3
	xvsubsp    x20,x27,x46
	vperm      v24,v7,v8,v11
	xvmaddmsp  x17,x28,x31
	xvmaddmsp  x14,x21,x9
	xvmaddasp  x50,x22,x5
	xxlor      x21,x12,x12
	vadduwm    v9,v6,v15
	xvmaddasp  x62,x23,x1
	xxsel      x57,x9,x41,x51
	vslw       v14,v12,v2
	xvcmpgtsp  x61,x8,x49
	xxsel      x19,x46,x10,x24
	xvmaddmsp  x17,x28,x30
	vperm      v9,v13,v13,v3
	xvmaddasp  x21,x20,x13
	xvcmpgtsp  x18,x8,x45
	xxsel      x47,x4,x41,x32
	xvcmpgtsp  x16,x6,x49
	vadduwm    v14,v6,v14
	addi       r7,r3,160
	xxland     x19,x11,x19
	xvmsubasp  x26,x25,x1
	xvmaddmsp  x28,x17,x9
	vslw       v11,v17,v2
	xvmaddmsp  x21,x20,x31
	xvmulsp    x19,x28,x19
	vperm      v9,v4,v5,v15
	xxsel      x59,x43,x10,x61
	xvsubsp    x22,x50,x41
	vadduwm    v28,v6,v11
	vperm      v9,v16,v16,v3
	xvmaddasp  x26,x25,x5
	xvmulsp    x43,x14,x58
	vslw       v19,v13,v2
	xvmaddmsp  x21,x20,x30
	xxland     x58,x2,x62
	xvmaddmsp  x20,x21,x9
	xvmulsp    x63,x43,x63
	xxsel      x25,x51,x10,x18
	xxsel      x43,x4,x41,x32
	vadduwm    v23,v6,v19
	xxsel      x60,x9,x60,x61
	xxlxor     x51,x16,x33
	xvcmpgtsp  x61,x42,x7
	xxlor      x16,x12,x12
	lxvd2x     x27,r4,r31
	xxlor      x41,x0,x0
	xvcmpgtsp  x15,x6,x44
	xxland     x14,x9,x57
	xvmulsp    x63,x63,x14
	vperm      v22,v4,v5,v11
	xxsel      x57,x9,x46,x24
	xvmaddasp  x16,x22,x13
	xxland     x14,x11,x25
	xxland     x46,x11,x59
	vperm      v27,v7,v8,v15
	xvmaddmsp  x16,x22,x31
	xvsubsp    x25,x26,x54
	xvsubsp    x47,x58,x3
	xvmaddasp  x41,x27,x1
	stxvd2x    x56,SP,r8
	xvmulsp    x20,x20,x46
	xxsel      x58,x9,x55,x18
	vperm      v21,v7,v8,v11
	xvmsubasp  x47,x23,x1
	xvcmpgtsp  x56,x6,x42
	xvmulsp    x43,x19,x52
	xxsel      x26,x63,x11,x61
	xxland     x18,x9,x57
	xvmaddasp  x47,x23,x5
	xvmaddmsp  x16,x22,x30
	xvcmpgtsp  x17,x44,x7
	xvmulsp    x63,x43,x18
	xxlor      x42,x0,x0
	vperm      v11,v9,v9,v3
	vperm      v12,v30,v30,v3
	addi       r15,r0,96
	stxvd2x    x59,SP,r15
	lxvd2x     x29,r4,r11
	xvcmpgtsp  x50,x49,x7
	xxlxor     x49,x33,x56
	xxlor      x18,x12,x12
	addi       r15,r0,112
	stxvd2x    x58,SP,r15
	xxlxor     x24,x15,x33
	xvmaddasp  x42,x29,x1
	xxsel      x56,x4,x44,x32
	vperm      v12,v4,v5,v24
	xxland     x15,x9,x60
	xvmaddasp  x18,x25,x13
	xxland     x60,x2,x41
	lxvd2x     x28,r4,r12
	xvsubsp    x21,x60,x3
	xvsubsp    x23,x47,x44
	xvcmpgtsp  x58,x8,x62
	vslw       v15,v30,v2
	lxvd2x     x52,SP,r8
	xvmulsp    x59,x20,x52
	xvmsubasp  x21,x27,x1
	xxsel      x46,x4,x43,x32
	vadduwm    v29,v6,v15
	xvmaddmsp  x18,x25,x31
	xvmaddmsp  x22,x16,x9
	xxlor      x43,x0,x0
	xvcmpgtsp  x16,x6,x45
	xvmaddasp  x43,x28,x1
	xvmaddasp  x21,x27,x5
	vslw       v12,v16,v2
	xvmulsp    x14,x22,x14
	xxsel      x22,x63,x11,x17
	xxland     x49,x26,x49
	xxlor      x63,x12,x12
	vperm      v23,v4,v5,v14
	addi       r8,r0,144
	stxvd2x    x61,SP,r8
	vperm      v25,v10,v10,v3
	vperm      v24,v7,v8,v24
	addi       r14,r0,128
	stxvd2x    x55,SP,r14
	xxland     x55,x22,x24
	xvcmpgtsp  x24,x45,x7
	xxsel      x45,x4,x57,x32
	lxvd2x     x57,SP,r8
	xvmaddasp  x63,x23,x13
	xvcmpgtsp  x20,x8,x48
	xxsel      x17,x47,x10,x58
	xvcmpgtsp  x61,x6,x48
	xvcmpgtsp  x26,x62,x7
	xxsel      x54,x44,x10,x20
	xvmaddmsp  x18,x25,x30
	xxland     x19,x2,x42
	lxvd2x     x27,r4,r30
	xvmulsp    x59,x59,x15
	vadduwm    v12,v6,v12
	xvcmpgtsp  x22,x48,x7
	lxvd2x     x15,SP,r14
	xxsel      x20,x9,x44,x20
	xvsubsp    x19,x19,x3
	xxlor      x44,x0,x0
	xxsel      x48,x9,x57,x58
	addi       r8,r4,160
	xvcmpgtsp  x60,x8,x41
	xxlxor     x16,x16,x33
	xxland     x52,x2,x43
	xvcmpgtsp  x58,x6,x62
	vslw       v15,v9,v2
	addi       r15,r0,96
	lxvd2x     x62,SP,r15
	xvmsubasp  x19,x29,x1
	xvmaddmsp  x63,x23,x31
	xvmaddasp  x44,x27,x1
	xvmulsp    x57,x14,x62
	xxland     x62,x11,x17
	xvmaddmsp  x25,x18,x9
	addi       r15,r0,112
	lxvd2x     x14,SP,r15
	xvsubsp    x21,x21,x15
	xxland     x15,x9,x20
	xxland     x20,x9,x48
	xxlxor     x18,x33,x61
	stxvd2x    x49,r7,r29
	xxland     x61,x11,x54
	xxland     x17,x9,x14
	vperm      v17,v4,v5,v13
	stxvd2x    x55,r7,r27
	xvsubsp    x14,x52,x3
	xxsel      x54,x47,x10,x60
	bc         BO_dCTR_ZERO,CR0_LT,__Lb54
	stw        r13,404(SP)
	addi       r15,r0,160
	addi       r14,r0,176
	addi       r13,r0,192
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
__L600:                                 # 0x00000600 (H.10.NO_SYMBOL+0x600)
	xxsel      x55,x59,x11,x50
	xvmaddasp  x19,x29,x5
	lxvd2x     x29,r0,r8
	xxlor      x59,x12,x12
	vperm      v20,v7,v8,v14
	stxvd2x    x52,SP,r15
	xxland     x50,x11,x54
	vadduwm    v16,v6,v15
	xvmaddasp  x59,x21,x13
	xvmaddmsp  x63,x23,x30
	xxland     x47,x51,x55
	xxlxor     x58,x33,x58
	vperm      v14,v11,v11,v3
	xvmulsp    x51,x25,x61
	xvmulsp    x17,x57,x17
	xxsel      x46,x4,x46,x32
	stxvd2x    x47,r7,r25
	xxlor      x47,x0,x0
	xvmaddasp  x47,x29,x1
	xxsel      x60,x9,x48,x60
	vperm      v22,v4,v5,v14
	xvmsubasp  x14,x28,x1
	xvcmpgtsp  x61,x8,x42
	vslw       v16,v10,v2
	xvsubsp    x25,x19,x49
	xvcmpgtsp  x55,x41,x7
	xvmaddmsp  x59,x21,x31
	xxland     x49,x9,x60
	xvmaddmsp  x23,x63,x9
	xvcmpgtsp  x41,x6,x41
	xxland     x19,x2,x44
	xvmulsp    x63,x51,x53
	xxsel      x51,x48,x10,x61
	xvsubsp    x19,x19,x3
	vperm      v21,v7,v8,v14
	xxsel      x24,x17,x11,x24
	xvmaddasp  x14,x28,x5
	lxvd2x     x28,r8,r28
	xxlor      x17,x12,x12
	vperm      v20,v7,v8,v13
	xvmaddasp  x17,x25,x13
	vadduwm    v16,v6,v16
	xxland     x51,x11,x51
	xvmaddmsp  x59,x21,x30
	xxland     x45,x16,x24
	xxlxor     x60,x33,x41
	vperm      v9,v12,v12,v3
	xvmulsp    x16,x23,x62
	xvmulsp    x57,x63,x15
	stxvd2x    x45,r7,r26
	xxsel      x45,x4,x41,x32
	xxlor      x41,x0,x0
	xvmaddasp  x41,x28,x1
	xxsel      x23,x9,x48,x61
	vperm      v31,v4,v5,v13
	xvmsubasp  x19,x27,x1
	xvcmpgtsp  x62,x8,x43
	vslw       v16,v11,v2
	xvsubsp    x24,x14,x54
	xvcmpgtsp  x54,x42,x7
	xxland     x23,x9,x23
	xvmaddmsp  x17,x25,x31
	xvmaddmsp  x21,x59,x9
	xvcmpgtsp  x42,x6,x42
	xxland     x15,x2,x47
	xvmulsp    x59,x16,x56
	xvsubsp    x15,x15,x3
	xxsel      x16,x48,x10,x62
	vadduwm    v16,v6,v16
	xxsel      x22,x57,x11,x22
	xvmaddasp  x19,x27,x5
	lxvd2x     x27,r8,r19
	xxland     x16,x11,x16
	xxlor      x14,x12,x12
	xvmaddasp  x14,x24,x13
	xxland     x46,x18,x22
	xvmaddmsp  x17,x25,x30
	xxlxor     x61,x33,x42
	vperm      v10,v15,v15,v3
	xvmulsp    x50,x21,x50
	xvmulsp    x18,x59,x20
	stxvd2x    x46,r7,r22
	xxsel      x46,x4,x42,x32
	xxlor      x42,x0,x0
	xvmaddasp  x42,x27,x1
	xxsel      x21,x9,x48,x62
	xvmsubasp  x15,x29,x1
	vperm      v30,v4,v5,v14
	xvcmpgtsp  x20,x8,x44
	xvsubsp    x22,x19,x63
	vslw       v16,v12,v2
	xxland     x19,x9,x21
	xvcmpgtsp  x59,x43,x7
	xvmaddmsp  x14,x24,x31
	xvmaddmsp  x25,x17,x9
	xxland     x21,x2,x41
	xvcmpgtsp  x43,x6,x43
	lxvd2x     x56,SP,r15
	xvmulsp    x17,x50,x56
	xxsel      x63,x48,x10,x20
	xvsubsp    x21,x21,x3
	xxsel      x50,x18,x11,x26
	xvmaddasp  x15,x29,x5
	lxvd2x     x26,r8,r16
	xxland     x18,x11,x63
	vadduwm    v16,v6,v16
	vperm      v24,v7,v8,v13
	xxlor      x63,x12,x12
	xvmaddasp  x63,x22,x13
	stxvd2x    x56,SP,r14
	xxland     x45,x50,x58
	xvmaddmsp  x14,x24,x30
	xxlxor     x50,x33,x43
	vperm      v11,v9,v9,v3
	xvmulsp    x51,x25,x51
	xvmulsp    x29,x17,x49
	stxvd2x    x45,r7,r24
	xxsel      x45,x4,x43,x32
	xxlor      x43,x0,x0
	xvmaddasp  x43,x26,x1
	xxsel      x49,x9,x48,x20
	vperm      v25,v4,v5,v13
	xvmsubasp  x21,x28,x1
	xvsubsp    x20,x15,x62
	vslw       v16,v15,v2
	xvcmpgtsp  x25,x8,x47
	xvcmpgtsp  x58,x44,x7
	xxland     x49,x9,x49
	xvmaddmsp  x63,x22,x31
	xvmaddmsp  x24,x14,x9
	xvcmpgtsp  x44,x6,x44
	xxland     x17,x2,x42
	xvmulsp    x16,x24,x16
	xvmulsp    x14,x51,x52
	xxsel      x15,x48,x10,x25
	xvsubsp    x17,x17,x3
	xxsel      x51,x29,x11,x55
	xvmaddasp  x21,x28,x5
	lxvd2x     x29,r8,r9
	vadduwm    v16,v6,v16
	xxlor      x62,x12,x12
	xxland     x15,x11,x15
	vperm      v20,v7,v8,v14
	xvmaddasp  x62,x20,x13
	xxland     x46,x51,x60
	xvmaddmsp  x63,x22,x30
	xxlxor     x51,x33,x44
	vperm      v12,v10,v10,v3
	xvmulsp    x28,x14,x23
	stxvd2x    x46,r7,r21
	xxsel      x46,x4,x44,x32
	xxlor      x44,x0,x0
	xvmaddasp  x44,x29,x1
	xxsel      x24,x9,x48,x25
	vperm      v24,v4,v5,v14
	xvmsubasp  x17,x27,x1
	xvsubsp    x25,x21,x57
	vslw       v16,v9,v2
	xvcmpgtsp  x23,x8,x41
	xvcmpgtsp  x57,x47,x7
	xxland     x24,x9,x24
	xvmaddmsp  x62,x20,x31
	addi       r7,r7,160
	xvmaddmsp  x22,x63,x9
	xvcmpgtsp  x60,x6,x47
	xxland     x47,x2,x43
	xvmulsp    x16,x16,x53
	xvsubsp    x14,x47,x3
	xxsel      x21,x48,x10,x23
	xxsel      x55,x28,x11,x54
	xvmaddasp  x17,x27,x5
	lxvd2x     x28,r8,r10
	vadduwm    v15,v6,v16
	xxland     x21,x11,x21
	vperm      v21,v7,v8,v13
	xxlor      x63,x12,x12
	xvmaddasp  x63,x25,x13
	xxland     x48,x55,x61
	xvmaddmsp  x62,x20,x30
	xxlxor     x60,x33,x60
	vperm      v13,v11,v11,v3
	xvmulsp    x27,x22,x18
	xvmulsp    x18,x16,x19
	xxsel      x45,x4,x45,x32
	stxvd2x    x48,r7,r23
	xxlor      x48,x0,x0
	xvmaddasp  x48,x28,x1
	xxsel      x22,x9,x47,x23
	vperm      v23,v4,v5,v13
	xvmsubasp  x14,x26,x1
	xvcmpgtsp  x19,x8,x42
	vslw       v15,v10,v2
	xvsubsp    x23,x17,x56
	xvcmpgtsp  x56,x41,x7
	xvmaddmsp  x63,x25,x31
	xxland     x22,x9,x22
	xvmaddmsp  x20,x62,x9
	xvcmpgtsp  x41,x6,x41
	xxland     x17,x2,x44
	lxvd2x     x54,SP,r14
	xvmulsp    x62,x27,x54
	xxsel      x16,x47,x10,x19
	xvsubsp    x17,x17,x3
	xxsel      x61,x18,x11,x59
	xvmaddasp  x14,x26,x5
	lxvd2x     x27,r8,r31
	vperm      v27,v7,v8,v14
	xxland     x18,x11,x16
	stxvd2x    x59,SP,r13
	vadduwm    v15,v6,v15
	xxlor      x16,x12,x12
	xvmaddasp  x16,x23,x13
	xxland     x46,x50,x61
	xvmaddmsp  x63,x25,x30
	xxlxor     x59,x33,x41
	vperm      v9,v12,v12,v3
	xvmulsp    x20,x20,x15
	xvmulsp    x54,x62,x49
	stxvd2x    x46,r7,r20
	xxsel      x46,x4,x41,x32
	xxlor      x41,x0,x0
	xvmaddasp  x41,x27,x1
	xxsel      x49,x9,x47,x19
	vperm      v29,v4,v5,v14
	xvmsubasp  x17,x29,x1
	xvcmpgtsp  x62,x8,x43
	xvsubsp    x26,x14,x55
	vslw       v15,v11,v2
	xvcmpgtsp  x50,x42,x7
	xxland     x49,x9,x49
	xvmaddmsp  x16,x23,x31
	xvmaddmsp  x25,x63,x9
	xvcmpgtsp  x42,x6,x42
	xxland     x19,x2,x48
	xvmulsp    x14,x20,x52
	xvsubsp    x15,x19,x3
	xxsel      x20,x47,x10,x62
	xxsel      x63,x54,x11,x58
	xvmaddasp  x17,x29,x5
	lxvd2x     x29,r8,r11
	vadduwm    v15,v6,v15
	xxlor      x19,x12,x12
	xxland     x20,x11,x20
	vperm      v22,v7,v8,v13
	xvmaddasp  x19,x26,x13
	xxland     x45,x51,x63
	xvmaddmsp  x16,x23,x30
	xxlxor     x51,x33,x42
	vperm      v10,v16,v16,v3
	xvmulsp    x21,x25,x21
	xvmulsp    x14,x14,x24
	stxvd2x    x45,r7,r18
	xxsel      x45,x4,x42,x32
	xxlor      x42,x0,x0
	xvmaddasp  x42,x29,x1
	xxsel      x58,x9,x47,x62
	xvmsubasp  x15,x28,x1
	vperm      v30,v4,v5,v13
	xvcmpgtsp  x63,x8,x44
	xvsubsp    x25,x17,x61
	vslw       v15,v12,v2
	xxland     x17,x9,x58
	xvmaddmsp  x19,x26,x31
	xvcmpgtsp  x24,x43,x7
	xvmaddmsp  x23,x16,x9
	xvcmpgtsp  x43,x6,x43
	xxland     x16,x2,x41
	xvmulsp    x58,x21,x53
	xvsubsp    x21,x16,x3
	xxsel      x61,x47,x10,x63
	xxsel      x16,x14,x11,x57
	xvmaddasp  x15,x28,x5
	lxvd2x     x28,r8,r12
	ori        r0,r0,0x0000
	xxlor      x14,x12,x12
	vperm      v21,v7,v8,v14
	vadduwm    v15,v6,v15
	xxland     x61,x11,x61
	xvmaddasp  x14,x25,x13
	xvmaddmsp  x19,x26,x30
	xxland     x46,x16,x60
	xxlxor     x16,x33,x43
	vperm      v11,v9,v9,v3
	xvmaddmsp  x14,x25,x31
	xvmulsp    x57,x23,x18
	xvmulsp    x18,x58,x22
	xvcmpgtsp  x22,x44,x7
	stxvd2x    x46,r7,r29
	xxsel      x46,x4,x43,x32
	xxlor      x43,x0,x0
	xvmaddasp  x43,x28,x1
	xxsel      x63,x9,x47,x63
	vperm      v26,v4,v5,v14
	xvmsubasp  x21,x27,x1
	xvcmpgtsp  x60,x8,x48
	xvsubsp    x23,x15,x62
	vslw       v15,v16,v2
	xxland     x15,x9,x63
	xvmaddmsp  x26,x19,x9
	xvcmpgtsp  x44,x6,x44
	xxland     x19,x2,x42
	lxvd2x     x52,SP,r13
	xvmulsp    x55,x57,x52
	xxsel      x63,x47,x10,x60
	xvsubsp    x19,x19,x3
	xxsel      x18,x18,x11,x56
	xvmaddasp  x21,x27,x5
	lxvd2x     x27,r8,r30
	vadduwm    v15,v6,v15
	xxland     x62,x11,x63
	vperm      v24,v7,v8,v13
	xxlor      x63,x12,x12
	xvmaddasp  x63,x23,x13
	xxland     x45,x18,x59
	xvmaddmsp  x14,x25,x30
	xxlxor     x18,x33,x44
	vperm      v12,v10,v10,v3
	xvmulsp    x57,x26,x20
	xvmulsp    x59,x55,x49
	stxvd2x    x45,r7,r27
	xxsel      x45,x4,x44,x32
	xxlor      x44,x0,x0
	xvmaddasp  x44,x27,x1
	xxsel      x20,x9,x47,x60
	vperm      v17,v4,v5,v13
	xvmsubasp  x19,x29,x1
	xvcmpgtsp  x60,x8,x41
	vslw       v15,v9,v2
	xvsubsp    x21,x21,x58
	xvcmpgtsp  x26,x48,x7
	xxland     x20,x9,x20
	xvmaddmsp  x63,x23,x31
	addi       r8,r8,160
	xvmaddmsp  x25,x14,x9
	xvcmpgtsp  x58,x6,x48
	xxland     x48,x2,x43
	xvmulsp    x57,x57,x54
	xxsel      x54,x47,x10,x60
	xvsubsp    x14,x48,x3
	bc         BO_dCTR_NZERO,CR0_LT,__L600
	lwz        r13,404(SP)
__Lb54:                                 # 0x00000b54 (H.10.NO_SYMBOL+0xb54)
	xxsel      x48,x59,x11,x50
	xvmaddasp  x19,x29,x5
	xvmulsp    x50,x25,x61
	xvmulsp    x29,x57,x17
	xvmulsp    x25,x50,x53
	vperm      v29,v7,v8,v14
	xvmaddmsp  x63,x23,x30
	xvmaddmsp  x23,x63,x9
	xxsel      x46,x29,x11,x24
	vperm      v18,v7,v8,v13
	xvsubsp    x0,x19,x49
	xvmulsp    x45,x25,x15
	vslw       v17,v11,v2
	xxland     x29,x2,x44
	xxland     x2,x16,x46
	xxland     x46,x48,x51
	stxvd2x    x46,r7,r25
	vadduwm    v16,v6,v17
	vadduwm    v14,v6,v15
	ori        r0,r0,0x0000
	xvmulsp    x47,x23,x62
	xxsel      x45,x45,x11,x22
	xvsubsp    x3,x29,x3
	xvmulsp    x47,x47,x56
	xxsel      x24,x9,x46,x60
	xxland     x51,x18,x45
	xvmsubasp  x14,x28,x1
	xvmulsp    x25,x47,x20
	vperm      v13,v11,v11,v3
	xxsel      x45,x4,x45,x32
	xxland     x29,x11,x54
	xvmsubasp  x3,x27,x1
	vslw       v14,v12,v2
	xvmaddasp  x14,x28,x5
	xxlxor     x28,x33,x58
	vperm      v3,v12,v12,v3
	stxvd2x    x2,r7,r26
	vslw       v2,v10,v2
	xvmaddasp  x3,x27,x5
	xxsel      x32,x4,x35,x32
	vperm      v3,v4,v5,v0
	xxlor      x27,x12,x12
	xxsel      x4,x25,x11,x26
	xxland     x4,x4,x28
	vperm      v15,v4,v5,v13
	vperm      v0,v7,v8,v0
	xxlor      x28,x12,x12
	xvcmpgtsp  x5,x41,x7
	xvmaddasp  x28,x21,x13
	xvsubsp    x2,x3,x35
	xvmaddasp  x27,x0,x13
	vadduwm    v4,v2,v6
	stxvd2x    x4,r7,r24
	xvmaddmsp  x28,x21,x31
	xvmaddmsp  x27,x0,x31
	xvmaddmsp  x28,x21,x30
	xvmaddmsp  x27,x0,x30
	vadduwm    v3,v6,v14
	xvcmpgtsp  x4,x44,x7
	xvmaddmsp  x0,x27,x9
	xvmaddmsp  x21,x28,x9
	xvsubsp    x3,x14,x47
	xvcmpgtsp  x37,x6,x44
	stxvd2x    x51,r7,r22
	xxland     x1,x9,x24
	xvcmpgtsp  x38,x6,x41
	xxlxor     x37,x33,x37
	xvcmpgtsp  x41,x8,x44
	xxlxor     x38,x33,x38
	xvcmpgtsp  x44,x8,x42
	xvcmpgtsp  x8,x8,x43
	xxsel      x35,x9,x35,x41
	xxsel      x47,x34,x10,x44
	xxsel      x36,x9,x36,x44
	vperm      v2,v7,v8,v13
	xvmulsp    x39,x21,x29
	ori        r0,r0,0x0000
	xxlor      x29,x12,x12
	xxsel      x44,x49,x10,x8
	xvmulsp    x39,x39,x61
	xxsel      x10,x46,x10,x41
	xvcmpgtsp  x49,x42,x7
	xvmaddasp  x29,x3,x13
	xvcmpgtsp  x42,x6,x42
	xvmaddasp  x12,x2,x13
	xvmulsp    x1,x39,x1
	xvcmpgtsp  x6,x6,x43
	xxland     x35,x9,x35
	xxlxor     x40,x33,x42
	xxsel      x8,x9,x48,x8
	xvmaddmsp  x29,x3,x31
	xxland     x42,x11,x47
	xvmaddmsp  x29,x3,x30
	xxlxor     x6,x6,x33
	xxsel      x1,x1,x11,x5
	xxland     x8,x8,x9
	xvcmpgtsp  x7,x43,x7
	xvmulsp    x0,x0,x42
	xxland     x33,x9,x36
	xxland     x36,x11,x44
	xxland     x10,x10,x11
	xvmaddasp  x31,x2,x12
	xvmaddmsp  x3,x29,x9
	xxland     x1,x1,x38
	xvmaddasp  x30,x2,x31
	xvmulsp    x0,x0,x50
	addi       r10,r6,10
	xvmulsp    x0,x0,x33
	stxvd2x    x1,r7,r21
	xvmaddasp  x9,x2,x30
	xxsel      x0,x0,x11,x49
	lwz        r14,408(SP)
	xvmulsp    x1,x3,x36
	lwz        r15,412(SP)
	xvmulsp    x2,x9,x10
	lwz        r16,416(SP)
	xxland     x0,x0,x40
	addi       r6,r0,-48
	stxvd2x    x0,r7,r6
	lwz        r20,432(SP)
	lwz        r18,424(SP)
	lwz        r22,440(SP)
	lwz        r19,428(SP)
	addi       r6,r0,208
	lvx        v20,SP,r6
	lwz        r24,448(SP)
	xvmulsp    x1,x1,x34
	lwz        r25,452(SP)
	xvmulsp    x2,x2,x32
	addi       r6,r0,224
	lvx        v21,SP,r6
	lwz        r26,456(SP)
	xvmulsp    x2,x2,x35
	lwz        r27,460(SP)
	xvmulsp    x1,x1,x8
	xxsel      x1,x1,x11,x7
	xxsel      x2,x2,x11,x4
	lwz        r28,464(SP)
	lwz        r23,444(SP)
	addi       r6,r0,240
	lvx        v22,SP,r6
	xxland     x0,x1,x6
	lwz        r31,476(SP)
	lwz        r30,472(SP)
	xxland     x1,x2,x37
	addi       r6,r0,256
	lvx        v23,SP,r6
	lfd        fp14,480(SP)
	lfd        fp16,496(SP)
	lfd        fp15,488(SP)
	lwz        r29,468(SP)
	addi       r6,r0,272
	lvx        v24,SP,r6
	lfd        fp18,512(SP)
	lfd        fp20,528(SP)
	lfd        fp19,520(SP)
	lfd        fp17,504(SP)
	addi       r6,r0,288
	lvx        v25,SP,r6
	lfd        fp22,544(SP)
	lfd        fp24,560(SP)
	lfd        fp23,552(SP)
	lfd        fp21,536(SP)
	addi       r6,r0,304
	lvx        v26,SP,r6
	lfd        fp26,576(SP)
	lfd        fp28,592(SP)
	lfd        fp27,584(SP)
	lfd        fp25,568(SP)
	addi       r6,r0,320
	lvx        v27,SP,r6
	lfd        fp30,608(SP)
	lwz        r21,436(SP)
	lfd        fp31,616(SP)
	lfd        fp29,600(SP)
	addi       r6,r0,336
	lvx        v28,SP,r6
	addi       r6,r0,352
	lvx        v29,SP,r6
	addi       r6,r0,368
	lvx        v30,SP,r6
	addi       r8,r0,-32
	stxvd2x    x0,r7,r8
	addi       r6,r0,384
	lvx        v31,SP,r6
	stxvd2x    x1,r7,r17
	lwz        r17,420(SP)
__Le40:                                 # 0x00000e40 (H.10.NO_SYMBOL+0xe40)
	cmp        0,0,r5,r10
	bc         BO_IF_NOT,CR0_GT,__L1818
	stfd       fp31,616(SP)
	stfd       fp30,608(SP)
	stfd       fp29,600(SP)
	stfd       fp20,528(SP)
	lwz        r6,T.18.NO_SYMBOL(RTOC)
	stw        r31,476(SP)
	stw        r30,472(SP)
	stw        r29,468(SP)
	stw        r28,464(SP)
	stw        r27,460(SP)
	subf       r7,r10,r5
	rlwinm     r8,r10,4,0,27
	addi       r27,r0,424
	lxvdsx     x5,r6,r27
	vspltisb   v0,7
	mtspr      CTR,r7
	add        r7,r4,r8
	addi       r9,r0,16
	addi       r27,r0,432
	lxvdsx     x6,r6,r27
	vspltisb   v2,-1
	addi       r31,r0,-64
	lxvd2x     x3,r0,r7
	vspltisw   v1,12
	addi       r27,r0,440
	lxvdsx     x36,r6,r27
	addi       r10,r0,32
	stw        r26,456(SP)
	addi       r28,r0,-48
	addi       r26,r0,-16
	addi       r27,r0,448
	lxvdsx     x7,r6,r27
	add        r8,r3,r8
	addi       r27,r0,456
	lxvdsx     x8,r6,r27
	addi       r27,r0,464
	lxvdsx     x9,r6,r27
	addi       r27,r0,480
	lxvd2x     x37,r6,r27
	addi       r27,r0,496
	lxvdsx     x10,r6,r27
	addi       r11,r0,400
	lxvdsx     x20,r6,r11
	xxlor      x2,x20,x20
	addi       r11,r0,48
	addi       r27,r0,512
	lxvd2x     x38,r6,r27
	addi       r27,r0,528
	lxvd2x     x39,r6,r27
	addi       r12,r0,408
	lxvdsx     x0,r6,r12
	addi       r12,r0,80
	xvmaddasp  x2,x3,x0
	addi       r27,r0,544
	lxvd2x     x40,r6,r27
	xxpermdi   x3,x3,x3,1
	xxpermdi   x35,x2,x2,1
	addi       r27,r0,560
	lxvd2x     x41,r6,r27
	addi       r27,r0,584
	lxvdsx     x11,r6,r27
	addi       r27,r0,592
	lxvdsx     x12,r6,r27
	addi       r30,r0,576
	lxvdsx     x1,r6,r30
	xxland     x42,x1,x35
	addi       r30,r0,-32
	addi       r27,r0,600
	lxvdsx     x13,r6,r27
	addi       r27,r0,608
	lxvdsx     x31,r6,r27
	addi       r27,r0,616
	lxvdsx     x30,r6,r27
	addi       r29,r0,416
	lxvdsx     x4,r6,r29
	addi       r29,r0,64
	addi       r27,r0,624
	lxvdsx     x29,r6,r27
	bc         BO_dCTR_ZERO,CR0_LT,__L1734
	xxlor      x2,x20,x20
	stfd       fp28,592(SP)
	xxpermdi   x28,x3,x3,1
	stfd       fp27,584(SP)
	lxvd2x     x3,r7,r9
	stfd       fp26,576(SP)
	xvmaddasp  x2,x3,x0
	xvsubsp    x26,x42,x11
	xxpermdi   x3,x3,x3,1
	bc         BO_dCTR_ZERO,CR0_LT,__L167c
	vperm      v10,v3,v3,v5
	stfd       fp25,568(SP)
	stfd       fp24,560(SP)
	xxlor      x27,x26,x26
	lxvd2x     x25,r7,r10
	xvmsubasp  x27,x28,x0
	xxpermdi   x50,x35,x35,1
	xxpermdi   x26,x28,x28,1
	xxpermdi   x28,x3,x3,1
	xxpermdi   x35,x2,x2,1
	xxsel      x43,x10,x42,x32
	xxlor      x2,x20,x20
	xvmaddasp  x27,x26,x12
	xxland     x45,x1,x35
	xvmaddasp  x2,x25,x0
	xxpermdi   x3,x25,x25,1
	vperm      v12,v8,v9,v11
	xvsubsp    x26,x45,x11
	bc         BO_dCTR_ZERO,CR0_LT,__L15dc
	xvsubsp    x25,x27,x44
	stfd       fp23,552(SP)
	xxlor      x23,x26,x26
	vperm      v10,v3,v3,v5
	xxpermdi   x51,x50,x50,1
	xvmsubasp  x23,x28,x0
	lxvd2x     x24,r7,r11
	xxpermdi   x50,x35,x35,1
	xxpermdi   x27,x28,x28,1
	xxpermdi   x35,x2,x2,1
	xxlor      x2,x20,x20
	xxland     x44,x1,x35
	xxpermdi   x28,x3,x3,1
	xxpermdi   x3,x24,x24,1
	xvmaddmsp  x27,x12,x23
	xvmaddasp  x2,x24,x0
	xvsubsp    x26,x44,x11
	bc         BO_dCTR_ZERO,CR0_LT,__L1550
	stfd       fp22,544(SP)
	stfd       fp21,536(SP)
	stfd       fp19,520(SP)
	xxlor      x19,x13,x13
	xxlor      x21,x26,x26
	xxpermdi   x44,x43,x43,1
	xvmaddasp  x19,x25,x31
	xxsel      x43,x10,x42,x32
	xxpermdi   x24,x25,x25,1
	ori        r0,r0,0x0000
	xxpermdi   x23,x51,x51,1
	vperm      v10,v8,v9,v11
	lxvd2x     x22,r7,r29
	xxpermdi   x51,x50,x50,1
	xvmsubasp  x21,x28,x0
	addi       r8,r8,16
	xvmaddmsp  x19,x24,x30
	xvsubsp    x25,x27,x42
	vperm      v10,v3,v3,v5
	xxpermdi   x27,x28,x28,1
	xxpermdi   x50,x35,x35,1
	xxpermdi   x28,x3,x3,1
	xxpermdi   x35,x2,x2,1
	xxpermdi   x3,x22,x22,1
	xxlor      x2,x20,x20
	xvmaddasp  x2,x22,x0
	xxland     x45,x1,x35
	xxpermdi   x22,x44,x44,1
	xvmaddmsp  x27,x12,x21
	xvsubsp    x26,x45,x11
	bc         BO_dCTR_ZERO,CR0_LT,__L14dc
	xxpermdi   x46,x43,x43,1
	stfd       fp18,512(SP)
	xxpermdi   x44,x23,x23,1
	stfd       fp17,504(SP)
	xxlor      x23,x29,x29
	stfd       fp16,496(SP)
	xxsel      x43,x10,x42,x32
	xxlor      x16,x13,x13
	xxpermdi   x21,x24,x24,1
	xvmaddasp  x23,x24,x19
	xvmaddasp  x16,x25,x31
	xxlor      x19,x30,x30
	vperm      v10,v8,v9,v11
	xxpermdi   x24,x25,x25,1
	xvsubsp    x25,x27,x42
	lxvd2x     x17,r7,r12
	xxpermdi   x27,x28,x28,1
	xvmaddmsp  x21,x23,x7
	vperm      v10,v3,v3,v5
	xxpermdi   x23,x51,x51,1
	addi       r6,r7,16
	vslw       v17,v12,v1
	addi       r8,r8,16
	xvcmpgtsp  x18,x6,x44
	xxpermdi   x51,x50,x50,1
	xxpermdi   x45,x22,x22,1
	xvmsubasp  x26,x28,x0
	xvmaddasp  x19,x24,x16
	xxpermdi   x50,x35,x35,1
	xxsel      x16,x49,x8,x18
	xxpermdi   x35,x2,x2,1
	xxlor      x2,x20,x20
	xvmaddmsp  x27,x12,x26
	xxpermdi   x28,x3,x3,1
	xvmaddasp  x2,x17,x0
	xxland     x47,x1,x35
	xxpermdi   x22,x46,x46,1
	xxpermdi   x3,x17,x17,1
	xvsubsp    x26,x47,x11
	bc         BO_dCTR_ZERO,CR0_LT,__L1488
	addi       r9,r0,384
	stvx       v31,SP,r9
	addi       r9,r0,368
	stvx       v30,SP,r9
	addi       r9,r0,352
	stvx       v29,SP,r9
	stfd       fp15,488(SP)
	xxlor      x15,x26,x26
	xxland     x47,x9,x16
	xxlor      x16,x13,x13
	stfd       fp14,480(SP)
	xvmaddasp  x16,x25,x31
	xvmsubasp  x15,x28,x0
	xxpermdi   x17,x44,x44,1
	xxpermdi   x61,x43,x43,1
	xxpermdi   x44,x23,x23,1
	xxpermdi   x23,x51,x51,1
	xvmaddmsp  x19,x24,x29
	xvmulsp    x14,x21,x47
	xxsel      x43,x10,x42,x32
	xxpermdi   x51,x50,x50,1
	xxpermdi   x21,x24,x24,1
	vperm      v15,v6,v7,v13
	vperm      v14,v8,v9,v11
	lxvd2x     x63,r6,r12
	vperm      v10,v3,v3,v5
	xxpermdi   x50,x35,x35,1
	xxpermdi   x35,x2,x2,1
	xxpermdi   x62,x18,x18,1
	xxlor      x2,x20,x20
	xxpermdi   x48,x49,x49,1
	xvmaddmsp  x21,x19,x7
	xxpermdi   x24,x25,x25,1
	vslw       v17,v12,v1
	xvcmpgtsp  x18,x6,x44
	xxlor      x19,x30,x30
	xvsubsp    x25,x27,x46
	xxland     x26,x1,x35
	xvmaddasp  x2,x63,x0
	xxpermdi   x27,x28,x28,1
	vadduwm    v14,v4,v16
	xvmaddasp  x19,x24,x16
	xvmaddmsp  x27,x12,x15
	xvsubsp    x26,x26,x11
	addi       r8,r8,16
	xxsel      x16,x49,x8,x18
	xxpermdi   x17,x17,x17,1
	addi       r6,r7,32
	xxpermdi   x45,x22,x22,1
	xxpermdi   x28,x3,x3,1
	xxpermdi   x22,x61,x61,1
	xvmulsp    x14,x14,x47
	xxsel      x47,x7,x46,x62
	xxpermdi   x3,x63,x63,1
	bc         BO_dCTR_ZERO,CR0_LT,__L1448
	addi       r9,r0,336
	stvx       v28,SP,r9
	xxland     x61,x9,x16
	xxlor      x15,x29,x29
	xxland     x46,x7,x47
	xxlor      x16,x13,x13
	addi       r9,r0,320
	stvx       v27,SP,r9
	xvmaddasp  x16,x25,x31
	xvmaddasp  x15,x24,x19
	addi       r9,r0,304
	stvx       v26,SP,r9
	xxpermdi   x48,x43,x43,1
	xxlor      x62,x26,x26
	xxsel      x43,x10,x42,x32
	xvmsubasp  x62,x28,x0
	xxlor      x19,x30,x30
	xxpermdi   x26,x44,x44,1
	vperm      v10,v3,v3,v5
	xvmulsp    x14,x14,x46
	xxpermdi   x47,x49,x49,1
	lxvd2x     x63,r6,r12
	xxpermdi   x44,x23,x23,1
	addi       r6,r7,48
	xxpermdi   x23,x51,x51,1
	addi       r9,r0,288
	stvx       v25,SP,r9
	addi       r8,r8,16
	xxpermdi   x51,x50,x50,1
	xxpermdi   x50,x35,x35,1
	xxpermdi   x35,x2,x2,1
	vperm      v14,v6,v7,v13
	xxlor      x2,x20,x20
	xvmulsp    x61,x21,x61
	vperm      v13,v8,v9,v11
	xxpermdi   x57,x18,x18,1
	vslw       v17,v12,v1
	xxpermdi   x21,x24,x24,1
	xvcmpgtsp  x60,x4,x17
	xxpermdi   x24,x25,x25,1
	xvsubsp    x25,x27,x45
	xvcmpgtsp  x18,x6,x44
	xvcmpgtsp  x59,x17,x5
	xxpermdi   x27,x28,x28,1
	xxland     x58,x1,x35
	vadduwm    v13,v4,v15
	xvmaddasp  x2,x63,x0
	xvmaddasp  x19,x24,x16
	xvmaddmsp  x21,x15,x7
	xxsel      x47,x7,x45,x57
	xxpermdi   x45,x22,x22,1
	xxpermdi   x17,x26,x26,1
	xxpermdi   x28,x3,x3,1
	xxsel      x15,x14,x9,x59
	xxsel      x16,x49,x8,x18
	xxpermdi   x3,x63,x63,1
	xvsubsp    x26,x58,x11
	xxlxor     x63,x34,x60
	xvmulsp    x14,x61,x46
	xvmaddmsp  x27,x12,x62
	xxpermdi   x22,x48,x48,1
	bc         BO_dCTR_ZERO,CR0_LT,__L1420
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
	ori        r0,r0,0x0000
__L1340:                                # 0x00001340 (H.10.NO_SYMBOL+0x1340)
	xxland     x46,x9,x16
	xxlor      x16,x13,x13
	xxpermdi   x48,x43,x43,1
	xxsel      x43,x10,x42,x32
	lxvd2x     x61,r6,r12
	xvmaddasp  x16,x25,x31
	xxland     x42,x15,x63
	xxlor      x58,x29,x29
	xxland     x47,x47,x7
	xxpermdi   x63,x44,x44,1
	xvmaddasp  x58,x24,x19
	xxpermdi   x44,x23,x23,1
	addi       r6,r6,16
	xxpermdi   x23,x51,x51,1
	xxpermdi   x51,x50,x50,1
	xvmulsp    x60,x21,x46
	xvmulsp    x15,x14,x47
	xxpermdi   x50,x35,x35,1
	vperm      v14,v8,v9,v11
	stxvd2x    x42,r8,r31
	xxlor      x62,x26,x26
	vperm      v10,v3,v3,v5
	xvmsubasp  x62,x28,x0
	xxpermdi   x35,x2,x2,1
	xxpermdi   x21,x24,x24,1
	xxpermdi   x24,x25,x25,1
	xvsubsp    x25,x27,x46
	xxpermdi   x46,x49,x49,1
	xxlor      x2,x20,x20
	vslw       v17,v12,v1
	xxpermdi   x47,x18,x18,1
	xvcmpgtsp  x18,x6,x44
	xvmaddasp  x2,x61,x0
	vperm      v13,v6,v7,v13
	xxlor      x19,x30,x30
	xxpermdi   x27,x28,x28,1
	addi       r8,r8,16
	xvmaddasp  x19,x24,x16
	xxland     x26,x1,x35
	xvmaddmsp  x21,x58,x7
	xvcmpgtsp  x59,x4,x17
	xxpermdi   x28,x3,x3,1
	xxpermdi   x3,x61,x61,1
	xvmulsp    x14,x60,x45
	xvcmpgtsp  x61,x17,x5
	xxpermdi   x17,x63,x63,1
	vadduwm    v14,v4,v14
	xxsel      x16,x49,x8,x18
	xxpermdi   x45,x22,x22,1
	xvsubsp    x26,x26,x11
	xxpermdi   x22,x48,x48,1
	xxlxor     x63,x34,x59
	xxsel      x15,x15,x9,x61
	xxsel      x47,x7,x46,x47
	xvmaddmsp  x27,x12,x62
	bc         BO_dCTR_NZERO,CR0_LT,__L1340
__L1420:                                # 0x00001420 (H.10.NO_SYMBOL+0x1420)
	xxland     x46,x15,x63
	addi       r6,r0,288
	lvx        v25,SP,r6
	addi       r6,r0,304
	lvx        v26,SP,r6
	addi       r6,r0,320
	lvx        v27,SP,r6
	addi       r6,r0,336
	lvx        v28,SP,r6
	stxvd2x    x46,r8,r31
__L1448:                                # 0x00001448 (H.10.NO_SYMBOL+0x1448)
	xxland     x46,x7,x47
	xvcmpgtsp  x47,x4,x17
	xvcmpgtsp  x48,x17,x5
	xvmulsp    x46,x14,x46
	lfd        fp15,488(SP)
	xxlxor     x47,x47,x34
	addi       r6,r0,352
	lvx        v29,SP,r6
	lfd        fp14,480(SP)
	xxsel      x46,x46,x9,x48
	addi       r6,r0,368
	lvx        v30,SP,r6
	xxland     x46,x46,x47
	addi       r6,r0,384
	lvx        v31,SP,r6
	stxvd2x    x46,r8,r28
__L1488:                                # 0x00001488 (H.10.NO_SYMBOL+0x1488)
	xxland     x46,x9,x16
	xxpermdi   x47,x44,x44,1
	xxpermdi   x48,x18,x18,1
	xxpermdi   x44,x49,x49,1
	vperm      v13,v6,v7,v13
	vadduwm    v12,v4,v12
	xvmulsp    x46,x21,x46
	xxpermdi   x47,x47,x47,1
	lfd        fp16,496(SP)
	lfd        fp17,504(SP)
	lfd        fp18,512(SP)
	xxsel      x44,x7,x44,x48
	xvmulsp    x45,x46,x45
	xvcmpgtsp  x46,x4,x47
	xvcmpgtsp  x47,x47,x5
	xxland     x44,x44,x7
	xvmulsp    x44,x45,x44
	xxlxor     x45,x34,x46
	xxsel      x44,x44,x9,x47
	xxland     x44,x44,x45
	stxvd2x    x44,r8,r30
__L14dc:                                # 0x000014dc (H.10.NO_SYMBOL+0x14dc)
	xvmaddmsp  x19,x24,x29
	xxpermdi   x44,x23,x23,1
	xxpermdi   x24,x24,x24,1
	xxpermdi   x45,x22,x22,1
	xvcmpgtsp  x47,x6,x44
	xvmaddmsp  x24,x19,x7
	vslw       v14,v12,v1
	vperm      v13,v6,v7,v13
	xxpermdi   x48,x47,x47,1
	xxsel      x47,x46,x8,x47
	xxpermdi   x49,x44,x44,1
	xxpermdi   x44,x46,x46,1
	lfd        fp22,544(SP)
	lfd        fp21,536(SP)
	xxland     x46,x9,x47
	vadduwm    v12,v4,v12
	xxpermdi   x47,x49,x49,1
	lfd        fp19,520(SP)
	xvmulsp    x46,x24,x46
	xvmulsp    x45,x46,x45
	xxsel      x44,x7,x44,x48
	xvcmpgtsp  x46,x47,x5
	xvcmpgtsp  x48,x4,x47
	xxland     x44,x44,x7
	xxlxor     x47,x34,x48
	xvmulsp    x44,x45,x44
	xxsel      x44,x44,x9,x46
	xxland     x44,x44,x47
	stxvd2x    x44,r8,r26
__L1550:                                # 0x00001550 (H.10.NO_SYMBOL+0x1550)
	xxlor      x24,x13,x13
	xxpermdi   x43,x43,x43,1
	addi       r8,r8,16
	xxpermdi   x23,x25,x25,1
	xxpermdi   x44,x51,x51,1
	xxpermdi   x44,x44,x44,1
	vslw       v13,v12,v1
	xvmaddasp  x24,x25,x31
	xxpermdi   x25,x23,x23,1
	xvcmpgtsp  x46,x6,x44
	xxpermdi   x47,x44,x44,1
	xxpermdi   x43,x43,x43,1
	xvmaddmsp  x24,x23,x30
	xxpermdi   x48,x46,x46,1
	xxpermdi   x44,x45,x45,1
	xxpermdi   x47,x47,x47,1
	xxpermdi   x43,x43,x43,1
	xvmaddmsp  x24,x23,x29
	vadduwm    v12,v4,v12
	xxsel      x45,x45,x8,x46
	xvmaddmsp  x25,x24,x7
	vperm      v11,v6,v7,v11
	xvcmpgtsp  x46,x4,x47
	xxland     x45,x9,x45
	xvmulsp    x45,x25,x45
	xvcmpgtsp  x47,x47,x5
	xxlxor     x46,x46,x34
	xxsel      x44,x7,x44,x48
	lfd        fp23,552(SP)
	xxland     x44,x44,x7
	xvmulsp    x43,x45,x43
	xvmulsp    x43,x43,x44
	xxsel      x43,x43,x9,x47
	xxland     x43,x43,x46
	stxvd2x    x43,r8,r26
__L15dc:                                # 0x000015dc (H.10.NO_SYMBOL+0x15dc)
	xxsel      x42,x10,x42,x32
	xxpermdi   x44,x50,x50,1
	addi       r8,r8,16
	xxpermdi   x43,x42,x42,1
	xxpermdi   x44,x44,x44,1
	vperm      v10,v8,v9,v10
	xxpermdi   x43,x43,x43,1
	xvsubsp    x25,x27,x42
	xxlor      x27,x13,x13
	xxpermdi   x42,x44,x44,1
	xvmaddasp  x27,x25,x31
	xxpermdi   x45,x42,x42,1
	xxpermdi   x24,x25,x25,1
	xvcmpgtsp  x46,x6,x42
	vslw       v12,v10,v1
	xvmaddmsp  x27,x24,x30
	xxpermdi   x43,x43,x43,1
	xxpermdi   x25,x24,x24,1
	xxsel      x42,x44,x8,x46
	xxpermdi   x46,x46,x46,1
	xxland     x47,x9,x42
	xxpermdi   x42,x44,x44,1
	xvmaddmsp  x27,x24,x29
	vperm      v11,v6,v7,v11
	xvmaddmsp  x25,x27,x7
	vadduwm    v10,v4,v10
	xxpermdi   x44,x45,x45,1
	lfd        fp24,560(SP)
	xvmulsp    x45,x25,x47
	xvmulsp    x43,x45,x43
	xxsel      x42,x7,x42,x46
	xvcmpgtsp  x46,x4,x44
	xvcmpgtsp  x44,x44,x5
	lfd        fp25,568(SP)
	xxland     x42,x42,x7
	xxlxor     x46,x46,x34
	xvmulsp    x42,x43,x42
	xxsel      x42,x42,x9,x44
	xxland     x42,x42,x46
	stxvd2x    x42,r8,r26
__L167c:                                # 0x0000167c (H.10.NO_SYMBOL+0x167c)
	xxpermdi   x42,x35,x35,1
	vperm      v3,v3,v3,v5
	xvmsubasp  x26,x28,x0
	xxlor      x27,x13,x13
	addi       r8,r8,16
	xxsel      x35,x10,x35,x32
	xxpermdi   x43,x42,x42,1
	xxpermdi   x28,x28,x28,1
	xxpermdi   x44,x35,x35,1
	xvmaddasp  x26,x28,x12
	vperm      v10,v8,v9,v3
	xxpermdi   x35,x43,x43,1
	xxpermdi   x43,x44,x44,1
	xxpermdi   x35,x35,x35,1
	xvsubsp    x28,x26,x42
	xxpermdi   x43,x43,x43,1
	xvmaddasp  x27,x28,x31
	xxpermdi   x26,x28,x28,1
	xxpermdi   x28,x26,x26,1
	xvmaddmsp  x27,x26,x30
	xvcmpgtsp  x46,x6,x35
	vslw       v10,v3,v1
	xxpermdi   x44,x35,x35,1
	xvmaddmsp  x27,x26,x29
	xxpermdi   x45,x46,x46,1
	xxsel      x46,x42,x8,x46
	xxpermdi   x35,x42,x42,1
	vperm      v10,v6,v7,v11
	xvmaddmsp  x28,x27,x7
	xxland     x43,x9,x46
	vadduwm    v3,v3,v4
	xvmulsp    x43,x28,x43
	xxpermdi   x44,x44,x44,1
	lfd        fp26,576(SP)
	xxsel      x35,x7,x35,x45
	lfd        fp27,584(SP)
	xvmulsp    x42,x43,x42
	xvcmpgtsp  x43,x4,x44
	xvcmpgtsp  x44,x44,x5
	xxland     x35,x35,x7
	lfd        fp28,592(SP)
	xvmulsp    x35,x42,x35
	xxsel      x35,x35,x9,x44
	xxlxor     x42,x34,x43
	xxland     x35,x35,x42
	stxvd2x    x35,r8,r26
__L1734:                                # 0x00001734 (H.10.NO_SYMBOL+0x1734)
	xxpermdi   x35,x2,x2,1
	xxpermdi   x2,x3,x3,1
	ori        r10,r5,0x0000
	xxland     x3,x1,x35
	addi       r5,r8,16
	xxpermdi   x1,x2,x2,1
	xvsubsp    x3,x3,x11
	xxpermdi   x42,x35,x35,1
	lwz        r27,460(SP)
	vperm      v3,v3,v3,v5
	lwz        r28,464(SP)
	xvmsubasp  x3,x2,x0
	xxsel      x32,x10,x35,x32
	xxpermdi   x0,x42,x42,1
	lwz        r29,468(SP)
	xvmaddasp  x3,x1,x12
	vperm      v3,v8,v9,v0
	xxpermdi   x1,x32,x32,1
	xxpermdi   x0,x0,x0,1
	xxpermdi   x32,x0,x0,1
	xvsubsp    x2,x3,x35
	lwz        r30,472(SP)
	lwz        r31,476(SP)
	lfd        fp20,528(SP)
	xvcmpgtsp  x0,x6,x32
	xxpermdi   x6,x1,x1,1
	xvmaddasp  x13,x2,x31
	xxpermdi   x2,x2,x2,1
	vslw       v1,v0,v1
	xxpermdi   x1,x32,x32,1
	xvmaddasp  x30,x2,x13
	xxpermdi   x3,x2,x2,1
	xxsel      x8,x33,x8,x0
	xxpermdi   x32,x6,x6,1
	xxpermdi   x33,x33,x33,1
	xxland     x6,x8,x9
	xxpermdi   x0,x0,x0,1
	xvmaddasp  x29,x2,x30
	vperm      v0,v6,v7,v0
	xvmaddmsp  x3,x29,x7
	vadduwm    v1,v1,v4
	xxpermdi   x1,x1,x1,1
	lfd        fp29,600(SP)
	lfd        fp30,608(SP)
	xvmulsp    x2,x3,x6
	xxsel      x0,x7,x33,x0
	xvcmpgtsp  x3,x4,x1
	xvcmpgtsp  x1,x1,x5
	lfd        fp31,616(SP)
	xxland     x0,x0,x7
	xvmulsp    x2,x2,x32
	xxlxor     x3,x3,x34
	xvmulsp    x0,x2,x0
	xxsel      x0,x0,x9,x1
	xxland     x0,x0,x3
	stxvd2x    x0,r5,r26
	lwz        r26,456(SP)
__L1818:                                # 0x00001818 (H.10.NO_SYMBOL+0x1818)
	rlwinm     r10,r10,2,0,29
__L181c:                                # 0x0000181c (H.10.NO_SYMBOL+0x181c)
	cmp        0,0,r0,r10
	bc         BO_IF_NOT,CR0_GT,__L21e0
	stfd       fp31,616(SP)
	stfd       fp30,608(SP)
	vspltisb   v1,7
	subf       r0,r10,r0
	rlwinm     r6,r10,2,0,29
	lwz        r5,T.18.NO_SYMBOL(RTOC)
	addi       r7,r0,400
	lxvdsx     x1,r5,r7
	mtspr      CTR,r0
	vspltisb   v2,-1
	addi       r0,r6,-4
	add        r4,r4,r0
	vspltisw   v0,12
	add        r3,r3,r0
	stfd       fp29,600(SP)
	lfs        fp0,4(r4)
	stfd       fp28,592(SP)
	stfd       fp27,584(SP)
	addi       r0,r0,408
	lxvdsx     x2,r5,r0
	xscvdpspn  x0,x0
	xxspltw    x27,x0,0
	addi       r0,r0,416
	lxvdsx     x3,r5,r0
	addi       r0,r0,424
	lxvdsx     x4,r5,r0
	addi       r0,r0,432
	lxvdsx     x5,r5,r0
	addi       r0,r0,440
	lxvdsx     x35,r5,r0
	addi       r0,r0,448
	lxvdsx     x6,r5,r0
	addi       r0,r0,456
	lxvdsx     x7,r5,r0
	addi       r0,r0,464
	lxvdsx     x8,r5,r0
	addi       r0,r0,480
	lxvd2x     x36,r5,r0
	addi       r0,r0,496
	lxvdsx     x9,r5,r0
	addi       r0,r0,512
	lxvd2x     x37,r5,r0
	addi       r0,r0,528
	lxvd2x     x38,r5,r0
	addi       r0,r0,544
	lxvd2x     x39,r5,r0
	addi       r0,r0,560
	lxvd2x     x40,r5,r0
	addi       r0,r0,576
	lxvdsx     x10,r5,r0
	addi       r0,r0,584
	lxvdsx     x11,r5,r0
	addi       r0,r0,592
	lxvdsx     x12,r5,r0
	addi       r0,r0,600
	lxvdsx     x13,r5,r0
	addi       r0,r0,608
	lxvdsx     x31,r5,r0
	addi       r0,r0,616
	lxvdsx     x30,r5,r0
	addi       r0,r0,624
	lxvdsx     x29,r5,r0
	bc         BO_dCTR_ZERO,CR0_LT,__L2108
	stfd       fp26,576(SP)
	xxpermdi   x48,x27,x27,1
	lfs        fp0,8(r4)
	xxlor      x26,x1,x1
	xvmaddasp  x26,x27,x2
	xscvdpspn  x0,x0
	xxpermdi   x41,x26,x26,1
	xxspltw    x27,x0,0
	bc         BO_dCTR_ZERO,CR0_LT,__L2044
	xxlor      x26,x1,x1
	stfd       fp25,568(SP)
	xxland     x43,x10,x41
	lfs        fp0,12(r4)
	ori        r0,r0,0x0000
	xxpermdi   x28,x48,x48,1
	xvmaddasp  x26,x27,x2
	vperm      v10,v9,v9,v4
	xscvdpspn  x0,x0
	xvsubsp    x25,x43,x11
	xxpermdi   x48,x27,x27,1
	xxpermdi   x43,x41,x41,1
	xxspltw    x27,x0,0
	bc         BO_dCTR_ZERO,CR0_LT,__L1f90
	lfs        fp0,16(r4)
	stfd       fp24,560(SP)
	xxlor      x24,x25,x25
	stfd       fp23,552(SP)
	xxpermdi   x25,x28,x28,1
	xxpermdi   x41,x26,x26,1
	xvmsubasp  x24,x28,x2
	xxlor      x26,x1,x1
	xxland     x44,x10,x41
	xxsel      x42,x9,x42,x33
	xvmaddasp  x26,x27,x2
	xscvdpspn  x0,x0
	xxpermdi   x28,x48,x48,1
	xxpermdi   x48,x27,x27,1
	vperm      v14,v7,v8,v10
	xvmaddasp  x24,x25,x12
	xvsubsp    x25,x44,x11
	xxspltw    x27,x0,0
	xxpermdi   x49,x43,x43,1
	bc         BO_dCTR_ZERO,CR0_LT,__L1efc
	lfs        fp0,20(r4)
	xvmsubasp  x25,x28,x2
	vperm      v11,v9,v9,v4
	xxpermdi   x44,x41,x41,1
	xxpermdi   x41,x26,x26,1
	stfd       fp22,544(SP)
	xscvdpspn  x0,x0
	xvsubsp    x23,x24,x46
	xxpermdi   x24,x28,x28,1
	xxlor      x26,x1,x1
	xvmaddasp  x26,x27,x2
	xvmaddmsp  x24,x12,x25
	xxpermdi   x28,x48,x48,1
	xxpermdi   x48,x27,x27,1
	xxspltw    x27,x0,0
	xxpermdi   x50,x49,x49,1
	xxpermdi   x51,x42,x42,1
	xxland     x0,x10,x41
	xxlor      x22,x13,x13
	xxsel      x42,x9,x43,x33
	xxpermdi   x49,x44,x44,1
	xvmaddasp  x22,x23,x31
	xvsubsp    x25,x0,x11
	vperm      v14,v7,v8,v10
	bc         BO_dCTR_ZERO,CR0_LT,__L1e7c
	stfd       fp21,536(SP)
	stfd       fp20,528(SP)
	stfd       fp19,520(SP)
	xxlor      x0,x25,x25
	ori        r0,r0,0x0000
	xxlor      x19,x30,x30
	xvmsubasp  x0,x28,x2
	vperm      v12,v9,v9,v4
	xxpermdi   x45,x41,x41,1
	xxpermdi   x41,x26,x26,1
	xxlor      x26,x1,x1
	addi       r3,r3,4
	xxpermdi   x43,x50,x50,1
	xxpermdi   x21,x51,x51,1
	xxpermdi   x20,x23,x23,1
	xvsubsp    x23,x24,x46
	xvmaddasp  x26,x27,x2
	xxpermdi   x51,x42,x42,1
	xxpermdi   x24,x28,x28,1
	xvmaddasp  x19,x20,x22
	xxpermdi   x50,x49,x49,1
	addi       r5,r4,4
	xxlor      x22,x13,x13
	xvmaddmsp  x24,x12,x0
	xxsel      x42,x9,x44,x33
	xvmaddasp  x22,x23,x31
	xvmaddmsp  x19,x20,x29
	xxland     x0,x10,x41
	xxpermdi   x28,x48,x48,1
	xvsubsp    x25,x0,x11
	xxpermdi   x49,x45,x45,1
	xxpermdi   x48,x27,x27,1
	lfs        fp0,24(r4)
	vperm      v14,v7,v8,v10
	xscvdpspn  x0,x0
	xxspltw    x27,x0,0
	bc         BO_dCTR_ZERO,CR0_LT,__L1e0c
	vperm      v12,v9,v9,v4
	stfd       fp18,512(SP)
	stfd       fp17,504(SP)
	xvmsubasp  x25,x28,x2
	stfd       fp16,496(SP)
	xxpermdi   x47,x21,x21,1
	xxlor      x16,x30,x30
	xxpermdi   x0,x41,x41,1
	xxpermdi   x17,x20,x20,1
	xxpermdi   x20,x23,x23,1
	xvmaddmsp  x17,x19,x6
	xvmaddasp  x16,x20,x22
	xvsubsp    x23,x24,x46
	xxlor      x19,x29,x29
	xxlor      x22,x13,x13
	xxpermdi   x41,x26,x26,1
	addi       r4,r4,8
	xxpermdi   x24,x28,x28,1
	xxlor      x26,x1,x1
	xxpermdi   x21,x51,x51,1
	xxland     x45,x10,x41
	xxpermdi   x51,x42,x42,1
	xvmaddasp  x26,x27,x2
	xxsel      x42,x9,x44,x33
	addi       r3,r3,4
	xxpermdi   x28,x48,x48,1
	xxpermdi   x18,x43,x43,1
	xxpermdi   x48,x27,x27,1
	xvmaddmsp  x24,x12,x25
	xvmaddasp  x22,x23,x31
	xvsubsp    x25,x45,x11
	vperm      v14,v7,v8,v10
	xvmaddasp  x19,x20,x16
	xvcmpgtsp  x16,x5,x43
	vslw       v12,v11,v0
	xxpermdi   x43,x50,x50,1
	xxpermdi   x50,x49,x49,1
	xxpermdi   x49,x0,x0,1
	lfs        fp0,24(r5)
	xscvdpspn  x0,x0
	xxsel      x45,x44,x7,x16
	xxspltw    x27,x0,0
	xxland     x45,x8,x45
	xvmulsp    x17,x17,x45
	bc         BO_dCTR_ZERO,CR0_LT,__L1dc0
	addi       r0,r0,384
	stvx       v31,SP,r0
	lfs        fp0,24(r4)
	xxpermdi   x44,x44,x44,1
	vperm      v13,v5,v6,v15
	addi       r0,r0,368
	stvx       v30,SP,r0
	vadduwm    v31,v3,v12
	vperm      v30,v9,v9,v4
	xxpermdi   x47,x16,x16,1
	addi       r0,r0,352
	stvx       v29,SP,r0
	xvcmpgtsp  x16,x5,x43
	xxlor      x61,x25,x25
	xvmulsp    x45,x17,x45
	vslw       v12,v11,v0
	addi       r0,r0,336
	stvx       v28,SP,r0
	xscvdpspn  x0,x0
	xxpermdi   x60,x41,x41,1
	xxpermdi   x41,x26,x26,1
	xxlor      x26,x1,x1
	addi       r0,r0,320
	stvx       v27,SP,r0
	addi       r4,r4,4
	xxpermdi   x59,x20,x20,1
	addi       r3,r3,4
	xxpermdi   x20,x23,x23,1
	xvsubsp    x23,x24,x46
	xxland     x46,x10,x41
	xvmsubasp  x61,x28,x2
	xvmaddmsp  x59,x19,x6
	stfd       fp15,488(SP)
	stfd       fp14,480(SP)
	xvsubsp    x25,x46,x11
	xxsel      x47,x6,x63,x47
	xxpermdi   x24,x28,x28,1
	xxlor      x19,x30,x30
	xxsel      x46,x44,x7,x16
	xxpermdi   x14,x18,x18,1
	xvmaddmsp  x24,x12,x61
	xvmaddasp  x26,x27,x2
	xvmaddasp  x19,x20,x22
	xxland     x28,x6,x47
	xxlor      x22,x13,x13
	xxpermdi   x47,x21,x21,1
	xxpermdi   x18,x43,x43,1
	xxland     x46,x8,x46
	xxpermdi   x21,x51,x51,1
	xxpermdi   x51,x42,x42,1
	xvmulsp    x15,x45,x28
	xxsel      x42,x9,x62,x33
	xxpermdi   x43,x50,x50,1
	xxpermdi   x50,x49,x49,1
	xxpermdi   x49,x60,x60,1
	xxpermdi   x28,x48,x48,1
	xvmaddasp  x22,x23,x31
	xxpermdi   x48,x27,x27,1
	xvmulsp    x17,x59,x46
	xvmaddmsp  x19,x20,x29
	vperm      v14,v7,v8,v10
	xxspltw    x27,x0,0
	bc         BO_dCTR_ZERO,CR0_LT,__L1d74
	addi       r0,r0,304
	stvx       v26,SP,r0
	ori        r0,r0,0x0000
__L1c80:                                # 0x00001c80 (H.10.NO_SYMBOL+0x1c80)
	vperm      v13,v9,v9,v4
	xxlor      x62,x25,x25
	xxpermdi   x63,x41,x41,1
	lfs        fp0,24(r4)
	xvmsubasp  x62,x28,x2
	xxpermdi   x61,x20,x20,1
	xxpermdi   x20,x23,x23,1
	xvsubsp    x23,x24,x46
	xxpermdi   x60,x16,x16,1
	xxpermdi   x41,x26,x26,1
	xxlor      x26,x1,x1
	xvmaddasp  x26,x27,x2
	vperm      v15,v5,v6,v15
	xxpermdi   x46,x44,x44,1
	xvcmpgtsp  x16,x5,x43
	vslw       v12,v11,v0
	xxlor      x59,x30,x30
	xvmaddasp  x59,x20,x22
	xvcmpgtsp  x22,x3,x14
	xvmaddmsp  x61,x19,x6
	xxpermdi   x24,x28,x28,1
	xxland     x25,x10,x41
	xvcmpgtsp  x19,x14,x4
	vadduwm    v14,v3,v14
	xvmulsp    x58,x17,x47
	xscvdpspn  x47,x0
	xxpermdi   x28,x48,x48,1
	addi       r4,r4,4
	xxpermdi   x48,x27,x27,1
	xvsubsp    x25,x25,x11
	xxsel      x0,x44,x7,x16
	xxspltw    x27,x47,0
	xxsel      x19,x15,x8,x19
	xxlxor     x17,x22,x34
	xxsel      x46,x6,x46,x60
	xvmaddmsp  x24,x12,x62
	xxland     x0,x0,x8
	xxpermdi   x47,x21,x21,1
	xxpermdi   x21,x51,x51,1
	xxlor      x22,x13,x13
	xvmaddasp  x22,x23,x31
	xxpermdi   x51,x42,x42,1
	xxland     x15,x17,x19
	xxland     x46,x46,x6
	xxlor      x19,x29,x29
	xvmaddasp  x19,x20,x59
	xxsel      x42,x9,x45,x33
	xxpermdi   x14,x18,x18,1
	xxpermdi   x18,x43,x43,1
	xvmulsp    x17,x61,x0
	xscvspdpn  x0,x15
	xvmulsp    x15,x58,x46
	xxpermdi   x43,x50,x50,1
	xxpermdi   x50,x49,x49,1
	vperm      v14,v7,v8,v10
	stfs       fp0,-8(r3)
	xxpermdi   x49,x63,x63,1
	addi       r3,r3,4
	bc         BO_dCTR_NZERO,CR0_LT,__L1c80
	addi       r0,r0,304
	lvx        v26,SP,r0
__L1d74:                                # 0x00001d74 (H.10.NO_SYMBOL+0x1d74)
	xvcmpgtsp  x0,x3,x14
	xvcmpgtsp  x45,x14,x4
	xxlxor     x0,x0,x34
	xxsel      x45,x15,x8,x45
	lfd        fp14,480(SP)
	addi       r0,r0,320
	lvx        v27,SP,r0
	lfd        fp15,488(SP)
	xxland     x0,x0,x45
	xscvspdpn  x0,x0
	stfs       fp0,-8(r3)
	addi       r0,r0,336
	lvx        v28,SP,r0
	addi       r0,r0,352
	lvx        v29,SP,r0
	addi       r0,r0,368
	lvx        v30,SP,r0
	addi       r0,r0,384
	lvx        v31,SP,r0
__L1dc0:                                # 0x00001dc0 (H.10.NO_SYMBOL+0x1dc0)
	xxpermdi   x0,x16,x16,1
	vperm      v13,v5,v6,v15
	xxpermdi   x44,x44,x44,1
	xxpermdi   x47,x18,x18,1
	vadduwm    v12,v3,v12
	xvmulsp    x45,x17,x45
	xxsel      x0,x6,x44,x0
	xvcmpgtsp  x44,x3,x47
	lfd        fp16,496(SP)
	xvcmpgtsp  x47,x47,x4
	lfd        fp17,504(SP)
	xxland     x0,x0,x6
	lfd        fp18,512(SP)
	xvmulsp    x0,x45,x0
	xxlxor     x44,x44,x34
	xxsel      x0,x0,x8,x47
	xxland     x0,x0,x44
	xscvspdpn  x0,x0
	stfs       fp0,-4(r3)
__L1e0c:                                # 0x00001e0c (H.10.NO_SYMBOL+0x1e0c)
	xxpermdi   x0,x20,x20,1
	xvcmpgtsp  x47,x5,x43
	vslw       v12,v11,v0
	xxpermdi   x45,x21,x21,1
	xxpermdi   x21,x43,x43,1
	xxpermdi   x43,x44,x44,1
	xxsel      x20,x44,x7,x47
	xvmaddmsp  x0,x19,x6
	xxpermdi   x47,x47,x47,1
	vperm      v12,v5,v6,v13
	xxland     x45,x8,x20
	vadduwm    v11,v3,v11
	lfd        fp19,520(SP)
	xxpermdi   x21,x21,x21,1
	lfd        fp20,528(SP)
	xvmulsp    x0,x0,x45
	xxsel      x43,x6,x43,x47
	xvmulsp    x0,x0,x44
	xvcmpgtsp  x44,x3,x21
	xvcmpgtsp  x45,x21,x4
	lfd        fp21,536(SP)
	xxland     x43,x43,x6
	xxlxor     x44,x44,x34
	xvmulsp    x0,x0,x43
	xxsel      x0,x0,x8,x45
	xxland     x0,x0,x44
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L1e7c:                                # 0x00001e7c (H.10.NO_SYMBOL+0x1e7c)
	xxpermdi   x0,x23,x23,1
	xxpermdi   x45,x51,x51,1
	xxpermdi   x43,x50,x50,1
	addi       r3,r3,4
	xvmaddmsp  x22,x0,x30
	xxpermdi   x23,x0,x0,1
	xvmaddmsp  x22,x0,x29
	xvcmpgtsp  x0,x5,x43
	vslw       v12,v11,v0
	xxpermdi   x45,x45,x45,1
	xxsel      x47,x44,x7,x0
	xvmaddmsp  x23,x22,x6
	xxpermdi   x44,x44,x44,1
	vperm      v13,v5,v6,v13
	xxland     x47,x8,x47
	xxpermdi   x50,x43,x43,1
	vadduwm    v11,v3,v12
	xxpermdi   x0,x0,x0,1
	lfd        fp22,544(SP)
	xvmulsp    x47,x23,x47
	xvmulsp    x44,x47,x45
	xxsel      x0,x6,x43,x0
	xxpermdi   x43,x50,x50,1
	xvcmpgtsp  x45,x3,x43
	xxland     x0,x0,x6
	xvcmpgtsp  x43,x43,x4
	xvmulsp    x0,x44,x0
	xxlxor     x44,x34,x45
	xxsel      x0,x0,x8,x43
	xxland     x0,x0,x44
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L1efc:                                # 0x00001efc (H.10.NO_SYMBOL+0x1efc)
	xvsubsp    x24,x24,x46
	xxpermdi   x0,x42,x42,1
	xxpermdi   x42,x49,x49,1
	addi       r3,r3,4
	xxpermdi   x23,x24,x24,1
	xxpermdi   x0,x0,x0,1
	xvmaddmsp  x24,x31,x13
	xxpermdi   x42,x42,x42,1
	xxpermdi   x43,x0,x0,1
	xxpermdi   x0,x23,x23,1
	xvcmpgtsp  x45,x5,x42
	xvmaddmsp  x24,x23,x30
	xxpermdi   x46,x42,x42,1
	vperm      v11,v5,v6,v11
	vslw       v10,v10,v0
	xxpermdi   x44,x45,x45,1
	xvmaddmsp  x24,x23,x29
	xxpermdi   x46,x46,x46,1
	xxsel      x45,x42,x7,x45
	xvmaddmsp  x0,x24,x6
	xxpermdi   x42,x42,x42,1
	xvcmpgtsp  x47,x3,x46
	xvcmpgtsp  x46,x46,x4
	xxland     x45,x8,x45
	lfd        fp23,552(SP)
	xvmulsp    x0,x0,x45
	lfd        fp24,560(SP)
	vadduwm    v10,v3,v10
	xvmulsp    x0,x0,x43
	xxlxor     x45,x34,x47
	xxsel      x42,x6,x42,x44
	xxland     x42,x42,x6
	xvmulsp    x0,x0,x42
	xxsel      x0,x0,x8,x46
	xxland     x0,x0,x45
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L1f90:                                # 0x00001f90 (H.10.NO_SYMBOL+0x1f90)
	vperm      v10,v9,v9,v4
	xvmsubasp  x25,x28,x2
	xxpermdi   x0,x41,x41,1
	xxpermdi   x28,x28,x28,1
	xvmaddasp  x25,x28,x12
	xxsel      x41,x9,x42,x33
	xxpermdi   x0,x0,x0,1
	xxlor      x28,x13,x13
	addi       r3,r3,4
	vperm      v10,v7,v8,v9
	xxpermdi   x41,x41,x41,1
	xxpermdi   x0,x0,x0,1
	xvsubsp    x25,x25,x42
	xvmaddasp  x28,x25,x31
	xxpermdi   x41,x41,x41,1
	xxpermdi   x25,x25,x25,1
	xxpermdi   x42,x0,x0,1
	xxpermdi   x0,x25,x25,1
	xvmaddmsp  x28,x25,x30
	xxpermdi   x41,x41,x41,1
	vslw       v11,v10,v0
	xvcmpgtsp  x45,x5,x42
	xxpermdi   x44,x42,x42,1
	vperm      v9,v5,v6,v9
	xvmaddmsp  x28,x25,x29
	xxpermdi   x42,x43,x43,1
	xxsel      x43,x43,x7,x45
	xvmaddmsp  x0,x28,x6
	xxpermdi   x44,x44,x44,1
	xxpermdi   x46,x45,x45,1
	vadduwm    v10,v3,v10
	xxland     x43,x8,x43
	xvcmpgtsp  x45,x3,x44
	xxsel      x42,x6,x42,x46
	xvcmpgtsp  x44,x44,x4
	xvmulsp    x0,x0,x43
	lfd        fp25,568(SP)
	xxlxor     x43,x34,x45
	xvmulsp    x0,x0,x41
	xxland     x42,x42,x6
	xvmulsp    x0,x0,x42
	xxsel      x0,x0,x8,x44
	xxland     x0,x0,x43
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L2044:                                # 0x00002044 (H.10.NO_SYMBOL+0x2044)
	xxpermdi   x41,x26,x26,1
	xxpermdi   x26,x48,x48,1
	addi       r3,r3,4
	xxland     x0,x10,x41
	xxpermdi   x28,x26,x26,1
	xvsubsp    x0,x0,x11
	vperm      v10,v9,v9,v4
	xxpermdi   x43,x41,x41,1
	xvmsubasp  x0,x26,x2
	xvmaddasp  x0,x28,x12
	xxsel      x41,x9,x42,x33
	xxpermdi   x44,x43,x43,1
	vperm      v10,v7,v8,v9
	xxpermdi   x43,x41,x41,1
	xxpermdi   x41,x44,x44,1
	xvsubsp    x28,x0,x42
	xxpermdi   x26,x28,x28,1
	xxpermdi   x41,x41,x41,1
	xxlor      x0,x13,x13
	xxpermdi   x42,x43,x43,1
	xvmaddasp  x0,x28,x31
	xvcmpgtsp  x44,x5,x41
	xxpermdi   x28,x26,x26,1
	vslw       v11,v9,v0
	xxpermdi   x42,x42,x42,1
	xxpermdi   x45,x41,x41,1
	xvmaddmsp  x0,x26,x30
	xxsel      x46,x43,x7,x44
	vperm      v9,v5,v6,v10
	xxpermdi   x44,x44,x44,1
	xxpermdi   x42,x43,x43,1
	xvmaddmsp  x0,x26,x29
	xxland     x46,x8,x46
	xvmaddmsp  x28,x0,x6
	vadduwm    v10,v3,v10
	lfd        fp26,576(SP)
	xxpermdi   x43,x45,x45,1
	xxsel      x42,x6,x42,x44
	xvcmpgtsp  x0,x3,x43
	xvcmpgtsp  x43,x43,x4
	xvmulsp    x44,x28,x46
	xvmulsp    x41,x44,x41
	xxland     x42,x42,x6
	xxlxor     x0,x0,x34
	xvmulsp    x41,x41,x42
	xxsel      x41,x41,x8,x43
	xxland     x0,x0,x41
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L2108:                                # 0x00002108 (H.10.NO_SYMBOL+0x2108)
	xvmaddasp  x1,x27,x2
	xxpermdi   x0,x27,x27,1
	addi       r3,r3,4
	lfd        fp27,584(SP)
	xxpermdi   x0,x0,x0,1
	xxpermdi   x41,x1,x1,1
	xxpermdi   x28,x0,x0,1
	xxland     x1,x10,x41
	vperm      v4,v9,v9,v4
	xxpermdi   x10,x41,x41,1
	xxsel      x33,x9,x36,x33
	xvsubsp    x1,x1,x11
	xvmsubasp  x1,x0,x2
	vperm      v4,v7,v8,v1
	xvmaddasp  x1,x28,x12
	xxpermdi   x0,x10,x10,1
	xxpermdi   x2,x33,x33,1
	lfd        fp28,592(SP)
	xxpermdi   x0,x0,x0,1
	xxpermdi   x33,x0,x0,1
	xvsubsp    x0,x1,x36
	vslw       v0,v1,v0
	xvmaddasp  x13,x0,x31
	xxpermdi   x9,x2,x2,1
	xvcmpgtsp  x1,x5,x33
	xxpermdi   x0,x0,x0,1
	lfd        fp31,616(SP)
	xxpermdi   x36,x9,x9,1
	xvmaddasp  x30,x0,x13
	xxsel      x5,x32,x7,x1
	xxpermdi   x2,x0,x0,1
	xvmaddasp  x29,x0,x30
	xxland     x5,x5,x8
	vperm      v4,v5,v6,v4
	xxpermdi   x32,x32,x32,1
	xvmaddmsp  x2,x29,x6
	vadduwm    v0,v0,v3
	xvmulsp    x2,x2,x5
	lfd        fp29,600(SP)
	xxpermdi   x1,x1,x1,1
	lfd        fp30,608(SP)
	xxpermdi   x0,x33,x33,1
	xxsel      x1,x6,x32,x1
	xvmulsp    x2,x2,x36
	xxpermdi   x0,x0,x0,1
	xvcmpgtsp  x3,x3,x0
	xxland     x1,x1,x6
	xvmulsp    x1,x2,x1
	xvcmpgtsp  x0,x0,x4
	xxlxor     x2,x3,x34
	xxsel      x0,x1,x8,x0
	xxland     x0,x0,x2
	xscvspdpn  x0,x0
	stfs       fp0,0(r3)
__L21e0:                                # 0x000021e0 (H.10.NO_SYMBOL+0x21e0)
	addi       SP,SP,624
	bclr       BO_ALWAYS,CR0_LT
__L21e8:                                # 0x000021e8 (H.10.NO_SYMBOL+0x21e8)
	addi       SP,SP,624
	bclr       BO_ALWAYS,CR0_LT
	.long	0x00000000
# traceback table
	.byte	0x00			# VERSION=0
	.byte	0x09			# LANG=TB_CPLUSPLUS
	.byte	0x22			# IS_GL=0,IS_EPROL=0,HAS_TBOFF=1
					# INT_PROC=0,HAS_CTL=0,TOCLESS=0
					# FP_PRESENT=1,LOG_ABORT=0
	.byte	0x00			# INT_HNDL=0,NAME_PRESENT=0
					# USES_ALLOCA=0,CL_DIS_INV=WALK_ONCOND
					# SAVES_CR=0,SAVES_LR=0
	.byte	0x92			# STORES_BC=1,FPR_SAVED=18
	.byte	0x13			# GPR_SAVED=19
	.byte	0x00			# FIXEDPARMS=0
	.byte	0x00			# FLOATPARMS=0,PARMSONSTK=0
	.long	0x000021f0		# TB_OFFSET
# End of traceback table
# End	csect	H.10.NO_SYMBOL{PR}

# .data section


	.toc	                        # 0x00002200 
T.28.vsexp:
	.tc	H.28.vsexp{TC},vsexp{DS}
T.18.NO_SYMBOL:
	.tc	H.18.NO_SYMBOL{TC},H.20.NO_SYMBOL{RO}
T.26.vsexp_:
	.tc	H.26.vsexp_{TC},vsexp{DS}


	.csect	vsexp{DS}               
vsexp_:                                 # 0x0000220c (vsexp)
	.long	.vsexp                  # "\0\0\0\0"
	.long	TOC{TC0}                # "\0\0"\0"
	.long	0x00000000              # "\0\0\0\0"
# End	csect	vsexp{DS}
	.long	0x00000000              # "\0\0\0\0"
	.long	0x00000000              # "\0\0\0\0"
	.long	0x00000000              # "\0\0\0\0"


	.csect	H.20.NO_SYMBOL{RO}, 4   
	.long	0xbebebe00              # "\276\276\276\0"
	.long	0x003e3e3e              # "\0>>>"
	.long	0xdf9f4000              # "\337\237@\0"
	.long	0x003fa0df              # "\0?\240\337"
	.long	0x3d636e20              # "=cn "
	.long	0x3d636e20              # "=cn "
	.long	0x3d636e20              # "=cn "
	.long	0x3d636e20              # "=cn "
	.long	0xfffff800              # "\377\377\370\0"
	.long	0xfffff800              # "\377\377\370\0"
	.long	0xfffff800              # "\377\377\370\0"
	.long	0xfffff800              # "\377\377\370\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x00800000              # "\0\200\0\0"
	.long	0x00800000              # "\0\200\0\0"
	.long	0x00800000              # "\0\200\0\0"
	.long	0x00800000              # "\0\200\0\0"
	.long	0xfffc0600              # "\377\374\006\0"
	.long	0x00fd00ff              # "\0\375\0\377"
	.long	0xc0401500              # "\300@\025\0"
	.long	0x00ced478              # "\0\316\324x"
	.long	0x02020202              # "\002\002\002\002"
	.long	0x06060606              # "\006\006\006\006"
	.long	0x0a0a0a0a              # "\n\n\n\n"
	.long	0x0e0e0e0e              # "\016\016\016\016"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x3f317217              # "?1r\027"
	.long	0x3f317217              # "?1r\027"
	.long	0x3f317217              # "?1r\027"
	.long	0x3f317217              # "?1r\027"
	.long	0x3f3f3f40              # "???@"
	.long	0x40404040              # "@@@@"
	.long	0xbdcee000              # "\275\316\340\0"
	.long	0x00111e2d              # "\0\021\036-"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x00000000              # "\0\0\0\0"
	.long	0x00000000              # "\0\0\0\0"
	.long	0x00000000              # "\0\0\0\0"
	.long	0x00000000              # "\0\0\0\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x32a57060              # "2\245p`"
	.long	0x32a57060              # "2\245p`"
	.long	0x32a57060              # "2\245p`"
	.long	0x32a57060              # "2\245p`"
	.long	0x0825cb00              # "\b%\313\0"
	.long	0x00c3f558              # "\0\303\365X"
	.long	0xb498f200              # "\264\230\362\0"
	.long	0x009c601f              # "\0\234`\037"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x07070707              # "\a\a\a\a"
	.long	0x07070707              # "\a\a\a\a"
	.long	0x07070707              # "\a\a\a\a"
	.long	0x07070707              # "\a\a\a\a"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x45c3f400              # "E\303\364\0"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x3fb8aa3b              # "?\270\252;"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x45bf4c00              # "E\277L\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x45c7f400              # "E\307\364\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x45c00800              # "E\300\b\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x3f000000              # "?\0\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x3f800000              # "?\200\0\0"
	.long	0x00800000              # "\0\200\0\0"
	.long	0x00800000              # "\0\200\0\0"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x7f800000              # "\177\200\0\0"
	.long	0x49424d20              # "IBM "
	.long	0x49424d20              # "IBM "
	.long	0x02020202              # "\002\002\002\002"
	.long	0x06060606              # "\006\006\006\006"
	.long	0x0a0a0a0a              # "\n\n\n\n"
	.long	0x0e0e0e0e              # "\016\016\016\016"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x00081018              # "\0\b\020\030"
	.long	0x49424d20              # "IBM "
	.long	0x49424d20              # "IBM "
	.long	0x3f3f3f40              # "???@"
	.long	0x40404040              # "@@@@"
	.long	0xbdcee000              # "\275\316\340\0"
	.long	0x00111e2d              # "\0\021\036-"
	.long	0x0825cb00              # "\b%\313\0"
	.long	0x00c3f558              # "\0\303\365X"
	.long	0xb498f200              # "\264\230\362\0"
	.long	0x009c601f              # "\0\234`\037"
	.long	0xbebebe00              # "\276\276\276\0"
	.long	0x003e3e3e              # "\0>>>"
	.long	0xdf9f4000              # "\337\237@\0"
	.long	0x003fa0df              # "\0?\240\337"
	.long	0xfffc0600              # "\377\374\006\0"
	.long	0x00fd00ff              # "\0\375\0\377"
	.long	0xc0401500              # "\300@\025\0"
	.long	0x00ced478              # "\0\316\324x"
	.long	0xfffff800              # "\377\377\370\0"
	.long	0xfffff800              # "\377\377\370\0"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x45c3f000              # "E\303\360\0"
	.long	0x32a57060              # "2\245p`"
	.long	0x32a57060              # "2\245p`"
	.long	0x3d636e20              # "=cn "
	.long	0x3d636e20              # "=cn "
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3c1da481              # "<\035\244\201"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x3e75fdef              # ">u\375\357"
	.long	0x3f317217              # "?1r\027"
	.long	0x3f317217              # "?1r\027"
# End	csect	H.20.NO_SYMBOL{RO}



# .bss section


# dwarf sections

# end dwarf sections
