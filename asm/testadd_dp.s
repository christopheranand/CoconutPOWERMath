.set r0,0; .set SP,1; .set RTOC,2; .set r3,3; .set r4,4
.set r5,5; .set r6,6; .set r7,7; .set r8,8; .set r9,9
.set r10,10; .set r11,11; .set r12,12; .set r13,13; .set r14,14
.set r15,15; .set r16,16; .set r17,17; .set r18,18; .set r19,19
.set r20,20; .set r21,21; .set r22,22; .set r23,23; .set r24,24
.set r25,25; .set r26,26; .set r27,27; .set r28,28; .set r29,29
.set r30,30; .set r31,31
.set v0,0; .set v1,1; .set v2,2; .set v3,3; .set v4,4
.set v5,5; .set v6,6; .set v7,7; .set v8,8; .set v9,9
.set v10,10; .set v11,11; .set v12,12; .set v13,13; .set v14,14
.set v15,15; .set v16,16; .set v17,17; .set v18,18; .set v19,19
.set v20,20; .set v21,21; .set v22,22; .set v23,23; .set v24,24
.set v25,25; .set v26,26; .set v27,27; .set v28,28; .set v29,29
.set v30,30; .set v31,31
.set BO_ALWAYS,20; .set CR0_LT,0
    .csect .text[PR],2
    .file "square.c","IBM Open XL C/C++ for AIX 17.1.1 (5725-C72, 5765-J18), version 17.1.1.4, LLVM version 15.0.0git"
    .globl square[DS]
    .globl  .square
    .align  4
    .csect square[DS],2
    .vbyte  4, .square
    .vbyte  4, TOC[TC0]
    .vbyte  4, 0
    .csect .text[PR],2
.square:
    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs
    stw     r13,-76(r12)             #save r13
    stw     r14,-72(r12)             #save r14
    stw     r15,-68(r12)             #save r15
    stw     r16,-64(r12)             #save r16
    stw     r17,-60(r12)             #save r17
    stw     r18,-56(r12)             #save r18
    stw     r19,-52(r12)             #save r19
    stw     r20,-48(r12)             #save r20
    stw     r21,-44(r12)             #save r21
    stw     r22,-40(r12)             #save r22
    stw     r23,-36(r12)             #save r23
    stw     r24,-32(r12)             #save r24
    stw     r25,-28(r12)             #save r25
    stw     r26,-24(r12)             #save r26
    stw     r27,-20(r12)             #save r27
    stw     r28,-16(r12)             #save r28
    stw     r29,-12(r12)             #save r29
    stw     r30,-8(r12)              #save r30
    stw     r31,-4(r12)              #save r31
    stfd     v14,-144(SP)             #save v14
    stfd     v15,-136(SP)             #save v15
    stfd     v16,-128(SP)             #save v16
    stfd     v17,-120(SP)             #save v17
    stfd     v18,-112(SP)             #save v18
    stfd     v19,-104(SP)             #save v19
    stfd     v20,-96(SP)              #save v20
    stfd     v21,-88(SP)              #save v21
    stfd     v22,-80(SP)              #save v22
    stfd     v23,-72(SP)              #save v23
    stfd     v24,-64(SP)              #save v24
    stfd     v25,-56(SP)              #save v25
    stfd     v26,-48(SP)              #save v26
    stfd     v27,-40(SP)              #save v27
    stfd     v28,-32(SP)              #save v28
    stfd     v29,-24(SP)              #save v29
    stfd     v30,-16(SP)              #save v30
    stfd     v31,-8(SP)               #save v31
    l          r5,0(r5)
    mulli      r5,r5,8
    l r6,L..constants(RTOC)
    l r7,16(r6)
    lxv v0,32(r6)
    l r8,0(r6)
TESTADD_INIT:
    xor r26,r5,r5
    xor r27,r26,r26
    cmpi 0,r5,0 ; ble RETURN
TESTADD_PROLOGUE0:
    addc r31,r3,r26
    addc r30,r4,r26
    lxv v29,0(r30)
    addc r30,r4,r26
    lxv v31,16(r30)
    addc r29,r4,r26
    xvadddp v30,v31,v0
    xvadddp v31,v29,v0
    stxv v31,0(r31)
    addc r30,r3,r26
TESTADD_LOOPHEAD:
    xor r28,r28,r7
    xor r27,r27,r7
    sub r31,r5,r8
    cmp 0,r26,r31 ; bge TESTADD_EPILOGUE0
TESTADD_KERNEL:
    stxv v30,16(r30)
    lxv v30,32(r29)
    addc r31,r3,r26
    xvadddp v31,v30,v0
    stxv v31,32(r31)
    addc r31,r4,r26
    lxv v30,48(r31)
    xvadddp v31,v30,v0
    addc r31,r4,r26
    lxv v29,64(r31)
    addc r30,r4,r26
    addc r31,r3,r26
    xvadddp v30,v29,v0
    lxv v29,80(r30)
    stxv v30,64(r31)
    addc r31,r3,r26
    xvadddp v30,v29,v0
    stxv v31,48(r31)
    addc r30,r3,r26
    addc r29,r4,r26
TESTADD_LOOPEND:
    addc r26,r26,r8
    b TESTADD_LOOPHEAD
TESTADD_EPILOGUE0:
    stxv v30,16(r30)
    lxv v31,32(r29)
    xvadddp v30,v31,v0
    addc r31,r3,r26
    stxv v30,32(r31)
    addc r31,r3,r26
    addc r5,r4,r26
    lxv v31,48(r5)
    xvadddp v30,v31,v0
    stxv v30,48(r31)
RETURN:
    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs
    lwz     r13,-76(r12)             #restore r13
    lwz     r14,-72(r12)             #restore r14
    lwz     r15,-68(r12)             #restore r15
    lwz     r16,-64(r12)             #restore r16
    lwz     r17,-60(r12)             #restore r17
    lwz     r18,-56(r12)             #restore r18
    lwz     r19,-52(r12)             #restore r19
    lwz     r20,-48(r12)             #restore r20
    lwz     r21,-44(r12)             #restore r21
    lwz     r22,-40(r12)             #restore r22
    lwz     r23,-36(r12)             #restore r23
    lwz     r24,-32(r12)             #restore r24
    lwz     r25,-28(r12)             #restore r25
    lwz     r26,-24(r12)             #restore r26
    lwz     r27,-20(r12)             #restore r27
    lwz     r28,-16(r12)             #restore r28
    lwz     r29,-12(r12)             #restore r29
    lwz     r30,-8(r12)              #restore r30
    lwz     r31,-4(r12)              #restore r31
    lfd     v14,-144(SP)              #restore r14
    lfd     v15,-136(SP)              #restore r15
    lfd     v16,-128(SP)              #restore r16
    lfd     v17,-120(SP)              #restore r17
    lfd     v18,-112(SP)              #restore r18
    lfd     v19,-104(SP)              #restore r19
    lfd     v20,-96(SP)               #restore r20
    lfd     v21,-88(SP)               #restore r21
    lfd     v22,-80(SP)               #restore r22
    lfd     v23,-72(SP)               #restore r23
    lfd     v24,-64(SP)               #restore r24
    lfd     v25,-56(SP)               #restore r25
    lfd     v26,-48(SP)               #restore r26
    lfd     v27,-40(SP)               #restore r27
    lfd     v28,-32(SP)               #restore r28
    lfd     r29,-24(SP)               #restore r29
    lfd     r30,-16(SP)               #restore r30
    lfd     r31,-8(SP)                #restore r31
    blr
L..square0:
    .vbyte  4, 0x00000000
    .byte   0x00
    .byte   0x09
    .byte   0x22
    .byte   0x40
    .byte   0x00
    .byte   0x00
    .byte   0x03
    .byte   0x01
    .vbyte  4, 0x00000000
    .vbyte  4, L..square0-.square
    .vbyte  2, 0x0006
    .byte   "square"
    .csect constants[RW], 3
    .globl constants[RW]
    .long 0x0000000000000040
    .long 0x0000000000000000
    .long 0x0000000000000200
    .long 0x0000000000000000
    .long 0x3ff0000000000000
    .long 0x3ff0000000000000
    .toc
L..constants:
    .tc constants[TC],constants[RW]
