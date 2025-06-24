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
    mulli      r5,r5,4
    l r6,L..constants(RTOC)
    l r7,16(r6)
    lxv v0,496(r6)
    lxv v4,480(r6)
    lxv v1,400(r6)
    lxv v3,144(r6)
    lxv v2,64(r6)
    lxv v6,32(r6)
    lxv v5,48(r6)
    l r8,0(r6)
SILU_INIT:
    xor r28,r5,r5
    xor r29,r28,r28
    cmpi 0,r5,0 ; ble RETURN
SILU_PROLOGUE0:
    addc r31,r4,r28
    lxv v27,0(r31)
    addc r31,r4,r28
    lxv v25,16(r31)
    addc r31,r4,r28
    lxv v28,32(r31)
    lxv v31,448(r6)
    vxor v30,v28,v31
    lxv v31,384(r6)
    xvmulsp v20,v30,v31
    lxv v31,432(r6)
    vcmpgtfp v24,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v24
    xvaddsp v31,v31,v20
    vctsxs v22,v31,23
    lxv v31,288(r6)
    vrlq v23,v22,v31
    lxv v30,272(r6)
    lxv v29,304(r6)
    lxv v31,256(r6)
    vperm v30,v23,v23,v30
    vsel v17,v29,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v23,208(r6)
    lxv v29,192(r6)
    vperm v15,v30,v31,v17
    vperm v19,v23,v29,v17
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v23,336(r6)
    lxv v29,320(r6)
    vperm v16,v30,v31,v17
    vperm v29,v23,v29,v17
    lxv v31,352(r6)
    vsel v30,v0,v22,v31
    vmr v10,v30
    vmr v31,v30
    xvmaddmsp v30,v29,v16
    xvmaddmsp v31,v30,v19
    xvmaddmsp v10,v31,v15
    lxv v31,368(r6)
    vand v30,v22,v31
    vadduwm v11,v30,v0
    lxv v31,464(r6)
    vsel v15,v31,v0,v24
    lxv v31,448(r6)
    vxor v30,v25,v31
    lxv v31,384(r6)
    xvmulsp v29,v30,v31
    vcmpeqfp v19,v29,v29
    lxv v31,112(r6)
    vcmpgtfp v16,v31,v29
    lxv v31,128(r6)
    vcmpgtfp v17,v29,v31
    lxv v31,432(r6)
    vcmpgtfp v22,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v22
    xvaddsp v31,v31,v29
    vctsxs v21,v31,23
    lxv v31,288(r6)
    vrlq v23,v21,v31
    lxv v30,272(r6)
    lxv v29,304(r6)
    lxv v31,256(r6)
    vperm v30,v23,v23,v30
    vsel v12,v29,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v23,208(r6)
    lxv v29,192(r6)
    vperm v24,v30,v31,v12
    vperm v14,v23,v29,v12
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v23,336(r6)
    lxv v29,320(r6)
    vperm v13,v30,v31,v12
    vperm v29,v23,v29,v12
    lxv v31,352(r6)
    vsel v23,v0,v21,v31
    vmr v30,v23
    vmr v31,v23
    xvmaddmsp v23,v29,v13
    xvmaddmsp v31,v23,v14
    xvmaddmsp v30,v31,v24
    lxv v31,368(r6)
    vand v29,v21,v31
    vadduwm v31,v29,v0
    xvmulsp v29,v31,v30
    lxv v31,464(r6)
    vsel v31,v31,v0,v22
    xvmulsp v29,v31,v29
    vsel v31,v29,v3,v17
    vsel v30,v31,v1,v16
    vsel v30,v4,v30,v19
    xvaddsp v24,v0,v30
    vand v14,v24,v2
    vcmpgtfp v18,v14,v6
    vcmpgtfp v29,v14,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v13,v30,v31,v29
    lxv v31,448(r6)
    vxor v16,v27,v31
    lxv v31,384(r6)
    xvmulsp v19,v16,v31
    vcmpeqfp v30,v19,v19
    lxv v31,112(r6)
    vcmpgtfp v29,v31,v19
    lxv v31,128(r6)
    vcmpgtfp v23,v19,v31
    lxv v31,432(r6)
    vcmpgtfp v21,v16,v31
    lxv v16,416(r6)
    vsel v31,v16,v1,v21
    xvaddsp v31,v31,v19
    vctsxs v26,v31,23
    lxv v31,288(r6)
    vrlq v16,v26,v31
    lxv v19,272(r6)
    lxv v17,304(r6)
    lxv v31,256(r6)
    vperm v19,v16,v16,v19
    vsel v12,v17,v19,v31
    lxv v19,176(r6)
    lxv v31,160(r6)
    lxv v17,208(r6)
    lxv v16,192(r6)
    vperm v19,v19,v31,v12
    vperm v17,v17,v16,v12
    lxv v16,240(r6)
    lxv v31,224(r6)
    lxv v8,336(r6)
    lxv v9,320(r6)
    vperm v22,v16,v31,v12
    vperm v12,v8,v9,v12
    lxv v16,352(r6)
    vsel v9,v0,v26,v16
    vmr v31,v9
    vmr v16,v9
    xvmaddmsp v9,v12,v22
    xvmaddmsp v16,v9,v17
    xvmaddmsp v31,v16,v19
    lxv v19,368(r6)
    vand v16,v26,v19
    vadduwm v19,v16,v0
    xvmulsp v31,v19,v31
    lxv v19,464(r6)
    vsel v19,v19,v0,v21
    xvmulsp v16,v19,v31
    vsel v23,v16,v3,v23
    vsel v31,v23,v1,v29
    vsel v30,v4,v31,v30
    xvaddsp v22,v0,v30
    vand v17,v22,v2
    vcmpgtfp v31,v17,v0
    lxv v29,96(r6)
    lxv v30,80(r6)
    vsel v16,v29,v30,v31
    xvmulsp v12,v16,v17
    vrefp v26,v12
SILU_LOOPHEAD:
    xor r30,r30,r7
    xor r29,r29,r7
    sub r31,r5,r8
    cmp 0,r28,r31 ; bge SILU_EPILOGUE0
SILU_KERNEL:
    xvmulsp v19,v13,v14
    xvmulsp v31,v11,v10
    xvnmsubmsp v12,v26,v0
    xvmaddmsp v12,v26,v26
    xvmulsp v23,v15,v31
    vrefp v26,v19
    lxv v31,128(r6)
    vcmpgtfp v29,v20,v31
    xvnmsubmsp v19,v26,v0
    vsel v23,v23,v3,v29
    lxv v31,112(r6)
    vcmpgtfp v30,v31,v20
    xvmulsp v15,v16,v12
    vcmpgefp v16,v5,v17
    vcmpgefp v21,v5,v14
    vcmpgtfp v17,v17,v6
    vcmpeqfp v29,v20,v20
    vsel v16,v15,v3,v16
    xvmaddmsp v19,v26,v26
    vsel v23,v23,v1,v30
    xvmulsp v20,v13,v19
    vsel v29,v4,v23,v29
    vsel v26,v16,v1,v17
    vcmpeqfp v23,v22,v22
    vsel v23,v4,v26,v23
    vsel v30,v20,v3,v21
    xvaddsp v21,v0,v29
    vsel v29,v30,v1,v18
    vcmpeqfp v30,v24,v24
    vand v20,v21,v2
    vsel v31,v22,v23,v2
    xvmulsp v26,v27,v31
    vsel v22,v4,v29,v30
    vcmpgtfp v23,v20,v0
    addc r31,r3,r28
    lxv v29,96(r6)
    lxv v30,80(r6)
    vsel v31,v24,v22,v2
    vsel v23,v29,v30,v23
    stxv v26,0(r31)
    xvmulsp v31,v25,v31
    xvmulsp v29,v23,v20
    addc r31,r3,r28
    vrefp v30,v29
    stxv v31,16(r31)
    addc r31,r4,r28
    xvnmsubmsp v29,v30,v0
    lxv v26,48(r31)
    xvmaddmsp v29,v30,v30
    lxv v31,448(r6)
    vxor v30,v26,v31
    xvmulsp v24,v23,v29
    vcmpgefp v29,v5,v20
    vcmpgtfp v23,v20,v6
    lxv v31,432(r6)
    vcmpgtfp v25,v30,v31
    vsel v22,v24,v3,v29
    lxv v31,384(r6)
    xvmulsp v24,v30,v31
    lxv v29,416(r6)
    vsel v30,v29,v1,v25
    vsel v23,v22,v1,v23
    vcmpeqfp v29,v21,v21
    xvaddsp v30,v30,v24
    vsel v29,v4,v23,v29
    vctsxs v20,v30,23
    vsel v29,v21,v29,v2
    lxv v31,288(r6)
    vrlq v30,v20,v31
    xvmulsp v29,v28,v29
    lxv v31,272(r6)
    vperm v23,v30,v30,v31
    addc r31,r3,r28
    stxv v29,32(r31)
    addc r31,r4,r28
    lxv v29,464(r6)
    lxv v30,304(r6)
    lxv v31,256(r6)
    vsel v19,v29,v0,v25
    vsel v22,v30,v23,v31
    lxv v27,64(r31)
    lxv v29,352(r6)
    lxv v30,336(r6)
    lxv v31,320(r6)
    vsel v21,v0,v20,v29
    vperm v25,v30,v31,v22
    lxv v30,240(r6)
    lxv v31,224(r6)
    vperm v23,v30,v31,v22
    lxv v31,448(r6)
    vxor v15,v27,v31
    lxv v31,368(r6)
    vand v20,v20,v31
    addc r31,r4,r28
    vmr v28,v21
    vmr v29,v21
    lxv v30,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v21,v25,v23
    vperm v23,v30,v31,v22
    lxv v25,80(r31)
    lxv v31,432(r6)
    vcmpgtfp v17,v15,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    vperm v30,v30,v31,v22
    vadduwm v31,v20,v0
    xvmaddmsp v28,v21,v23
    lxv v23,384(r6)
    xvmulsp v22,v15,v23
    lxv v15,416(r6)
    vsel v15,v15,v1,v17
    lxv v23,448(r6)
    vxor v23,v25,v23
    xvmaddmsp v29,v28,v30
    lxv v30,432(r6)
    vcmpgtfp v20,v23,v30
    xvaddsp v15,v15,v22
    xvmulsp v31,v31,v29
    lxv v30,384(r6)
    xvmulsp v16,v23,v30
    vctsxs v14,v15,23
    lxv v29,416(r6)
    vsel v23,v29,v1,v20
    xvmulsp v29,v19,v31
    lxv v31,128(r6)
    vcmpgtfp v30,v24,v31
    xvaddsp v23,v23,v16
    lxv v31,288(r6)
    vrlq v15,v14,v31
    vsel v28,v29,v3,v30
    lxv v31,112(r6)
    vcmpgtfp v29,v31,v24
    vcmpeqfp v30,v24,v24
    lxv v31,272(r6)
    vperm v19,v15,v15,v31
    vctsxs v24,v23,23
    vsel v29,v28,v1,v29
    lxv v31,288(r6)
    vrlq v23,v24,v31
    lxv v28,304(r6)
    lxv v15,256(r6)
    vsel v18,v28,v19,v15
    vsel v30,v4,v29,v30
    lxv v15,464(r6)
    lxv v29,352(r6)
    vsel v17,v15,v0,v17
    vsel v12,v0,v14,v29
    lxv v19,336(r6)
    lxv v15,320(r6)
    lxv v29,240(r6)
    lxv v31,224(r6)
    vperm v28,v19,v15,v18
    vperm v15,v29,v31,v18
    lxv v31,272(r6)
    vperm v29,v23,v23,v31
    xvaddsp v21,v0,v30
    lxv v31,368(r6)
    vand v14,v14,v31
    vmr v19,v12
    vmr v13,v12
    lxv v30,304(r6)
    lxv v31,256(r6)
    vsel v10,v30,v29,v31
    xvmaddmsp v12,v28,v15
    lxv v30,208(r6)
    lxv v31,192(r6)
    vperm v29,v30,v31,v18
    addc r31,r4,r28
    vand v23,v21,v2
    lxv v30,176(r6)
    lxv v31,160(r6)
    vperm v15,v30,v31,v18
    lxv v28,96(r31)
    lxv v31,464(r6)
    vsel v18,v31,v0,v20
    vadduwm v9,v14,v0
    lxv v31,352(r6)
    xvmaddmsp v19,v12,v29
    vsel v8,v0,v24,v31
    lxv v20,336(r6)
    lxv v29,320(r6)
    lxv v30,240(r6)
    lxv v31,224(r6)
    vperm v20,v20,v29,v10
    vperm v29,v30,v31,v10
    vmr v12,v8
    vmr v14,v8
    lxv v31,368(r6)
    vand v7,v24,v31
    lxv v30,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v8,v20,v29
    vperm v11,v30,v31,v10
    xvmaddmsp v13,v19,v15
    vcmpgtfp v29,v23,v0
    lxv v31,448(r6)
    vxor v15,v28,v31
    lxv v30,96(r6)
    lxv v31,80(r6)
    lxv v19,176(r6)
    lxv v20,160(r6)
    vsel v24,v30,v31,v29
    vperm v30,v19,v20,v10
    xvmulsp v29,v9,v13
    vadduwm v13,v7,v0
    lxv v31,432(r6)
    vcmpgtfp v19,v15,v31
    xvmaddmsp v12,v8,v11
    lxv v31,384(r6)
    xvmulsp v20,v15,v31
    xvmaddmsp v14,v12,v30
    xvmulsp v12,v17,v29
    xvmulsp v29,v24,v23
    lxv v31,128(r6)
    vcmpgtfp v17,v22,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v19
    vrefp v30,v29
    vsel v11,v12,v3,v17
    lxv v17,112(r6)
    vcmpgtfp v12,v17,v22
    xvaddsp v17,v31,v20
    xvmulsp v31,v13,v14
    vcmpeqfp v13,v22,v22
    xvmulsp v14,v18,v31
    lxv v31,128(r6)
    vcmpgtfp v15,v16,v31
    xvnmsubmsp v29,v30,v0
    vsel v31,v11,v1,v12
    vctsxs v18,v17,23
    xvmaddmsp v29,v30,v30
    vsel v13,v4,v31,v13
    lxv v31,288(r6)
    vrlq v30,v18,v31
    vsel v17,v14,v3,v15
    lxv v31,112(r6)
    vcmpgtfp v22,v31,v16
    vcmpeqfp v15,v16,v16
    vsel v14,v17,v1,v22
    xvmulsp v24,v24,v29
    vcmpgefp v29,v5,v23
    xvaddsp v22,v0,v13
    lxv v31,272(r6)
    vperm v16,v30,v30,v31
    vcmpgtfp v30,v23,v6
    vsel v29,v24,v3,v29
    vand v17,v22,v2
    lxv v23,304(r6)
    lxv v31,256(r6)
    vsel v13,v23,v16,v31
    vsel v16,v4,v14,v15
    lxv v23,464(r6)
    vsel v15,v23,v0,v19
    vsel v29,v29,v1,v30
    vcmpeqfp v30,v21,v21
    xvaddsp v24,v0,v16
    lxv v16,352(r6)
    lxv v23,336(r6)
    lxv v31,320(r6)
    vsel v12,v0,v18,v16
    vperm v14,v23,v31,v13
    lxv v16,240(r6)
    lxv v23,224(r6)
    vperm v23,v16,v23,v13
    vsel v30,v4,v29,v30
    vcmpgtfp v29,v17,v0
    lxv v31,368(r6)
    vand v11,v18,v31
    lxv v16,96(r6)
    lxv v31,80(r6)
    vsel v16,v16,v31,v29
    vmr v19,v12
    vmr v10,v12
    lxv v29,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v12,v14,v23
    vperm v18,v29,v31,v13
    vand v14,v24,v2
    lxv v23,176(r6)
    lxv v29,160(r6)
    vperm v23,v23,v29,v13
    vsel v31,v21,v30,v2
    vadduwm v11,v11,v0
    vcmpgtfp v21,v14,v0
    xvmaddmsp v19,v12,v18
    xvmulsp v31,v26,v31
    xvmulsp v12,v16,v17
    vrefp v26,v12
    lxv v29,96(r6)
    lxv v30,80(r6)
    xvmaddmsp v10,v19,v23
    vsel v13,v29,v30,v21
    vcmpgtfp v18,v14,v6
    addc r31,r3,r28
    stxv v31,48(r31)
SILU_LOOPEND:
    addc r28,r28,r8
    b SILU_LOOPHEAD
SILU_EPILOGUE0:
    xvmulsp v30,v11,v10
    vcmpeqfp v29,v20,v20
    lxv v31,112(r6)
    vcmpgtfp v23,v31,v20
    lxv v31,128(r6)
    vcmpgtfp v20,v20,v31
    xvmulsp v15,v15,v30
    vsel v31,v15,v3,v20
    vsel v30,v31,v1,v23
    vsel v30,v4,v30,v29
    xvaddsp v11,v0,v30
    vcmpeqfp v29,v11,v11
    vand v21,v11,v2
    vcmpgtfp v23,v21,v6
    vcmpgefp v15,v5,v21
    vcmpgtfp v20,v21,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v19,v30,v31,v20
    xvmulsp v20,v19,v21
    vrefp v30,v20
    xvnmsubmsp v20,v30,v0
    xvmaddmsp v20,v30,v30
    xvmulsp v20,v19,v20
    vsel v31,v20,v3,v15
    vsel v23,v31,v1,v23
    vsel v30,v4,v23,v29
    vsel v29,v11,v30,v2
    vcmpgefp v23,v5,v14
    xvmulsp v15,v13,v14
    vrefp v30,v15
    xvnmsubmsp v15,v30,v0
    xvmaddmsp v15,v30,v30
    xvmulsp v15,v13,v15
    vsel v31,v15,v3,v23
    vsel v15,v31,v1,v18
    vcmpeqfp v23,v24,v24
    vsel v30,v4,v15,v23
    vsel v23,v24,v30,v2
    xvnmsubmsp v12,v26,v0
    xvmaddmsp v12,v26,v26
    xvmulsp v19,v16,v12
    vcmpgtfp v15,v17,v6
    vcmpgefp v20,v5,v17
    vsel v31,v19,v3,v20
    vsel v20,v31,v1,v15
    vcmpeqfp v15,v22,v22
    vsel v30,v4,v20,v15
    vsel v15,v22,v30,v2
    xvmulsp v31,v28,v29
    xvmulsp v30,v25,v23
    xvmulsp v29,v27,v15
    addc r31,r3,r28
    stxv v29,0(r31)
    addc r31,r3,r28
    stxv v30,16(r31)
    addc r31,r3,r28
    stxv v31,32(r31)
    addc r31,r3,r28
    addc r5,r4,r28
    lxv v15,48(r5)
    lxv v31,448(r6)
    vxor v30,v15,v31
    lxv v31,384(r6)
    xvmulsp v29,v30,v31
    vcmpeqfp v20,v29,v29
    lxv v31,112(r6)
    vcmpgtfp v19,v31,v29
    lxv v31,128(r6)
    vcmpgtfp v16,v29,v31
    lxv v31,432(r6)
    vcmpgtfp v14,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v14
    xvaddsp v31,v31,v29
    vctsxs v13,v31,23
    lxv v31,288(r6)
    vrlq v23,v13,v31
    lxv v30,272(r6)
    lxv v29,304(r6)
    lxv v31,256(r6)
    vperm v30,v23,v23,v30
    vsel v28,v29,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v23,208(r6)
    lxv v29,192(r6)
    vperm v17,v30,v31,v28
    vperm v24,v23,v29,v28
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v23,336(r6)
    lxv v29,320(r6)
    vperm v22,v30,v31,v28
    vperm v29,v23,v29,v28
    lxv v31,352(r6)
    vsel v23,v0,v13,v31
    vmr v30,v23
    vmr v31,v23
    xvmaddmsp v23,v29,v22
    xvmaddmsp v31,v23,v24
    xvmaddmsp v30,v31,v17
    lxv v31,368(r6)
    vand v29,v13,v31
    vadduwm v31,v29,v0
    xvmulsp v29,v31,v30
    lxv v31,464(r6)
    vsel v31,v31,v0,v14
    xvmulsp v29,v31,v29
    vsel v31,v29,v3,v16
    vsel v30,v31,v1,v19
    vsel v30,v4,v30,v20
    xvaddsp v24,v0,v30
    vcmpeqfp v29,v24,v24
    vand v17,v24,v2
    vcmpgtfp v23,v17,v6
    vcmpgefp v19,v5,v17
    vcmpgtfp v20,v17,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v16,v30,v31,v20
    xvmulsp v20,v16,v17
    vrefp v30,v20
    xvnmsubmsp v20,v30,v0
    xvmaddmsp v20,v30,v30
    xvmulsp v20,v16,v20
    vsel v31,v20,v3,v19
    vsel v23,v31,v1,v23
    vsel v30,v4,v23,v29
    vsel v31,v24,v30,v2
    xvmulsp v31,v15,v31
    stxv v31,48(r31)
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
    .align 2
    .vbyte 4, 0x00000040
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000200
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x7f7fffff
    .vbyte 4, 0x7f7fffff
    .vbyte 4, 0x7f7fffff
    .vbyte 4, 0x7f7fffff
    .vbyte 4, 0x00200000
    .vbyte 4, 0x00200000
    .vbyte 4, 0x00200000
    .vbyte 4, 0x00200000
    .vbyte 4, 0x7fffffff
    .vbyte 4, 0x7fffffff
    .vbyte 4, 0x7fffffff
    .vbyte 4, 0x7fffffff
    .vbyte 4, 0x2f800000
    .vbyte 4, 0x2f800000
    .vbyte 4, 0x2f800000
    .vbyte 4, 0x2f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0xc3200000
    .vbyte 4, 0xc3200000
    .vbyte 4, 0xc3200000
    .vbyte 4, 0xc3200000
    .vbyte 4, 0x4300ffff
    .vbyte 4, 0x4300ffff
    .vbyte 4, 0x4300ffff
    .vbyte 4, 0x4300ffff
    .vbyte 4, 0x7f800000
    .vbyte 4, 0x7f800000
    .vbyte 4, 0x7f800000
    .vbyte 4, 0x7f800000
    .vbyte 4, 0x01067e8b
    .vbyte 4, 0x527dd750
    .vbyte 4, 0x2758b9e3
    .vbyte 4, 0x7937ab58
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0x3e3f3f3f
    .vbyte 4, 0x31415265
    .vbyte 4, 0xfa081422
    .vbyte 4, 0x1bc99db2
    .vbyte 4, 0x2413a17a
    .vbyte 4, 0x96e2fd82
    .vbyte 4, 0x6e62dab9
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0x3e3e3e3f
    .vbyte 4, 0x0c182635
    .vbyte 4, 0xc6d8eb00
    .vbyte 4, 0x83d8bf5c
    .vbyte 4, 0x6bad0a9e
    .vbyte 4, 0x6cc8c2b3
    .vbyte 4, 0x9f4ffc58
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0xbfd0e3f8
    .vbyte 4, 0x8793a1af
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x04040404
    .vbyte 4, 0x00000000
    .vbyte 4, 0x0c0c0c0c
    .vbyte 4, 0x08080808
    .vbyte 4, 0x00000004
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0xe518aabc
    .vbyte 4, 0x71772ff6
    .vbyte 4, 0xe61761a0
    .vbyte 4, 0x95935393
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0xa7b7c7d9
    .vbyte 4, 0x6d818d99
    .vbyte 4, 0x000fffff
    .vbyte 4, 0x000fffff
    .vbyte 4, 0x000fffff
    .vbyte 4, 0x000fffff
    .vbyte 4, 0xff800000
    .vbyte 4, 0xff800000
    .vbyte 4, 0xff800000
    .vbyte 4, 0xff800000
    .vbyte 4, 0x3fb8aa3b
    .vbyte 4, 0x3fb8aa3b
    .vbyte 4, 0x3fb8aa3b
    .vbyte 4, 0x3fb8aa3b
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x42800000
    .vbyte 4, 0x42800000
    .vbyte 4, 0x42800000
    .vbyte 4, 0x42800000
    .vbyte 4, 0xc2000000
    .vbyte 4, 0xc2000000
    .vbyte 4, 0xc2000000
    .vbyte 4, 0xc2000000
    .vbyte 4, 0x80000000
    .vbyte 4, 0x80000000
    .vbyte 4, 0x80000000
    .vbyte 4, 0x80000000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .toc
L..constants:
    .tc constants[TC],constants[RW]
