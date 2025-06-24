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
    lxv v0,512(r6)
    lxv v4,496(r6)
    lxv v1,400(r6)
    lxv v3,144(r6)
    lxv v2,64(r6)
    lxv v6,48(r6)
    lxv v5,32(r6)
    l r8,0(r6)
GELUTHREE_INIT:
    xor r28,r5,r5
    xor r29,r28,r28
    cmpi 0,r5,0 ; ble RETURN
GELUTHREE_PROLOGUE0:
    addc r31,r4,r28
    lxv v26,0(r31)
    addc r31,r4,r28
    lxv v28,16(r31)
    addc r31,r4,r28
    lxv v29,32(r31)
    lxv v31,464(r6)
    xvmulsp v30,v29,v31
    lxv v31,448(r6)
    vxor v30,v30,v31
    lxv v31,384(r6)
    xvmulsp v27,v30,v31
    vcmpeqfp v14,v27,v27
    lxv v31,112(r6)
    vcmpgtfp v22,v31,v27
    lxv v31,128(r6)
    vcmpgtfp v10,v27,v31
    lxv v31,432(r6)
    vcmpgtfp v9,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v9
    xvaddsp v31,v31,v27
    vctsxs v18,v31,23
    lxv v31,288(r6)
    vrlq v24,v18,v31
    lxv v30,272(r6)
    lxv v27,304(r6)
    lxv v31,256(r6)
    vperm v30,v24,v24,v30
    vsel v11,v27,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v24,208(r6)
    lxv v27,192(r6)
    vperm v17,v30,v31,v11
    vperm v21,v24,v27,v11
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v24,336(r6)
    lxv v27,320(r6)
    vperm v16,v30,v31,v11
    vperm v27,v24,v27,v11
    lxv v31,352(r6)
    vsel v24,v0,v18,v31
    vmr v30,v24
    vmr v31,v24
    xvmaddmsp v24,v27,v16
    xvmaddmsp v31,v24,v21
    xvmaddmsp v30,v31,v17
    lxv v31,368(r6)
    vand v27,v18,v31
    vadduwm v31,v27,v0
    xvmulsp v27,v31,v30
    lxv v31,480(r6)
    vsel v31,v31,v0,v9
    xvmulsp v27,v31,v27
    vsel v31,v27,v3,v10
    vsel v22,v31,v1,v22
    lxv v31,464(r6)
    xvmulsp v30,v28,v31
    lxv v31,448(r6)
    vxor v30,v30,v31
    lxv v31,384(r6)
    xvmulsp v27,v30,v31
    vcmpeqfp v10,v27,v27
    lxv v31,112(r6)
    vcmpgtfp v17,v31,v27
    lxv v31,128(r6)
    vcmpgtfp v21,v27,v31
    lxv v31,432(r6)
    vcmpgtfp v18,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v18
    xvaddsp v31,v31,v27
    vctsxs v20,v31,23
    lxv v31,288(r6)
    vrlq v24,v20,v31
    lxv v30,272(r6)
    lxv v27,304(r6)
    lxv v31,256(r6)
    vperm v30,v24,v24,v30
    vsel v23,v27,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v24,208(r6)
    lxv v27,192(r6)
    vperm v16,v30,v31,v23
    vperm v11,v24,v27,v23
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v24,336(r6)
    lxv v27,320(r6)
    vperm v9,v30,v31,v23
    vperm v27,v24,v27,v23
    lxv v31,352(r6)
    vsel v24,v0,v20,v31
    vmr v30,v24
    vmr v31,v24
    xvmaddmsp v24,v27,v9
    xvmaddmsp v31,v24,v11
    xvmaddmsp v30,v31,v16
    lxv v31,368(r6)
    vand v27,v20,v31
    vadduwm v31,v27,v0
    xvmulsp v27,v31,v30
    lxv v31,480(r6)
    vsel v31,v31,v0,v18
    xvmulsp v27,v31,v27
    vsel v31,v27,v3,v21
    vsel v30,v31,v1,v17
    vsel v30,v4,v30,v10
    xvaddsp v23,v0,v30
    vand v10,v23,v2
    vcmpgtfp v17,v10,v5
    vcmpgefp v11,v6,v10
    vcmpgtfp v27,v10,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v24,v30,v31,v27
    xvmulsp v9,v24,v10
    vrefp v30,v9
    xvnmsubmsp v9,v30,v0
    xvmaddmsp v9,v30,v30
    lxv v31,464(r6)
    xvmulsp v30,v26,v31
    lxv v31,448(r6)
    vxor v16,v30,v31
    lxv v31,384(r6)
    xvmulsp v21,v16,v31
    vcmpeqfp v30,v21,v21
    lxv v31,112(r6)
    vcmpgtfp v27,v31,v21
    lxv v31,128(r6)
    vcmpgtfp v10,v21,v31
    lxv v31,432(r6)
    vcmpgtfp v19,v16,v31
    lxv v16,416(r6)
    vsel v31,v16,v1,v19
    xvaddsp v31,v31,v21
    vctsxs v13,v31,23
    lxv v31,288(r6)
    vrlq v16,v13,v31
    lxv v21,272(r6)
    lxv v18,304(r6)
    lxv v31,256(r6)
    vperm v21,v16,v16,v21
    vsel v15,v18,v21,v31
    lxv v21,176(r6)
    lxv v31,160(r6)
    lxv v18,208(r6)
    lxv v16,192(r6)
    vperm v21,v21,v31,v15
    vperm v18,v18,v16,v15
    lxv v16,240(r6)
    lxv v31,224(r6)
    lxv v12,336(r6)
    lxv v8,320(r6)
    vperm v20,v16,v31,v15
    vperm v15,v12,v8,v15
    lxv v16,352(r6)
    vsel v8,v0,v13,v16
    vmr v31,v8
    vmr v16,v8
    xvmaddmsp v8,v15,v20
    xvmaddmsp v16,v8,v18
    xvmaddmsp v31,v16,v21
    lxv v21,368(r6)
    vand v16,v13,v21
    vadduwm v21,v16,v0
    xvmulsp v31,v21,v31
    lxv v21,480(r6)
    vsel v21,v21,v0,v19
    xvmulsp v16,v21,v31
    vsel v10,v16,v3,v10
    vsel v31,v10,v1,v27
    vsel v30,v4,v31,v30
    xvaddsp v20,v0,v30
    vand v27,v20,v2
    vcmpgtfp v18,v27,v5
    vcmpgefp v30,v6,v27
    vcmpgtfp v31,v27,v0
    lxv v21,96(r6)
    lxv v10,80(r6)
    vsel v21,v21,v10,v31
    xvmulsp v10,v21,v27
    vrefp v27,v10
    xvnmsubmsp v10,v27,v0
    xvmaddmsp v10,v27,v27
    xvmulsp v27,v21,v10
    vsel v10,v27,v3,v30
GELUTHREE_LOOPHEAD:
    xor r30,r30,r7
    xor r29,r29,r7
    sub r31,r5,r8
    cmp 0,r28,r31 ; bge GELUTHREE_EPILOGUE0
GELUTHREE_KERNEL:
    vsel v27,v4,v22,v14
    vsel v22,v10,v1,v18
    vcmpeqfp v21,v20,v20
    xvmulsp v24,v24,v9
    vsel v25,v4,v22,v21
    vsel v24,v24,v3,v11
    xvaddsp v22,v0,v27
    vand v21,v22,v2
    vsel v31,v20,v25,v2
    vsel v27,v24,v1,v17
    vcmpeqfp v30,v23,v23
    xvmulsp v26,v26,v31
    vsel v20,v4,v27,v30
    vcmpgtfp v27,v21,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v24,v30,v31,v27
    addc r31,r3,r28
    stxv v26,0(r31)
    vsel v31,v23,v20,v2
    xvmulsp v20,v28,v31
    xvmulsp v27,v24,v21
    vrefp v30,v27
    addc r31,r3,r28
    stxv v20,16(r31)
    xvnmsubmsp v27,v30,v0
    xvmaddmsp v27,v30,v30
    xvmulsp v24,v24,v27
    vcmpgefp v27,v6,v21
    vcmpgtfp v30,v21,v5
    vsel v27,v24,v3,v27
    addc r31,r4,r28
    vsel v27,v27,v1,v30
    vcmpeqfp v30,v22,v22
    lxv v25,48(r31)
    vsel v27,v4,v27,v30
    lxv v31,464(r6)
    xvmulsp v30,v25,v31
    vsel v27,v22,v27,v2
    lxv v31,448(r6)
    vxor v30,v30,v31
    xvmulsp v27,v29,v27
    addc r31,r3,r28
    lxv v31,432(r6)
    vcmpgtfp v21,v30,v31
    lxv v31,384(r6)
    xvmulsp v22,v30,v31
    lxv v30,416(r6)
    stxv v27,32(r31)
    vsel v31,v30,v1,v21
    addc r31,r4,r28
    lxv v26,64(r31)
    xvaddsp v31,v31,v22
    vctsxs v20,v31,23
    addc r31,r4,r28
    lxv v31,464(r6)
    xvmulsp v30,v26,v31
    lxv v28,80(r31)
    lxv v31,448(r6)
    vxor v27,v30,v31
    lxv v31,288(r6)
    vrlq v30,v20,v31
    lxv v31,272(r6)
    vperm v24,v30,v30,v31
    lxv v31,432(r6)
    vcmpgtfp v17,v27,v31
    lxv v31,464(r6)
    xvmulsp v30,v28,v31
    lxv v31,384(r6)
    xvmulsp v23,v27,v31
    lxv v27,416(r6)
    vsel v10,v27,v1,v17
    lxv v31,448(r6)
    vxor v11,v30,v31
    lxv v29,480(r6)
    lxv v30,304(r6)
    lxv v31,256(r6)
    vsel v15,v29,v0,v21
    vsel v19,v30,v24,v31
    lxv v29,352(r6)
    lxv v30,336(r6)
    lxv v31,320(r6)
    vsel v18,v0,v20,v29
    vperm v21,v30,v31,v19
    lxv v30,240(r6)
    lxv v31,224(r6)
    vperm v24,v30,v31,v19
    lxv v31,368(r6)
    vand v16,v20,v31
    xvaddsp v30,v10,v23
    lxv v31,432(r6)
    vcmpgtfp v14,v11,v31
    lxv v31,384(r6)
    xvmulsp v20,v11,v31
    vctsxs v11,v30,23
    lxv v30,416(r6)
    vsel v10,v30,v1,v14
    vmr v29,v18
    vmr v27,v18
    lxv v30,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v18,v21,v24
    vperm v24,v30,v31,v19
    lxv v30,176(r6)
    lxv v31,160(r6)
    vperm v30,v30,v31,v19
    xvmaddmsp v29,v18,v24
    vadduwm v31,v16,v0
    lxv v24,288(r6)
    vrlq v21,v11,v24
    xvaddsp v24,v10,v20
    lxv v10,272(r6)
    vperm v10,v21,v21,v10
    vctsxs v18,v24,23
    xvmaddmsp v27,v29,v30
    xvmulsp v31,v31,v27
    lxv v27,304(r6)
    lxv v30,256(r6)
    vsel v12,v27,v10,v30
    lxv v30,288(r6)
    vrlq v27,v18,v30
    lxv v21,352(r6)
    lxv v10,336(r6)
    lxv v24,320(r6)
    vsel v8,v0,v11,v21
    vperm v9,v10,v24,v12
    lxv v10,240(r6)
    lxv v24,224(r6)
    lxv v30,272(r6)
    vperm v16,v10,v24,v12
    vperm v21,v27,v27,v30
    lxv v30,480(r6)
    vsel v17,v30,v0,v17
    xvmulsp v27,v15,v31
    lxv v31,128(r6)
    vcmpgtfp v30,v22,v31
    lxv v31,368(r6)
    vand v15,v11,v31
    addc r31,r4,r28
    vsel v24,v27,v3,v30
    lxv v31,112(r6)
    vcmpgtfp v27,v31,v22
    lxv v29,96(r31)
    vmr v10,v8
    vmr v11,v8
    vcmpeqfp v30,v22,v22
    lxv v22,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v8,v9,v16
    vperm v19,v22,v31,v12
    lxv v22,304(r6)
    lxv v31,256(r6)
    lxv v9,176(r6)
    lxv v16,160(r6)
    vsel v13,v22,v21,v31
    vperm v12,v9,v16,v12
    lxv v31,352(r6)
    xvmaddmsp v10,v8,v19
    vsel v8,v0,v18,v31
    lxv v19,336(r6)
    lxv v21,320(r6)
    lxv v22,240(r6)
    lxv v31,224(r6)
    vperm v9,v19,v21,v13
    vperm v21,v22,v31,v13
    lxv v31,480(r6)
    vsel v16,v31,v0,v14
    vadduwm v14,v15,v0
    vsel v27,v24,v1,v27
    lxv v31,464(r6)
    xvmulsp v22,v29,v31
    vsel v30,v4,v27,v30
    lxv v31,448(r6)
    vxor v19,v22,v31
    lxv v31,368(r6)
    vand v7,v18,v31
    vmr v15,v8
    vmr v18,v8
    xvmaddmsp v11,v10,v12
    xvmaddmsp v8,v9,v21
    lxv v22,208(r6)
    lxv v24,192(r6)
    lxv v27,176(r6)
    lxv v31,160(r6)
    vperm v22,v22,v24,v13
    vperm v27,v27,v31,v13
    xvmulsp v10,v14,v11
    xvmaddmsp v15,v8,v22
    vadduwm v24,v7,v0
    xvaddsp v21,v0,v30
    lxv v31,432(r6)
    vcmpgtfp v11,v19,v31
    vand v22,v21,v2
    lxv v31,384(r6)
    xvmulsp v19,v19,v31
    lxv v30,416(r6)
    vsel v30,v30,v1,v11
    xvmulsp v10,v17,v10
    lxv v31,128(r6)
    vcmpgtfp v31,v23,v31
    xvmaddmsp v18,v15,v27
    vsel v9,v10,v3,v31
    lxv v31,112(r6)
    vcmpgtfp v14,v31,v23
    xvmulsp v18,v24,v18
    vcmpgtfp v27,v22,v0
    xvaddsp v17,v30,v19
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v24,v30,v31,v27
    vcmpeqfp v10,v23,v23
    vctsxs v15,v17,23
    vsel v17,v9,v1,v14
    xvmulsp v27,v16,v18
    lxv v31,128(r6)
    vcmpgtfp v30,v20,v31
    vsel v10,v4,v17,v10
    vsel v18,v27,v3,v30
    lxv v31,112(r6)
    vcmpgtfp v23,v31,v20
    xvmulsp v27,v24,v22
    lxv v31,288(r6)
    vrlq v17,v15,v31
    vrefp v30,v27
    lxv v31,272(r6)
    vperm v16,v17,v17,v31
    vcmpeqfp v14,v20,v20
    xvaddsp v20,v0,v10
    vsel v10,v18,v1,v23
    vand v18,v20,v2
    lxv v31,480(r6)
    vsel v13,v4,v10,v14
    vsel v14,v31,v0,v11
    lxv v23,304(r6)
    lxv v17,256(r6)
    xvnmsubmsp v27,v30,v0
    vsel v10,v23,v16,v17
    lxv v31,352(r6)
    xvmaddmsp v27,v30,v30
    vsel v9,v0,v15,v31
    lxv v23,336(r6)
    lxv v17,320(r6)
    lxv v30,240(r6)
    lxv v31,224(r6)
    vperm v11,v23,v17,v10
    vperm v17,v30,v31,v10
    lxv v31,368(r6)
    vand v8,v15,v31
    vcmpgtfp v30,v18,v0
    xvaddsp v23,v0,v13
    lxv v16,96(r6)
    lxv v31,80(r6)
    vsel v15,v16,v31,v30
    vand v16,v23,v2
    vmr v12,v9
    vmr v13,v9
    xvmulsp v24,v24,v27
    vcmpgefp v27,v6,v22
    vcmpgtfp v30,v22,v5
    lxv v22,208(r6)
    lxv v31,192(r6)
    xvmaddmsp v9,v11,v17
    vperm v11,v22,v31,v10
    lxv v17,176(r6)
    lxv v22,160(r6)
    vsel v27,v24,v3,v27
    vperm v17,v17,v22,v10
    xvmaddmsp v12,v9,v11
    vadduwm v11,v8,v0
    xvmulsp v10,v15,v18
    vcmpgtfp v9,v16,v0
    vrefp v8,v10
    lxv v22,96(r6)
    lxv v24,80(r6)
    vsel v24,v22,v24,v9
    vsel v27,v27,v1,v30
    vcmpeqfp v30,v21,v21
    xvmaddmsp v13,v12,v17
    vsel v30,v4,v27,v30
    xvmulsp v27,v11,v13
    vcmpgtfp v17,v16,v5
    xvnmsubmsp v10,v8,v0
    xvmulsp v9,v24,v16
    xvmaddmsp v10,v8,v8
    vrefp v11,v9
    vsel v31,v21,v30,v2
    xvmulsp v22,v14,v27
    lxv v30,128(r6)
    vcmpgtfp v27,v19,v30
    xvmulsp v31,v25,v31
    vsel v22,v22,v3,v27
    lxv v30,112(r6)
    vcmpgtfp v27,v30,v19
    vcmpeqfp v14,v19,v19
    xvmulsp v10,v15,v10
    vcmpgefp v21,v6,v18
    xvnmsubmsp v9,v11,v0
    vcmpgtfp v18,v18,v5
    vsel v10,v10,v3,v21
    xvmaddmsp v9,v11,v11
    vcmpgefp v11,v6,v16
    addc r31,r3,r28
    stxv v31,48(r31)
    vsel v22,v22,v1,v27
GELUTHREE_LOOPEND:
    addc r28,r28,r8
    b GELUTHREE_LOOPHEAD
GELUTHREE_EPILOGUE0:
    vsel v30,v4,v22,v14
    xvaddsp v19,v0,v30
    vcmpeqfp v27,v19,v19
    vand v15,v19,v2
    vcmpgtfp v22,v15,v5
    vcmpgefp v21,v6,v15
    vcmpgtfp v16,v15,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v14,v30,v31,v16
    xvmulsp v16,v14,v15
    vrefp v30,v16
    xvnmsubmsp v16,v30,v0
    xvmaddmsp v16,v30,v30
    xvmulsp v16,v14,v16
    vsel v31,v16,v3,v21
    vsel v22,v31,v1,v22
    vsel v30,v4,v22,v27
    vsel v27,v19,v30,v2
    xvmulsp v24,v24,v9
    vsel v31,v24,v3,v11
    vsel v22,v31,v1,v17
    vcmpeqfp v24,v23,v23
    vsel v30,v4,v22,v24
    vsel v24,v23,v30,v2
    vsel v10,v10,v1,v18
    vcmpeqfp v22,v20,v20
    vsel v30,v4,v10,v22
    vsel v22,v20,v30,v2
    xvmulsp v31,v29,v27
    xvmulsp v30,v28,v24
    xvmulsp v27,v26,v22
    addc r31,r3,r28
    stxv v27,0(r31)
    addc r31,r3,r28
    stxv v30,16(r31)
    addc r31,r3,r28
    stxv v31,32(r31)
    addc r31,r3,r28
    addc r5,r4,r28
    lxv v22,48(r5)
    lxv v31,464(r6)
    xvmulsp v30,v22,v31
    lxv v31,448(r6)
    vxor v30,v30,v31
    lxv v31,384(r6)
    xvmulsp v27,v30,v31
    vcmpeqfp v10,v27,v27
    lxv v31,112(r6)
    vcmpgtfp v17,v31,v27
    lxv v31,128(r6)
    vcmpgtfp v21,v27,v31
    lxv v31,432(r6)
    vcmpgtfp v23,v30,v31
    lxv v30,416(r6)
    vsel v31,v30,v1,v23
    xvaddsp v31,v31,v27
    vctsxs v20,v31,23
    lxv v31,288(r6)
    vrlq v24,v20,v31
    lxv v30,272(r6)
    lxv v27,304(r6)
    lxv v31,256(r6)
    vperm v30,v24,v24,v30
    vsel v18,v27,v30,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    lxv v24,208(r6)
    lxv v27,192(r6)
    vperm v11,v30,v31,v18
    vperm v16,v24,v27,v18
    lxv v30,240(r6)
    lxv v31,224(r6)
    lxv v24,336(r6)
    lxv v27,320(r6)
    vperm v9,v30,v31,v18
    vperm v27,v24,v27,v18
    lxv v31,352(r6)
    vsel v24,v0,v20,v31
    vmr v30,v24
    vmr v31,v24
    xvmaddmsp v24,v27,v9
    xvmaddmsp v31,v24,v16
    xvmaddmsp v30,v31,v11
    lxv v31,368(r6)
    vand v27,v20,v31
    vadduwm v31,v27,v0
    xvmulsp v27,v31,v30
    lxv v31,480(r6)
    vsel v31,v31,v0,v23
    xvmulsp v27,v31,v27
    vsel v31,v27,v3,v21
    vsel v30,v31,v1,v17
    vsel v30,v4,v30,v10
    xvaddsp v16,v0,v30
    vcmpeqfp v27,v16,v16
    vand v11,v16,v2
    vcmpgtfp v24,v11,v5
    vcmpgefp v10,v6,v11
    vcmpgtfp v17,v11,v0
    lxv v30,96(r6)
    lxv v31,80(r6)
    vsel v21,v30,v31,v17
    xvmulsp v17,v21,v11
    vrefp v30,v17
    xvnmsubmsp v17,v30,v0
    xvmaddmsp v17,v30,v30
    xvmulsp v17,v21,v17
    vsel v31,v17,v3,v10
    vsel v24,v31,v1,v24
    vsel v30,v4,v24,v27
    vsel v31,v16,v30,v2
    xvmulsp v31,v22,v31
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
    .vbyte 4, 0x3fd9db23
    .vbyte 4, 0x3fd9db23
    .vbyte 4, 0x3fd9db23
    .vbyte 4, 0x3fd9db23
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
