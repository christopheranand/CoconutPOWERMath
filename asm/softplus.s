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
    lxv v0,640(r6)
    l r8,0(r6)
SOFTPLUS_INIT:
    xor r27,r5,r5
    xor r28,r27,r27
    cmpi 0,r5,0 ; ble RETURN
SOFTPLUS_PROLOGUE0:
    addc r31,r3,r27
    addc r30,r4,r27
    lxv v26,0(r30)
    addc r30,r4,r27
    lxv v31,16(r30)
    addc r30,r4,r27
    lxv v12,32(r30)
    addc r30,r4,r27
    lxv v29,48(r30)
    lxv v30,624(r6)
    xvmulsp v27,v29,v30
    vcmpeqfp v18,v27,v27
    lxv v30,352(r6)
    vcmpgtfp v20,v30,v27
    lxv v30,368(r6)
    vcmpgtfp v13,v27,v30
    lxv v30,672(r6)
    vcmpgtfp v22,v29,v30
    lxv v29,656(r6)
    vsel v30,v29,v0,v22
    xvaddsp v30,v30,v27
    vctsxs v28,v30,23
    lxv v30,528(r6)
    vrlq v10,v28,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v10,v10,v29
    vsel v24,v27,v29,v30
    lxv v29,416(r6)
    lxv v30,400(r6)
    lxv v10,448(r6)
    lxv v27,432(r6)
    vperm v14,v29,v30,v24
    vperm v15,v10,v27,v24
    lxv v29,480(r6)
    lxv v30,464(r6)
    lxv v10,576(r6)
    lxv v27,560(r6)
    vperm v16,v29,v30,v24
    vperm v27,v10,v27,v24
    lxv v29,736(r6)
    lxv v30,592(r6)
    vsel v10,v29,v28,v30
    vmr v29,v10
    vmr v30,v10
    xvmaddmsp v10,v27,v16
    xvmaddmsp v30,v10,v15
    xvmaddmsp v29,v30,v14
    lxv v30,608(r6)
    vand v27,v28,v30
    lxv v30,736(r6)
    vadduwm v30,v27,v30
    xvmulsp v27,v30,v29
    lxv v29,736(r6)
    lxv v30,688(r6)
    vsel v30,v30,v29,v22
    xvmulsp v27,v30,v27
    lxv v30,384(r6)
    vsel v30,v27,v30,v13
    vsel v29,v30,v0,v20
    lxv v30,752(r6)
    vsel v29,v30,v29,v18
    lxv v30,736(r6)
    xvaddsp v20,v30,v29
    lxv v30,704(r6)
    vcmpgtfp v18,v30,v20
    lxv v30,64(r6)
    lxv v10,736(r6)
    lxv v27,720(r6)
    vsel v22,v0,v30,v18
    vsel v30,v10,v27,v18
    xvmulsp v25,v30,v20
    lxv v30,144(r6)
    vslw v27,v25,v30
    lxv v29,144(r6)
    lxv v30,128(r6)
    vperm v29,v27,v29,v30
    lxv v30,112(r6)
    vadduwm v30,v29,v30
    vcfsx v15,v30,0
    lxv v30,528(r6)
    vrlq v10,v25,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v10,v10,v29
    vsel v21,v27,v29,v30
    lxv v29,208(r6)
    lxv v30,192(r6)
    lxv v18,240(r6)
    lxv v27,224(r6)
    vperm v11,v29,v30,v21
    vperm v13,v18,v27,v21
    lxv v29,272(r6)
    lxv v30,256(r6)
    lxv v10,304(r6)
    lxv v27,288(r6)
    vperm v16,v29,v30,v21
    vperm v24,v10,v27,v21
    lxv v29,320(r6)
    lxv v10,736(r6)
    lxv v27,336(r6)
    vperm v30,v29,v0,v21
    vsel v29,v10,v25,v27
    xvsubsp v30,v29,v30
    vmr v20,v30
    vmr v18,v30
    vmr v14,v30
    vmr v28,v30
    xvmaddmsp v30,v24,v16
    xvmaddmsp v28,v30,v13
    lxv v30,624(r6)
    xvmulsp v27,v12,v30
    vcmpeqfp v13,v27,v27
    lxv v30,352(r6)
    vcmpgtfp v16,v30,v27
    lxv v30,368(r6)
    vcmpgtfp v24,v27,v30
    lxv v30,672(r6)
    vcmpgtfp v12,v12,v30
    lxv v29,656(r6)
    vsel v30,v29,v0,v12
    xvaddsp v30,v30,v27
    vctsxs v19,v30,23
    lxv v30,528(r6)
    vrlq v10,v19,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v10,v10,v29
    vsel v8,v27,v29,v30
    lxv v29,416(r6)
    lxv v30,400(r6)
    lxv v10,448(r6)
    lxv v27,432(r6)
    vperm v17,v29,v30,v8
    vperm v7,v10,v27,v8
    lxv v29,480(r6)
    lxv v30,464(r6)
    lxv v10,576(r6)
    lxv v27,560(r6)
    vperm v9,v29,v30,v8
    vperm v27,v10,v27,v8
    lxv v29,736(r6)
    lxv v30,592(r6)
    vsel v10,v29,v19,v30
    vmr v29,v10
    vmr v30,v10
    xvmaddmsp v10,v27,v9
    xvmaddmsp v30,v10,v7
    xvmaddmsp v29,v30,v17
    lxv v30,608(r6)
    vand v27,v19,v30
    lxv v30,736(r6)
    vadduwm v30,v27,v30
    xvmulsp v27,v30,v29
    lxv v29,736(r6)
    lxv v30,688(r6)
    vsel v30,v30,v29,v12
    xvmulsp v27,v30,v27
    lxv v30,384(r6)
    vsel v30,v27,v30,v24
    vsel v29,v30,v0,v16
    lxv v30,752(r6)
    vsel v29,v30,v29,v13
    lxv v30,736(r6)
    xvaddsp v24,v30,v29
    lxv v30,704(r6)
    vcmpgtfp v13,v30,v24
    lxv v30,64(r6)
    lxv v10,736(r6)
    lxv v27,720(r6)
    vsel v12,v0,v30,v13
    vsel v30,v10,v27,v13
    xvmulsp v23,v30,v24
    vcmpgtfp v16,v0,v23
    lxv v30,144(r6)
    vslw v27,v23,v30
    lxv v29,144(r6)
    lxv v30,128(r6)
    vperm v29,v27,v29,v30
    lxv v30,112(r6)
    vadduwm v30,v29,v30
    vcfsx v10,v30,0
    lxv v30,528(r6)
    vrlq v13,v23,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v13,v13,v29
    vsel v19,v27,v29,v30
    lxv v29,96(r6)
    lxv v30,80(r6)
    vperm v30,v29,v30,v19
    xvaddsp v13,v10,v30
    lxv v29,176(r6)
    lxv v30,160(r6)
    lxv v10,208(r6)
    lxv v27,192(r6)
    vperm v24,v29,v30,v19
    vperm v17,v10,v27,v19
    lxv v29,240(r6)
    lxv v30,224(r6)
    lxv v10,272(r6)
    lxv v27,256(r6)
    vperm v7,v29,v30,v19
    vperm v9,v10,v27,v19
    lxv v29,304(r6)
    lxv v30,288(r6)
    lxv v10,320(r6)
    vperm v8,v29,v30,v19
    vperm v27,v10,v0,v19
    lxv v29,736(r6)
    lxv v30,336(r6)
    vsel v30,v29,v23,v30
    xvsubsp v19,v30,v27
    vmr v30,v19
    vmr v29,v19
    vmr v27,v19
    vmr v10,v19
    xvmaddmsp v19,v8,v9
    xvmaddmsp v10,v19,v7
    xvmaddmsp v27,v10,v17
    xvmaddmsp v29,v27,v24
    xvmaddmsp v30,v29,v13
    xvsubsp v12,v30,v12
    lxv v30,624(r6)
    xvmulsp v27,v31,v30
    vcmpeqfp v13,v27,v27
    lxv v30,352(r6)
    vcmpgtfp v24,v30,v27
    lxv v30,368(r6)
    vcmpgtfp v17,v27,v30
    lxv v30,672(r6)
    vcmpgtfp v31,v31,v30
    lxv v29,656(r6)
    vsel v30,v29,v0,v31
    xvaddsp v30,v30,v27
    vctsxs v7,v30,23
    lxv v30,528(r6)
    vrlq v10,v7,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v10,v10,v29
    vsel v6,v27,v29,v30
    lxv v29,416(r6)
    lxv v30,400(r6)
    lxv v10,448(r6)
    lxv v27,432(r6)
    vperm v19,v29,v30,v6
    vperm v9,v10,v27,v6
    lxv v29,480(r6)
    lxv v30,464(r6)
    lxv v10,576(r6)
    lxv v27,560(r6)
    vperm v8,v29,v30,v6
    vperm v27,v10,v27,v6
    lxv v29,736(r6)
    lxv v30,592(r6)
    vsel v10,v29,v7,v30
    vmr v29,v10
    vmr v30,v10
    xvmaddmsp v10,v27,v8
    xvmaddmsp v30,v10,v9
    xvmaddmsp v29,v30,v19
    lxv v30,608(r6)
    vand v27,v7,v30
    lxv v30,736(r6)
    vadduwm v30,v27,v30
    xvmulsp v27,v30,v29
    lxv v29,736(r6)
    lxv v30,688(r6)
    vsel v30,v30,v29,v31
    xvmulsp v27,v30,v27
    lxv v30,384(r6)
    vsel v30,v27,v30,v17
    vsel v29,v30,v0,v24
    lxv v30,752(r6)
    vsel v29,v30,v29,v13
    lxv v30,736(r6)
    xvaddsp v13,v30,v29
    lxv v30,704(r6)
    vcmpgtfp v31,v30,v13
    lxv v30,64(r6)
    lxv v10,736(r6)
    lxv v27,720(r6)
    vsel v24,v0,v30,v31
    vsel v30,v10,v27,v31
    xvmulsp v13,v30,v13
    vcmpeqfp v17,v13,v13
    vcmpeqfp v31,v13,v0
    lxv v30,48(r6)
    vcmpgtfp v19,v13,v30
    vcmpgtfp v7,v0,v13
    lxv v30,144(r6)
    vslw v27,v13,v30
    lxv v29,144(r6)
    lxv v30,128(r6)
    vperm v29,v27,v29,v30
    lxv v30,112(r6)
    vadduwm v30,v29,v30
    vcfsx v10,v30,0
    lxv v30,528(r6)
    vrlq v9,v13,v30
    lxv v29,512(r6)
    lxv v27,544(r6)
    lxv v30,496(r6)
    vperm v29,v9,v9,v29
    vsel v9,v27,v29,v30
    lxv v29,96(r6)
    lxv v30,80(r6)
    vperm v30,v29,v30,v9
    xvaddsp v8,v10,v30
    lxv v29,176(r6)
    lxv v30,160(r6)
    lxv v10,208(r6)
    lxv v27,192(r6)
    vperm v6,v29,v30,v9
    vperm v5,v10,v27,v9
    lxv v29,240(r6)
    lxv v30,224(r6)
    lxv v10,272(r6)
    lxv v27,256(r6)
    vperm v4,v29,v30,v9
    vperm v3,v10,v27,v9
    lxv v29,304(r6)
    lxv v30,288(r6)
    lxv v10,320(r6)
    vperm v2,v29,v30,v9
    vperm v27,v10,v0,v9
    lxv v29,736(r6)
    lxv v30,336(r6)
    vsel v30,v29,v13,v30
    xvsubsp v13,v30,v27
    vmr v30,v13
    vmr v29,v13
    vmr v27,v13
    vmr v10,v13
    xvmaddmsp v13,v2,v3
    xvmaddmsp v10,v13,v4
    xvmaddmsp v27,v10,v5
    xvmaddmsp v29,v27,v6
    xvmaddmsp v30,v29,v8
    xvsubsp v27,v30,v24
    lxv v29,752(r6)
    lxv v30,384(r6)
    vsel v29,v27,v29,v7
    vsel v27,v29,v30,v19
    lxv v30,608(r6)
    lxv v29,752(r6)
    vsel v30,v27,v30,v31
    vsel v13,v29,v30,v17
    lxv v30,32(r6)
    xvmulsp v27,v30,v13
    lxv v30,624(r6)
    xvmulsp v24,v26,v30
    vcmpeqfp v29,v24,v24
    lxv v30,352(r6)
    vcmpgtfp v10,v30,v24
    lxv v30,368(r6)
    vcmpgtfp v31,v24,v30
    lxv v30,672(r6)
    vcmpgtfp v7,v26,v30
    lxv v26,656(r6)
    vsel v30,v26,v0,v7
    xvaddsp v30,v30,v24
    vctsxs v8,v30,23
    lxv v30,528(r6)
    vrlq v24,v8,v30
    lxv v26,512(r6)
    lxv v9,544(r6)
    lxv v30,496(r6)
    vperm v26,v24,v24,v26
    vsel v17,v9,v26,v30
    lxv v26,416(r6)
    lxv v30,400(r6)
    lxv v9,448(r6)
    lxv v24,432(r6)
    vperm v26,v26,v30,v17
    vperm v9,v9,v24,v17
    lxv v24,480(r6)
    lxv v30,464(r6)
    lxv v5,576(r6)
    lxv v6,560(r6)
    vperm v19,v24,v30,v17
    vperm v17,v5,v6,v17
    lxv v24,592(r6)
    lxv v30,736(r6)
    vsel v6,v30,v8,v24
    vmr v30,v6
    vmr v24,v6
    xvmaddmsp v6,v17,v19
    xvmaddmsp v24,v6,v9
    xvmaddmsp v30,v24,v26
    lxv v26,608(r6)
    vand v26,v8,v26
    lxv v24,736(r6)
    vadduwm v26,v26,v24
    xvmulsp v30,v26,v30
    lxv v24,736(r6)
    lxv v26,688(r6)
    vsel v26,v26,v24,v7
    xvmulsp v24,v26,v30
    lxv v26,384(r6)
    vsel v26,v24,v26,v31
    vsel v30,v26,v0,v10
    lxv v10,752(r6)
    vsel v29,v10,v30,v29
    lxv v30,736(r6)
    xvaddsp v31,v30,v29
    lxv v30,704(r6)
    vcmpgtfp v26,v30,v31
    lxv v10,64(r6)
    lxv v24,720(r6)
    lxv v29,736(r6)
    vsel v7,v0,v10,v26
    vsel v30,v29,v24,v26
    xvmulsp v19,v30,v31
    vcmpeqfp v26,v19,v19
    vcmpeqfp v31,v19,v0
    lxv v30,48(r6)
    vcmpgtfp v24,v19,v30
    vcmpgtfp v17,v0,v19
    lxv v30,144(r6)
    vslw v10,v19,v30
    lxv v29,128(r6)
    lxv v30,144(r6)
    vperm v29,v10,v30,v29
    lxv v30,112(r6)
    vadduwm v30,v29,v30
    vcfsx v9,v30,0
    lxv v30,528(r6)
    vrlq v8,v19,v30
    lxv v29,512(r6)
    lxv v10,544(r6)
    lxv v30,496(r6)
    vperm v29,v8,v8,v29
    vsel v4,v10,v29,v30
    lxv v29,96(r6)
    lxv v30,80(r6)
    vperm v30,v29,v30,v4
    xvaddsp v29,v9,v30
    lxv v10,176(r6)
    lxv v30,160(r6)
    lxv v6,208(r6)
    lxv v8,192(r6)
    vperm v9,v10,v30,v4
    vperm v8,v6,v8,v4
    lxv v10,240(r6)
    lxv v30,224(r6)
    lxv v3,272(r6)
    lxv v6,256(r6)
    vperm v5,v10,v30,v4
    vperm v3,v3,v6,v4
    lxv v10,304(r6)
    lxv v30,288(r6)
    lxv v1,320(r6)
    vperm v2,v10,v30,v4
    vperm v10,v1,v0,v4
    lxv v6,336(r6)
    lxv v30,736(r6)
    vsel v30,v30,v19,v6
    xvsubsp v4,v30,v10
    vmr v30,v4
    vmr v10,v4
    vmr v19,v4
    vmr v6,v4
    xvmaddmsp v4,v2,v3
    xvmaddmsp v6,v4,v5
    xvmaddmsp v19,v6,v8
    xvmaddmsp v10,v19,v9
    xvmaddmsp v30,v10,v29
    xvsubsp v10,v30,v7
    lxv v29,752(r6)
    lxv v30,384(r6)
    vsel v29,v10,v29,v17
    vsel v10,v29,v30,v24
    lxv v29,608(r6)
    lxv v30,752(r6)
    vsel v29,v10,v29,v31
    vsel v29,v30,v29,v26
    lxv v30,32(r6)
    xvmulsp v30,v30,v29
    lxv v10,768(r6)
    xvmaddmsp v10,v29,v30
    stxv v10,0(r31)
SOFTPLUS_LOOPHEAD:
    xor r29,r29,r7
    xor r28,r28,r7
    sub r31,r5,r8
    cmp 0,r27,r31 ; bge SOFTPLUS_EPILOGUE0
SOFTPLUS_KERNEL:
    lxv v24,768(r6)
    lxv v31,752(r6)
    xvmaddmsp v24,v13,v27
    vsel v26,v12,v31,v16
    lxv v31,48(r6)
    vcmpgtfp v27,v23,v31
    lxv v30,176(r6)
    lxv v31,160(r6)
    xvmaddmsp v14,v28,v11
    vperm v28,v30,v31,v21
    lxv v30,96(r6)
    lxv v31,80(r6)
    lxv v29,384(r6)
    vperm v30,v30,v31,v21
    vsel v26,v26,v29,v27
    vcmpeqfp v27,v23,v0
    xvmaddmsp v18,v14,v28
    xvaddsp v30,v15,v30
    vcmpeqfp v29,v23,v23
    addc r31,r3,r27
    lxv v31,608(r6)
    stxv v24,16(r31)
    vsel v28,v26,v31,v27
    lxv v31,752(r6)
    xvmaddmsp v20,v18,v30
    vsel v26,v31,v28,v29
    xvsubsp v29,v20,v22
    vcmpgtfp v30,v0,v25
    lxv v31,32(r6)
    xvmulsp v27,v31,v26
    lxv v31,752(r6)
    lxv v28,768(r6)
    vsel v29,v29,v31,v30
    xvmaddmsp v28,v26,v27
    lxv v31,48(r6)
    vcmpgtfp v30,v25,v31
    lxv v31,384(r6)
    vsel v29,v29,v31,v30
    vcmpeqfp v30,v25,v0
    addc r31,r3,r27
    lxv v31,608(r6)
    stxv v28,32(r31)
    vsel v29,v29,v31,v30
    vcmpeqfp v30,v25,v25
    lxv v31,752(r6)
    vsel v29,v31,v29,v30
    lxv v31,32(r6)
    xvmulsp v30,v31,v29
    lxv v31,768(r6)
    xvmaddmsp v31,v29,v30
    addc r31,r4,r27
    lxv v29,64(r31)
    addc r31,r4,r27
    lxv v30,672(r6)
    vcmpgtfp v10,v29,v30
    lxv v27,80(r31)
    lxv v30,624(r6)
    xvmulsp v24,v29,v30
    lxv v29,656(r6)
    vsel v30,v29,v0,v10
    xvaddsp v29,v30,v24
    lxv v30,672(r6)
    vcmpgtfp v14,v27,v30
    vctsxs v15,v29,23
    lxv v29,656(r6)
    vsel v29,v29,v0,v14
    lxv v30,624(r6)
    xvmulsp v22,v27,v30
    addc r31,r4,r27
    lxv v30,528(r6)
    vrlq v27,v15,v30
    xvaddsp v30,v29,v22
    lxv v11,96(r31)
    lxv v29,512(r6)
    vperm v29,v27,v27,v29
    vctsxs v13,v30,23
    lxv v27,544(r6)
    lxv v30,496(r6)
    vsel v16,v27,v29,v30
    lxv v30,528(r6)
    vrlq v29,v13,v30
    lxv v30,672(r6)
    vcmpgtfp v20,v11,v30
    lxv v18,592(r6)
    lxv v30,736(r6)
    lxv v26,576(r6)
    lxv v27,560(r6)
    vsel v21,v30,v15,v18
    vperm v28,v26,v27,v16
    lxv v27,480(r6)
    lxv v30,464(r6)
    lxv v18,736(r6)
    lxv v26,688(r6)
    vperm v12,v27,v30,v16
    vsel v19,v26,v18,v10
    lxv v30,512(r6)
    vperm v18,v29,v29,v30
    lxv v30,608(r6)
    vand v9,v15,v30
    vmr v10,v21
    vmr v15,v21
    addc r31,r4,r27
    lxv v29,656(r6)
    vsel v26,v29,v0,v20
    lxv v30,624(r6)
    xvmulsp v27,v11,v30
    lxv v29,448(r6)
    lxv v30,432(r6)
    xvmaddmsp v21,v28,v12
    vperm v12,v29,v30,v16
    lxv v29,544(r6)
    lxv v30,496(r6)
    vsel v17,v29,v18,v30
    lxv v25,112(r31)
    xvaddsp v18,v26,v27
    lxv v29,416(r6)
    lxv v30,400(r6)
    vperm v7,v29,v30,v16
    xvmaddmsp v10,v21,v12
    lxv v30,736(r6)
    vadduwm v12,v9,v30
    lxv v26,736(r6)
    lxv v29,688(r6)
    lxv v28,736(r6)
    lxv v30,592(r6)
    vsel v21,v29,v26,v14
    vsel v9,v28,v13,v30
    lxv v28,576(r6)
    lxv v26,560(r6)
    lxv v29,480(r6)
    lxv v30,464(r6)
    vperm v26,v28,v26,v17
    vperm v29,v29,v30,v17
    vmr v11,v9
    vmr v16,v9
    lxv v30,672(r6)
    vcmpgtfp v28,v25,v30
    vctsxs v14,v18,23
    xvmaddmsp v15,v10,v7
    xvmaddmsp v9,v26,v29
    lxv v29,448(r6)
    lxv v30,432(r6)
    vperm v10,v29,v30,v17
    lxv v30,528(r6)
    vrlq v18,v14,v30
    lxv v30,608(r6)
    vand v13,v13,v30
    lxv v30,624(r6)
    xvmulsp v25,v25,v30
    lxv v29,656(r6)
    vsel v26,v29,v0,v28
    xvmulsp v7,v12,v15
    lxv v29,416(r6)
    lxv v30,400(r6)
    vperm v15,v29,v30,v17
    lxv v30,736(r6)
    vadduwm v12,v13,v30
    xvmaddmsp v11,v9,v10
    xvaddsp v26,v26,v25
    lxv v30,512(r6)
    vperm v18,v18,v18,v30
    xvmulsp v17,v19,v7
    lxv v30,368(r6)
    vcmpgtfp v10,v24,v30
    lxv v29,544(r6)
    lxv v30,496(r6)
    xvmaddmsp v16,v11,v15
    vsel v13,v29,v18,v30
    vctsxs v19,v26,23
    lxv v30,384(r6)
    vsel v17,v17,v30,v10
    lxv v30,352(r6)
    vcmpgtfp v10,v30,v24
    vcmpeqfp v15,v24,v24
    lxv v29,736(r6)
    lxv v30,688(r6)
    vsel v24,v30,v29,v20
    xvmulsp v12,v12,v16
    lxv v30,528(r6)
    vrlq v18,v19,v30
    lxv v20,736(r6)
    lxv v26,592(r6)
    lxv v29,576(r6)
    lxv v30,560(r6)
    vsel v11,v20,v14,v26
    vperm v16,v29,v30,v13
    lxv v29,480(r6)
    lxv v30,464(r6)
    vperm v20,v29,v30,v13
    vsel v7,v17,v0,v10
    xvmulsp v9,v21,v12
    lxv v30,368(r6)
    vcmpgtfp v10,v22,v30
    vmr v12,v11
    vmr v21,v11
    lxv v29,448(r6)
    lxv v30,432(r6)
    xvmaddmsp v11,v16,v20
    vperm v17,v29,v30,v13
    lxv v30,608(r6)
    vand v14,v14,v30
    lxv v30,512(r6)
    lxv v29,752(r6)
    vperm v26,v18,v18,v30
    vsel v7,v29,v7,v15
    lxv v29,416(r6)
    lxv v30,400(r6)
    vperm v20,v29,v30,v13
    lxv v30,736(r6)
    vadduwm v16,v14,v30
    lxv v30,384(r6)
    vsel v10,v9,v30,v10
    lxv v30,352(r6)
    vcmpgtfp v18,v30,v22
    vcmpeqfp v13,v22,v22
    lxv v29,544(r6)
    lxv v30,496(r6)
    vsel v15,v29,v26,v30
    xvmaddmsp v12,v11,v17
    lxv v30,736(r6)
    xvaddsp v14,v30,v7
    vsel v11,v10,v0,v18
    xvmaddmsp v21,v12,v20
    lxv v22,736(r6)
    lxv v26,592(r6)
    lxv v29,576(r6)
    lxv v30,560(r6)
    vsel v17,v22,v19,v26
    vperm v18,v29,v30,v15
    lxv v29,480(r6)
    lxv v30,464(r6)
    vperm v26,v29,v30,v15
    lxv v30,704(r6)
    vcmpgtfp v22,v30,v14
    lxv v29,736(r6)
    lxv v30,688(r6)
    vsel v23,v30,v29,v28
    lxv v30,608(r6)
    vand v19,v19,v30
    lxv v30,752(r6)
    vsel v13,v30,v11,v13
    vmr v20,v17
    vmr v28,v17
    lxv v29,448(r6)
    lxv v30,432(r6)
    xvmaddmsp v17,v18,v26
    vperm v26,v29,v30,v15
    xvmulsp v29,v16,v21
    lxv v18,720(r6)
    lxv v30,736(r6)
    vsel v10,v30,v18,v22
    lxv v30,736(r6)
    xvaddsp v21,v30,v13
    xvmulsp v16,v24,v29
    lxv v30,368(r6)
    vcmpgtfp v18,v27,v30
    lxv v29,416(r6)
    lxv v30,400(r6)
    vperm v29,v29,v30,v15
    lxv v30,736(r6)
    vadduwm v24,v19,v30
    xvmaddmsp v20,v17,v26
    xvmulsp v26,v10,v14
    lxv v30,704(r6)
    vcmpgtfp v19,v30,v21
    lxv v30,384(r6)
    xvmaddmsp v28,v20,v29
    vsel v18,v16,v30,v18
    lxv v30,352(r6)
    vcmpgtfp v20,v30,v27
    lxv v30,528(r6)
    vrlq v10,v26,v30
    lxv v29,736(r6)
    lxv v30,720(r6)
    vsel v15,v29,v30,v19
    vcmpeqfp v14,v27,v27
    vsel v18,v18,v0,v20
    xvmulsp v29,v24,v28
    lxv v30,512(r6)
    vperm v10,v10,v10,v30
    xvmulsp v28,v15,v21
    xvmulsp v20,v23,v29
    lxv v27,752(r6)
    lxv v29,544(r6)
    lxv v30,496(r6)
    vsel v27,v27,v18,v14
    vsel v24,v29,v10,v30
    lxv v30,528(r6)
    vrlq v18,v28,v30
    lxv v30,368(r6)
    vcmpgtfp v29,v25,v30
    lxv v30,736(r6)
    xvaddsp v15,v30,v27
    lxv v30,384(r6)
    vsel v21,v20,v30,v29
    lxv v30,352(r6)
    vcmpgtfp v14,v30,v25
    lxv v29,64(r6)
    vsel v22,v0,v29,v22
    vcmpeqfp v20,v25,v25
    lxv v10,336(r6)
    lxv v30,736(r6)
    lxv v27,320(r6)
    vsel v10,v30,v26,v10
    vperm v29,v27,v0,v24
    lxv v30,512(r6)
    vperm v18,v18,v18,v30
    lxv v30,704(r6)
    vcmpgtfp v13,v30,v15
    xvsubsp v16,v10,v29
    lxv v10,304(r6)
    lxv v27,288(r6)
    lxv v29,272(r6)
    lxv v30,256(r6)
    vperm v12,v10,v27,v24
    vperm v9,v29,v30,v24
    lxv v30,144(r6)
    vslw v10,v26,v30
    lxv v29,544(r6)
    lxv v30,496(r6)
    vsel v17,v29,v18,v30
    vsel v14,v21,v0,v14
    lxv v25,736(r6)
    lxv v27,720(r6)
    lxv v30,64(r6)
    vsel v18,v25,v27,v13
    vsel v21,v0,v30,v19
    lxv v27,128(r6)
    lxv v30,144(r6)
    lxv v29,752(r6)
    vperm v7,v10,v30,v27
    vsel v20,v29,v14,v20
    vmr v10,v16
    vmr v11,v16
    vmr v14,v16
    vmr v19,v16
    lxv v25,736(r6)
    lxv v27,336(r6)
    lxv v29,320(r6)
    vsel v27,v25,v28,v27
    vperm v30,v29,v0,v17
    lxv v25,240(r6)
    lxv v29,224(r6)
    xvmaddmsp v16,v12,v9
    vperm v12,v25,v29,v24
    xvmulsp v23,v18,v15
    lxv v18,208(r6)
    lxv v29,192(r6)
    xvmaddmsp v10,v16,v12
    vperm v5,v18,v29,v24
    lxv v29,112(r6)
    vadduwm v18,v7,v29
    xvsubsp v6,v27,v30
    lxv v25,304(r6)
    lxv v27,288(r6)
    lxv v29,272(r6)
    lxv v30,256(r6)
    vperm v4,v25,v27,v17
    vperm v7,v29,v30,v17
    lxv v30,144(r6)
    vslw v27,v28,v30
    lxv v30,736(r6)
    xvaddsp v25,v30,v20
    lxv v30,528(r6)
    vrlq v20,v23,v30
    lxv v29,144(r6)
    lxv v30,128(r6)
    vperm v9,v27,v29,v30
    vcfsx v3,v18,0
    lxv v30,704(r6)
    vcmpgtfp v18,v30,v25
    vmr v8,v6
    vmr v12,v6
    vmr v15,v6
    vmr v16,v6
    lxv v29,176(r6)
    lxv v30,160(r6)
    xvmaddmsp v11,v10,v5
    vperm v5,v29,v30,v24
    lxv v29,96(r6)
    lxv v30,80(r6)
    vperm v10,v29,v30,v24
    xvmaddmsp v6,v4,v7
    lxv v29,240(r6)
    lxv v30,224(r6)
    lxv v27,512(r6)
    vperm v24,v29,v30,v17
    vperm v20,v20,v20,v27
    xvmaddmsp v14,v11,v5
    xvaddsp v7,v3,v10
    lxv v29,208(r6)
    lxv v30,192(r6)
    xvmaddmsp v8,v6,v24
    vperm v10,v29,v30,v17
    lxv v30,112(r6)
    vadduwm v11,v9,v30
    lxv v29,544(r6)
    lxv v30,496(r6)
    lxv v24,736(r6)
    lxv v27,720(r6)
    vsel v20,v29,v20,v30
    vsel v30,v24,v27,v18
    vcfsx v11,v11,0
    xvmulsp v25,v30,v25
    lxv v30,64(r6)
    vsel v24,v0,v30,v13
    xvmaddmsp v19,v14,v7
    lxv v29,176(r6)
    lxv v30,160(r6)
    xvmaddmsp v12,v8,v10
    vperm v10,v29,v30,v17
    lxv v29,96(r6)
    lxv v30,80(r6)
    lxv v14,736(r6)
    lxv v27,336(r6)
    vperm v13,v29,v30,v17
    vsel v27,v14,v23,v27
    lxv v29,320(r6)
    vperm v29,v29,v0,v20
    xvsubsp v17,v19,v22
    vcmpgtfp v9,v0,v26
    xvmaddmsp v15,v12,v10
    xvaddsp v19,v11,v13
    xvsubsp v10,v27,v29
    lxv v22,304(r6)
    lxv v27,288(r6)
    lxv v29,272(r6)
    lxv v30,256(r6)
    vperm v11,v22,v27,v20
    vperm v12,v29,v30,v20
    lxv v30,144(r6)
    vslw v14,v23,v30
    lxv v30,528(r6)
    vrlq v22,v25,v30
    lxv v29,144(r6)
    lxv v30,128(r6)
    lxv v27,512(r6)
    vperm v13,v14,v29,v30
    vperm v27,v22,v22,v27
    lxv v30,752(r6)
    vsel v22,v17,v30,v9
    lxv v30,48(r6)
    vcmpgtfp v29,v26,v30
    xvmaddmsp v16,v15,v19
    vmr v15,v10
    vmr v14,v10
    vmr v17,v10
    vmr v19,v10
    lxv v30,384(r6)
    vsel v9,v22,v30,v29
    vcmpeqfp v7,v26,v0
    xvsubsp v6,v16,v21
    vcmpgtfp v8,v0,v28
    lxv v29,240(r6)
    lxv v30,224(r6)
    xvmaddmsp v10,v11,v12
    vperm v22,v29,v30,v20
    lxv v29,208(r6)
    lxv v30,192(r6)
    xvmaddmsp v15,v10,v22
    vperm v12,v29,v30,v20
    lxv v30,112(r6)
    vadduwm v22,v13,v30
    lxv v29,544(r6)
    lxv v30,496(r6)
    vsel v21,v29,v27,v30
    vcmpeqfp v13,v26,v26
    vcfsx v16,v22,0
    lxv v30,64(r6)
    lxv v27,608(r6)
    vsel v22,v0,v30,v18
    vsel v18,v9,v27,v7
    lxv v30,752(r6)
    vsel v26,v6,v30,v8
    lxv v30,48(r6)
    vcmpgtfp v27,v28,v30
    lxv v29,752(r6)
    lxv v30,384(r6)
    vsel v18,v29,v18,v13
    vsel v13,v26,v30,v27
    vcmpeqfp v11,v28,v0
    lxv v26,736(r6)
    lxv v27,336(r6)
    lxv v29,320(r6)
    vsel v26,v26,v25,v27
    vperm v27,v29,v0,v21
    lxv v29,176(r6)
    lxv v30,160(r6)
    xvmaddmsp v14,v15,v12
    vperm v15,v29,v30,v20
    lxv v29,96(r6)
    lxv v30,80(r6)
    vperm v30,v29,v30,v20
    xvmaddmsp v17,v14,v15
    xvaddsp v12,v16,v30
    xvsubsp v15,v26,v27
    lxv v26,304(r6)
    lxv v27,288(r6)
    lxv v29,272(r6)
    lxv v30,256(r6)
    vperm v16,v26,v27,v21
    vperm v26,v29,v30,v21
    lxv v30,144(r6)
    vslw v27,v25,v30
    vcmpeqfp v20,v28,v28
    lxv v29,144(r6)
    lxv v30,128(r6)
    vperm v27,v27,v29,v30
    lxv v30,32(r6)
    xvmulsp v14,v30,v18
    lxv v30,608(r6)
    lxv v10,768(r6)
    vsel v29,v13,v30,v11
    xvmaddmsp v10,v18,v14
    lxv v30,752(r6)
    vsel v13,v30,v29,v20
    vmr v28,v15
    vmr v14,v15
    vmr v18,v15
    vmr v20,v15
    xvmaddmsp v19,v17,v12
    xvmaddmsp v15,v16,v26
    lxv v29,240(r6)
    lxv v30,224(r6)
    vperm v26,v29,v30,v21
    xvsubsp v12,v19,v24
    vcmpgtfp v16,v0,v23
    lxv v29,208(r6)
    lxv v30,192(r6)
    xvmaddmsp v28,v15,v26
    vperm v11,v29,v30,v21
    lxv v30,112(r6)
    vadduwm v30,v27,v30
    vcfsx v15,v30,0
    addc r31,r3,r27
    addc r30,r3,r27
    stxv v31,48(r31)
    stxv v10,64(r30)
    lxv v31,32(r6)
    xvmulsp v27,v31,v13
SOFTPLUS_LOOPEND:
    addc r27,r27,r8
    b SOFTPLUS_LOOPHEAD
SOFTPLUS_EPILOGUE0:
    lxv v29,96(r6)
    lxv v30,80(r6)
    xvmaddmsp v14,v28,v11
    vperm v30,v29,v30,v21
    xvaddsp v26,v15,v30
    lxv v29,176(r6)
    lxv v30,160(r6)
    vperm v30,v29,v30,v21
    xvmaddmsp v18,v14,v30
    xvmaddmsp v20,v18,v26
    xvsubsp v20,v20,v22
    vcmpeqfp v26,v25,v25
    vcmpeqfp v18,v25,v0
    lxv v30,48(r6)
    vcmpgtfp v10,v25,v30
    vcmpgtfp v31,v0,v25
    lxv v29,752(r6)
    lxv v30,384(r6)
    vsel v29,v20,v29,v31
    vsel v10,v29,v30,v10
    lxv v30,608(r6)
    lxv v29,752(r6)
    vsel v30,v10,v30,v18
    vsel v18,v29,v30,v26
    lxv v30,32(r6)
    xvmulsp v29,v30,v18
    lxv v26,768(r6)
    lxv v30,752(r6)
    xvmaddmsp v26,v18,v29
    vsel v20,v12,v30,v16
    vcmpeqfp v18,v23,v23
    vcmpeqfp v10,v23,v0
    lxv v30,48(r6)
    vcmpgtfp v31,v23,v30
    lxv v29,384(r6)
    lxv v30,608(r6)
    vsel v29,v20,v29,v31
    vsel v29,v29,v30,v10
    lxv v30,752(r6)
    vsel v18,v30,v29,v18
    lxv v30,32(r6)
    xvmulsp v29,v30,v18
    lxv v30,768(r6)
    lxv v10,768(r6)
    xvmaddmsp v30,v18,v29
    xvmaddmsp v10,v13,v27
    addc r31,r3,r27
    stxv v10,16(r31)
    addc r31,r3,r27
    stxv v30,32(r31)
    addc r31,r3,r27
    stxv v26,48(r31)
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
    .vbyte 4, 0x33851592
    .vbyte 4, 0x33851592
    .vbyte 4, 0x33851592
    .vbyte 4, 0x33851592
    .vbyte 4, 0x4eff0000
    .vbyte 4, 0x4eff0000
    .vbyte 4, 0x4eff0000
    .vbyte 4, 0x4eff0000
    .vbyte 4, 0x42000000
    .vbyte 4, 0x42000000
    .vbyte 4, 0x42000000
    .vbyte 4, 0x42000000
    .vbyte 4, 0xc050ae00
    .vbyte 4, 0x0000d33a
    .vbyte 4, 0x1b06d100
    .vbyte 4, 0x00d2c3a1
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0x003e3e3e
    .vbyte 4, 0x15334e80
    .vbyte 4, 0x002ea4eb
    .vbyte 4, 0xffffff81
    .vbyte 4, 0xffffff81
    .vbyte 4, 0xffffff81
    .vbyte 4, 0xffffff81
    .vbyte 4, 0x10101004
    .vbyte 4, 0x10101000
    .vbyte 4, 0x1010100c
    .vbyte 4, 0x10101008
    .vbyte 4, 0x00000001
    .vbyte 4, 0x00000001
    .vbyte 4, 0x00000001
    .vbyte 4, 0x00000001
    .vbyte 4, 0x38470baa
    .vbyte 4, 0xaa25bb4d
    .vbyte 4, 0x51adb33d
    .vbyte 4, 0x3c8b6343
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0x76635338
    .vbyte 4, 0xb8a49386
    .vbyte 4, 0x25dd31aa
    .vbyte 4, 0xa9e75e58
    .vbyte 4, 0x5a38c82b
    .vbyte 4, 0xd5fb79cb
    .vbyte 4, 0xbebebebe
    .vbyte 4, 0xbfbfbebe
    .vbyte 4, 0xa48b7138
    .vbyte 4, 0x3811ecc3
    .vbyte 4, 0xda73b943
    .vbyte 4, 0x0ac8f856
    .vbyte 4, 0x86f426b0
    .vbyte 4, 0xe0c9f58e
    .vbyte 4, 0x3e3d3d3d
    .vbyte 4, 0x3e3e3e3e
    .vbyte 4, 0x11e5b776
    .vbyte 4, 0xf6ac7b3d
    .vbyte 4, 0x1c9c0d18
    .vbyte 4, 0xecc9aea9
    .vbyte 4, 0x0a99d0de
    .vbyte 4, 0xb5f311c9
    .vbyte 4, 0xbdbdbdbc
    .vbyte 4, 0xbebebebd
    .vbyte 4, 0x90511cb7
    .vbyte 4, 0xb46114cb
    .vbyte 4, 0xd7fd4889
    .vbyte 4, 0x71223f44
    .vbyte 4, 0x227e7489
    .vbyte 4, 0x1674f101
    .vbyte 4, 0x3c3c3c3c
    .vbyte 4, 0x3e3d3d3d
    .vbyte 4, 0xffad732b
    .vbyte 4, 0x63fd9942
    .vbyte 4, 0x3f3f3f40
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0xc0d0e000
    .vbyte 4, 0x8090a0b0
    .vbyte 4, 0x007fffff
    .vbyte 4, 0x007fffff
    .vbyte 4, 0x007fffff
    .vbyte 4, 0x007fffff
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
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x01000000
    .vbyte 4, 0x01000000
    .vbyte 4, 0x01000000
    .vbyte 4, 0x01000000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x4f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x3f317219
    .vbyte 4, 0x3f317219
    .vbyte 4, 0x3f317219
    .vbyte 4, 0x3f317219
    .toc
L..constants:
    .tc constants[TC],constants[RW]
