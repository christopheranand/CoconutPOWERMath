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
.set v30,30; .set v31,31; .set v32,32; .set v33,33; .set v34,34
.set v35,35; .set v36,36; .set v37,37; .set v38,38; .set v39,39
.set v40,40; .set v41,41; .set v42,42; .set v43,43; .set v44,44
.set v45,45; .set v46,46; .set v47,47; .set v48,48; .set v49,49
.set v50,50; .set v51,51; .set v52,52; .set v53,53; .set v54,54
.set v55,55; .set v56,56; .set v57,57; .set v58,58; .set v59,59
.set v60,60; .set v61,61; .set v62,62; .set v63,63
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
    lwz          r5,0(r5)
    mulli      r5,r5,4
    lwz r6,L..constants(RTOC)
SIGMOID_INIT:
    xor r28,r5,r5
    xor r29,r28,r28
    ld r30,16(r6)
    cmpi 0,r5,0 ; ble RETURN
SIGMOID_PROLOGUE0:
    addc r31,r4,r28
    lxv v50,0(r31)
    addc r31,r4,r28
    lxv v55,16(r31)
    addc r31,r4,r28
    lxv v54,32(r31)
    addc r31,r4,r28
    lxv v61,48(r31)
    lxv v60,448(r6)
    vxor v29,v29,v28
    lxv v60,384(r6)
    xvmulsp v52,v61,v60
    lxv v60,432(r6)
    vcmpgtfp v14,v29,v28
    lxv v61,416(r6)
    lxv v60,400(r6)
    vsel v28,v29,v28,v14
    xvaddsp v60,v60,v52
    vctsxs v15,v28,23
    lxv v60,288(r6)
    vrlq v21,v15,v28
    lxv v61,272(r6)
    lxv v59,304(r6)
    lxv v60,256(r6)
    vperm v29,v21,v21,v29
    vsel v12,v27,v29,v28
    lxv v61,240(r6)
    lxv v60,224(r6)
    lxv v53,336(r6)
    lxv v59,320(r6)
    vperm v9,v29,v28,v12
    vperm v10,v21,v27,v12
    lxv v61,496(r6)
    lxv v60,352(r6)
    lxv v53,496(r6)
    lxv v59,464(r6)
    vsel v11,v29,v15,v28
    vsel v13,v27,v21,v14
    lxv v60,448(r6)
    vxor v29,v22,v28
    lxv v60,384(r6)
    xvmulsp v54,v61,v60
    lxv v60,128(r6)
    vcmpgtfp v17,v22,v28
    lxv v60,432(r6)
    vcmpgtfp v14,v29,v28
    lxv v61,416(r6)
    lxv v60,400(r6)
    vsel v28,v29,v28,v14
    xvaddsp v60,v60,v54
    vctsxs v26,v28,23
    lxv v60,288(r6)
    vrlq v21,v26,v28
    lxv v61,272(r6)
    lxv v59,304(r6)
    lxv v60,256(r6)
    vperm v29,v21,v21,v29
    vsel v19,v27,v29,v28
    lxv v61,176(r6)
    lxv v60,160(r6)
    lxv v53,208(r6)
    lxv v59,192(r6)
    vperm v16,v29,v28,v19
    vperm v24,v21,v27,v19
    lxv v61,240(r6)
    lxv v60,224(r6)
    lxv v53,336(r6)
    lxv v59,320(r6)
    vperm v25,v29,v28,v19
    vperm v27,v21,v27,v19
    lxv v61,496(r6)
    lxv v60,352(r6)
    vsel v28,v29,v26,v28
    xxlor v53,v60,v60
    xxlor v61,v60,v60
    xvmaddmsp v60,v59,v57
    xvmaddmsp v61,v60,v56
    xvmaddmsp v53,v61,v48
    lxv v60,368(r6)
    vand v29,v26,v28
    lxv v60,496(r6)
    vadduwm v28,v29,v28
    xvmulsp v59,v60,v53
    lxv v61,496(r6)
    lxv v60,464(r6)
    vsel v28,v28,v29,v14
    xvmulsp v46,v60,v59
    lxv v60,448(r6)
    vxor v29,v23,v28
    lxv v60,384(r6)
    xvmulsp v59,v61,v60
    vcmpeqfp v23,v27,v27
    lxv v60,112(r6)
    vcmpgtfp v16,v28,v27
    lxv v60,128(r6)
    vcmpgtfp v24,v27,v28
    lxv v60,432(r6)
    vcmpgtfp v25,v29,v28
    lxv v61,416(r6)
    lxv v60,400(r6)
    vsel v28,v29,v28,v25
    xvaddsp v60,v60,v59
    vctsxs v26,v28,23
    lxv v60,288(r6)
    vrlq v21,v26,v28
    lxv v61,272(r6)
    lxv v59,304(r6)
    lxv v60,256(r6)
    vperm v29,v21,v21,v29
    vsel v6,v27,v29,v28
    lxv v61,176(r6)
    lxv v60,160(r6)
    lxv v53,208(r6)
    lxv v59,192(r6)
    vperm v19,v29,v28,v6
    vperm v8,v21,v27,v6
    lxv v61,240(r6)
    lxv v60,224(r6)
    lxv v53,336(r6)
    lxv v59,320(r6)
    vperm v7,v29,v28,v6
    vperm v27,v21,v27,v6
    lxv v61,496(r6)
    lxv v60,352(r6)
    vsel v28,v29,v26,v28
    xxlor v53,v60,v60
    xxlor v61,v60,v60
    xvmaddmsp v60,v59,v39
    xvmaddmsp v61,v60,v40
    xvmaddmsp v53,v61,v51
    lxv v60,368(r6)
    vand v29,v26,v28
    lxv v60,496(r6)
    vadduwm v28,v29,v28
    xvmulsp v59,v60,v53
    lxv v61,496(r6)
    lxv v60,464(r6)
    vsel v28,v28,v29,v25
    xvmulsp v59,v60,v59
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v27,v28,v24
    vsel v29,v28,v29,v16
    lxv v60,480(r6)
    vsel v28,v28,v29,v23
    lxv v61,496(r6)
    xvaddsp v58,v61,v60
    lxv v60,64(r6)
    vand v16,v26,v28
    lxv v60,32(r6)
    vcmpgtfp v23,v16,v28
    lxv v60,448(r6)
    vxor v24,v18,v28
    lxv v60,384(r6)
    xvmulsp v50,v56,v60
    vcmpeqfp v29,v18,v18
    lxv v60,112(r6)
    vcmpgtfp v27,v28,v18
    lxv v60,128(r6)
    vcmpgtfp v21,v18,v28
    lxv v60,432(r6)
    vcmpgtfp v8,v24,v28
    lxv v56,416(r6)
    lxv v60,400(r6)
    vsel v28,v24,v28,v8
    xvaddsp v60,v60,v50
    vctsxs v7,v28,23
    lxv v60,288(r6)
    vrlq v24,v7,v28
    lxv v50,272(r6)
    lxv v57,304(r6)
    lxv v60,256(r6)
    vperm v18,v24,v24,v18
    vsel v19,v25,v18,v28
    lxv v50,176(r6)
    lxv v60,160(r6)
    lxv v57,208(r6)
    lxv v56,192(r6)
    vperm v18,v18,v28,v19
    vperm v24,v25,v24,v19
    lxv v57,240(r6)
    lxv v60,224(r6)
    lxv v37,336(r6)
    lxv v38,320(r6)
    vperm v25,v25,v28,v19
    vperm v19,v5,v6,v19
    lxv v38,352(r6)
    lxv v60,496(r6)
    vsel v28,v28,v7,v6
    xxlor v38,v60,v60
    xxlor v37,v60,v60
    xvmaddmsp v60,v51,v57
    xvmaddmsp v37,v60,v56
    xvmaddmsp v38,v37,v50
    lxv v60,368(r6)
    vand v18,v7,v28
    lxv v60,496(r6)
    vadduwm v28,v18,v28
    xvmulsp v56,v60,v38
    lxv v50,496(r6)
    lxv v60,464(r6)
    vsel v28,v28,v18,v8
    xvmulsp v56,v60,v56
    lxv v50,144(r6)
    lxv v60,400(r6)
    vsel v21,v24,v18,v21
    vsel v28,v21,v28,v27
    lxv v59,480(r6)
    vsel v28,v27,v28,v29
    lxv v61,496(r6)
    xvaddsp v57,v61,v60
    lxv v60,64(r6)
    vand v19,v25,v28
    lxv v60,496(r6)
    vcmpgtfp v28,v19,v28
    lxv v59,96(r6)
    lxv v61,80(r6)
    vsel v18,v27,v29,v28
SIGMOID_LOOPHEAD:
    ld r31,16(r6)
    xor r30,r30,r31
    ld r31,16(r6)
    xor r29,r29,r31
    ld r31,0(r6)
    sub r31,r5,r31
    cmp 0,r28,r31 ; bge SIGMOID_EPILOGUE0
SIGMOID_KERNEL:
    lxv v61,368(r6)
    vand v24,v15,v29
    xxlor v40,v43,v43
    xxlor v38,v43,v43
    lxv v60,208(r6)
    lxv v61,192(r6)
    xvmaddmsp v43,v42,v41
    vperm v27,v28,v29,v12
    lxv v61,144(r6)
    vsel v15,v14,v29,v17
    lxv v61,112(r6)
    vcmpgtfp v17,v29,v22
    lxv v61,496(r6)
    vcmpgtfp v14,v16,v29
    vcmpeqfp v21,v22,v22
    lxv v60,176(r6)
    lxv v61,160(r6)
    vperm v22,v28,v29,v12
    lxv v61,496(r6)
    vadduwm v24,v24,v29
    xvmaddmsp v40,v43,v59
    xvmulsp v44,v50,v51
    lxv v59,400(r6)
    lxv v60,96(r6)
    lxv v61,80(r6)
    vsel v27,v15,v27,v17
    vsel v14,v28,v29,v14
    vrefp v28,v12
    lxv v61,480(r6)
    xvmaddmsp v38,v40,v54
    vsel v29,v29,v27,v21
    xvmulsp v47,v46,v48
    xvmulsp v59,v56,v38
    lxv v53,496(r6)
    xvnmsubmsp v44,v60,v53
    vrefp v22,v15
    xvmaddmsp v44,v60,v60
    lxv v60,496(r6)
    xvaddsp v56,v60,v61
    xvmulsp v59,v45,v59
    lxv v61,64(r6)
    vand v17,v24,v29
    lxv v60,496(r6)
    xvnmsubmsp v47,v54,v60
    lxv v61,128(r6)
    vcmpgtfp v28,v20,v29
    lxv v61,48(r6)
    vcmpgefp v16,v29,v16
    lxv v61,144(r6)
    vsel v21,v27,v29,v28
    xvmulsp v59,v50,v44
    lxv v61,48(r6)
    vcmpgefp v28,v29,v19
    xvmaddmsp v47,v54,v54
    lxv v61,32(r6)
    vcmpgtfp v13,v19,v29
    lxv v61,144(r6)
    vsel v12,v27,v29,v28
    lxv v61,112(r6)
    vcmpgtfp v22,v29,v20
    lxv v61,496(r6)
    vcmpgtfp v27,v17,v29
    lxv v60,96(r6)
    lxv v61,80(r6)
    vsel v19,v28,v29,v27
    xvmulsp v50,v46,v47
    vcmpeqfp v27,v20,v20
    lxv v60,400(r6)
    lxv v61,400(r6)
    vsel v22,v21,v28,v22
    vsel v15,v12,v29,v13
    vcmpeqfp v21,v25,v25
    lxv v61,144(r6)
    lxv v60,480(r6)
    vsel v20,v18,v29,v16
    vsel v16,v28,v15,v21
    lxv v61,480(r6)
    vsel v27,v29,v22,v27
    xvmulsp v53,v51,v49
    vrefp v22,v21
    lxv v61,400(r6)
    vsel v20,v20,v29,v23
    vcmpeqfp v23,v26,v26
    addc r31,r3,r28
    lxv v61,64(r6)
    lxv v60,480(r6)
    vsel v29,v25,v16,v29
    vsel v28,v28,v20,v23
    lxv v46,496(r6)
    stxv v61,0(r31)
    xvnmsubmsp v53,v54,v46
    xvmaddmsp v53,v54,v54
    lxv v61,496(r6)
    xvaddsp v55,v61,v59
    lxv v61,64(r6)
    vsel v29,v26,v28,v29
    addc r31,r3,r28
    stxv v61,16(r31)
    xvmulsp v54,v51,v53
    lxv v61,48(r6)
    vcmpgefp v27,v29,v17
    lxv v61,32(r6)
    vcmpgtfp v28,v17,v29
    lxv v61,144(r6)
    vsel v27,v22,v29,v27
    lxv v61,64(r6)
    vand v25,v23,v29
    lxv v61,400(r6)
    vsel v27,v27,v29,v28
    vcmpeqfp v28,v24,v24
    lxv v61,480(r6)
    vsel v22,v29,v27,v28
    lxv v61,496(r6)
    vcmpgtfp v27,v25,v29
    lxv v60,96(r6)
    lxv v61,80(r6)
    vsel v27,v28,v29,v27
    addc r31,r3,r28
    lxv v61,64(r6)
    vsel v29,v24,v22,v29
    stxv v61,32(r31)
    xvmulsp v60,v59,v57
    vrefp v29,v28
    lxv v53,496(r6)
    xvnmsubmsp v60,v61,v53
    xvmaddmsp v60,v61,v61
    xvmulsp v58,v59,v60
    lxv v61,48(r6)
    vcmpgefp v27,v29,v25
    lxv v61,32(r6)
    vcmpgtfp v28,v25,v29
    lxv v61,144(r6)
    vsel v27,v26,v29,v27
    addc r31,r4,r28
    lxv v53,64(r31)
    lxv v61,400(r6)
    vsel v27,v27,v29,v28
    vcmpeqfp v28,v23,v23
    lxv v61,480(r6)
    vsel v28,v29,v27,v28
    addc r31,r4,r28
    lxv v61,448(r6)
    vxor v27,v21,v29
    lxv v53,80(r31)
    lxv v61,64(r6)
    vsel v29,v23,v28,v29
    lxv v60,432(r6)
    vcmpgtfp v22,v27,v28
    lxv v60,384(r6)
    xvmulsp v48,v59,v60
    lxv v60,448(r6)
    vxor v27,v21,v28
    lxv v53,416(r6)
    lxv v60,400(r6)
    vsel v21,v21,v28,v22
    lxv v60,432(r6)
    vcmpgtfp v12,v27,v28
    xvaddsp v53,v53,v48
    lxv v60,384(r6)
    xvmulsp v56,v59,v60
    addc r31,r4,r28
    lxv v46,96(r31)
    lxv v59,416(r6)
    lxv v60,400(r6)
    vsel v28,v27,v28,v12
    vctsxs v19,v21,23
    xvaddsp v60,v60,v56
    lxv v59,288(r6)
    vrlq v27,v19,v27
    vctsxs v25,v28,23
    lxv v60,272(r6)
    vperm v27,v27,v27,v28
    lxv v60,288(r6)
    vrlq v23,v25,v28
    lxv v60,448(r6)
    vxor v21,v14,v28
    lxv v46,304(r6)
    lxv v60,256(r6)
    vsel v13,v14,v27,v28
    lxv v60,432(r6)
    vcmpgtfp v15,v21,v28
    addc r31,r4,r28
    lxv v49,112(r31)
    lxv v60,272(r6)
    lxv v46,352(r6)
    lxv v59,496(r6)
    vperm v23,v23,v23,v28
    vsel v11,v27,v19,v14
    lxv v50,336(r6)
    lxv v46,320(r6)
    lxv v59,240(r6)
    lxv v60,224(r6)
    vperm v18,v18,v14,v13
    vperm v14,v27,v28,v13
    lxv v58,496(r6)
    lxv v52,464(r6)
    lxv v59,304(r6)
    lxv v60,256(r6)
    vsel v20,v20,v26,v22
    vsel v26,v27,v23,v28
    lxv v60,384(r6)
    xvmulsp v54,v53,v60
    lxv v60,368(r6)
    vand v9,v19,v28
    lxv v59,416(r6)
    lxv v60,400(r6)
    vsel v21,v27,v28,v15
    xxlor v39,v43,v43
    xxlor v40,v43,v43
    lxv v59,208(r6)
    lxv v60,192(r6)
    xvmaddmsp v43,v50,v46
    vperm v10,v27,v28,v13
    lxv v59,496(r6)
    lxv v60,464(r6)
    lxv v46,176(r6)
    lxv v55,160(r6)
    vsel v19,v28,v27,v12
    vperm v13,v14,v23,v13
    xvaddsp v50,v53,v54
    lxv v55,496(r6)
    lxv v53,352(r6)
    lxv v59,336(r6)
    lxv v60,320(r6)
    vsel v14,v23,v25,v21
    vperm v23,v27,v28,v26
    lxv v59,240(r6)
    lxv v60,224(r6)
    vperm v21,v27,v28,v26
    lxv v60,496(r6)
    vadduwm v9,v9,v28
    xvmaddmsp v39,v43,v42
    xxlor v43,v46,v46
    xxlor v38,v46,v46
    lxv v59,208(r6)
    lxv v60,192(r6)
    xvmaddmsp v46,v55,v53
    vperm v12,v27,v28,v26
    lxv v60,448(r6)
    vxor v21,v17,v28
    lxv v60,368(r6)
    vand v17,v25,v28
    vctsxs v25,v18,23
    lxv v59,176(r6)
    lxv v60,160(r6)
    xvmaddmsp v40,v39,v45
    vperm v23,v27,v28,v26
    lxv v60,496(r6)
    vadduwm v13,v17,v28
    lxv v60,288(r6)
    vrlq v27,v25,v28
    xvmaddmsp v43,v46,v44
    xvmulsp v46,v41,v40
    xvmaddmsp v38,v43,v55
    lxv v60,432(r6)
    vcmpgtfp v17,v21,v28
    lxv v60,272(r6)
    vperm v18,v27,v27,v28
    xvmulsp v46,v52,v46
    lxv v60,128(r6)
    vcmpgtfp v12,v16,v28
    lxv v60,384(r6)
    xvmulsp v52,v53,v60
    lxv v59,304(r6)
    lxv v60,256(r6)
    lxv v55,416(r6)
    lxv v53,400(r6)
    vsel v26,v27,v18,v28
    vsel v21,v23,v21,v17
    xvmulsp v59,v45,v38
    lxv v60,144(r6)
    vsel v10,v14,v28,v12
    lxv v60,112(r6)
    vcmpgtfp v11,v28,v16
    xvmulsp v46,v51,v59
    lxv v60,128(r6)
    vcmpgtfp v12,v24,v28
    lxv v59,496(r6)
    lxv v60,464(r6)
    vsel v18,v28,v27,v15
    vcmpeqfp v13,v16,v16
    xvaddsp v48,v53,v52
    lxv v55,496(r6)
    lxv v53,352(r6)
    lxv v59,336(r6)
    lxv v60,320(r6)
    vsel v19,v23,v25,v21
    vperm v15,v27,v28,v26
    lxv v59,240(r6)
    lxv v60,224(r6)
    lxv v53,400(r6)
    vperm v23,v27,v28,v26
    vsel v21,v10,v21,v11
    lxv v60,368(r6)
    vand v25,v25,v28
    xxlor v43,v51,v51
    xxlor v38,v51,v51
    lxv v59,208(r6)
    lxv v60,192(r6)
    xvmaddmsp v51,v47,v55
    vperm v23,v27,v28,v26
    vctsxs v15,v16,23
    lxv v60,144(r6)
    vsel v12,v14,v28,v12
    lxv v60,112(r6)
    vcmpgtfp v16,v28,v24
    lxv v60,480(r6)
    vsel v14,v28,v21,v13
    vcmpeqfp v24,v24,v24
    lxv v53,400(r6)
    lxv v59,176(r6)
    lxv v60,160(r6)
    vsel v12,v12,v21,v16
    vperm v21,v27,v28,v26
    lxv v60,496(r6)
    vadduwm v16,v25,v28
    lxv v60,288(r6)
    vrlq v27,v15,v28
    xvmaddmsp v43,v51,v55
    lxv v60,496(r6)
    xvaddsp v57,v60,v46
    lxv v60,272(r6)
    xvmaddmsp v38,v43,v53
    vperm v21,v27,v27,v28
    lxv v60,480(r6)
    vsel v28,v28,v12,v24
    lxv v59,64(r6)
    vand v19,v25,v27
    lxv v59,496(r6)
    xvaddsp v58,v59,v60
    lxv v59,304(r6)
    lxv v60,256(r6)
    vsel v12,v27,v21,v28
    xvmulsp v53,v48,v38
    lxv v59,496(r6)
    lxv v60,464(r6)
    vsel v13,v28,v27,v17
    xvmulsp v46,v50,v53
    lxv v55,496(r6)
    lxv v53,352(r6)
    lxv v59,336(r6)
    lxv v60,320(r6)
    vsel v11,v23,v15,v21
    vperm v10,v27,v28,v12
    lxv v59,240(r6)
    lxv v60,224(r6)
    vperm v9,v27,v28,v12
    lxv v60,496(r6)
    vcmpgtfp v27,v19,v28
    lxv v60,128(r6)
    vcmpgtfp v17,v22,v28
    lxv v60,64(r6)
    vand v16,v26,v28
    lxv v53,96(r6)
    lxv v60,80(r6)
    vsel v18,v21,v28,v27
    addc r31,r3,r28
    stxv v61,48(r31)
    lxv v61,32(r6)
    vcmpgtfp v23,v16,v29
SIGMOID_LOOPEND:
    ld r31,0(r6)
    addc r28,r28,r31
    b SIGMOID_LOOPHEAD
SIGMOID_EPILOGUE0:
    lxv v61,176(r6)
    lxv v60,160(r6)
    lxv v53,208(r6)
    lxv v59,192(r6)
    vperm v28,v29,v28,v12
    vperm v29,v21,v27,v12
    xxlor v59,v43,v43
    xxlor v53,v43,v43
    xvmaddmsp v43,v42,v41
    xvmaddmsp v53,v43,v61
    xvmaddmsp v59,v53,v60
    lxv v60,368(r6)
    vand v29,v15,v28
    lxv v60,496(r6)
    vadduwm v28,v29,v28
    xvmulsp v61,v60,v59
    vcmpeqfp v27,v20,v20
    lxv v60,112(r6)
    vcmpgtfp v21,v28,v20
    lxv v60,128(r6)
    vcmpgtfp v12,v20,v28
    xvmulsp v47,v45,v61
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v15,v28,v12
    vsel v29,v28,v29,v21
    lxv v60,480(r6)
    vsel v28,v28,v29,v27
    lxv v61,496(r6)
    xvaddsp v56,v61,v60
    vcmpeqfp v27,v24,v24
    lxv v60,64(r6)
    vand v13,v24,v28
    lxv v60,32(r6)
    vcmpgtfp v21,v13,v28
    lxv v60,48(r6)
    vcmpgefp v12,v28,v13
    lxv v60,496(r6)
    vcmpgtfp v15,v13,v28
    lxv v61,96(r6)
    lxv v60,80(r6)
    vsel v15,v29,v28,v15
    xvmulsp v61,v47,v45
    vrefp v28,v29
    lxv v45,496(r6)
    xvnmsubmsp v61,v60,v45
    xvmaddmsp v61,v60,v60
    xvmulsp v47,v47,v61
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v15,v28,v12
    vsel v21,v28,v29,v21
    lxv v61,480(r6)
    lxv v60,64(r6)
    vsel v29,v29,v21,v27
    vsel v27,v24,v29,v28
    lxv v60,144(r6)
    vsel v15,v14,v28,v17
    vcmpeqfp v21,v22,v22
    lxv v60,112(r6)
    vcmpgtfp v14,v28,v22
    lxv v60,400(r6)
    lxv v61,480(r6)
    vsel v28,v15,v28,v14
    vsel v28,v29,v28,v21
    lxv v61,496(r6)
    xvaddsp v45,v61,v60
    vcmpeqfp v21,v13,v13
    lxv v60,64(r6)
    vand v22,v13,v28
    lxv v60,32(r6)
    vcmpgtfp v14,v22,v28
    lxv v60,48(r6)
    vcmpgefp v12,v28,v22
    lxv v60,496(r6)
    vcmpgtfp v15,v22,v28
    lxv v61,96(r6)
    lxv v60,80(r6)
    vsel v15,v29,v28,v15
    xvmulsp v61,v47,v54
    vrefp v28,v29
    lxv v56,496(r6)
    xvnmsubmsp v61,v60,v56
    xvmaddmsp v61,v60,v60
    xvmulsp v47,v47,v61
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v15,v28,v12
    vsel v14,v28,v29,v14
    lxv v61,480(r6)
    lxv v60,64(r6)
    vsel v29,v29,v14,v21
    vsel v21,v13,v29,v28
    lxv v60,48(r6)
    vcmpgefp v14,v28,v16
    lxv v60,496(r6)
    vcmpgtfp v15,v16,v28
    lxv v61,96(r6)
    lxv v60,80(r6)
    vsel v15,v29,v28,v15
    xvmulsp v61,v47,v48
    vrefp v28,v29
    lxv v56,496(r6)
    xvnmsubmsp v61,v60,v56
    xvmaddmsp v61,v60,v60
    xvmulsp v47,v47,v61
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v15,v28,v14
    vsel v23,v28,v29,v23
    vcmpeqfp v14,v26,v26
    lxv v61,480(r6)
    lxv v60,64(r6)
    vsel v29,v29,v23,v14
    vsel v14,v26,v29,v28
    lxv v60,32(r6)
    vcmpgtfp v15,v19,v28
    lxv v60,48(r6)
    vcmpgefp v23,v28,v19
    xvmulsp v61,v50,v51
    vrefp v28,v29
    lxv v56,496(r6)
    xvnmsubmsp v61,v60,v56
    xvmaddmsp v61,v60,v60
    xvmulsp v44,v50,v61
    lxv v60,144(r6)
    lxv v61,400(r6)
    vsel v28,v12,v28,v23
    vsel v23,v28,v29,v15
    vcmpeqfp v15,v25,v25
    lxv v61,480(r6)
    lxv v60,64(r6)
    vsel v29,v29,v23,v15
    vsel v28,v25,v29,v28
    addc r31,r3,r28
    stxv v60,0(r31)
    addc r31,r3,r28
    stxv v46,16(r31)
    addc r31,r3,r28
    stxv v53,32(r31)
    addc r31,r3,r28
    stxv v59,48(r31)
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
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000040
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000040
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000200
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000200
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
    .vbyte 4, 0x527dd750
    .vbyte 4, 0x01067e8b
    .vbyte 4, 0x7937ab58
    .vbyte 4, 0x2758b9e3
    .vbyte 4, 0x3e3f3f3f
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0xfa081422
    .vbyte 4, 0x31415265
    .vbyte 4, 0x2413a17a
    .vbyte 4, 0x1bc99db2
    .vbyte 4, 0x6e62dab9
    .vbyte 4, 0x96e2fd82
    .vbyte 4, 0x3e3e3e3f
    .vbyte 4, 0x3f3f3f3f
    .vbyte 4, 0xc6d8eb00
    .vbyte 4, 0x0c182635
    .vbyte 4, 0x6bad0a9e
    .vbyte 4, 0x83d8bf5c
    .vbyte 4, 0x9f4ffc58
    .vbyte 4, 0x6cc8c2b3
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x8793a1af
    .vbyte 4, 0xbfd0e3f8
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x07070707
    .vbyte 4, 0x00000000
    .vbyte 4, 0x04040404
    .vbyte 4, 0x08080808
    .vbyte 4, 0x0c0c0c0c
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000004
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0x00081018
    .vbyte 4, 0x71772ff6
    .vbyte 4, 0xe518aabc
    .vbyte 4, 0x95935393
    .vbyte 4, 0xe61761a0
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x3d3d3d3d
    .vbyte 4, 0x6d818d99
    .vbyte 4, 0xa7b7c7d9
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
