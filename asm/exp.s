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
    .file "exp.c","IBM Open XL C/C++ for AIX 17.1.1 (5725-C72, 5765-J18), version 17.1.1.4, LLVM version 15.0.0git"
    .globl exp[DS]
    .globl  .exp
    .align  4
    .csect exp[DS],3
    .vbyte  8, .exp
    .vbyte  8, TOC[TC0]
    .vbyte  8, 0
    .csect .text[PR],2
.exp:
    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs
    std     r13,-76(r12)             #save r13
    std     r14,-72(r12)             #save r14
    std     r15,-68(r12)             #save r15
    std     r16,-64(r12)             #save r16
    std     r17,-60(r12)             #save r17
    std     r18,-56(r12)             #save r18
    std     r19,-52(r12)             #save r19
    std     r20,-48(r12)             #save r20
    std     r21,-44(r12)             #save r21
    std     r22,-40(r12)             #save r22
    std     r23,-36(r12)             #save r23
    std     r24,-32(r12)             #save r24
    std     r25,-28(r12)             #save r25
    std     r26,-24(r12)             #save r26
    std     r27,-20(r12)             #save r27
    std     r28,-16(r12)             #save r28
    std     r29,-12(r12)             #save r29
    std     r30,-8(r12)              #save r30
    std     r31,-4(r12)              #save r31
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
    ld r6,L..constants(RTOC)
    la r27,-624(SP)
    lxv v2,368(r6)
    lxv v3,352(r6)
    lxv v0,336(r6)
    lxv v4,320(r6)
    lxv v5,48(r6)
    lxv v6,416(r6)
    lxv v7,400(r6)
    lxv v1,384(r6)
    lxv v8,304(r6)
    lxv v32,384(r6)
    lxv v9,288(r6)
    lxv v33,272(r6)
    lxv v34,256(r6)
    lxv v10,240(r6)
    lxv v35,224(r6)
    lxv v36,208(r6)
    lxv v11,192(r6)
    lxv v37,176(r6)
    lxv v38,160(r6)
    lxv v39,144(r6)
    lxv v40,128(r6)
    lxv v41,112(r6)
    lxv v42,96(r6)
    lxv v12,80(r6)
    lxv v13,64(r6)
    ld r9,32(r6)
    ld r8,16(r6)
    ld r7,0(r6)
EXP_INIT:
    xor r28,r5,r5
    cmpi 0,r5,127 ; ble EXP_TAILSTOREINIT
EXP_PROLOGUE0:
    addc r31,r4,r28
    lxv v61,0(r31)
    addc r31,r4,r28
    xvcmpgtsp v59,v61,v2
    lxv v53,16(r31)
    xvmulsp v55,v61,v4
    xxsel v60,v3,v0,v59
    xvaddsp v61,v60,v55
    xvcmpgtsp v44,v53,v2
    addc r31,r4,r28
    xvmulsp v52,v53,v4
    xxsel v50,v3,v0,v44
    lxv v49,32(r31)
    xvcmpgtsp v28,v5,v52
EXP_LOOPHEAD:
    sub r31,r5,r9
    cmp 0,r28,r31 ; bge EXP_EPILOGUE0
EXP_KERNEL:
    vctsxs v21,v29,23
    vrlq v28,v21,v3
    xvaddsp v61,v50,v52
    vctsxs v18,v29,23
    xvcmpgtsp v45,v49,v2
    vperm v29,v28,v28,v4
    addc r31,r4,r28
    xxsel v57,v3,v0,v45
    xvmulsp v51,v49,v4
    lxv v31,48(r31)
    xxsel v49,v10,v61,v11
    vrlq v28,v18,v3
    vperm v28,v28,v28,v4
    xvaddsp v61,v57,v51
    xxsel v25,v7,v1,v59
    xxsel v59,v1,v53,v9
    vperm v22,v1,v2,v17
    vperm v25,v5,v6,v17
    xxland v47,v53,v8
    xvcmpgtsp v43,v31,v2
    xxlor v26,v59,v59
    xxlor v29,v59,v59
    vctsxs v26,v29,23
    xvmaddmsp v59,v54,v57
    vperm v14,v7,v8,v17
    xxsel v53,v10,v60,v11
    xxsel v27,v1,v50,v9
    vperm v24,v1,v2,v21
    vperm v22,v5,v6,v21
    vrlq v28,v26,v3
    xxsel v30,v7,v1,v44
    vperm v16,v9,v10,v17
    xxland v49,v50,v8
    vadduwm v15,v15,v0
    xvmaddmsp v26,v59,v46
    addc r31,r4,r28
    xxsel v50,v3,v0,v43
    xvmulsp v59,v31,v4
    lxv v24,64(r31)
    xvaddsp v57,v50,v59
    xxlor v44,v27,v27
    xxlor v31,v27,v27
    vperm v18,v28,v28,v4
    xvmaddmsp v29,v26,v48
    xvmaddmsp v27,v56,v54
    vperm v22,v7,v8,v21
    vperm v21,v9,v10,v21
    xvmaddmsp v44,v27,v54
    xxsel v56,v10,v50,v11
    vadduwm v16,v17,v0
    xvmulsp v47,v47,v29
    xxsel v29,v7,v1,v45
    xvcmpgtsp v46,v24,v2
    vctsxs v17,v25,23
    vrlq v18,v17,v3
    xxsel v27,v1,v58,v9
    vperm v22,v1,v2,v24
    vperm v25,v5,v6,v24
    xvmulsp v26,v25,v47
    xvcmpgtsp v61,v55,v13
    xvmaddmsp v31,v44,v53
    xxlor v47,v27,v27
    xxlor v45,v27,v27
    xvmulsp v48,v48,v31
    xvmaddmsp v27,v54,v57
    vperm v21,v7,v8,v24
    xxland v54,v58,v8
    xxsel v44,v26,v12,v61
    addc r31,r4,r28
    xvcmpgtsp v31,v5,v55
    vperm v24,v9,v10,v24
    xxsel v61,v3,v0,v46
    xvmulsp v57,v24,v4
    lxv v25,80(r31)
    xvaddsp v60,v61,v57
    vadduwm v26,v22,v0
    vperm v29,v18,v18,v4
    vcmpeqfp v18,v23,v23
    xxsel v54,v10,v61,v11
    xvmaddmsp v47,v27,v53
    xxsel v27,v44,v0,v31
    xvmulsp v55,v30,v48
    xvcmpgtsp v53,v52,v13
    xxsel v30,v55,v12,v53
    xvmaddmsp v45,v47,v56
    xxsel v44,v7,v1,v43
    vcmpeqfp v16,v20,v20
    xvcmpgtsp v47,v25,v2
    xxsel v26,v6,v27,v50
    vctsxs v24,v28,23
    vrlq v20,v24,v3
    xxsel v31,v1,v49,v9
    vperm v18,v1,v2,v22
    vperm v23,v5,v6,v22
    xxlor v43,v31,v31
    xxlor v27,v31,v31
    xvmaddmsp v31,v50,v55
    vperm v21,v7,v8,v22
    addc r31,r3,r28
    xvmulsp v61,v58,v45
    stxv v26,0(r31)
    xxsel v50,v30,v0,v28
    xxland v49,v49,v8
    xxsel v30,v6,v50,v48
    xvmulsp v45,v29,v61
    xvcmpgtsp v48,v51,v13
    addc r31,r4,r28
    vperm v23,v9,v10,v22
    xxsel v61,v3,v0,v47
    xvmulsp v50,v25,v4
    lxv v26,96(r31)
    xvaddsp v60,v61,v50
    vadduwm v26,v17,v0
    vperm v29,v20,v20,v4
    xxsel v54,v10,v61,v11
    xvmaddmsp v43,v31,v53
    xvmaddmsp v27,v43,v55
    addc r31,r3,r28
    xxsel v52,v45,v12,v48
    xvcmpgtsp v49,v5,v51
    xxsel v28,v7,v1,v46
    stxv v30,16(r31)
    vcmpeqfp v16,v19,v19
    xxsel v30,v52,v0,v49
    xvcmpgtsp v46,v26,v2
    vctsxs v17,v28,23
    vrlq v20,v17,v3
    xxsel v45,v1,v56,v9
    vperm v23,v1,v2,v22
    vperm v19,v5,v6,v22
    xxlor v43,v45,v45
    xxlor v31,v45,v45
    xvmaddmsp v45,v55,v51
    vperm v21,v7,v8,v22
    xvmulsp v61,v58,v27
    xxland v56,v56,v8
    xvmulsp v29,v44,v61
    xvcmpgtsp v27,v59,v13
    xxsel v60,v6,v30,v48
    addc r31,r3,r28
    stxv v60,32(r31)
    addc r31,r4,r28
    vperm v23,v9,v10,v22
    xxsel v61,v3,v0,v46
    xvmulsp v51,v26,v4
    lxv v26,112(r31)
    vadduwm v26,v24,v0
    xvaddsp v60,v61,v51
    vperm v29,v20,v20,v4
    xxsel v54,v10,v61,v11
    xvmaddmsp v43,v45,v53
    xxsel v44,v7,v1,v47
    xvmaddmsp v31,v43,v55
    xxsel v52,v29,v12,v27
    xvcmpgtsp v53,v5,v59
    vcmpeqfp v16,v27,v27
    xxsel v30,v52,v0,v53
    vctsxs v21,v28,23
    xvcmpgtsp v47,v26,v2
    vrlq v27,v21,v3
    xxsel v27,v1,v49,v9
    vperm v23,v1,v2,v22
    vperm v20,v5,v6,v22
    xxlor v43,v27,v27
    xxlor v45,v27,v27
    xvmaddmsp v27,v55,v52
    vperm v24,v7,v8,v22
    xvmulsp v61,v58,v31
    xvmulsp v55,v26,v4
    xxland v49,v49,v8
    xvmulsp v29,v28,v61
    xvcmpgtsp v28,v57,v13
    xxsel v52,v6,v30,v48
    xxsel v61,v3,v0,v47
    addc r31,r3,r28
    stxv v52,48(r31)
    vperm v20,v9,v10,v22
    vadduwm v17,v17,v0
    vperm v28,v27,v27,v4
    xvaddsp v61,v61,v55
    xxsel v31,v7,v1,v46
    xxsel v54,v10,v60,v11
    xvmaddmsp v43,v27,v56
    xvmaddmsp v45,v43,v52
    xxsel v56,v29,v12,v28
    xvcmpgtsp v52,v5,v57
    vcmpeqfp v26,v25,v25
    xxsel v27,v56,v0,v52
    vctsxs v24,v29,23
    xxsel v43,v1,v53,v9
    vperm v20,v1,v2,v22
    vperm v25,v5,v6,v22
    vrlq v27,v24,v3
    xxlor v48,v43,v43
    xxlor v46,v43,v43
    xvmaddmsp v43,v52,v57
    vperm v25,v7,v8,v22
    xvmulsp v61,v49,v45
    xxland v52,v53,v8
    xvmulsp v45,v44,v61
    xvcmpgtsp v49,v50,v13
    xxsel v60,v6,v27,v58
    addc r31,r3,r28
    stxv v60,64(r31)
    vperm v27,v27,v27,v4
    xxsel v58,v7,v1,v47
    vperm v28,v9,v10,v22
    vadduwm v22,v20,v0
    xvmaddmsp v48,v43,v57
    xxsel v52,v10,v59,v11
    xvmaddmsp v46,v48,v60
    xxsel v49,v45,v12,v49
    xvcmpgtsp v57,v5,v50
    vcmpeqfp v21,v18,v18
    xxsel v47,v49,v0,v57
    xxsel v48,v1,v56,v9
    vperm v25,v1,v2,v20
    vperm v27,v5,v6,v20
    xxland v50,v56,v8
    xxlor v56,v48,v48
    xxlor v49,v48,v48
    xvmulsp v60,v54,v46
    xvmaddmsp v48,v57,v59
    vperm v27,v7,v8,v20
    xvmulsp v54,v31,v60
    xvcmpgtsp v57,v51,v13
    xxsel v60,v6,v47,v53
    addc r31,r3,r28
    stxv v60,80(r31)
    vperm v28,v9,v10,v20
    vadduwm v29,v18,v0
    xvmaddmsp v56,v48,v59
    xxsel v57,v54,v12,v57
    xvcmpgtsp v50,v5,v51
    xvmaddmsp v49,v56,v60
    vcmpeqfp v28,v19,v19
    xxsel v50,v57,v0,v50
    xvmulsp v53,v61,v49
    xxsel v50,v6,v50,v60
    xvmulsp v53,v58,v53
    addc r31,r3,r28
    xvcmpgtsp v61,v55,v13
    stxv v50,96(r31)
    xxsel v50,v53,v12,v61
    addc r31,r4,r28
    lxv v57,128(r31)
    xvcmpgtsp v53,v5,v55
    vcmpeqfp v29,v23,v23
    xxsel v53,v50,v0,v53
    addc r31,r4,r28
    xvcmpgtsp v59,v57,v2
    lxv v50,144(r31)
    xxsel v60,v6,v53,v61
    xvmulsp v55,v57,v4
    xxsel v61,v3,v0,v59
    xvaddsp v61,v61,v55
    xvcmpgtsp v44,v50,v2
    addc r30,r4,r28
    addc r31,r3,r28
    xvmulsp v52,v50,v4
    xxsel v50,v3,v0,v44
    lxv v49,160(r30)
    stxv v60,112(r31)
    xvcmpgtsp v28,v5,v52
EXP_LOOPEND:
    addc r28,r28,r8
    b EXP_LOOPHEAD
EXP_EPILOGUE0:
    vctsxs v19,v29,23
    vrlq v28,v19,v3
    xvaddsp v61,v50,v52
    vctsxs v21,v29,23
    xvcmpgtsp v31,v49,v2
    vperm v29,v28,v28,v4
    addc r31,r4,r28
    xxsel v50,v3,v0,v31
    xvmulsp v57,v49,v4
    lxv v43,48(r31)
    xxsel v49,v10,v61,v11
    vrlq v28,v21,v3
    vperm v28,v28,v28,v4
    xvaddsp v61,v50,v57
    xxsel v26,v7,v1,v59
    xxsel v59,v1,v51,v9
    vperm v18,v1,v2,v17
    vperm v22,v5,v6,v17
    xxland v47,v51,v8
    xvcmpgtsp v45,v43,v2
    xxlor v30,v59,v59
    xxlor v29,v59,v59
    vctsxs v26,v29,23
    xvmaddmsp v59,v50,v54
    vperm v14,v7,v8,v17
    xxsel v51,v10,v60,v11
    xxsel v27,v1,v53,v9
    vperm v24,v1,v2,v19
    vperm v18,v5,v6,v19
    vrlq v28,v26,v3
    xxsel v44,v7,v1,v44
    vperm v16,v9,v10,v17
    xxland v49,v53,v8
    vadduwm v15,v15,v0
    xvmaddmsp v30,v59,v46
    addc r31,r4,r28
    xxsel v53,v3,v0,v45
    xvmulsp v59,v43,v4
    lxv v25,64(r31)
    xvaddsp v54,v53,v59
    xxlor v46,v27,v27
    xxlor v43,v27,v27
    vperm v21,v28,v28,v4
    xvmaddmsp v29,v30,v48
    xvmaddmsp v27,v56,v50
    vperm v18,v7,v8,v19
    vperm v24,v9,v10,v19
    xvmaddmsp v46,v27,v50
    xxsel v51,v10,v53,v11
    vadduwm v16,v17,v0
    xvmulsp v47,v47,v29
    xxsel v30,v7,v1,v31
    xvcmpgtsp v31,v25,v2
    vctsxs v17,v22,23
    vrlq v21,v17,v3
    xxsel v27,v1,v58,v9
    vperm v18,v1,v2,v19
    vperm v22,v5,v6,v19
    xvmulsp v29,v26,v47
    xvcmpgtsp v61,v55,v13
    xvmaddmsp v43,v46,v56
    xxlor v47,v27,v27
    xxlor v46,v27,v27
    xvmulsp v43,v48,v43
    xvmaddmsp v27,v50,v54
    vperm v24,v7,v8,v19
    xxland v58,v58,v8
    xxsel v26,v29,v12,v61
    addc r31,r4,r28
    xvcmpgtsp v29,v5,v55
    vperm v18,v9,v10,v19
    xxsel v61,v3,v0,v31
    xvmulsp v54,v25,v4
    lxv v25,80(r31)
    xvaddsp v60,v61,v54
    vadduwm v26,v26,v0
    vperm v29,v21,v21,v4
    vcmpeqfp v21,v23,v23
    xxsel v55,v10,v61,v11
    xvmaddmsp v47,v27,v56
    xxsel v51,v26,v0,v29
    xvmulsp v56,v44,v43
    xvcmpgtsp v48,v52,v13
    xxsel v29,v56,v12,v48
    xvmaddmsp v46,v47,v50
    xxsel v44,v7,v1,v45
    vcmpeqfp v16,v20,v20
    xvcmpgtsp v47,v25,v2
    xxsel v26,v6,v51,v53
    vctsxs v19,v28,23
    vrlq v21,v19,v3
    xxsel v43,v1,v49,v9
    vperm v20,v1,v2,v23
    vperm v18,v5,v6,v23
    xxlor v27,v43,v43
    xxlor v45,v43,v43
    xvmaddmsp v43,v52,v50
    vperm v24,v7,v8,v23
    addc r31,r3,r28
    xvmulsp v61,v58,v46
    stxv v26,0(r31)
    xxsel v50,v29,v0,v28
    xxland v49,v49,v8
    xxsel v28,v6,v50,v48
    xvmulsp v46,v30,v61
    xvcmpgtsp v58,v57,v13
    addc r31,r4,r28
    vperm v18,v9,v10,v23
    xxsel v61,v3,v0,v47
    xvmulsp v52,v25,v4
    lxv v26,96(r31)
    xvaddsp v60,v61,v52
    vadduwm v17,v17,v0
    vperm v29,v21,v21,v4
    xxsel v55,v10,v61,v11
    xvmaddmsp v27,v43,v56
    xvmaddmsp v45,v27,v50
    addc r31,r3,r28
    xxsel v56,v46,v12,v58
    xvcmpgtsp v50,v5,v57
    xxsel v30,v7,v1,v31
    stxv v28,16(r31)
    vcmpeqfp v16,v25,v25
    xxsel v29,v56,v0,v50
    xvcmpgtsp v27,v26,v2
    vctsxs v26,v28,23
    vrlq v25,v26,v3
    xxsel v43,v1,v51,v9
    vperm v18,v1,v2,v23
    vperm v21,v5,v6,v23
    xxlor v31,v43,v43
    xxlor v46,v43,v43
    xvmaddmsp v43,v50,v53
    vperm v24,v7,v8,v23
    xvmulsp v61,v49,v45
    xxland v51,v51,v8
    xvmulsp v28,v44,v61
    xvcmpgtsp v45,v59,v13
    xxsel v60,v6,v29,v48
    addc r31,r3,r28
    stxv v60,32(r31)
    addc r31,r4,r28
    vperm v21,v9,v10,v23
    xxsel v61,v3,v0,v27
    xvmulsp v50,v26,v4
    lxv v26,112(r31)
    vadduwm v23,v19,v0
    xvaddsp v60,v61,v50
    vperm v29,v25,v25,v4
    xxsel v49,v10,v61,v11
    xvmaddmsp v31,v43,v56
    xxsel v44,v7,v1,v47
    xvmaddmsp v46,v31,v53
    xxsel v57,v28,v12,v45
    xvcmpgtsp v53,v5,v59
    vcmpeqfp v16,v27,v27
    xxsel v29,v57,v0,v53
    vctsxs v19,v28,23
    xvcmpgtsp v47,v26,v2
    vrlq v27,v19,v3
    xxsel v45,v1,v58,v9
    vperm v21,v1,v2,v17
    vperm v25,v5,v6,v17
    xxlor v31,v45,v45
    xxlor v43,v45,v45
    xvmaddmsp v45,v53,v57
    vperm v24,v7,v8,v17
    xvmulsp v61,v55,v46
    xvmulsp v55,v26,v4
    xxland v53,v58,v8
    xvmulsp v28,v30,v61
    xvcmpgtsp v30,v54,v13
    xxsel v57,v6,v29,v48
    xxsel v61,v3,v0,v47
    addc r31,r3,r28
    stxv v57,48(r31)
    vperm v25,v9,v10,v17
    vadduwm v17,v21,v0
    vperm v28,v27,v27,v4
    xvaddsp v61,v61,v55
    xxsel v46,v7,v1,v27
    xxsel v53,v10,v60,v11
    xvmaddmsp v31,v45,v56
    xvmaddmsp v43,v31,v57
    xxsel v48,v28,v12,v30
    xvcmpgtsp v57,v5,v54
    vcmpeqfp v26,v22,v22
    xxsel v45,v48,v0,v57
    vctsxs v24,v29,23
    xxsel v31,v1,v51,v9
    vperm v22,v1,v2,v21
    vperm v25,v5,v6,v21
    vrlq v27,v24,v3
    xxlor v48,v31,v31
    xxlor v27,v31,v31
    xvmaddmsp v31,v54,v57
    vperm v25,v7,v8,v21
    xvmulsp v61,v49,v43
    xxland v54,v51,v8
    xvmulsp v43,v44,v61
    xvcmpgtsp v51,v52,v13
    xxsel v60,v6,v45,v58
    addc r31,r3,r28
    stxv v60,64(r31)
    vperm v27,v27,v27,v4
    xxsel v47,v7,v1,v47
    vperm v28,v9,v10,v21
    vadduwm v21,v22,v0
    xvmaddmsp v48,v31,v57
    xxsel v54,v10,v59,v11
    xvmaddmsp v27,v48,v60
    xxsel v48,v43,v12,v51
    xvcmpgtsp v49,v5,v52
    vcmpeqfp v19,v20,v20
    xxsel v31,v48,v0,v49
    xxsel v58,v1,v56,v9
    vperm v25,v1,v2,v22
    vperm v27,v5,v6,v22
    xxland v52,v56,v8
    xxlor v48,v58,v58
    xxlor v49,v58,v58
    xvmulsp v60,v53,v27
    xvmaddmsp v58,v57,v59
    vperm v27,v7,v8,v22
    xvmulsp v56,v46,v60
    xvcmpgtsp v57,v50,v13
    xxsel v60,v6,v31,v51
    addc r31,r3,r28
    stxv v60,80(r31)
    vperm v28,v9,v10,v22
    vadduwm v29,v20,v0
    xvmaddmsp v48,v58,v59
    xxsel v54,v56,v12,v57
    xvcmpgtsp v57,v5,v50
    xvmaddmsp v49,v48,v60
    vcmpeqfp v28,v18,v18
    xxsel v50,v54,v0,v57
    xvmulsp v53,v61,v49
    xxsel v50,v6,v50,v60
    xvmulsp v53,v47,v53
    addc r31,r3,r28
    xvcmpgtsp v61,v55,v13
    stxv v50,96(r31)
    xxsel v57,v53,v12,v61
    xvcmpgtsp v50,v5,v55
    vcmpeqfp v29,v23,v23
    xxsel v60,v57,v0,v50
    xxsel v60,v6,v60,v61
    addc r31,r3,r28
    stxv v60,112(r31)
EXP_POSTCNTINCR:
    addc r28,r28,r8
EXP_TAILSTOREINIT:
    xor r26,r5,r5
    xor r24,r28,r28
    cmp 0,r28,r5 ; bge RETURN
EXP_TAILSTOREHEAD:
    sub r29,r5,r7
    addc r31,r27,r26
    addc r25,r28,r26
    addc r30,r4,r25
    ld r30,0(r30)
    std r30,0(r31)
    addc r26,r7,r26
    cmp 0,r25,r29 ; blt EXP_TAILSTOREHEAD
EXP_TAIL:
    addc r4,r27,r24
    lxv v61,0(r4)
    addc r4,r27,r24
    xvcmpgtsp v49,v61,v2
    lxv v53,16(r4)
    xvmulsp v55,v61,v4
    xxsel v60,v3,v0,v49
    xvaddsp v61,v60,v55
    xvcmpgtsp v59,v53,v2
    addc r4,r27,r24
    xvmulsp v52,v53,v4
    xxsel v50,v3,v0,v59
    lxv v57,32(r4)
    xvcmpgtsp v29,v5,v52
    vctsxs v21,v29,23
    vrlq v28,v21,v3
    xvaddsp v61,v50,v52
    vctsxs v19,v29,23
    xvcmpgtsp v43,v57,v2
    vperm v29,v28,v28,v4
    addc r4,r27,r24
    xxsel v50,v3,v0,v43
    xvmulsp v58,v57,v4
    lxv v44,48(r4)
    xxsel v56,v10,v61,v11
    vrlq v28,v19,v3
    vperm v28,v28,v28,v4
    xvaddsp v61,v50,v58
    xxsel v25,v7,v1,v49
    xxsel v31,v1,v53,v9
    vperm v18,v1,v2,v24
    vperm v25,v5,v6,v24
    xxland v46,v53,v8
    xvcmpgtsp v45,v44,v2
    xxlor v26,v31,v31
    xxlor v27,v31,v31
    vctsxs v17,v29,23
    xvmaddmsp v31,v50,v57
    vperm v15,v7,v8,v24
    xxsel v53,v10,v60,v11
    xxsel v30,v1,v51,v9
    vperm v22,v1,v2,v21
    vperm v18,v5,v6,v21
    vrlq v28,v17,v3
    xxsel v28,v7,v1,v59
    vperm v24,v9,v10,v24
    xxland v48,v51,v8
    vadduwm v14,v14,v0
    xvmaddmsp v26,v31,v47
    addc r4,r27,r24
    xxsel v57,v3,v0,v45
    xvmulsp v59,v44,v4
    lxv v24,64(r4)
    xvaddsp v51,v57,v59
    xxlor v44,v30,v30
    xxlor v31,v30,v30
    vperm v25,v28,v28,v4
    xvmaddmsp v27,v26,v56
    xvmaddmsp v30,v54,v50
    vperm v18,v7,v8,v21
    vperm v24,v9,v10,v21
    xvmaddmsp v44,v30,v50
    xxsel v53,v10,v57,v11
    vadduwm v16,v16,v0
    xvmulsp v46,v46,v27
    xxsel v30,v7,v1,v43
    xvcmpgtsp v47,v24,v2
    vctsxs v22,v19,23
    vrlq v19,v22,v3
    xxsel v43,v1,v49,v9
    vperm v18,v1,v2,v21
    vperm v25,v5,v6,v21
    xvmulsp v27,v25,v46
    xvcmpgtsp v61,v55,v13
    xvmaddmsp v31,v44,v56
    xxlor v46,v43,v43
    xxlor v44,v43,v43
    xvmulsp v48,v48,v31
    xvmaddmsp v43,v50,v57
    vperm v24,v7,v8,v21
    xxland v49,v49,v8
    xxsel v27,v27,v12,v61
    addc r4,r27,r24
    xvcmpgtsp v31,v5,v55
    vperm v18,v9,v10,v21
    xxsel v61,v3,v0,v47
    xvmulsp v57,v24,v4
    lxv v26,80(r4)
    xvaddsp v60,v61,v57
    vadduwm v17,v17,v0
    vperm v29,v19,v19,v4
    vcmpeqfp v19,v23,v23
    xxsel v53,v10,v61,v11
    xvmaddmsp v46,v43,v56
    xxsel v43,v27,v0,v31
    xvmulsp v55,v28,v48
    xvcmpgtsp v56,v52,v13
    xxsel v28,v55,v12,v56
    xvmaddmsp v44,v46,v50
    xxsel v31,v7,v1,v45
    vcmpeqfp v24,v20,v20
    xvcmpgtsp v48,v26,v2
    xxsel v27,v6,v43,v51
    vctsxs v18,v28,23
    vrlq v23,v18,v3
    xxsel v45,v1,v54,v9
    vperm v19,v1,v2,v21
    vperm v20,v5,v6,v21
    xxlor v46,v45,v45
    xxlor v43,v45,v45
    xvmaddmsp v45,v51,v52
    vperm v19,v7,v8,v21
    addc r4,r27,r24
    xvmulsp v61,v49,v44
    stxv v27,0(r4)
    xxsel v49,v28,v0,v29
    xxland v54,v54,v8
    xxsel v28,v6,v49,v56
    xvmulsp v29,v30,v61
    xvcmpgtsp v44,v58,v13
    addc r4,r27,r24
    vperm v20,v9,v10,v21
    xxsel v61,v3,v0,v48
    xvmulsp v56,v26,v4
    lxv v27,96(r4)
    xvaddsp v60,v61,v56
    vadduwm v22,v22,v0
    vperm v29,v23,v23,v4
    xxsel v53,v10,v61,v11
    xvmaddmsp v46,v45,v51
    xvmaddmsp v43,v46,v52
    addc r4,r27,r24
    xxsel v51,v29,v12,v44
    xvcmpgtsp v49,v5,v58
    xxsel v29,v7,v1,v47
    stxv v28,16(r4)
    vcmpeqfp v26,v26,v26
    xxsel v30,v51,v0,v49
    xvcmpgtsp v46,v27,v2
    vctsxs v17,v28,23
    vrlq v23,v17,v3
    xxsel v45,v1,v50,v9
    vperm v19,v1,v2,v21
    vperm v20,v5,v6,v21
    xxlor v47,v45,v45
    xxlor v44,v45,v45
    xvmaddmsp v45,v51,v52
    vperm v19,v7,v8,v21
    xvmulsp v61,v54,v43
    xxland v50,v50,v8
    xvmulsp v31,v31,v61
    xvcmpgtsp v43,v59,v13
    xxsel v60,v6,v30,v58
    addc r4,r27,r24
    stxv v60,32(r4)
    addc r4,r27,r24
    vperm v20,v9,v10,v21
    xxsel v61,v3,v0,v46
    xvmulsp v58,v27,v4
    lxv v28,112(r4)
    vadduwm v22,v18,v0
    xvaddsp v60,v61,v58
    vperm v29,v23,v23,v4
    xxsel v50,v10,v61,v11
    xvmaddmsp v47,v45,v51
    xxsel v30,v7,v1,v48
    xvmaddmsp v44,v47,v52
    xxsel v52,v31,v12,v43
    xvcmpgtsp v53,v5,v59
    vcmpeqfp v16,v27,v27
    xxsel v27,v52,v0,v53
    vctsxs v19,v28,23
    xvcmpgtsp v47,v28,v2
    vrlq v27,v19,v3
    xxsel v31,v1,v49,v9
    vperm v20,v1,v2,v18
    vperm v23,v5,v6,v18
    xxlor v45,v31,v31
    xxlor v43,v31,v31
    xvmaddmsp v31,v52,v55
    vperm v21,v7,v8,v18
    xvmulsp v61,v54,v44
    xvmulsp v54,v28,v4
    xxland v52,v49,v8
    xvmulsp v28,v29,v61
    xvcmpgtsp v29,v57,v13
    xxsel v49,v6,v27,v48
    xxsel v61,v3,v0,v47
    addc r4,r27,r24
    stxv v49,48(r4)
    vperm v23,v9,v10,v18
    vadduwm v18,v20,v0
    vperm v28,v27,v27,v4
    xvaddsp v61,v61,v54
    xxsel v44,v7,v1,v46
    xxsel v52,v10,v60,v11
    xvmaddmsp v45,v31,v53
    xvmaddmsp v43,v45,v55
    xxsel v55,v28,v12,v29
    xvcmpgtsp v53,v5,v57
    vcmpeqfp v17,v25,v25
    xxsel v31,v55,v0,v53
    vctsxs v21,v29,23
    xxsel v45,v1,v51,v9
    vperm v23,v1,v2,v20
    vperm v25,v5,v6,v20
    vrlq v27,v21,v3
    xxlor v48,v45,v45
    xxlor v46,v45,v45
    xvmaddmsp v45,v55,v57
    vperm v25,v7,v8,v20
    xvmulsp v61,v50,v43
    xxland v55,v51,v8
    xvmulsp v43,v30,v61
    xvcmpgtsp v51,v56,v13
    xxsel v60,v6,v31,v49
    addc r4,r27,r24
    stxv v60,64(r4)
    vperm v27,v27,v27,v4
    xxsel v47,v7,v1,v47
    vperm v28,v9,v10,v20
    vadduwm v20,v23,v0
    xvmaddmsp v48,v45,v57
    xxsel v55,v10,v59,v11
    xvmaddmsp v46,v48,v60
    xxsel v49,v43,v12,v51
    xvcmpgtsp v57,v5,v56
    vcmpeqfp v19,v24,v24
    xxsel v45,v49,v0,v57
    xxsel v48,v1,v53,v9
    vperm v25,v1,v2,v23
    vperm v27,v5,v6,v23
    xxland v56,v53,v8
    xxlor v49,v48,v48
    xxlor v50,v48,v48
    xvmulsp v60,v52,v46
    xvmaddmsp v48,v57,v59
    vperm v27,v7,v8,v23
    xvmulsp v52,v44,v60
    xvcmpgtsp v57,v58,v13
    xxsel v60,v6,v45,v51
    addc r4,r27,r24
    stxv v60,80(r4)
    vperm v28,v9,v10,v23
    vadduwm v29,v24,v0
    xvmaddmsp v49,v48,v59
    xxsel v52,v52,v12,v57
    xvcmpgtsp v57,v5,v58
    xvmaddmsp v50,v49,v60
    vcmpeqfp v28,v26,v26
    xxsel v57,v52,v0,v57
    xvmulsp v53,v61,v50
    xxsel v50,v6,v57,v60
    xvmulsp v53,v47,v53
    addc r4,r27,r24
    xvcmpgtsp v61,v54,v13
    stxv v50,96(r4)
    xxsel v57,v53,v12,v61
    xvcmpgtsp v50,v5,v54
    vcmpeqfp v29,v22,v22
    xxsel v60,v57,v0,v50
    xxsel v60,v6,v60,v61
    addc r4,r27,r24
    stxv v60,112(r4)
EXP_TAILLOADINIT:
    xor r26,r26,r26
EXP_TAILLOADHEAD:
    sub r30,r5,r7
    addc r4,r27,r26
    ld r4,0(r4)
    addc r29,r28,r26
    addc r31,r3,r29
    std r4,0(r31)
    addc r26,r7,r26
    cmp 0,r29,r30 ; blt EXP_TAILLOADHEAD
RETURN:
    subi r12,SP,152 # compute gpr save pointer, 152 = 8 bytes * 19 gprs
    ld    r13,-76(r12)             #restore r13
    ld    r14,-72(r12)             #restore r14
    ld    r15,-68(r12)             #restore r15
    ld    r16,-64(r12)             #restore r16
    ld    r17,-60(r12)             #restore r17
    ld    r18,-56(r12)             #restore r18
    ld    r19,-52(r12)             #restore r19
    ld    r20,-48(r12)             #restore r20
    ld    r21,-44(r12)             #restore r21
    ld    r22,-40(r12)             #restore r22
    ld    r23,-36(r12)             #restore r23
    ld    r24,-32(r12)             #restore r24
    ld    r25,-28(r12)             #restore r25
    ld    r26,-24(r12)             #restore r26
    ld    r27,-20(r12)             #restore r27
    ld    r28,-16(r12)             #restore r28
    ld    r29,-12(r12)             #restore r29
    ld    r30,-8(r12)              #restore r30
    ld    r31,-4(r12)              #restore r31
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
L..exp0:
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
    .vbyte  4, L..exp0-.exp
    .vbyte  2, 0x0006
    .byte   "exp"
    .csect constants[RW], 3
    .globl constants[RW]
    .align 2
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000008
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000008
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000080
    .vbyte 4, 0x00000000
    .vbyte 4, 0x00000080
    .vbyte 4, 0x00000000
    .vbyte 4, 0x000000ff
    .vbyte 4, 0x00000000
    .vbyte 4, 0x000000ff
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
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x3f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x1f800000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .vbyte 4, 0x7fc00000
    .toc
L..constants:
    .tc constants[TC],constants[RW]
