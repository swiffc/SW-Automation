C          DATA SET T3H17D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H17D(M1,M2,M3,I,J,II,IX,IY,IXY,K3,K4,L,M,LL,K,X60,   00000010
     & IGO)                                                             00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      M1 = 3                                                            00000100
      M2 = 4                                                            00000110
      M3 = 1                                                            00000120
      CALL T3H11D (I,J,II,2,60000,IY,IXY,M1,M2,M3,5)                    00000130
      IF (YBLOC(6) .LE. 48.0) GO TO 10                                  00000140
      K4 = 3                                                            00000150
      GO TO 20                                                          00000160
C                                                                       00000170
   10 K4 = 6                                                            00000180
      IF (ABS(Y(K1)-YBLOC(K4)) .LT. ABS(Y(K1)-YBLOC(1)))  GO TO 30      00000190
C                                                                       00000200
   20 J = 1                                                             00000210
      K = J                                                             00000220
      GO TO 40                                                          00000230
C                                                                       00000240
   30 J = K4                                                            00000250
      K = -1                                                            00000260
C                                                                       00000270
   40 DO 50 I = 1,6                                                     00000280
      X(I) = XBLOC(J)                                                   00000290
      Y(I) = YBLOC(J)                                                   00000300
      J = J + K                                                         00000310
   50 CONTINUE                                                          00000320
C                                                                       00000330
      DO 160 LL = 1,K4                                                  00000340
      K3 = LL                                                           00000350
      CALL T3H14D (K,LL,K3,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3)           00000360
      IF (LL .EQ. 2  .AND.  K4 .EQ. 6) M3 = 7                           00000370
  160 CONTINUE                                                          00000380
      M = 1                                                             00000390
      IF (K4 .EQ. 3) M = M + 1                                          00000400
      CALL T3H11D (I,J,II,1,IX,IY,IXY,8,1,1,M)                          00000410
      IF (M .EQ. 1) RETURN                                              00000420
      M1 = 2                                                            00000430
      M2 = 3                                                            00000440
      IY = (XWIDE(MARK) - 46.) * 1000. + .5                             00000450
      IF (IY .LT. 25000)  IY = 25000                                    00000460
      CALL T3H11D (I,J,II,4,-3000,IY,IXY,M1,M2,1,5)                     00000470
      X60 = 0.                                                          00000480
      DO 190 LL = 1,3                                                   00000490
      X(LL) = XLONG(MARK) - X(LL+3)                                     00000500
      Y(LL) = XWIDE(MARK) - Y(LL+3)                                     00000510
      K3 = LL                                                           00000520
      CALL T3H14D (K,LL,K3,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3)           00000530
  190 CONTINUE                                                          00000540
      RETURN                                                            00000550
      END                                                               00000560
