C          DATA SET T3H18D     AT LEVEL 002 AS OF 03/20/81              00000000
      SUBROUTINE T3H18D (ISPLIT,I,J,K,L,M,N,LL,K3,K4,XX,YY,IGO,M1,M2,M3,00000010
     & X60,HOLD1,HOLD2,IX,IY,IXY,II,YLOAD)                              00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      IF (ISPLIT .EQ. 1)  GO TO 10                                      00000100
      N = 7                                                             00000110
      IF (XWIDE(MARK) .GT. 48.5)  N = N + 1                             00000120
      GO TO 20                                                          00000130
   10 N = 0                                                             00000140
   20 CALL T3H16D (LL,J,K,XX,YY,YLOAD,-3.,N)                            00000150
      N = ISPLIT                                                        00000160
      KOUNT = -1                                                        00000170
      LINE = 25                                                         00000180
      IY = YLOAD * 1000. + .5                                           00000190
   40 IGO = 4                                                           00000200
      M1 = 2                                                            00000210
      M2 = 3                                                            00000220
      M3 = 12                                                           00000230
   50 CALL T3H11D (I,J,II,IGO,-3000,IY,IXY,M1,M2,M3,6)                  00000240
      M3 = 1                                                            00000250
      X60 = 0.0                                                         00000260
      K4 = 0                                                            00000270
      DO 60 L = 1,LL                                                    00000280
      CALL T3H13D (K3,K4,X60,XX,YY,I,J,K,M,HOLD1,HOLD2)                 00000290
      CALL T3H14D (K,K3,K4,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3)           00000300
      IF (K4 .LT. K1)  GO TO 55                                         00000310
      GO TO (70,110),  N                                                00000320
   55 CALL T3H15D (XX,X60,   I,J,II,IGO,IX,IY,IXY,M1,M2,M3,K4)          00000330
   60 CONTINUE                                                          00000340
   70 IF (K2 .EQ. 0)  GO TO 75                                          00000350
      K = 2                                                             00000360
      GO TO 80                                                          00000370
   75 K = 1                                                             00000380
   80 CALL T3H11D (I,J,II,1,IX,IY,IXY,8,M2,M3,K)                        00000390
      GO TO (120,85),  K                                                00000400
   85 DO 90 L = 1,K2                                                    00000410
      M = 101 - L                                                       00000420
      CALL T3H12D (XX,YY,M,L)                                           00000430
   90 CONTINUE                                                          00000440
      K1 = K2                                                           00000450
      K2 = 0                                                            00000460
      IF (N .EQ. 2)  GO TO 100                                          00000470
      IY = (XWIDE(MARK) - 46.) * 1000. + .5                             00000480
      GO TO 40                                                          00000490
  100 N = 1                                                             00000500
      M1 = 3                                                            00000510
      M2 = 4                                                            00000520
      IGO = 2                                                           00000530
      GO TO 50                                                          00000540
  110 CALL T3H17D (M1,M2,M3,I,J,II,IX,IY,IXY,K3,K4,L,M,LL,K,X60,IGO)    00000550
      IF (K2 .NE. 0)  GO TO 85                                          00000560
  120 RETURN                                                            00000570
      END                                                               00000580
