C          DATA SET T3H14D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H14D(K,K3,K4,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3)      00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
C   THIS SUBROUTINE EDITS & WRITES AN ORDERED COLLECTION OF COORDINATES.00000090
C   NOTE - FURTHER DEVELOPMENT MAY BE REQ'D TO EXPAND THE SCOPE OF SITUA00000100
      DO 130 K = K3,K4                                                  00000110
      GO TO ( 40, 10, 20,140, 20, 30, 30,140, 45, 10,140), M1           00000120
   10 M1 = 1                                                            00000130
      GO TO 50                                                          00000140
   20 M1 = 6                                                            00000150
      IF (K .EQ. 1) GO TO 50                                            00000160
      IF (Y(K).LT.12.0 .OR. Y(K).GT.28.0 .OR. Y(K).NE.Y(K-1)) GO TO 50  00000170
      GO TO 70                                                          00000180
   30 M1 = 1                                                            00000190
   40 IF (M3 .EQ. 10)  GO TO 50                                         00000200
      IF (M3 .NE. 7)  GO TO 45                                          00000210
      M1 = M3                                                           00000220
      M3 = 1                                                            00000230
   45 IF (Y(K) .EQ. Y(K-1)) GO TO 70                                    00000240
      IF (X(K) .EQ. X(K-1)) GO TO 60                                    00000250
   50 M2 = 1                                                            00000260
      IGO = 4                                                           00000270
      GO TO 80                                                          00000280
   60 IGO = 3                                                           00000290
      GO TO 80                                                          00000300
   70 IGO = 2                                                           00000310
   80 IF (X(K) .LT. 0.)  GO TO 90                                       00000320
      IX = (X(K) - X60) * 1000. + .5                                    00000330
      GO TO 100                                                         00000340
   90 IX = (X(K) - X60) * 1000. - .5                                    00000350
  100 IF (Y(K) .LT. 0.)  GO TO 110                                      00000360
      IY = Y(K) * 1000. + .5                                            00000370
      GO TO 120                                                         00000380
  110 IY = Y(K) * 1000. - .5                                            00000390
  120 CALL T3H11D (I,J,II,IGO,IX,IY,IXY,M1,M2,1,1)                      00000400
  130 CONTINUE                                                          00000410
  140 RETURN                                                            00000420
      END                                                               00000430
