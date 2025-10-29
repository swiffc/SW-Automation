C          DATA SET T3H22D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H22D(II,ID,I,J,K,M,N,ED,DIST)                        00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      MARK = II                                                         00000090
      GO TO (200,200,10,40,40,40,40,200,200,200,200,50,20),  MARK       00000100
C                                                                       00000110
   10 Y(2) = 1.875                                                      00000120
      MARK2 = 1                                                         00000130
      GO TO 30                                                          00000140
C                                                                       00000150
   20 Y(2) = 2.125                                                      00000160
      MARK2 = 2                                                         00000170
C                                                                       00000180
   30 Y(1) = 1.875                                                      00000190
      ID = 2                                                            00000200
      X(ID) = ED                                                        00000210
      GO TO 60                                                          00000220
C                                                                       00000230
   40 ID = 1                                                            00000240
      MARK2 = ID                                                        00000250
      Y(ID) = 1.125                                                     00000260
      GO TO 60                                                          00000270
C                                                                       00000280
   50 Y(1) = 1.75                                                       00000290
      Y(2) = 1.875                                                      00000300
      ID = 2                                                            00000310
      IF (NUMBER(4) .EQ. 0)  MARK2 = ID                                 00000320
      X(ID) = ED + 1.0                                                  00000330
C                                                                       00000340
   60 X(1) = ED                                                         00000350
      DO 80 I = 1,N                                                     00000360
      DO 70 J = 1,ID                                                    00000370
      X(I*ID+J) = X((I-1)*ID+J) + DIST                                  00000380
   70 CONTINUE                                                          00000390
   80 CONTINUE                                                          00000400
      M= N + 1                                                          00000410
C                                                                       00000420
      IF (MARK .EQ. 12 .AND. MARK2 .EQ. 1)  GO TO 90                    00000430
      GO TO 120                                                         00000440
C                                                                       00000450
   90 M = M + M                                                         00000460
      X(M+1) = X(M-1) + 8.0                                             00000470
      X(M+2) = X(M) + 6.0                                               00000480
      DO 110 I = 1,N                                                    00000490
      DO 100 J = 1,ID                                                   00000500
      X(I*ID+J+M) = X((I-1)*ID+J+M) + DIST                              00000510
  100 CONTINUE                                                          00000520
  110 CONTINUE                                                          00000530
C                                                                       00000540
  120 X(M*ID+1) = XLONG(MARK)                                           00000550
      CALL T3H21D (ID,I,J,K,M)                                          00000560
C                                                                       00000570
  200 RETURN                                                            00000580
      END                                                               00000590
