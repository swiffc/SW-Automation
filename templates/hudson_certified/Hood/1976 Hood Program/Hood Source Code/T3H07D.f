C          DATA SET T3H07D     AT LEVEL 003 AS OF 03/16/77              00000000
      SUBROUTINE T3H07D(I,J,II,ID,ICOUNT,ZIP)                           00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DIMENSION FRAME(7), SPACER(112)                                   00000090
      DATA SPACER/ 4*0.,  3*0.,6.,  3*0.,12.,  3*0.,18.,  3*0.,24.,     00000100
     1 3*0.,30.,  2*0.,2*12.,  2*0.,12.,18.,  2*0.,12.,24.,             00000110
     2 2*0.,15.,24.,  2*0.,18.,24.,  2*0.,21.,24.,  2*0.,2*24.,         00000120
     3 2*0.,27.,24.,  0.,12.,18.,24.,  0.,9.,2*24.,  0.,12.,2*24.,      00000130
     4 0.,15.,2*24.,  0.,18.,2*24.,  0.,21.,2*24.,  0.,3*24.,           00000140
     5 0.,24.,27.,24.,  12.,2*21.,24.,  15.,2*21.,24.,  12.,3*24.,      00000150
     6 15.,3*24.,  18.,3*24.,  21.,3*24./                               00000160
      IF(IRUN.EQ.0)WRITE(NWTR,100)                                      00000170
  100 FORMAT ('0',23X,'"D" ANGLE FRAME ALIGNMENT COORDINATES :')        00000180
      LINE = LINE + 1                                                   00000190
      DO 110 II = 1,ICOUNT                                              00000200
      IF (II .NE. 1 .AND. WIDTH(II) .EQ. WIDTH(II-1))  GO TO 30         00000210
C                                                                       00000220
      ID = WIDTH(II) / 6.0 - 1.0                                        00000230
      ZIP = ID                                                          00000240
      ZIP = WIDTH(II) - (ZIP * 6.0 + 6.0)                               00000250
      IF (ID .GT. 6)  ZIP = ZIP * .5                                    00000260
      ID = ID * 4 - 3                                                   00000270
C                                                                       00000280
      J = 1                                                             00000290
      DO 20 I = 1,4                                                     00000300
      FRAME(I) = SPACER(ID+I-1)                                         00000310
      IF (FRAME(I) .EQ. 0.0 .OR. J .EQ. 2)  GO TO 10                    00000320
      J = 2                                                             00000330
      FRAME(I) = FRAME(I) + ZIP                                         00000340
   10 FRAME(8-I) = FRAME(I)                                             00000350
   20 CONTINUE                                                          00000360
C                                                                       00000370
   30 IF (ID .EQ. 1)  X(K1+1) = X(K1+1) + ZIP                           00000380
C                                                                       00000390
      DO 50 I = 1,8                                                     00000400
      IF (I .EQ. 1)  GO TO 40                                           00000410
      IF (FRAME(I-1) .EQ. 0.0)  GO TO 50                                00000420
      X(K1+1) = X(K1) + FRAME(I-1)                                      00000430
      Y(K1+1) = Y(K1)                                                   00000440
   40 IF (X(K1+1) .GE. XLONG(MARK))  RETURN                             00000450
      CALL T3H01D                                                       00000460
   50 CONTINUE                                                          00000470
C                                                                       00000480
      IF (II .GE. ICOUNT)  GO TO 110                                    00000490
      X(K1+1) = X(K1) + SPACE(II) + 12.                                 00000500
      IF (ID .EQ. 1)  X(K1+1) = X(K1+1) + ZIP                           00000510
      Y(K1+1) = Y(K1)                                                   00000520
  110 CONTINUE                                                          00000530
      RETURN                                                            00000540
      END                                                               00000550
