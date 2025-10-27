C          DATA SET T3H04D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H04D(L,SIN,COS,ZAP,WP,CODE2,I2,XD,ISPLIT,K,I,J)      00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DATA SET/.707106781/                                              00000090
C                                                                       00000100
      XD = -SET                                                         00000110
      IF (SIN .LE. XD)  GO TO 10                                        00000120
C                                                                       00000130
      IF(IRUN.EQ.0)WRITE(NWTR,100)                                      00000140
  100 FORMAT('0',23X,'TOP "SHEAR CUT-OFF" COORDINATES :')               00000150
      LINE = LINE + 1                                                   00000160
      X(K1+1) = WP + 2.388864088                                        00000170
      Y(K1+1) = 0.0                                                     00000180
      CALL T3H02D(2,ISPLIT,K,I,1,J,XD,SET,3.378363992)                  00000190
      KOUNT = KOUNT - (NUMBER(MARK) * 4 / ISPLIT)                       00000200
C                                                                       00000210
   10 IF(IRUN.EQ.0)WRITE(NWTR,105)                                      00000220
  105 FORMAT ('0',23X,'CORNER SPLICE ANGLE ALIGNMENT COORDINATES :')    00000230
      LINE = LINE + 1                                                   00000240
C   PLOT FIRST COORDINATE.  "ZAP" = DIST. FROM TOP WORK POINT TO THE TAN00000250
C   THE FIRST HOLE.  "2.0" = EDGE DISTANCE TO THE FIRST HOLE.           00000260
      L = K1 + 1                                                        00000270
      X(L) = (SIN*ZAP) + (COS*2.0) + WP                                 00000280
      Y(L) = (COS*ZAP) - (SIN*2.0) + 2.875                              00000290
C   "CODE2" IS A PRE-DETERMINED MAX. DIST. BETWEEN EXTREMES.            00000300
      IF (MARK .EQ. 1)  CALL T3H00D (21.,CODE2,I2,XD,J,.25)             00000310
C   "CODE2" IS NOW THE FINITE DIST. BETWEEN BOLTS.                      00000320
      CALL T3H02D (2,ISPLIT,K,I,I2,J,SIN,COS,CODE2)                     00000330
      IF (SIN .EQ. 0.0)  RETURN                                         00000340
C   IF THIS PANEL NEEDS THE CORNERS SHEARED OFF AFTER THE PANEL-MASTER  00000350
C   OPERATION, THEN PLOT TWO HOLES (NO BOLTS) THAT WILL MARK THE EDGE OF00000360
C   MAT'L TO BE CUT OFF.                                                00000370
      IF(IRUN.EQ.0)WRITE(NWTR,110)                                      00000380
  110 FORMAT ('0',23X,'CORNER "SHEAR CUT-OFF" COORDINATES :')           00000390
      LINE = LINE + 1                                                   00000400
      X(K1+1) = X(L) - COS*2.34375                                      00000410
      Y(K1+1) = Y(L) + SIN*2.34375                                      00000420
      XD = CODE2 * I2                                                   00000430
      CALL T3H02D (2,ISPLIT,K,I,1,J,SIN,COS,XD)                         00000440
      KOUNT = KOUNT - (NUMBER(MARK) * 4 / ISPLIT)                       00000450
      RETURN                                                            00000460
      END                                                               00000470
