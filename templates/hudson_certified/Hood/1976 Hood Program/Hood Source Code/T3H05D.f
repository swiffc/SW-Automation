C          DATA SET T3H05D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H05D (ISPLIT,K,A,CODE3,I3,XD,I,JJ,SLOPE)             00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      IF(IRUN.EQ.0)WRITE(NWTR,100)                                      00000090
  100 FORMAT ('0',23X,'VERTICAL ALIGNMENT COORDINATES :')               00000100
      LINE = LINE + 1                                                   00000110
      Y(K1+1) = 5.875                                                   00000120
      GO TO (10,20),  ISPLIT                                            00000130
C   THIS INDICATES A WHOLE PANEL WITH A VERT. STIFF. BOLTED RIGHT ON THE00000140
   10 X(K1+1) = XLONG(MARK) / 2.                                        00000150
C   IF THIS IS A COMMON PANEL-TYPE UNIT, DROP THE STIFFENER 1.5625" .   00000160
      IF (MARK .NE. 2 .AND. NUMBER(11) .NE. 0)  Y(K1+1) = 7.4375        00000170
      XD = 1.5                                                          00000180
      IF (A .LT. 0.)  XD = XD - (A / H1 * 2.)                           00000190
      GO TO 30                                                          00000200
C                                                                       00000210
C   THIS INDICATES A HALF-PANEL WITH AN ATTACHED FLANGE SERVING AS A VER00000220
   20 X(K1+1) = XLONG(MARK) - 1.25                                      00000230
      XD = 3.                                                           00000240
   30 CODE3 = SLOPE - XD - Y(K1+1) + 2.875                              00000250
C   "CODE3" IS NOW THE MAX. DIST. BETWEEN TWO EXTREME LOCATIONS.        00000260
      CALL T3H00D (21.,CODE3,I3,XD,JJ,.25)                              00000270
C   "CODE3" IS NOW THE FINITE DIST. BETWEEN EACH BOLT.                  00000280
      CALL T3H02D (2,2,K,I,I3,JJ,0.,1.,CODE3)                           00000290
      IF (ISPLIT .EQ. 1)  RETURN                                        00000300
      KOUNT = KOUNT - (NUMBER(MARK)*(I3+1)/ISPLIT) - (NUMBER(MARK)*8)   00000310
C   RELIEF PURPOSES. ALSO, PLOT THE COORDINATES REQ'D TO "BLOCK-OUT" THE00000320
      IF(IRUN.EQ.0)WRITE(NWTR,110)                                      00000330
  110 FORMAT ('0',23X,'"BEND RELIEF" COORDINATES FOR SPLIT PANEL :')    00000340
      LINE = LINE + 1                                                   00000350
      X(K1+1) = X(K1) - 1.375                                           00000360
      Y(K1+1) = 2.6875                                                  00000370
      CALL T3H01D                                                       00000380
      X(K1+1) = X(K1)                                                   00000390
      Y(K1+1) = SLOPE + 2.875                                           00000400
      CALL T3H01D                                                       00000410
      K = K1                                                            00000420
      IF(IRUN.EQ.0)WRITE(NWTR,120)                                      00000430
  120 FORMAT ('0',23X,'"BLOCK-OUT" COORDINATES FOR SPLIT PANEL :')      00000440
      LINE = LINE + 1                                                   00000450
      X(K+1) = X(K) + .8125                                             00000460
      X(K+2) = X(K+1)                                                   00000470
      X(K+3) = X(K+2) + 1.375                                           00000480
      X(K+4) = X(K+3)                                                   00000490
      X(K+5) = X(K+2)                                                   00000500
      X(K+6) = X(K+1)                                                   00000510
      Y(K+1) = .5                                                       00000520
      Y(K+2) = Y(K+1) + 1.375                                           00000530
      Y(K+3) = Y(K+2)                                                   00000540
      Y(K+4) = Y(K) + .8125                                             00000550
      Y(K+5) = Y(K+4)                                                   00000560
      Y(K+6) = XWIDE(MARK) - .5                                         00000570
      DO 130 I = 1,6                                                    00000580
      CALL T3H01D                                                       00000590
      XBLOC(I) = X(K1)                                                  00000600
      YBLOC(I) = Y(K1)                                                  00000610
  130 CONTINUE                                                          00000620
      K1 = K                                                            00000630
      RETURN                                                            00000640
      END                                                               00000650
