C          DATA SET T3H03D     AT LEVEL 002 AS OF 10/04/79              00000000
      SUBROUTINE T3H03D (K,WP,CODE1,I1,XD,I,JJ,ISPLIT,INDEX)            00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      IF(IRUN.EQ.0)WRITE(NWTR,100)                                      00000090
  100 FORMAT ('0',23X,'TOP PANEL ALIGNMENT COORDINATES :')              00000100
      LINE = LINE + 1                                                   00000110
      X(1) = WP + 4.                                                    00000120
      Y(1) = 1.25                                                       00000130
C   "CODE1 = PRELIM. MAX. DIST. BETWEEN EXTREME LOCATIONS.              00000140
      CODE1 = F - 7.                                                    00000150
      CALL T3H00D (25.,CODE1,I1,XD,JJ,0.0)                              00000160
C   "CODE1" = IS NOW THE FINITE DIST. BETWEEN EACH BOLT.                00000170
      I1 = I1 - 1                                                       00000180
C   GO PLOT ALL BUT THE LAST COORDINATE.  THE LAST ONE HAS A DIFFERENT G00000190
C        BECAUSE IT IS COMMON TO THE HOOD PANEL, TOP PANEL, AND THE FAN 00000200
      CALL T3H02D (2,2,K,I,I1,JJ,1.,0.,CODE1)                           00000210
      I1 = I1 + 1                                                       00000220
      X(K1+1) = X(K1) + CODE1                                           00000230
      Y(K1+1) = SQRT((F-1.875)**2 - 9.) - F + 2.875                     00000240
      CALL T3H01D                                                       00000250
      IF(ISPLIT.EQ.1) CALL T3H02D(1,ISPLIT,K,I,I1,JJ,1.,0.,CODE1)       00000260
      IF (INDEX .LT. 3)  RETURN                                         00000270
C   IF THIS IS A COMMON PANEL, THEN PLOT THE COORDINATES FOR THE COMMON 00000280
C        STIFFENER.                                                     00000290
      IF(IRUN.EQ.0)WRITE(NWTR,110)                                      00000300
  110 FORMAT ('0',23X,'COMMON PANEL STIFFENER ALIGNMENT COORDINATES :') 00000310
      LINE = LINE + 1                                                   00000320
      X(K1+1) = WP + 3.                                                 00000330
      Y(K1+1) = 4.4375                                                  00000340
      CALL T3H02D (2,ISPLIT,K,I,I1,JJ,1.,0.,CODE1)                      00000350
      KOUNT = KOUNT + (NUMBER(MARK)*(I1+1)*2/ISPLIT)                    00000360
      RETURN                                                            00000370
      END                                                               00000380
