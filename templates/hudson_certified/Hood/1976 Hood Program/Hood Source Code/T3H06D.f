C          DATA SET T3H06D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H06D (ISPLIT,ZIP,ZOP,A,K,CODE4,I4,XD,I,JJ)           00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      IF(IRUN.EQ.0)WRITE(NWTR,100)                                      00000090
  100 FORMAT ('0',23X,'DIAGONAL STIFFENER COORDINATES :')               00000100
      LINE = LINE + 1                                                   00000110
C   PLOT FIRST COORDINATE NEAR THE LAST HOLE OF VERTICAL CALCULATIONS.  00000120
      X(K1+1) = X(K1) - 4.                                              00000130
      GO TO (10,20),  ISPLIT                                            00000140
   10 Y(K1+1) = Y(K1)                                                   00000150
      GO TO 30                                                          00000160
C   ON A SPLIT PANEL, (K1) & (K1-1) ARE BEND-RELIEF COORDINATES AND ARE 00000170
C   CONSEQUENCE HERE.                                                   00000180
   20 Y(K1+1) = Y(K1-2)                                                 00000190
   30 ZIP = F - 10.                                                     00000200
      ZOP = Y(K1+1) - 5.875                                             00000210
      IF (MARK .NE. 2 .AND. NUMBER(11) .NE. 0)  ZOP = ZOP - 1.5625      00000220
      CODE4 = SQRT(ZOP**2 + ZIP**2)                                     00000230
C   "CODE4" IS NOW THE MAX. DIST. BETWEEN EXTREMES.                     00000240
      CALL T3H00D (24.,CODE4,I4,XD,JJ,.25)                              00000250
C   "CODE4" IS NOW THE FINITE DIST. BETWEEN EACH BOLT.                  00000260
      XD = CODE4 * XD                                                   00000270
C   "XD" IS NOW THE FINITE DIST. BETWEEN EXTREMES.                      00000280
      ZIP = -(SQRT(XD**2 - ZOP**2)) / XD                                00000290
      ZOP = -ZOP / XD                                                   00000300
C   "ZIP" & "ZOP" ARE THE SINE & COSINE OF DIRECTION OF THE DIAG. STIFFE00000310
      CALL T3H02D (2,ISPLIT,K,I,I4,JJ,ZIP,ZOP,CODE4)                    00000320
      RETURN                                                            00000330
      END                                                               00000340
