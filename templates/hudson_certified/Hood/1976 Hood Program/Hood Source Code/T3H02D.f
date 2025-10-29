C          DATA SET T3H02D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H02D (IGO1,IGO2,N,I,II,JJ,SIN,COS,ZIP)               00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
C   "IGO1" DIVIDES THIS SUBROUTINE INTO THREE MAJOR PARTS :             00000090
      N = IGO1                                                          00000100
   10 K2 = K1 + 1                                                       00000110
      JJ = K2 + II                                                      00000120
      DO 500 I = K2,JJ                                                  00000130
      GO TO (100,200,300)  , N                                          00000140
C        (1) PLOTS 'X','Y' SYMMETRICAL ABOUT THE TRUE VERTICAL CENTER.  00000150
C            PATTERN OF COORDINATES NEED NOT BE IN A LINE NOR EQUALLY SP00000160
C             "II" = TOTAL NO. OF SPACES REQ'D.                         00000170
C             "XLONG" = THE LENGTH OF THE PANEL.                        00000180
  100 X(I) = XLONG(MARK) - X(K2+K2-I-1)                                 00000190
      Y(I) = Y(K2+K2-I-1)                                               00000200
      GO TO 400                                                         00000210
C        (2) PLOTS 'X','Y' IN A LINEAR (MUST BE EQUALLY SPACED) MOVEMENT00000220
C             "II" = SAME AS ABOVE.                                     00000230
C             "ZIP" = THE LENGTH OF EACH EQUAL SPACE.                   00000240
C             "SIN" & "COS" = THE DIRECTION OF THE LINEAR COORDINATES.  00000250
  200 IF (I .EQ. K2)  GO TO 400                                         00000260
      X(I) = X(I-1) + SIN*ZIP                                           00000270
      Y(I) = Y(I-1) + COS*ZIP                                           00000280
      GO TO 400                                                         00000290
C        (3) PLOTS 'X','Y' SYMMETRICAL ABOUT THE TRUE HORIZONTAL CENTER.00000300
C            PATTERN OF COORDINATES NEED NOT BE IN A LINE NOR EQUALLY SP00000310
C             "II" = SAME AS ABOVE.                                     00000320
C             "XWIDE" = THE WIDTH OF THE PANEL.                         00000330
  300 X(I) = X(K2+K2-I-1)                                               00000340
      Y(I) = XWIDE(MARK) - Y(K2+K2-I-1)                                 00000350
  400 CALL T3H01D                                                       00000360
  500 CONTINUE                                                          00000370
      IF (N .EQ. IGO2)   RETURN                                         00000380
      N = IGO2                                                          00000390
      GO TO 10                                                          00000400
      END                                                               00000410
