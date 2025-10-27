C          DATA SET T3H16D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H16D (I,J,K,XX,YY,YLOAD,XLOAD,NEXTRA)                00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
C         THIS SUBROUTINE SORTS AND COUNTS ALL ELEMENTS IN EACH SET; DET00000090
C         THE FIRST COORDINATE IN EACH SET; SETS THE MAT'L CLAMPS; AND D00000100
C         MINES THE TOTAL NO. OF PAGES REQ'D FOR EDITING THE PROGRAM SHE00000110
      K = K1                                                            00000120
      KOUNT = K                                                         00000130
      K1 = 0                                                            00000140
      K2 = 101                                                          00000150
      DO 100 I = 1,KOUNT                                                00000160
C         SEPARATE 'ARRAY' INTO TWO PARTS, IF REQ'D                     00000170
      IF (Y(I) .GT. 48.0) GO TO 20                                      00000180
C         'K1' COUNTS ELEMENTS IN PART 1                                00000190
      K1 = K1 + 1                                                       00000200
      CALL T3H12D(XX,YY,I,K1)                                           00000210
C         CHOOSE THE FIRST ELEMENT IN PART 1                            00000220
      IF (SQRT((YLOAD-Y(K1))**2 + (XLOAD-X(K1))**2)  .LT.               00000230
     *    SQRT((YLOAD-Y(1))**2  + (XLOAD-X(1))**2))                     00000240
     *    CALL T3H12D (XX,YY,K1,1)                                      00000250
      GO TO 30                                                          00000260
C                                                                       00000270
C         'K2' COUNTS ELEMENTS IN PART 2                                00000280
   20 K2 = K2 - 1                                                       00000290
      X(I) = XLONG(MARK) - X(I)                                         00000300
      Y(I) = XWIDE(MARK) - Y(I)                                         00000310
      CALL T3H12D(XX,YY,I,K2)                                           00000320
      IF (SQRT((YLOAD-Y(K2))**2 + (XLOAD-X(K2))**2)  .LT.               00000330
     *    SQRT((YLOAD-Y(100))**2+ (XLOAD-X(100))**2))                   00000340
     *    CALL T3H12D (XX,YY,K2,100)                                    00000350
   30 IF (YY .GE. 1.21875)  GO TO 100                                   00000360
      K = K + 1                                                         00000370
      LINE = XLONG(MARK)/60.0 + 1.0                                     00000380
      IF (LINE .EQ. 1)  GO TO 80                                        00000390
      J = XX/60.0 + 1.0                                                 00000400
      IF (LINE .NE. J)  GO TO 80                                        00000410
      X(K) = XX - XLONG(MARK) + 57.0                                    00000420
      GO TO 100                                                         00000430
   80 X(K) = XX                                                         00000440
      DO 90 J = 1,LINE                                                  00000450
      IF (X(K) .LE. 60.)  GO TO 100                                     00000460
      X(K) = X(K) -60.0                                                 00000470
   90 CONTINUE                                                          00000480
  100 CONTINUE                                                          00000490
      K2 = -K2 + 101                                                    00000500
      IF (K2 .EQ. 0) GO TO 200                                          00000510
C         ADD "NICK" HOLE USED FOR LOADING THE SECOND PHASE OF OPERATION00000520
C         ROTATING THE PLATE 180 DEGREES                                00000530
      K1 = K1 + 1                                                       00000540
      X(K1) = XLONG(MARK) + .34375                                      00000550
      Y(K1) = XWIDE(MARK) - 23.                                         00000560
      IF (Y(K1) .GT. 48.0)  Y(K1) = 48.0                                00000570
C         PRE-SET THE MATERIAL CLAMPS AT "TOP" OF PLATE                 00000580
  200 XX = 5.                                                           00000590
      IF (XLONG(MARK) .LT. 55.)  GO TO 210                              00000600
      YY = 50.                                                          00000610
      GO TO 220                                                         00000620
C                                                                       00000630
  210 YY = XLONG(MARK) - 5.                                             00000640
  220 IF (K .LE. KOUNT)  GO TO 300                                      00000650
C                                                                       00000660
      LINE = K - KOUNT                                                  00000670
      KOUNT = KOUNT + 1                                                 00000680
      DO 260 J = 1,LINE                                                 00000690
      DO 250 I = KOUNT,K                                                00000700
      IF (X(I) - 27.5)  230,250,240                                     00000710
  230 IF (X(I) .GT. (XX-3.) .AND. X(I) .LT. (XX+3.))  XX = X(I) + 3.0   00000720
      GO TO 250                                                         00000730
  240 IF (X(I) .GT. (YY-3.) .AND. X(I) .LT. (YY+3.))  YY = X(I) - 3.0   00000740
  250 CONTINUE                                                          00000750
  260 CONTINUE                                                          00000760
  300 MCLMP1 = XX + .5                                                  00000770
      MCLMP2 = YY + .5                                                  00000780
C         COUNT NO. OF RE-POSITION CYCLES REQ'D                         00000790
      I = (XLONG(MARK) + 3.) / 60.1                                     00000800
C         BEGIN COUNT OF TOTAL NO. OF "BLOCKS" REQ'D IN PROGRAM/TAPE    00000810
      J = I * 2 + K1 + 1                                                00000820
      IF (K2 .NE. 0) J = J + (I*2) + K2 + 2                             00000830
C         ADD EXTRA BLOCKS, IF REQ'D                                    00000840
      XX = J + NEXTRA                                                   00000850
C         COUNT PAGES OF PROGRAM SHEETS                                 00000860
      NPAGE = 1                                                         00000870
      NPAGES = (XX-16.) / 22. + 2.                                      00000880
      I = I + 1                                                         00000890
      RETURN                                                            00000900
      END                                                               00000910
