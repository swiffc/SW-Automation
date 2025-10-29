C          DATA SET T3H09D     AT LEVEL 003 AS OF 09/08/76              00000000
      SUBROUTINE T3H09D (INDEX,NSYM,IBOLT,SETBAC,A,CORNER,SLOPE,BEVEL,  00000010
     * FANCL,ISPLIT,I,J,K,L,II,JJ,LL,WP,CODE1,I1,XD,B,SIN,COS,ZAP,CODE2,00000020
     * I2,CODE3,I3,CODE4,I4,LTUBE,ICOUNT,D,CODE5,I5,G5)                 00000030
      COMMON IRUN, IDATEX(3)                                             00000040
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000050
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000060
     *,INTS                                                             00000070
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000080
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000090
      COMMON/INOUT/NRDR,NWTR                                            00000100
C   INDEX CLASSIFIES THE TYPE OF PANEL :                                00000110
C        (1) = END PANEL (COULD BE EITHER MARKED #1 OR #14              00000120
C        (2) = SIDE PANEL (MARKED #2 ONLY)                              00000130
C        (3) = COMMON CENTER PANEL (COULD BE MARKED #11 OR #15)         00000140
      INDEX = MARK                                                      00000150
      IF (MARK .EQ. 14)  INDEX = 1                                      00000160
      IF (MARK .EQ. 11 .OR. MARK .EQ. 15)  INDEX = 3                    00000170
C        "SETBAC" = ONE-HALF OF THE AMT. OF MAT'L SAVED WHEN BENDING THE00000180
C                   OR BOTTOM FLANGES.                                  00000190
C        "BEVEL" = THE TANGENT (EXPRESSED TO FEET), BY WHICH THE PANEL I00000200
C        "TW" = THE TOTAL PLATE WIDTH TO BE SHEARED FOR FABRICATION.    00000210
C        "ISPLIT" = THE CODE BY WHICH DETERMINES WHETHER THIS IS A WHOLE00000220
C                   HALF-PANEL(2).                                      00000230
C        "TL" = TOTAL LENGTH OF PLATE TO BE SHEARED.                    00000240
      SETBAC = SQRT((1. + A/SLOPE) / (1. - A/SLOPE)) * .1875            00000250
      BEVEL = A/H1*12.                                                  00000260
      XWIDE(MARK) = SLOPE - (SETBAC * 2.) + 5.625                       00000270
      IF (ISHIP .NE. 0 .AND. INDEX .NE. 2)  GO TO 10                    00000280
      IF (FANCL .GT. 120.)  GO TO 10                                    00000290
        ISPLIT = 1                                                      00000300
      XLONG(MARK) = FANCL * 2.                                          00000310
      GO TO 20                                                          00000320
   10   ISPLIT = 2                                                      00000330
      XLONG(MARK) = FANCL + 2.625                                       00000340
C   DETERMINE THE TOTAL NO. OF PANELS REQ'D.                            00000350
   20 K = NOFAN*2                                                       00000360
      GO TO (30,70,40), INDEX                                           00000370
   30 IF (NUMBER(11) .NE. 0)  K = 2                                     00000380
      GO TO 50                                                          00000390
   40 K = NOFAN - 1                                                     00000400
   50 GO TO (70,60), NSYM                                               00000410
   60 MARK2 = NSYM                                                      00000420
      GO TO 80                                                          00000430
   70 K = K * ISPLIT                                                    00000440
      MARK2 = ISPLIT                                                    00000450
   80 NUMBER(MARK) = K * LUNITS                                         00000460
      LINE = 30                                                         00000470
      K1 = 0                                                            00000480
      KOUNT = IBOLT                                                     00000490
      CALL T3H01D                                                       00000500
C         BEGIN BOLT COORDINATE CALCULATIONS                            00000510
C         "TOP PANEL" ALIGNMENT                                         00000520
      CALL T3H03D       (K,WP,CODE1,I1,XD,I,J,ISPLIT,INDEX)             00000530
C         "CORNER SPLICE ANGLE" ALIGNMENT                               00000540
      SIN = B/CORNER                                                    00000550
      COS = SLOPE/CORNER                                                00000560
      CALL T3H04D(L,SIN,COS,ZAP,WP,CODE2,I2,XD,ISPLIT,K,I,J)            00000570
C         "VERTICAL STIFFENER" ALIGNMENT                                00000580
      CALL T3H05D       (ISPLIT,K,A,CODE3,I3,XD,I,J,SLOPE)              00000590
C         "DIAGONAL STIFFENER" ALIGNMENT                                00000600
      CALL T3H06D (ISPLIT,SIN,COS,A,K,CODE4,I4,XD,I,J)                  00000610
      Y(K1+1) = XWIDE(MARK) - 1.25                                      00000620
      GO TO (130,140,120),  INDEX                                       00000630
  120 Y(K1+1) = Y(K1+1) + .25                                           00000640
  130 X(K1+1) = 2.75                                                    00000650
      IF (B  .GT. 0.)  X(K1+1) = X(K1+1) + B                            00000660
C         "TUBE BUNDLE SPACER SEAL" ALIGNMENT                           00000670
      CALL T3H07D(I,J,II,JJ,ICOUNT,SIN)                                 00000680
      GO TO 150                                                         00000690
C         "TUBE BUNDLE FRAME" ALIGNMENT                                 00000700
  140 CALL T3H08D(ISPLIT,B,D,CODE5,I5,I,J,K,SIN,G5)                     00000710
  150 IBOLT = KOUNT                                                     00000720
      XD = .0625                                                        00000730
      I = XWIDE(MARK) / XD + .5                                         00000740
      XWIDE(MARK) = XD * I                                              00000750
      RETURN                                                            00000760
      END                                                               00000770
