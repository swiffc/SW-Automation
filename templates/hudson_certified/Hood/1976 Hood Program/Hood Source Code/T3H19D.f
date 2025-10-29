C          DATA SET T3H19D     AT LEVEL 002 AS OF 09/08/76              00000000
      SUBROUTINE T3H19D (M,IGO1,IGO2,FANCL,LTUBE,NSHAFT,I,J,K,L,II,LL,  00000010
     * XX,YY,M1,M2,M3,IX,IY,IXY,K3,K4,IGO,X60,ZIP,ZOP,IBOLT,N,A,D,G,I5) 00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      DIMENSION INCRE(12)                                               00000100
      DATA INCRE/-188,50,-137,137,-50,188,50,188,137,137,188,50/        00000110
C                                                                       00000120
      MARK2 = 1                                                         00000130
      MARK3 = MARK2                                                     00000140
      M = 8                                                             00000150
      IGO1 = 1                                                          00000160
      IGO2 = IGO1                                                       00000170
C                                                                       00000180
   10 NUMBER(M) = NUMBER(M) + LUNITS * 2                                00000190
      XWIDE(M) = SPACE(IGO1) + 6.0                                      00000200
      IF (NUMBER(11) .GT. 0)  GO TO 15                                  00000210
C                                                                       00000220
      XLONG(M) = FANCL                                                  00000230
      NUMBER(M+1) = NUMBER(M+1) + (NOFAN-1) * LUNITS *2                 00000240
      GO TO 20                                                          00000250
C                                                                       00000260
   15 XLONG(M) = FANCL - .25                                            00000270
      NUMBER(M+1) = NUMBER(M+1) + (NOFAN-1) * LUNITS                    00000280
      NUMBER(M+2) = NUMBER(M+1)                                         00000290
      XWIDE(M+2) = XWIDE(M)                                             00000300
      XLONG(M+2) = XLONG(M) - 2.5                                       00000310
C                                                                       00000320
   20 XWIDE(M+1) = XWIDE(M)                                             00000330
      XLONG(M+1) = XLONG(M)                                             00000340
C                                                                       00000350
      GO TO (30,210),  IGO2                                             00000360
   30 MARK = M                                                          00000370
      GO TO 50                                                          00000380
   40 MARK = MARK + 1                                                   00000390
   50 K1 = 0                                                            00000400
      LINE = 30                                                         00000410
      CALL T3H01D                                                       00000420
      Y(1) = 1.25                                                       00000430
      CALL T3H08D(3,A,D,G,I5,I,J,K,XX,YY)                               00000440
      IF (MARK .EQ. 8)  N =(II+1) * 2                                   00000450
      IF (IGO1 .EQ. NSHAFT)  GO TO 55                                   00000460
      II = 0                                                            00000470
      GO TO 60                                                          00000480
C                                                                       00000490
   55 X(K1+1) = XLONG(MARK) - 2.375                                     00000500
      Y(K1+1) = XWIDE(MARK) * .5                                        00000510
      CALL T3H01D                                                       00000520
      II = 9                                                            00000530
C                                                                       00000540
   60 IF (MARK .EQ. M)  II = II + 9                                     00000550
C                                                                       00000560
C  60 IF (MARK .NE. M)  GO TO 100                                       00000570
C     II = II + 9                                                       00000580
C     WRITE (8,65)                                                      00000590
C  65 FORMAT('0',23X,'FRAME LIFTING LUG "BLOCK-OUT" COORDINATES :')     00000600
C     LINE = LINE + 1                                                   00000610
C     DO 70 I = 1,64,3                                                  00000620
C     IF (NTUBE(I) .EQ. LTUBE)  GO TO 80                                00000630
C  70 CONTINUE                                                          00000640
C     GO TO 300                                                         00000650
C  80 ZIP = NTUBE(I+1)                                                  00000660
C     ZOP = NTUBE(I+2)                                                  00000670
C     ZOP = ZOP - CC - 1.875                                            00000680
C     ZIP = ZIP - CC - 1.875                                            00000690
C     K = K1                                                            00000700
C     Y(K1+1) = 3.375                                                   00000710
C     DO 90 I = 1,2                                                     00000720
C     X(K1+1) = XLONG(M) - ZIP                                          00000730
C     CALL T3CRNT (2,2,N,I,3,JJ,1.,0.,1.25)                             00000740
C     ZIP = ZOP                                                         00000750
C     Y(K1+1) = XWIDE(M) - Y(K1)                                        00000760
C  90 CONTINUE                                                          00000770
C                                                                       00000780
      IF (IRUN .EQ. 1)  GO TO 201                                       00000790
      CALL T3H16D (LL,J,K,XX,YY,5.,-3.,II)                              00000800
      IF (MARK .EQ. M)  K2 = K1 + 8                                     00000810
      KOUNT = -1                                                        00000820
      LINE = 25                                                         00000830
      M1 = 2                                                            00000840
      M2 = 3                                                            00000850
      M3 = 1                                                            00000860
      CALL T3H11D (I,J,II,4,-3000,5000,IXY,M1,M2,M3,6)                  00000870
C                                                                       00000880
      K4 = 0                                                            00000890
      X60 = 0.                                                          00000900
      DO 110 L = 1,LL                                                   00000910
      CALL T3H13D (K3,K4,X60,XX,YY,I,II,J,K,ZIP,ZOP)                    00000920
      CALL T3H14D (K,K3,K4,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3)           00000930
      IF (K4 .GE. K1)  GO TO 120                                        00000940
      CALL T3H15D (XX,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3,K4)             00000950
  110 CONTINUE                                                          00000960
C                                                                       00000970
  120 IF (MARK .NE. M) GO TO 180                                        00000980
      IY = (XWIDE(M)+3.) * 1000. + .5                                   00000990
      CALL T3H11D (I,J,II,3,IX,IY,IXY,3,4,M3,4)                         00001000
      M1 = 6                                                            00001010
      DO 170 K = 1,8                                                    00001020
      CALL T3H11D (I,J,II,1,IX,IY,IXY,M1,M2,M3,1)                       00001030
      IF (K .EQ. 3 .OR. (K .EQ. 7 .AND. IGO1 .EQ. NSHAFT))  GO TO 160   00001040
      M1 = 1                                                            00001050
      GO TO 170                                                         00001060
  160 M1 = 7                                                            00001070
  170 CONTINUE                                                          00001080
C                                                                       00001090
  180 IF (IGO1 .NE. NSHAFT)  GO TO 200                                  00001100
      IY = XWIDE(MARK) * 500. - 374.5                                   00001110
      CALL T3H11D (I,J,II,4,60000,IY,IXY,3,4,M3,8)                      00001120
      CALL T3H11D (I,J,II,2,57000,IY,IXY,6,M2,M3,1)                     00001130
      CALL T3H11D (I,J,II,1,IX,IY,IXY,10,M2,M3,1)                       00001140
      WRITE (NWTR,185)                                                  00001150
  185 FORMAT ('+',55X,'TAPE OPTION : USE 2-3/8" RAD. MASTER')           00001160
      DO 190 K = 1,11,2                                                 00001170
      IX = INCRE(K)                                                     00001180
      IY = INCRE(K+1)                                                   00001190
      CALL T3H11D (I,J,II,4,IX,IY,IXY,1,M2,M3,1)                        00001200
  190 CONTINUE                                                          00001210
      M2 = 11                                                           00001220
C                                                                       00001230
  200 M1 = 8                                                            00001240
      CALL T3H11D (I,J,II,1,IX,IY,IXY,M1,M2,M3,1)                       00001250
  201 CONTINUE                                                          00001260
      IF (MARK .LT. (M+2) .AND. NUMBER(MARK+1) .GT. 0)  GO TO 40        00001270
C                                                                       00001280
C                                                                       00001290
  210 IGO = NSHAFT + 1                                                  00001300
      GO TO (220,240,300),  IGO1                                        00001310
C                                                                       00001320
  220 IF (SPACE(IGO1+1) .NE. SPACE(1))  GO TO 230                       00001330
      GO TO (280,230,230,280),  IGO                                     00001340
  230 M = 16                                                            00001350
      GO TO 265                                                         00001360
C                                                                       00001370
  240 IF (SPACE(IGO1+1) .NE. SPACE(2))  GO TO 250                       00001380
      GO TO (280,280,250,250),  IGO                                     00001390
  250 IF (SPACE(IGO1+1) .NE. SPACE(1))  GO TO 260                       00001400
      GO TO (270,260,270,260),  IGO                                     00001410
C                                                                       00001420
  260 IF (M .EQ. 8)  GO TO 230                                          00001430
      M = 19                                                            00001440
  265 IGO2 = 1                                                          00001450
      GO TO 290                                                         00001460
C                                                                       00001470
  270 M = 8                                                             00001480
  280 IGO2 = 2                                                          00001490
C                                                                       00001500
  290 IGO1 = IGO1 + 1                                                   00001510
      IF (SPACE(IGO1) .GT. 0.0)  GO TO 10                               00001520
C                                                                       00001530
  300 J = 8                                                             00001540
      K = 10                                                            00001550
C                                                                       00001560
  310 DO 320 I = J,K                                                    00001570
      IBOLT = IBOLT + (NUMBER(I) * N)                                   00001580
      IF(I .EQ. K .OR. I .EQ. 18)  IBOLT = IBOLT + (NUMBER(I) * 2)      00001590
  320 CONTINUE                                                          00001600
C                                                                       00001610
      IF (K .NE. 10)  RETURN                                            00001620
      J = 16                                                            00001630
      K = 21                                                            00001640
      GO TO 310                                                         00001650
      END                                                               00001660
