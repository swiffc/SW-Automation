C          DATA SET T3DFTPAN   AT LEVEL 137 AS OF 10/01/90              00000000
C     FAN/HOOD PANEL MASTER PROGRAM                                     00000010
C     HUDSON PRODUCTS CORP                                              00000020
      DIMENSION  FRAME(4)                                               00000030
      DIMENSION  MARKER(21), MATL(21)                                   00000040
      DIMENSION  NCODE(35), CODE(35), CASE(3), FRAMEX(8,4), NU(21)      00000050
      
      INTEGER*4 BETA, OMEGA
      
      DIMENSION  MEDOL(23), MARKAL(2), STD(12), DELTA(6),  EPSILO(12),  00000060
     1    DANGLE(3,31), LAMBDA(31), GAMMA(31,2,3), IOTA(6), SIGMA(6),   00000070
     2    MU(21), NET(12,4), ALPHA(2,16), BETA(4,6), OMEGA(7)           00000080
     3    , FORMA(2), FORMB(7), FORMC(5), FRAMEY(32)                    00000090
      DIMENSION   XY(2,100)                                             00000100
      DIMENSION   C7(2,7,7), C6(2,7,6), C5(2,7,5), C4(2,7,4), C3(2,7,3),00000110
     &            CHASE(2,9,2)                                          00000120
      DIMENSION  GUARDS(2,13)                                           00000130
      COMMON IRUN, IDATEX(3)                                             00000140
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000150
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000160
     *,INTS                                                             00000170
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000180
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000190
      COMMON/INOUT/NRDR,NWTR                                            00000200
C     NAMELIST/TRACE/ NUMBER,XWIDE,NCODE,CODE                           00000210
      INTEGER   FANSZ,VEQTY,VSQTY,EVBOLT,SVBOLT                         00000220
      DATA MEDOL/'   5',' 5.5','   6',' 6.5','   7',' 7.5','   8',      00000230
     1    ' 8.5','   9',' 9.5','  10','10.5','  11','11.5','  12',      00000240
     2    '12.5','  13','13.5','  14','14.5','  15','15.5','  16'/      00000250
      DATA MARKAL / '    ',' R/L'/                                      00000260
      DATA STD / 2*5.5,4.5,5.5,3.75,6.25,1.5,4.5,7.5,1.5,1.75,2.0/      00000270
      DATA DELTA, EPSILO / '3CMA','3CMB','3CMC','3CMD','3CME','3CMF',   00000280
     1           '''A1''','''A2''','''A3''','''B1''','''B2''','''B3''', 00000290
     2           ' ''C''',' ''D''',' ''E''',' ''F''',' ''G''',' ''H'''/ 00000300
      DATA DANGLE / 18*0.,                                              00000310
     1 12.,2*0.,    12.,2*0.,    12.,2*0.,    15.,2*0.,    18.,2*0.,    00000320
     2 21.,2*0.,    24.,2*0.,    27.,2*0.,    12.,18.,0.,  9.,24.,0.,   00000330
     3 12.,24.,0.,  15.,24.,0.,  18.,24.,0.,  21.,24.,0.,  2*24.,0.,    00000340
     4 24.,27.,0.,  12.,2*21.,   15.,2*21.,   12.,2*24.,   15.,2*24.,   00000350
     5 18.,2*24.,   21.,2*24.,    3*24.,      3*25.,       3*26. /      00000360
      DATA  LAMBDA, GAMMA /                                             00000370
     1      6,7,8,9,10,11,                                              00000380
     2      12,13,14,15,16,17,                                          00000390
     3      18,19,20,21,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,   00000400
     A    2*22. ,5*28. ,41. ,2*48. ,2*57. ,3*75. ,3*96.,13*0. ,         00000410
     C    2*22. ,28. ,4*41. ,48. ,2*55. ,2*64. ,3*90. ,3*111. ,13*0. ,  00000430
     E    6*0. ,28. ,44. ,47. ,50. ,53. ,56. ,62. ,2*76. ,81. ,96. ,    00000450
     F    2*111. ,122. ,6*130. ,2*146. ,127. ,123. ,129.,               00000460
     G    6*0. ,41. ,51. ,54. ,57. ,60. ,63. ,76. ,2*83. ,88. ,         00000470
     H    111. ,2*121. ,132. ,6*146. ,2*156. ,145. ,133. ,139. ,        00000480
     I    12*0. ,77. ,84. ,88. ,2*96. ,111. ,2*121. ,127. ,135. ,       00000490
     &    125. ,122. ,2*130. ,2*133. ,3*130. ,                          00000495
     J    12*0. ,90. ,97. ,101. ,2*111. ,119. ,2*131. ,137. ,145. ,     00000500
     &    143. ,132. ,2*146. ,2*143. ,3*146. /                          00000505
      DATA  IOTA, SIGMA, MU / 11110,11209,11399,12018,12026,12132,      00000510
     A                        5.625,5.625, 7.65, 2.44, 3.71, 3.07,      00000520
     B                        2*3,5,2*4,2*6,3*2,3,2*5,2*3,6*2 /         00000530
      DATA NET / 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 8, 8,                    00000540
     A           4, 4, 4, 4, 6, 6, 6, 6, 6, 8, 8, 8,                    00000550
     B     861,1016,1178,1325,1494,1657,1819,1981,2144,2313,2475,2638,  00000560
     C     189, 251, 321, 399, 485, 579, 682, 793, 912,1039,1175,1318/  00000570
      DATA ALPHA /    2*'    ',' 1/1','6   ',' 1/8','    ',' 3/1',      00000580
     A    '6   ',' 1/4','    ',' 5/1','6   ',' 3/8','    ',' 7/1',      00000590
     B    '6   ',' 1/2','    ',' 9/1','6   ',' 5/8','    ',' 11/',      00000600
     C    '16  ',' 3/4','    ',' 13/','16  ',' 7/8','    ',' 15/',      00000610
     D    '16  '/                                                       00000620
      DATA BETA, OMEGA /  ',14X',',''10',' GA ','BENT',                 00000630
     A                    ',14X',',''10',' GA ','GALV',                 00000640
     B                    ',14X',',''3/','16  ','BENT',                 00000650
     C                    ',''L ','2 X ','2 X ','3/16',                 00000660
     D                    ',''L ','3 X ','3 X ','3/16',                 00000670
     E                    ',''L ','3 X ','2 X ','3/16',                 00000680
     F    '(''+''',',48X',4*'    ','''/) ' /                            00000690
      DATA  FORMA,FORMB,FORMC/ '    ',                                  00000700
     1                         ','',''',                                00000710
     2                  ',36X',                                         00000720
     3                  ',43X',                                         00000730
     4                  ',51X',                                         00000740
     5                  ',59X',                                         00000750
     6                  ',67X',                                         00000760
     7                  ',75X',                                         00000770
     8                  ',83X',                                         00000780
     9         '(''+''',     2*'    '  ,',I3,','2A4)'/                  00000790
      DATA  GUARDS /  '3G02',10.5,  '3G05',31.9,  '3G06',37.1,          00000800
     A  '3G07',42.8,  '3G08',48.9,  '3G09',64.3,  '3G10',72.1,          00000810
     B  '3G11',80.2,  '3G12',88.7,  '3G13',97.6,  '3G14',106.6,         00000820
     C  '3G15',116.,  '3G16',125.8/                                     00000830
      DATA  ZERO, HALF, ONE, TWO, THREE, SIX, TWELVE,      QTR, GROSS/  00000840
     *      0.0 , .5  , 1. , 2. ,   3. , 6. ,   12. ,      .25,  144./  00000850
C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 00000860
C                                 MAY 23, 1975                          00000870
C   JOBNO - JOB NUMBER                                                  00000880
C   IDRAW - DRAWING SHEET NUMBER ASSIGNMENT                             00000890
C                                                                       00000900
C   MODEL - NOMINAL UNIT WIDTH                                          00000910
C   LTUBE - NOMINAL TUBE LENGTH                                         00000920
C   MODAL - NO. OF ROWS OF TUBES                                        00000930
C                                                                       00000940
C   NOFAN - NO. OF FANS ON UNIT                                         00000950
C   ITYPE - TYPE OF UNIT                                                00000960
C                                                                       00000970
C   H - NOMINAL HOOD HEIGHT (IN/DEC)                                    00000980
C   F - NOMINAL FAN DIAMETER (IN/DEC)                                   00000990
C                                                                       00001000
C   MTYPE - 1 IS FOR THE PLENUM PANELS TO BE 10 GA. BLACK.              00001010
C     "   - 2  "  "   "    "      "    "  "  10 GA. GALV.               00001020
C     "   - 3  "  "   "    "      "    "  "  3/16 BLACK(THIS IS STD.)   00001030
C                                                                       00001040
C   NCOPY - NO. OF SUMMARY OUTPUT COPIES PER DWG.                       00001050
C   ISHIP - SHIPPING CODE FOR UNIT (BLANK/ZERO=WHOLE;1-9=HALF UNIT)     00001060
C   NOREV - DRAWING REVISION NO.                                        00001070
C   LUNITS - NO. OF UNITS                                               00001080
C                                                                       00001090
C   FRAME(1-4) - TOTAL WIDTH(IN/DEC) FOR EACH BUNDLE                    00001100
C   SPACE(1-3) - DISTANCE(IN/DEC) BETWEEN MULTIPLE BUNDLES              00001110
C   E - DISTANCE(IN/DEC) FROM CL OF UNIT TO OUTERMOST D-ANGLE GAUGE LINE00001120
C                                                                       00001130
C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 00001140
      NRDR = 2                                                          00001100
      NWTR = 6                                                          00001110
      NDBGR = 9
      OPEN(NRDR,FILE='T3DFTPAN.input')
C     OPEN(NWTR,FILE='T3DFTPAN.output',STATUS='UNKNOWN',ACTION='PRINT')
      OPEN(NWTR,FILE='T3DFTPAN.output',STATUS='UNKNOWN')

      OPEN(7,FILE='T3DFTPAN.export',STATUS='UNKNOWN')
      OPEN(8,FILE='T3DFTPAN.weight',STATUS='UNKNOWN')

      I = 1                                                             00001170
C     CALL P2DATE(I,J,K,L)                                              00001180
C     IDATEX(3) = L - ((L/100)*100)                                      00001190
C     IDATEX(2) = K                                                      00001200
C     IDATEX(1) = J                                                      00001210

C New Date function calculations
      CALL IDATE(J,K,L)
      IDATEX(1)=J
      IDATEX(2)=K
      if (L < 50) IDATEX(3)=L + 2000
      if (L > 50) IDATEX(3)=L + 1900
 
 
C RJD 8/5/92 Changed the Read so that each value was on a line by itself
C the FORMAT statement used to be 1000 it is now 7010
 7010 FORMAT(A4,/,A3,/,A4,/,I2,/,A2,/,I1,/,A4,/,F2.0,/,F3.0,/,F2.0,/,4(I
     &1,/),I2,/,F5.2,/,F5.3,/,F5.2,/,F5.3,/,F5.2,/,F5.3,/,F5.2,/,F7.4,/,
     &I3,/,I1)                

    5 READ (NRDR,7010,END=999)  JOBNO, IDRAW, MODEL, LTUBE, MODAL,      00001220
     &    NOFAN, ITYPE, H, F, WINDL, MTYPE, NCOPY, ISHIP, NOREV,        00001230
     &    LUNITS, FRAME(1), SPACE(1), FRAME(2), SPACE(2), FRAME(3),
     &    SPACE(3), FRAME(4), E, INTS, ITEMS


C This wouldn't work on the Mac, the corrected version is above.
C    &    LUNITS, ( FRAME(I),SPACE(I) ,I=1,3), FRAME(4),                00001240
C    &    E, INTS, ITEMS                                                00001250
 1000 FORMAT(A4,A3,A4,1X,I2,A2,I1,A4,F2.0,F3.0,F2.0,4I1,I2,             00001260
     *    F5.2,F5.3,F5.2,F5.3,F5.2,F5.3,F5.2,F7.4,I3,I1)                00001270
C    *    3(F5.2,F5.3),F5.2,F7.4,I3,I1)                                 00001270

C  "IGO" - SWITCH FOR POSSIBLY INADEQUATE INPUT DATA.                   00001280
C  "ICOUNT" - NO OF TUBE BUNDLES IN UNIT.                               00001290
C  "TOTAL" - TOTAL WIDTH OF UNIT.                                       00001300
      IGO = 1                                                           00001310
      IF (FRAME(IGO) .GT. ZERO)  GO TO 13                               00001320
      IGO = 2                                                           00001330
      DO 10 I = 1,23                                                    00001340
          IF (MEDOL(I) .EQ. MODEL)  GO TO 12                            00001350
   10 CONTINUE                                                          00001360
      IF (E .EQ. ZERO)   GO TO 999                                      00001370
   11 WRITE (NWTR,1001)  JOBNO, IDRAW                                   00001380
 1001 FORMAT(//23X,A4,'-',A4,'  H A S    I N A D M I S S I B L E    I00001390
     1 N P U T    D A T A'//23X,'P L E A S E    C O R R E C T    T H I S00001400
     2    D A T A    &    R E - E N T E R')                             00001410
          GO TO 5                                                       00001420
   12 FRAME(1) = SIX * (I+9)                                            00001430
   13 TOTAL = FRAME(1)                                                  00001440
      ZAP = TOTAL * HALF                                                00001450
      ICOUNT = 1                                                        00001460
      DO 15 I = 1,3                                                     00001470
          IF (SPACE(I) .GT. ZERO)  GO TO (14,11), IGO                   00001480
          IF (FRAME(I+1) .GT. ZERO)  GO TO 11                           00001490
               GO TO (18,17,16), ICOUNT                                 00001500
   14     IF (FRAME(I+1) .LE. ZERO)  GO TO 11                           00001510
          TOTAL = TOTAL + SPACE(I) + FRAME(I+1)                         00001520
          ZAP = TOTAL * HALF                                            00001530
          ICOUNT = ICOUNT + 1                                           00001540
   15 CONTINUE                                                          00001550
C  "NSHAFT=0" -THE FAN SHAFT PASSES THRU A BUNDLE (SHAFT-THRU-UNIT).    00001560
C  "NSHAFT=1" -THE FAN SHAFT PASSES BETWEEN BUNDLES 1 & 2.              00001570
C  "NSHAFT=2" -THE FAN SHAFT PASSES BETWEEN BUNDLES 2 & 3.              00001580
C  "NSHAFT=3" -THE FAN SHAFT PASSES BETWEEN BUNDLES 3 & 4.              00001590
      IF (SPACE(3) .LT. SIX)  GO TO 16                                  00001600
      IF ((FRAME(1)+SPACE(1)+FRAME(2)+SPACE(2)+FRAME(3)+THREE) .LE. ZAP 00001610
     *   .AND. (FRAME(4)+THREE) .LE. ZAP)   GO TO 21                    00001620
   16 IF (SPACE(2) .LT. SIX)  GO TO 17                                  00001630
      IF ((FRAME(1)+SPACE(1)+FRAME(2)+THREE) .LE. ZAP  .AND.            00001640
     &    (FRAME(3)+SPACE(3)+FRAME(4)+THREE) .LE. ZAP)   GO TO 20       00001650
   17 IF (SPACE(1) .LT. SIX)  GO TO 18                                  00001660
      IF ((FRAME(1)+THREE) .LE. ZAP  .AND.                              00001670
     *    (FRAME(2)+SPACE(2)+FRAME(3)+SPACE(3)+FRAME(4)+THREE) .LE. ZAP)00001680
     *    GO TO 19                                                      00001690
   18 NSHAFT = 0                                                        00001700
          GO TO (23,24,22,24),  ICOUNT                                  00001710
   19 NSHAFT = 1                                                        00001720
          GO TO (11,23,24,24),  ICOUNT                                  00001730
   20 NSHAFT = 2                                                        00001740
          GO TO (11,11,24,22),  ICOUNT                                  00001750
   21 NSHAFT = 3                                                        00001760
          GO TO 24                                                      00001770
C  "NSYM=1" -SYMMETRICAL UNIT.                                          00001780
C  "NSYM=2" -NON-SYMMETRICAL UNIT.                                      00001790
   22 IF (FRAME(1) .NE. FRAME(ICOUNT))  GO TO 24                        00001800
   23 NSYM = 1                                                          00001810
          GO TO 25                                                      00001820
   24 NSYM = 2                                                          00001830
C  FURNISH DATA FOR ANY IMPLIED INPUT DATA:                             00001840
   25 IF (H .EQ. ZERO)  H = 36.                                         00001850
      IF (NCOPY .EQ. 0)  NCOPY = 3                                      00001860
      IF  (LUNITS .EQ. 0)  LUNITS = 1                                   00001870
C  "FANSZ" -FAN DIA. IN FEET (NOTE: INTEGER FORMAT)                     00001880
      IF (F .EQ. ZERO)  GO TO 11                                        00001890
      FANSZ = F / TWELVE + HALF                                         00001900
C  "FANWP" -FROM CENTERLINE(CL) OF FAN TO THE TOP WORK POINT(W.P.).     00001910
      FANWP = F * HALF + 3.25                                           00001920
C  DETERMINE THE PL THICKNESS & MAT'L I.D. FOR THE PLENUM PANELS :      00001930
      IF (MTYPE .GT. 0)   GO TO (28,27,26),  MTYPE                      00001940
   26 MU(1) = 3                                                         00001950
      THK = .1875                                                       00001960
          GO TO 30                                                      00001970
   27 MU(1) = 2                                                         00001980
          GO TO 29                                                      00001990
   28 MU(1) = 1                                                         00002000
   29 THK = .125                                                        00002010
   30 MU(2) = MU(1)                                                     00002020
      MU(11) = MU(1)                                                    00002030
      MU(14) = MU(1)                                                    00002040
      MU(15) = MU(1)                                                    00002050
CLEAR DATA COUNTERS :                                                   00002060
      DO 33 I = 1,32                                                    00002070
          IF (I .GT. 29)  GO TO 32                                      00002080
          IF (I .GT. 21)  GO TO 31                                      00002090
          NUMBER(I) = 0                                                 00002100
          MARKER(I) = MARKAL(1)                                         00002110
          XWIDE(I)  = TWELVE                                            00002120
   31     NCODE(I) = 1                                                  00002130
          CODE(I) = ZERO                                                00002140
   32     FRAMEY(I) = ZERO                                              00002150
   33 CONTINUE                                                          00002160
          NCODE(30)=1
          CODE(30)=ZERO
          NCODE(31)=1
          CODE(31)=ZERO
          NCODE(32)=1
          CODE(32)=ZERO
          NCODE(33)=1
          CODE(33)=ZERO
          NCODE(34)=1
          CODE(34)=ZERO
          NCODE(35)=1
          CODE(35)=ZERO
C                                            BEGIN FLOWCHART "AA"       00002170
COMPUTES :                                                              00002180
C         CODE: 1, 20, 21                                               00002190
C         FANCLE, FANCLS                                                00002200
      IF (LTUBE .EQ. 0)  GO TO 11                                       00002210
      IF (NOFAN .EQ. 0)  GO TO 11                                       00002220
      J = LTUBE - (FANSZ + 1) * NOFAN                                   00002230
      IGO = NOFAN                                                       00002240
      IF (J)  11, 34, 35                                                00002250
   34 IGO = IGO + 3                                                     00002260
   35 IF (E .GT. ZERO)  GO TO 37                                        00002270
   36 E = SIX * LTUBE - STD(IGO)                                        00002280
   37 CODE(1) = (FANWP * NOFAN - E + STD(IGO+6)) / NOFAN                00002290
      IF (J .EQ. 0 .AND. CODE(1) .NE. ZERO .AND. NOFAN .GT. 1) GO TO 36 00002300
      CODE(20) = H - .375                                               00002310
      CODE(21) = (F - TOTAL + 13.) * HALF                               00002320
      FANCLE = FANWP                                                    00002330
      IF (CODE(21) .LT. ZERO)  FANCLE = FANCLE - CODE(21)               00002340
      IF (CODE(1))  40, 41, 43                                          00002350
   40 FANCLS = ABS(CODE(1))                                             00002360
          GO TO 44                                                      00002370
   41 GO TO (43,42,42),  NOFAN                                          00002380
   42 FANCLS = QTR                                                      00002390
          GO TO 44                                                      00002400
   43 FANCLS = ZERO                                                     00002410
   44 FANCLS = FANCLS + FANWP                                           00002420
C                                            BEGIN FLOWCHART "AB"       00002430
COMPUTES :                                                              00002440
C         IVS, SLOPE, EDGE                                              00002450
C         CASE: 2                                                       00002460
C         NUMBER: 2                                                     00002470
C         MARKER: 2                                                     00002480
C         XLONG: 2, 3, 13                                               00002490
C         XWIDE: 1, 2                                                   00002500
C         CODE: 2, 3, 4, 14, 22, 23, 24, 25, 26, 27                     00002510
C         NCODE: 14                                                     00002520
      IF (FANCLS .GT. 120.)  GO TO 45                                   00002530
      IVS = 1                                                           00002540
          GO TO 46                                                      00002550
   45 IVS = 2                                                           00002560
   46 CASE(2) = EPSILO(IVS+6)                                           00002570
      NUMBER(2) = LUNITS * NOFAN * IVS * 2                              00002580
      MARKER(2) = MARKAL(IVS)                                           00002590
      XLONG(2) = FANCLS * (-IVS+3) + (THREE-TWO*THK) * (IVS-1)          00002600
      DO 49 I = 1,2                                                     00002610
          CODE(I+24) = (CODE(I*20-19)**2 + CODE(20)**2)**HALF           00002620
          ZAP = CODE(I*20-19) / CODE(20)                                00002630
          IF (ZAP .LT. -1.0)   GO TO 47                                 00002640
          CODE(I*20-17) = TWELVE * ZAP                                  00002650
          CODE(I*20-16) = TWELVE                                        00002660
               GO TO 48                                                 00002670
   47     CODE(I*20-17) = TWELVE                                        00002680
          CODE(I*20-16) = TWELVE / ZAP                                  00002690
   48     ZAP = ((ONE + CODE(I*20-19) / CODE(I+24))  /                  00002700
     *           (ONE - CODE(I*20-19) / CODE(I+24)))**HALF * THK        00002710
          CODE(I*20-18) = - ZAP + 2.75                                  00002720
          XWIDE(I) = CODE(I*20-18) - ZAP + CODE(I+24) + 2.875           00002730
   49 CONTINUE                                                          00002740
      SLOPE = (CODE(1)**2 + CODE(20)**2 + CODE(21)**2)**HALF            00002750
      CODE(27) = ONE                                                    00002760
      IF (ABS(CODE(1)) .LT. ABS(CODE(21)))   GO TO 50                   00002770
      ZAP = ABS(CODE(1)) / CODE(26)                                     00002780
          GO TO 51                                                      00002790
   50 ZAP = ABS(CODE(21)) / CODE(25)                                    00002800
      IF (CODE(1))  51, 52, 51                                          00002810
   51 IF (ZAP .GT. CODE(27))  GO TO 54                                  00002820
      EDGE = 1.25                                                       00002830
          GO TO 55                                                      00002840
   52 IF (CODE(21))  54, 54, 53                                         00002850
   53 CODE(27) = CODE(27) + 1.5                                         00002860
   54 EDGE = THREE                                                      00002870
      ZAP = ZAP + HALF + .1875                                          00002880
          GO TO 56                                                      00002890
   55 ZAP = ZAP * 3.25 + .1875                                          00002900
   56 CODE(27) = CODE(27) + EDGE                                        00002910
      ZIP = SLOPE - CODE(27) - EDGE - ZAP                               00002920
      ZAP = 24.                                                         00002930
      DO 57 I = 1,12                                                    00002940
          IF ((ZAP * I) .GE. ZIP)  GO TO 58                             00002950
   57 CONTINUE                                                          00002960
   58 NCODE(14) = I                                                     00002970
      I = ZIP / QTR / NCODE(14)                                         00002980
      CODE(14) = QTR * I * NCODE(14)                                    00002990
      DO 59 I = 3,13,10                                                 00003000
          XLONG(I) = TWO * EDGE + CODE(14)                              00003010
   59 CONTINUE                                                          00003020
C                                            BEGIN FLOWCHART "AC"       00003030
COMPUTES  :                                                             00003040
C         DWG, IVE                                                      00003050
C         CASE: 1, 3                                                    00003060
C         NUMBER: 1, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15                  00003070
C         MARKER: 1, 11, 12, 13, 14, 15                                 00003080
C         XLONG: 1, 11, 12, 14, 15                                      00003090
C         XWIDE: 11, 14, 15                                             00003100
      IF (CODE(1) .EQ. ZERO)   GO TO (60,71,71),  NOFAN                 00003110
   60 NUMBER(1) = LUNITS * NOFAN * 2                                    00003120
      IF (CODE(1))   61, 61, 64                                         00003130
   61 IF (CODE(21))  62, 62, 63                                         00003140
   62 DWG = DELTA(1)                                                    00003150
          GO TO 67                                                      00003160
   63 DWG = DELTA(2)                                                    00003170
          GO TO 67                                                      00003180
   64 IF (CODE(21))  65, 65, 66                                         00003190
   65 DWG = DELTA(5)                                                    00003200
          GO TO 67                                                      00003210
   66 DWG = DELTA(6)                                                    00003220
CONTINUE                                          FLOWCHART "AC"  P.2   00003230
   67 IF (EDGE - THREE)  68, 69, 69                                     00003240
   68 CASE(3) = EPSILO(9)                                               00003250
          GO TO 70                                                      00003260
   69 CASE(3) = EPSILO(10)                                              00003270
   70 IF (ISHIP .GT. 0 .OR. FANCLE .GT. 120.)   GO TO (80,82),  NSYM    00003280
          GO TO (75,77),  NSYM                                          00003290
CONTINUE                                          FLOWCHART "AC"  P.3   00003300
   71 NUMBER(1) = LUNITS * TWO                                          00003310
      NUMBER(11) = LUNITS * (NOFAN - 1)                                 00003320
      IF (CODE(21))  72, 72, 73                                         00003330
   72 DWG = DELTA(3)                                                    00003340
          GO TO 74                                                      00003350
   73 DWG = DELTA(4)                                                    00003360
      MARKER(13) = MARKAL(2)                                            00003370
   74 IF (ISHIP .GT. 0 .OR. FANCLE .GT. 120.)  GO TO 79                 00003380
      CASE(3) = EPSILO(11)                                              00003390
          GO TO (75,76),  NSYM                                          00003400
   75 J = 1                                                             00003410
          GO TO 88                                                      00003420
   76 GO TO (11,77,78),  NOFAN                                          00003430
   77 J = 2                                                             00003440
          GO TO 87                                                      00003450
   78 J = 3                                                             00003460
          GO TO 85                                                      00003470
CONTINUE                                          FLOWCHART "AC"  P.4   00003480
   79 CASE(3) = EPSILO(12)                                              00003490
          GO TO (80,81),  NSYM                                          00003500
   80 J = 4                                                             00003510
          GO TO 85                                                      00003520
   81     GO TO (11,82,83),  NOFAN                                      00003530
   82 J = 5                                                             00003540
          GO TO 84                                                      00003550
   83 J = 6                                                             00003560
   84 NUMBER(15) = NUMBER(11)                                           00003570
      IF (J .EQ. 5)  GO TO 86                                           00003580
   85 MARKER(11) = MARKAL(2)                                            00003590
      IF (J .LT. 5)  GO TO 87                                           00003600
   86 NUMBER(14) = NUMBER(1)                                            00003610
   87 MARKER(1) = MARKAL(2)                                             00003620
      IF (J .GT. 3)  GO TO 89                                           00003630
   88 IVE = 1                                                           00003640
          GO TO 90                                                      00003650
   89 IVE = 2                                                           00003660
   90 CASE(1) = EPSILO(J)                                               00003670
      NUMBER(1) = NUMBER(1) * IVE - NUMBER(14)                          00003680
      XLONG(1) = FANCLE * (-IVE+3) + (THREE-TWO*THK) * (IVE-1)          00003690
      NUMBER(11) = NUMBER(11) * IVE - NUMBER(15)                        00003700
      DO 93 I = 1,5                                                     00003710
          GO TO (92,93,93,91,91),  I                                    00003720
   91     MARKER(I+10) = MARKER(I*10-39)                                00003730
   92     XLONG(I+10) = XLONG(1)                                        00003740
          XWIDE(I+10) = XWIDE(1)                                        00003750
          NUMBER(3) = NUMBER(3) + NUMBER(I+10)                          00003760
   93 CONTINUE                                                          00003770
      NUMBER(3) = (NUMBER(3) + NUMBER(1)) * 2 / IVE                     00003780
      NUMBER(4) = (NUMBER(1) + NUMBER(11)) * (-IVE+2)                   00003790
      NUMBER(5) = NUMBER(2) * (-IVS+2)                                  00003800
      NUMBER(6) = NUMBER(3)                                             00003810
      NUMBER(7) = NUMBER(2) * 2 / IVS                                   00003820
      NUMBER(12) = NUMBER(11) + NUMBER(15)                              00003830
      NUMBER(13) = NUMBER(12) * 2 / IVE                                 00003840
      MARKER(12) = MARKAL(IVE)                                          00003850
      XLONG(12) = FANWP * (-IVE+3) - QTR * (IVE-1)                      00003860
C                                            BEGIN FLOWCHART "AD"       00003870
COMPUTES :                                                              00003880
C         XWIDE: 3                                                      00003890
C         XLONG: 4, 5, 6, 7                                             00003900
C         CODE:  5, 6, 11, 12, 13, 15, 18, 19, 28, 29                   00003910
C         NCODE: 5, 6, 11, 12, 13, 15                                   00003920
      IF (NUMBER(11) .GT. 0)  GO TO 98                                  00003930
      DO 95 I = 1,21,20                                                 00003940
          IF (CODE(I) .EQ. ZERO)  GO TO 96                              00003950
   95 CONTINUE                                                          00003960
      CODE(18) = TWELVE * (ONE - (SLOPE**2 / CODE(20)**2))  /           00003970
     *  (SLOPE / CODE(20) * ((CODE(1)/CODE(21)) + (CODE(21)/CODE(1))))  00003980
          GO TO 97                                                      00003990
   96 CODE(18) = ZERO                                                   00004000
   97 ZAP = (CODE(18)**2 + TWELVE**2)**HALF                             00004010
      ZAP = ((ONE + CODE(18) / ZAP) / (ONE - CODE(18) / ZAP))**.5 *.187500004020
      CODE(19) = -ZAP + 3.125                                           00004030
      XWIDE(3) = CODE(19) * TWO                                         00004040
CONTINUE                                          FLOWCHART "AD"  P.2   00004050
   98 J = IVE                                                           00004060
      IF (NUMBER(11) .GT. 0)  GO TO 101                                 00004070
      ZAP = ZERO                                                        00004080
          GO TO 102                                                     00004090
  101 ZAP = 1.5625                                                      00004100
  102 K = 1                                                             00004110
      DO 108 I = 1,2                                                    00004120
          N = I * 6 - 1                                                 00004130
          CODE(N) = CODE(I+24)  -  THREE * J  -  ZAP * (-J+2)  -        00004140
     *         (TWO * ABS(CODE(I*20-19)) / CODE(20) + 1.25) * (-J+2)    00004150
          IF (I .EQ. 1 .AND. CODE(1) .LT. ZERO)                         00004160
     *         CODE(N) = CODE(N) - CODE(20) / (CODE(20) - .125) * .125  00004170
          IGO = 1                                                       00004180
          ZIP = 21.                                                     00004190
  103     DO 104 L = 1,12                                               00004200
               IF ((ZIP * L) .GE. CODE(N))  GO TO 105                   00004210
  104     CONTINUE                                                      00004220
  105     NCODE(N) = L                                                  00004230
          L = CODE(N) / QTR / NCODE(N)                                  00004240
          CODE(N) = QTR * L * NCODE(N)                                  00004250
           IF ((CODE(N)/(NCODE(N)-1)) .EQ. ZIP)  NCODE(N) = NCODE(N) - 100004260
          XLONG(N-K+IGO-I) = CODE(N) + 2.5                              00004270
               GO TO (106,107),  IGO                                    00004280
  106     CODE(N+1) = ((CODE(N) - (ZAP * (J-1)))**2  +                  00004290
     *                 (FANWP - 10.)**2) **HALF                         00004300
          N = N + 1                                                     00004310
          IGO = 2                                                       00004320
          ZIP = 24.                                                     00004330
               GO TO 103                                                00004340
  107     J = IVS                                                       00004350
          ZAP = ZERO                                                    00004360
          K = 5                                                         00004370
  108 CONTINUE                                                          00004380
      CODE(13) = FANWP - 7.                                             00004390
      CODE(15) = FANCLS - 4.5                                           00004400
      IF (NUMBER(11) .EQ. 0)   GO TO (110,112,111),  NOFAN              00004410
      CODE(28) = 5.                                                     00004420
          GO TO 113                                                     00004430
  110 IF (CODE(1) .GT. ZERO)  CODE(15) = CODE(15) - CODE(1)             00004440
  111 CODE(28) = 3.5                                                    00004450
          GO TO 113                                                     00004460
  112 CODE(28) = 2.5                                                    00004470
  113 CODE(15) = CODE(15) - CODE(28)                                    00004480
      ZAP = 25.                                                         00004490
      DO 116 I = 13,15,2                                                00004500
          DO 114 J = 1,12                                               00004510
               IF ((ZAP * J) .GE. CODE(I))  GO TO 115                   00004520
  114     CONTINUE                                                      00004530
  115     NCODE(I) = J                                                  00004540
          ZAP = 27.                                                     00004550
  116 CONTINUE                                                          00004560
      CODE(29) = ((F * HALF + 1.375)**2 - 9.)**HALF - FANWP + 2.875     00004570
C                                            BEGIN FLOWCHART "AE"       00004580
COMPUTES :                                                              00004590
C         FRAMEX(8,4)                                                   00004600
C         CODE: 7, 8, 9, 10, 16, 17                                     00004610
C         NUMBER: 8, 9, 10, 16, 17, 18, 19, 20, 21                      00004620
C         XLONG: 8, 9, 10, 16, 17, 18, 19, 20, 21                       00004630
C         XWIDE: 8, 9, 10, 16, 17, 18, 19, 20, 21                       00004640
      DO 121 I = 1,ICOUNT                                               00004650
          IF (I .GT. 1)  FRAMEX(8,I-1) = SPACE(I-1) + TWELVE            00004660
          J = FRAME(I) / SIX - .98                                      00004670
          ZAP = TWELVE                                                  00004680
          ZIP = FRAME(I)  -  SIX * J  -  SIX                            00004690
          DO 120 K = 1,3                                                00004700
               FRAMEX(K,I) = DANGLE(K,J)                                00004710
               IF (J .GT. 6  .AND.  K .EQ. 1)                           00004720
     &             FRAMEX(K,I) = FRAMEX(K,I) + ZIP * HALF               00004730
               FRAMEX(-K+8,I) = FRAMEX(K,I)                             00004740
               ZAP = ZAP + FRAMEX(K,I) * TWO                            00004750
  120     CONTINUE                                                      00004760
          FRAMEX(4,I) = FRAME(I) - ZAP                                  00004770
  121 CONTINUE                                                          00004780
      FRAMEX(8,ICOUNT) = ZERO                                           00004790
      ZAP = 2.75                                                        00004800
      IF (CODE(21) .GT. ZERO)  ZAP = ZAP + CODE(21)                     00004810
      J = 1                                                             00004820
      L = J                                                             00004830
      DO 124 I = 1,ICOUNT                                               00004840
          DO 123 K = 1,8                                                00004850
               IF (FRAMEX(K,I) .EQ. ZERO)  GO TO 123                    00004860
               ZAP = ZAP + FRAMEX(K,I)                                  00004870
               FRAMEY(L) = FRAMEX(K,I)                                  00004880
               L = L + 1                                                00004890
               IF (ZAP .LT. (FANCLE * J))  GO TO 122                    00004900
               CODE(9) = ZAP - FANCLE                                   00004910
               CODE(8) = FRAMEX(K,I) - CODE(9)                          00004920
               J = 2                                                    00004930
                    GO TO 123                                           00004940
  122          CODE(J*J+6) = CODE(J*J+6) + FRAMEX(K,I)                  00004950
               NCODE(J*J+6) = NCODE(J*J+6) + 1                          00004960
  123     CONTINUE                                                      00004970
  124 CONTINUE                                                          00004980
      DO 125 I = 7,10,3                                                 00004990
          NCODE(I) = NCODE(I) - 1                                       00005000
  125 CONTINUE                                                          00005010
CONTINUE                                          FLOWCHART "AE"  P.2   00005020
      DO 130 I = 1,31                                                   00005030
          IF (LAMBDA(I) .GE. LTUBE) GO TO 131                           00005040
  130 CONTINUE                                                          00005050
      GO TO 11                                                          00005060
  131 J = I                                                             00005070
      IF (GAMMA(J,1,NOFAN) .EQ. ZERO) GO TO 11                          00005080
      IF (GAMMA(J,2,NOFAN) .EQ. ZERO) GO TO 11                          00005090
      IF (NUMBER(11) .GT. 0)  GO TO 132                                 00005100
      ZAP = 1.5                                                         00005110
          GO TO 133                                                     00005120
  132 ZAP = -1.25                                                       00005130
  133 CODE(16) = GAMMA(J,1,NOFAN) - (E + ZAP) * (NOFAN - 1) / NOFAN     00005140
      CODE(17) = GAMMA(J,2,NOFAN) - (E + ZAP) * (NOFAN - 1) / NOFAN     00005150
      IF (CODE(16))  135, 11, 134                                       00005160
  134 INSEAL = 1                                                        00005170
          GO TO 136                                                     00005180
  135 INSEAL = 2                                                        00005190
      CODE(16) = ABS(CODE(16))                                          00005200
  136 CODE(17) = ABS(CODE(17))                                          00005210
CONTINUE                                          FLOWCHART "AE"  P.3   00005220
      IF (ICOUNT .EQ. 1)  GO TO 150                                     00005230
      J = 8                                                             00005240
      K = 7                                                             00005250
      DO 142 I = 2,ICOUNT                                               00005260
          NUMBER(K+1) = LUNITS * 2                                      00005270
          XWIDE(K+1) = SPACE(I-1) + SIX                                 00005280
          XLONG(K+1) = CODE(15) + CODE(28) + 4.5                        00005290
          IF (NOFAN .EQ. 1)  GO TO 141                                  00005300
          IF (NUMBER(11) .EQ. 0)  GO TO 140                             00005310
          XLONG(K+1) = XLONG(K+1) - QTR                                 00005320
          NUMBER(K+3) = LUNITS * (NOFAN - 1)                            00005330
          XWIDE(K+3) = XWIDE(K+1)                                       00005340
          XLONG(K+3) = XLONG(K+1) - 2.5                                 00005350
  140     NUMBER(K+2) = LUNITS * NOFAN * 2 - NUMBER(K+1) - NUMBER(K+3)  00005360
          XWIDE(K+2) = XWIDE(K+1)                                       00005370
          XLONG(K+2) = XLONG(K+1)                                       00005380
  141     K = K + J                                                     00005390
          J = 3                                                         00005400
  142 CONTINUE                                                          00005410
      IF (ICOUNT .EQ. 2)  GO TO 150                                     00005420
      IF (SPACE(1) .NE. SPACE(2))   GO TO 144                           00005430
      DO 143 I = 8,10                                                   00005440
          NUMBER(I) = NUMBER(I) + NUMBER(I+8)                           00005450
          NUMBER(I+8) = 0                                               00005460
  143 CONTINUE                                                          00005470
  144 IF (ICOUNT .EQ. 3)  GO TO 150                                     00005480
      IF (SPACE(1) .NE. SPACE(3))   GO TO 145                           00005490
      J = 7                                                             00005500
          GO TO 146                                                     00005510
  145 IF (NUMBER(16) .GT. 0)   GO TO 150                                00005520
      J = 15                                                            00005530
  146 DO 147 I = 1,3                                                    00005540
          NUMBER(I+J) = NUMBER(I+J) + NUMBER(I+18)                      00005550
          NUMBER(I+18) = 0                                              00005560
          XWIDE(I+J) = XWIDE(I+18)                                      00005570
  147 CONTINUE                                                          00005580
  150 CONTINUE                                                          00005590
COMPARE THE VALUES IN CODES 7 & 10 EACH TO DETERMINE IF THEY HAVE       00005600
C         "EQUAL" OR "UNEQUAL" SPACED VALUES.                           00005610
      IGO = 1                                                           00005620
      J = IGO                                                           00005630
      K = NCODE(7) - 1                                                  00005640
  151 IF (J .GT. K)   GO TO (156,158),  IGO                             00005650
      DO 152 I = J,K                                                    00005660
          IF (FRAMEY(I) .NE. FRAMEY(I+1))   GO TO (155,157), IGO        00005670
  152 CONTINUE                                                          00005680
          GO TO (153,154),  IGO                                         00005690
  153 NSWCH1 = 1                                                        00005700
          GO TO 156                                                     00005710
  154 NSWCH2 = 1                                                        00005720
          GO TO 158                                                     00005730
  155 NSWCH1 = 2                                                        00005740
  156 IGO = 2                                                           00005750
      J = NCODE(7) + 2                                                  00005760
      K = NCODE(7) + NCODE(10)                                          00005770
          GO TO 151                                                     00005780
  157 NSWCH2 = 2                                                        00005790
  158 CONTINUE                                                          00005800
      ZOP = CODE(6) - CODE(12)                                          00005810
      IF (ZOP)  160, 162, 161                                           00005820
  160 IF ((ZOP + THREE) .LT. ZERO)   GO TO 163                          00005830
      CODE(12) = CODE(6)                                                00005840
      NCODE(12) = NCODE(6)                                              00005850
          GO TO 162                                                     00005860
  161 IF ((ZOP - THREE) .GT. ZERO)   GO TO 163                          00005870
      CODE(6) = CODE(12)                                                00005880
      NCODE(6) = NCODE(12)                                              00005890
      XLONG(6) = XLONG(7)                                               00005900
  162 NUMBER(6) = NUMBER(6) + NUMBER(7)                                 00005910
      NUMBER(7) = 0                                                     00005920
  163 IF (IVE .EQ. 2  .OR.  IVS .EQ. 2)   GO TO 167                     00005930
      ZOP = CODE(5) - CODE(11)                                          00005940
      IF (ZOP)  164, 166, 165                                           00005950
  164 IF ((ZOP + TWO) .LT. ZERO)   GO TO 167                            00005960
      CODE(11) = CODE(5)                                                00005970
      NCODE(11) = NCODE(5)                                              00005980
          GO TO 166                                                     00005990
  165 IF ((ZOP - TWO) .GT. ZERO)   GO TO 167                            00006000
      CODE(5) = CODE(11)                                                00006010
      NCODE(5) = NCODE(11)                                              00006020
      XLONG(4) = XLONG(5)                                               00006030
  166 NUMBER(4) = NUMBER(4) + NUMBER(5)                                 00006040
      NUMBER(5) = 0                                                     00006050
  167 CONTINUE                                                          00006060
      CALL T3H31D(WINDL,MTYPE,CASE(1),CASE(2),CODE(25),CODE(26),        00006070
     1CODE(30),CODE(31),CODE(32),CODE(33),CODE(34),                     00006080
     2CODE(35),NCODE(5),NCODE(11),EVBOLT,SVBOLT,VEQTY,VSQTY,NVE,NVS)    00006090

C                                                                       00006100
COMPUTE BOLT COUNT & NET DRAWING WEIGHT :                               00006110
      NUMERO = LUNITS * NOFAN * 4                                       00006120
      IBOLT = NUMERO * 6                                                00006130
      LB = 0                                                            00006140
      K = FANSZ - 4                                                     00006150
      IF (K .GT. 12)  K = 12                                            00006160
      DO 200 I = 1,2                                                    00006170
          IBOLT = IBOLT + NUMERO * NET(K,I)                             00006180
          LB = LB + NUMERO * NET(K,I+2) / 10                            00006190
  200 CONTINUE                                                          00006200
      N = NUMBER(1) + NUMBER(11) + NUMBER(14) + NUMBER(15)              00006210
      IBOLT = IBOLT +  (NCODE(5) + 1) * N / IVE                         00006220
     2              +  (NCODE(7) + 1) * N / IVE                         00006230
     3              + (NCODE(10) + 1) * N / IVE                         00006240
     4              + (NCODE(11) + 1) * NUMBER(2) / IVS                 00006250
     6              + (NCODE(13) + 1) * NUMBER(12) * 2 / IVE            00006260
     7              + (NCODE(14) + 1) * (NUMBER(3) * 2 + NUMBER(13))    00006270
     8              + (NCODE(15) + 1) * LUNITS * NOFAN * ICOUNT * 4     00006280
     9              + EVBOLT + SVBOLT                                   00006290
C                                                                       00006300
C    1              +  (NCODE(6) + 1) * NUMBER(6)                       00006310
C    5              + (NOCDE(12) + 1) * NUMBER(7)                       00006320
C                                                                       00006330
      IGO = IVE                                                         00006340
      DO 202 I = 1,21                                                   00006350
          K = MU(I)                                                     00006360
          IF (I .EQ. 3 .AND. NUMBER(11) .EQ. 0)  K = 3                  00006370
          NU(I) = K                                                     00006380
      IF (MTYPE .EQ. 2  .AND.  I .LE. 2 .OR. I .EQ. 11 .OR. I .EQ. 14   00006390
     *    .OR. I .EQ. 15)    NU(I) = 1                                  00006400
          MATL(I) = IOTA(K)                                             00006410
          ZAP = SIGMA(K) * XLONG(I) * XWIDE(I) * NUMBER(I) /GROSS +HALF 00006420
          IF (I .GT. 2)   GO TO 201                                     00006430
          ZAP = ZAP - SIGMA(K) * (ABS(CODE(-I*20+41)) * CODE(I+24) +    00006440
     &          ABS(CODE(-I*20+41)) * 5.75 + 8.2656) * N / GROSS / IGO  00006450
          IF (IGO .EQ. 2)  ZAP = ZAP - SIGMA(K) * 0.10753 * N           00006460
          N = NUMBER(2)                                                 00006470
          IGO = IVS                                                     00006480
  201     LB = LB + ZAP                                                 00006490
  202 CONTINUE                                                          00006500
      IF (NUMBER(11) .EQ. 0)   GO TO (207,205),  IVE                    00006510
      DO 203 I = 1,4                                                    00006520
          NCODE(I) = 0                                                  00006530
  203 CONTINUE                                                          00006540
      DO 204 I = 18,19                                                  00006550
          NCODE(I) = 0                                                  00006560
  204 CONTINUE                                                          00006570
          GO TO (207,205),  IVE                                         00006580
  205 IF (NUMBER(14) .GT. 0)   GO TO 207                                00006590
      DO 206 I = 9,10                                                   00006600
          NCODE(I) = 0                                                  00006610
  206 CONTINUE                                                          00006620
  207 IF (ICOUNT .GT. 1)  GO TO 209                                     00006630
      DO 208 I = 16,17                                                  00006640
          NCODE(I) = 0                                                  00006650
  208 CONTINUE                                                          00006660
  209 CONTINUE                                                          00006670
C     IF (NCOPY .EQ. 1)   WRITE (NWTR,1002)                             00006680
C     IF (NCOPY .EQ. 1)            WRITE(NWTR,TRACE)                    00006690
 1002 FORMAT('1')                                                       00006700
 1003 FORMAT('+',36X,'-*-*- WARNING - THIS IS REVISION #',I1,' -*-*-'/  00006710
     *    38X,'*-*-*  DESTROY ALL PREVIOUS DATA  *-*-*'/)               00006720
 1004 FORMAT(' ',9X,'X',70X,'JOB: ',A4,7X,'X'/                          00006730
     A    77X,'DRAWING: ',A4/                                           00006740
     B    77X,'DATE:',3X,2(I2,'/'),I4/                                  00006750
     C    43X,'HUDSON  PRODUCTS  CORPORATION'//                         00006760
     D    41X,'M A T E R I A L    S U M M A R Y'/                       00006770
     E    41X,8('- '),3X,7('- ')/                                       00006780
     F    22X,2('   PIECE'),'   MAT''L'/                                00006790
     G    25X,'MARK     QTY     I.D.',9X,'M A T E R I A L',9X,'LENGTH'//00006800
     H    25X,'-')                                                      00006810
 1005 FORMAT('+',I27,A4,I5,I9,29X,'X',I4,'''-',I2,2A4)                  00006820
 1006 FORMAT('+',48X,'PL',I3,A4,A3,'X',6X,'(    )')                     00006830
 1007 FORMAT('+',25X,'03R',I2,I6,'    03R',I2,'   STD. QTR. FAN RING ASS00006840
     *''Y')                                                             00006850
 1008 FORMAT('+',14X,2(14X,'0'))                                        00006860
 1009 FORMAT(' ',25X,'03T',I2,I6,'    03T',I2,'   STD. QTR. TOP PANEL') 00006870
 1010 FORMAT('0',63X,'TOTAL NET WEIGHT =',I7,'#'/64X,'TOTAL BOLT COUNT =00006880
     A'   ,I7//      37X,'M A S T E R - D E T A I L    L I S T I N G'/  00006890
     B    37X,2(6('- '),2X),7(' -')/                                    00006900
     C    35X,'TO BE USED ONLY W/ MASTER DWG: ''',A4,''''/              00006910
     D    35X,'AND ONLY W/ THESE THREE CASES: ',2(A4,','),A4/           00006920
     E    2(25X,'CODE  INCHES',13X)/)                                   00006930
 1011 FORMAT(' ',I27,' --',I3,2A4)                                      00006940
 1012 FORMAT('+',29X,'>')                                               00006950
 1013 FORMAT('+',39X,'W/',I2,' EQUAL BOLT SPACES')                      00006960
 1014 FORMAT('+',24X,'*',14X,'W/',I2,' UNEQUAL BOLT SPACES')            00006970
 1015 FORMAT('+',74X,I3,' --',I3,2A4)                                   00006980
 1016 FORMAT('+',79X,'>')                                               00006990
 1017 FORMAT(' ',24X,'*',I2,'(L-TO-R):')                                00007000
 1018 FORMAT(' ',24X,'*',I2,'(R-TO-L):')                                00007010
 1019 FORMAT('+',90X,','/)                                              00007020
      IF (NCOPY .NE. 1)   NCOPY = 20                                    00007030
      NN = FANSZ + 20                                                   00007040
      IF (ITEMS .EQ. 1)   NN = NN + 20                                  00007050

C RJD 6/18/93 Adjust fan part numbers 25...29 and 45... 49 to 65...69
      IF ( (NN .GE. 25) .AND. (NN .LE. 29)) NN = NN + 40
      IF ( (NN .GE. 45) .AND. (NN .LE. 49)) NN = NN + 20

 7000 FORMAT(F12.8)
 7001 FORMAT(A4)
      WRITE(7,7000) XWIDE(1)
      WRITE(7,7000) XWIDE(2)
      WRITE(7,7001) DWG
      WRITE(7,7001) CASE(1)
      WRITE(7,7001) CASE(2)
      WRITE(7,7001) CASE(3)
      WRITE(7,7000) ABS(CODE(1))
      WRITE(7,7000) ABS(CODE(20))
      WRITE(7,7000) ABS(CODE(21))

      DO 250 I = 1,NCOPY                                                00007060
C         WRITE (NWTR,1002)                                             00007070
          IF (NOREV .GT. 0)   WRITE (NWTR,1003)  NOREV                  00007080
          WRITE (NWTR,1004)   JOBNO, IDRAW, IDATEX                      00007090
               J = 0                                                    00007100
  210          J = J + 1                                                00007110
               IF (J .EQ. 6) GO TO 210                                  00007120
               IF (J .EQ. 7) GO TO 210                                  00007130
               IF (J .GT. 21) GO TO 216                                 00007140
               IF (J .EQ. 4) NUMBER(J) = VEQTY                          00007150
               IF (J .EQ. 5) NUMBER(J) = VSQTY                          00007160
               IF (NUMBER(J) .EQ. 0) GO TO 210                          00007170
               ZAP = XLONG(J)                                           00007180
               ZIP = TWELVE                                             00007190
               IGO = 1                                                  00007200
                    GO TO 225                                           00007210
  211          KK = K                                                   00007220
               ZIP = ONE                                                00007230
               IGO = 2                                                  00007240
                    GO TO 225                                           00007250
  212          WRITE (NWTR,1005)  J, MARKER(J), NUMBER(J), MATL(J), KK, 00007260
     *                            K, (ALPHA(M,N),M=1,2)                 00007270
               IF (NU(J) .GT. 3)  GO TO 214                             00007280
               ZAP = XWIDE(J)                                           00007290
               IGO = 3                                                  00007300
                    GO TO 225                                           00007310
  213          WRITE (NWTR,1006)  K, (ALPHA(M,N),M=1,2)                 00007320
  214          N = NU(J)                                                00007330
               DO 215 M = 1,4                                           00007340
                    OMEGA(M+2) = BETA(M,N)                              00007350
  215          CONTINUE                                                 00007360
               WRITE (NWTR,OMEGA)                                       00007370
                    GO TO 210                                           00007380
  216     IF (INTS .EQ. 0)   GO TO 219                                  00007390
          IF (INTS .EQ. 1  .OR.  FANSZ .LE. 9)   GO TO 218              00007400
          L = LUNITS * NOFAN                                            00007410
          IF (INTS .EQ. 100)   GO TO 217                                00007420
          IF (INTS .EQ. 33)    L = L / 3                                00007430
          IF (INTS .EQ. 50)    L = L / 2                                00007440
          IF (INTS .EQ. 66  .OR.  INTS .EQ. 67)   L = L * 2 / 3         00007450
  217     IF (I .EQ. 1)   LB = LB + GUARDS(2,1) * L                     00007460
          WRITE (NWTR,1050)   GUARDS(1,1), L, GUARDS(1,1)               00007470
 1050 FORMAT('+',25X,'0',A4,I6,4X,'0',A4,'   STD. AV FAN GUARD CLIP'/)  00007480
  218     J = FANSZ - 3                                                 00007490
          IF (I .EQ. 1)   LB = LB + GUARDS(2,J) * NUMERO                00007500
          WRITE (NWTR,1051)   GUARDS(1,J), NUMERO, GUARDS(1,J)          00007510
 1051 FORMAT('+',25X,'0',A4,I6,4X,'0',A4,'   STD. QTR. FAN GUARD ASS''Y'00007520
     &/)                                                                00007530
  219 CONTINUE                                                          00007540
          WRITE (NWTR,1007)   NN, NUMERO, NN                            00007550
          WRITE (NWTR,1009)   FANSZ, NUMERO, FANSZ                      00007560
          IF (FANSZ .LT. 10)   WRITE (NWTR,1008)                        00007570
      WRITE (NWTR,1010)   LB, IBOLT, DWG, CASE                          00007580

      WRITE (8,8000) LB
 8000 FORMAT(I7.7)

COUNTER FOR L.H. COL. OF CODES IS "L"                                   00007590
COUNTER FOR R.H. COL. OF CODES IS "J"                                   00007600
               IF (NCODE(11) .EQ. 0) NCODE(11) = NCODE(5)               00007610
               IF (NCODE(5) .EQ. 0) NCODE(5)  = NCODE(11)               00007620
               L = 0                                                    00007630
               J = 15                                                   00007640
               ZIP = ONE                                                00007650
  220          L = L + 1                                                00007660
               IF (L .EQ. 6) GO TO 220                                  00007670
               IF (L .EQ. 12) GO TO 220                                 00007680
               IF (L .LT. 16)   GO TO 221                               00007690
          IF (J .GT. 29)   GO TO 230                                    00007700
               WRITE (NWTR,1011)                                        00007710
                    GO TO 224                                           00007720
  221          IF (NCODE(L) .EQ. 0)  GO TO 220                          00007730
               ZAP = ABS(CODE(L))                                       00007740
               IGO = 4                                                  00007750
                    GO TO 225                                           00007760
  222          WRITE (NWTR,1011)  L, K, (ALPHA(M,N),M=1,2)              00007770
               WRITE (NWTR,1012)                                        00007780
               IF (NCODE(L) .EQ. 1)  GO TO 224                          00007790
               IF (L .EQ. 7)   GO TO (227,223),  NSWCH1                 00007800
               IF (L .EQ. 10)  GO TO (227,223),  NSWCH2                 00007810
  227          WRITE (NWTR,1013)  NCODE(L)                              00007820
                    GO TO 224                                           00007830
  223          WRITE (NWTR,1014)  NCODE(L)                              00007840
  224          J = J + 1                                                00007850
               IF (J .GT. 29)  GO TO 220                                00007860
               IF (NCODE(J) .EQ. 0)  GO TO 224                          00007870
               ZAP = ABS(CODE(J))                                       00007880
               IGO = 5                                                  00007890
  225     K = (ZAP + .03125) / ZIP                                      00007900
          ZAP = ZAP - ZIP * K                                           00007910
          N = ZAP * 16. + 1.5                                           00007920
          GO TO (211,212,213,222,226,238),  IGO                         00007930
  226          WRITE (NWTR,1015)  J, K, (ALPHA(M,N),M=1,2)              00007940
               WRITE (NWTR,1016)                                        00007950
                    GO TO 220                                           00007960
C                                                                       00007970
  230     DO 1249 II = 1, 6                                             00007980
          III =  II + 29                                                00007990
          J = III                                                       00008000
          K = CODE(III)                                                 00008010
          ZAP = CODE(III) - K                                           00008020
          N = ZAP * 16. + 1.5                                           00008030
          IF (J .EQ. 30)GO TO 1243                                      00008040
          IF (J .EQ. 31)GO TO 1242                                      00008050
          IF (J .EQ. 32)GO TO 1242                                      00008060
          IF (J .EQ. 33)GO TO 1244                                      00008070
          IF (J .EQ. 34)GO TO 1242                                      00008080
          IF (J .EQ. 35)GO TO 1242                                      00008090
          GO TO 1249                                                    00008100
1243      WRITE (NWTR,11270) NVE                                        00008110
          WRITE (NWTR,11275)                                            00008120
          WRITE (NWTR,11276)                                            00008130
          GO TO 1241                                                    00008140
1244      WRITE (NWTR,11271) NVS                                        00008150
          WRITE (NWTR,11275)                                            00008160
          WRITE (NWTR,11276)                                            00008170
1241      WRITE (NWTR,11272) J, K, (ALPHA(M,N),M=1,2)                   00008180
          GO TO 1240                                                    00008190
1242      IF (CODE(III) .LE. 0.0) GO TO 1249                            00008200
          WRITE (NWTR,11273) J, K, (ALPHA(M,N),M=1,2)                   00008210
1240      WRITE (NWTR,11274)                                            00008220
1249      CONTINUE                                                      00008230
11270     FORMAT(' ',35X,I3,' STIFF. IN EACH CENTER/END PANEL.---')     00008240
11271     FORMAT(' ',35X,I3,' STIFF. IN EACH SIDE PANEL.---------')     00008250
11272     FORMAT('+',74X,I3,' --',I3,2A4)                               00008260
11273     FORMAT(' ',74X,I3,' --',I3,2A4)                               00008270
11274     FORMAT('+',79X,'>')                                           00008280
11275     FORMAT('+',73X,'>')                                           00008290
11276     FORMAT('+',72X,'>')                                           00008300
C                                                                       00008310
          WRITE (NWTR,1011)                                             00008320
          IGO = 6                                                       00008330
          IF (NCODE(7) .LE. 1  .OR.  NSWCH1 .EQ. 1)   GO TO 232         00008340
          L = 7                                                         00008350
          LL = 0                                                        00008360
  231     WRITE (NWTR,1017)  L                                          00008370
               GO TO 234                                                00008380
  232     IF (NCODE(10) .LE. 1  .OR.  NSWCH2 .EQ. 1)  GO TO 240         00008390
          L = 10                                                        00008400
          LL = NCODE(7) + 1                                             00008410
          IF (LL .LT. 2)  LL = 2                                        00008420
          DO 233 J = 1,3                                                00008430
               IF (EPSILO(J) .EQ. CASE(1))  GO TO 231                   00008440
  233     CONTINUE                                                      00008450
          WRITE (NWTR,1018)  L                                          00008460
  234     LLL = NCODE(L) + LL                                           00008470
  235     J = 0                                                         00008480
  236     LL = LL + 1                                                   00008490
          IF (LL .LE. LLL)  GO TO 237                                   00008500
          IF (L .EQ. 7)     GO TO 232                                   00008510
               GO TO 240                                                00008520
  237     IF (FRAMEY(LL) .EQ. ZERO)  GO TO 236                          00008530
          ZAP = FRAMEY(LL)                                              00008540
          J = J + 1                                                     00008550
          IF (J .LE. 2)   FORMC(3) = FORMA(J)                           00008560
          FORMC(2) = FORMB(J)                                           00008570
               GO TO 225                                                00008580
  238     WRITE (NWTR,FORMC)  K, (ALPHA(M,N),M=1,2)                     00008590
          IF (J .LT. 7)  GO TO 236                                      00008600
          IF (LL .LT. LLL)   WRITE (NWTR,1019)                          00008610
                GO TO 235                                               00008620
  240     WRITE (NWTR,1052)    I                                        00008630
 1052 FORMAT ('0',1X,I90//)                                             00008640
          IF (NOREV .NE. 0)   WRITE (NWTR,1003)   NOREV                 00008650
  250 CONTINUE                                                          00008660
C                                                                       00008670
      WRITE (NWTR,1002)                                                 00008680
      WRITE (NWTR,12000)                                                00008690
12000 FORMAT(' ',43X,'INPUT DATA')                                      00008700
      WRITE (NWTR,12001) JOBNO                                          00008710
      WRITE (NWTR,12002) IDRAW                                          00008720
      WRITE (NWTR,12003) MODEL                                          00008730
      WRITE (NWTR,12004) LTUBE                                          00008740
      WRITE (NWTR,12005) MODAL                                          00008750
      WRITE (NWTR,12006) NOFAN                                          00008760
      WRITE (NWTR,12007) ITYPE                                          00008770
      WRITE (NWTR,12008) H                                              00008780
      WRITE (NWTR,12009) F                                              00008790
      WRITE (NWTR,12010) WINDL                                          00008800
      WRITE (NWTR,12011) MTYPE                                          00008810
      WRITE (NWTR,12012) NCOPY                                          00008820
      WRITE (NWTR,12013) ISHIP                                          00008830
      WRITE (NWTR,12014) NOREV                                          00008840
      WRITE (NWTR,12015) LUNITS                                         00008850
      WRITE (NWTR,12016) FRAME(1)                                       00008860
      WRITE (NWTR,12017) SPACE(1)                                       00008870
      WRITE (NWTR,12018) FRAME(2)                                       00008880
      WRITE (NWTR,12019) SPACE(2)                                       00008890
      WRITE (NWTR,12020) FRAME(3)                                       00008900
      WRITE (NWTR,12021) SPACE(3)                                       00008910
      WRITE (NWTR,12022) FRAME(4)                                       00008920
      WRITE (NWTR,12023) E                                              00008930
      WRITE (NWTR,12024) INTS                                           00008940
      WRITE (NWTR,12025) ITEMS                                          00008950
12001 FORMAT(' ',24X,'JOB NO. ---------------= ',A5)                    00008960
12002 FORMAT(' ',24X,'DRAWING NO. -----------= ',A4)                    00008970
12003 FORMAT(' ',24X,'MODEL NO. -------------= ',A5)                    00008980
12004 FORMAT(' ',24X,'TUBE LNG. -------------= ',I3)                    00008990
12005 FORMAT(' ',24X,'NO. OF ROWS -----------= ',A3)                    00009000
12006 FORMAT(' ',24X,'NO. OF FANS -----------= ',I2)                    00009010
12007 FORMAT(' ',24X,'UNIT TYPE -------------= ',A5)                    00009020
12008 FORMAT(' ',24X,'HOOD HGT. -------------= ',F4.1)                  00009030
12009 FORMAT(' ',24X,'FAN DIAM. -------------= ',F5.1)                  00009040
12010 FORMAT(' ',24X,'WIND LOAD -------------= ',F4.1)                  00009050
12011 FORMAT(' ',24X,'PLENUM MATL. CODE -----= ',I2)                    00009060
12012 FORMAT(' ',24X,'SUMMARY COPIES --------= ',I2)                    00009070
12013 FORMAT(' ',24X,'UNIT SHIPPING CODE ----= ',I2)                    00009080
12014 FORMAT(' ',24X,'DRAWING REVISION ------= ',I2)                    00009090
12015 FORMAT(' ',24X,'UNIT QTY. -------------= ',I3)                    00009100
12016 FORMAT(' ',24X,'FIRST BUNDLE WIDTH ----= ',F6.2)                  00009110
12017 FORMAT(' ',24X,'BUNDLE CLEARANCE ------= ',F6.3)                  00009120
12018 FORMAT(' ',24X,'SECOND BUNDLE WIDTH ---= ',F6.2)                  00009130
12019 FORMAT(' ',24X,'BUNDLE CLEARANCE ------= ',F6.3)                  00009140
12020 FORMAT(' ',24X,'THIRD BUNDLE WIDTH ----= ',F6.2)                  00009150
12021 FORMAT(' ',24X,'BUNDLE CLEARANCE ------= ',F6.3)                  00009160
12022 FORMAT(' ',24X,'FOURTH BUNDLE WIDHT ---= ',F6.2)                  00009170
12023 FORMAT(' ',24X,'UNIT CL TO D-ANBLE GA. = ',F8.4)                  00009180
12024 FORMAT(' ',24X,'FAN GUARDS ------------= ',I4)                    00009190
12025 FORMAT(' ',24X,'FAN RING CODE ---------= ',I2)                    00009200
COORDINATE PLOTTING FOR ALL HOOD PANELS BEGINS HERE * * * * * * * * * * 00009210
      IGO = 1                                                           00009220
      MARK = IGO                                                        00009230
  300 XY(1,1) = -THREE                                                  00009240
      XY(2,1) = 3.                                                      00009250
COORDINATES TO CONNECT THE CORNER SPLICE ANGLE TO ALL HOOD PANELS:      00009260
      ZIP = CODE(IGO+24) / SLOPE                                        00009270
      ZAP = CODE(-IGO*20+41) / SLOPE                                    00009280
      IF (ZAP)  301, 302, 303                                           00009290
  301 ZOP = ABS(CODE(-IGO*20+41))                                       00009300
          GO TO 305                                                     00009310
  302 GO TO (303,304),  IGO                                             00009320
  303 ZOP = ZERO                                                        00009330
          GO TO 305                                                     00009340
  304 ZOP = QTR                                                         00009350
  305 XY(1,2) = ZAP * CODE(27) + ZIP * TWO + ZOP                        00009360
      XY(2,2) = ZIP * CODE(27) - ZAP * TWO + 2.875                      00009370
      K = 2                                                             00009380
      N = NCODE(14)                                                     00009390
      DO 306 I = 1,N                                                    00009400
          XY(1,K+1) = ZAP * CODE(14) / N + XY(1,K)                      00009410
          XY(2,K+1) = ZIP * CODE(14) / N + XY(2,K)                      00009420
          K = K + 1                                                     00009430
  306 CONTINUE                                                          00009440
COORDINATES FOR EDGE MARKING THE CORNER CUT-OFFS FOR 2ND SHEARING OPER: 00009450
C     IF (ZOP .EQ. QTR)   GO TO (310,311),  IGO                         00009460
C 310 XY(1,K+1) = ZAP * CODE(27) - ZIP * .34375 + ZOP                   00009470
C     XY(2,K+1) = ZIP * CODE(27) + ZAP * .34375 + 2.875                 00009480
C     XY(1,K+2) = ZAP * CODE(14) + XY(1,K+1)                            00009490
C     XY(2,K+2) = ZIP * CODE(14) + XY(2,K+1)                            00009500
C     K = K + 2                                                         00009510
C 311 XY(1,K+1) = ZOP                                                   00009520
C     XY(2,K+1) = 2.3888                                                00009530
C     XY(1,K+2) = XY(1,K+1) + 1.8888                                    00009540
C     XY(2,K+2) = XY(2,K+1) - 1.8888                                    00009550
C     K = K + 3                                                         00009560
      K = K + 1                                                         00009570
COORDINATES TO CONNECT THE TOP PANEL & FAN RING TO ALL HOOD PANELS:     00009580
      XY(1,K) = ZOP + 4.0                                               00009590
      XY(2,K) = 1.25                                                    00009600
      N = NCODE(13)                                                     00009610
      DO 320 I = 1,N                                                    00009620
          XY(1,K+1) = CODE(13) / N + XY(1,K)                            00009630
          IF (I .LT. N)  XY(2,K+1) = XY(2,K)                            00009640
          K = K + 1                                                     00009650
  320 CONTINUE                                                          00009660
      XY(2,K) = CODE(29)                                                00009670
COORDINATES TO CONNECT THE COMMON CENTER ANGLE TO COMMON PANELS ONLY:   00009680
      IF (MARK .EQ. 11  .OR.  MARK .EQ. 15)   GO TO 329                 00009690
          GO TO 340                                                     00009700
  329 K = K + 1                                                         00009710
      XY(1,K) = ZOP + THREE                                             00009720
      XY(2,K) = 4.4375                                                  00009730
      DO 330 I = 1,N                                                    00009740
          XY(1,K+1) = CODE(13) / N + XY(1,K)                            00009750
          XY(2,K+1) = XY(2,K)                                           00009760
          K = K + 1                                                     00009770
  330 CONTINUE                                                          00009780
      XY(1,K+1) = XY(1,K)                                               00009790
          GO TO 342                                                     00009800
COORDINATES TO CONNECT THE DIAGONAL BRACE ANGLE TO ALL HOOD PANELS:     00009810
  340 XY(1,K+1) = XY(1,K) - ONE                                         00009820
      IF (NUMBER(11) .GT. 0)   GO TO (342,341), IGO                     00009830
  341 CONTINUE                                                          00009840
C     XY(2,K+1) = 5.875                                                 00009850
          GO TO 343                                                     00009860
  342 CONTINUE                                                          00009870
C     XY(2,K+1) = 7.4375                                                00009880
          GO TO (343,344),  IVE                                         00009890
  343 ZIP = CODE(IGO*6-1)                                               00009900
          GO TO 345                                                     00009910
  344 ZIP = CODE(IGO*6-1) - 1.5625                                      00009920
  345 ZAP = SQRT(CODE(IGO*6)**2 - ZIP**2)                               00009930
C     XY(2,K+1) = XY(2,K+1) + ZIP                                       00009940
C     K = K + 1                                                         00009950
C     KK = K                                                            00009960
      N = NCODE(IGO*6)                                                  00009970
      DO 346 I = 1,N                                                    00009980
C         XY(1,K+1) = -ZAP / N + XY(1,K)                                00009990
C         XY(2,K+1) = -ZIP / N + XY(2,K)                                00010000
C         K = K + 1                                                     00010010
  346 CONTINUE                                                          00010020
C     XY(2,K) = XY(2,KK) - ZIP                                          00010030
          GO TO (360,350),  IGO                                         00010040
COORDINATES TO CONNECT THE TUBE BUNDLE FRAME TO SIDE PANELS ONLY:       00010050
  350 K = K + 1                                                         00010060
      XY(1,K) = CODE(28)                                                00010070
      IF (CODE(1) .GT. ZERO)  XY(1,K) = XY(1,K) + CODE(1)               00010080
      XY(2,K) = XWIDE(MARK) - 1.25                                      00010090
      N = NCODE(15)                                                     00010100
      DO 351 I = 1,N                                                    00010110
          XY(1,K+1) = CODE(15) / N + XY(1,K)                            00010120
          XY(2,K+1) = XY(2,K)                                           00010130
          K = K + 1                                                     00010140
  351 CONTINUE                                                          00010150
          GO TO (361,372),  IVS                                         00010160
COMMENT - AT THIS POINT, ALL COORDINATES ARE SYMMETRICAL TO THE CENTER- 00010170
C         LINE OF THE FAN.  IF THE RESPECTIVE PANEL IS A "WHOLE" PANEL, 00010180
C         THEN PLOT THE COMPLEMENT COORDINATES.                         00010190
  360     GO TO (361,372),  IVE                                         00010200
  361 N = K                                                             00010210
      DO 362 I = 2,N                                                    00010220
          K = K + 1                                                     00010230
          XY(1,K) = XLONG(MARK) - XY(1,I)                               00010240
          XY(2,K) = XY(2,I)                                             00010250
  362 CONTINUE                                                          00010260
COORDINATES FOR FAN CENTERLINE CONNECTIONS TO ALL PANELS:               00010270
      IF (NUMBER(11) .GT. 0)   GO TO (371,370),  IGO                    00010280
  370 XY(2,K+1) = 5.875                                                 00010290
      SAVEXY = 5.875                                                    00010300
      GO TO (2371,2372), IGO                                            00010310
 2371 NK = 1                                                            00010320
      NNN = NVE                                                         00010330
      IF (NVE .EQ. 2) GO TO 1389                                        00010340
      IF (NVE .EQ. 4) GO TO 1389                                        00010350
      IF (NVE .EQ. 6) GO TO 1389                                        00010360
          GO TO 373                                                     00010370
 2372 NK = 4                                                            00010380
      NNN = NVS                                                         00010390
      IF (NVS .EQ. 2) GO TO 1389                                        00010400
      IF (NVS .EQ. 4) GO TO 1389                                        00010410
      IF (NVS .EQ. 6) GO TO 1389                                        00010420
          GO TO 373                                                     00010430
  371 XY(2,K+1) = 7.4375                                                00010440
      SAVEXY = 7.4375                                                   00010450
      GO TO (2373,2374), IGO                                            00010460
 2373 NK = 1                                                            00010470
      NNN = NVE                                                         00010480
      IF (NVE .EQ. 2) GO TO 1389                                        00010490
      IF (NVE .EQ. 4) GO TO 1389                                        00010500
      IF (NVE .EQ. 6) GO TO 1389                                        00010510
          GO TO 373                                                     00010520
 2374 NK = 4                                                            00010530
      NNN = NVS                                                         00010540
      IF (NVS .EQ. 2) GO TO 1389                                        00010550
      IF (NVS .EQ. 4) GO TO 1389                                        00010560
      IF (NVS .EQ. 6) GO TO 1389                                        00010570
          GO TO 373                                                     00010580
C 372 XY(1,K+1) = XLONG(MARK) - THREE + THK + .1875                     00010590
C     XY (2,K+1) = 2.6875                                               00010600
C     XY(1,K+2) = XY(1,K+1)                                             00010610
C     XY(2,K+2) = XWIDE(MARK) - CODE(IGO*20-18) + .1875                 00010620
C     K = K + 2                                                         00010630
  372 XY(1,K+1) = XLONG(MARK) - 1.25                                    00010640
          GO TO 370                                                     00010650
  373 XY(1,K+1) = XLONG(MARK) * HALF                                    00010660
      K = K + 1                                                         00010670
      N = NCODE(IGO*6-1)                                                00010680
      DO 374 I = 1,N                                                    00010690
          XY(1,K+1) = XY(1,K)                                           00010700
          XY(2,K+1) = CODE(IGO*6-1) / N + XY(2,K)                       00010710
          K = K + 1                                                     00010720
  374 CONTINUE                                                          00010730
C                                                                       00010740
      NNN = NNN - 1                                                     00010750
 1389 K = K + 1                                                         00010760
      GO TO (1401,1402,1403,1401,1402,1403), NK                         00010770
 1401 XY(1,K) = XLONG(MARK) * HALF - CODE(NK+29)                        00010780
      GO TO(1501,1502),IGO                                              00010790
 1501 IF(IVE .EQ. 2)XY(1,K)=XLONG(MARK)-1.25-CODE(NK+29)                00010800
      GO TO 1391                                                        00010810
 1502 IF(IVS .EQ. 2)XY(1,K)=XLONG(MARK)-1.25-CODE(NK+29)                00010820
      GO TO 1391                                                        00010830
 1402 XY(1,K) = XLONG(MARK) * HALF - (CODE(NK+28) +  CODE(NK+29))       00010840
      GO TO(1503,1504),IGO                                              00010850
 1503 IF(IVE .EQ. 2)                                                    00010860
     1XY(1,K)=XLONG(MARK)-1.25- (CODE(NK+28)+ CODE(NK+29))              00010870
      GO TO 1391                                                        00010880
 1504 IF(IVS .EQ. 2)                                                    00010890
     1XY(1,K)=XLONG(MARK)-1.25- (CODE(NK+28)+ CODE(NK+29))              00010900
      GO TO 1391                                                        00010910
 1403 XY(1,K)=XLONG(MARK)*HALF- (CODE(NK+27)+ CODE(NK+28)+ CODE(NK+29)) 00010920
      GO TO(1505,1506),IGO                                              00010930
 1505 IF(IVE .EQ. 2)                                                    00010940
     1XY(1,K)=XLONG(MARK)-1.25- (CODE(NK+27)+ CODE(NK+28)+ CODE(NK+29)) 00010950
      GO TO 1391                                                        00010960
 1506 IF(IVS .EQ. 2)                                                    00010970
     1XY(1,K)=XLONG(MARK)-1.25- (CODE(NK+27)+ CODE(NK+28)+ CODE(NK+29)) 00010980
 1391 XY(2,K) = SAVEXY                                                  00010990
      N = NCODE(IGO*6-1)                                                00011000
      DO 1374 I = 1,N                                                   00011010
          XY(1,K+1) = XY(1,K)                                           00011020
          XY(2,K+1) = CODE(IGO*6-1) / N + XY(2,K)                       00011030
          K = K + 1                                                     00011040
 1374 CONTINUE                                                          00011050
      K = K + 1                                                         00011060
      GO TO (1411,1412,1413,1411,1412,1413), NK                         00011070
 1411 GO TO(1511,1512),IGO                                              00011080
 1511 IF(IVE .EQ. 2)GO TO 1398                                          00011090
      XY(1,K)=XLONG(MARK)*HALF+  CODE(NK+29)                            00011100
      GO TO 1397                                                        00011110
 1512 IF(IVS .EQ. 2)GO TO 1398                                          00011120
      XY(1,K)=XLONG(MARK)*HALF+  CODE(NK+29)                            00011130
      GO TO 1397                                                        00011140
 1412 GO TO(1513,1514),IGO                                              00011150
 1513 IF(IVE .EQ. 2)GO TO 1398                                          00011160
      XY(1,K)=XLONG(MARK)*HALF+ (CODE(NK+28)+ CODE(NK+29))              00011170
      GO TO 1397                                                        00011180
 1514 IF(IVS .EQ. 2)GO TO 1398                                          00011190
      XY(1,K)=XLONG(MARK)*HALF+ (CODE(NK+28)+ CODE(NK+29))              00011200
      GO TO 1397                                                        00011210
 1413 GO TO(1515,1516),IGO                                              00011220
 1515 IF(IVE .EQ. 2)GO TO 1398                                          00011230
      XY(1,K)=XLONG(MARK)*HALF+ (CODE(NK+27)+ CODE(NK+28)+ CODE(NK+29)) 00011240
      GO TO 1397                                                        00011250
 1516 IF(IVS .EQ. 2)GO TO 1398                                          00011260
      XY(1,K)=XLONG(MARK)*HALF+ (CODE(NK+27)+ CODE(NK+28)+ CODE(NK+29)) 00011270
 1397 XY(2,K) = SAVEXY                                                  00011280
      N = NCODE(IGO*6-1)                                                00011290
      DO 1375 I = 1,N                                                   00011300
          XY(1,K+1) = XY(1,K)                                           00011310
          XY(2,K+1) = CODE(IGO*6-1) / N + XY(2,K)                       00011320
          K = K + 1                                                     00011330
 1375 CONTINUE                                                          00011340
 1398     NK = NK + 1                                                   00011350
          NNN = NNN - 2                                                 00011360
          IF (NNN .LE. 0) GO TO 1399                                    00011370
          GO TO 1389                                                    00011380
C                                                                       00011390
 1399     GO TO (380,400),  IGO                                         00011400
COORDINATES TO CONNECT THE TUBE BUNDLE ASS'Y TO END & COMMON PANELS ONLY00011410
  380 K = K + 1                                                         00011420
      XY(1,K) = 2.75                                                    00011430
      IF (CODE(21) .GT. ZERO)   XY(1,K) = XY(1,K) + CODE(21)            00011440
      IF (MARK .EQ. 1 .OR. MARK .EQ. 14)   XY(2,K) = XWIDE(MARK) - 1.25 00011450
      IF (MARK .EQ. 11 .OR. MARK .EQ. 15)                               00011460
     *    XY(2,K) = XWIDE(MARK) - CODE(2) + 1.5625                      00011470
      IF (MARK .GT. 11)   GO TO 390                                     00011480
      ZAP = CODE(7) + XY(1,K)                                           00011490
      DO 383 I = 1,32                                                   00011500
          IF (FRAMEY(I) .LE. ZERO)   GO TO 400                          00011510
          IF ((XY(1,K) + FRAMEY(I)) .GT. ZAP)   GO TO (382,400),  IVE   00011520
  382     XY(1,K+1) = XY(1,K) + FRAMEY(I)                               00011530
          XY(2,K+1) = XY(2,K)                                           00011540
          K = K + 1                                                     00011550
  383 CONTINUE                                                          00011560
          GO TO 400                                                     00011570
  390 ZAP = CODE(10) + XY(1,K)                                          00011580
      DO 393 J = 1,32                                                   00011590
          IF (FRAMEY(-J+33) .LE. ZERO)   GO TO 393                      00011600
          IF ((XY(1,K) + FRAMEY(-J+33)) .GT. ZAP)   GO TO 400           00011610
          XY(1,K+1) = XY(1,K) + FRAMEY(-J+33)                           00011620
          XY(2,K+1) = XY(2,K)                                           00011630
          K = K + 1                                                     00011640
  393 CONTINUE                                                          00011650
COORDINATES ARE NOW COMPLETE FOR THIS PIECE MARK.  OUTPUT THIS LISTING. 00011660
  400 CONTINUE                                                          00011670
      WRITE (NWTR,1002)                                                 00011680
      WRITE (NWTR,1020)   K, JOBNO, IDRAW, MARK, MARKER(MARK)           00011690
 1020 FORMAT('0',24X,I2,' COORDINATES FOR ',2(A4,' - '),I2,A4//         00011700
     &    33X,'X',15X,'Y'/)                                             00011710
 1021 FORMAT(' ',24X,I2,')',F11.3,F18.3,'  (',I2)                       00011720

 7002 FORMAT(I3)
 7003 FORMAT(F12.8,',',F12.8)
      WRITE(7,7002) K
 
      DO 401 J = 1,K                                                    00011730
          WRITE (NWTR,1021)  J, (XY(I,J),I=1,2), J                      00011740
	  
	  WRITE(7,7003) (XY(I,J),I=1,2)
	  
  401 CONTINUE                                                          00011750
C     IF  ((MARK .EQ. 1  .AND.  IVE .EQ. 1)   .OR.                      00011760
C    &     (MARK .EQ. 2  .AND.  IVS .EQ. 1))  GO TO 402                 00011770

C RJD 8/4/92 Disabling the "Mirror Image" routine
      GO TO 404                                                         00011780
      WRITE (NWTR,1002)                                                 00011790
      WRITE (NWTR,1020)   K, JOBNO, IDRAW, MARK, MARKER(MARK)           00011800
      WRITE (NWTR,1022)                                                 00011810
 1022 FORMAT (' ',22X,'MIRROR IMAGE WAS EXECUTED'/)                     00011820
      DO 403 J = 1,K                                                    00011830
          XY(1,J) = XLONG(MARK) - XY(1,J)                               00011840
          XY(2,J) = XWIDE(MARK) - XY(2,J)                               00011850
          WRITE (NWTR,1021)   J, (XY(I,J),I=1,2), J                     00011860
  403 CONTINUE                                                          00011870
  404 CONTINUE                                                          00011880
C******THE FOLLOWING IS FOR THE 647 NC PROGRAM FORMATTING ONLY: ********00011890
      DO 395 J = 2,K                                                    00011900
          X(J-1) = XY(1,J)                                              00011910
          Y(J-1) = XY(2,J)                                              00011920
  395 CONTINUE                                                          00011930
      K1 = K - 1                                                        00011940
      IDIOT = IVE                                                       00011950
      IF (MARK .EQ. 2)   IDIOT = IVS                                    00011960
      MARK2 = IDIOT                                                     00011970
      MARK3 = 1                                                         00011980
  396 CONTINUE                                                          00011990
      IF (MARK .EQ. 3) CALL T3H18D                                      00012000
     &(IDIOT,I,J,KK,L,M,N,LL,K3,K4,XX,YY,IDO,M1,M2,M3,X60,              00012010
     &    ZIP,ZOP,IX,IY,IXY,II,5.)                                      00012020
C***********************************************************************00012030
      IF (MARK .EQ. 3)   GO TO 420                                      00012040
      IF (MARK .EQ. 1)   GO TO 405                                      00012050
      IF (MARK .EQ. 14)  GO TO 406                                      00012060
      IF (MARK .EQ. 11)  GO TO 407                                      00012070
      IF (MARK .EQ. 15)  GO TO 408                                      00012080
          GO TO 409                                                     00012090
  405 IF (NUMBER(14) .EQ. 0)  GO TO 406                                 00012100
      MARK = 14                                                         00012110
          GO TO 300                                                     00012120
  406 IF (NUMBER(11) .EQ. 0)  GO TO 407                                 00012130
      MARK = 11                                                         00012140
          GO TO 300                                                     00012150
  407 IF (NUMBER(15) .EQ. 0)  GO TO 408                                 00012160
      MARK = 15                                                         00012170
          GO TO 300                                                     00012180
  408 MARK = 2                                                          00012190
      IGO = MARK                                                        00012200
          GO TO 300                                                     00012210
  409 CONTINUE                                                          00012220
      IF (NUMBER(11) .NE. 0)   GO TO 421                                00012230
      MARK = 3                                                          00012240
      MARK2 = 1                                                         00012250
      IDIOT = MARK2                                                     00012260
      K1 = NCODE(14) * 2 + 2                                            00012270
      X(1) = EDGE                                                       00012280
      X(2) = X(1)                                                       00012290
      Y(2) = 1.25                                                       00012300
      Y(1) = XWIDE(3) - Y(2)                                            00012310
      N = NCODE(14)                                                     00012320
      DO 411 J = 1,N                                                    00012330
          DO 410 I = 1,2                                                00012340
              X(J*2+I) = X(I) + CODE(14) * J / N                        00012350
              Y(J*2+I) = Y(I)                                           00012360
  410     CONTINUE                                                      00012370
  411 CONTINUE                                                          00012380
          GO TO 396                                                     00012390
C                                                                       00012400
  420 M = 4                                                             00012410
          GO TO 422                                                     00012420
  421 M = 3                                                             00012430
  422 DO 440 II = M,13                                                  00012440
          IF (NUMBER(II) .EQ. 0)   GO TO 440                            00012450
C         GO TO(440,440,423,424,425,426,427,440,440,440,440,429,423),II 00012460
          GO TO(440,440,423,424,425,440,440,440,440,440,440,429,423),II 00012470
  423     IDO = NCODE(14)                                               00012480
          ZIP = CODE(14)                                                00012490
          ZAP = EDGE                                                    00012500
              GO TO 439                                                 00012510
  424     IDO = NCODE(5)                                                00012520
          ZIP = CODE(5)                                                 00012530
              GO TO 428                                                 00012540
  425     IDO = NCODE(11)                                               00012550
          ZIP = CODE(11)                                                00012560
              GO TO 428                                                 00012570
  426     IDO = NCODE(6)                                                00012580
          ZIP = CODE(6)                                                 00012590
              GO TO 428                                                 00012600
  427     IDO = NCODE(12)                                               00012610
          ZIP = CODE(12)                                                00012620
  428     ZAP = 1.25                                                    00012630
              GO TO 439                                                 00012640
  429     IDO = NCODE(13)                                               00012650
          ZIP = CODE(13)                                                00012660
          ZAP = THREE                                                   00012670
  439     ZIP = ZIP / IDO                                               00012680
          CALL T3H22D (II,JJ,I,J,K,L,IDO,ZAP,ZIP)                       00012690
  440 CONTINUE                                                          00012700
C                                                                       00012710
CCC  @ % * < - / Q W E R T Y U I O P A S D F G H J K L Z X C V B N M , .00012720
CCC  # , $ . - 0 + _ ) [ \ | 1 2 3 &   > : ; ^ ' 4 5 6   ? " = ! ( 7 8 900012730
CCC  @ % * < - / Q W E R T Y U I O P A S D F G H J K L Z X C V B N M , .00012740
C                                                                       00012750

C  RJD 8/4/92 Do not loop back and read more data.
C     GO TO 5                                                           00012760
  999 CLOSE(NRDR,STATUS='KEEP')
      CLOSE(NWTR,STATUS='KEEP')
      CLOSE(7,STATUS='KEEP')
      CLOSE(8,STATUS='KEEP')
      CALL PageControl
      STOP                                                              00012770
      END                                                               00012780
