C          DATA SET T3H25D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H25D (INDEX,I,J,K,II,JJ,M,N,ZAP)                     00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DIMENSION CNTRL(4),FORM1(5),FORM2(5),FORM3(4),BETA(24),ALPHA(19)  00000090
      DATA CNTRL/'(''+''','('' ''','(''0''','///)'/                     00000100
      DATA FORM1/'    ','    ',',5(''','. '')','    '/                  00000110
      DATA FORM2/'    ','    ',',I2,','1X,2','A3) '/                    00000120
      DATA FORM3/'    ','    ',',''.''',')   '/                         00000130
      DATA BETA/',30X',',40X',',42X',',44X',',46X',',48X',',50X',',52X',00000140
     *',54X',',56X',',58X',',60X',',62X',',64X',',66X',',68X',',70X',   00000150
     *',71X',',72X',',73X',',74X',',75X',',76X',',77X'/                 00000160
      DATA ALPHA/'"  ','1/1','1/8','3/1','1/4','5/1','3/8','7/1','1/2', 00000170
     *'9/1','5/8','11/','3/4','13/','7/8','15/','   ','6" ','16"'/      00000180
C                                                                       00000190
C                                                                       00000200
      ZAP = ABS(ZAP)                                                    00000210
      CALL T3H23D (2,ZAP,M,N)                                           00000220
      GO TO  (1,2,2,5,5),  INDEX                                        00000230
    1 WRITE (NWTR,101)  K2, ALPHA(M), ALPHA(N)                          00000240
  101 FORMAT(' ',43X,'12 " OVER',I3,1X,2A3)                             00000250
      GO TO 5                                                           00000260
C                                                                       00000270
    2 WRITE (NWTR,102)  K2, ALPHA(M), ALPHA(N)                          00000280
  102 FORMAT(' ',43X,I2,1X,2A3,' OVER 12 "')                            00000290
C                                                                       00000300
    5 FORM1(1) = CNTRL(1)                                               00000310
      FORM1(2) = BETA(17)                                               00000320
      FORM1(5) = FORM3(4)                                               00000330
      WRITE (NWTR,FORM1)                                                00000340
C                                                                       00000350
      ZAP = X(1)                                                        00000360
      CALL T3H23D (2,ZAP,M,N)                                           00000370
      FORM2(1) = CNTRL(1)                                               00000380
      FORM2(2) = BETA(19)                                               00000390
C                                                                       00000400
      GO TO (10,10,30,40,40),  INDEX                                    00000410
C                                                                       00000420
   10 II = 16                                                           00000430
      JJ = -1                                                           00000440
      GO TO (20,50),  INDEX                                             00000450
   20 J = 7                                                             00000460
      FORM3(1) = CNTRL(2)                                               00000470
      GO TO 60                                                          00000480
C                                                                       00000490
   30 II = 18                                                           00000500
      JJ = 1                                                            00000510
      GO TO 50                                                          00000520
C                                                                       00000530
   40 II = 17                                                           00000540
      JJ = 0                                                            00000550
C                                                                       00000560
   50 J = 3                                                             00000570
      FORM3(1) = CNTRL(3)                                               00000580
C                                                                       00000590
   60 K = 1                                                             00000600
C                                                                       00000610
   70 DO 80 I = 1,J                                                     00000620
      FORM3(2) = BETA(II)                                               00000630
      WRITE (NWTR,FORM3)                                                00000640
      II = II + JJ                                                      00000650
      IF (I .EQ. 1)WRITE(NWTR,FORM2) K2, ALPHA(M), ALPHA(N)             00000660
   80 CONTINUE                                                          00000670
C                                                                       00000680
      GO TO (100,200),  K                                               00000690
C                                                                       00000700
  100 ZAP = H1                                                          00000710
      CALL T3H23D (2,ZAP,M,N)                                           00000720
      J = J + 1                                                         00000730
      K = K + 1                                                         00000740
C                                                                       00000750
      GO TO (110,110,120,120,120),  INDEX                               00000760
C                                                                       00000770
  110 FORM2(2) = BETA(16)                                               00000780
      GO TO 70                                                          00000790
C                                                                       00000800
  120 FORM2(2) = BETA(24)                                               00000810
      GO TO 70                                                          00000820
C                                                                       00000830
C                                                                       00000840
  200 ZAP = X(2)                                                        00000850
      CALL T3H23D (2,ZAP,M,N)                                           00000860
      FORM1(1) = CNTRL(2)                                               00000870
      FORM1(5) = CNTRL(4)                                               00000880
C                                                                       00000890
      GO TO (210,220,230,240,250),  INDEX                               00000900
C                                                                       00000910
  210 FORM1(2) = BETA(1)                                                00000920
      FORM2(1) = CNTRL(1)                                               00000930
      FORM2(2) = BETA(1)                                                00000940
      GO TO 270                                                         00000950
C                                                                       00000960
  220 FORM1(2) = BETA(5)                                                00000970
      FORM2(2) = BETA(5)                                                00000980
      GO TO 260                                                         00000990
C                                                                       00001000
  230 FORM1(2) = BETA(17)                                               00001010
      FORM2(2) = BETA(16)                                               00001020
      GO TO 260                                                         00001030
C                                                                       00001040
  240 FORM1(2) = BETA(13)                                               00001050
      FORM2(2) = BETA(12)                                               00001060
      GO TO 260                                                         00001070
C                                                                       00001080
  250 FORM1(2) = BETA(20)                                               00001090
      FORM2(2) = BETA(19)                                               00001100
C                                                                       00001110
  260 FORM2(1) = CNTRL(2)                                               00001120
C                                                                       00001130
  270 WRITE(NWTR,FORM2)K2,ALPHA(M), ALPHA(N)                            00001140
      WRITE (NWTR,FORM1)                                                00001150
      RETURN                                                            00001160
      END                                                               00001170
