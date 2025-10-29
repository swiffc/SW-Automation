C          DATA SET T3H26D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H26D(I,J,K,II,JJ,IGO,INDEX,ZAP,SBI,DIHED,IVE,NSYM,   00000010
     * SBE,BVE,IVS,SBS,BVS,M,N)                                         00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      DIMENSION LABEL(2)                                                00000100
      DATA LABEL,GROSS/'    ',' R/L',144.0/                             00000110
C                                                                       00000120
      WRITE (NWTR,101) IDATE                                            00000130
  101 FORMAT('1'/76X,'DATE: ',2(I2,'/'),I2//9X,'X',29X,'HUDSON  PRODUCTS00000140
     *  CORPORATION',27X,'X'/41X,'HOOD - PLATE BENDING CHART'///)       00000150
C                                                                       00000160
      IF (XWIDE(3) .GT. 6.25)  GO TO 70                                 00000170
C                                                                       00000180
      IGO = 0                                                           00000190
      MARK = 3                                                          00000200
      MARK2 = 1                                                         00000210
C                                                                       00000220
   10 IF (NUMBER(MARK) .EQ. 0)  GO TO 30                                00000230
C                                                                       00000240
      WRITE (NWTR,102) JOBNO,IDRAW,MARK,LABEL(MARK2)                    00000250
  102 FORMAT(' ',43X,A4,' - ',A4,' -',I3,A4)                            00000260
C                                                                       00000270
   30 IGO = IGO + 1                                                     00000280
      GO TO (40,110,120,130,140,220),  IGO                              00000290
C                                                                       00000300
   40 ZAP = ABS(DIHED)                                                  00000310
      X(1) = -SBI + 3.125                                               00000320
      IF (DIHED) 50,230,60                                              00000330
C                                                                       00000340
   50 CALL T3H24D (1,I,J,K,ZAP)                                         00000350
      GO TO 70                                                          00000360
C                                                                       00000370
   60 CALL T3H24D (2,I,J,K,ZAP)                                         00000380
C                                                                       00000390
   70 IGO = 1                                                           00000400
      MARK = IGO                                                        00000410
      GO TO (80,100),  IVE                                              00000420
   80 GO TO (90,100),  NSYM                                             00000430
C                                                                       00000440
   90 MARK2 = 1                                                         00000450
      GO TO 10                                                          00000460
C                                                                       00000470
  100 MARK2 =2                                                          00000480
      GO TO 10                                                          00000490
C                                                                       00000500
  110 MARK = 14                                                         00000510
      GO TO 10                                                          00000520
C                                                                       00000530
  120 MARK = 11                                                         00000540
      GO TO 10                                                          00000550
C                                                                       00000560
  130 MARK = 15                                                         00000570
      GO TO 10                                                          00000580
C                                                                       00000590
  140 WRITE (NWTR,106)                                                  00000600
  106 FORMAT(' ',43X,'(END PANEL VIEW)')                                00000610
C                                                                       00000620
      X(1) = 2.875                                                      00000630
      X(2) = -SBE + 2.75                                                00000640
      ZAP = BVE                                                         00000650
C                                                                       00000660
  150 IF (ZAP) 160,190,180                                              00000670
C                                                                       00000680
  160 IF (ZAP .GE. -12.)  GO TO 170                                     00000690
      ZAP = GROSS/ZAP                                                   00000700
      INDEX = 1                                                         00000710
      GO TO 200                                                         00000720
C                                                                       00000730
  170 INDEX = 2                                                         00000740
      GO TO 200                                                         00000750
C                                                                       00000760
  180 INDEX = 3                                                         00000770
      GO TO 200                                                         00000780
C                                                                       00000790
  190 INDEX = 4                                                         00000800
C                                                                       00000810
  200 CALL T3H25D (INDEX,I,J,K,II,JJ,M,N,ZAP)                           00000820
C                                                                       00000830
      IF (MARK .EQ. 2)  GO TO 230                                       00000840
C                                                                       00000850
  210 MARK = 2                                                          00000860
      MARK2 = IVS                                                       00000870
      GO TO 10                                                          00000880
C                                                                       00000890
  220 WRITE (NWTR,108)                                                  00000900
  108 FORMAT(' ',43X,'(SIDE PANEL VIEW)')                               00000910
C                                                                       00000920
      X(2) = -SBS + 2.75                                                00000930
      ZAP = BVS                                                         00000940
      GO TO 150                                                         00000950
C                                                                       00000960
  230 RETURN                                                            00000970
      END                                                               00000980
