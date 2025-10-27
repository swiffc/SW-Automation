C          DATA SET T3H20D     AT LEVEL 002 AS OF 12/23/76              00000000
      SUBROUTINE T3H20D(FANSZ,I,IVE,BVE,SBE,SLE,A,E30,I3E,E40,I4E,IVS,  00000010
     *BVS,SBS,SLS,B,S30,I3S,S40,I4S,ZIP,ED20,IBOLT,M1,M2,M3)            00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      DIMENSION BOLTWT(12,3)                                            00000100
      INTEGER FANSZ                                                     00000110
      DATA BOLTWT/6.,7.,7.,8.,8.,9.,9.,10.,10.,11.,12.,12.,      114.9, 00000120
     A135.8,157.6,177.4,200.2,222.6,244.1,265.9,288.2,310.4,332.2,354., 00000130
     B18.9,25.1,32.1,39.9,48.5,57.9,68.2,79.3,91.2,103.9,117.5,131.8/   00000140
      DATA FOOT,GROSS,FACTO/12.,144.,-7.65/                             00000150
C                                                                       00000160
      DO 421 I = 1,21                                                   00000170
      GO TO(400,400,401,403,408,409,412,413,413,413,400,414,418,400,400,00000180
     * 413,413,413,413,413,413),  I                                     00000190
  400 ZIP = 7.65                                                        00000200
      GO TO 420                                                         00000210
C                                                                       00000220
  401 NUMBER(13) = (NUMBER(11) + NUMBER(15)) * 2 / IVE                  00000230
      NUMBER(I) = NUMBER(2) * 2 / IVS - NUMBER(13)                      00000240
      IF (XWIDE(I) .LT. FOOT)  GO TO 400                                00000250
  402 ZIP = 4.9                                                         00000260
      GO TO 419                                                         00000270
C                                                                       00000280
  403 GO TO (404,405),  IVE                                             00000290
  404 NUMBER(I) = NUMBER(1) + NUMBER(11)                                00000300
      XLONG(I) = E30 * I3E + 2.5                                        00000310
      IF (E30 .EQ. S30 .AND. I3E .EQ. I3S .AND. IVS .EQ. 1)  GO TO 406  00000320
  405 IF (IVS .EQ. 1)  NUMBER(I+1) = NUMBER(2)                          00000330
      GO TO 407                                                         00000340
  406 NUMBER(I) = NUMBER(I) + NUMBER(2)                                 00000350
  407 ZIP = 3.19                                                        00000360
      GO TO 419                                                         00000370
C                                                                       00000380
  408 XLONG(I) = S30 * I3S + 2.5                                        00000390
      GO TO 407                                                         00000400
C                                                                       00000410
  409 NUMBER(I) = NUMBER(3)                                             00000420
      XLONG(I) = E40 * I4E + 2.5                                        00000430
      IF (E40 .EQ. S40 .AND. I4E .EQ. I4S)  GO TO 410                   00000440
      NUMBER(I+1) = NUMBER(2) * 2 / IVS                                 00000450
      GO TO 411                                                         00000460
  410 NUMBER(I) = NUMBER(I) + NUMBER(2) * 2 / IVS                       00000470
  411 ZIP = 4.1                                                         00000480
      GO TO 419                                                         00000490
C                                                                       00000500
  412 XLONG(I) = S40 * I4S + 2.5                                        00000510
      GO TO 411                                                         00000520
C                                                                       00000530
  413 ZIP = 5.3                                                         00000540
      GO TO 420                                                         00000550
C                                                                       00000560
  414 NUMBER(I) = NUMBER(11) * IVE                                      00000570
      GO TO (415,417),  IVE                                             00000580
  415 XLONG(I) = F * 2.                                                 00000590
      GO TO 402                                                         00000600
  417 XLONG(I) = F - .25                                                00000610
      GO TO 402                                                         00000620
C                                                                       00000630
  418 XLONG(I) = XLONG(3)                                               00000640
      GO TO 402                                                         00000650
  419 XWIDE(I) = FOOT                                                   00000660
  420 IF (NUMBER(I) .EQ. 0)  GO TO 421                                  00000670
      LBS(I) = XLONG(I)/FOOT * XWIDE(I)/FOOT * ZIP * NUMBER(I) + .5     00000680
  421 CONTINUE                                                          00000690
C                                                                       00000700
      K1 = FANSZ - 4                                                    00000710
      IF (K1 .GT. 12) K1 = 12                                           00000720
      M1 = LUNITS * NOFAN * 4                                           00000730
      IBOLT = (BOLTWT(K1,1) * M1 + .5) + IBOLT                          00000740
      M2 = BOLTWT(K1,2) * M1 + .5                                       00000750
      M3 = BOLTWT(K1,3) * M1 + .5                                       00000760
C                                                                       00000770
COMPUTE THE SCRAP LOSS (IN LBS.) FOR ALL OF THE END & COMMON PANELS.    00000780
C  FIRST STEP IS TO COMPUTE SQ. INCHES OF SCRAP LOST IN THE "CORNERS".  00000790
      ZIP = 8.265625                                                    00000800
      IF (BVS)1,5,3                                                     00000810
    1 ZIP = ZIP + B * 5.75                                              00000820
      GO TO 4                                                           00000830
    3 ZIP = ZIP + ((B * 2.) + (-SBE + 2.75) * B / SLE) * (-SBE + 2.75)  00000840
    4 ZIP = ZIP + B * SLE                                               00000850
    5 GO TO (7,6),  IVE                                                 00000860
C  SECOND STEP IS TO COMPUTE THE SCRAP LOST TO "BLOCK-OUTS" ON SPLIT" PA00000870
    6 ZIP = ZIP / 2. + 8.0859375                                        00000880
      IF (BVE .NE. 0.0)  ZIP = ZIP + 3.955078125                        00000890
      ZIP = ZIP + (-SBE + 2.75) * 2.8125                                00000900
C  THIRD STEP IS TO CONVERT SQ. IN. TO SQ. FT. AND COMPUTE TOTAL SCRAP W00000910
C  FOR ALL PANELS CONCERNED.                                            00000920
    7 Y(51)=ZIP/GROSS * FACTO * (NUMBER(1) + NUMBER(11) + NUMBER(14) +  00000930
     *    NUMBER(15)) - .5                                              00000940
C  END OF SCRAP LOSS COMPUTATION FOR END & COMMON PANELS.               00000950
COMPUTE THE SCRAP LOSS (IN LBS.) FOR THE SIDE PANELS.                   00000960
      ZIP = 8.265625                                                    00000970
      IF (BVE) 11,12,13                                                 00000980
   11 ZIP = ZIP + A * 5.75                                              00000990
      GO TO 14                                                          00001000
   12 ZIP = ZIP + 1.5                                                   00001010
      GO TO 15                                                          00001020
   13 ZIP = ZIP + ((A * 2.) + (-SBS + 2.75) * A / SLS) * (-SBS + 2.75)  00001030
   14 ZIP = ZIP + A * SLS                                               00001040
   15 GOTO (17,16),  IVS                                                00001050
   16 ZIP = ZIP / 2. + 8.0859375                                        00001060
      IF (BVS .NE. 0.0)  ZIP = ZIP + 3.955078125                        00001070
      ZIP = ZIP + (-SBS + 2.75) * 2.8125                                00001080
   17 Y(52)=ZIP/GROSS * FACTO * NUMBER(2) - .5                          00001090
COMPUTE THE SCRAP LOSS FOR THE CORNER SPLICE ANGLES.                    00001100
      IF(ED20 .NE. 3.)  GO TO 29                                        00001110
      IF (XWIDE(3) .EQ. FOOT)  GO TO 20                                 00001120
      ZIP = FACTO                                                       00001130
      GO TO 21                                                          00001140
   20 ZIP = -10.2                                                       00001150
      IF (NUMBER(13) .NE. 0 .AND. BVS .LT. 0.0)  ZIP = ZIP * 2.0        00001160
   21 Y(53)= ZIP * 6.25 / GROSS * (NUMBER(3) + NUMBER(13)) - .5         00001170
      GO TO 30                                                          00001180
   29 Y(53)= 0.0                                                        00001190
   30 ZIP = -63.75                                                      00001200
      Y(54)= ZIP / GROSS * ((-IVE + 3) * NUMBER(12)) - .5               00001210
      RETURN                                                            00001220
      END                                                               00001230
