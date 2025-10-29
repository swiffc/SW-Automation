C          DATA SET T3H30D     AT LEVEL 014 AS OF 12/07/77              00000000
      SUBROUTINE T3H30D(FANSZ,A,B,E,H,I,J,K,L,M,N,II,W,K3,K4,M1,M2,M3,  00000010
     1IVE,BVE,SBE,SLE,E30,I3E,E40,I4E,IVS,BVS,SBS,SLS,S30,I3S,S40,I4S,  00000020
     2DIHED,SLI,ED20,S20,ID2,S10,ID1,ZIP,SBI,NSYM,IBOLT,ITYPE,LTUBE,    00000030
     3MODAL,MODEL,NCOPY,S50,I5S,G5,ITEMS)                               00000040
      COMMON IRUN, IDATEX(3)                                             00000050
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000060
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000070
     *,INTS                                                             00000080
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000090
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000100
      COMMON/INOUT/NRDR,NWTR                                            00000110
      INTEGER FANSZ                                                     00000120
      DIMENSION MAT(2), ALPHA(19), A1(2,4), BETA(3,19), N1(21), N2(21), 00000130
     * N3(21), N4(21), N5(4), GAMMA(4,4)                                00000140
     * ,ITEMS(2)                                                        00000150
      DATA MAT/'    ',' R/L'/                                           00000160
      DATA ALPHA/'"   ',' 1/1',' 1/8',' 3/1',' 1/4',' 5/1',' 3/8',' 7/1'00000170
     *,' 1/2',' 9/1',' 5/8',' 11/',' 3/4',' 13/',' 7/8',' 15/','    ',  00000180
     *'6"  ','16" '/                                                    00000190
      DATA A1/'ONE ','END ','TWO ','ENDS',' THE',' TOP','  BO','TTOM'/  00000200
      DATA N1/1,1,1,2,2,3,3,4,4,4,1,5,5,1,1,4,4,4,4,4,4/                00000210
      DATA N2/6,7,8,9,9,10,10,11,11,11,12,13,8,6,12,11,11,11,11,11,11/  00000220
      DATA N3/16,16,19,6,7,6,7,15,15,15,16,12,14,16,16,15,15,15,15,15,  00000230
     *15/                                                               00000240
      DATA N4/14,3,13,5,6,7,12,9,10,16,15,8,4,11,2,17,18,19,20,21,1/    00000250
      DATA N5/2,1,1,2/                                                  00000260
      DATA BETA/' 3/1','6 (B','ENT)',                                   00000270
     A          'L 2 ',' X  ','2  X',                                   00000280
     B          'L 3 ',' X  ','2  X',                                   00000290
     C          ' 10 ','GA(G','ALV)',                                   00000300
     D          'L 3 ',' X  ','3  X',                                   00000310
     E          ' END',' PAN','ELS ',                                   00000320
     F          'SIDE',' PAN','ELS ',                                   00000330
     G          'SPLI','CE A','NG. ',                                   00000340
     H          'VERT','. ST','IFF.',                                   00000350
     I          'DIAG','. ST','IFF.',                                   00000360
     J          '  AI','R SE','ALS ',                                   00000370
     K          'COMN',' PAN','ELS ',                                   00000380
     L          'COMN','. ST','IFF.',                                   00000390
     M          ' ALL',' PAN','ELS ',                                   00000400
     N          'TUBE',' BUN','DLES',                                   00000410
     O          'ARE ','VERT','ICAL',                                   00000420
     P          'SLOP','E IN',':   ',                                   00000430
     Q          'SLOP','E OU','T:  ',                                   00000440
     R          'BEND',' WAS',' NIL'/                                   00000450
      DATA GAMMA/'N/C ','PROG','RAMM','ING ',                           00000460
     A           'MAT''','L PU','RCHA','SING',                          00000470
     B           'MAT''','L TA','KE-O','FF  ',                          00000480
     C           'DRAF','TING',' ORI','GIN '/                           00000490
      DATA GROSS/144./                                                  00000500
      DO 300 II = 1,NCOPY                                               00000510
      L = M2 + M3                                                       00000520
      NPAGE = 1                                                         00000530
      NPAGES = N5(II)                                                   00000540
  203 WRITE(NWTR,101) (GAMMA(I,II),I=1,4),NPAGE,NPAGES,IDATE            00000550
     * ,ITEMS                                                           00000560
  101 FORMAT('1',19X,'TO:  ',4A4,                                       00000570
     *           34X,'PAGE:',I3,' OF',I2/75X,'DATE: ',2(I2,'/'),I2      00000580
     *           / 75X,'ITEM: ',2A4                      / 9X,'X',29X,  00000590
     *'HUDSON  PRODUCTS  CORPORATION',27X,'X'/42X,                      00000600
     *'MATERIAL SUMMARY SHEET')                                         00000610
      I = H                                                             00000620
      WRITE(NWTR,102)JOBNO,IDRAW, MODEL, LTUBE, MODAL, NOFAN, ITYPE,    00000630
     * LUNITS, FANSZ, I                                                 00000640
  102 FORMAT('0',T21,'JOB #    DRAWING     MODEL     TYPE    UNITS   FAN00000650
     * DIA.   HOOD HT.'/20X,A4,7X, A4,4X,A4,'-',I2,A2,3X,I1,A4,I6,I9,   00000660
     *'''',I10,'"'/)                                                    00000670
      GO TO (204,205), NPAGE                                            00000680
  204 GO TO (202,205,205,202),  II                                      00000690
  202 ZIP = E                                                           00000700
      CALL T3H23D(1,ZIP,K3,K4)                                          00000710
      WRITE(NWTR,103)K1,K2,ALPHA(K3), ALPHA(K4)                         00000720
  103 FORMAT(' ',T36,'CL UNIT-TO-''D''ANGLE GA. =',I5,'''-',I2,2A4)     00000730
      ZIP = W                                                           00000740
      CALL T3H23D(1,ZIP,K3,K4)                                          00000750
      WRITE(NWTR,104)K1,K2,ALPHA(K3), ALPHA(K4), A, B, SLE, SLS, SLI    00000760
  104 FORMAT(' ',T36,'THE TOTAL WIDTH OF UNIT =',I5,'''-',I2,2A4//      00000770
     *                                23X,'A =',F11.4,'"',8X,'B =',F11.400000780
     *,'"'/23X,'SLE =',F9.4,'"',8X,'SLS =',F9.4,'"',8X,'SLI =',F9.4,    00000790
     *'"')                                                              00000800
      WRITE(NWTR,105)SBE,SBS, SBI                                       00000810
  105 FORMAT(' ',T24,'SBE =',F9.4,'"',8X,'SBS =',F9.4,'"',8X,'SBI =',   00000820
     * F9.4,'"')                                                        00000830
  205 WRITE (NWTR,106)                                                  00000840
  106 FORMAT('0',T20,'MARK #   QTY',8X,'MATERIAL DESCRIPTIVES', 9X,     00000850
     * 'LENGTH',7X,'WEIGHT'/19X,'---- -   ---',8X,8('-'),' ',12('-'),   00000860
     * 9X,6('-'),7X,6('-')/)                                            00000870
      GO TO (206,283), NPAGE                                            00000880
  206 MARK = 21                                                         00000890
      GO TO (278,284), NSYM                                             00000900
  210 IF (NUMBER(MARK) .EQ. 0)  GO TO (300,300,300,276,276,300,276,286, 00000910
     *276,262,285,276,217,285,215,286,276,262,286,276,262),  MARK       00000920
      ZIP = XLONG(MARK)                                                 00000930
      CALL T3H23D(1,ZIP,K3,K4)                                          00000940
      WRITE (NWTR,107) MARK,MAT(MARK2),NUMBER(MARK),K1,K2,ALPHA(K3),    00000950
     *ALPHA(K4),LBS(MARK)                                               00000960
  107 FORMAT(' ',T19,'-',I2,A4,I6,32X,'X',I5,'''-',I2,2A4,I6,'#')       00000970
      L = L + LBS(MARK)                                                 00000980
      K = N1(MARK)                                                      00000990
C "N1" IS THE KEY TO THE MATERIAL TYPE FOR EACH PIECE MARK#.            00001000
      M = N2(MARK)                                                      00001010
C "N2" IS THE KEY TO THE NAME THIS PIECE MARK IS CALLED BY.             00001020
      N = N3(MARK)                                                      00001030
C "N3" IS THE KEY TO AN IDENTIFIABLE CHARACTERISTIC OR ASSOCIATION.     00001040
      GO TO (211,213,213,211,213),  K                                   00001050
  211 IF (MARK .EQ. 3 .AND. XWIDE(MARK) .EQ. 12.)  GO TO 212            00001060
      ZIP = XWIDE(MARK)                                                 00001070
      CALL T3H23D (2,ZIP,K3,K4)                                         00001080
      WRITE (NWTR,108)  K2,ALPHA(K3), ALPHA(K4), (BETA(J,K),J=1,3)      00001090
  108 FORMAT('+',T35,'PL',I3,2A4,'X ',3A4)                              00001100
      GO TO 214                                                         00001110
  212 K = 5                                                             00001120
  213 WRITE (NWTR,109)  (BETA(J,K),J=1,3)                               00001130
  109 FORMAT('+',T35,3A4,2X,'1/4')                                      00001140
  214 GO TO (285,216,276,259,262,260,262,290,290,262,285,218,217,285,   00001150
     *215,290,290,262,290,290,262),  MARK                               00001160
  215 MARK3 = 1                                                         00001170
      ZIP = BVE                                                         00001180
      GO TO 225                                                         00001190
  216 MARK3 = 2                                                         00001200
      ZIP = BVS                                                         00001210
      GO TO 225                                                         00001220
  217 MARK3 = 3                                                         00001230
      ZIP = DIHED                                                       00001240
      GO TO 225                                                         00001250
  218 MARK3 = 4                                                         00001260
  225 I = Y(MARK3+50)                                                   00001270
      IF (I .EQ. 0)  GO TO (228,291,291,228),  II                       00001280
      WRITE (NWTR,110)  I                                               00001290
  110 FORMAT(' ',T35,'TOTAL FAB. WT. LOST TO SCRAP =',I23,'#')          00001300
      L = L + I                                                         00001310
      GO TO (228,291,291,228),  II                                      00001320
  228 GO TO (236,236,229,230),  MARK3                                   00001330
  229 IF (ED20 .NE. 3.)  GO TO 234                                      00001340
      IF (NUMBER(13) .EQ. 0)  GO TO 231                                 00001350
  230 GOTO (231,232),  MARK2                                            00001360
  231 J = 1                                                             00001370
      GO TO 233                                                         00001380
  232 J = 2                                                             00001390
  233 WRITE (NWTR,111)  (A1(I,J),I=1,2)                                 00001400
  111 FORMAT(' ',T35,'NOTE: CLIP BOTH FLG. CORNERS'/T41,'ON ',2A4)      00001410
      IF (MARK3 .EQ. 4)  GO TO 262                                      00001420
  234 IF (K .EQ. 1)   GO TO 236                                         00001430
      IF (DIHED)  238,261,240                                           00001440
  236 IF (ZIP)  237,240,239                                             00001450
  237 N = 18                                                            00001460
  238 ZIP = -ZIP                                                        00001470
      GO TO 240                                                         00001480
  239 N = 17                                                            00001490
  240 WRITE (NWTR,112)  (BETA(I,M),I=1,3), (BETA(J,N),J=1,3)            00001500
  112 FORMAT(' ',T35,6A4)                                               00001510
      IF (N .EQ. 16)  GO TO 246                                         00001520
      IF (ZIP .GT. 12.)  GO TO 245                                      00001530
      CALL T3H23D (2,ZIP,K3,K4)                                         00001540
      WRITE (NWTR,113)  K2, ALPHA(K3), ALPHA(K4)                        00001550
  113 FORMAT(' ',T46,I2,2A4,'OVER 12"')                                 00001560
      GO TO 246                                                         00001570
  245 ZIP = GROSS / ZIP                                                 00001580
      CALL T3H23D (2,ZIP,K3,K4)                                         00001590
      WRITE (NWTR,114)  K2, ALPHA(K3), ALPHA(K4)                        00001600
  114 FORMAT(' ',T46,'12" OVER',I3,2A4)                                 00001610
  246 GO TO (247,249,264),  MARK3                                       00001620
  247 GO TO (275,248),  IVE                                             00001630
  248 IF (BVE)  251,254,252                                             00001640
  249 ZIP = S50 * I5S                                                   00001650
      CALL T3H23D(1,ZIP,K3,K4)                                          00001660
      WRITE (NWTR,125) K1,K2,ALPHA(K3),ALPHA(K4)                        00001670
  125 FORMAT(' ',T35,'THE TOTAL OF SPACES ''G'' ON THE'/                00001680
     *36X,'BOTTOM FLANGE =',I4,'''-',I2,2A4)                            00001690
      ZIP = G5                                                          00001700
      CALL T3H23D(2,ZIP,K3,K4)                                          00001710
      WRITE (NWTR,118) K2,ALPHA(K3),ALPHA(K4)                           00001720
      ZIP = S50                                                         00001730
      CALL T3H23D(2,ZIP,K3,K4)                                          00001740
      WRITE(NWTR,119) I5S,K2,ALPHA(K3),ALPHA(K4)                        00001750
      GO TO (275,250),  MARK2                                           00001760
  250 IF (BVS)  251,254,252                                             00001770
  251 J = 4                                                             00001780
      GO TO 253                                                         00001790
  252 J = 3                                                             00001800
  253 WRITE (NWTR,115)  (A1(I,J),I=1,2)                                 00001810
  115 FORMAT(' ',T35,'NOTE: CLIP',2A4,' CORNER OF')                     00001820
  254 WRITE (NWTR,116)                                                  00001830
  116 FORMAT(' ',T41,'THE VERTICAL FLANGE')                             00001840
      ZIP = 3.                                                          00001850
      GO TO 264                                                         00001860
  259 GO TO (260,262),  IVS                                             00001870
  260 IF (NUMBER(MARK+1) .GT. 0)  GO TO 262                             00001880
  261 N = 14                                                            00001890
  262 GO TO (287,291,291,287),  II                                      00001900
  287 WRITE (NWTR,117)  (BETA(I,M),I=1,3), (BETA(J,N),J=1,3)            00001910
  117 FORMAT(' ',T35,3A4,' FOR ',3A4)                                   00001920
      ZIP = 1.25                                                        00001930
  264 GO TO(266,267,265,266,267,268,269,275,275,275,266,270,265,266,266,00001940
     *275,275,275,275,275,275),  MARK                                   00001950
  265 ZIP = ED20                                                        00001960
      I = ID2                                                           00001970
      X(1) = S20                                                        00001980
      GO TO 271                                                         00001990
  266 I = I3E                                                           00002000
      X(1) = E30                                                        00002010
      GO TO 271                                                         00002020
  267 I = I3S                                                           00002030
      X(1) = S30                                                        00002040
      GO TO 271                                                         00002050
  268 I = I4E                                                           00002060
      X(1) = E40                                                        00002070
      GO TO 271                                                         00002080
  269 I = I4S                                                           00002090
      X(1) = S40                                                        00002100
      GO TO 271                                                         00002110
  270 ZIP = 4.0                                                         00002120
      I = ID1                                                           00002130
      X(1) = S10                                                        00002140
  271 CALL T3H23D (2,ZIP,K3,K4)                                         00002150
      WRITE(NWTR,118)K2,ALPHA(K3), ALPHA(K4)                            00002160
  118 FORMAT(' ',T35,'HOLES: FIRST HOLE AT',I3,2A4)                     00002170
      ZIP = X(1)                                                        00002180
      CALL T3H23D (2,ZIP,K3,K4)                                         00002190
      WRITE(NWTR,119)I,K2, ALPHA(K3), ALPHA(K4)                         00002200
  119 FORMAT(' ',T42,'THEN',I2,' SPA. @',I3,2A4)                        00002210
      IF (MARK .NE. 12)  GO TO 275                                      00002220
      J = 1                                                             00002230
      GO TO (272,273),  IVE                                             00002240
  272 K = 6                                                             00002250
      WRITE(NWTR,119)J,K, ALPHA(J)                                      00002260
      WRITE(NWTR,119)I,K2, ALPHA(K3), ALPHA(K4)                         00002270
  273 K = 3                                                             00002280
      WRITE(NWTR,118)K,ALPHA(J)                                         00002290
      WRITE(NWTR,119)I,K2, ALPHA(K3), ALPHA(K4)                         00002300
      GO TO (274,275),  IVE                                             00002310
      GO TO (274,275),  IVE                                             00002320
  274 K = 8                                                             00002330
      WRITE(NWTR,119)J,K, ALPHA(J)                                      00002340
      WRITE(NWTR,119)I,K2, ALPHA(K3), ALPHA(K4)                         00002350
  275 WRITE (NWTR,120)                                                  00002360
  120 FORMAT('0',T19,3('*-'),'*  *-*-   ',15('*-'),4X,4('*-'),6X,3('*-')00002370
     */)                                                                00002380
      GO TO 276                                                         00002390
  290 GO TO (276,291,291,276),  II                                      00002400
  291 WRITE (NWTR,126)                                                  00002410
  126 FORMAT(' ')                                                       00002420
  276 GO TO(285,283,280,283,281,283,278,283,283,283,285,283,283,285,279,00002430
     *283,283,283,283,283,286),  MARK                                   00002440
  278 GO TO (283,284),  IVE                                             00002450
  279 GO TO (283,284),  IVS                                             00002460
  280 IF (BVS)  283,284,284                                             00002470
  281 GO TO (283,282), NPAGES                                           00002480
  282 NPAGE = 2                                                         00002490
      GO TO 203                                                         00002500
  283 MARK2 = 1                                                         00002510
      GO TO 285                                                         00002520
  284 MARK2 = 2                                                         00002530
  285 MARK = N4(MARK)                                                   00002540
      GO TO 210                                                         00002550
C                                                                       00002560
C  ITEMIZED BREAK-DOWN OF STANDARD STRUCTURAL PARTS.                    00002570
  286 I = M1 * 12                                                       00002580
      M2 = M2 - I                                                       00002590
      WRITE (NWTR,127)  M1,I                                            00002600
  127 FORMAT(' ',T19,'-03RS1',I7,3X,'RING SPLICE PLATE',I36,'#')        00002610
      IF (II .EQ. 2 .OR. II .EQ. 3)  WRITE (NWTR,126)                   00002620
      WRITE (NWTR,121)  FANSZ,M1,M2                                     00002630
      M2 = M2 + I                                                       00002640
  121 FORMAT(' ',T19,'-03S',I2,I7,3X,'QUARTER FAN RING ASS''Y',I31,'#') 00002650
      IF (FANSZ .LT. 10)  WRITE (NWTR,122)                              00002660
  122 FORMAT('+',T23,'0')                                               00002670
      IF (II .EQ. 2 .OR. II .EQ. 3)  WRITE (NWTR,126)                   00002680
      WRITE(NWTR,123)FANSZ,M1, M3                                       00002690
  123 FORMAT(' ',T19,'-03T',I2,I7,3X,'TOP PANEL',I44,'#')               00002700
      IF (FANSZ .LT. 10)  WRITE (NWTR,122)                              00002710
      WRITE (NWTR,120)                                                  00002720
                   WRITE(NWTR,124)L,IBOLT                               00002730
  124 FORMAT('0',T57,'NET DRAWING WEIGHT =',I11,'#'//18X'DRAWING BOLT CO00002740
     *UNT =',I7)                                                        00002750
  300 CONTINUE                                                          00002760
      RETURN                                                            00002770
      END                                                               00002780
