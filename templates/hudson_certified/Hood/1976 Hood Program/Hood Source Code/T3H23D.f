C          DATA SET T3H23D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H23D(IGO,ZAP,K3,K4)                                  00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DATA T32,T16,FOOT/.03125,.0625,12.0/                              00000090
C  THIS SUBROUTINE CONVERTS REAL NUMBER(ZAP) -TO-                       00000100
C                                                 FEET(K1),             00000110
C                                                 INCHES(K2),           00000120
C                                                 FRACTIONS OF INCH(K3 &00000130
      GO TO (1,2),  IGO                                                 00000140
    1 K1 = (ZAP + T32) / FOOT                                           00000150
      ZAP = ZAP - (FOOT * K1)                                           00000160
    2 K2 = ZAP + T32                                                    00000170
      ZAP = ZAP - K2                                                    00000180
      K3 = ZAP / T16 + 1.5                                              00000190
      GO TO (10,20,30,20,30,20,30,20,30,20,30,40,30,40,30,40), K3       00000200
   10 K4 = 17                                                           00000210
      GO TO 50                                                          00000220
   20 K4 = 18                                                           00000230
      GO TO 50                                                          00000240
   30 K4 = 1                                                            00000250
      GO TO 50                                                          00000260
   40 K4 = 19                                                           00000270
   50 RETURN                                                            00000280
      END                                                               00000290
