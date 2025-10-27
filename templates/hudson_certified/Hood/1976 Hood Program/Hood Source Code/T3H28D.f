C          DATA SET T3H28D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H28D(ZAP,ZIP,ZOP)                                    00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 00000090
C          THIS SUBROUTINE USES TWO RECTANGULAR COORDINATES (ZIP & ZOP) 00000100
C          THE POLAR COORDINATE (ZAP).                                  00000110
C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 00000120
      ZAP = SQRT (ZIP**2 + ZOP**2)                                      00000130
      RETURN                                                            00000140
      END                                                               00000150
