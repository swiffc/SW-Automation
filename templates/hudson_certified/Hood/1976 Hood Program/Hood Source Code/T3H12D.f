C          DATA SET T3H12D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H12D(XX,YY,I,K)                                      00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      XX = X(I)                                                         00000090
      YY = Y(I)                                                         00000100
      X(I) = X(K)                                                       00000110
      Y(I) = Y(K)                                                       00000120
      X(K) = XX                                                         00000130
      Y(K) = YY                                                         00000140
      RETURN                                                            00000150
      END                                                               00000160
