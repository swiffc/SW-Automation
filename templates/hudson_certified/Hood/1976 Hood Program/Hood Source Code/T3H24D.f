C          DATA SET T3H24D     AT LEVEL 001 AS OF 08/31/76              00000000
      SUBROUTINE T3H24D (II,I,J,K,ZAP)                                  00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DIMENSION FORMT(4), BETA(8), ALPHA(19)                            00000090
      DATA FORMT/'('' ''','    ',',''.''',')   '/                       00000100
      DATA BETA/',73X',',72X',',71X',',70X',',65X',',66X',',67X',',68X'/00000110
      DATA ALPHA/'"  ','1/1','1/8','3/1','1/4','5/1','3/8','7/1','1/2', 00000120
     *'9/1','5/8','11/','3/4','13/','7/8','15/','   ','6" ','16"'/      00000130
C                                                                       00000140
C                                                                       00000150
      WRITE (NWTR,101)                                                  00000160
  101 FORMAT(' ',43X,'(CORNER SPLICE)')                                 00000170
      CALL T3H23D (2,ZAP,J,K)                                           00000180
      DO 10 I = 1,4                                                     00000190
      FORMT(2) = BETA((II-1)*4+I)                                       00000200
      WRITE (NWTR,FORMT)                                                00000210
      IF (I .EQ. 1)  WRITE (NWTR,102) K2, ALPHA(J), ALPHA(K)            00000220
  102 FORMAT('+',43X,I2,1X,2A3,' OVER 12 "')                            00000230
   10 CONTINUE                                                          00000240
C                                                                       00000250
      ZAP = X(1)                                                        00000260
      CALL T3H23D (2,ZAP,I,J)                                           00000270
      WRITE(NWTR,103)K2, ALPHA(I), ALPHA(J)                             00000280
  103 FORMAT('+',59X,I2,1X,2A3/62X,5('. ')///)                          00000290
      RETURN                                                            00000300
      END                                                               00000310
