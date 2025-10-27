C          DATA SET T3H21D     AT LEVEL 002 AS OF 03/30/81              00000000
C          DATA SET T3H21D     AT LEVEL 001 AS OF 08/31/76              00000010
      SUBROUTINE T3H21D (ID,I,J,K,M)                                    00000020
      COMMON IRUN, IDATEX(3)                                             00000030
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000040
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000050
     *,INTS                                                             00000060
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000070
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000080
      COMMON/INOUT/NRDR,NWTR                                            00000090
      DIMENSION LABEL(7), NUMER(8), NFRMT(11)                           00000100
      DATA LABEL,NUMER,NFRMT/'    ',' R/L','   3','   4','   5','   6', 00000110
     A'   7',  '"   ','1/8"','1/4"','3/8"','1/2"','5/8"','3/4"','7/8"', 00000120
     B '(''0''',',20X',',I2,','4X,''','X'',I','    ',',   ','    ',     00000130
     C 'X,''M','5'', ','I1) '/                                          00000140
C                                                                       00000150
      WRITE(NWTR,99)JOBNO,IDRAW,MARK,LABEL(MARK2)                       00000160
   99 FORMAT(1X,'A   ',A4,A4,I4,A4,59X)                                 00000170
      WRITE(NWTR,101)JOBNO,IDRAW,MARK,LABEL(MARK2),IDATE                00000180
  101 FORMAT('1'///9X,'X',29X,'HUDSON  PRODUCTS  CORPORATION',27X,'X'// 00000190
     *43X,'CLIP - MASTER  PROGRAM'//20X,'JOB & DRAWING : ',A4,' - ',A4, 00000200
     *' -',I3,A4//20X,'DATE :  ',2(I2,'/'),I2//)                        00000210
C                                                                       00000220
      DO 30 I = 1,ID                                                    00000230
      K1 = Y(I)                                                         00000240
      Y(I) = Y(I) - K1                                                  00000250
      K2 = Y(I) / .125 + 1.5                                            00000260
      WRITE (NWTR,105) I,K1,NUMER(K2)                                   00000270
  105 FORMAT(' ',19X,'GAUGE FOR M5',I1,' PUNCH =',I3,1X,A4)             00000280
   30 CONTINUE                                                          00000290
C                                                                       00000300
      WRITE (NWTR,106)                                                  00000310
  106 FORMAT('0',19X,10('7777'))                                        00000320
      KOUNT = 1                                                         00000330
      DO 100 I = 1,M                                                    00000340
      DO 90 J = 1,ID                                                    00000350
      K = J                                                             00000360
   40 K1 = X(KOUNT) * 1000. + .5                                        00000370
      DO 50 K2 = 3,7                                                    00000380
      IF (K1 .LT. (10**K2))  GO TO 60                                   00000390
   50 CONTINUE                                                          00000400
   60 NFRMT(6) = LABEL(K2)                                              00000410
      NFRMT(8) = LABEL(10-K2)                                           00000420
      WRITE (NWTR,NFRMT) KOUNT,K1,K                                     00000430
      WRITE(NWTR,107) KOUNT,K1,K                                        00000440
  107 FORMAT(1X,I2,'X',I6,'M5',I1,67X)                                  00000450
      KOUNT = KOUNT + 1                                                 00000460
C                                                                       00000470
      IF (I .EQ. M .AND. K .EQ. ID)  GO TO 70                           00000480
      GO TO 90                                                          00000490
C                                                                       00000500
   70 K = 3                                                             00000510
      GO TO 40                                                          00000520
C                                                                       00000530
   90 CONTINUE                                                          00000540
  100 CONTINUE                                                          00000550
      RETURN                                                            00000560
      END                                                               00000570
