C          DATA SET T3H01D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H01D                                                 00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      LINE = LINE + 1                                                   00000090
      IF (LINE .LT. 29)  GO TO 100                                      00000100
      IF(IRUN.EQ.1)GO TO 20                                             00000110
      WRITE (NWTR,10) JOBNO,IDRAW,MARK                                  00000120
   10 FORMAT('1'///9X,'X',23X,A4,' - ',A4,' -',I3,46X,'X')              00000130
   20 CONTINUE                                                          00000140
      LINE = 1                                                          00000150
      IF (K1 .EQ. 0)  RETURN                                            00000160
  100 K1 = K1 + 1                                                       00000170
      KOUNT = KOUNT + NUMBER(MARK)                                      00000180
C   "KOUNT" IS TEMPORARY HOLDING AREA FOR BOLT COUNT.                   00000190
      IF(IRUN.EQ.1)GO TO 30                                             00000200
      WRITE (NWTR,105)  K1,X(K1),Y(K1)                                  00000210
  105 FORMAT ('0',23X,I3,4X,'X =',F9.4,4X,'Y =',F9.4)                   00000220
   30 CONTINUE                                                          00000230
      RETURN                                                            00000240
      END                                                               00000250
