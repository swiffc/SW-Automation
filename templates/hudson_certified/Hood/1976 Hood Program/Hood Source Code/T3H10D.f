C          DATA SET T3H10D     AT LEVEL 005 AS OF 03/25/81              00000000
C          DATA SET T3H10D     AT LEVEL 004 AS OF 03/18/81              00000010
C          DATA SET T3H10D     AT LEVEL 003 AS OF 02/20/81              00000020
C          DATA SET T3H10D     AT LEVEL 002 AS OF 02/20/81              00000030
C          DATA SET T3H10D     AT LEVEL 001 AS OF 08/26/76              00000040
      SUBROUTINE T3H10D                                                 00000050
      COMMON IRUN, IDATEX(3)                                             00000060
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000070
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000080
     *,INTS                                                             00000090
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000100
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000110
      COMMON/INOUT/NRDR,NWTR                                            00000120
      DIMENSION LABEL(7)                                                00000130
      DATA LABEL/'    ',' R/L',' SEC','T #1','T #2','T #3',' ALL'/      00000140
C                                                                       00000150
C     NWDISK=7                                                          00000160
      NWDISK=NWTR                                                       00000160
      IF (NPAGE .EQ. 1)WRITE(NWDISK,150)JOBNO,IDRAW,MARK,MCLMP1,MCLMP2  00000170
  150 FORMAT(1X,'N   ',A4,A4,I4,I4,I4,55X)                              00000180
      WRITE(NWTR,100)JOBNO,IDRAW,MARK,LABEL(MARK2),LABEL(MARK3),NPAGE,  00000190
     *NPAGES,IDATE,INTS                                                 00000200
  100 FORMAT('1'///9X,'X',29X,'HUDSON  PRODUCTS  CORPORATION',27X,'X'// 00000210
     1 43X,'PANEL  MASTER  PROGRAM'//                                   00000220
     2 16X,'JOB & DRAWING :  ',A4,' - ',A4,' -',I3,2A4,                 00000230
     3           T75,'SHEET :',I2,' OF',I2//16X,'DATE :',3X,2(I2,'/'),  00000240
     4 I2,41X,'BY : ',A3)                                               00000250
C                                                                       00000260
      IF (NPAGE .GT. 1) GO TO 110                                       00000270
C                                                                       00000280
      WRITE (NWTR,106) MCLMP1,MCLMP2                                    00000290
  106 FORMAT('0',15X,'MATERIAL CLAMPS POSITIONS :',I5,                  00000300
     2                 '"   AND',I5,'"'/'0',15X,'AUXILIARY  FUNCTIONS (M00000310
     3 CODE):'/'0',15X,'* M00  PROGRAM STOP',8X,'* M06  TOOL CHANGE',8X,00000320
     4'M71  REPOSITION CLAMPS'/' ',17X,'M73  RAPID TRAVERSE',32X,'M85  N00000330
     5O PUNCH'/' ',15X,'* M87  LOAD POSITION, CLEAR BLOCK COUNTER',12X, 00000340
     6'M75  CANCEL NO PUNCH'/' ',17X,'M76  INCREMENTAL - MUST BE IN SEPA00000350
     7RATE BLOCK',7X,'M77  ABSOLUTE'/' ',17X,'M99  MATERIAL CLAMP RELEAS00000360
     8E - MUST BE IN SEPARATE BLOCK'/'0',15X,'* THESE COMMANDS ALSO STOP00000370
     9 PROGRAM AFTER OTHER COMMANDS IN BLOCK COMPLETE')                 00000380
      LINE = 7                                                          00000390
      GO TO 115                                                         00000400
C                                                                       00000410
  110 LINE = 1                                                          00000420
  115 WRITE (NWTR,120)                                                  00000430
  120 FORMAT('0',15X,'BLOCK    X'  ,8X,'Y      M     M     M'/' ',16X,  00000440
     *  'NO.    DIM.     DIM.   CODE  CODE  CODE',18X,'REMARKS')        00000450
      NPAGE = NPAGE + 1                                                 00000460
      RETURN                                                            00000470
      END                                                               00000480
