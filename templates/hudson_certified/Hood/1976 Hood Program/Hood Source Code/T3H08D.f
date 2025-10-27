C          DATA SET T3H08D     AT LEVEL 003 AS OF 10/07/76              00000000
      SUBROUTINE T3H08D(M,A,D,G,I5,I,J,K,ZIP,G51)                       00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DIMENSION STD(6)                                                  00000090
      DATA STD/9.5,18.5,31.5,9.5,20.25,29.75/                           00000100
C 'M=1' INDICATES THE SIDE PANEL IS "WHOLE".                            00000110
C 'M=2' INDICATES THE SIDE PANEL IS "SPLIT".                            00000120
C 'M=3' INDICATES THE TUBE BUNDLE FRAME AIR SEALS.                      00000130
      GO TO (30,20,10),  M                                              00000140
   10 X(K1+1) = XLONG(MARK) - 4.5                                       00000150
      IF (IRUN .EQ. 0)   WRITE (NWTR,11)                                00000160
   11 FORMAT ('0',23X,'AIR SEAL TO FRAME ALIGNMENT COORDINATES :')      00000170
      GO TO 50                                                          00000180
   20 X(K1+1) = XLONG(MARK) - 7.125                                     00000190
      GO TO 40                                                          00000200
   30 X(K1+1) = XLONG(MARK) / 2. - 4.5                                  00000210
   40 IF (IRUN .EQ. 0)   WRITE (NWTR,41)                                00000220
   41 FORMAT ('0',23X,'SIDE PANEL TO FRAME ALIGNMENT COORDINATES :')    00000230
      I = NOFAN                                                         00000240
      IF (A .EQ. 0.0)  I = I + 3                                        00000250
      G = (D - STD(I)) / NOFAN                                          00000260
      G51 = X(K1+1) - G                                                 00000270
      CALL T3H00D(27.,G,I5,ZIP,I,0.0)                                   00000280
C                                                                       00000290
   50 CALL T3H02D(2,M,I,J,I5,K,-1.,0.,G)                                00000300
      RETURN                                                            00000310
      END                                                               00000320
