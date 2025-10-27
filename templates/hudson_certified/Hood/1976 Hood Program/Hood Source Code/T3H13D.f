C          DATA SET T3H13D     AT LEVEL 003 AS OF 10/04/79              00000000
      SUBROUTINE T3H13D (K3,K4,X60,XX,YY,I,II,J,JJ,HOLD1,HOLD2)         00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
C   THE FIRST PART SELECTS & COLLECTS ALL COORDINATES BASED ON THE "X" B00000090
C        WITHIN THE RANGE OF THE PANEL-MASTER'S PRESENT POSITION ON THE 00000100
      K3 = K4 + 1                                                       00000110
      DO 100 I = K3,K1                                                  00000120
      IF (X(I) .GT. (X60+60.))  GO TO 100                               00000130
      K4 = K4 + 1                                                       00000140
      CALL T3H12D (XX,YY,I,K4)                                          00000150
  100 CONTINUE                                                          00000160
C   THE SECOND PART ARRANGES THE SET OF COORDINATES IN A SHORT, BUT EXPE00000170
C        ORDER.                                                         00000180
      DO 80 J = K3,K4                                                   00000190
      IF (J .EQ. 1)  GO TO 80                                           00000200
      DO 70 I = J,K4                                                    00000210
      XX = ABS(X(I) - X(J-1))                                           00000220
      YY = ABS(Y(I) - Y(J-1))                                           00000230
      IF (XX .GE. YY)  GO TO 10                                         00000240
      XX = YY - XX                                                      00000250
      YY = YY - XX                                                      00000260
      GO TO 20                                                          00000270
   10 XX = XX - YY                                                      00000280
   20 HOLD2 = SQRT(YY**2 *2) + XX                                       00000290
      IF (I .EQ. J)  GO TO 60                                           00000300
      IF ((X(J-1)-X60) .EQ. 0.0)  GO TO 40                              00000310
      IF ( ABS((X(I)-X60)/(X(J-1)-X60)*HOLD2) -                         00000320
     *     ABS((X(J)-X60)/(X(J-1)-X60)*HOLD1) )  50, 40, 70             00000330
   40 IF (X(I) .GT. X(J))  GO TO 70                                     00000340
   50 CALL T3H12D (XX,YY,I,J)                                           00000350
   60 HOLD1 = HOLD2                                                     00000360
   70 CONTINUE                                                          00000370
   80 CONTINUE                                                          00000380
      RETURN                                                            00000390
      END                                                               00000400
