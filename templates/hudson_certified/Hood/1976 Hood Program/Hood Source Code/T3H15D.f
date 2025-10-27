C          DATA SET T3H15D     AT LEVEL 001 AS OF 08/26/76              00000000
      SUBROUTINE T3H15D (XX,X60,I,J,II,IGO,IX,IY,IXY,M1,M2,M3,K4)       00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      IF (X(K4) .GT. X60)  GO TO 100                                    00000090
C   THIS IS THE SET-UP FOR A "BACKWARD" RE-POSITION CYCLE.              00000100
      XX = X(K4) - X60                                                  00000110
      IX = 0                                                            00000120
      GO TO 120                                                         00000130
C   THIS IS THE SET-UP FOR A "FORWARD" RE-POSITION CYCLE.               00000140
  100 XX = XLONG(MARK) - 57. - X60                                      00000150
      IF (XX .GT. 60.0)  XX = 60.0                                      00000160
      X60 = X60 + XX                                                    00000170
      IF (XX .GT. 10.0) GO TO 110                                       00000180
      IX = 50000                                                        00000190
      GO TO 120                                                         00000200
  110 IX = 60000                                                        00000210
  120 IF (Y(K4) .LT. 12.0) GO TO 130                                    00000220
      IF (Y(K4) .GT. 28.0) GO TO 140                                    00000230
      IGO = 2                                                           00000240
      GO TO 160                                                         00000250
  130 IY = 12000                                                        00000260
      GO TO 150                                                         00000270
  140 IY = 28000                                                        00000280
  150 IGO = 4                                                           00000290
  160 M1 = 4                                                            00000300
  170 CALL T3H11D (I,J,II,IGO,IX,IY,IXY,M1,M2,M3,1)                     00000310
      IF (M1 .EQ. 4) GO TO 180                                          00000320
      GO TO 200                                                         00000330
  180 M1 = 5                                                            00000340
      IGO = 2                                                           00000350
      IX = IX - (XX*1000.)                                              00000360
      GO TO 170                                                         00000370
  200 RETURN                                                            00000380
      END                                                               00000390
