C  Uses routines  DGESV, DGETRF and DGETRI from lapack.

C     removed PARM IS for scratch arrays in KF and calls but do more clean up
C  RR can be p*p, QQ can be n*n

C           Copyright 1993, 1994, 1995, 1996  Bank of Canada.
C           Copyright 1996, 1997  Paul Gilbert.
C           Copyright 1998, 2001, 2004  Bank of Canada.

C  Any person using this software has the right to use,
C        reproduce and distribute it.

C  The Bank does not warrant the accuracy of the information contained in the 
C  software. User assumes all liability for any loss or damages, either direct 
C  or indirect, arising from the use of the software.

C -------------------------------------------------------------------

C  A C code version of this code has also been distribute, but is not current.
C  It was generated using the following (extracted from the f2c readme).
C NOTE:	You may exercise f2c by sending netlib@netlib.bell-labs.com
C 	a message whose first line is "execute f2c" and whose remaining
C 	lines are the Fortran 77 source that you wish to have converted.
C 	Return mail brings you the resulting C, with f2c's error
C 	messages between #ifdef uNdEfInEd and #endif at the end.

C Comments below for compiling are fairly old. As of 2005 the code has been
C   compiled for a few years using standard package build procedure in R.

C Compile with: f77 -c -o dsefor.Sun4.o dsefor.f
C           or
C               f77 -c -o dsefor.Sun5.o dsefor.f
C           or
C             /opt/SUNWspro/f77 -c -o dsefor.S3.3.Sun5.o dsefor.f
C           or
C               f77.SC3.01.Sun4       -c -o dsefor.S3.3.Sun4.o dsefor.f
C     (Splus 3.3 requires compiler SC3.0.1 not SC3.0 as for Splus 3.1 and 3.2)

C           or  (with Solaris f77  (SunPro) )   for R

C               f77  -G -pic -o dse.so dsefor.f
C   or
C               f77  -G -pic -ansi -o dse.so dsefor.f
C   or
C        f77 -G -pic -ansi -L/home/asd3/opt/SUNWspro.old/lib -lF77 -lM77 -lm 
C               -lc -lucb -o dse.so dsefor.f


C   or
C               f77  -G -pic -fast -o dse.so dsefor.f   bombs
C   or
C               f77  -c -pic -o dsefor.o dsefor.f
C               ld -G -o dse.so dsefor.o

C                  -pic:  Generate pic code with short offset
C                  -c:    Produce '.o' file. Do not run ld.
C                  -P:    Generate optimized code
C                  -fast:  Bundled set of options for best performance
C                  -G:     Create shared object
C                  -O1:    Generate optimized code
C                  -O2:    Generate optimized code
C                  -O3:    Generate optimized code
C                  -O4:    Generate optimized code
C                  -O:     Generate optimized code
C                  Suffix 'so':    Shared object


C           or  (the following seems to be the preferred method for Splus)
C               splus COMPILE dsefor.f
C                 and then mv dsefor.o to dsefor.Sunx.o
C  
C   The pararameter IS=xxx controls the maximum size of the state in KF models.
C   A larger state makes S take more memory. 
C Compile with: f77 -c -o dsefor.Sun4.large.o dsefor.f
C           or
C               f77 -c -o dsefor.Sun5.large.o dsefor.f
C  
C   
C        1         2         3         4         5         6         712       8
C      SUBROUTINE ERROR(STR,L, IS,N)
CC  STR is a string of length L and IS is an integer vector of length N
C      INTEGER L,N
C      INTEGER IS(N)
C      CHARACTER STR(L)
CC      CALL INTPR(STR,L,IS,N)
CC      WRITE(STR,IS)
C      RETURN 
C      END

C      SUBROUTINE DBPR(STR,L, IS,N)
CC  STR is a string of length L and IS is an integer vector of length N
C      INTEGER L,N
C      INTEGER IS(N)
C      CHARACTER STR(L)
CC      CALL INTPR(STR,L,IS,N)
CC      WRITE(STR,IS)
C      RETURN 
C      END

C      SUBROUTINE DBPRDB(STR,L, R,N)
CC  STR is a string of length L and R is an double real vector of length N
C      INTEGER L,N
C      DOUBLE PRECISION R(N)
C      CHARACTER STR(L)
CC      CALL DBLEPR(STR,L, R, N)
CC      WRITE(STR,R)
C      RETURN 
C      END


      SUBROUTINE SIMSS(Y,Z,M,N,P,NSMPL,U,W,E,F,G,H,FK,Q,R, GAIN)
C
C  Simulate a state space model model.
C  See s code simulate.ss for details 
      INTEGER M,N,P, NSMPL
      INTEGER GAIN
      DOUBLE PRECISION Z(NSMPL,N)
      DOUBLE PRECISION Y(NSMPL,P),U(NSMPL,M) 
      DOUBLE PRECISION W(NSMPL,P),E(NSMPL,N) 
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),R(P,P),Q(N,N),FK(N,P)
C
C Note: the first period is done in the calling S routine so that
C   intial conditions can be handled there.
C
      DO 1000 IT=2,NSMPL

      DO 2001 I=1,N
         Z(IT,I)=0.0
         DO 2001 K=1,N
2001         Z(IT,I)=Z(IT,I)+F(I,K)*Z(IT-1,K)
      IF (M.NE.0) THEN
         DO 2003 I=1,N
            DO 2003 K=1,M
2003          Z(IT,I)=Z(IT,I)+G(I,K)*U(IT,K)
      ENDIF
      IF (GAIN .EQ. 1) THEN
        DO 2004 I=1,N
          DO 2004 K=1,P
2004        Z(IT,I)=Z(IT,I)+FK(I,K)*W(IT-1,K)
      ELSE
        DO 2005 I=1,N
          DO 2005 K=1,N
2005        Z(IT,I)=Z(IT,I)+Q(I,K)*E(IT-1,K)
      ENDIF

      DO 2010 I=1,P
         Y(IT,I)=0.0
         DO 2010 J=1,N
2010        Y(IT,I)= Y(IT,I) + H(I,J)*Z(IT,J)
      IF (GAIN .EQ. 1) THEN
         DO 2014 I=1,P
2014        Y(IT,I)=Y(IT,I)+W(IT,I)
      ELSE
         DO 2015 I=1,P
           DO 2015 K=1,P
2015        Y(IT,I)=Y(IT,I)+R(I,K)*W(IT,K)
      ENDIF
1000  CONTINUE
      RETURN 
      END
C
      SUBROUTINE SMOOTH( Z, TRKERR, U,Y, N,M,P,NSMPL, F,G,H,RR,
     +  IS, A,D,L,PT1, ZT, IPIV)
C see also the version XMOOTH below for debugging.
C
C   Calculate the smoothed state for the model:
C
C        z(t) = Fz(t-1) + Gu(t) + Qe(t)
C        y(t) = Hz(t)  + Rw(t)
C 
C   see KF and KF.s for details.
C   Z should be supplied as the filtered estimate of the state and is
C     returned as the smoothed estimate, and similarily for the 
C     tracking error TRKERR. 

      PARAMETER (NSTART=1)
      INTEGER N,M,P, NSMPL
      DOUBLE PRECISION Z(NSMPL,N),TRKERR(NSMPL,N,N)
      DOUBLE PRECISION U(NSMPL,M), Y(NSMPL,P)
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),RR(P,P)
C   
C   IS is the maximum state dimension  and the maximum output
C     dimension  (used for working arrays)
C   The following are for scratch space
      DOUBLE PRECISION A(IS,IS),D(IS,IS),L(IS,IS),PT1(IS,IS)
      DOUBLE PRECISION ZT(IS)
      INTEGER IPIV(IS,IS)

C
C      CALL DBPR('M     ',6, M,1)
C      CALL DBPR('N     ',6, N,1)
C      CALL DBPR('P     ',6, P,1)
C      CALL DBPR('NSMPL  ',6, NSMPL,1)
      
C
C        RR= RR' 
C
C  Next period state and tracking error get clobbered in 
C    backwards recursion, so:             
      DO 902 I=1,N
        DO 902 J=1,N
902         PT1(I,J)=TRKERR(NSMPL,I,J)

      DO 1000 IT=NSMPL-1,NSTART,-1

C                                          D=H*P(t|t-1)*H' + RR
      DO 2 I=1,N
         DO 2 J=1,P
            A(J,I)=0.0D0
            DO 2 K=1,N
  2            A(J,I)=A(J,I)+TRKERR(IT,I,K)*H(J,K)
C                     A now has  ( P(t|t-1)*H' )'                 
      DO 3 I=1,P
         DO 3 J=1,P
            D(I,J)=RR(I,J)
            DO 3 K=1,N
  3            D(I,J)=D(I,J)+H(I,K)*A(J,K)
C                     D now has  H*P(t|t-1)*H' + RR                

C                        Kalman gain    K=P(t|t-1)*H'*inv(D)   (A5)
C  For inverse call DGETRI, for solve call DGETRS.
CC    Invert method ( invert in place )
C      CALL DGETRF( P, P, D, IS, IPIV, INFO )
C      CALL DGETRI( P, D, IS, IPIV, L, IS*IS, INFO )
C
C      DO 4 I=1,N
C         DO 4 J=1,P
C            L(I,J)=0.0D0
C            DO 4 K=1,P
C  4            L(I,J)=L(I,J)+A(K,I)*D(K,J)

C     Solve  method (solve in place. Result is in non-inverted array.)
      CALL DGETRF( P, P, D, IS, IPIV, INFO )
C       A has been transposed to use here 
      CALL DGETRS( 'T', P, N, D, IS, IPIV, A, IS, INFO )

      DO 4 I=1,N
         DO 4 J=1,P
  4            L(I,J)=A(J,I)


C                       L now contains the Kalman gain K

C      IF (IT.EQ.NSMPL-1) THEN
C         CALL DBPRDB('K in L ',7,L,IS*IS)
C      ENDIF

C  E(z(t)|y(t),u(t+1) ZT = z(t|t) = Z(t|t-1) + K*(y - H*Z(t|t-1))  (A6)
      DO 107 I=1,P
            A(I,1)=Y(IT,I)
            DO 107 K=1,N
107            A(I,1)=A(I,1) - H(I,K)*Z(IT,K)
C                     A(,1) now has  y - H*Z(t|t-1)                   
      DO 109 I=1,N
            ZT(I)=Z(IT,I)
            DO 109 K=1,P
109            ZT(I)=ZT(I) + L(I,K)*A(K,1)
C                     ZT now has  Z(t|t-1) + K*(y - H*Z(t|t-1))                  


C                           P(t|t) = P(t|t-1) - K*H*P(t|t-1)      (A7)
      DO 7 I=1,N
         DO 7 J=1,N
            A(I,J)=0.0D0
            DO 7 K=1,P
  7            A(I,J)=A(I,J) + L(I,K)*H(K,J)
C                     A now has  K*H                   
      DO 8 I=1,N
         DO 8 J=1,N
            L(I,J)=TRKERR(IT,I,J)
            DO 8 K=1,N
  8            L(I,J)=L(I,J) - A(I,K)*TRKERR(IT,K,J)
C                     L now has  P(t|t) = P(t|t-1) - K*H*P(t|t-1)               
      DO 9 I=1,N
            DO 9 J=1,N
  9            A(I,J)=(L(I,J)+L(J,I))/2.0D0
C                                        A now contains P(t|t)
C      IF (IT.EQ.NSMPL-1) THEN
C         CALL DBPRDB('P(t|t) in A ',12,A,IS*IS)
C      ENDIF

C                                   J = P(t|t)*F'*inv(P(t+1|t))  (A8)
      DO 51 I=1,N
        DO 51 J=1,N
 51        L(I,J)=PT1(I,J)

C  For inverse call DGETRI, for solve call DGETRS.
CC    Invert method ( invert in place )
C      CALL DGETRF( N, N, L, IS, IPIV, INFO )
C      CALL DGETRI( N, L, IS, IPIV, D, IS*IS, INFO )
C
CC  transpose D to be consistent with solve method
C      DO 52 I=1,N
C        DO 52 J=1,N
C            D(J,I)=0.0D0
C            DO 52 K=1,N
C 52            D(J,I)=D(J,I) + F(K,I)*L(K,J)

C     Solve  method (solve in place. Result is in non-inverted array.)
      CALL DGETRF( N, N, L, IS, IPIV, INFO )
      DO 52 I=1,N
        DO 52 J=1,N
 52        D(I,J)=F(I,J)
      CALL DGETRS( 'T', N, N, L, IS, IPIV, D, IS, INFO )


C                       D  now contains ( F'*inv(P(t+1|t)) )'
      DO 53 I=1,N
        DO 53 J=1,N
         L(I,J)=0.0D0
            DO 53 K=1,N
 53            L(I,J)=L(I,J) + A(I,K)*D(J,K)
C                         L now contains J and A still contains P(t|t).

C            smoothed state sm[t] = ZT + J*(sm[t+1] - F*ZT - G*u(t+1))  (A9)
      DO 16 I=1,N
         D(I,1)=Z(IT+1,I)
         DO 16 K=1,N
 16            D(I,1)=D(I,1) - F(I,K)*ZT(K)
C                         D now contains sm[t+1] - F*ZT 
      IF (M.NE.0) THEN
         DO 17 I=1,N
            DO 17 K=1,M
 17            D(I,1)=D(I,1) - G(I,K)*U(IT+1,K)
      ENDIF
C                         D now contains sm[t+1] - F*ZT - G*u(t+1)
      DO 18 I=1,N
         Z(IT,I)=ZT(I)
         DO 18 K=1,N
 18           Z(IT,I)=Z(IT,I) + L(I,K)*D(K,1)
C                        Z now contains ZT + J*(sm[t+1] - F*ZT - G*u(t+1))

C  smoothed tracking error strk[t]= P[t|t] + J*(strk[t+1]-trk[t+1])*J'  (A10)
C     L contains J and A contains P(t|t) and PT1 contains trk[t+1].
      DO 26 I=1,N
         DO 26 J=1,N
 26          PT1(I,J)=TRKERR(IT+1,I,J) - PT1(I,J)
C                       PT1 now contains (strk[t+1]-trk[t+1])
      DO 27 I=1,N
         DO 27 J=1,N
            D(I,J)=0.0D0
            DO 27 K=1,N
 27            D(I,J)=D(I,J)+PT1(I,K)*L(J,K)
C                       D now contains (strk[t+1]-trk[t+1])*J'
      DO 28 I=1,N
         DO 28 J=1,N
            DO 28 K=1,N
 28            A(I,J)=A(I,J)+L(I,K)*D(K,J)
C                       A now contains P[t|t] + J*(strk[t+1]-trk[t+1])*J'
      DO 29 I=1,N
         DO 29 J=1,N
 29         PT1(I,J)=TRKERR(IT,I,J)
C  PT1 now contains trk[t+1], the filter tracking error for the next iteration  
      DO 30 I=1,N
            DO 30 J=1,N
 30            TRKERR(IT,I,J)=(A(I,J)+A(J,I))/2.0D0
C  TRKERR(IT,,) now contains smtrk[t+1], the smoother tracking error  

1000  CONTINUE

      RETURN 
      END
C


      SUBROUTINE KF(EY, HPERR,PRDERR,ERRWT, LSTATE,STATE,
     + LTKERR,TRKERR,
     + M,N,P,NSMPL,NPRED,NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)
C
C   Calculate the likelihood value for the model:
C
C        z(t) = Fz(t-1) + Gu(t) + Qe(t-1)
C        y(t) = Hz(t)  + Rw(t)
C 
C or the innovations model:
C        z(t) = Fz(t-1) + Gu(t) + FKw(t-1)
C        y(t) = Hz(t)  + w(t)
C  
C  
C  FK is the Kalman gain
C  If GAIN is true then FK is taken as given (innovations model)
C
C  M is the dimension of the input u.
C  N is the dimension of the state z and the system noise e.
C  P is the dimension of the output y and the ouput noise w.
C  NSMPL is the length of the data series to use for residual
C      and likelihood calculations.
C  NPRED is the period to predict ahead (past NSMPL) (for z and y)
C  NACC is the actual first (time) dimension of Y and U.
C 
C   STATE is the one step ahead ESTIMATE OF STATE.
C   It is returned only if LSTATE is TRUE.
C   Z0  is the initial state (often set to zero).
C   P0  is the initial state tracking error (often set to I and 
C   totally ignored in innovations models).
C   PP is  the one step ahead est. cov matrix of the state estimation error.
C   TRKERR is the history of PP at each period. 
C   It is returned only for non-innovations models (GAIN=FALSE) and
C     then only if LTKERR is TRUE.
C   EY is the output prediction. EY is used to store WW during computation!
C   The prediction error at each period is WW (innovations) = Y - EY.
C     If HPERR is equal or greater than one then weighted prediction 
C     errors are calculated up to the horizon indicated
C     by HPERR. The weights taken from ERRWT are applied to the squared
C     error at each period ahead.

C   If HPERR is zero and LSTATE and LTKERR are false then ERRWT,
C    PRDERR, STATE, and TRKERR are not referenced, 
C    so KF can be called with dummy arguments as 
C    in KFP and GEND.
C             
C   IS is the maximum state dimension  and the maximum output
C     dimension  (used for working arrays)
C    NSTART is not properly implemented and must be set to 1.
C
C      PARAMETER (IS=100,NSTART=1)
      PARAMETER (NSTART=1)

      INTEGER HPERR, M,N,P, NSMPL, NACC

      DOUBLE PRECISION EY(NPRED,P), PRDERR(NSMPL,P)
      DOUBLE PRECISION ERRWT(HPERR)
      DOUBLE PRECISION STATE(NPRED,N),TRKERR(NPRED,N,N), Z0(N),P0(N,N)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),R(P,P),Q(N,N),FK(N,P)

      INTEGER LSTATE, LTKERR, GAIN
C   
C
      INTEGER IPIV(IS,IS)

      DOUBLE PRECISION A(IS,IS),AA(IS,IS),PP(IS,IS),QQ(N,N),RR(P,P)
      DOUBLE PRECISION Z(IS), ZZ(IS), WW(IS)

      DOUBLE PRECISION HW
      INTEGER LPERR
C
C      CALL DBPR('M     ',6, M,1)
C      CALL DBPR('N     ',6, N,1)
C      CALL DBPR('P     ',6, P,1)
C      CALL DBPR('NSMPL  ',6, NSMPL,1)
C      CALL DBPR('NPRED  ',6, NPRED,1)
C      CALL DBPR('NACC  ',6, NACC,1)
C      CALL DBPRDB('R  ',3,R,9)
C      IF (GAIN.EQ.1) THEN 
C         CALL DBPR('GAIN is T',9, 1,0)
C      ELSE
C         CALL DBPR('GAIN is F',9, 1,0)
C      ENDIF
C      CALL DBPRDB('Q  ',3,Q,N*N)
C      CALL DBPR('HPERR ',6, HPERR,1)
      
      LPERR= 0
      IF (HPERR.GT.0) THEN
         DO 500 I=1,HPERR
           IF (ERRWT(I).GT.0.0)  LPERR= 1
 500     CONTINUE    
      ENDIF
C
C     initial innovation.
      DO 209 I=1,P
 209         WW(I)=0.0
C     initial state.
      DO 210 I=1,N
 210         ZZ(I)=Z0(I) 
C
      IF (GAIN .NE. 1) THEN
C        initial tracking error.
         DO 220 I=1,N
            DO 220 J=1,N
 220           PP(I,J)=P0(I,J)
C
C        RR= RR'    QQ= QQ'
C

         DO 230 I=1,N
          DO 230 J=1,N
            QQ(I,J)=0.0D0
            DO 230 K=1,N
 230           QQ(I,J)=QQ(I,J)+Q(I,K)*Q(J,K)
         DO 236 I=1,P
           DO 236 J=1,P
            RR(I,J)=0.0D0
            DO 236 K=1,P
 236           RR(I,J)=RR(I,J)+R(I,K)*R(J,K) 
       ENDIF
C
C    Start Time loop
C
      DO 1000 IT=NSTART,NSMPL
      IF (GAIN .NE. 1) THEN  
                                   
C   Kalman gain  FK = F*P(t|t-1)*H'* inv( H*P(t|t-1)*H' + RR')
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ P(t|t-1)*H' ~~~~~~
      DO 5 I=1,N
         DO 5 J=1,P
            AA(I,J)=0.0D0
            DO 5 K=1,N
  5            AA(I,J)=AA(I,J)+PP(I,K)*H(J,K)

C    ~~~~~~~~~~~~~~~~~~F*P(t|t-1)*H'~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 6 I=1,N
         DO 6 J=1,P
            FK(I,J)=0.0D0
            DO 6 K=1,N
  6           FK(I,J)=FK(I,J)+F(I,K)*AA(K,J)

C     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~H*P(t|t-1)*H' + RR'~~~
      DO 7 I=1,P
         DO 7 J=1,P
            A(I,J)=RR(I,J)
            DO 7 K=1,N
  7            A(I,J)=A(I,J)+H(I,K)*AA(K,J)
C      CALL DBPRDB('DO 7 A ',7, A,(IS*IS))
C     force symetry.
      DO 9 I=1,P
         DO 9 J=1,P
  9         AA(I,J)=(A(I,J)+A(J,I))/2.0D0

C     ~~~~~~~~~~~~~FK =F*P(t|t-1)*H'* inv( H*P(t|t-1)*H' + RR')
C  For inverse call DGETRI, for solve call DGETRS.
C     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ inv( H*P(t|t-1)*H' + RR')
CC    Invert method ( invert in place )
C      CALL DGETRF( P, P, AA, IS, IPIV, INFO )
C      CALL DGETRI( P, AA, IS, IPIV, A, IS*IS, INFO )
C
C     ~~~~~~~~~~~~~~~~~F*P(t|t-1)*H'* inv( H*P(t|t-1)*H' + RR')
C      DO 14 I=1,N
C         DO 14 J=1,P
C            A(I,J)=0.0D0
C            DO 14 K=1,P
C  14           A(I,J)=A(I,J)+FK(I,K)*AA(K,J)
C
C      DO 15 I=1,N
C         DO 15 J=1,P
C  15           FK(I,J)=A(I,J)

C     Solve  method (solve in place. Result is in non-inverted array.)
C     ~~~~~~~~~~~~~~~~~F*P(t|t-1)*H'* inv( H*P(t|t-1)*H' + RR')
      CALL DGETRF( P, P, AA, IS, IPIV, INFO )
C       need KF transpose 
      DO 14 I=1,P
         DO 14 J=1,N
  14           A(I,J)=FK(J,I)
      CALL DGETRS( 'T',  P, N, AA, IS, IPIV, A, IS, INFO )

      DO 15 I=1,N
         DO 15 J=1,P
  15           FK(I,J)=A(J,I)

C   Now FK has the Kalman gain

C      IF (IT.EQ.1) THEN
C         CALL DBPRDB('K ',2,FK,N*P)
C      ENDIF
C
C     P(t|t-1)= F*P(t-|t-2)*F' -  K*H*P(t-1|t-2)*F' + Q*Q' 
C
      DO 2 I=1,N
         DO 2 J=1,N
            A(I,J)=0.0D0
            DO 2 K=1,N
  2            A(I,J)=A(I,J)+PP(I,K)*F(J,K)
      DO 3 I=1,N
         DO 3 J=1,N
            AA(I,J)=QQ(I,J)
            DO 3 K=1,N
  3            AA(I,J)=AA(I,J)+F(I,K)*A(K,J)
      DO 18 I=1,P
         DO 18 J=1,N
            PP(I,J)=0.0D0
            DO 18 K=1,N
  18           PP(I,J)=PP(I,J)+H(I,K)*A(K,J)
      DO 19 I=1,N
         DO 19 J=1,N
           DO 19 K=1,P
  19        AA(I,J)=AA(I,J)-FK(I,K)*PP(K,J)
C     force symetry to avoid numerical round off problems
      DO 20 I=1,N
         DO 20 J=1,N
  20        PP(I,J)=( AA(I,J)+AA(J,I) )/2.0D0
C      IF (IT.EQ.1) THEN
C         CALL DBPRDB('PP  ',4,PP,IS*IS)
C      ENDIF
      IF(LTKERR .EQ. 1) THEN
         DO 21 I=1,N
            DO 21 J=1,N
  21           TRKERR(IT,I,J)=PP(I,J)
      ENDIF
      ENDIF
C   end of Kalman gain and tracking error update ( if NOT GAIN )
C
C   one step ahead state estimate
C     z(t|t-1)= Fz(t-1|t-2) + FK*WW(t-1) + Gu(t)
C
      DO 1 I=1,N
         Z(I)=0.0
         DO 1 K=1,N
  1         Z(I)=Z(I)+F(I,K)*ZZ(K)
      DO 22 I=1,N
         DO 22 K=1,P
  22        Z(I)=Z(I)+FK(I,K)*WW(K)
      DO 23 I=1,N
         DO 23 K=1,M
  23        Z(I)=Z(I)+G(I,K)*U(IT,K)
      DO 24 I=1,N
 24         ZZ(I)=Z(I)
      IF (LSTATE .EQ. 1) THEN
         DO 25 I=1,N
 25         STATE(IT,I)=Z(I)
      ENDIF
C      CALL DBPRDB('Z  ',3,Z,N)


C  one step ahead prediction  EY(t)=H*z(t|t-1))
C
C  innovations  WW(t)=y(t)-H*z(t|t-1))
C
C   EY stores history of predition error WW to reconstruct predictions
      DO 10 I=1,P
         WW(I)=Y(IT,I)
         DO 10 J=1,N
  10        WW(I)= WW(I) - H(I,J)*Z(J)
      DO 11 I=1,P
  11        EY(IT,I)= WW(I)
C      CALL DBPRDB('WW ',3,WW,P)

C   Return weighted prediction error
      IF (LPERR .EQ. 1) THEN
         DO 401 I=1,P
 401          PRDERR(IT,I)= ERRWT(1)*WW(I)**2
         IF (HPERR.GT.1) THEN
            DO 400 K=2,HPERR        
              IF ((IT+K-1).LE.NSMPL) THEN
                 DO 402 I=1,N
 402                AA(1,I) = Z(I)
                 DO 409 I=1,N
 409                Z(I) = 0.0
                 DO 403 I=1,N
                    DO 403 J=1,N
 403                   Z(I)= Z(I)+F(I,J)*AA(1,J)
                 IF (K.EQ.2) THEN
                    DO 406 I=1,N
                       DO 406 J=1,P
 406                      Z(I)= Z(I)+FK(I,J)*WW(J)
                 ENDIF
                 IF(M.NE.0) THEN
                    DO 404 I=1,N
                       DO 404 J=1,M
 404                      Z(I)= Z(I)+G(I,J)*U(IT+K-1,J)
                 ENDIF
                 DO 408 I=1,P
                    HW=0.0
                    DO 407 J=1,N
 407                   HW = HW+ H(I,J)*Z(J)
 408                PRDERR(IT,I)= PRDERR(IT,I) +
     +                               ERRWT(K)*(Y(IT+K-1,I)-HW)**2
C                 IF (IT.GT.97) THEN
C                    CALL DBPR('K     ',6, K,1)
C                    CALL DBPRDB('PRDERR ',8,PRDERR,NSMPL*P)
C                 ENDIF
              ENDIF
 400      CONTINUE
         ENDIF
      ENDIF

1000  CONTINUE
C  end of time loop


C     reconstruct predictions
      DO 41 IT=NSTART,NSMPL
         DO 41 I=1,P
  41        EY(IT,I)=Y(IT,I) - EY(IT,I)
C
C    Start multi-step prediction loop
C
      IF (NPRED .GT. NSMPL) THEN  
      DO 2000 IT=NSMPL+1, NPRED
C
      DO 2001 I=1,N
         Z(I)=0.0
         DO 2001 K=1,N
2001         Z(I)=Z(I)+F(I,K)*ZZ(K)
      IF (M.NE.0) THEN
         DO 2003 I=1,N
            DO 2003 K=1,M
2003           Z(I)=Z(I)+G(I,K)*U(IT,K)
      ENDIF
C   use prediction error from last sample point (i.e. for first prediction)
      IF (IT.EQ.(NSMPL+1)) THEN
         DO 2004 I=1,N
            DO 2004 K=1,P
2004           Z(I)=Z(I)+FK(I,K)*WW(K)
      ENDIF
      IF (LSTATE .EQ. 1) THEN
         DO 2005 I=1,N
2005        STATE(IT,I)=Z(I)
      ENDIF
      DO 2010 I=1,P
         EY(IT,I)=0.0
         DO 2010 J=1,N
2010        EY(IT,I)= EY(IT,I) + H(I,J)*Z(J)
      DO 2024 I=1,N
2024        ZZ(I)=Z(I)

2000  CONTINUE
      ENDIF
C  end of multi-step prediction loop

      RETURN 
      END
C
C
C        1         2         3         4         5         6         7         8
      SUBROUTINE SIMRMA(Y,Y0,M,P,IA,IB,IC,NSMPL,U,U0,W,W0,A,B,C,TREND)
C      Simulate an ARMA model. See documentation in ARMA and in the S version.
      PARAMETER (NSTART=1)

      INTEGER M,P,IA,IB,IC, NSMPL

      DOUBLE PRECISION Y(NSMPL,P),U(NSMPL,M),Y0(IA,P), U0(IC,M)
      DOUBLE PRECISION W(NSMPL,P), W0(IB,P)
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M), TREND(NSMPL,P)

C       CALL DBPRDB('inSIMARMA ',7, 1,1)
C       CALL DBPR('M     ',6, M,1)
C       CALL DBPRDB('A     ',6, A,(IA*P*P))
C      IF (IT.LE.5) CALL DBPRDB('step1 ',6, Y(IT,3),1)
C      CALL DBPRDB('C ',2, C,(IC*P*M) )
C      CALL DBPRDB('U0 ',3, U0,(IC*M) )
C      CALL DBPRDB('U  ',3, U,(NSMPL*M) )

      DO 2001 I=1,P
        DO 2001 IT=NSTART,NSMPL
2001         Y(IT,I)= 0.0D0

      DO 1 IT=NSTART,NSMPL 
      DO 1 I=1,P
 1         Y(IT,I)= TREND(IT,I)

      DO 1000 IT=NSTART,NSMPL
 
C      IF (IT.LE.5) CALL DBPRDB('step1 ',6, Y(IT,3),1)
      DO 5 L=2,IA
         IF ((IT+1).LE.L) THEN
            DO 2 I=1,P
               DO 2 J=1,P
2               Y(IT,I)=Y(IT,I)-A(L,I,J)*Y0(L-IT,J)
         ELSE
            DO 3 I=1,P
               DO 3 J=1,P
3               Y(IT,I)=Y(IT,I)-A(L,I,J)* Y(IT+1-L,J)
         ENDIF
5     CONTINUE
C      IF (IT.LE.5) CALL DBPRDB('step2 ',6, Y(IT,3),1)

      DO 15 L=1,IB
         IF ((IT+1).LE.L) THEN
            DO 12 I=1,P
               DO 12 J=1,P
12               Y(IT,I)=Y(IT,I)+B(L,I,J)*W0(L-IT,J)
         ELSE
            DO 13 I=1,P
               DO 13 J=1,P
13               Y(IT,I)=Y(IT,I)+B(L,I,J)* W(IT+1-L,J)
         ENDIF
15    CONTINUE
C      IF (IT.LE.5) CALL DBPRDB('step3 ',6, Y(IT,3),1)
      IF (M.GT.0) THEN
      DO 25 L=1,IC
         IF ((IT+1).LE.L) THEN
            DO 22 I=1,P
               DO 22 J=1,M
22               Y(IT,I)=Y(IT,I)+C(L,I,J)*U0(L-IT,J)
C         IF (IT.LE.5) CALL DBPRDB('stepa ',6, Y(IT,3),1)
         ELSE
            DO 23 I=1,P
               DO 23 J=1,M
23               Y(IT,I)=Y(IT,I)+C(L,I,J)* U(IT+1-L,J)
C           IF (IT.LE.5) CALL DBPRDB('stepb ',6, Y(IT,3),1)
         ENDIF
25    CONTINUE
      ENDIF
C      IF (IT.LE.5) CALL DBPRDB('step4 ',6, Y(IT,3),1)

1000  CONTINUE
      RETURN 
      END

      SUBROUTINE ARMA(EY, HPERR, PRDERR, ERRWT,                  
     + M,P,IA,IB,IC,NSMPL,NPRED,NACC,  U,Y , A,B,C, TREND, 
     + IS,AA,BB,WW,IPIV)
C sampleT is the length of data which should be used for estimation.
C Calculate the one-step ahead predictions, and likelihood value for the model:
C
C       A(L)y(t) =  B(L)w(t) + C(L)u(t)  + TREND(t)
C 
C A(L) (axpxp) is the auto-regressive polynomial array.
C B(L) (bxpxp) is the moving-average polynomial array.
C C(L) (cxpxm) is the  input polynomial array.
C TREND is a constant vector added at each period.
C y is the p dimensional output data.
C u is the m dimensional control (input) data.

C  M is the dimension of the input u.
C  P is the dimension of the output y and the ouput noise w.
C  NSMPL is the length of the data series to use for residual
C      and likelihood calculations.
C  NPRED is the period to predict (past NSMPL)
C  NACC is the actual first (time) dimension of Y and U.
C 
C   EY is the output prediction. Initially EY is used to store WW.
C   The prediction error WW (innovations) = Y - EY.

C   Weighted prediction errors are returned in PRDERR.
C     If HPERR is equal or greater than one then weighted prediction 
C     errors are calculated up to the horizon indicated
C     by HPERR. The weights taken from ERRWT are applied to the squared
C     error at each period ahead.

C   If HPERR is zero or all elements of ERRWT are zero then
C   LPERR is set false then PRDERR is not referenced, so ARMA can be called
C    with dummy arguments as in GEND.
C             
C    NSTART is not properly implemented and must be set to 1.
C
      PARAMETER (NSTART=1)

      INTEGER M,P,IA,IB,IC, NSMPL, NACC
      INTEGER HPERR

      DOUBLE PRECISION EY(NPRED,P), PRDERR(NSMPL,P)
      DOUBLE PRECISION ERRWT(HPERR)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M), TREND(NPRED,P)
C   IS should be max(P,M)
      DOUBLE PRECISION AA(IS,IS),BB(IS,IS),WW(IS)
C   
C
      INTEGER LPERR
      INTEGER IPIV(IS,IS)
C
C       CALL DBPRDB('inARMA ',7, 1,1)
C      CALL DBPR('M     ',6, M,1)
C      CALL DBPR('P     ',6, P,1)
C      CALL DBPR('IA    ',6, IA,1)
C      CALL DBPR('IB    ',6, IB,1)
C      CALL DBPR('IC    ',6, IC,1)
C      CALL DBPR('NSMPL  ',6, NSMPL,1)
C      CALL DBPR('NPRED  ',6, NPRED,1)
C      CALL DBPR('NACC  ',6, NACC,1)
C       CALL DBPRDB('A     ',6, A,(IA*P*P))
C       CALL DBPRDB('B     ',6, B,(IB*P*P))
C       CALL DBPRDB('C     ',6, C,(IC*P*M))


      LPERR= 0
      IF (HPERR.GT.0) THEN
         DO 500 I=1,HPERR
           IF (ERRWT(I).GT.0.0)  LPERR= 1
 500     CONTINUE    
      ENDIF
C
C     Ensure B(0) = I by inverting B(0) and multiplying through
C        B(1,,) is not modified yet as B(0) is needed later, but
C        it is not referenced through the time loop, so effectively
C        asummed = I.
C
      DO 300 I=1,P
         DO 300 J=1,P
300         BB(I,J)=B(1,I,J)

C      CALL INVERS(BB,P,IS,DETOM)
C     invert in place. 
      CALL DGETRF( P, P, BB, IS, IPIV, INFO )
C  For inverse call DGETRI, for solve call DGETRS.
      CALL DGETRI( P, BB, IS, IPIV, AA, IS*IS, INFO )
C      CALL DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )

      DO 302 L=1,IA
         DO 301 I=1,P
            DO 301 J=1,P
               AA(I,J)=A(L,I,J)
301            A(L,I,J)=0.0D+0
         DO 302 I=1,P
            DO 302 J=1,P
               DO 302 K=1,P
302               A(L,I,J)=A(L,I,J)+BB(I,K)*AA(K,J)
      DO 304 L=2,IB
         DO 303 I=1,P
            DO 303 J=1,P
               AA(I,J)=B(L,I,J)
303            B(L,I,J)=0.0D+0
         DO 304 I=1,P
            DO 304 J=1,P
               DO 304 K=1,P
304               B(L,I,J)=B(L,I,J)+BB(I,K)*AA(K,J)
      DO 306 L=1,IC
         DO 305 I=1,P
            DO 305 J=1,M
               AA(I,J)=C(L,I,J)
305            C(L,I,J)=0.0D+0
         DO 306 I=1,P
            DO 306 J=1,M
               DO 306 K=1,P
306               C(L,I,J)=C(L,I,J)+BB(I,K)*AA(K,J)
      DO 308 IT=1,NSMPL
         DO 307 I=1,P
            AA(I,1)=TREND(IT,I)
307         TREND(IT,I)=0.0D+0
         DO 308 I=1,P
            DO 308 K=1,P
308            TREND(IT,I)=TREND(IT,I) + BB(I,K)*AA(I,1)
         DO 309 IT=1,NPRED
            DO 309 J=1,P
309            EY(IT,J)=0.0D+0
C
C    Start Time loop
C
      DO 1000 IT=NSTART,NSMPL
C  
      DO 1 I=1,P
 1         WW(I)= -TREND(IT,I)
C
      DO 22 L=1,IA
         IF (L.LE.IT) THEN
            DO 2 I=1,P
               DO 2 J=1,P
2               WW(I)=WW(I)+A(L,I,J)*Y(IT+1-L,J)
         ENDIF
22    CONTINUE
C
      IF (IB.GE.2) THEN
         DO 23 L=2,IB
            IF (L.LE.IT) THEN
               DO 3 I=1,P
                  DO 3 J=1,P
3                  WW(I)=WW(I)-B(L,I,J)*EY(IT+1-L,J)
            ENDIF
23       CONTINUE
      ENDIF
C
      DO 24 L=1,IC
         IF (L.LE.IT) THEN
            DO 4 I=1,P
               DO 4 J=1,M
C         CALL DBPRDB('C     ',6, C(L,I,J),1)
C         CALL DBPRDB('U     ',6,U(IT+1-L,J) ,1)
4               WW(I)=WW(I)-C(L,I,J)*U(IT+1-L,J)
         ENDIF
24    CONTINUE
C      IF (IT.LE.3) THEN
C         CALL DBPRDB('ww  ',4, WW,P)
C      ENDIF
C
C   EY stores history of predition error WW to reconstruct predictions
      DO 10 I=1,P
10         EY(IT,I)=WW(I)

C   Return weighted prediction error
      IF (LPERR .EQ. 1) THEN
         DO 410 I=1,P
 410          PRDERR(IT,I)= ERRWT(1)*WW(I)**2
         IF (HPERR.GT.1) THEN
            DO 400 K=2,HPERR        
              IF ((IT+K-1).LE.NSMPL) THEN
                 DO 401 I=1,P
 401                WW(I)= -TREND(IT,I)
C
              DO 4022 L=1,IA
                  IF (L.LT.(IT+K)) THEN
                     DO 402 I=1,P
                        DO 402 J=1,P
402                        WW(I)=WW(I)+A(L,I,J)*Y(IT+K-L,J)
                  ENDIF
4022          CONTINUE
C
              IF (IB.GE.2) THEN
                 DO 4023 L=2,IB
                    IF (L.LT.(IT+K)) THEN
                       DO 403 I=1,P
                          DO 403 J=1,P
403                          WW(I)=WW(I)-B(L,I,J)*EY(IT+K-L,J)
                    ENDIF
4023             CONTINUE
              ENDIF
C
              DO 4024 L=1,IC
                 IF (L.LT.(IT+K)) THEN
                    DO 404 I=1,P
                       DO 404 J=1,M
404                       WW(I)=WW(I)-C(L,I,J)*U(IT+K-L,J)
                 ENDIF
4024           CONTINUE
C              correction for WW by B0. 
C      NB this has not been well tested and models with B0 != I may not work !!
               DO 407 I=1,P
                  DO 407 J=1,P
 407                BB(I,1) = BB(I,1) + B(1,I,J) * WW(J)
               DO 408 I=1,P
 408              PRDERR(IT,I)= PRDERR(IT,I) + ERRWT(K)*BB(I,1)**2
C                 IF (IT.GT.97) THEN
C                    CALL DBPR('K     ',6, K,1)
C                    CALL DBPRDB('PRDERR ',8,PRDERR,NSMPL*P)
C                 ENDIF
              ENDIF
 400      CONTINUE
         ENDIF
      ENDIF

1000  CONTINUE
C  end of time loop

C
      DO 45 IT=NSTART,NSMPL
         DO 40 I=1,P
  40        WW(I)=0.0
         DO 41 I=1,P
           DO 41 J=1,P
  41        WW(I)=WW(I) + B(1,I,J) *EY(IT,J)
         DO 45 I=1,P
  45        EY(IT,I)=Y(IT,I) - WW(I)
C
C    Start multi-step prediction loop
C
      IF (NSMPL .LT. NPRED) THEN
C  B(1,,) now needs to be filled in as I (storage was previously use for B0).
      DO 2298 I=1,P
         DO 2298 J=1,P
2298         B(1,I,J)=0.0
      DO 2299 I=1,P
2299         B(1,I,I)=1.0

C
C     Ensure A(0) = I by inverting A(0) and multiplying through
C
      DO 2300 I=1,P
         DO 2300 J=1,P
2300         BB(I,J)=A(1,I,J)

C      CALL INVERS(BB,P,IS,DETOM)
C     invert in place. 
      CALL DGETRF( P, P, BB, IS, IPIV, INFO )
C  For inverse call DGETRI, for solve call DGETRS.
      CALL DGETRI( P, BB, IS, IPIV, AA, IS*IS, INFO )
C      CALL DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )

      DO 2302 L=1,IA
         DO 2301 I=1,P
            DO 2301 J=1,P
               AA(I,J)=A(L,I,J)
2301            A(L,I,J)=0.0D+0
         DO 2302 I=1,P
            DO 2302 J=1,P
               DO 2302 K=1,P
2302               A(L,I,J)=A(L,I,J)+BB(I,K)*AA(K,J)
      DO 2304 L=1,IB
         DO 2303 I=1,P
            DO 2303 J=1,P
               AA(I,J)=B(L,I,J)
2303            B(L,I,J)=0.0D+0
         DO 2304 I=1,P
            DO 2304 J=1,P
               DO 2304 K=1,P
2304               B(L,I,J)=B(L,I,J)+BB(I,K)*AA(K,J)
      DO 2306 L=1,IC
         DO 2305 I=1,P
            DO 2305 J=1,M
               AA(I,J)=C(L,I,J)
2305            C(L,I,J)=0.0D+0
         DO 2306 I=1,P
            DO 2306 J=1,M
               DO 2306 K=1,P
2306               C(L,I,J)=C(L,I,J)+BB(I,K)*AA(K,J)
      DO 2308 IT=NSMPL+1,NPRED
         DO 2307 I=1,P
            AA(I,1)=TREND(IT,I)
2307         TREND(IT,I)=0.0D+0
         DO 2308 I=1,P
            DO 2308 K=1,P
2308            TREND(IT,I)=TREND(IT,I) + BB(I,K)*AA(I,1)

      DO 2000 IT=NSMPL+1,NPRED
C  
      DO 2001 I=1,P
2001         EY(IT,I)= TREND(IT,I)
C
      IF (IA.GE.2) THEN
        DO 2002 L=2,IA
         DO 2002 I=1,P
            DO 2002 J=1,P
                IF ((IT+1-L) .LE.NSMPL) THEN
                   EY(IT,I)=EY(IT,I)-A(L,I,J)* Y(IT+1-L,J)
                ELSE
                   EY(IT,I)=EY(IT,I)-A(L,I,J)*EY(IT+1-L,J)
                ENDIF
2002    CONTINUE
      ENDIF
      IF (IB.GE.2) THEN
         DO 2004 L=2,IB
            IF ((IT+1-L).LE.NSMPL) THEN
               DO 2003 I=1,P
                 DO 2003 J=1,P
         EY(IT,I)=EY(IT,I)+B(L,I,J)*(Y(IT+1-L,J)-EY(IT+1-L,J))
2003           CONTINUE
            ENDIF
2004     CONTINUE
      ENDIF
C
      DO 2005 L=1,IC
         DO 2005 I=1,P
            DO 2005 J=1,M
                  EY(IT,I)=EY(IT,I)+C(L,I,J)*U(IT+1-L,J)
2005  CONTINUE

2000  CONTINUE
      ENDIF
C  end of multi-step prediction loop

      RETURN 
      END

            
      SUBROUTINE KFP(EY, HPERR, PRDERR, ERRWT,
     + M,N,P,NSMPL,NPRED,NACC,U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     + ITH,PARM,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

C
C Put parameters into arrays (as in S function setArrays) and call KF
C
C  The state and tracking error are not calculated.
C       Use KF if these are needed.
C
C  It is assummed that M,N,P, the dimensions of the parameter
C    arrays - are given. Trying to calculate these causes problems.
C
      INTEGER HPERR
      INTEGER M,N,P, NSMPL, NPRED, NACC
      INTEGER ITH,ICT,IP(ITH),JP(ITH),IN(ICT),JN(ICT)
      INTEGER AP(ITH),AN(ICT)

      DOUBLE PRECISION EY(NPRED,P), PRDERR(NSMPL,P)
      DOUBLE PRECISION ERRWT(HPERR)
C      DOUBLE PRECISION STATE(NPRED,N),TRKERR(NPRED,N,N)
      DOUBLE PRECISION STATE(1,1),TRKERR(1,1,1)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),FK(N,P),R(P,P),Q(N,N)
      DOUBLE PRECISION  Z0(N), P0(N,N)

      INTEGER LSTATE, LTKERR
      INTEGER GAIN

      DOUBLE PRECISION A(IS,IS),AA(IS,IS),PP(IS,IS),QQ(N,N),RR(P,P)
      DOUBLE PRECISION Z(IS), ZZ(IS), WW(IS)
      INTEGER IPIV(IS,IS)

C..bug in S: passing characters is unreliable
C   use integer for AP and AN...
      DOUBLE PRECISION PARM(ITH),CONST(ICT)
C  state and trkerr are not used but must be passed to KF
C 
      INTEGER I,J
C
      LSTATE = 0
      LTKERR = 0

      DO 1 I=1,N
            DO 1 J=1,N
1              F(I,J) = 0.0D+0
      DO 2 I=1,N
            DO 2 J=1,M
2              G(I,J) = 0.0D+0
      DO 3 I=1,P
            DO 3 J=1,N
3              H(I,J) = 0.0D+0
      DO 4 I=1,N
            DO 4 J=1,P
4              FK(I,J) = 0.0D+0
      DO 5 I=1,N
            DO 5 J=1,N
5              Q(I,J) = 0.0D+0
      DO 6 I=1,P
            DO 6 J=1,P
6              R(I,J) = 0.0D+0
      DO 7 I=1,P
7              Z0(I) = 0.0D+0
      IF (ITH.GT.0) THEN
         DO 101 I=1,ITH
            IF   (AP(I).EQ.1) THEN  
               F(IP(I),JP(I)) = PARM(I)
            ELSEIF(AP(I).EQ.2) THEN
               G(IP(I),JP(I)) = PARM(I)
            ELSEIF(AP(I).EQ.3) THEN 
               H(IP(I),JP(I)) = PARM(I)   
            ELSEIF(AP(I).EQ.4) THEN 
              FK(IP(I),JP(I)) = PARM(I)   
            ELSEIF(AP(I).EQ.5) THEN 
               Q(IP(I),JP(I)) = PARM(I)   
            ELSEIF(AP(I).EQ.6) THEN 
               R(IP(I),JP(I)) = PARM(I)   
            ELSEIF(AP(I).EQ.7) THEN 
               Z0(IP(I)) = PARM(I)   
            ELSEIF(AP(I).EQ.8) THEN 
               P0(IP(I),JP(I)) = PARM(I)   
            ENDIF
101      CONTINUE   
      ENDIF
      IF (ICT.GT.0) THEN
         DO 102 I=1,ICT
            IF   (AN(I).EQ.1) THEN 
               F(IN(I),JN(I)) = CONST(I)
            ELSEIF(AN(I).EQ.2)  THEN
               G(IN(I),JN(I)) = CONST(I)
            ELSEIF(AN(I).EQ.3)  THEN
               H(IN(I),JN(I)) = CONST(I)   
            ELSEIF(AN(I).EQ.4) THEN 
              FK(IN(I),JN(I)) = CONST(I)   
            ELSEIF(AN(I).EQ.5) THEN 
               Q(IN(I),JN(I)) = CONST(I)   
            ELSEIF(AN(I).EQ.6) THEN 
               R(IN(I),JN(I)) = CONST(I)   
            ELSEIF(AN(I).EQ.7) THEN 
               Z0(IN(I)) = CONST(I)   
            ELSEIF(AN(I).EQ.8) THEN 
               P0(IN(I),JN(I)) = CONST(I)   
            ENDIF
102      CONTINUE   
      ENDIF

      CALL KF(EY, HPERR, PRDERR, ERRWT, LSTATE,STATE, LTKERR, TRKERR,
     + M,N,P,NSMPL,NPRED,NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)
      RETURN
      END
C

      SUBROUTINE KFPRJ(PROJ, DSCARD, HORIZ, NHO,
     + EY, M,N,P, NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

C  multiple calls to KF for prediction at given horizons.
C     See S program project.
C
C  The state and tracking error are not calculate.

      INTEGER DSCARD, NHO, HORIZ(NHO)
      INTEGER M,N,P, NACC

      DOUBLE PRECISION PROJ(NHO,NACC,P)
      DOUBLE PRECISION EY(NACC,P)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),FK(N,P),R(P,P),Q(N,N)
      DOUBLE PRECISION  Z0(N), P0(N,N)

      INTEGER GAIN

      DOUBLE PRECISION A(IS,IS),AA(IS,IS),PP(IS,IS),QQ(N,N),RR(P,P)
      DOUBLE PRECISION Z(IS), ZZ(IS), WW(IS)
      INTEGER IPIV(IS,IS)

C  state and trkerr are not used but must be passed to KF
C 
      INTEGER LSTATE, LTKERR
      INTEGER HO,J, IT, MHORIZ
      INTEGER HPERR
      DOUBLE PRECISION PRDERR(1,1), ERRWT(1)
      DOUBLE PRECISION STATE(1,1),TRKERR(1,1,1)

      LSTATE = 0
      LTKERR = 0
      HPERR  = 0
      MHORIZ = HORIZ(1)
      DO 1 I=2, NHO
 1       MHORIZ=MIN(MHORIZ,HORIZ(I))

      DO 10 IT=DSCARD, (NACC-MHORIZ)
C       this assumes HORIZ is sorted in ascending order
        IF (IT.GT.(NACC-HORIZ(NHO))) THEN
            NHO = NHO-1
C            CALL DBPR('NHO   ',6, NHO,1)
        ENDIF

        CALL KF(EY, HPERR, PRDERR, ERRWT, LSTATE,STATE, LTKERR,TRKERR,
     +     M,N,P, IT , NACC,NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     +     IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

        DO 4 HO=1,NHO
            DO 4 J=1,P
              PROJ(HO,IT+HORIZ(HO),J) = EY(IT+HORIZ(HO),J) 
4     CONTINUE
10    CONTINUE

      RETURN
      END

      SUBROUTINE KFEPR(COV, DSCARD, HORIZ, NH, NT,
     + EY, M,N,P, NPRED,NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

C  multiple calls to KF for prediction evaluation.
C     See S program predictions.cov.TSmodel
C
C  The state and tracking error are not calculate.

      INTEGER DSCARD, NH, HORIZ(NH), NT(NH)
      INTEGER M,N,P, NPRED, NACC

      DOUBLE PRECISION COV(NH,P,P)
      DOUBLE PRECISION EY(NPRED,P)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION F(N,N),G(N,M),H(P,N),FK(N,P),R(P,P),Q(N,N)
      DOUBLE PRECISION  Z0(N), P0(N,N)

      INTEGER GAIN

      DOUBLE PRECISION A(IS,IS),AA(IS,IS),PP(IS,IS),QQ(N,N),RR(P,P)
      DOUBLE PRECISION Z(IS), ZZ(IS), WW(IS)
      INTEGER IPIV(IS,IS)

C  state and trkerr are not used but must be passed to KF
C 
      INTEGER LSTATE, LTKERR
      INTEGER I,J, IT, HI
      INTEGER HPERR
      DOUBLE PRECISION PRDERR(1,1), ERRWT(1), MF
      DOUBLE PRECISION STATE(1,1),TRKERR(1,1,1)

C
C        CALL DBPR('NPRED ',6, NPRED,1)
C        CALL DBPR('HORIZ ',6, HORIZ(1),1)
C        CALL DBPR('DSCARD',7, DSCARD,1)
      LSTATE = 0
      LTKERR = 0
      HPERR  = 0
      DO 1 I=1,NH
1        NT(I) = 0
      DO 2 K=1,NH
         DO 2 I=1,P
            DO 2 J=1,P
2               COV(K,I,J)= 0.0D0

      DO 10 IT=DSCARD, 1+NPRED-HORIZ(1)

        CALL KF(EY, HPERR, PRDERR, ERRWT, LSTATE,STATE, LTKERR,TRKERR,
     +     M,N,P, IT ,NPRED,NACC,  U,Y, F,G,H,FK, Q,R, GAIN,Z0,P0,
     +     IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)
C       this assumes HORIZ is sorted in ascending order


        IF ((IT-1+HORIZ(NH)).GT.NPRED) THEN
           NH = NH-1
        ENDIF
        DO 4 I=1,NH
           HI = IT-1+HORIZ(I)
           DO 4 J=1,P
4            EY(I,J) = EY(HI,J) - Y(HI,J)
        DO 5 K=1,NH
           NT(K) = NT(K)+1
           MF= DBLE(NT(K)-1)/DBLE(NT(K))
           DO 5 I=1,P
              DO 5 J=1,P
                COV(K,I,J)= COV(K,I,J)*MF + EY(K,I)*EY(K,J)/NT(K)
5     CONTINUE

10    CONTINUE

      RETURN
      END


      SUBROUTINE ARMAP(EY, HPERR, PRDERR, ERRWT,
     + M,P,IA,IB,IC,NSMPL,NPRED,NACC,  U,Y , A,B,C, TREND,
     + ITH,PARM,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     + IS,AA,BB,WW, IPIV)
C
C Put parameters into arrays (as in S function setArrays) and call ARMA
C
C  It is assummed that M,P,IA,IB, and IC - the dimensions of the parameter
C    arrays - are given. Trying to calculate these causes problems.
C
      INTEGER M,P,IA,IB,IC, NSMPL, NACC
      INTEGER HPERR
      INTEGER ITH,ICT,IP(ITH),JP(ITH),LP(ITH),IN(ICT),JN(ICT),LN(ICT)

      DOUBLE PRECISION EY(NPRED,P), PRDERR(NSMPL,P)
      DOUBLE PRECISION ERRWT(HPERR)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M), TREND(NPRED,P) 
      DOUBLE PRECISION AA(IS,IS), BB(IS,IS), WW(IS)
      INTEGER IPIV(IS,IS)

C..bug in S: passing characters is unreliable
C   use integer for AP and AN...
      INTEGER AP(ITH),AN(ICT)
      DOUBLE PRECISION PARM(ITH),CONST(ICT)
C 
      INTEGER I,J,L
C
      DO 1 L=1,IA
         DO 1 I=1,P
            DO 1 J=1,P
1              A(L,I,J) = 0.0
      DO 2 L=1,IB
         DO 2 I=1,P
            DO 2 J=1,P
2              B(L,I,J) = 0.0 
      DO 3 L=1,IC
         DO 3 I=1,P
            DO 3 J=1,M
3              C(L,I,J) = 0.0
      DO 4 I=1,NPRED
      DO 4 J=1,P
4         TREND(I,J) = 0.0
C       IA=0
C       IB=0
C       IC=0
      IF (ITH.GT.0) THEN
         DO 101 I=1,ITH
            IF   (AP(I).EQ.1) THEN  
               A(LP(I),IP(I),JP(I)) = PARM(I)
C               IA=MAX(IA,LP(I))
            ELSEIF(AP(I).EQ.2) THEN
               B(LP(I),IP(I),JP(I)) = PARM(I)
C               IB=MAX(IB,LP(I))
            ELSEIF(AP(I).EQ.3) THEN 
               C(LP(I),IP(I),JP(I)) = PARM(I)   
C               IC=MAX(IC,LP(I))
            ELSEIF(AP(I).EQ.4) THEN 
               TREND(IP(I),JP(I)) = PARM(I)  
            ENDIF
101      CONTINUE   
      ENDIF
      IF (ICT.GT.0) THEN
         DO 102 I=1,ICT
            IF   (AN(I).EQ.1) THEN 
               A(LN(I),IN(I),JN(I)) = CONST(I)
C               IA=MAX(IA,LN(I))
            ELSEIF(AN(I).EQ.2)  THEN
               B(LN(I),IN(I),JN(I)) = CONST(I)
C               IB=MAX(IB,LN(I))
            ELSEIF(AN(I).EQ.3)  THEN
               C(LN(I),IN(I),JN(I)) = CONST(I)   
C               IC=MAX(IC,LN(I))
            ELSEIF(AN(I).EQ.4) THEN 
               TREND(IN(I),JN(I)) = CONST(I)  
            ENDIF
102      CONTINUE   
      ENDIF
      
      CALL ARMA(EY, HPERR, PRDERR, ERRWT,
     +M,P,IA,IB,IC,NSMPL,NPRED,NACC,U,Y, A,B,C, TREND, 
     + IS,AA,BB,WW, IPIV)
      RETURN
      END

      SUBROUTINE RMAPRJ(PROJ, DSCARD, HORIZ, NHO,
     + EY, M,P,IA,IB,IC,NACC,  U,Y , A,B,C, TREND, 
     + IS,AA,BB,WW, IPIV)

C  multiple calls to ARMA for for prediction at given horizons.
C     See S program horizonForecasts.TSmodel
C
C  Note: If DSCARD is too small then forecasting starts based on little (or 
C          no) data and the results will be spurious.

      INTEGER  DSCARD, NHO, HORIZ(NHO)
      INTEGER M,P, IA, IB, IC, NACC
      INTEGER HPERR

      DOUBLE PRECISION PROJ(NHO,NACC,P)
      DOUBLE PRECISION EY(NACC,P)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M), TREND(NACC,P)
      DOUBLE PRECISION AA(IS,IS), BB(IS,IS), WW(IS)
      DOUBLE PRECISION PRDERR(1,1), ERRWT(1)
      INTEGER IPIV(IS,IS)

      INTEGER I,J, IT, HO, MHORIZ

      HPERR = 0
      MHORIZ = HORIZ(1)
      DO 1 I=2, NHO
 1       MHORIZ=MIN(MHORIZ,HORIZ(I))

      DO 10 IT=DSCARD, (NACC-MHORIZ)
C       this assumes HORIZ is sorted in ascending order
        IF (IT.GT.(NACC-HORIZ(NHO))) NHO = NHO-1

        CALL ARMA(EY, HPERR, PRDERR, ERRWT,                  
     +     M,P,IA,IB,IC,IT,NACC,NACC,  U,Y , A,B,C, TREND, 
     +     IS,AA,BB,WW, IPIV)

        DO 4 HO=1,NHO
            DO 4 J=1,P
4             PROJ(HO,IT+HORIZ(HO),J) = EY(IT+HORIZ(HO),J) 
10    CONTINUE

      RETURN
      END


      SUBROUTINE RMAEPR(COV, DSCARD, HORIZ, NH, NT,
     + EY, M,P,IA,IB,IC,NPRED,NACC,  U,Y , A,B,C, TREND,
     + IS,AA,BB,WW, IPIV)

C  multiple calls to ARMA for prediction analysis.
C     See S program forecastCov
C
C  Note: If DSCARD is too small then forecasting starts based on little (or 
C          no) data and the results will be spurious.

      INTEGER DSCARD, NH, HORIZ(NH), NT(NH)
      INTEGER M,P, IA, IB, IC, NPRED, NACC

      DOUBLE PRECISION COV(NH,P,P)
      DOUBLE PRECISION EY(NPRED,P)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M), TREND(NPRED,P)
      DOUBLE PRECISION AA(IS,IS),BB(IS,IS), WW(IS)
      INTEGER IPIV(IS,IS)


      INTEGER HPERR
      DOUBLE PRECISION PRDERR(1,1), ERRWT(1), MF

      INTEGER I,J, IT, HI

C
C        CALL DBPR('NPRED ',6, NPRED,1)
C        CALL DBPR('HORIZ ',6, HORIZ(1),1)
C        CALL DBPR('DSCARD',7, DSCARD,1)
      HPERR = 0
      DO 1 I=1,NH
1        NT(I) = 0
      DO 2 K=1,NH
         DO 2 I=1,P
            DO 2 J=1,P
2               COV(K,I,J)= 0.0D0

      DO 10 IT=DSCARD, 1+NPRED-HORIZ(1)

        CALL ARMA(EY, HPERR, PRDERR, ERRWT,                  
     +     M,P,IA,IB,IC,IT,NPRED,NACC,  U,Y , A,B,C, TREND, 
     +     IS,AA,BB,WW, IPIV)

C       Eliminate longer horizons as date runs out.
C       This assumes HORIZ is sorted in ascending order.
        IF ((IT-1+HORIZ(NH)).GT.NPRED) THEN
           NH = NH-1
        ENDIF
        DO 4 I=1,NH
           HI = IT-1+HORIZ(I)
           DO 4 J=1,P
4            EY(I,J) = EY(HI,J) - Y(HI,J)
        DO 5 K=1,NH
           NT(K) = NT(K)+1
           MF= DBLE(NT(K)-1)/DBLE(NT(K))
           DO 5 I=1,P
              DO 5 J=1,P
                COV(K,I,J)= COV(K,I,J)*MF + EY(K,I)*EY(K,J)/NT(K)
5     CONTINUE

10    CONTINUE

      RETURN
      END

      SUBROUTINE DATEPR(COV, DSCARD, HORIZ, NH, NT, P, NPRED, ERR)

C     See S program predictions.cov.TSdata
C
      INTEGER DSCARD, NH, HORIZ(NH), NT(NH)
      INTEGER P, NPRED

      DOUBLE PRECISION COV(NH,P,P)
      DOUBLE PRECISION ERR(NPRED,P) 


      INTEGER I,J,K, IT, HI

      DOUBLE PRECISION  MF
C
C        CALL DBPR('NPRED ',6, NPRED,1)
C        CALL DBPR('HORIZ ',6, HORIZ(1),1)
C        CALL DBPR('DSCARD',7, DSCARD,1)

      DO 1 I=1,NH
1        NT(I) = 0
      DO 2 K=1,NH
         DO 2 I=1,P
            DO 2 J=1,P
2               COV(K,I,J)= 0.0D0

      DO 10 IT=DSCARD, 1+NPRED-HORIZ(1)
C       this assumes HORIZ is sorted in ascending order
        IF ((IT-1+HORIZ(NH)).GT.NPRED) THEN
           NH = NH-1
        ENDIF
        DO 5 K=1,NH
           NT(K) = NT(K)+1
           MF= DBLE(NT(K)-1)/DBLE(NT(K))
           HI = IT-1+HORIZ(K)
           DO 5 I=1,P
              DO 5 J=1,P
                COV(K,I,J)= COV(K,I,J)*MF + ERR(HI,I)*ERR(HI,J)/NT(K)
5     CONTINUE
10    CONTINUE
      RETURN
      END

C routines for curvature calculation
C
      SUBROUTINE GENDK(D,ITH,X0,DELTA0,N,ND,F0,RD,HAPROX,HDIAG,
     + DAPROX, X, DELTA,F1,F2,
     + M,P,NSMPL,NACC,  U,Y , 
     + AP,IP,JP,ICT,CONST,AN,IN,JN,
     + NS,Z0,P0,F,G,H,FK,Q,R,GAIN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)
C 
C
C      The function must have a single vector arguement X.
C  X0   the parameter vector.
C  X    is the working copy (altered by DELTA).
C  ITH   is the length of the parameter vector.
C  F0  is the value (in sample/residual space) of the function.
C      (only the space is needed, the function is calculated).
C  N   is the dimension of the sample space (length of F0).
C  DELTA0  gives the fraction of X to use for the initial 
C           numerical approximation.
C  ND  is the number of columns of matrix D.( first
C               der. & lower triangle of Hessian)
C  EPS     is used for zero elements of X.
C  RD       the number of Richardson improvement iterations.
C  V=2       reduction factor for Richardson iterations.
C       V could be a parameter but the way the reduction formula is
C        coded assumes it is =2
C

      INTEGER ITH,N,RD

      DOUBLE PRECISION  X0(ITH),X(ITH), D(N,ND),DELTA0(ITH)
      DOUBLE PRECISION  F0(N), DAPROX(N,RD),HDIAG(N,ITH),HAPROX(N,RD)
      DOUBLE PRECISION DELTA(ITH),F1(N),F2(N)
C      
      INTEGER I,J,K,II,MC,UP
      DOUBLE PRECISION V,MD
C
C  parameters passed directly to ARMAp and/or KFp:
C
      INTEGER HPERR
      INTEGER M,NS,P, NSMPL, NACC
C      INTEGER IA,IB,IC

      DOUBLE PRECISION PRDERR(1,1), ERRWT(1)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
C      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M)
C
C      F, Q, and R are used for scratch space in the call to ARMAP instead of:
C      DOUBLE PRECISION AA(IS,IS), BB(IS,IS), WW(IS)
C        this could cause some problems ... some checks are made.

C       Z0(NS) is used for TREND(P) in ARMA models
C     PARM(ITH) for ARMAP/KFP is X(ITH) in GEND, EY is function value
C   
      INTEGER ICT,IP(ITH),JP(ITH),IN(ICT),JN(ICT)    
C      INTEGER LP(ITH), LN(ICT) 

C      DOUBLE PRECISION Z(NSMPL,NS),TRKERR(NSMPL,NS,NS)
      DOUBLE PRECISION Z0(NS), P0(NS,NS)
      DOUBLE PRECISION F(NS,NS),G(NS,M),H(P,NS)
      DOUBLE PRECISION FK(NS,P),Q(NS,NS),R(P,P)

      INTEGER GAIN         

      DOUBLE PRECISION A(IS,IS),AA(IS,IS),PP(IS,IS),QQ(N,N),RR(P,P)
      DOUBLE PRECISION Z(IS), ZZ(IS), WW(IS)
      INTEGER IPIV(IS,IS)

C..bug in S: passing characters is unreliable
C   use integer for AP and AN...
      INTEGER AP(ITH),AN(ICT)
      DOUBLE PRECISION CONST(ICT)

C      CALL DBPR('starting gend N=',16, N,1)
C      CALL DBPR('            ITH=',16, ITH,1)
C      CALL DBPR('             ND=',16, ND,1)

      HPERR = 0
      V=2.0
      DO 1 II=1,ITH   
1          X(II) =X0(II)
      CALL KFP(F0, HPERR,PRDERR, ERRWT,
     +      M,NS,P,NSMPL,NSMPL,NACC,U,Y, F,G,H,FK, Q,R, GAIN, Z0,P0,
     +      ITH,X,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

C                   each parameter  - first deriv. & hessian diagonal
      DO 100 I=1,ITH
         DO 10 II=1,ITH   
10          DELTA(II) =DELTA0(II)
C                                 successively reduce DELTA 
C
C  This could be done without both X and X0 by adding and then subtracting
C   DELTA, but accumulated round off error seems to affect the result.
         DO 20 K=1,RD 
            X(I)=X0(I)+DELTA(I)
            CALL KFP(F1, HPERR,PRDERR, ERRWT,
     +         M,NS,P,NSMPL,NSMPL,NACC,U,Y, F,G,H,FK, Q,R, GAIN, Z0,P0,
     +         ITH,X,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

            X(I)=X0(I)-DELTA(I) 
            CALL KFP(F2, HPERR,PRDERR, ERRWT,
     +         M,NS,P,NSMPL,NSMPL,NACC,U,Y, F,G,H,FK, Q,R, GAIN, Z0,P0,
     +         ITH,X,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

            X(I)=X0(I) 
            DO 15 II=1,N   
15             DAPROX(II,K) = (F1(II) - F2(II))  / (2.0*DELTA(I))  
            DO 16 II=1,N   
16             HAPROX(II,K) =(F1(II)-2.0*F0(II)+F2(II))/ DELTA(I)**2 
            DELTA(I) = DELTA(I)/V   
20       CONTINUE   
         DO 30 MC=1,(RD-1)
           MD=4.0D0**MC
           DO 30 K=1,(RD-MC)
             DO 25 II=1,N   
25             DAPROX(II,K)=(DAPROX(II,K+1)*MD-DAPROX(II,K))/(MD-1)
             DO 26 II=1,N   
26             HAPROX(II,K)=(HAPROX(II,K+1)*MD-HAPROX(II,K))/(MD-1)
30       CONTINUE 
         DO 31 II=1,N   
31         D(II,I) = DAPROX(II,1)
         DO 32 II=1,N   
32         HDIAG(II,I) = HAPROX(II,1)
100   CONTINUE
C
C                  2nd derivative  - do lower half of hessian only
      UP = ITH
C      CALL DBPR('2nd deriv. UP=\n',16, UP,1)
      DO 200 I=1,ITH   
         DO 200 J=1,I 
            UP = UP + 1
C      CALL DBPR('      UP=\n',11, UP,1)
            IF (I.EQ.J) THEN
               DO 120 II=1,N   
120               D(II,UP) = HDIAG(II,I)
            ELSE 
               DO 121 II=1,ITH   
121               DELTA(II) =DELTA0(II)
C                                successively reduce DELTA 
               DO 150 K=1,RD
                 X(I)=X0(I)+DELTA(I) 
                 X(J)=X0(J)+DELTA(J) 
                 CALL KFP(F1, HPERR,PRDERR, ERRWT,
     +         M,NS,P,NSMPL,NSMPL,NACC,U,Y, F,G,H,FK, Q,R, GAIN, Z0,P0,
     +         ITH,X,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

                 X(I)=X0(I)-DELTA(I) 
                 X(J)=X0(J)-DELTA(J) 
                 CALL KFP(F2, HPERR,PRDERR, ERRWT,
     +         M,NS,P,NSMPL,NSMPL,NACC,U,Y, F,G,H,FK, Q,R, GAIN, Z0,P0,
     +         ITH,X,AP,IP,JP,ICT,CONST,AN,IN,JN,
     + IS, A, AA, PP, QQ, RR, Z, ZZ, WW, IPIV)

                 X(I)=X0(I) 
                 X(J)=X0(J) 
                 DO 130 II=1,N   
130                 DAPROX(II,K)=(F1(II)-2.0*F0(II)+F2(II)
     ,          -HDIAG(II,I)*DELTA(I)**2-HDIAG(II,J)*DELTA(J)**2)/
     ,                    (2.0*DELTA(I)*DELTA(J))   
                 DO 140 II=1,ITH   
140                 DELTA(II) = DELTA(II)/V
150            CONTINUE
               DO 190 MC=1,(RD-1)
                 MD=4.0D0**MC
                 DO 170 K=1,(RD-MC)
                  DO 170 II=1,N   
170                DAPROX(II,K)=
     ,                (DAPROX(II,K+1)*MD-DAPROX(II,K))/(MD-1.0)
                 DO 180 II=1,N   
180                 D(II,UP) = DAPROX(II,1)
190            CONTINUE
            ENDIF  
200   CONTINUE
C      DBPRDB('gend returning D[1,1]=',24, D(1,1),1)
      RETURN
      END

C routines for curvature calculation
C
      SUBROUTINE GENDA(D,ITH,X0,DELTA0,N,ND,F0,RD,HAPROX,HDIAG,
     + DAPROX, X, DELTA,F1,F2,
     + M,P,NSMPL,NACC,  U,Y , 
     + AP,IP,JP,ICT,CONST,AN,IN,JN,
     + LP,LN,IA,IB,IC,A,B,C,TREND,
     + IS,AA,BB,WW, IPIV)
C 
C
C      The function must have a single vector arguement X.
C  X0   the parameter vector.
C  X    is the working copy (altered by DELTA).
C  ITH   is the length of the parameter vector.
C  F0  is the value (in sample/residual space) of the function.
C      (only the space is needed, the function is calculated).
C  N   is the dimension of the sample space (length of F0).
C  DELTA0  gives the fraction of X to use for the initial 
C           numerical approximation.
C  ND  is the number of columns of matrix D.( first
C               der. & lower triangle of Hessian)
C  EPS     is used for zero elements of X.
C  RD       the number of Richardson improvement iterations.
C  V=2       reduction factor for Richardson iterations.
C       V could be a parameter but the way the reduction formula is
C        coded assumes it is =2
C

      INTEGER ITH,N,RD

      DOUBLE PRECISION  X0(ITH),X(ITH), D(N,ND),DELTA0(ITH)
      DOUBLE PRECISION  F0(N), DAPROX(N,RD),HDIAG(N,ITH),HAPROX(N,RD)
      DOUBLE PRECISION DELTA(ITH),F1(N),F2(N)
C      
      INTEGER I,J,K,II,MC,UP
      DOUBLE PRECISION V,MD
C
C  parameters passed directly to ARMAp and/or KFp:
C
      INTEGER HPERR
      INTEGER M,P,NSMPL, NACC
C      INTEGER NS
      INTEGER IA,IB,IC

      DOUBLE PRECISION PRDERR(1,1), ERRWT(1)
      DOUBLE PRECISION Y(NACC,P),U(NACC,M) 
      DOUBLE PRECISION A(IA,P,P),B(IB,P,P),C(IC,P,M)
C
      DOUBLE PRECISION AA(IS,IS), BB(IS,IS), WW(IS)

C     PARM(ITH) for ARMAP/KFP is X(ITH) in GEND, EY is function value
C   
      INTEGER ICT,IP(ITH),JP(ITH),LP(ITH),IN(ICT),JN(ICT),LN(ICT)    

C      DOUBLE PRECISION Z(NSMPL,NS),TRKERR(NSMPL,NS,NS)
      DOUBLE PRECISION TREND(NACC,P)
C      DOUBLE PRECISION P0(NS,NS)
C      DOUBLE PRECISION F(NS,NS),G(NS,M),H(P,NS)
C      DOUBLE PRECISION FK(NS,P),Q(NS,NS),R(P,P)

C      INTEGER GAIN         
C
C..bug in S: passing characters is unreliable
C   use integer for AP and AN...
      INTEGER AP(ITH),AN(ICT)
      DOUBLE PRECISION CONST(ICT)
      INTEGER IPIV(IS,IS)

C      CALL DBPR('starting gend N=',16, N,1)
C      CALL DBPR('            ITH=',16, ITH,1)
C      CALL DBPR('             ND=',16, ND,1)


      HPERR = 0
      V=2.0
      DO 1 II=1,ITH   
1          X(II) =X0(II)
      CALL ARMAP(F0, HPERR,PRDERR, ERRWT,
     +      M,P,IA,IB,IC,NSMPL,NSMPL,NACC,  U,Y , A,B,C, TREND,
     +      ITH,X,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     +      IS,AA,BB,WW, IPIV)

C                   each parameter  - first deriv. & hessian diagonal
      DO 100 I=1,ITH
         DO 10 II=1,ITH   
10          DELTA(II) =DELTA0(II)
C                                 successively reduce DELTA 
C
C  This could be done without both X and X0 by adding and then subtracting
C   DELTA, but accumulated round off error seems to affect the result.
         DO 20 K=1,RD 
            X(I)=X0(I)+DELTA(I)
            CALL ARMAP(F1, HPERR,PRDERR, ERRWT,
     +         M,P,IA,IB,IC,NSMPL,NSMPL,NACC,  U,Y , A,B,C, TREND, 
     +         ITH,X,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     +         IS,AA,BB,WW, IPIV)  
            
            X(I)=X0(I)-DELTA(I) 
            CALL ARMAP(F2, HPERR,PRDERR, ERRWT,
     +         M,P,IA,IB,IC,NSMPL,NSMPL,NACC,  U,Y , A,B,C, TREND,
     +         ITH,X,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     +         IS,AA,BB,WW, IPIV)
            
            X(I)=X0(I) 
            DO 15 II=1,N   
15             DAPROX(II,K) = (F1(II) - F2(II))  / (2.0*DELTA(I))  
            DO 16 II=1,N   
16             HAPROX(II,K) =(F1(II)-2.0*F0(II)+F2(II))/ DELTA(I)**2 
            DELTA(I) = DELTA(I)/V   
20       CONTINUE   
         DO 30 MC=1,(RD-1)
           MD=4.0D0**MC
           DO 30 K=1,(RD-MC)
             DO 25 II=1,N   
25             DAPROX(II,K)=(DAPROX(II,K+1)*MD-DAPROX(II,K))/(MD-1)
             DO 26 II=1,N   
26             HAPROX(II,K)=(HAPROX(II,K+1)*MD-HAPROX(II,K))/(MD-1)
30       CONTINUE 
         DO 31 II=1,N   
31         D(II,I) = DAPROX(II,1)
         DO 32 II=1,N   
32         HDIAG(II,I) = HAPROX(II,1)
100   CONTINUE
C
C                  2nd derivative  - do lower half of hessian only
      UP = ITH
C      CALL DBPR('2nd deriv. UP=\n',16, UP,1)
      DO 200 I=1,ITH   
         DO 200 J=1,I 
            UP = UP + 1
C      CALL DBPR('      UP=\n',11, UP,1)
            IF (I.EQ.J) THEN
               DO 120 II=1,N   
120               D(II,UP) = HDIAG(II,I)
            ELSE 
               DO 121 II=1,ITH   
121               DELTA(II) =DELTA0(II)
C                                successively reduce DELTA 
               DO 150 K=1,RD
                 X(I)=X0(I)+DELTA(I) 
                 X(J)=X0(J)+DELTA(J) 
                 CALL ARMAP(F1, HPERR,PRDERR, ERRWT,
     +              M,P,IA,IB,IC,NSMPL,NSMPL,NACC, U,Y, A,B,C, TREND,
     +              ITH,X,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     +              IS,AA,BB,WW, IPIV)
                 
                 X(I)=X0(I)-DELTA(I) 
                 X(J)=X0(J)-DELTA(J) 
                 CALL ARMAP(F2, HPERR,PRDERR, ERRWT,
     +              M,P,IA,IB,IC,NSMPL,NSMPL,NACC, U,Y, A,B,C, TREND,
     +              ITH,X,AP,LP,IP,JP,ICT,CONST,AN,LN,IN,JN, 
     +              IS,AA,BB,WW, IPIV)
                 
                 X(I)=X0(I) 
                 X(J)=X0(J) 
                 DO 130 II=1,N   
130                 DAPROX(II,K)=(F1(II)-2.0*F0(II)+F2(II)
     ,          -HDIAG(II,I)*DELTA(I)**2-HDIAG(II,J)*DELTA(J)**2)/
     ,                    (2.0*DELTA(I)*DELTA(J))   
                 DO 140 II=1,ITH   
140                 DELTA(II) = DELTA(II)/V
150            CONTINUE
               DO 190 MC=1,(RD-1)
                 MD=4.0D0**MC
                 DO 170 K=1,(RD-MC)
                  DO 170 II=1,N   
170                DAPROX(II,K)=
     ,                (DAPROX(II,K+1)*MD-DAPROX(II,K))/(MD-1.0)
                 DO 180 II=1,N   
180                 D(II,UP) = DAPROX(II,1)
190            CONTINUE
            ENDIF  
200   CONTINUE
C      DBPRDB('gend returning D[1,1]=',24, D(1,1),1)
      RETURN
      END
