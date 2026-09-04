c
c  growth : A Library of Normal Distribution Growth Curve Models
c  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
c
c  This program is free software; you can redistribute it and/or modify
c  it under the terms of the GNU General Public License as published by
c  the Free Software Foundation; either version 2 of the License, or
c  (at your option) any later version.
c
c  This program is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c  GNU General Public License for more details.
c
c  You should have received a copy of the GNU General Public License
c  along with this program; if not, write to the Free Software
c  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
c
c  SYNOPSIS
c
c     subroutine kalman(np,par,like,xx,y,sse,nq,nlp,ns,nt1,model,
c    +t,nobs,nod,in,cv,ncv,nxcv,nx,p,x,state,innov,cstate,exx)
c
c  DESCRIPTION
c
c    Function to compute the likelihood function for the multivariate
c normal distribution with ARMA and random intercept using Kalman
c filtering
c
c
c Polynomial growth curves with continuous time ARMA within subject
c errors, the KALMAN FILTER method.    May 17, 1990
c
c Ref: Jones, R. H. and Ackerson, L. M. (1990) Serial correlation in
c      unequally spaced longitudinal data, Biometrika.
c
c Richard H. Jones
c Department of Preventive Medicine and Biometrics
c School of Medicine, Box B-119
c University of Colorado Health Sciences Center
c Denver, CO 80262
c U.S.A.
c (303) 270-6860
c modified by J.K. Lindsey, May 1991 and January, 1997
c
c NQ, number of random effects
c NOD, number of distinct elements in random effects covariance matrix
c      1 < NOD < NQ*(NQ+1)/2.  These elements are entered as the upper
c      triangular factorization of the covariance matrix.
c NR, maximum number of nonlinear parameters,
c     MODEL(1)+MODEL(2)+MODEL(3)+NOD
c NCV, number of covariates or grouping variables
c NLP, number of linear parameters
c ND must be at least NLP+1
c NS, number of subjects
c
c NOTE: if these parameter values are changed, they must also be
c       changed in the two subroutines.
c
c      implicit none
c      CALL KALMAN(NP,P,LIKE)
c      CALL BACK(XX,NLP,ND,NLP1)
c      CALL TTVERT(XX,NLP,ND,IER)
c         CALL FACTOR(TESNUM,NCONT,ND,NCONT+1,IER)
c         CALL OPTOUT(NR,NP,KALMAN,IPR,XPLS,FPLS,AA,STEP,F,NDIGIT,TYPX)
c      CALL RESID(NP,P,BETA,AVE,PRED,RES)
C

      SUBROUTINE RESID(NP,PAR,BETA,AVE,PRED,SDR,RES,Y,SSE,NQ,NLP,NS,
     +NT,MODEL,T,NOBS,NOD,IN,CV,NXCV,NX,NCV,P,X)
c
      IMPLICIT NONE
      INTEGER MAXRE,MAXAR
      PARAMETER(MAXRE=6,MAXAR=6)
c
      INTEGER NQ,NLP,NS,NT,NOD,NX,NCV,NP
      INTEGER NAR,NMA,NLP1,IND,I,J,L,N1,II,JJ,KK,LL
      DOUBLE PRECISION SSE,V,VEC(MAXRE),PMA(MAXAR-1)
      DOUBLE PRECISION P(NP),P22(MAXRE,MAXRE),VARI
      double precision PAR(NP),T(NT),Y(NT),CV(NCV,NS),
     +PRED(NT),SDR(NT),RES(NT),BETA(NLP),AVE,X(NLP+1)
      REAL MSE,STATE(MAXRE),INNOV,D(MAXRE,MAXRE),U(MAXRE,MAXRE)
      REAL Z(MAXRE),SSD(MAXRE),DT,SD,R
      INTEGER MODEL(3),NOBS(NS),IN(NOD,2),NXCV(NCV)
      DOUBLE COMPLEX RR(MAXAR),UU(MAXAR,MAXAR),UI(MAXAR,MAXAR)
      DOUBLE COMPLEX P12(MAXAR,MAXRE),EXX,DC(MAXAR),HTEMP(MAXAR)
      DOUBLE COMPLEX H(MAXAR),HP(MAXAR),K(MAXAR),EX(MAXAR)
      DOUBLE COMPLEX P110(MAXAR,MAXAR),P11(MAXAR,MAXAR)
      DOUBLE COMPLEX CSTATE(MAXAR)
c      CHARACTER*32 FILNAM
C
      NAR=MODEL(1)
      NMA=MODEL(2)
      X(1)=1.0
      Z(1)=1.0
      MSE=SSE/(NT-NLP-NP)
      SD=SQRT(MSE)
      NLP1=NLP+1
      IND=NAR+NMA+MODEL(3)
      R=0.0
      IF(MODEL(3).EQ.1)THEN
         R=MSE*PAR(IND)**2
      ENDIF
      IF(NAR.GE.1)THEN
         DO 15 I=1,NAR
            P(I)=PAR(I)
   15    CONTINUE
         CALL ROOTS(NAR,P,RR)
         CALL TRANS(NAR,RR,UU)
         DO 25 I=1,NAR
            DO 20 J=1,NAR
               UI(I,J)=UU(I,J)
   20       CONTINUE
   25    CONTINUE
         CALL CVERT(NAR,UI)
         CALL INIT(NAR,RR,UI,P110,VARI)
      ENDIF
      HTEMP(1)=(1.0,0.0)
      H(1)=(1.0,0.0)
c      IF (NAR.GT.1) THEN
         DO 30 I=2,NAR
            HTEMP(I)=(0.0,0.0)
   30    CONTINUE
c      ENDIF
      IF(NMA.GT.0)THEN
         CALL UNMA(NAR,NMA,PAR,PMA)
         DO 35 I=1,NMA
            HTEMP(I+1)=PMA(I)
   35    CONTINUE
      ENDIF
c      IF (NAR.GE.1) THEN
         DO 45 I=1,NAR
            H(I)=(0.0,0.0)
            DO 40 J=1,NAR
               H(I)=H(I)+HTEMP(J)*UU(J,I)
   40       CONTINUE
   45    CONTINUE
c      ENDIF
C
C Initialize D matrix
C
      IF(NOD.GT.0)THEN
         DO 55 I=1,NQ
            DO 50 L=1,NQ
               U(I,L)=0.0
   50       CONTINUE
   55    CONTINUE
      ENDIF
C
C Construct an upper triangular matrix U.  The actual initial input of
c the matrix D will be U'U. This will ensure positive definiteness of
c the matrix D.
C
c      IF (NOD.GE.1) THEN
        DO 60 I=1,NOD
           IND=IND+1
           U(IN(I,1),IN(I,2))=SD*PAR(IND)
   60   CONTINUE
c      ENDIF
      IF(NOD.GT.0)THEN
         DO 75 J=1,NQ
            DO 70 I=1,J
               D(I,J)=0.0
               DO 65 L=1,I
                  D(I,J)=D(I,J)+U(L,I)*U(L,J)
   65          CONTINUE
   70       CONTINUE
   75    CONTINUE
      ENDIF
C
C Start subject loop
C
      NT=0
      IF (NAR.GE.1) THEN
         N1=NAR
      ELSE
         N1=1
      ENDIF
      DO 2000 JJ=1,NS
C
C Initialize State vector and covariance matrix
C
         DO 80 I=1,N1
            CSTATE(I)=(0.0,0.0)
   80    CONTINUE
         IF(NOD.GT.0)THEN
            DO 83 I=1,NQ
               STATE(I)=0.0
   83       CONTINUE
         ENDIF
         IF(NAR.EQ.0)THEN
            P11(1,1)=MSE
         ELSE
            DO 87 I=1,N1
               DO 86 J=1,N1
                  P11(I,J)=MSE*P110(I,J)
   86          CONTINUE
c               p11(i,i)=dreal(p11(i,i)) changed JKL
               P11(I,I)=DBLE(P11(I,I))
   87       CONTINUE
         ENDIF
         IF(NOD.GT.0)THEN
            DO 95 I=1,N1
               DO 90 J=1,NQ
                  P12(I,J)=(0.0,0.0)
   90          CONTINUE
   95       CONTINUE
            DO 105 I=1,NQ
               DO 100 J=I,NQ
                  P22(I,J)=D(I,J)
                  P22(J,I)=P22(I,J)
  100          CONTINUE
  105       CONTINUE
         ENDIF
C
C Start within subject loop.
C
         DO 1000 LL=1,NOBS(JJ)
c            IF(NQ.GE.2)THEN
               DO 110 J=2,NQ
                  Z(J)=(T(NT+LL)-T(NT+1))**(J-1)
  110          CONTINUE
c            ENDIF
c            IF(NX.GE.2)THEN
               DO 115 J=2,NX
                  X(J)=T(NT+LL)**(J-1)
  115          CONTINUE
c            ENDIF
c            IF(NCV.GE.1)THEN
               II=NX
               DO 125 KK=1,NCV
c                  IF(NXCV(KK).GE.1)THEN
                     DO 120 J=1,NXCV(KK)
                        II=II+1
                        X(II)=X(J)*CV(KK,JJ)
 120                 CONTINUE
c                  ENDIF
 125           CONTINUE
c            ENDIF
            X(NLP1)=Y(NT+LL)
C
C Predict state vector from time LL-1 to time LL
C and calculate one step prediction of the covariance matrix
C
            EXX=(0.0,0.0)
            IF(NAR.EQ.0) THEN
               CSTATE(1)=(0.0,0.0)
               P11(1,1)=MSE*(1.0,0.0)
               IF(NOD.GT.0)THEN
                  DO 135 I=1,NQ
                     P12(1,I)=(0.0,0.0)
  135             CONTINUE
               ENDIF
            ELSE
               IF(LL.EQ.1) THEN
                  DT=0
               ELSE
                  DT=T(NT+LL)-T(NT+LL-1)
               END IF
               DO 150 I=1,NAR
c                  dc(i)=cdexp(rr(i)*dt) changed JKL
                  DC(I)=CEXP(cmplx(RR(I)*DT))
                  CSTATE(I)=CSTATE(I)*DC(I)
                  EXX=EXX+CSTATE(I)*H(I)
  150          CONTINUE
               DO 165 I=1,NAR
                  DO 160 J=I,NAR
                     p11(i,j)=dc(i)*p11(i,j)*dconjg(dc(j))-(ui(i,nar)
     +               *dconjg(ui(j,nar))/(rr(i)+dconjg(rr(j))))*mse*
     +               ((1.0,0.0)-dc(i)*dconjg(dc(j)))/vari
                     p11(j,i)=dconjg(p11(i,j))
  160             CONTINUE
c                  p11(i,i)=dreal(p11(i,i)) changed JKL
                  P11(I,I)=DBLE(P11(I,I))
  165          CONTINUE
               IF(NOD.GT.0)THEN
                  DO 175 I=1,NAR
                     DO 170 J=1,NQ
                        P12(I,J)=DC(I)*P12(I,J)
  170                CONTINUE
  175             CONTINUE
               ENDIF
            ENDIF
c
c Calculate residuals
c
            PRED(NT+LL)=EXX
            DO 801 I=1,NLP
               PRED(NT+LL)=PRED(NT+LL)+BETA(I)*X(I)
  801       CONTINUE
C
C Calculate innovations
C 
            IF(NOD.GT.0)THEN
               DO 180 J=1,NQ
                  PRED(NT+LL)=PRED(NT+LL)+Z(J)*STATE(J)
  180          CONTINUE
            ENDIF
            INNOV=X(NLP1)-PRED(NT+LL)
C
C Calculate innovation variance
C
         IF(NOD.GT.0)THEN
            DO 195 I=1,N1
               EX(I)=(0.0,0.0)
               DO 190 J=1,NQ
                  EX(I)=EX(I)+P12(I,J)*Z(J)
  190          CONTINUE
  195       CONTINUE
            DO 205 I=1,NQ
               VEC(I)=0.0
               DO 200 J=1,NQ
                  VEC(I)=VEC(I)+P22(I,J)*Z(J)
  200          CONTINUE
  205       CONTINUE
         ENDIF
         DO 215 I=1,N1
            HP(I)=(0.0,0.0)
            DO 210 J=1,N1
               HP(I)=HP(I)+H(J)*P11(J,I)
  210       CONTINUE
  215    CONTINUE
c
c add observational error R
c
         V=R
         DO 220 I=1,N1
            v=v+hp(i)*dconjg(h(i))
            IF(NOD.GT.0)THEN
c               v=v+(2.0,0.0)*dreal(h(i)*ex(i)) changed JKL
               V=V+(2.0,0.0)*DBLE(H(I)*EX(I))
            ENDIF
  220    CONTINUE
         IF(NOD.GT.0)THEN
            DO 225 I=1,NQ
               V=V+Z(I)*VEC(I)
  225       CONTINUE
         ENDIF
         SDR(NT+LL)=DSQRT(V)
         RES(NT+LL)=INNOV/SDR(NT+LL)
C
C Calculate Kalman gain
C
            DO 245 I=1,N1
               K(I)=(0.0,0.0)
               DO 240 J=1,N1
                  k(i)=k(i)+p11(i,j)*dconjg(h(j))
  240          CONTINUE
               IF(NOD.GT.0)THEN
                  K(I)=(K(I)+EX(I))
               ENDIF
  245       CONTINUE
            IF(NOD.GT.0)THEN
               DO 255 I=1,NQ
                  K(N1+I)=(0.0,0.0)
                  DO 250 J=1,N1
                     k(i+n1)=k(i+n1)+dconjg(p12(j,i))*dconjg(h(j))
  250             CONTINUE
                  K(I+N1)=(K(I+N1)+VEC(I))
  255          CONTINUE
            ENDIF
C
C Update state vector
C
            DO 265 I=1,N1
               CSTATE(I)=CSTATE(I)+K(I)*INNOV/V
  265       CONTINUE
            IF(NOD.GT.0)THEN
               DO 275 I=1,NQ
                  STATE(I)=STATE(I)+K(N1+I)*INNOV/V
  275          CONTINUE
            ENDIF
C
C Update state covariance matrix
C
            DO 310 I=1,N1
               DO 305 J=I,N1
                  p11(i,j)=p11(i,j)-k(i)*dconjg(k(j))/v
                  p11(j,i)=dconjg(p11(i,j))
  305          CONTINUE
c               p11(i,i)=dreal(p11(i,i)) changed JKL
               P11(I,I)=DBLE(P11(I,I))
  310       CONTINUE
            IF(NOD.GT.0)THEN
               DO 320 I=1,N1
                  DO 315 J=1,NQ
                     p12(i,j)=p12(i,j)-k(i)*dconjg(k(j+n1))/v
  315             CONTINUE
  320          CONTINUE
               DO 330 I=1,NQ
                  DO 325 J=I,NQ
                     p22(i,j)=p22(i,j)-k(i+n1)*dconjg(k(j+n1))/v
                     P22(J,I)=P22(I,J)
  325             CONTINUE
  330          CONTINUE
            ENDIF
 1000    CONTINUE
c         IF(IRES.EQ.1)THEN
            DO 410 LL=1,NOBS(JJ)
               T(NT+LL)=T(NT+LL)+AVE
  410       CONTINUE
c         ENDIF
         IF(NQ.GE.1)THEN
            DO 915 I=1,NQ
               SSD(I)=DSQRT(P22(I,I))
  915       CONTINUE
         ENDIF
         NT=NT+NOBS(JJ)
 2000 CONTINUE
      RETURN
      END
c
      SUBROUTINE ROOTS(N,P,R)

      IMPLICIT NONE

      INTEGER I,N
      DOUBLE PRECISION P(N),RE,A,FAC,ROOT
      DOUBLE COMPLEX R(N)
      DO 10 I=1,N,2
         IF(I.LT.N)THEN
            RE=DEXP(P(I+1))/2.0D0
            A=DEXP(P(I))
            FAC=RE*RE-A
            ROOT=DSQRT(DABS(FAC))
            IF(FAC.LT.0.0)THEN
               r(i)=dcmplx(-re,-root)
               r(i+1)=dcmplx(-re,root)
            ELSE
               R(I)=-RE-ROOT
               R(I+1)=-RE+ROOT
            END IF
c     lines added JKL
            if(dble(r(i)).eq.0.)r(i)=1e-10
            if(dble(r(i+1)).eq.0.)r(i+1)=1e-10
         ELSE
            R(I)=-DEXP(P(I))
c     line added JKL
            if(dble(r(i)).eq.0.)r(i)=1e-10
         END IF
   10 CONTINUE
      RETURN
      END
c
      SUBROUTINE TRANS(N,R,U)
      IMPLICIT NONE

      INTEGER MAXAR
      PARAMETER(MAXAR=6)

      INTEGER I,J,N
      DOUBLE COMPLEX R(N),U(MAXAR,MAXAR)
      DO 20 J=1,N
         U(1,J)=(1.0,0.0)
c         if(n.gt.1)then   two lines eliminated JKL
            DO 10 I=2,N
               U(I,J)=U(I-1,J)*R(J)
   10       CONTINUE
c         end if
   20 CONTINUE
      RETURN
      END
c
      SUBROUTINE INIT(N,R,UI,COV,VARI)

      IMPLICIT NONE
      INTEGER MAXAR
      PARAMETER(MAXAR=6)

      INTEGER I,J,K,L,N
      DOUBLE PRECISION RCOV(MAXAR,MAXAR),VARI
      DOUBLE COMPLEX R(N),UI(MAXAR,MAXAR),RES(MAXAR),COV(MAXAR,MAXAR)
      DOUBLE COMPLEX EX
c  first calculate the residues of the poles
      DO 20 J=1,N
c         RES(J)=-2.0*REAL(R(J))  changed JKL
         RES(J)=-2.0*dble(R(J))
         DO 10 K=1,N
            if(k.ne.j)res(j)=res(j)*(r(k)-r(j))*(dconjg(r(k))+r(j))
   10    CONTINUE
   20 CONTINUE
c Calculate real covariance matrix of state vector
c Scale so upper left hand corner, the variance of the process, is 1
      DO 40 K=1,N
         DO 35 L=K,N
            EX=(0.0,0.0)
            DO 30 J=1,N
               EX=EX+(R(J)**(K-1))*((-R(J))**(L-1))/RES(J)
   30       CONTINUE
            RCOV(K,L)=EX
            IF(L.EQ.1)VARI=EX
            RCOV(K,L)=RCOV(K,L)/VARI
            RCOV(L,K)=RCOV(K,L)
   35    CONTINUE
   40 CONTINUE
c Rotate to get complex covariance matrix of rotated state
      DO 80 I=1,N
         DO 70 L=I,N
            COV(I,L)=(0.0,0.0)
            DO 60 J=1,N
               DO 50 K=1,N
                  cov(i,l)=cov(i,l)+ui(i,j)*rcov(j,k)*dconjg(ui(l,k))
   50          CONTINUE
   60       CONTINUE
            cov(l,i)=dconjg(cov(i,l))
   70    CONTINUE
   80 CONTINUE
      RETURN
      END
c
      SUBROUTINE CVERT(N,UI)

      IMPLICIT NONE
      INTEGER MAXAR
      PARAMETER(MAXAR=6)

      INTEGER I,J,K,N
      DOUBLE PRECISION A
      DOUBLE COMPLEX UI(MAXAR,MAXAR),T
c
c  inverts a N by N complex matrix
c  first dimensions of ui in calling program must be MAXAR
c
      DO 40 I=1,N
c         a=cdabs(ui(i,i)) changed JKL
         A=CABS(cmplx(UI(I,I)))
            T=(1.0,0.0)/UI(I,I)
            UI(I,I)=(1.0,0.0)
            DO 10 J=1,N
               UI(I,J)=T*UI(I,J)
   10       CONTINUE
            DO 30 K=1,N
               IF(I.NE.K)THEN
                  T=UI(K,I)
                  UI(K,I)=(0.0,0.0)
                  DO 20 J=1,N
                     UI(K,J)=UI(K,J)-T*UI(I,J)
   20             CONTINUE
               END IF
   30       CONTINUE
   40 CONTINUE
      RETURN
      END
c
      SUBROUTINE UNMA(NAR,NMA,P,PMA)

      IMPLICIT NONE
      INTEGER MAXAR,MAXMA
      PARAMETER(MAXAR=6,MAXMA=5)

      INTEGER I,J,NAR,NMA
      double precision P(NAR+NMA)
      DOUBLE PRECISION PMA(NMA),TEMP(MAXAR+MAXMA)
c
c This subroutine untransforms the moving average coefficients.  The
c input coefficients are squared to give nonnegative coefficients that
c are coefficients in quadratic factors with a terminating linear
c factor if NMA is odd.  The polynomial factors are:
c
c (1 + (p(nar+1)**2)*z + (p(nar+2)**2)*z**2)
c (1 + (p(nar+3)**2)*z + (p(nar+4)**2)*z**2)
c    .
c    .
c    .
c (1 + (p(nar+nma-1)**2)*z + (p(nar+nma)**2)*z**2) if NMA is even
c (1 + (p(nar+nma)**2)*z)                          if NMA is odd
c
c The NMA coefficients of the polynomial obtained by multiplying the
c factors are calculated and stored in PMA
c
      DO 10 I=1,NMA
         PMA(I)=P(NAR+I)**2
   10 CONTINUE
      IF(NMA.LE.2)RETURN
      DO 50 I=3,NMA,2
         TEMP(1)=PMA(I)
         DO 20 J=2,I
            TEMP(J)=PMA(J-1)*PMA(I)
   20    CONTINUE
         IF(I.LT.NMA)THEN
            TEMP(2)=TEMP(2)+PMA(I+1)
            DO 30 J=3,I
               TEMP(J)=TEMP(J)+PMA(J-2)*PMA(I+1)
   30       CONTINUE
            PMA(I+1)=PMA(I-1)*PMA(I+1)
         ENDIF
         DO 40 J=1,I-1
            PMA(J)=PMA(J)+TEMP(J)
   40    CONTINUE
         PMA(I)=TEMP(I)
   50 CONTINUE
      RETURN
      END
c----------------------------------------------------------------------
      SUBROUTINE KALMAN(NP,PAR,LIKE,XX,Y,SSE,NQ,NLP,NS,NT1,MODEL,
     +T,NOBS,NOD,IN,CV,NCV,NXCV,NX,P,X,STATE,INNOV,CSTATE,EXX)
c
      IMPLICIT NONE
      INTEGER MAXRE,MAXAR
      PARAMETER(MAXRE=6,MAXAR=6)
c
      INTEGER NQ,NLP,NS,NT,NOD,NX,NCV,NP,NT1
      INTEGER I,J,L,II,JJ,KK,LL,NAR,NMA,NLP1,IND,N1,IER
      DOUBLE PRECISION XX(NLP+1,NLP+1),LIKE,SSE,DET,V,DEN,VEC(MAXRE)
      DOUBLE PRECISION P(NP),P22(MAXRE,MAXRE),VARI,PMA(MAXAR-1)
      double precision PAR(NP),T(NT1),Y(NT1),CV(NCV,NS),X(NLP+1),
     + STATE(MAXRE,NLP+1),INNOV(NLP+1)
      DOUBLE COMPLEX CSTATE(MAXAR,NLP+1),EXX(NLP+1)
      REAL D(MAXRE,MAXRE),U(MAXRE,MAXRE),Z(MAXRE),R,DT
      DOUBLE COMPLEX RR(MAXAR),UU(MAXAR,MAXAR),UI(MAXAR,MAXAR)
      DOUBLE COMPLEX P12(MAXAR,MAXRE),DC(MAXAR),HTEMP(MAXAR)
      DOUBLE COMPLEX H(MAXAR),HP(MAXAR),K(MAXAR)
      DOUBLE COMPLEX P110(MAXAR,MAXAR),P11(MAXAR,MAXAR)
      INTEGER MODEL(3),NOBS(NS),IN(NOD,2),NXCV(NCV)
C
C Initialize the determinant term in the likelihood
C and [X'X:X'Y] term.
C
      NAR=MODEL(1)
      NMA=MODEL(2)
      X(1)=1.0
      Z(1)=1.0
      NLP1=NLP+1
      DET=0.0
      DO 10 J=1,NLP1
         DO 5 I=1,J
            XX(I,J)=0.0
    5    CONTINUE
   10 CONTINUE
      IND=NAR+NMA+MODEL(3)
      R=0.0
      IF(MODEL(3).EQ.1)THEN
         R=PAR(IND)**2
      ENDIF
      IF(NAR.GE.1)THEN
         DO 15 I=1,NAR
            P(I)=PAR(I)
   15    CONTINUE
         CALL ROOTS(NAR,P,RR)
         CALL TRANS(NAR,RR,UU)
         DO 25 I=1,NAR
            DO 20 J=1,NAR
               UI(I,J)=UU(I,J)
   20       CONTINUE
   25    CONTINUE
         CALL CVERT(NAR,UI)
         CALL INIT(NAR,RR,UI,P110,VARI)
      ENDIF
      HTEMP(1)=(1.0,0.0)
      H(1)=(1.0,0.0)
c      IF (NAR.GT.1) THEN
         DO 30 I=2,NAR
            HTEMP(I)=(0.0,0.0)
   30    CONTINUE
c      ENDIF
      IF(NMA.GT.0)THEN
         CALL UNMA(NAR,NMA,PAR,PMA)
         DO 35 I=1,NMA
            HTEMP(I+1)=PMA(I)
   35    CONTINUE
      ENDIF
c      IF (NAR.GE.1) THEN
         DO 45 I=1,NAR
            H(I)=(0.0,0.0)
            DO 40 J=1,NAR
               H(I)=H(I)+HTEMP(J)*UU(J,I)
   40       CONTINUE
   45    CONTINUE
c      ENDIF
C
C Initialize D matrix
C
      IF(NOD.GT.0)THEN
         DO 55 I=1,NQ
            DO 50 L=1,NQ
               U(I,L)=0.0
   50       CONTINUE
   55    CONTINUE
      ENDIF
C
C Construct an upper triangular matrix U.  The actual initial input of
c the matrix D will be U'U. This will ensure positive definiteness of
c the matrix D.
C
c      IF (NOD.GE.1) THEN
        DO 60 I=1,NOD
           IND=IND+1
           U(IN(I,1),IN(I,2))=PAR(IND)
   60   CONTINUE
c      ENDIF
      IF(NOD.GT.0)THEN
         DO 75 J=1,NQ
            DO 70 I=1,J
               D(I,J)=0.0
               DO 65 L=1,I
                  D(I,J)=D(I,J)+U(L,I)*U(L,J)
   65          CONTINUE
   70       CONTINUE
   75    CONTINUE
      ENDIF
C
C Start subject loop
C
      NT=0
      IF (NAR.GE.1) THEN
         N1=NAR
      ELSE
         N1=1
      ENDIF
      DO 2000 JJ=1,NS
C
C Initialize State vector and covariance matrix
C
         DO 85 J=1,NLP1
            DO 80 I=1,N1
               CSTATE(I,J)=(0.0,0.0)
   80       CONTINUE
            IF(NOD.GT.0)THEN
               DO 83 I=1,NQ
                  STATE(I,J)=0.0
   83          CONTINUE
            ENDIF
   85    CONTINUE
         IF(NAR.EQ.0)THEN
            P11(1,1)=(1.0D0,0.0D0)
         ELSE
            DO 87 I=1,N1
               DO 86 J=1,N1
                  P11(I,J)=P110(I,J)
   86          CONTINUE
c               p11(i,i)=dreal(p110(i,i))  changed JKL
               P11(I,I)=DBLE(P110(I,I))
   87       CONTINUE
         ENDIF
         IF(NOD.GT.0)THEN
            DO 95 I=1,N1
               DO 90 J=1,NQ
                  P12(I,J)=(0.0,0.0)
   90          CONTINUE
   95       CONTINUE
            DO 105 I=1,NQ
               DO 100 J=I,NQ
                  P22(I,J)=D(I,J)
                  P22(J,I)=P22(I,J)
  100          CONTINUE
  105       CONTINUE
         ENDIF
C
C Start within subject loop.
C
         DO 1000 LL=1,NOBS(JJ)
c            IF(NQ.GE.2)THEN
               DO 110 J=2,NQ
                  Z(J)=(T(NT+LL)-T(NT+1))**(J-1)
  110          CONTINUE
c            ENDIF
c            IF(NX.GE.2)THEN
               DO 115 J=2,NX
                  X(J)=T(NT+LL)**(J-1)
  115          CONTINUE
c            ENDIF
c            IF(NCV.GE.1)THEN
               II=NX
               DO 125 KK=1,NCV
c                  IF(NXCV(KK).GE.1)THEN
                     DO 120 J=1,NXCV(KK)
                        II=II+1
                        X(II)=X(J)*CV(KK,JJ)
 120                 CONTINUE
c                  ENDIF
 125           CONTINUE
c            ENDIF
            X(NLP1)=Y(NT+LL)
C
C Predict state vector from time LL-1 to time LL
C and calculate one step prediction of the covariance matrix
C
               DO 140 I=1,NLP1
                  EXX(I)=(0.0,0.0)
  140          CONTINUE
            IF(NAR.EQ.0) THEN
               DO 130 I=1,NLP1
                  CSTATE(1,I)=(0.0,0.0)
  130          CONTINUE
               P11(1,1)=(1.0,0.0)
               IF(NOD.GT.0)THEN
                  DO 135 I=1,NQ
                     P12(1,I)=(0.0,0.0)
  135             CONTINUE
               ENDIF
            ELSE
               IF(LL.EQ.1) THEN
                  DT=0
               ELSE
                  DT=T(NT+LL)-T(NT+LL-1)
               END IF
c  next 3 lines moved up out of IF block  JKL
c               DO 140 I=1,NLP1
c                  EXX(I)=(0.0,0.0)
c  140          CONTINUE
               DO 150 I=1,NAR
c                  dc(i)=cdexp(rr(i)*dt) changed JKL
                  DC(I)=CEXP(cmplx(RR(I)*DT))
                  DO 145 J=1,NLP1
                     CSTATE(I,J)=CSTATE(I,J)*DC(I)
                     EXX(J)=EXX(J)+CSTATE(I,J)*H(I)
  145             CONTINUE
  150          CONTINUE
               DO 165 I=1,NAR
                  DO 160 J=I,NAR
                     p11(i,j)=dc(i)*p11(i,j)*dconjg(dc(j))-(ui(i,nar)
     +               *dconjg(ui(j,nar))/(rr(i)+dconjg(rr(j))))*
     +               ((1.0,0.0)-dc(i)*dconjg(dc(j)))/vari
                     p11(j,i)=dconjg(p11(i,j))
  160             CONTINUE
c                  p11(i,i)=dreal(p11(i,i)) changed JKL
                  P11(I,I)=DBLE(P11(I,I))
  165          CONTINUE
               IF(NOD.GT.0)THEN
                  DO 175 I=1,NAR
                     DO 170 J=1,NQ
                        P12(I,J)=DC(I)*P12(I,J)
  170                CONTINUE
  175             CONTINUE
               ENDIF
            ENDIF
C
C Calculate innovations
C 
            DO 185 I=1,NLP1
               INNOV(I)=X(I)-EXX(I)
               IF(NOD.GT.0)THEN
                  DO 180 J=1,NQ
                     INNOV(I)=INNOV(I)-Z(J)*STATE(J,I)
  180             CONTINUE
               ENDIF
  185       CONTINUE
C
C Calculate innovation variance
C
         IF(NOD.GT.0)THEN
            DO 195 I=1,N1
               EXX(I)=(0.0,0.0)
               DO 190 J=1,NQ
                  EXX(I)=EXX(I)+P12(I,J)*Z(J)
  190          CONTINUE
  195       CONTINUE
            DO 205 I=1,NQ
               VEC(I)=0.0
               DO 200 J=1,NQ
                  VEC(I)=VEC(I)+P22(I,J)*Z(J)
  200          CONTINUE
  205       CONTINUE
         ENDIF
         DO 215 I=1,N1
            HP(I)=(0.0,0.0)
            DO 210 J=1,N1
               HP(I)=HP(I)+H(J)*P11(J,I)
  210       CONTINUE
  215    CONTINUE
c
c add observational error R
c
         V=R
         DO 220 I=1,N1
            v=v+hp(i)*dconjg(h(i))
            IF(NOD.GT.0)THEN
c               v=v+(2.0,0.0)*dreal(h(i)*exx(i)) changed JKL
               V=V+(2.0,0.0)*DBLE(H(I)*EXX(I))
            ENDIF
  220    CONTINUE
         IF(NOD.GT.0)THEN
            DO 225 I=1,NQ
               V=V+Z(I)*VEC(I)
  225       CONTINUE
         ENDIF
C
C Accumulate X'X augumented by X'Y and the determinant DET
C
            DO 235 J=1,NLP1
               DO 230 I=1,J
                  XX(I,J)=XX(I,J)+INNOV(I)*INNOV(J)/V
  230          CONTINUE
  235       CONTINUE
            DET=DET+DLOG(V)
C
C Calculate Kalman gain
C
            DO 245 I=1,N1
               K(I)=(0.0,0.0)
               DO 240 J=1,N1
                  k(i)=k(i)+p11(i,j)*dconjg(h(j))
  240          CONTINUE
               IF(NOD.GT.0)THEN
                  K(I)=(K(I)+EXX(I))
               ENDIF
  245       CONTINUE
            IF(NOD.GT.0)THEN
               DO 255 I=1,NQ
                  K(N1+I)=(0.0,0.0)
                  DO 250 J=1,N1
                     k(i+n1)=k(i+n1)+dconjg(p12(j,i))*dconjg(h(j))
  250             CONTINUE
                  K(I+N1)=(K(I+N1)+VEC(I))
  255          CONTINUE
            ENDIF
C
C Update state vector
C
            DO 265 I=1,N1
               DO 260 J=1,NLP1
                  CSTATE(I,J)=CSTATE(I,J)+K(I)*INNOV(J)/V
  260          CONTINUE
  265       CONTINUE
            IF(NOD.GT.0)THEN
               DO 275 I=1,NQ
                  DO 270 J=1,NLP1
                     STATE(I,J)=STATE(I,J)+K(N1+I)*INNOV(J)/V
  270             CONTINUE
  275          CONTINUE
            ENDIF
C
C Update state covariance matrix
C
            DO 310 I=1,N1
               DO 305 J=I,N1
                  p11(i,j)=p11(i,j)-k(i)*dconjg(k(j))/v
                  p11(j,i)=dconjg(p11(i,j))
  305          CONTINUE
c               p11(i,i)=dreal(p11(i,i)) changed JKL
               P11(I,I)=DBLE(P11(I,I))
  310       CONTINUE
            IF(NOD.GT.0)THEN
               DO 320 I=1,N1
                  DO 315 J=1,NQ
                     p12(i,j)=p12(i,j)-k(i)*dconjg(k(j+n1))/v
  315             CONTINUE
  320          CONTINUE
               DO 330 I=1,NQ
                  DO 325 J=I,NQ
                     p22(i,j)=p22(i,j)-k(i+n1)*dconjg(k(j+n1))/v
                     P22(J,I)=P22(I,J)
  325             CONTINUE
  330          CONTINUE
            ENDIF
 1000    CONTINUE
         NT=NT+NOBS(JJ)
 2000 CONTINUE
C
C Factor the normal equations to obtain the residual 
C sums of squares.
C
      SSE=XX(NLP1,NLP1)
      CALL FACTOR(XX,NLP,IER)
      DO 340 I=1,NLP
         SSE=SSE-XX(I,NLP1)**2
  340 CONTINUE
C
C Calculate -2 ln likelihood
C
      DEN=FLOAT(NT)
      LIKE=NT*DLOG(SSE/DEN)+DET+NT*(1+ALOG(2.0*3.141592654))
      RETURN
      END


      SUBROUTINE FACTOR(V,N,IER)
      IMPLICIT NONE

      INTEGER N,IER,I,I1,J,IP1,K,M
      DOUBLE PRECISION V(N+1,N+1),TOL,EX
C
C  THIS SUBROUTINE DOES AN IN PLACE UPPER TRIANGULAR CHOLESKY
C  FACTORIZATION OF A POSITIVE DEFINITE MATRIX, V=T'T.  THE MATRIX
C  MAY BE AUGMENTED BY ANY NUMBER OF COLUMNS WHICH WILL BE REDUCED
C  BY PREMULTIPLICATION BY T' INVERSE.  REF: GRAYBILL, F. A.,
C  THEORY AND APPLICATION OF THE LINEAR MODEL, DUXBURY PRESS,
C  1976, P. 232.
C
C  INPUT TO SUBROUTINE
C      V  POSITIVE DEFINITE N BY N MATRIX TO BE FACTORED.  ONLY
C         THE UPPER TRIANGULAR PART IS USED AND IS REPLACED BY T.
C         V MAY BE AUGMENTED BY M-N COLUMNS TO BE PREMULTIPLIED
C         BY T' INVERSE.
C      N  THE NUMBER OF ROWS OF V
C      ND  FIRST DIMENSION OF V IN THE CALLING PROGRAM
C      M  THE NUMBER OF COLUMNS OF V.  M MUST BE .GE. N
C
C  OUTPUT FROM SUBROUTINE
C      IER=0  NO ERRORS
C          1  PARAMETERS OUT OF RANGE
C          2  MATRIX NOT POSITIVE DEFINITE
C
       IER=0
C  CHECK INPUT PARAMETERS
      IF(N.LT.1) GO TO 70
C
C  CALCULATE NORM OF MATRIX
C
      M=N+1
      EX=0.
      DO 5 I=1,N
         EX=EX+ABS(V(I,I))
    5 CONTINUE
      TOL=(EX/N)*1.0D-24
      DO 60 I=1,N
         I1=I-1
         IF(I1.LT.1)GO TO 20
            DO 10 J=1,I1
               V(I,I)=V(I,I)-V(J,I)**2
   10       CONTINUE
   20    CONTINUE
         IF(V(I,I).LE.TOL) THEN
            V(I,I)=0.
            IER=2
         ELSE
            V(I,I)=DSQRT(V(I,I))
         END IF
         IP1=I+1
         IF(IP1.GT.M)GO TO 100
         DO 50 J=IP1,M
            IF(I1.LT.1)GO TO 40
               DO 30 K=1,I1
                  V(I,J)=V(I,J)-V(K,I)*V(K,J)
   30          CONTINUE
   40       CONTINUE
            IF(V(I,I).LE.TOL) THEN
               V(I,J)=0.
            ELSE
               V(I,J)=V(I,J)/V(I,I)
            END IF
   50    CONTINUE
   60 CONTINUE
      GO TO 100
   70 IER=1
  100 RETURN
      END
C
      SUBROUTINE TTVERT(T,N)
      IMPLICIT NONE

      INTEGER I,J,II,JJ,K,N
      DOUBLE PRECISION T(N+1,N+1),EX
C
C  THIS SUBROUTINE REPLACES THE UPPER TRIANGULAR FACTOR T BY T'T
C  INVERSE.  REF: GRAYBILL, F. A.  THEORY AND APPLICATION OF THE
C  LINEAR MODEL (1976), DUXBURY PRESS, P. 246.
C
C  INPUT TO SUBROUTINE
C      T   UPPER TRIANGULAR N BY N MATRIX
C      ND  FIRST DIMENSION OF T IN CALLING PROGRAM
C
C  OUTPUT FROM SUBROUTINE
C      T   T'T INVERSE WHICH IS SYMMETRIC
C      IER=0  NO ERRORS
C          1  PARAMETERS OUT OF RANGE
C          2  INPUT MATRIX NOT POSITIVE ON DIAGONALS
C
C  CHECK INPUT PARAMETERS
c      IF(N.LT.1)GO TO 60
c      IER=0
      DO 50 JJ=1,N
         J=N+1-JJ
C  CALCULATE DIAGONAL ELEMENTS
         IF(T(J,J).LE.0.0) THEN
            T(J,J)=0
         ELSE
         EX=1.0D0/T(J,J)
         IF(J.LT.N)THEN
            DO 10 K=J+1,N
               EX=EX-T(J,K)*T(K,J)
   10       CONTINUE
         END IF
         T(J,J)=EX/T(J,J)
         END IF
C  FILL IN ROW J FROM TRANSPOSED POSITION
         IF(J.LT.N)THEN
            DO 20 K=J+1,N
               T(J,K)=T(K,J)
   20       CONTINUE
         END IF
C  CALCULATE COLUMN J AND STORE IN TRANSPOSED POSITION
         IF(J.EQ.1)RETURN
         DO 40 II=1,J-1
            I=J-II
            IF(T(I,I).GT.0.0) THEN
            EX=0.0
            DO 30 K=I+1,N
               EX=EX+T(I,K)*T(J,K)
   30       CONTINUE
            T(J,I)=-EX/T(I,I)
            ELSE
               T(J,I)=0.0
            END IF
   40    CONTINUE
   50 CONTINUE
c   60 IER=1
      RETURN
      END
C
      SUBROUTINE BACK(T,N)
      IMPLICIT NONE

      INTEGER N,N1,I,J,II,I1,K,M
      DOUBLE PRECISION T(N+1,N+1)
      M=N+1
      N1=N+1
      DO 40 J=N1,M
         DO 30 I=1,N
            II=N+1-I
            I1=II+1
            IF(I1.GT.N) GO TO 20
               DO 10 K=I1,N
                  T(II,J)=T(II,J)-T(II,K)*T(K,J)
   10          CONTINUE
   20       CONTINUE
         IF(T(II,II).LE.0) THEN
            T(II,J)=0.
         ELSE
            T(II,J)=T(II,J)/T(II,II)
         END IF
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
