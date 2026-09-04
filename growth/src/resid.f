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
c      subroutine resid2(np,par,ave,pred,rpred,sdr,res,y,mse,ns,
c     +nt,model,t,nobs,nod,p)
c
c  DESCRIPTION
c
c    Function to compute the recursive fitted values for the multivariate
c normal distribution with AR(1) and/or random intercept using Kalman
c filtering
c
c Richard H. Jones
c Department of Preventive Medicine and Biometrics
c School of Medicine, Box B-119
c University of Colorado Health Sciences Center
c Denver, CO 80262
c U.S.A.
c (303) 270-6860
c modified by J.K. Lindsey, April, 1999
c

      SUBROUTINE RESID2(NP,PAR,AVE,PRED,RPRED,SDR,RES,Y,MSE,NS,
     +     NT,MODEL,T,NOBS,NOD,P)
c
      IMPLICIT NONE
      INTEGER MAXRE,MAXAR
      PARAMETER(MAXRE=1,MAXAR=1)
c
      INTEGER NS,NT,NOD,NP,IND,JJ,LL
      DOUBLE PRECISION V,VEC(MAXRE),P(NP),P22(MAXRE,MAXRE),VARI
      double precision MSE,PAR(NP),T(NT),Y(NT),
     +RPRED(NT),PRED(NT),SDR(NT),RES(NT),AVE
      REAL STATE(MAXRE),INNOV,D(MAXRE,MAXRE),U(MAXRE,MAXRE)
      REAL SSD(MAXRE),DT,SD
      INTEGER MODEL,NOBS(NS)
      DOUBLE COMPLEX RR(MAXAR),UU(MAXAR,MAXAR),UI(MAXAR,MAXAR)
      DOUBLE COMPLEX P12(MAXAR,MAXRE),EXX,DC(MAXAR),HTEMP(MAXAR)
      DOUBLE COMPLEX H(MAXAR),HP(MAXAR),K(MAXAR+1),EX(MAXAR)
      DOUBLE COMPLEX P110(MAXAR,MAXAR),P11(MAXAR,MAXAR)
      DOUBLE COMPLEX CSTATE(MAXAR)
C
      SD=SQRT(MSE)
      IND=MODEL
      IF(MODEL.GE.1)THEN
         P(1)=PAR(1)
         CALL ROOTS(MODEL,P,RR)
         CALL TRANS(MODEL,RR,UU)
         UI(1,1)=UU(1,1)
         CALL CVERT(MODEL,UI)
         CALL INIT(MODEL,RR,UI,P110,VARI)
      ENDIF
      HTEMP(1)=(1.0,0.0)
      H(1)=(1.0,0.0)
      IF(MODEL.GE.1) THEN
         H(1)=HTEMP(1)*UU(1,1)
      ENDIF
C
C Initialize D matrix
C
      IF(NOD.GT.0)THEN
         U(1,1)=0.0
      ENDIF
C
C Construct an upper triangular matrix U.  The actual initial input of
c the matrix D will be U'U. This will ensure positive definiteness of
c the matrix D.
C
      IF (NOD.GE.1) THEN
         IND=IND+1
         U(1,1)=SD*PAR(IND)
         D(1,1)=U(1,1)*U(1,1)
      ENDIF
C
C Start subject loop
C
      NT=0
      DO 2000 JJ=1,NS
C
C Initialize State vector and covariance matrix
C
         CSTATE(1)=(0.0,0.0)
         IF(NOD.GT.0)THEN
            STATE(1)=0.0
         ENDIF
         IF(MODEL.EQ.0)THEN
            P11(1,1)=MSE
         ELSE
            P11(1,1)=dble(MSE*P110(1,1))
         ENDIF
         IF(NOD.GT.0)THEN
            P12(1,1)=(0.0,0.0)
            P22(1,1)=D(1,1)
         ENDIF
C
C Start within subject loop.
C
         DO 1000 LL=1,NOBS(JJ)
C
C Predict state vector from time LL-1 to time LL
C and calculate one step prediction of the covariance matrix
C
            EXX=(0.0,0.0)
            IF(MODEL.EQ.0) THEN
               CSTATE(1)=(0.0,0.0)
               P11(1,1)=MSE*(1.0,0.0)
               IF(NOD.GT.0)THEN
                  P12(1,1)=(0.0,0.0)
               ENDIF
            ELSE
               IF(LL.EQ.1) THEN
                  DT=0
               ELSE
                  DT=T(NT+LL)-T(NT+LL-1)
               END IF
               DC(1)=CEXP(cmplx(RR(1)*DT))
               CSTATE(1)=CSTATE(1)*DC(1)
               EXX=EXX+CSTATE(1)*H(1)
               p11(1,1)=dc(1)*p11(1,1)*dconjg(dc(1))-(ui(1,1)
     +              *dconjg(ui(1,1))/(rr(1)+dconjg(rr(1))))*mse*
     +              ((1.0,0.0)-dc(1)*dconjg(dc(1)))/vari
               P11(1,1)=DBLE(P11(1,1))
               IF(NOD.GT.0)THEN
                  P12(1,1)=DC(1)*P12(1,1)
               ENDIF
            ENDIF
c
c Calculate residuals
c
            RPRED(NT+LL)=EXX+PRED(NT+LL)
C
C Calculate innovations
C 
            IF(NOD.GT.0)THEN
               RPRED(NT+LL)=RPRED(NT+LL)+STATE(1)
            ENDIF
            INNOV=Y(NT+LL)-RPRED(NT+LL)
C
C Calculate innovation variance
C
            IF(NOD.GT.0)THEN
               EX(1)=P12(1,1)
               VEC(1)=P22(1,1)
            ENDIF
            HP(1)=H(1)*P11(1,1)
c
            V=hp(1)*dconjg(h(1))
            IF(NOD.GT.0)THEN
               V=V+(2.0,0.0)*DBLE(H(1)*EX(1))
            ENDIF
            IF(NOD.GT.0)THEN
               V=V+VEC(1)
            ENDIF
            SDR(NT+LL)=DSQRT(V)
            RES(NT+LL)=INNOV/SDR(NT+LL)
C
C Calculate Kalman gain
C
            K(1)=p11(1,1)*dconjg(h(1))
            IF(NOD.GT.0)THEN
               K(1)=(K(1)+EX(1))
            ENDIF
            IF(NOD.GT.0)THEN
               K(2)=VEC(1)+dconjg(p12(1,1))*dconjg(h(1))
            ENDIF
C
C Update state vector
C
            CSTATE(1)=CSTATE(1)+K(1)*INNOV/V
            IF(NOD.GT.0)THEN
               STATE(1)=STATE(1)+K(2)*INNOV/V
            ENDIF
C
C Update state covariance matrix
C
            p11(1,1)=dble(p11(1,1)-k(1)*dconjg(k(1))/v)
            IF(NOD.GT.0)THEN
               p12(1,1)=p12(1,1)-k(1)*dconjg(k(2))/v
               p22(1,1)=p22(1,1)-k(2)*dconjg(k(2))/v
            ENDIF
 1000    CONTINUE
         DO 410 LL=1,NOBS(JJ)
            T(NT+LL)=T(NT+LL)+AVE
 410     CONTINUE
         IF(NOD.GT.0)THEN
            SSD(1)=DSQRT(P22(1,1))
         ENDIF
         NT=NT+NOBS(JJ)
 2000 CONTINUE
      RETURN
      END
