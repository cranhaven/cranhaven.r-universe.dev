c
c  event : A Library of Special Functions for Event Histories
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
c    subroutine weibull(iconst,rconst,istime,squant,valrho,
c   +     includ,info,itabl,xtabl,nrr,info2,itabl2,xtabl2,ipedig,nrr2,
c   +     nccov,ndcov,nstime,nquant,igam,inor,xgam,xnor,bound,icons,
c   +     beta,std,gradf,hess,xmom,surv,xlik,idf)
c    subroutine cox(iconst,rconst,istime,squant,includ,info,itabl,
c   +     xtabl,nrr,info2,itabl2,xtabl2,iplus1,iplus2,ipedig,nrr2,
c   +     nccov,ndcov,nstime,nquant,igam,inor,xgam,xnor,bound,
c   +     icons,beta,std,gradf,hess,xmom,surv,xlik,idf,km,resid,ut)
c
c  DESCRIPTION
c
c    Functions to calculate the likelihoods for Weibull and Cox models
c  with frailties
c
      subroutine weibull(iconst,rconst,istime,squant,valrho,
     +     includ,info,itabl,xtabl,nrr,info2,itabl2,xtabl2,ipedig,nrr2,
     +     nccov,ndcov,nstime,nquant,igam,inor,xgam,xnor,bound,icons,
     +     beta,std,gradf,hess,xmom,surv,xlik,idf)
C//////////////////////////////////////////////////////////////////////*
C  WEIBULL       *                                    *  16 / 09 /1997 *
C                *                        Version 3.0 of February 1998 *
C                *            modified for R by J.K.Lindsey, June 1998 *
C//////////////////////////////////////////////////////////////////////*

      include 'parinclu.h'

      integer iconst(18)
      double precision rconst(1)

      INTEGER NCOVMOD,NCCOV,NDCOV,NSTRATA,ICHECK 
      integer nrr,nrr2,nquant,nstime,nrule,nrelmat,nani,nrelmat2,nrec
      INTEGER ISTIME(NSTIME),INCLUD(nccov+ndcov),MAXTIM,iter_bf,itoev
      INTEGER IPEDIG(4,NDIMAX),itabl2(nrr2,ndcov),info2(nrr2,4)
      integer itabl(nrr,ndcov),err,model
      real(8) SQUANT(NQUANT),xtabl(nrr,nccov),xtabl2(nrr2,nccov)
      real(8) VALRHO(MXSTRA),DLOGRHO(MXSTRA)
      real(8) HESS(NDIMAX,NDIMAX),surv(nstimax*nrr,3)
c
      INTEGER I,J,JJ,NDIM,N
      INTEGER IFAIL,NSURV,NCONS
      INTEGER NSTATUS,NOPTION,NPEST,IUSER(3),IRK,IRK0
      INTEGER NTOT,NFAIL(NDIMAX),ICONS(0:NDIMAX)
      INTEGER IDF(2),iplus1(mxef),iplus2(mxef)
      INTEGER IFIRST(MXEF),ILAST(MXEF),IANAL(MXEF),NJOINT,NINTTDEP             
      INTEGER NDENS,ICRHO,INTEGAM,NDIMB
      INTEGER NRAND,MAXRAND,IGAM(0:NCCOV+NDCOV,3),INOR(0:NCCOV+NDCOV,3)
      INTEGER IRANK(NDIMAX),NCOL(MXEF)
      INTEGER ITGAUSS,KITER
      INTEGER NWITHIN,IWITHCOL,NO_LOG
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)
C
      real(8) F,W,OBJ,LOB,UPB,BEST,DFMIN 
      real(8) EPS_BF,DLDET,DLDET0,XLIK(2)
      real(8) BETA(NDIMAX),GRADF(NDIMAX),STORBETA(NDIMAX,0:NPGAUSS)
      real(8) STD(NDIMAX),BOUND(NCCOV+NDCOV,3)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) BETAWOC(2*MXSTRA+3)
      real(8) WGAUSS(5,5),XGAUSS(5,5),XMOM(0:3),XMOMP(2),W2
      real(8) OBJ0,XMUK,XMUKM1,SIGK,SIGKM1,R1,R2,VAL0,VAL1,GVAL
      real(8) VECLOG(NTIMMAX),VECEXP(NTIMMAX,MXSTRA)
C    
      EXTERNAL FTOMIN
      double precision FTOMIN
C           
      COMMON/BL1/IFIRST,ILAST,NCOL,IRANK
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL5/NFAIL,NTOT
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL14/NDIM
      COMMON/BL16/VECLOG,VECEXP,BETAWOC,MAXTIM
      COMMON/BL18/DLOGRHO,NO_LOG
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA
      common/bl21/model
C-----------------------------------------------------------------------
C   ROOTS USED IN THE GAUSS-HERMITE QUADRATURE  
C-----------------------------------------------------------------------
      DATA(WGAUSS(J,3),J=1,3)/
     + 0.417771379105166318,1.67108551642066683,0.417771379105166318/
      DATA(WGAUSS(J,4),J=1,4)/
     + 0.114993714684505977,1.13832042263099442,
     + 1.13832042263099487,0.114993714684505977/
      DATA(WGAUSS(J,5),J=1,5)/
     + 0.0282181455332160026,0.556661785214017657,
     + 1.33686841313653382,0.556661785214018656,0.02821814553321600266/
      DATA(XGAUSS(J,3),J=1,3)/
     + -1.73205080756887786,0.D0,1.73205080756887786/
      DATA(XGAUSS(J,4),J=1,4)/
     + -2.33441421833897689,-0.741963784302725915,
     + 0.741963784302725360,2.33441421833897689/
      DATA(XGAUSS(J,5),J=1,5)/
     + -2.85697001387280558,-1.35562617997426593,0.D0,
     + 1.35562617997426460,2.85697001387280558/

c      DATA((WGAUSS(J,I),J=1,I),I=3,5)/
c     + 0.417771379105166318,1.67108551642066683,0.417771379105166318,
c     + 0.114993714684505977,1.13832042263099442,
c     + 1.13832042263099487,0.114993714684505977,
c     + 0.0282181455332160026,0.556661785214017657,
c     + 1.33686841313653382,0.556661785214018656,0.02821814553321600266/
c      DATA((XGAUSS(J,I),J=1,I),I=3,5)/
c     + -1.73205080756887786,0.D0,1.73205080756887786,
c     + -2.33441421833897689,-0.741963784302725915,
c     + 0.741963784302725360,2.33441421833897689,
c     + -2.85697001387280558,-1.35562617997426593,0.D0,
c     + 1.35562617997426460,2.85697001387280558/ 

c step 0 : store constants

      ncons=iconst(1)
      nsurv=iconst(2)
      nstrata=iconst(3)
      no_log=iconst(4)
      nrelmat=iconst(5)
      integam=iconst(6)
      nwithin=iconst(7)
      ncovmod=iconst(8)
      icrho=iconst(11)
      ninttdep=iconst(12)
      njoint=iconst(13)
      npest=iconst(14)
      model=iconst(15)
      nrule=iconst(16)
      iter_bf=iconst(18)
      maxrand=nccov+ndcov
      nrelmat2=nrelmat

      eps_bf=rconst(1)

      err=0
      itoev=0
C           
C-----------------------------------------------------------------------
C STEP 1 : READ MODEL SPECIFICATION , INITIALIZATION
C          CHECK INPUT FILE, COMPUTES ELEMENTARY STATISTICS             
C          PREPARE WORKING FILE FOR SUBROUTINE FWEIB 
C-----------------------------------------------------------------------
      CALL INIT(NSURV,NCONS,NSTATUS,itabl,xtabl,nrr,
     +     includ,istime,squant,valrho,igam,inor,xgam,icons,beta,info,
     +     nccov,ndcov,nstime,nquant,itabl2,xtabl2,ipedig,nrr2,err)
      if(err.gt.0)goto 9999

      IF (NCOVMOD.EQ.0) GOTO  80
C-----------------------------------------------------------------------
C STEP 2 : FIND OR CHECK CONSTRAINTS                
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C IF NCONS = -1, LOOK FOR CONSTRAINTS               
C-----------------------------------------------------------------------
      IF (NCONS.EQ.-1) THEN     
        DO 10 I=1,NDIMB    
          IF (NFAIL(I).NE.0) BETA(I) = 0.005D0*MOD(I,10)                 
  10    CONTINUE
        NOPTION=90               
        DO 6252 J=1,NCCOV+NDCOV 
          IANAL(J)=1
 6252   CONTINUE
        CALL FWEIB2D(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,VALRHO
     +    ,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,err)
        if(err.gt.0)goto 9999
        ICHECK=0
C----------------------------------------------------------------------
C IF NCONS = 2, CHECK THAT THE CONSTRAINTS SUPPLIED BY THE USER ARE     
C VALID     
C-----------------------------------------------------------------------
      ELSE IF (NCONS.GE.2) THEN 
        DO 20 I=1,NDIMB
          IF (NFAIL(I).NE.0) BETA(I) = 0.005D0*MOD(I,10)
   20   CONTINUE      
         DO 6254 J=1,NCCOV+NDCOV 
          IANAL(J)=1
 6254    CONTINUE
C
C COMPUTATION OF DLDET AND IRK WITH SUPPLIED CONSTRAINTS 
C
        ICHECK = 2
        IRK=0
        CALL FWEIB2D(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,VALRHO
     +   ,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,err)
        if(err.gt.0)goto 9999
        DO 22 I=1,NDIM
	  IF (STD(I).GT.1.d-8.AND.BETA(I).GT.-999.D0) IRK=IRK+1 
C REINITIALIZATION OF VECTOR BETA
          IF (NFAIL(I).NE.0.AND.I.LE.NDIMB) BETA(I) = 0.005D0*MOD(I,10)
   22   CONTINUE      
C
C COMPUTATION OF DLDET0 AND IRK0 WITH CORRECT SET OF CONSTRAINTS 
C
        NOPTION = 100
        CALL FWEIB2D(BETA,F,GRADF,STD,DLDET0,NSTATUS,NOPTION,NDIM,VALRHO
     +   ,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,err)
        if(err.gt.0)goto 9999
        ICHECK=0
        IRK0=0 
        DO 24 I=1,NDIM
          IF (STD(I).LT.1.d-8.AND.BETA(I).GT.-999.D0) THEN
	      ICHECK=ICHECK+1
            GRADF(ICHECK)=I
          ELSE 
            IRK0 = IRK0 +1
          ENDIF
   24   CONTINUE 
C----------------------------------------------------------------------- 
C  COMPARE RESULTS         
C----------------------------------------------------------------------- 
        W = (DLDET-DLDET0)/DLDET
        IF (DABS(W).LT.1.D-8) THEN
          IF (IRK.EQ.IRK0) THEN
            ICHECK=0
          ELSE
            ICHECK=-ICHECK
          ENDIF
        ENDIF
      ENDIF
C           
C-----------------------------------------------------------------------
C PRINT IF PROBLEMS             
C-----------------------------------------------------------------------
C             
      IF (NCONS.GE.2) THEN               
       IF (ICHECK.GT.0) THEN
         RETURN
       ELSE
         ICHECK=0
       ENDIF
       DO 95 I=1,NDIMB           
         IF (NFAIL(I).NE.0) THEN
            BETA(I) = 0.D0
         ELSE
            BETA(I) = -999.99D0
         ENDIF            
   95  CONTINUE                
      ENDIF 
C
C-----------------------------------------------------------------------
C STEP 3 : CALL THE MAXIMIZATION SUBROUTINE                
C-----------------------------------------------------------------------
   80  IFAIL = 0 
C-----------------------------------------------------------------------
C COMPUTE FWEIB WITH NO COVARIATE
C-----------------------------------------------------------------------
      IUSER(3)=-1
        DO 125 J=1,NCCOV+NDCOV  
          IANAL(J)=0           
  125   CONTINUE               
        DO 15 I=1,NDIMB        
          IF (NFAIL(I).NE.0) BETA(I) = 0.D0
   15   CONTINUE
        CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +       IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
        if(err.gt.0)goto 9999
          DO 128 I=NDIMB+1,NDIM
            BETAWOC(I-NDIMB) = BETA(I)
  128     CONTINUE
c
      IDF(1) = IUSER(1)
      XLIK(1) = F
        IF (NCOVMOD.EQ.0) THEN
           GOTO 750
        ENDIF
C-----------------------------------------------------------------------
C LAST ONE = FULL MODEL         
C-----------------------------------------------------------------------
      DO 200 J=1,NCCOV+NDCOV         
       IANAL(J)=1              
  200 CONTINUE
      IF (NPEST.EQ.0) THEN
        CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,IGAM,INOR,
     +        XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,hess,itoev,
     +        iter_bf,iplus1,iplus2,ipedig,err)
        if(err.gt.0)goto 9999
      ELSE IF (NPEST.GE.1) THEN
        JJ=0
        DO 202 J=1,MAXRAND
          IF (BOUND(J,1).GT.0.D0) JJ=J
  202   CONTINUE
        IF (NPEST.EQ.100) IUSER(3)=1 
        LOB=BOUND(JJ,1)
        UPB=BOUND(JJ,2)
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added onSeptember 16, 1997
C OBJ is the minimum value of FTOMIN
        BEST=DFMIN(LOB,UPB,FTOMIN,BOUND(JJ,3),OBJ,IGAM,
     +       INOR,XGAM,XNOR,BOUND,icons,beta,info,nrr,nccov,ndcov,
     +       hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
        if(err.gt.0)goto 9999
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        IF (IGAM(JJ,1).EQ.1) THEN
           XGAM(JJ)=BEST
        ELSE
           XNOR(JJ)=BEST
        ENDIF
       IF (NPEST.EQ.100) THEN           
         DO 690 J=1,NDIM
           STORBETA(J,0)=BETA(J) 
  690    CONTINUE
       OBJ0= OBJ
       XMUKM1=DLOG(BEST)
       SIGKM1= DABS(XMUKM1/20.D0)
       KITER=0
       DO 700 ITGAUSS=1,NITER_GAUSS
         IF (KITER.EQ.1) GOTO 701
         DO 705 N=0,3
           XMOM(N)=0.D0
 705     CONTINUE
         XMUK=0.D0
         SIGK=0.D0
         DO 710 I=1,NPGAUSS
           VAL0=XMUKM1+ SIGKM1 * XGAUSS(I,NPGAUSS)
           VAL1 = 0.5D0 * XGAUSS(I,NPGAUSS) * XGAUSS(I,NPGAUSS)
           W= DEXP(VAL0)
           IF (IGAM(JJ,1).EQ.1) THEN
             XGAM(JJ) = W
           ELSE
             XNOR(JJ) = W
           ENDIF
           IF (ITGAUSS.GT.1) THEN
             DO 712 J=1,NDIM
               BETA(J)=STORBETA(J,I) 
  712        CONTINUE
           ENDIF
           CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +          IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +          hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
           if(err.gt.0)goto 9999
           DO 714 J=1,NDIM
             STORBETA(J,I)=BETA(J) 
  714      CONTINUE
           GVAL =  OBJ0 - OBJ
           R1= SIGKM1 * VAL0 * DEXP(VAL0 + VAL1 + GVAL)
           R2= R1 * VAL0
           XMUK = XMUK + WGAUSS(I,NPGAUSS) * R1
           SIGK = SIGK + WGAUSS(I,NPGAUSS) * R2
           DO 720 N=0,3
              W = SIGKM1*DEXP((N+1.D0) * VAL0 + VAL1 + GVAL)
              XMOM(N)=XMOM(N) + WGAUSS(I,NPGAUSS) * W
 720       CONTINUE
 710       CONTINUE
           XMUK =XMUK / XMOM(0)
           SIGK =SIGK / XMOM(0)
           SIGK = DSQRT(SIGK - XMUK * XMUK)
           DO 730 N=1,3
              XMOM(N)=XMOM(N)/XMOM(0)
 730       CONTINUE
           XMOM(3) = XMOM(3) - 3.D0 * XMOM(1)* XMOM(2)
     +          + 2.D0 * XMOM(1)**3
           XMOM(2) = DSQRT(XMOM(2) - XMOM(1) * XMOM(1))
           XMOM(3) = XMOM(3) / (XMOM(2)**3)
           W = DABS(XMOM(1) - XMOMP(1))/XMOM(1) 
           W2 = DABS(XMOM(2) - XMOMP(2))/XMOM(2)
           IF (MAX(W,W2).LT.1.D-3) THEN
            KITER=1
           ENDIF
           XMOMP(1) = XMOM(1)
           XMOMP(2) = XMOM(2) 
           XMUKM1=XMUK
           SIGKM1=SIGK
 700    CONTINUE
 701    DO 740 J=1,NDIM
           BETA(J)=STORBETA(J,0) 
  740    CONTINUE
         IF (IGAM(JJ,1).EQ.1) THEN
           XGAM(JJ) = BEST 
         ELSE
           XNOR(JJ) = BEST 
         ENDIF
C
       ENDIF
       CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +      IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +      hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
        if(err.gt.0)goto 9999
      ENDIF
C
 750  XLIK(2) = F 
      IDF(2) = IUSER(1)        
C
C-----------------------------------------------------------------------
C NSTD = 1 ==> COMPUTE STANDARD ERRORS              
C-----------------------------------------------------------------------
  334   ICHECK = 0
        NOPTION=0
        CALL FWEIB2D(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,VALRHO
     +   ,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,err)
        if(err.gt.0)goto 9999
        IF (ICHECK.EQ.-1) GOTO 334
       IF (ICRHO.EQ.1) THEN   
         DO 325 I=NDIMB+NSTRATA+1,NDIM-NJOINT 
           J = I - NDIMB -NSTRATA
           IF (NO_LOG.NE.1) THEN 
             BETA(I) = DEXP(BETA(I))
           ENDIF
  325    CONTINUE
       ENDIF
C           
      IF(NSURV.NE.0)CALL PREDICTED(BETA,SQUANT,VALRHO,ISTIME,nrr,
     +        nccov,ndcov,nstime,nquant,info2,nrr2,surv)
      if(npest.eq.100.and.kiter.eq.0)err=11
      iconst(2)=ndim
      iconst(3)=itoev
 9999 iconst(1)=err
      RETURN  
C
      END
      SUBROUTINE OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,IGAM,
     +     INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,hess,
     +     itoev,iter_bf,iplus1,iplus2,IPEDIG,err)
C//////////////////////////////////////////////////////////////////////*
C  USE  SUBROUTINE LBFGS TO MAXIMIZE FUNCTION F         30 / 08 / 95   *  
C//////////////////////////////////////////////////////////////////////*
c
	include 'parinclu.h'
c
c parameters for lbfgs

      INTEGER NWORK,NSTATE,IUSER(3),ICONS(0:NDIMAX),err,iter_bf
      PARAMETER(NWORK=NDIMAX*(2*MVEC_BF +1)+2*MVEC_BF)

      integer nrr,nccov,ndcov,itoev,model
      INTEGER IFLAG,ICALL,NDIM,MP,LP,J,JJ,IPEDIG(4,NDIMAX)
      INTEGER NSTATUS,NOPTION,INFO(NRR,4),iplus1(nccov),iplus2(ndcov)
      INTEGER NRAND,MAXRAND,IGAM(0:NCCOV+NDCOV,3),INOR(0:NCCOV+NDCOV,3)
      real(8) HESS(NDIMAX,NDIMAX),VALRHO(MXSTRA)
      real(8) BETA(NDIMAX),GRADF(NDIMAX),DIAG(NDIMAX),W(NWORK)
      real(8) STD(NDIMAX),DLDET,OBJ,EPS_BF
      real(8) F,XTOL,GTOL,STPMIN,STPMAX,BOUND(NCCOV+NDCOV,3)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)

      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      COMMON/BL8/NRAND,MAXRAND
      common/bl21/model
C
      XTOL= 1.0D-16
      ICALL=1
      IFLAG=0
      JJ=0
      DO 202 J=1,MAXRAND
        IF (BOUND(J,1).GT.0.D0) JJ=J
  202 CONTINUE
      IF (IUSER(3).EQ.-1) THEN
        ITOEV=0
      ENDIF
 20   if(model.eq.0)then
         CALL FWEIB(BETA,F,GRADF,NDIM,NSTATE,IUSER,VALRHO,IPEDIG,IGAM,
     +     INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,err)
      else
         CALL FCOX(BETA,F,GRADF,NDIM,NSTATE,IUSER,nrr,icons,XGAM,XNOR,
     +     BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,ipedig,err)
      endif
       if(err.gt.0)return
      CALL LBFGS(NDIM,MVEC_BF,BETA,F,GRADF,.false.,DIAG,
     +           EPS_BF,XTOL,W,IFLAG)
      IF(IFLAG.LE.0)GO TO 50
      ICALL=ICALL + 1
      ITOEV=ITOEV+1
C     We allow at most ITER_BF evaluations of F and GRADF
      IF(ICALL.GT.ITER_BF) GO TO 50
      GO TO 20
  50  CONTINUE
C
      NOPTION=1
      IF (NSTATUS.NE.1) NSTATUS=0 
      IF (IUSER(3).EQ.-1) RETURN
      if(model.eq.0)then
         CALL FWEIB2D(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,
     +        VALRHO,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,
     +        nrr,nccov,ndcov,hess,err)
      else
         CALL FCOX2(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,nrr,
     +        icons,XGAM,XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,
     +        iplus1,iplus2,ipedig,err,hess)
      endif
      OBJ = F+0.5D0*DLDET
      IF (IUSER(3).EQ.99) F=OBJ
C
      RETURN
c***********************************************************************
       END  

      SUBROUTINE INIT(NSURV,NCONS,NSTATUS,itabl,xtabl,nrr,
     +     includ,istime,squant,valrho,igam,inor,xgam,icons,beta,info,
     +     nccov,ndcov,nstime,nquant,itabl2,xtabl2,ipedig,nrr2,err)
C***********************************************************************
C  INIT          *                                    *  16 / 03 /1995 *
C***********************************************************************
C  READ MODEL SPECIFICATIONS, WRITE WORKING FILE FOR SUBROUTINE FWEIB  *
C***********************************************************************

      include 'parinclu.h'

      integer nrr,nrr2,kincl,nquant,nstime,ndimbps,nrec,inrr,err           
      INTEGER NCCOV,NDCOV,NSTRATA,ICHECK
      INTEGER I,II,IW,J,JJ,K,KK,NDIM
      INTEGER NSURV,NCONS,NSTATUS
      integer itabl2(nrr2,ndcov),model
      INTEGER IPEDIG(4,NDIMAX),MA(MXEF)
      INTEGER ISTIME(NSTIME),IWORK(3*NDIMAX),ITIME
      INTEGER IND(MXEF),INCLUD(nccov+ndcov),MAXTIM
      integer integdep,iprevw,mxwithin,nh
      integer itabl(nrr,ndcov),ipreid
      integer ncen,nlef,nunc,ipretim,nani,ilong
      integer nrule,nrelmat,nrelmat2,nunk
      INTEGER NTOT,NFAIL(NDIMAX),ICONS(0:NDIMAX)
      INTEGER IFIRST(MXEF),ILAST(MXEF),IANAL(MXEF),NJOINT,NINTTDEP  
      INTEGER NDENS,ICRHO,INTEGAM,NDIMB,ICODE,INOR(0:NCCOV+NDCOV,3)
      INTEGER NRAND,MAXRAND,IGAM(0:NCCOV+NDCOV,3)
      INTEGER IRANK(NDIMAX),NCOL(MXEF),IDATA2(MXEF_USED,NRECMAX2)
      INTEGER NWITHIN,IWITHCOL,NO_LOG
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)
C
      REAL XDATA(NRECMAX,MXEF_USED),XDATA2(MXEF_USED,NRECMAX2)
      real(8) SUMT,EPS_BF
      real(8) BETA(NDIMAX),XGAM(0:NCCOV+NDCOV),BETAWOC(2*MXSTRA+3)
      real(8) VECLOG(NTIMMAX),VECEXP(NTIMMAX,MXSTRA),W1
      real(8) SQUANT(NQUANT),xtabl(nrr,nccov),xtim
      real(8) VALRHO(MXSTRA),DLOGRHO(MXSTRA),xtabl2(nrr2,nccov)
C
      COMMON/BL1/IFIRST,ILAST,NCOL,IRANK
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL5/NFAIL,NTOT
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL14/NDIM
      COMMON/BL16/VECLOG,VECEXP,BETAWOC,MAXTIM
      COMMON/BL18/DLOGRHO,NO_LOG
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA
      COMMON/DATA2/IDATA2
      common/bl21/model
c
      EQUIVALENCE (IDATA(1,1),XDATA(1,1))           
      EQUIVALENCE (IDATA2(1,1),XDATA2(1,1)) 

      iwithcol=0
      integdep=0
      iprevw=0
      ipreid=0
      ncen=0
      nlef=0
      nunc=0
      sumt=0
      nunk=0
      nrand=0
      do 2406 i=1,ndimax
         nfail(i)=0.
 2406 continue
C-----------------------------------------------------------------------
C  REORDER APPROPRIATE VECTORS  
C-----------------------------------------------------------------------
      IF (NSTIME.GT.0) CALL JSORT(ISTIME,NSTIME,ISTIME,IWORK,'A')           
      IF (NQUANT.GT.0) CALL XSORT(SQUANT,NQUANT,SQUANT,IWORK,'D')
      DO 2405 I=1,MAX(1,NSTRATA)
        IF (NO_LOG.EQ.1) THEN
          IF (VALRHO(I).EQ.0.D0) VALRHO(I)=1.D0
        ENDIF
        DLOGRHO(I)=DLOG(VALRHO(I))
 2405 CONTINUE
C           
      DO 2440 I=1,NCCOV+NDCOV
c       XMA(I)=-1.        
c       XMI(I)=10000.     
       MA(I)=-1          
c       MI(I)=10000
 2440 CONTINUE           

      do 198 inrr=1,nrr
      MXWITHIN = 0
      itime=info(inrr,1)
      icode=info(inrr,2)
c      DO 111 I=1,NCCOV+NDCOV
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
c added on December 12, 1996
C SEARCH FOR THE LARGEST 'HERD' IF THE 'INTEGRATE_OUT ... WITHIN ... '
C STATEMENT IS USED 
c        IF (I.EQ.nwithin) THEN
        if(nwithin.gt.0)then
          II = info(inrr,3)
          IF (ICODE.EQ.-2.AND.INTEGDEP.EQ.1) II= info(inrr+1,3)
          IF (II.EQ.IPREVW) THEN
            NH = NH +1
          ELSE
            MXWITHIN=MAX(MXWITHIN,NH)
            NH=0
            IPREVW = II
          ENDIF       
        ENDIF    
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
c  111 CONTINUE
C
C Computation of log(time)
C
c  seems to assume that veclog initialized to zero! JKL
      IF (VECLOG(ITIME).EQ.0.D0) THEN
        MAXTIM = MAX(MAXTIM,ITIME)
        VECLOG(ITIME)=DLOG(DBLE(ITIME))
        IF (ICRHO.EQ.0)VECEXP(ITIME,info(inrr,3)) =
     +   DEXP(VALRHO(info(inrr,3))*VECLOG(ITIME))
      ENDIF


      IF (ICODE.EQ.0) THEN
        XTIM = DBLE(ITIME)
        IF (ICRHO.EQ.0) THEN
          SUMT = SUMT + VECEXP(ITIME,info(inrr,3))  
        ELSE
          SUMT = SUMT + XTIM
        ENDIF 
        NCEN = NCEN + 1
      ELSE IF (ICODE.LE.-2) THEN
        NLEF = NLEF + 1
      ELSE IF (ICODE.GT.0) THEN
        XTIM = DBLE(ITIME)
        IF (ICRHO.EQ.0) THEN
          SUMT = SUMT + VECEXP(ITIME,info(inrr,3)) 
        ELSE
          SUMT = SUMT + XTIM
        ENDIF          
       NUNC = NUNC + 1
      ENDIF
C
C-----------------------------------------------------------------------
C FOR CONTINUOUS COVARIATES, MIN, MAX, AVER, STD
C FOR DISCRETE COVARIATES, MIN, MAX
C-----------------------------------------------------------------------
      DO 121 I=1,ndcov
         IW = itabl(inrr,i)
         MA(I) =MAX(MA(I),IW)
  121 CONTINUE

 198  continue

      NDIM = 0
      DO 220 I=1,NCCOV+NDCOV
C-----------------------------------------------------------------------
C   STATISTICS FOR CONTINUOUS COVARIATES            
C-----------------------------------------------------------------------
          IF (i.le.nccov) THEN
            IF (INCLUD(I).EQ.1)ndim = ndim + 1
            IFIRST(I) = I
            ILAST(I) = I
          ELSE
c             IF (INCLUD(I).EQ.1) THEN                   
C-----------------------------------------------------------------------
C   STATISTICS FOR DISCRETE COVARIATES              
C   IND(K)= FOR EACH (DISCRETE) COVARIATE, NUMBER OF PRECEDING LEVELS   
C                     IN VECTOR 'BETA'              
C-----------------------------------------------------------------------
              IFIRST(I) = NDIM + 1
              IF (INCLUD(I).EQ.1)NDIM = NDIM + MA(I-nccov)
              ILAST(I) = NDIM
c          ENDIF              
        ENDIF                  
  220 CONTINUE
C-----------------------------------------------------------------------
C   CHARACTERISTICS OF RANDOM COVARIATES            
C-----------------------------------------------------------------------
      IF (IGAM(0,1).NE.0) THEN 
        DO 240 I=1,NDCOV
           jj=I+nccov
          IGAM(jj,2) = IFIRST(jj)
          IGAM(jj,3) = ILAST(jj) 
  240   CONTINUE               
      ENDIF
      IF (INOR(0,1).NE.0) THEN 
        DO 242 I=1,NDCOV
           jj=I+nccov
          INOR(jj,2) = IFIRST(jj)
          INOR(jj,3) = ILAST(jj)  
  242   CONTINUE                
      ENDIF
C          
C-----------------------------------------------------------------------
C   PRINT STATISTICS ACCORDING TO CENSORING STATUS IN 'RUNLOG' FILE (=6)
C      + IN 'RESULTS' FILE (= UNIT 12)              
C      + IN 'RESULTS FOR PREDICTED RECORDS' FILE (= UNIT 13) IF RELEVANT      
C   AFTER ALL FIXED AND RANDOM EFFECTS, INTERCEPT + RHO'S + GAMMA
C----------------------------------------------------------------------- 
      NDIMB = NDIM
      if(model.eq.0)NDIM = NDIM + NSTRATA
      NDIMBPS = NDIM
      NTOT=NUNC+NCEN 
      IF (ICRHO.EQ.1) NDIM = NDIM + NSTRATA
      IF (NJOINT.EQ.1) NDIM=NDIM+1
C-----------------------------------------------------------------------
C   NDIM = SIZE OF VECTOR 'BETA'
C-----------------------------------------------------------------------
       DO 280 J=1,ndcov
          if(includ(j+nccov).eq.1.or.integam.gt.0)then
             IND(J) = IFIRST(J+nccov)-1
          else
             IND(J) = IFIRST(J+nccov-1)-1
          endif
  280  CONTINUE
       IF (NDIM.GT.NDIMAX) THEN
          err=6
          return
       ENDIF
C           
C-----------------------------------------------------------------------
C   SECOND READING OF THE INPUT FILE (UNIT = 2)     
C   'FINAL' STORAGE OF THE DATA AS USED IN SUBROUTINE FWEIB              
C      IN CORE IF NSTOR = 1     
C      IN WORKING FILE IN UNIT 4 IF NSTOR =0        
C-----------------------------------------------------------------------
      KINCL = 0 
      IPREVW = 0             
      NH=0
      IPRETIM=0
      nrec=nrr
      do 300 inrr=1,nrr
C-----------------------------------------------------------------------
C   VARIABLE 1 = TIME           
C   VARIABLE 2 = CODE (1 = UNCENSORED RECORD, 0 = RIGHT CENSORED RECORD 
C                      -1 = TIME-DEPENDENT COVARIATE CHANGE)                   
C   VARIABLE 3 = STRATUM 
C          OR IF NINTTDEP=1   =  PREVIOUS TIME (TO INTEGRATE OUT RANDOM
C                                            TIME-DEPENDENT COVARIATES)
C   VARIABLE 4 = NID       
C-----------------------------------------------------------------------
      ilong=max(1,info(inrr,1))
      IF (NSTRATA.GT.1) THEN
        ILONG = ILONG - IPRETIM
      ELSE
        IF (NINTTDEP.eq.0) THEN
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
C added on November 29, 1996
           IF (NWITHIN.NE.0) THEN
              IF (IPREVW.NE.INFO(INRR,3)) THEN
                 NH=99
                 IPREVW = INFO(INRR,3)
              ELSE 
                 NH = 0
              ENDIF
           ELSE
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
             INFO(INRR,3) = 1
             ILONG = ILONG - IPRETIM
           ENDIF
         ENDIF
      ENDIF
C-----------------------------------------------------------------------
C   IR1 FOLLOWING VARIABLES : CONTINUOUS COVARIATES 
C   THE AVERAGE VALUE OF THE CONTINUOUS COVARIATE IS SUBSTRACTED        
C   NFAIL = NUMBER OF UNCENSORED FAILURES           
C-----------------------------------------------------------------------
      K=NDIMB+MAX(1,info(inrr,3))
      IF (info(inrr,2).GT.0) NFAIL(K) = NFAIL(K) +1
      DO 310 I=1,nccov
        IF (info(inrr,2).GT.0) NFAIL(I) = NFAIL(I)+1
       XDATA(INRR,I)= XTABL(iNRR,I)
  310 CONTINUE
C-----------------------------------------------------------------------
C   IR2 FOLLOWING VARIABLES : DISCRETE COVARIATES   
C   THE LEVEL OF EACH EFFECT, INITIALLY NUMBERED 1 TO MAXLEVEL          
C            ARE RENUMBERED BY ADDING IND(I)        
C   NFAIL = NUMBER OF UNCENSORED FAILURES           
C----------------------------------------------------------------------- 
      DO 316 I=1,ndcov
          K = ITABL(iNRR,I)
          if(k.gt.0)then
             k = k + IND(I)
             IF (info(inrr,2).GT.0)NFAIL(K) = NFAIL(K)+1
          endif
          IDATA(INRR,I + NCCOV)= K
  316 CONTINUE
 300  continue
C
      IF (NSURV.NE.0) THEN    
C-----------------------------------------------------------------------
C   READING OF THE INPUT FILE (UNIT = 3) FOR THE COMPUTATION OF         
C   THE SURVIVOR CURVE (I.E. IF NSURV = 1)          
C      IN CORE IF NSTOR = 1     
C      IN WORKING FILE IN UNIT 7 IF NSTOR =0        
C----------------------------------------------------------------------- 
C
       do 400 inrr=1,nrr2
C-----------------------------------------------------------------------
C   IR1 FOLLOWING VARIABLES : CONTINUOUS COVARIATES 
C   THE AVERAGE VALUE OF THE CONTINUOUS COVARIATE IS SUBSTRACTED        
C-----------------------------------------------------------------------
      DO 410 I=1,nccov
	XDATA2(I,INRR)= XTABL2(iNRR,I)
  410 CONTINUE  
C-----------------------------------------------------------------------
C   IR2 FOLLOWING VARIABLES : DISCRETE COVARIATES   
C   THE LEVEL OF EACH EFFECT, INITIALLY NUMBERED 1 TO MAXLEVEL          
C            ARE RENUMBERED BY ADDING IND(I)        
C----------------------------------------------------------------------- 
       DO 416 I=1,ndcov
         K =ITABL2(iNRR,I)
         IF (K.GT.0) K = K + IND(I)              
         IDATA2(I + NCCOV,INRR)= K
  416  CONTINUE
 400   continue
      ENDIF
C           
C-----------------------------------------------------------------------
C   DEFINITION OF CONSTRAINTS IF NCONS = 0          
C-----------------------------------------------------------------------
      NRAND = IGAM(0,1) + INOR(0,1)                 
      IF (NCONS.EQ.0) THEN
       DO 430 I = 1,NCCOV+NDCOV
        JJ= 0
        IF (NRAND.NE.0) THEN  
               IF (IGAM(I,1).EQ.1.OR.INOR(I,1).EQ.1) JJ=1 
               IF (NRULE.EQ.3.AND.NRELMAT2.EQ.K-1) JJ=1              
        ENDIF 
        IF (JJ.EQ.1.OR.i.le.nccov.or.includ(i).eq.0) GOTO 430  
        JJ= 0
        J = 0
        DO 435  K = IFIRST(I),ILAST(I)
          IF (NFAIL(K).GT.JJ) THEN                
             JJ = NFAIL(K)
             J = K
          ENDIF
  435   CONTINUE
       ICONS(0) = ICONS(0) + 1  
       IF (ICONS(0).GT.NDIMAX) THEN
          err=1
          return
       ENDIF
       ICONS(ICONS(0)) = J
  430 CONTINUE
C-----------------------------------------------------------------------
C   NO CONSTRAINTS IF NCONS = -2
C-----------------------------------------------------------------------
      ELSE IF (NCONS.EQ.-2) THEN
       ICONS(0) = 0
      ENDIF 
C-----------------------------------------------------------------------
C   IF NFAIL = 0, THE SOLUTION IS KNOWN = -INFINITY |||                 
C-----------------------------------------------------------------------
c      DO 450 I=1,NDIMB        
c       IF (NFAIL(I).EQ.0) BETA(I) = -999.99D0
c  450 CONTINUE 
C-----------------------------------------------------------------------
C     STARTING VALUES (OR FIXED VALUES) FOR RHO
C-----------------------------------------------------------------------
       DO 460 I=1,NSTRATA    
         J = NDIMBPS + I
         W1 = VALRHO(I)
         IF (ICRHO.EQ.0) THEN
              IF (W1.EQ.0.D0) W1 = VALRHO(1)
         ENDIF     
         IF (NO_LOG.EQ.1) THEN
           BETA(J) = W1
         ELSE 
           BETA(J) = DLOG(W1)
         ENDIF
  460 CONTINUE
C
C MULTIPLICATION PAR 1.01 BELOW IS TO AVOID PROBLEMS WHEN THE STARTING
C VALUE IS EXACTLY THE MODE (THIS HAPPENS IN THE NO COVARIATE CASE)
C OTHERWISE, THE MAXIMISATION SUBROUTINE IS TRYING TO FIND A BETTER POINT
C AND NEVER FINDS IT...
C 
      BETA(NDIMB+1) = 1.01*(DLOG(DBLE(NUNC)) - DLOG(SUMT))
      DO 462  I=2,NSTRATA
        BETA(NDIMB+I) = BETA(NDIMB+1)
  462 CONTINUE
C-----------------------------------------------------------------------
C  STARTING VALUES FOR BETA(NDIM) IF COMPUTATION OF A GAMMA PARAMETER
C AS A JOINT MODE AFTER ALGEBRAIC INTEGRATION
C-----------------------------------------------------------------------  
       IF (NJOINT.EQ.1)
     +  BETA(NDIM)= DLOG(XGAM(INTEGAM))
C-----------------------------------------------------------------------
C   IF THERE IS (ARE) RANDOM VARIABLE(S), SET BACK ALL BETA'S TO 0      
C      = ALL ELEMENTS BETWEEN IFIRST(I) AND ILAST(I)
C-----------------------------------------------------------------------
      IF (IGAM(0,1).NE.0) THEN  
        DO 3452 I=1,NCCOV+NDCOV
          IF (IGAM(I,1).NE.0.AND.I.NE.INTEGAM) THEN
            DO 3454 J=IFIRST(I),ILAST(I)
              BETA(J)=0.D0
 3454       CONTINUE
          ENDIF                 
 3452    CONTINUE               
      ENDIF
      IF (INOR(0,1).NE.0) THEN
        DO 3462 I=1,NCCOV+NDCOV
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
c Modified on June 27,1997 
          IF (INOR(I,1).NE.0) THEN  
            IF (NRULE.EQ.3.AND.NRELMAT2.EQ.I) THEN
               JJ=ILAST(I+1)
            ELSE
               JJ=0
            ENDIF                  
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
            DO 3464 J=IFIRST(I),JJ
              BETA(J)=0.D0
 3464       CONTINUE
          ENDIF                 
 3462    CONTINUE
      ENDIF 
C           
C-----------------------------------------------------------------------
C   IF NFAIL = 0, THE SOLUTION IS KNOWN = -INFINITY ||| (EXCEPT FOR 
C    RANDOM VARIABLES)                  
C-----------------------------------------------------------------------
      DO 660 J=1,NCCOV+NDCOV
        KK=J-1
        DO 665 K= IFIRST(J),ILAST(J)
          IRANK(IFIRST(J))=K
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
c Modified on June 27,1997
          IF (NRULE.EQ.3.AND.KK.EQ.NRELMAT2) GOTO 665
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
          IF (IGAM(J,1).EQ.0.AND.INOR(J,1).EQ.0.AND.
     +     NFAIL(K).EQ.0) BETA(K)=-999.99D0
  665   CONTINUE
  660 CONTINUE

C end of INIT
C
      return
      end
      double precision FUNCTION FTOMIN(XC,IGAM,INOR,XGAM,
     +     XNOR,BOUND,icons,beta,info,nrr,nccov,ndcov,hess,itoev,
     +     iter_bf,iplus1,iplus2,ipedig,err)
C*********************************************************************** 
C  COMPUTE THE VALUE MINIMIZING F

      include 'parinclu.h'

      INTEGER NDIM,NCCOV,NDCOV,NJOINT,ICONS(0:NDIMAX)
      INTEGER ICHECK,INTEGAM,NDIMB,NRAND,MAXRAND,I,J
      integer nrr,err,iter_bf,itoev,IPEDIG(4,NDIMAX)
      INTEGER NWITHIN,IWITHCOL,NINTTDEP,NDENS
      INTEGER IGAM(0:NCCOV+NDCOV,3),INOR(0:NCCOV+NDCOV,3),IUSER(3)
      INTEGER INFO(NRR,4),iplus1(nccov),iplus2(ndcov)
      real(8) HESS(NDIMAX,NDIMAX),EPS_BF
      real(8) BETA(NDIMAX),GRADF(NDIMAX),BOUND(NCCOV+NDCOV,3)
      real(8) FC,OBJ,XC,XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
c
      COMMON/BL8/NRAND,MAXRAND  
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL14/NDIM
c
      J=0
      DO 1 I=1,MAXRAND
        IF (BOUND(I,1).NE.0.D0)  J = I
  1   CONTINUE
      IF (IGAM(J,1).NE.0) THEN
        XGAM(J)=XC
      ELSE
        XNOR(J)=XC
      ENDIF    
      IUSER(3)=99
      CALL OPTIMIZE(NDIM,FC,OBJ,GRADF,BETA,EPS_BF,IUSER,IGAM,INOR,
     +     XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,hess,itoev,
     +     iter_bf,iplus1,iplus2,ipedig,err)
      FTOMIN = OBJ  
      RETURN       
C*********************************************************************** 
      END 
      SUBROUTINE FWEIB(BETA,F,GRADF,NDIM,NSTATE,IUSER,VALRHO,IPEDIG,
     +     IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,err)   
C*********************************************************************** 
C  FWEIB         *                                    *  27 / 03 /1995 * 
C*********************************************************************** 
C  COMPUTE THE FINAL VALUE OF - LOG LIKELIHOOD AND ITS VECTOR OF FIRST * 
C       DERIVATIVES                                                    * 
C*********************************************************************** 
c 

	include 'parinclu.h'
c 
      INTEGER NSTATE,IUSER(3)  
C 
      INTEGER NDIM,ICHECK,LAST,NCCOV,NDCOV,ICRHO,LAST1,nrr
      INTEGER I,I2,J,JJ,K,KK,NREC,ICODE,IPRETIM,ITIM,IQ,IVALQ                 
      INTEGER ICONS(0:NDIMAX),IANAL(MXEF),err
      INTEGER NSTRATA,NRULE,NRELMAT,NANI,NRELMAT2     
      INTEGER IPEDIG(4,NDIMAX),POINT(NDIMAX)
      INTEGER IA,IP,IM,IGPM,ISTR,IDF,NDCOV0,NDCOV1,NDCOVP1
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)
      INTEGER INTEGAM,NDIMB,NDIMBPS,NJOINT,NINTTDEP 
      INTEGER NRAND,MAXRAND,IGAM(0:NCCOV+NDCOV,3)
      INTEGER NDENS,MAXTIM,NO_LOG,INOR(0:NCCOV+NDCOV,3)
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C added on December 10, 1996
      INTEGER KFIRST(NDIMAX),KLAST(NDIMAX),LISTYS(NDIMAX)  
      INTEGER IDHERD,NH,I1,MAXYS,NLISTYS,NCOND
      INTEGER NWITHIN,IWITHCOL
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      REAL XDATA(NRECMAX,MXEF_USED)
C 
      real(8) GAM0,XNQGAM,OMQGAM,R1GAM,OMEG,EPS_BF,CONST 
      real(8) F,U,W,W1,W2,W3,W4,XNS,XNQ,XCOV,X,DM1(7)    
      real(8) BETA(NDIMAX),GRADF(NDIMAX),DOMEG(NDIMAX)
      real(8) VECLOG(NTIMMAX),VECEXP(NTIMMAX,MXSTRA)
      real(8) VALRHO(MXSTRA),DLOGRHO(MXSTRA) 
      real(8) RHO,TRHO,TPRERHO,LRHO,DLTIM,DLPRETIM               
      real(8) GAMLOG,DIGAMA,GAMVAR,XNSFIX,BOUND(NCCOV+NDCOV,3)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) BETAWOC(2*MXSTRA+3)
C
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL16/VECLOG,VECEXP,BETAWOC,MAXTIM
      COMMON/BL18/DLOGRHO,NO_LOG
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA   
C
      EQUIVALENCE (IDATA(1,1),XDATA(1,1))  
      DATA (DM1(I),I=1,7)/1.D0,1.33333333333333D0,2.D0,                 
     +  1.D0,1.3333333333D0,1.06666666667D0,1.4545454545455D0/         
C          
C---------------------------------------------------------------------- 
C  INITIALIZATION                
C----------------------------------------------------------------------- 
C  
      NDCOV0 = NCCOV + 1 
      NDCOV1 = NCCOV + NDCOV 
      NDCOVP1 = NDCOV1 + 1 
 800  format(1x,a10,20F8.3)
      I2=0
      F = 0.D0    
      XNQ = 0.D0
      XNS = 0.D0 
      XNSFIX = 0.D0 
      OMEG=0.D0
      NDIMBPS= NDIMB + NSTRATA 
      MAXYS=1000
      IF (INTEGAM.NE.0) THEN
        IF (NJOINT.EQ.0) THEN
          GAMVAR = XGAM(INTEGAM)
        ELSE
          GAMVAR=DEXP(BETA(NDIM))
        ENDIF
      ENDIF
      DO 10 I = 1,NDIM          
        GRADF(I) = 0.D0           
        DOMEG(I) = 0.D0         
        POINT(I) = 0         
   10 CONTINUE 
      IPRETIM = 0  
C----------------------------------------------------------------------- 
c<<<<<<<< modified on April 17, 1997 for ICRHO=0 >>>>>>>>>>>>>>>>>>>>>>>
C  READ THE VALUE(S) OF RHO IF ICRHO=1
C----------------------------------------------------------------------- 
      IF (ICRHO.EQ.1) THEN 
        K = NSTRATA 
        IF (NO_LOG.EQ.1) THEN
          DO 11 I=1,NSTRATA
            VALRHO(I) = BETA(NDIMBPS+I)
            DLOGRHO(I) = DLOG(VALRHO(I))
  11      CONTINUE
        ELSE 
          DO 12 I=1,NSTRATA 
            DLOGRHO(I) = BETA(NDIMBPS+I) 
            VALRHO(I) = DEXP(DLOGRHO(I)) 
  12      CONTINUE 
        ENDIF
      ENDIF
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 13 I=1,NSTRATA
         VECEXP(1,I) =  1.D0 
	 DO 14 J=2,MAXTIM
         IF (VECLOG(J).NE.0.D0) THEN
           VECEXP(J,I) =  DEXP(VALRHO(I)*VECLOG(J))
	   ENDIF
  14     CONTINUE
  13  CONTINUE
C 
C CONSTRAINTS 
C 
      DO 19 I=1,ICONS(0) 
         BETA(ICONS(I)) =0.D0 
   19 CONTINUE 
C----------------------------------------------------------------------- 
C   RANDOM EFFECTS               
C----------------------------------------------------------------------- 
      IF (NRAND.NE.0) THEN       
       DO 300 I = 1,MAXRAND
       IF (IGAM(I,1).EQ.1.AND.IGAM(I,2).NE.0.AND.I.NE.INTEGAM) THEN             
         U = XGAM(I) * DLOG(XGAM(I)) - GAMLOG(XGAM(I)) 
         F = F - (IGAM(I,3) - IGAM(I,2) +1 ) * U
         DO 310 J = IGAM(I,2),IGAM(I,3)
             IF (BETA(J).LT.-20) BETA(J)=0.D0          
             W = DEXP(BETA(J))   
             F = F - XGAM(I) * (BETA(J) - W)     
             GRADF(J) = GRADF(J) - XGAM(I) * (1.D0 - W)
  310    CONTINUE            
       ELSE IF (INOR(I,1).EQ.1) THEN     
         CONST = - 0.5D0 * DLOG(XNOR(I))         
         IF (NRELMAT.EQ.0) THEN                  
           U = 1.D0 / XNOR(I)  
           DO 312 J = INOR(I,2),INOR(I,3)          
              IF (BETA(J).LT.-20) BETA(J)=0.D0          
              W = U * BETA(J)
              F = F -CONST + 0.5D0 * W * BETA(J)
              GRADF(J) = GRADF(J) + W
  312      CONTINUE          
         ELSE                
           U = 1.D0 / XNOR(NRELMAT2)               
           IF (NRULE.EQ.1.OR.NRULE.EQ.3) THEN      
             DO 320 K=1,NANI             
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LE.0) GOTO 320          
                   F = F - CONST 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                   IA = IPEDIG(1,K)                  
                   IP = IPEDIG(3,K)                  
                   IM = IPEDIG(4,K)  
                   X = BETA(IA)  
                   IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                 
                   IF (IM.GT.0) X = X - 0.5D0 * BETA(IM)                 
                   W = U * DM1(IPEDIG(2,K))          
                   X = W * X     
                   GRADF(IA) = GRADF(IA) + X
                   IF (IP.GT.0) GRADF(IP) = GRADF(IP) - 0.5D0 * X
                   IF (IM.GT.0) GRADF(IM) = GRADF(IM) - 0.5D0 * X
  320        CONTINUE        
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C Modified on June 27, 1997
             J=I
             IF (NRULE.EQ.3) J=I+1 
             DO 322 K=INOR(I,2),INOR(J,3)           
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
               F = F + 0.5D0 * BETA(K) * GRADF(K)  
  322        CONTINUE       
c  
           ELSE IF (NRULE.EQ.2) THEN               
             DO 330 K=1,NANI     
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LT.0) GOTO 330          
                   F = F - CONST 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                   IA = IPEDIG(1,K)                  
                   IP = IPEDIG(3,K)                  
                   IGPM = IPEDIG(4,K)                
                   X = BETA(IA)  
                   IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                 
                   IF (IGPM.GT.0) X = X - 0.25D0 * BETA(IGPM)            
                   W = U * DM1(IPEDIG(2,K))          
                   X = W * X     
                   GRADF(IA) = GRADF(IA) + X
                   IF (IP.GT.0) GRADF(IP) = GRADF(IP) - 0.5D0 * X
                   IF (IGPM.GT.0) GRADF(IGPM)= GRADF(IGPM) -0.25D0 * X
  330            CONTINUE        
               DO 332 K=INOR(I,2),INOR(I,3)      
                 F = F + 0.5D0 * BETA(K) * GRADF(K)  
  332          CONTINUE        
             ENDIF               
             ENDIF               
       ENDIF 
  300   CONTINUE                 
      ENDIF
C            
C----------------------------------------------------------------------- 
C   FOR EACH ELEMENTARY RECORD I :                   
C-----------------------------------------------------------------------   
      IQ = 0  
      LAST = -1 
      IPRETIM = 0  
      DLPRETIM = 0.D0 
      TPRERHO = 0.D0
C
 100  continue
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C added or modified on December 10, 1996
C  IF KEYWORD 'WITHIN' WAS USED WITH 'INTEGRATE_OUT', THIS PART
C  LOOKS AT THE TABLE OF RECORDS AND DETERMINES THE ORDER SUITABLE
C  FOR THE INTEGRATION: eg: year-season WITHIN herd. THIS ORDER IS
C  STORED IN INFO(*,3) AND INFO(*,4) IS UPDATED SO INFO(*,4) CORRESPONDING
C  TO DIFFERENT 'herds' ARE DIFFERENT. IT IS ASSUMED THAT THERE ARE
C  LESS THAN MAXYS 'year-seasons' per 'herd'.
      NCOND = NINTTDEP + NWITHIN
      IF (NWITHIN.NE.0) THEN
        IF (INFO(1,4).GT.MAXYS) GOTO 909 
        NH = MAXYS
        IDHERD = INFO(1,3)
        JJ = INFO(1,4)
        KFIRST(JJ) = 1
        KLAST(JJ) = 1
        NLISTYS= 1
        LISTYS(1)= JJ
        INFO(1,4)= NH +  JJ 
        DO 900 I=2, NREC
           IF (INFO(I,3).NE.IDHERD) THEN
              DO 910 I1 = 1,NLISTYS-1
                 JJ = LISTYS(I1) 
                 KK = LISTYS(I1+1) 
                 INFO(KLAST(JJ),3) = KFIRST(KK)
                 KFIRST(KK) = 0
                 KLAST(JJ) = 0
  910         CONTINUE
              NH = NH + MAXYS 
              IDHERD = INFO(I,3)
              KK = LISTYS(NLISTYS) 
              INFO(KLAST(KK),3)= I 
              KLAST(KK) = 0
              NLISTYS= 0
           ENDIF
           JJ = INFO(I,4)
           INFO(I,4)= NH + JJ 
           K = KLAST(JJ)
           IF (K.NE.0) THEN
              INFO(K,3) = I
c              INFO(K,4) = I
           ELSE
              NLISTYS= NLISTYS + 1
              LISTYS(NLISTYS)= JJ
              KFIRST(JJ)= I
           ENDIF
           KLAST(JJ)= I 
 900    CONTINUE 
        DO 930 I1 = 1,NLISTYS-1
          JJ = LISTYS(I1) 
          KK = LISTYS(I1+1) 
          INFO(KLAST(JJ),3) = KFIRST(KK)
          KLAST(JJ) = 0
  930   CONTINUE
        KK = LISTYS(NLISTYS)   
        INFO(KLAST(KK),3)= 0 
        KLAST(KK) = 0
      ENDIF
C 
 909  I=1
      DO 190 I1=1,NREC
        I2=I2+1
        IF (NWITHIN.EQ.0) THEN
          I=I1
        ELSE IF (I1.NE.1) THEN
          I = INFO(I,3)
        ENDIF
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        IVALQ = INFO(I,4)
C <<<<<<<<<<<<<<<<<<<<<<
C added October 16, 1996, modified (NCOND) December 10, 1996 
        ICODE = INFO(I,2)
        IF (NCOND.NE.0.AND.ICODE.EQ.-2) GOTO 190 
C <<<<<<<<<<<<<<<<<<<<<<
C----------------------------------------------------------------------- 
C   IF THE PREVIOUS RECORD WAS THE LAST RECORD WITH IVALQ = IQ      
C   UPDATE F (WEIBULL LIKELIHOOD) AND GRADF (ITS GRADIENT)           
C   NQ IS THE NUMBER OF OBSERVED FAILURES WITH IVALQ = IQ   
c 
c IF INTEGAM=0 ==> THERE IS NO INTEGRATION OF ANY RANDOM (LOGGAMMA) EFFECT 
c             ==> = REGULAR WEIBULL MODEL and LIKELIHOOD    
c IF INTEGAM >0 ==> the fixed effect indicated by IVALQ is integrated out 
c          assuming a gamma distribution    
C   + NJOINT=1 ==> and the corresponding parameter is estimated 
C            maximizing the posterior density jointly with the location parameters
C we need the first derivative of F with respect to this parameter too               
C----------------------------------------------------------------------- 
        IF (IQ.NE.0.AND.IVALQ.NE.IQ.AND.INTEGAM.NE.0) THEN
             GAM0= GAMVAR
             XNS=XNS+1.D0
           XNQGAM = XNQ + GAM0 
           OMQGAM = OMEG + GAM0 
           R1GAM = XNQGAM / OMQGAM
           F = F -GAMLOG(XNQGAM) + XNQGAM * DLOG(OMQGAM)
           IF (NJOINT.EQ.1.AND.IQ.GT.0)  GRADF(NDIM) = GRADF(NDIM) 
     +       - DIGAMA(XNQGAM) + DLOG(OMQGAM)+ R1GAM
C
   20    GRADF(LAST) = GRADF(LAST) + R1GAM * DOMEG(LAST)
         DOMEG(LAST) = 0.D0  
         LAST1 = POINT(LAST)     
         POINT(LAST) = 0 
         LAST = LAST1 
         IF (LAST.GT.0) GOTO 20      
C
         XNQ = 0.D0 
         OMEG = 0.D0  
         LAST = -1 
       ENDIF   
C----------------------------------------------------------------------- 
C   IF THE PREVIOUS RECORD HAS A DIFFERENT STRATUM ID ==> NEW STRATUM    
C   INITIALIZE CUM_RISK (CUMULATIVE RISK FOR ALL INDIVIDUALS) AND VECTOR     
C   DC_RISK (CUMULATED CONTRIBUTION FOR EACH LEVEL TO BE USED IN         
C   COMPUTING GRADF) + D2C_RISK (TO BE USED IN COMPUTING HESSIAN)        
C   
C  BUT IF NINTTDEP=1  , INFO(I,3) CONTAINS PREVIOUS TIME
C----------------------------------------------------------------------- 
       ITIM =INFO(I,1)        
       ICODE = INFO(I,2)   
       IF (NCOND.EQ.0) THEN
          ISTR = INFO(I,3)  
       ELSE
          ISTR=1
       ENDIF
       RHO = VALRHO(ISTR)    
       LRHO = DLOGRHO(ISTR)
       IQ = IVALQ  
C----------------------------------------------------------------------- 
C     W1 = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE                
C     FIRST, NCCOV CONTINUOUS COVARIATES, THEN NDCOV DISCRETE COVARIATES     
C     BETA = -999 = - INFINITY => W2 = EXP(W1) WILL BE = 0              
C----------------------------------------------------------------------- 
       K = 0  
       W1 = 0.D0                   
       DO 25 J=1,NCCOV    
         IF (IANAL(J).NE.0) THEN        
           XCOV = XDATA(I,J)    
           W1 = W1 + BETA(J) * XCOV  
           IF (BETA(J).LE.-999.D0) W1 = -999.9D0
         ENDIF
   25  CONTINUE               
       DO 30 J=NDCOV0,NDCOV1
         IF (IANAL(J).NE.0) THEN        
           K = IDATA(I,J)      
           IF (K.NE.0) W1 = W1 + BETA(K)
         ENDIF 
   30  CONTINUE 
       W1 = W1 + BETA(NDIMB+ISTR)
C          
C----------------------------------------------------------------------- 
C   DIVERGENCE ==> STOP          
C----------------------------------------------------------------------- 
       IF (W1.GT.100.D0) THEN
         err=3
         RETURN
       ENDIF
C          
C----------------------------------------------------------------------- 
C   COMPUTE W2 = EXP(W1) AND W3 = W2 * (ITIM**RHO - IPRETIM**RHO)   
C  IF ICRHO=1  COMPUTE:
C      IF NO_LOG=1    (MAXIMIZATION IS ON RHO): 
c    W4 = W2 * (ITIM**RHO * (LOG ITIM) - IPRETIM**RHO * (LOG IPRETIM)) 
C      IF NO_LOG^=1    (MAXIMIZATION IS ON LOG(RHO)): 
c    W4 = RHO * W4 ABOVE
C-----------------------------------------------------------------------
       W2 = 0.D0                
C          
       IF (W1.GT.-20.D0) W2 = DEXP(W1) 
       DLTIM = VECLOG(ITIM) 
       TRHO = VECEXP(ITIM,ISTR)   
C <<<<<<<<<<<<<<<<<<<<<<
C added October 16, 1996, modified (NCOND) December 10, 1996
      IF (ICODE.EQ.-2) THEN 
         IPRETIM= ITIM
         DLPRETIM = DLTIM 
         TPRERHO = TRHO 
         GOTO 190
      ENDIF
      IF (NCOND.NE.0) THEN
C <<<<<<<<<<<<<<<<<<<<<<
C
C BECAUSE THE ORDERING OF THE RECORDS DOES NOT PERMIT TO KEEP OLD
C VALUES OF DLTIM AND TRHO , THEY HAVE TO BE RECALCULATED
C
       IF (NINTTDEP.NE.0) THEN
          IPRETIM= INFO(I,3)
          IF (IPRETIM.EQ.0) THEN
              DLPRETIM=0.D0 
              TPRERHO=0.D0
           ELSE
	      DLPRETIM=VECLOG(IPRETIM)
              TPRERHO=VECEXP(IPRETIM,ISTR) 
           ENDIF
C <==> NWITHIN <> 0
C <<<<<<<<<<<<<<<<<<<<<<
C added February 27 1997
        ELSE IF (NWITHIN.NE.0) THEN 
C <<<<<<<<<<<<<<<<<<<<<<
C added December 10, 1996
          IF (I.EQ.1.OR.INFO(I-1,2).GE.0) THEN
C THE PREVIOUS RECORD CORRESPONDS TO ANOTHER ANIMAL
C <<<<<<<<<<<<<<<<<<<<<<
C added February 27 1997
            IPRETIM=0
            DLPRETIM=0.D0
            TPRERHO=0.D0
          ELSE IF (INFO(I-1,2).LT.0) THEN
C THE PREVIOUS RECORD CORRESPONDS TO THE SAME ANIMAL
            IPRETIM= INFO(I-1,1)
            DLPRETIM=VECLOG(IPRETIM)
            TPRERHO=VECEXP(IPRETIM,ISTR) 
          ENDIF
C <<<<<<<<<<<<<<<<<<<<<<
         ENDIF 
        ENDIF 
        W3 = W2 * (TRHO - TPRERHO)
c 
       OMEG = OMEG + W3
c        
       DO 55 J=1,NCCOV  
         IF (IANAL(J).NE.0) THEN       
           XCOV = XDATA(I,J)   
           IF (POINT(J).EQ.0) THEN  
             POINT(J) = LAST 
             LAST = J    
           ENDIF               
           DOMEG(J) = DOMEG(J) +  W3 * XCOV 
         ENDIF 
   55  CONTINUE               
       DO 70 J=NDCOV0,NDCOVP1 
         IF (J.LE.NDCOV1) THEN 
           IF (IANAL(J).EQ.0) GOTO 70  
           K = IDATA(I,J)
           IF (K.EQ.0) GOTO 70 
         ELSE 
           K = NDIMB+ISTR
         ENDIF            
         IF (POINT(K).EQ.0) THEN     
           POINT(K) = LAST 
           LAST = K  
         ENDIF     
         DOMEG(K) = DOMEG(K) +  W3  
   70  CONTINUE   
c 
C----------------------------------------------------------------------- 
c elements in first and 2nd derivatives with respect to rho's 
C----------------------------------------------------------------------- 
c 
       IF (ICRHO.EQ.1) THEN   
          W4 = W2 * (TRHO * DLTIM - TPRERHO * DLPRETIM)  
          IF (NO_LOG.NE.1) W4 = RHO * W4
          K = NDIMBPS + ISTR   
          IF (POINT(K).EQ.0) THEN       
            POINT(K) = LAST 
            LAST = K   
          ENDIF  
          DOMEG(K) = DOMEG(K) + W4  
       ENDIF 
C            
C----------------------------------------------------------------------- 
C   IF UNCENSORED RECORD, UPDATE F AND GRADF         
C----------------------------------------------------------------------- 
       K = 0 
       IF (ICODE.GE.1) THEN   
         XNQ = XNQ + 1.D0
         F = F - LRHO - (RHO - 1.D0) * DLTIM - W1
c
         DO 75 J=1,NCCOV  
           IF (IANAL(J).NE.0)           
     +        GRADF(J) = GRADF(J) - XDATA(I,J)
   75    CONTINUE   
         DO 80 J=NDCOV0,NDCOVP1 
           IF (J.LE.NDCOV1) THEN 
             IF (IANAL(J).NE.0) THEN 
               K = IDATA(I,J)
               IF (K.EQ.0) GOTO 80 
             ELSE 
               K=0  
             ENDIF 
           ELSE 
             K = NDIMB+ISTR
           ENDIF        
           IF (K.GT.0) GRADF(K) = GRADF(K) - 1.D0
   80    CONTINUE  

         IF (ICRHO.EQ.1) THEN 
           K = NDIMBPS + ISTR
           IF (NO_LOG.EQ.1) THEN
             GRADF(K) = GRADF(K) - (1.D0/RHO) - DLTIM
           ELSE 
             GRADF(K) = GRADF(K) - 1.D0 - RHO * DLTIM    
           ENDIF
         ENDIF              
       ENDIF      
  189  IF (NCOND.EQ.0) THEN
         IF (ICODE.GE.0) THEN    
           IPRETIM = 0  
           TPRERHO = 0.D0 
           DLPRETIM = 0.D0     
         ELSE 
           IPRETIM = ITIM   
           TPRERHO = TRHO 
           DLPRETIM = DLTIM 
         ENDIF   
       ENDIF
c           
  190 CONTINUE                   
C----------------------------------------------------------------------- 
C   LAST CONTRIBUTION TO F AND GRADF                 
C----------------------------------------------------------------------- 
      IF (INTEGAM.EQ.0) THEN     
C----------------------------------------------------------------------- 
C   REGULAR WEIBULL MODEL              
C-----------------------------------------------------------------------    
         F = F + OMEG
         R1GAM = 1.D0 
      ELSE     
C----------------------------------------------------------------------- 
C   WEIBULL MODEL WITH INTEGRATION OF A FRAILTY TERM       
C----------------------------------------------------------------------- 
          GAM0= GAMVAR
          XNS=XNS+1.D0
        XNQGAM = XNQ + GAM0 
        OMQGAM = OMEG + GAM0 
        R1GAM = XNQGAM / OMQGAM   
c  (note: some of these terms are constant when GAM0=constant)
        F = F - XNS * (GAMVAR * DLOG(GAMVAR) - GAMLOG(GAMVAR))
        F = F -GAMLOG(XNQGAM) + XNQGAM * DLOG(OMQGAM)
C derivative with respect to GAM0
        IF (NJOINT.EQ.1.AND.IQ.GT.0) 
     +          GRADF(NDIM) = GRADF(NDIM) 
     +            - DIGAMA(XNQGAM) + DLOG(OMQGAM)+ R1GAM
        IF (NJOINT.EQ.1) THEN 
           GRADF(NDIM) = GRADF(NDIM) 
     +            - XNS * (1.D0 + DLOG(GAMVAR) - DIGAMA(GAMVAR))
           GRADF(NDIM) = GAMVAR * GRADF(NDIM)
        ENDIF
      ENDIF  
c  
  220 X = DOMEG(LAST)     
      GRADF(LAST) = GRADF(LAST) + R1GAM * X
      DOMEG(LAST) = 0.D0 
      LAST1 = POINT(LAST)     
      POINT(LAST) = 0   
      LAST = LAST1 
      IF (LAST.GT.0) GOTO 220   
c 
      XNQ = 0.D0 
      OMEG = 0.D0 
      LAST = -1 
C----------------------------------------------------------------------- 
C   CONSTRAINTS ON GRADF + COUNT NON ZERO PARAMETERS 
C----------------------------------------------------------------------- 
      DO 250 I = 1,ICONS(0)
        GRADF(ICONS(I)) = 0.D0    
  250 CONTINUE                   
      IDF=0  
      DO 260 J=1,NDIM            
        IF (GRADF(J).NE.0.D0) IDF = IDF + 1           
  260 CONTINUE                   
      IUSER(1) = IDF
      IF (NSTATE.EQ.1) THEN      
        IUSER(2) = 1
      ELSE
        IUSER(2) = IUSER(2) + 1   
      ENDIF 
c
      RETURN 
C*********************************************************************** 
      END  
      SUBROUTINE FWEIB2D(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,
     +VALRHO,IPEDIG,IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,
     +ndcov,hess,err)
C*********************************************************************** 
C  FWEIB2D       *                                    *  23 / 04 /1996 * 
C*********************************************************************** 
C  COMPUTE THE FINAL VALUE OF - WEIB' PARTIAL LIKELIHOOD, ITS VECTOR OF* 
C       DERIVATIVES, THE MATRIX OF SECOND DERIVATIVES TO COMPUTE STD=  * 
C       VECTOR OF STANDARD ERRORS OF THE ESTIMATES                     *
C  "DENSE VERSION"
C***********************************************************************

	include 'parinclu.h'
c 
      INTEGER NDIM,ICHECK,LAST,NCCOV,ICRHO,NJOINT,LAST1,LAST2,err
      INTEGER I,J,K,KK,JJ,J1,J2,K2,M1,M2,ICODE,IPRETIM,ITIM,IQ,IVALQ
      INTEGER NSTRATA,NRULE,NRELMAT,NANI,NRELMAT2     
      INTEGER NDCOV,NDCOV0,NDCOV1,NDCOVP1        
      INTEGER IA,IP,IM,IGPM,ISTR,JOB,NREC,NO_LOG
      INTEGER INTEGAM,NDIMB,NDIMBPS,nrr
      INTEGER ICONS(0:NDIMAX),IPEDIG(4,NDIMAX),POINT(NDIMAX) 
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C added on December 10, 1996
      INTEGER KFIRST(NDIMAX),KLAST(NDIMAX),LISTYS(NDIMAX)  
      INTEGER IDHERD,NH,I1,MAXYS,NLISTYS,NCOND
      INTEGER NWITHIN,IWITHCOL
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      INTEGER NRAND,MAXRAND,IGAM(0:NCCOV+NDCOV,3)
      INTEGER NDENS,MAXTIM,INOR(0:NCCOV+NDCOV,3),IANAL(MXEF)
      INTEGER NSTATUS,NOPTION,NINTTDEP
C         
      REAL XDATA(NRECMAX,MXEF_USED)   
C
      real(8) HESS(NDIMAX,NDIMAX),D2OMEG(NDIMAX,NDIMAX)
      real(8) GAM0,CONST,XNQGAM,OMQGAM,R1GAM,R2GAM,R3GAM,OMEG      
      real(8) VECLOG(NTIMMAX),VECEXP(NTIMMAX,MXSTRA)
      real(8) F,U,W,W1,W2,W3,W4,W5,XNS,XNQ,XCOV,XCOV2,X,DM1(7)    
      real(8) BETA(NDIMAX),GRADF(NDIMAX),DOMEG(NDIMAX),DIAG2OM(NDIMAX) 
      real(8) VALRHO(MXSTRA),DLOGRHO(MXSTRA) 
      real(8) RHO,TRHO,TPRERHO,LRHO,DLTIM,DLPRETIM               
      real(8) GAMLOG,DIGAMA,TRIGAM,GAMVAR,XNSFIX
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) DLDET,WADD,WADD1,WADD2,BOUND(NCCOV+NDCOV,3)
      real(8) STD(NDIMAX),EPS_BF
      real(8) BETAWOC(2*MXSTRA+3)
C
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL16/VECLOG,VECEXP,BETAWOC,MAXTIM
      COMMON/BL18/DLOGRHO,NO_LOG
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA
C
      EQUIVALENCE (IDATA(1,1),XDATA(1,1))   
      DATA (DM1(I),I=1,7)/1.D0,1.33333333333333D0,2.D0,                  
     +  1.D0,1.3333333333D0,1.06666666667D0,1.4545454545455D0/           
C            
C----------------------------------------------------------------------- 
C  INITIALIZATION                
C----------------------------------------------------------------------- 
C 
      NDCOV0 = NCCOV + 1 
      NDCOV1 = NCCOV + NDCOV 
      NDCOVP1 = NDCOV1 + 1  
      F = 0.D0    
      XNS = 0.D0
      XNQ = 0.D0 
      XNSFIX = 0.D0 
      NDIMBPS = NDIMB + NSTRATA

      MAXYS=1000
      IF (INTEGAM.NE.0) THEN
        IF (NJOINT.EQ.0) THEN
          GAMVAR = XGAM(INTEGAM) 
        ELSE
          GAMVAR=DEXP(BETA(NDIM))
        ENDIF
      ENDIF 
c 
C----------------------------------------------------------------------- 
C INITIALIZING  
C----------------------------------------------------------------------- 
      DO 8 J=1,NDIM
        DO 9 I= 1,NDIM 
          HESS(J,I) = 0.D0 
    9   CONTINUE
c        POINTOM(J)=0 
    8 CONTINUE
      DO 10 I = 1,NDIM 
        GRADF(I) = 0.D0           
        DOMEG(I) = 0.D0         
        DIAG2OM(I) = 0.D0 
        POINT(I) = 0         
   10 CONTINUE
C 
      IPRETIM = 0  
C---------------------------------------------------------------------- 
c<<<<<<<< modified on April 17, 1997 for ICRHO=0 >>>>>>>>>>>>>>>>>>>>>>>
C  READ THE VALUE(S) OF RHO IF ICRHO=1
C---------------------------------------------------------------------- 
      IF (ICRHO.EQ.1) THEN 
        K = NSTRATA 
        IF (NO_LOG.EQ.1) THEN
          DO 11 I=1,NSTRATA
            VALRHO(I) = BETA(NDIMBPS+I)
            DLOGRHO(I) = DLOG(VALRHO(I))
  11      CONTINUE
        ELSE 
          DO 12 I=1,NSTRATA 
            DLOGRHO(I) = BETA(NDIMBPS+I) 
            VALRHO(I) = DEXP(DLOGRHO(I)) 
  12      CONTINUE 
        ENDIF
      ENDIF
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 13 I=1,NSTRATA
        VECEXP(1,I) =  1.D0 
        DO 14 J=2,MAXTIM
	      IF (VECLOG(J).NE.0.D0) THEN
             VECEXP(J,I) =  DEXP(VALRHO(I)*VECLOG(J))
	      ENDIF
  14    CONTINUE
  13  CONTINUE
C----------------------------------------------------------------------- 
C  CONSTRAINT(S)                 
C----------------------------------------------------------------------- 
      IF (ICHECK.EQ.0) THEN      
        DO 19 I = 1,ICONS(0)      
          BETA(ICONS(I)) = 0.D0 
   19   CONTINUE                 
      ENDIF  
C----------------------------------------------------------------------- 
C   RANDOM EFFECTS               
C----------------------------------------------------------------------- 
      IF (NRAND.NE.0) THEN       
       DO 300 I = 1,MAXRAND 
       IF (IGAM(I,1).EQ.1.AND.IGAM(I,2).NE.0.AND.I.NE.INTEGAM) THEN             
         U = XGAM(I) * DLOG(XGAM(I)) - GAMLOG(XGAM(I))               
         F = F - (IGAM(I,3) - IGAM(I,2) +1 ) * U
         DO 310 J = IGAM(I,2),IGAM(I,3)          
             W = DEXP(BETA(J))   
             F = F  - XGAM(I) * (BETA(J) - W)     
             GRADF(J) = GRADF(J) - XGAM(I) * (1.D0 - W)  
	     WADD = XGAM(I) * W  
	     HESS(J,J) = HESS(J,J) + WADD
  310    CONTINUE            
       ELSE IF (INOR(I,1).EQ.1) THEN
           CONST = - 0.5D0 * DLOG(XNOR(I))         
           IF (NRELMAT.EQ.0.OR.I.NE.NRELMAT2) THEN  
             U = 1.D0 / XNOR(I)
             DO 312 J = INOR(I,2),INOR(I,3)        
               W = U * BETA(J)
               F = F - CONST + 0.5D0 * W * BETA(J)
               GRADF(J) = GRADF(J) + W  
	       HESS(J,J) = HESS(J,J) + U 
  312        CONTINUE          
           ELSE                
             U = 1.D0 / XNOR(NRELMAT2)              
             IF (NRULE.EQ.1.OR.NRULE.EQ.3) THEN     
             DO 320 K=1,NANI    
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
               IF (IPEDIG(2,K).LE.0) GOTO 320
               F = F - CONST          
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
               IA = IPEDIG(1,K)                  
               IP = IPEDIG(3,K)                  
               IM = IPEDIG(4,K)  
               X = BETA(IA)  
               IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                 
               IF (IM.GT.0) X = X - 0.5D0 * BETA(IM)                 
               W = U * DM1(IPEDIG(2,K))          
               X = W * X     
               GRADF(IA) = GRADF(IA) + X   
	       HESS(IA,IA) = HESS(IA,IA) + W 
c     
               IF (IP.GT.0) THEN                 
                  GRADF(IP) = GRADF(IP) - 0.5D0 * X     
c 
                  J1 = MIN(IA,IP) 
                  J2 = MAX(IA,IP)  
	          WADD = -0.5D0 * W 
	          HESS(J1,J2) = HESS(J1,J2) + WADD 
     	          WADD = 0.25D0 * W 
	          HESS(IP,IP) = HESS(IP,IP) + WADD 
                ENDIF    
                IF (IM.GT.0) THEN 
	          GRADF(IM) = GRADF(IM) - 0.5D0 * X     
            	  J1 = MIN(IA,IM) 
                  J2 = MAX(IA,IM)  
    	          WADD = -0.5D0 * W 
		  HESS(J1,J2) = HESS(J1,J2) + WADD 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C bug corrected on October 21, 1996
                  WADD = 0.25D0 * W
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		  HESS(IM,IM) = HESS(IM,IM) + WADD 
c      
                  IF (IP.GT.0) THEN  
                    J1 = MIN(IP,IM) 
                    J2 = MAX(IP,IM) 
		    WADD = 0.25D0 * W 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added  on October 21, 1996
C (corresponds to same group of unknown parents for sire and dam 
                  IF (IP.EQ.IM) WADD = 2.D0 * WADD
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
		    HESS(J1,J2) = HESS(J1,J2) + WADD 
                   ENDIF   
                 ENDIF       
  320          CONTINUE        
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C Modified on June 27, 1997
               J=I
               IF (NRULE.EQ.3) J=I+1 
               DO 322 K=INOR(I,2),INOR(J,3)           
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                 F = F + 0.5 * BETA(K) * GRADF(K)  
  322          CONTINUE       
c 
             ELSE IF (NRULE.EQ.2) THEN            
               DO 330 K=1,NANI  
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                 IF (IPEDIG(2,K).LE.0) GOTO 330          
                 F = F - CONST 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                 IA = IPEDIG(1,K)                  
                 IP = IPEDIG(3,K)                  
                 IGPM = IPEDIG(4,K)                
                 X = BETA(IA)  
                 IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                 
                 IF (IGPM.GT.0) X = X - 0.25D0 * BETA(IGPM)            
                 W = U * DM1(IPEDIG(2,K))          
                 X = W * X     
                 GRADF(IA) = GRADF(IA) + X   
		 HESS(IA,IA) = HESS(IA,IA) + W 
c 
                 IF (IP.GT.0) THEN                 
                   GRADF(IP) = GRADF(IP) - 0.5D0 * X 
c				      
	           J1 = MIN(IA,IP) 
                   J2 = MAX(IA,IP)  
                   WADD = -0.5D0 * W 
        	   HESS(J1,J2) = HESS(J1,J2) + WADD 
                   WADD = 0.25D0 * W 
		   HESS(IP,IP) = HESS(IP,IP) + WADD 
                 ENDIF                  
                 IF (IGPM.GT.0) THEN 				                
                   GRADF(IGPM) = GRADF(IGPM) - 0.25D0 * X     
c 
     	           J1 = MIN(IA,IGPM) 
                   J2 = MAX(IA,IGPM)  
	           WADD = -0.25D0 * W 
	  	   HESS(J1,J2) = HESS(J1,J2) + WADD 
     	           WADD = 0.0625D0 * W 
	  	   HESS(IGPM,IGPM) = HESS(IGPM,IGPM) + WADD 
c      
                   IF (IP.GT.0) THEN  
                     J1 = MIN(IP,IGPM) 
                     J2 = MAX(IP,IGPM) 
		     WADD = 0.125D0 * W 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added  on October 21, 1996
C (corresponds to same group of unknown parents for sire and dam 
                    IF (IP.EQ.IGPM) WADD = 2.D0 * WADD
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	  	     HESS(J1,J2) = HESS(J1,J2) + WADD 
                   ENDIF   
c 
                 ENDIF        
  330          CONTINUE        
               DO 332 K=INOR(I,2),INOR(I,3)        
                 F = F + 0.5 * BETA(K) * GRADF(K)               
  332          CONTINUE                 
             ENDIF               
           ENDIF               
       ENDIF 
  300   CONTINUE                 
      ENDIF     
C            
C----------------------------------------------------------------------- 
C   FOR EACH ELEMENTARY RECORD I :                   
C-----------------------------------------------------------------------   
      IQ = 0  
      LAST = -1 
      IPRETIM = 0  
      DLPRETIM = 0.D0 
      OMEG=0.D0
      TPRERHO = 0.D0
 100  continue
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C added or modified on December 10, 1996
C  IF KEYWORD 'WITHIN' WAS USED WITH 'INTEGRATE_OUT', THIS PART
C  LOOKS AT THE TABLE OF RECORDS AND DETERMINES THE ORDER SUITABLE
C  FOR THE INTEGRATION: eg: year-season WITHIN herd. THIS ORDER IS
C  STORED IN INFO(*,3) AND INFO(*,4) IS UPDATED SO INFO(*,4) CORRESPONDING
C  TO DIFFERENT 'herds' ARE DIFFERENT. IT IS ASSUMED THAT THERE ARE
C  LESS THAN MAXYS 'year-seasons' per 'herd'.
      NCOND = NINTTDEP + NWITHIN
      IF (NWITHIN.NE.0) THEN
        IF (INFO(1,4).GT.MAXYS) GOTO 909 
        NH = MAXYS
        IDHERD = INFO(1,3)
        JJ = INFO(1,4)
        KFIRST(JJ) = 1
        KLAST(JJ) = 1
        NLISTYS= 1
        LISTYS(1)= JJ
        INFO(1,4)= NH +  JJ 
        DO 900 I=2, NREC
           IF (INFO(I,3).NE.IDHERD) THEN
              DO 910 I1 = 1,NLISTYS-1
                 JJ = LISTYS(I1) 
                 KK = LISTYS(I1+1) 
                 INFO(KLAST(JJ),3) = KFIRST(KK)
                 KFIRST(KK) = 0
                 KLAST(JJ) = 0
  910         CONTINUE
              NH = NH + MAXYS 
              IDHERD = INFO(I,3)
              KK = LISTYS(NLISTYS) 
              INFO(KLAST(KK),3)= I 
              KLAST(KK) = 0
              NLISTYS= 0
           ENDIF
           JJ = INFO(I,4)
           INFO(I,4)= NH + JJ 
           K = KLAST(JJ)
           IF (K.NE.0) THEN
              INFO(K,3) = I
c              INFO(K,4) = I
           ELSE
              NLISTYS= NLISTYS + 1
              LISTYS(NLISTYS)= JJ
              KFIRST(JJ)= I
           ENDIF
           KLAST(JJ)= I 
 900    CONTINUE 
        DO 930 I1 = 1,NLISTYS-1
          JJ = LISTYS(I1) 
          KK = LISTYS(I1+1) 
          INFO(KLAST(JJ),3) = KFIRST(KK)
          KLAST(JJ) = 0
  930   CONTINUE
        KK = LISTYS(NLISTYS) 
        INFO(KLAST(KK),3)= 0 
        KLAST(KK) = 0
      ENDIF
C 
 909  I=1 
      DO 190 I1=1,NREC
        IF (NWITHIN.EQ.0) THEN
          I=I1
        ELSE IF (I1.NE.1) THEN
          I = INFO(I,3)
        ENDIF
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        IVALQ = INFO(I,4)
C <<<<<<<<<<<<<<<<<<<<<<
C added October 16, 1996, modified (NCOND) December 10, 1996 
        ICODE = INFO(I,2)
        IF (NCOND.NE.0.AND.ICODE.EQ.-2)  GOTO 190 
C <<<<<<<<<<<<<<<<<<<<<<
C-----------------------------------------------------------------------   
C   IF THE PREVIOUS RECORD WAS THE LAST RECORD WITH IVALQ = IQ      
C   UPDATE F (WEIBULL LIKELIHOOD) AND GRADF (ITS GRADIENT)           
C   NQ IS THE NUMBER OF OBSERVED FAILURES WITH IVALQ = IQ   
c 
c IF INTEGAM=0 ==> THERE IS NO INTEGRATION OF ANY RANDOM (LOGGAMMA) EFFECT 
c             ==> = REGULAR WEIBULL MODEL and LIKELIHOOD    
c IF INTEGAM >0 ==> the fixed effect indicated by IVALQ is integrated out 
c          assuming a gamma distribution    
C----------------------------------------------------------------------- 
         IF (IQ.NE.0.AND.IVALQ.NE.IQ.AND.INTEGAM.NE.0) THEN
           GAM0= GAMVAR
           XNS=XNS+1.D0
         XNQGAM = XNQ + GAM0 
         OMQGAM = OMEG + GAM0 
         R1GAM = XNQGAM / OMQGAM 
         R2GAM = R1GAM / OMQGAM 
         F = F - GAMLOG(XNQGAM) + XNQGAM * DLOG(OMQGAM)   
         IF (NJOINT.EQ.1.AND.IQ.GT.0) THEN
            R3GAM = (XNQ - OMEG) / (OMQGAM*OMQGAM)
            GRADF(NDIM) = GRADF(NDIM) 
     +            - DIGAMA(XNQGAM) + DLOG(OMQGAM)+ R1GAM
            WADD=  - TRIGAM(XNQGAM)
     +           + (GAM0 + 2.D0*OMEG - XNQ)/(OMQGAM*OMQGAM)
            WADD = GAMVAR * GAMVAR * WADD
	    HESS(NDIM,NDIM) = HESS(NDIM,NDIM) + WADD
         ENDIF

c 
         LAST1=LAST  
   16    LAST2=LAST1 
         IF (NJOINT.EQ.1.AND.IQ.GT.0) THEN
           WADD = -R3GAM * GAMVAR * DOMEG(LAST1) 
	   HESS(LAST1,NDIM) = HESS(LAST1,NDIM) + WADD
         ENDIF
   15    J1 = MIN(LAST1,LAST2) 
         J2 = MAX(LAST1,LAST2)  
	 WADD = - R2GAM * DOMEG(LAST1) * DOMEG(LAST2)
	 IF (J1.EQ.J2) THEN
	   WADD = WADD + R1GAM * DIAG2OM(J1)
	   DIAG2OM(J1)=0.D0
         ELSE
	   WADD = WADD + R1GAM * D2OMEG(J2,J1)
	   D2OMEG(J2,J1)=0.D0
         ENDIF
	   HESS(J1,J2) = HESS(J1,J2) + WADD
c
         LAST2 = POINT(LAST2) 
         IP = IP+1
         IF (LAST2.GT.0) THEN 
            GOTO 15  
         ELSE 
           LAST1 = POINT(LAST1)     
           IF (LAST1.GT.0) GOTO 16 
         ENDIF 
c  
   20    GRADF(LAST) = GRADF(LAST) + R1GAM * DOMEG(LAST)     
         DOMEG(LAST) = 0.D0  
         LAST1 = POINT(LAST)     
         POINT(LAST) = 0 
         LAST = LAST1 
         IF (LAST.GT.0) GOTO 20      
c 
         XNQ = 0.D0 
         OMEG = 0.D0  
         LAST = -1 
c        LASTOM = -1 
       ENDIF   
C----------------------------------------------------------------------- 
C   IF THE PREVIOUS RECORD HAS A DIFFERENT STRATUM ID ==> NEW STRATUM    
C   INITIALIZE CUM_RISK (CUMULATIVE RISK FOR ALL INDIVIDUALS) AND VECTOR     
C   DC_RISK (CUMULATED CONTRIBUTION FOR EACH LEVEL TO BE USED IN         
C   COMPUTING GRADF) + D2C_RISK (TO BE USED IN COMPUTING HESSIAN)        
C   
C  BUT IF NINTTDEP=1  , INFO(I,3) CONTAINS PREVIOUS TIME
C----------------------------------------------------------------------- 
       ITIM =INFO(I,1)        
       ICODE = INFO(I,2)   
       IF (NCOND.EQ.0) THEN
          ISTR = INFO(I,3)  
       ELSE
          ISTR=1
       ENDIF
       RHO = VALRHO(ISTR)    
       LRHO = DLOGRHO(ISTR)   
       IQ = IVALQ 
C----------------------------------------------------------------------- 
C     W1 = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE                
C     FIRST, NCCOV CONTINUOUS COVARIATES, THEN NDCOV DISCRETE COVARIATES     
C     BETA = -999 = - INFINITY => W2 = EXP(W1) WILL BE = 0              
C----------------------------------------------------------------------- 
       K = 0  
       W1 = 0.D0
       DO 25 J=1,NCCOV            
         XCOV = XDATA(I,J)    
         W1 = W1 + BETA(J) * XCOV  
         IF (BETA(J).LE.-999.D0) W1 = -999.9D0    
   25  CONTINUE               
       DO 30 J=NDCOV0,NDCOV1         
         K = IDATA(I,J)      
         IF (K.NE.0) W1 = W1 + BETA(K)        
   30  CONTINUE 
       W1 = W1 + BETA(NDIMB+ISTR)               
C            
C----------------------------------------------------------------------- 
C   DIVERGENCE ==> STOP          
C----------------------------------------------------------------------- 
       IF (W1.GT.100.D0) THEN   
         err=3
         RETURN  
       ENDIF 
C            
C----------------------------------------------------------------------- 
C   COMPUTE W2 = EXP(W1) AND W3 = W2 * (ITIM**RHO - IPRETIM**RHO)   
C  IF ICRHO=1  COMPUTE: 
C      IF NO_LOG=1    (MAXIMIZATION IS ON RHO): 
c    W4 = W2 * (ITIM**RHO * (LOG ITIM) - IPRETIM**RHO * (LOG IPRETIM)) 
c    W5=[W2*(ITIM**RHO * (LOG ITIM)**2 - IPRETIM**RHO * (LOG IPRETIM)**2)]    
C      IF NO_LOG^=1    (MAXIMIZATION IS ON LOG(RHO)): 
c    W4 = RHO * W4 ABOVE
C    W5 = W4 + RHO * RHO * W5 ABOVE 
C----------------------------------------------------------------------- 
       W2 = 0.D0                  
C            
       IF (W1.GT.-20.D0) W2 = DEXP(W1) 
       DLTIM = VECLOG(ITIM) 
c       DLTIM = DLOG(DBLE(ITIM))
       TRHO = VECEXP(ITIM,ISTR)   
c       TRHO = DEXP(RHO*DLTIM)   
C <<<<<<<<<<<<<<<<<<<<<<
C added October 16, 1996, modified (NCOND) December 10, 1996
      IF (ICODE.EQ.-2) THEN
         IPRETIM= ITIM
         DLPRETIM = DLTIM 
         TPRERHO = TRHO 
         GOTO 190
      ENDIF
      IF (NCOND.NE.0) THEN
C <<<<<<<<<<<<<<<<<<<<<<
       IF (NINTTDEP.NE.0) THEN
C
C BECAUSE THE ORDERING OF THE RECORDS DOES NOT PERMIT TO KEEP OLD
C VALUES OF DLTIM AND TRHO , THEY HAVE TO BE RECALCULATED
C
         IPRETIM= INFO(I,3)
         IF (IPRETIM.EQ.0) THEN
            DLPRETIM=0 
            TPRERHO=0
         ELSE
	    DLPRETIM=VECLOG(IPRETIM)
            TPRERHO=VECEXP(IPRETIM,ISTR) 
         ENDIF
C <==> NWITHIN <> 0
C <<<<<<<<<<<<<<<<<<<<<<
C added February 27 1997
        ELSE IF (NWITHIN.NE.0) THEN 
C <<<<<<<<<<<<<<<<<<<<<<
C added December 10, 1996
          IF (I.EQ.1.OR.INFO(I-1,2).GE.0) THEN
C THE PREVIOUS RECORD CORRESPONDS TO ANOTHER ANIMAL
C <<<<<<<<<<<<<<<<<<<<<<
C added February 27 1997
            IPRETIM=0
            DLPRETIM=0.D0
            TPRERHO=0.D0
          ELSE IF (INFO(I-1,2).LT.0) THEN
C THE PREVIOUS RECORD CORRESPONDS TO THE SAME ANIMAL
            IPRETIM= INFO(I-1,1)
            DLPRETIM=VECLOG(IPRETIM)
            TPRERHO=VECEXP(IPRETIM,ISTR) 
          ENDIF
        ENDIF
C <<<<<<<<<<<<<<<<<<<<<<
       ENDIF 
C
       W3 = W2 * (TRHO - TPRERHO)  
c 
       OMEG = OMEG + W3  
c        
       DO 55 J=1,NCCOV            
         XCOV = XDATA(I,J)   
         IF (POINT(J).EQ.0) THEN  
           POINT(J) = LAST 
           LAST = J    
         ENDIF 
         DOMEG(J) = DOMEG(J) +  W3 * XCOV 
c         ITOPL = J    
         DO 58 J2=J,NCCOV  
           XCOV2 = XDATA(I,J2)
    	   IF (J.EQ.J2) THEN
             DIAG2OM(J) = DIAG2OM(J) + XCOV * XCOV2 * W3
           ELSE
	     D2OMEG(J2,J) = D2OMEG(J2,J) + XCOV * XCOV2* W3
           ENDIF
   58    CONTINUE  
         DO 60 J2=NDCOV0,NDCOVP1 
           IF (J2.LE.NDCOV1) THEN  
             K2 = IDATA(I,J2)
             IF (K2.EQ.0) GOTO 60 
           ELSE 
             K2 = NDIMB+ISTR
           ENDIF           
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  added on November 26, 1996
           WADD1= XCOV * W3
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	   D2OMEG(K2,J) = D2OMEG(K2,J) + WADD1
   60    CONTINUE  
   55  CONTINUE               
       DO 70 J=NDCOV0,NDCOVP1 
         IF (J.LE.NDCOV1) THEN  
           K = IDATA(I,J)
           IF (K.EQ.0) GOTO 70 
         ELSE 
           K = NDIMB+ISTR
         ENDIF              
         IF (POINT(K).EQ.0) THEN     
           POINT(K) = LAST 
           LAST = K  
         ENDIF 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  added on November 26, 1996
         WADD1= W3
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         DOMEG(K) = DOMEG(K) +  WADD1
         DO 65 J2=J,NDCOVP1   
           IF (J2.LE.NDCOV1) THEN  
             K2 = IDATA(I,J2)
             IF (K2.EQ.0) GOTO 65 
           ELSE 
             K2 = NDIMB+ISTR
           ENDIF  
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  added on November 26, 1996
           WADD2= WADD1
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           M1 = MIN(K,K2)  
           M2 = MAX(K,K2) 
    	   IF (M1.EQ.M2) THEN
	     DIAG2OM(M1) = DIAG2OM(M1) +  WADD2
           ELSE
	     D2OMEG(M2,M1) = D2OMEG(M2,M1) +  WADD2 
           ENDIF
   65    CONTINUE       
   70  CONTINUE        
c 
C----------------------------------------------------------------------- 
c elements in first and 2nd derivatives with respect to rho's 
C----------------------------------------------------------------------- 
c 
       IF (ICRHO.EQ.1) THEN   
          W4 = W2 * (TRHO * DLTIM - TPRERHO * DLPRETIM)  
          IF (NO_LOG.NE.1) W4 = RHO * W4 
          K = NDIMBPS + ISTR   
          IF (POINT(K).EQ.0) THEN       
            POINT(K) = LAST 
            LAST = K   
          ENDIF  
          DOMEG(K) = DOMEG(K) + W4  
          W5 =  W2 * (TRHO *DLTIM *DLTIM -  
     +                             TPRERHO *DLPRETIM *DLPRETIM)    
          IF (NO_LOG.NE.1) W5 = W4 + RHO * RHO * W5 
c 
          DIAG2OM(K) = DIAG2OM(K) + W5
          DO 85 J2=1,NCCOV  
           XCOV2 = XDATA(I,J2) 
	    D2OMEG(K,J2) = D2OMEG(K,J2) + XCOV2 *  W4
   85    CONTINUE  
         DO 90 J2=NDCOV0,NDCOVP1 
           IF (J2.LE.NDCOV1) THEN  
             K2 = IDATA(I,J2) 
             IF (K2.EQ.0) GOTO 90
           ELSE 
             K2 = NDIMB+ISTR 
           ENDIF            
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C  added on November 26, 1996
           WADD2= W4
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	   D2OMEG(K,K2) = D2OMEG(K,K2) +   WADD2
   90    CONTINUE    
       ENDIF 
C            
C----------------------------------------------------------------------- 
C   IF UNCENSORED RECORD, UPDATE F AND GRADF         
C----------------------------------------------------------------------- 
       K = 0 
       IF (ICODE.GE.1) THEN   
         XNQ = XNQ + 1.D0    
         F = F - LRHO - (RHO - 1.D0) * DLTIM - W1           
         DO 75 J=1,NCCOV             
           GRADF(J) = GRADF(J) - XDATA(I,J)        
   75    CONTINUE               
         DO 80 J= NDCOV0,NDCOVP1 
           IF (J.LE.NDCOV1) THEN  
             K = IDATA(I,J)
             IF (K.EQ.0) GOTO 80 
           ELSE 
             K = NDIMB+ISTR 
           ENDIF        
           GRADF(K) = GRADF(K) - 1.D0              
   80    CONTINUE   

         IF (ICRHO.EQ.1) THEN 
           K = NDIMBPS + ISTR   
           IF (NO_LOG.EQ.1) THEN
             GRADF(K) = GRADF(K) - (1.D0/RHO)  - DLTIM   
             WADD = 1.D0/(RHO*RHO) 
           ELSE        
             GRADF(K) = GRADF(K) - 1.D0 - RHO * DLTIM  
    	     WADD = - RHO * DLTIM 
           ENDIF        
c
           HESS(K,K) = HESS(K,K) + WADD
         ENDIF              
       ENDIF      
  189  IF (NCOND.EQ.0) THEN
         IF (ICODE.GE.0) THEN    
           IPRETIM = 0  
           TPRERHO = 0.D0 
           DLPRETIM = 0.D0     
         ELSE 
           IPRETIM = ITIM   
           TPRERHO = TRHO 
           DLPRETIM = DLTIM 
         ENDIF 
       ENDIF  
C            
  190 CONTINUE                   
C            
C----------------------------------------------------------------------- 
C   LAST CONTRIBUTION TO F AND GRADF                 
C----------------------------------------------------------------------- 
      IF (INTEGAM.EQ.0) THEN     
C----------------------------------------------------------------------- 
C   REGULAR WEIBULL MODEL              
C-----------------------------------------------------------------------  
         F = F + OMEG      
         R1GAM = 1.D0 
      ELSE     
C----------------------------------------------------------------------- 
C   WEIBULL MODEL WITH INTEGRATION OF A FRAILTY TERM              
C-----------------------------------------------------------------------  
          GAM0= GAMVAR
          XNS=XNS+1.D0
        XNQGAM = XNQ + GAM0 
        OMQGAM = OMEG + GAM0 
        R1GAM = XNQGAM / OMQGAM 
        R2GAM = R1GAM / OMQGAM    
C derivative with respect to GAMVAR
        F = F - XNS * (GAMVAR * DLOG(GAMVAR) - GAMLOG(GAMVAR))
        F = F -GAMLOG(XNQGAM) + XNQGAM * DLOG(OMQGAM)
        IF (NJOINT.EQ.1.AND.IQ.GT.0)  THEN
          R3GAM = (XNQ - OMEG) / (OMQGAM*OMQGAM)
          GRADF(NDIM) = GRADF(NDIM) 
     +         - DIGAMA(XNQGAM) + DLOG(OMQGAM)+ R1GAM
          WADD= - TRIGAM(XNQGAM)
     +         + (GAMVAR + 2.D0*OMEG - XNQ)/(OMQGAM*OMQGAM)
          WADD = GAMVAR * GAMVAR * WADD 
	  HESS(NDIM,NDIM) = HESS(NDIM,NDIM) + WADD
        ENDIF  
        IF (NJOINT.EQ.1)  THEN
          GRADF(NDIM) = GRADF(NDIM) 
     +         - XNS * (1.D0 + DLOG(GAMVAR) - DIGAMA(GAMVAR))
          WADD= - XNS * (1.D0/GAMVAR - TRIGAM(GAMVAR))
          WADD = GAMVAR * (GAMVAR * WADD + GRADF(NDIM))
	  HESS(NDIM,NDIM) = HESS(NDIM,NDIM) + WADD
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added on July 15, 1997
          GRADF(NDIM) = GAMVAR * GRADF(NDIM)
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        ENDIF  
      ENDIF  
c  
        LAST1=LAST  
  214   LAST2=LAST1  
        IF (INTEGAM.NE.0) THEN
          IF (NJOINT.EQ.1.AND.IQ.GT.0) THEN
             WADD = -R3GAM * GAMVAR * DOMEG(LAST1) 
  	     HESS(LAST1,NDIM) = HESS(LAST1,NDIM) + WADD
          ENDIF
        ENDIF
c
  215   J1 = MIN(LAST1,LAST2) 
        J2 = MAX(LAST1,LAST2)  
        IF (INTEGAM.EQ.0) THEN    
          X = 1.D0
          R2GAM=1.D0
          WADD=0.D0 
        ELSE 
          X = R1GAM 
	  WADD = - R2GAM * DOMEG(LAST1) * DOMEG(LAST2) 
        ENDIF  
	IF (J1.EQ.J2) THEN
	   WADD = WADD + X * DIAG2OM(J1)
	   DIAG2OM(J1)=0.D0
        ELSE
           WADD = WADD + X * D2OMEG(J2,J1)
  	   D2OMEG(J2,J1)=0.D0
        ENDIF
	HESS(J1,J2) = HESS(J1,J2) + WADD
c  
        LAST2 = POINT(LAST2)     
        IF (LAST2.GT.0) THEN 
          GOTO 215  
        ELSE 
          LAST1 = POINT(LAST1)     
          IF (LAST1.GT.0) GOTO 214 
        ENDIF   
c 
  220 X = DOMEG(LAST)     
      GRADF(LAST) = GRADF(LAST) + R1GAM * X       
      DOMEG(LAST) = 0.D0 
      LAST1 = POINT(LAST)     
      POINT(LAST) = 0   
      LAST = LAST1 
      IF (LAST.GT.0) GOTO 220   
c 
      XNQ = 0.D0 
      OMEG = 0.D0 
      LAST = -1 
C          
C---------------------------------------------------------------------- 
C   CONSTRAINTS ON GRADF AND HESSIAN                
C----------------------------------------------------------------------
       ICODE=0
       IF (NOPTION.GE.90) THEN
C---------------------------------------------------------------------- 
C  NUMERICAL FACTORIZATION TO LOOK FOR CONSTRAINTS  
C---------------------------------------------------------------------- 
         IF (NDCOV.NE.0) THEN      
           ICONS(0)=1
           J= ICONS(0)
           ICONS(J)=NCCOV+1
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c added on February 13, 1997
  204      IF (BETA(ICONS(J)).lt.-998.D0.AND.ICONS(J).LT.NDIMB) THEN
             ICONS(0) = ICONS(0)+1 
             J = J + 1
             ICONS(J) = ICONS(J-1)+1
             GOTO 204
           ENDIF
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         ELSE
           RETURN                
         ENDIF 
         ICODE=1
         CALL CHOLESKY(HESS,NDIMAX,NDIMB,J,ICODE,icons,err)
         if(err.gt.0)return
C Delete useless constraints (due to no observed failures)
         J=0
         DO 207 I=2,ICONS(0)
           IF (BETA(ICONS(I)).GT.-998.D0) THEN
             J=J+1
             ICONS(J)=ICONS(I)
           ENDIF   
  207    CONTINUE
         DO 208 I=J+1,ICONS(0)
           ICONS(I)=0
  208    CONTINUE
         ICONS(0)=J
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IF (NOPTION.EQ.90) RETURN
       ENDIF     
C
       IF (ICHECK.NE.1) THEN  
        DO 210 I = 1,ICONS(0)   
          GRADF(ICONS(I)) = 0.D0  
          DO 209 J=1,NDIM   
            J1 = MIN(J,ICONS(I)) 
            J2 = MAX(J,ICONS(I))   
            HESS(J1,J2) =0.D0 
  209     CONTINUE              
  210   CONTINUE 
      ENDIF
C---------------------------------------------------------------------- 
C   END OF THE CONSTRUCTION OF THE HESSIAN          
C----------------------------------------------------------------------
      DO 750 J=1,NDIM
        DO 752 I=J,NDIM
           HESS(I,J)=HESS(J,I)
  752   CONTINUE
  750 CONTINUE
c      
       DLDET=0.D0
C---------------------------------------------------------------------- 
C  NUMERICAL FACTORIZATION             
C---------------------------------------------------------------------- 
       CALL CHOLESKY(HESS,NDIMAX,NDIM,J,ICODE,icons,err)
       if(err.gt.0)return
C
C---------------------------------------------------------------------- 
C  IF NOPTION = 1 ==> COMPUTATION OF THE LOG-DETERMINANT              
C---------------------------------------------------------------------- 
      IF (NOPTION.EQ.1.OR.ICHECK.GT.0) THEN
	JOB=10
	CALL INVDET(HESS,NDIMAX,NDIM,DLDET,JOB)
      ENDIF
C----------------------------------------------------------------------- 
C  IF NOPTION = 0 ==> COMPUTATION OF THE STANDARD DEVIATIONS                  
C----------------------------------------------------------------------- 
      IF (NOPTION.EQ.0.OR.ICHECK.GT.0) THEN
        JOB=1
	CALL INVDET(HESS,NDIMAX,NDIM,DLDET,JOB)
        DO 760 I=1,NDIM
c 3 lines added JKL
        DO 762 J=I,NDIM
           HESS(J,I)=HESS(I,J)
  762   CONTINUE
          STD(I) = HESS(I,I)
          IF (STD(I).GT.1.D-10) THEN
             STD(I) = DSQRT(STD(I))
          ELSE IF (STD(I).LT.-1.D-6) THEN
             err=2
          ELSE
            STD(I)=0.D0
          ENDIF
  760   CONTINUE 
      ENDIF 
      RETURN 
C*********************************************************************** 
      END      
      SUBROUTINE PREDICTED(BETA,SQUANT,VALRHO,ISTIME,nrr,nccov,ndcov,
     +     nstime,nquant,info2,nrr2,surv)
C***********************************************************************
C  PREDICTED   *                                      *  22 / 02 /1996 *
C***********************************************************************
C  COMPUTE THE VALUES OF S(T) FOR GIVEN T or T FOR GIVEN S(T) FOR DUMMY*
C     RECORDS DEFINED BY THE "FUTURE" STATEMENT IN PREPARE             *
C***********************************************************************
c

	include 'parinclu.h'
c       
      INTEGER NDIMB,NCCOV,NDCOV,NO_LOG,nrr,nrr2,nn
      INTEGER NSTIME,NQUANT
      INTEGER ICODE,IS,IQ,IT,ITIM 
      INTEGER I,J,K,ISTR,MAXTIM           
      INTEGER NDENS,NSTRATA,ICRHO      
      INTEGER NDCOV0,NDCOV1,NDCOVP1,ICHECK
      INTEGER INTEGAM,NDIMBPS,NJOINT,NINTTDEP  
      INTEGER NWITHIN,IWITHCOL
      INTEGER IANI,IANIPREV,IREST(NSTIMAX),IRESQ(NSTIMAX)
      INTEGER ISTIME(NSTIME),IANAL(MXEF)
      INTEGER INFO2(nrr2,4),IDATA2(MXEF_USED,NRECMAX2)                   
C           
      REAL XDATA2(MXEF_USED,NRECMAX2) 
      real(8) SQUANT(NQUANT),surv(nstimax*nrr,3)
      real(8) CUM_RISK,U,W1,W2,W3,TRHO,XRHO,OMEG,EPS_BF 
      real(8) SX,RHO,XCOV,DLXST,REST(NSTIMAX)    
      real(8) BETA(NDIMAX),TPRERHO,LRHO,DLTIM
      real(8) BETAWOC(2*MXSTRA+3)
      real(8) VALRHO(MXSTRA),DLOGRHO(MXSTRA)
      real(8) VECLOG(NTIMMAX),VECEXP(NTIMMAX,MXSTRA)
C
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL16/VECLOG,VECEXP,BETAWOC,MAXTIM
      COMMON/BL18/DLOGRHO,NO_LOG
      COMMON/DATA2/IDATA2

      EQUIVALENCE (IDATA2(1,1),XDATA2(1,1))         
C           
C-----------------------------------------------------------------------
C  INITIALIZATION               
C-----------------------------------------------------------------------
C
      NDCOV0 = NCCOV + 1 
      NDCOV1 = NCCOV + NDCOV 
      NDCOVP1 = NDCOV1 + 1 
      OMEG=0.D0
      NDIMBPS= NDIMB + NSTRATA
C---------------------------------------------------------------------- 
c<<<<<<<< modified on April 17, 1997 for ICRHO=0 >>>>>>>>>>>>>>>>>>>>>>>
C  READ THE VALUE(S) OF RHO IF ICRHO=1
C---------------------------------------------------------------------- 
      IF (ICRHO.EQ.1) THEN 
        K = NSTRATA 
          DO 11 I=1,NSTRATA
            VALRHO(I) = BETA(NDIMBPS+I)
            DLOGRHO(I) = DLOG(VALRHO(I))
  11      CONTINUE
      ENDIF
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO 13 I=1,NSTRATA
         VECEXP(1,I) =  1.D0
         VECLOG(1)=0.D0 
	   DO 14 J=2,MAXTIM
             VECLOG(J) =  DLOG(DBLE(J)) 
             VECEXP(J,I) =  DEXP(VALRHO(I)*VECLOG(J))
  14     CONTINUE
  13  CONTINUE
C-----------------------------------------------------------------------
C  COMPUTATION OF (PART OF) THE SURVIVOR CURVE FOR INDIVIDUALS IN FILE(10)  
C-----------------------------------------------------------------------
      IANIPREV=-999             
      IT = 1
      IS=1
      IQ=1
      TPRERHO=0.D0
 300  continue
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C           
C-----------------------------------------------------------------------
C   FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
      nn=0
       DO 390 I=1,NRr2
         ITIM=INFO2(I,1) 
         ICODE=INFO2(I,2)
         ISTR=INFO2(I,3)
         IANI=INFO2(I,4) 
         RHO = VALRHO(ISTR)    
         LRHO = DLOGRHO(ISTR)          
         IF (IANI.NE.IANIPREV) THEN
            IF (IANIPREV.NE.-999) THEN
              DO 270 J = 1,IT-1
                 nn=nn+1
                 surv(nn,1)=IANIPREV
                 surv(nn,2)=irest(j)
                 surv(nn,3)=rest(j)
                REST(J)=0.D0       
  270         CONTINUE          
              DO 275 J = 1,NQUANT
                 nn=nn+1
                 surv(nn,1)=IANIPREV
                 surv(nn,2)=iresq(j)
                 surv(nn,3)=squant(j)
                IRESQ(J)=0     
  275         CONTINUE         
              OMEG = 0.D0      
              TPRERHO=0.D0     
              IS = 1           
              IQ = 1           
              IT = 1           
            ENDIF               
         ENDIF
         IANIPREV = IANI 
C---------------------------------------------------------------------- 
C     W1 = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE            
C     FIRST, NCCOV CONTINUOUS COVARIATES, THEN NDCOV DISCRETE COVARIATES
C     BETA = -999 = - INFINITY => W2 = EXP(W1) WILL BE = 0              
C----------------------------------------------------------------------
         W1 = 0.D0              
         DO 25 J=1,NCCOV       
           XCOV = XDATA2(J,I)  
           W1 = W1 + BETA(J) * XCOV
           IF (BETA(J).LE.-999.D0) W1 = -999.9D0  
   25    CONTINUE               
         DO 30 J=NDCOV0,NDCOV1  
           K = IDATA2(J,I)      
           W1 = W1 + BETA(K)
   30    CONTINUE 
         W1 = W1 + BETA(NDIMB+ISTR) 
C               
C----------------------------------------------------------------------- 
C   COMPUTE W2 = EXP(W1) AND W3 = W2 * (ITIM**RHO - PREVIOUS TIME**RHO)
C----------------------------------------------------------------------- 
         W2 = 0.D0  
         IF (W1.GT.-20.D0) W2 = DEXP(W1) 
         DLTIM = VECLOG(ITIM) 
         TRHO = VECEXP(ITIM,ISTR)
         W3 = W2 * (TRHO - TPRERHO)
c 
         OMEG = OMEG + W3          
C           
C-----------------------------------------------------------------------
C  IF THERE ARE SPECIFIC TIMES IN VECTOR ISTIME FOR WHICH S(T) IS TO    
C        BE COMPUTED (I.E., IF NSTIME>0)            
C    ==> CHECK WHETHER THE FIRST ONE (ISTIME(IS)) IS LESS THAN ITIM              
C-----------------------------------------------------------------------
  310    IF ((NSTIME.EQ.0).OR.(IS.GT.NSTIME)) GOTO 320                 
         IF (ISTIME(IS).LT.ITIM) THEN  
           DLXST = VECLOG(ISTIME(IS)) 
           XRHO = VECEXP(ISTIME(IS),ISTR)         
           CUM_RISK= OMEG - W2 * (TRHO - XRHO)
           SX = DEXP( - CUM_RISK) 
           IREST(IT) = ISTIME(IS)
           REST(IT) = SX    
           IT = IT + 1
           IS = IS+1    
           GOTO 310
         ENDIF                   
C                 
  320    IF ((NQUANT.EQ.0).OR.(IQ.GT.NQUANT)) GOTO 380  
         SX = DEXP( - OMEG)              
         IF (SX.LE.SQUANT(IQ)) THEN  
            IF (SX.EQ.SQUANT(IQ).OR.W2.EQ.0.D0) THEN
               IRESQ(IQ) = ITIM
            ELSE
               U = TRHO - (OMEG + DLOG(SQUANT(IQ))) / W2
               U = DEXP(DLOG(U)/RHO)
               IRESQ(IQ) = NINT(U)  
            ENDIF    
            IQ = IQ + 1        
            GOTO 320           
          ENDIF               
C    
  380    IF (ICODE.GE.0) THEN
            TPRERHO = 0.D0 
          ELSE  
            TPRERHO = TRHO
          ENDIF                 
  390   CONTINUE                
C
        DO 370 J = 1,IT-1
           nn=nn+1
           surv(nn,1)=IANI
           surv(nn,2)=irest(j)
           surv(nn,3)=rest(j)
  370   CONTINUE
        DO 375 J = 1,NQUANT
           nn=nn+1
           surv(nn,1)=IANIPREV
           surv(nn,2)=iresq(j)
           surv(nn,3)=squant(j)
  375   CONTINUE
C           
      RETURN
C***********************************************************************
      END   
C     ----------------------------------------------------------------------
C     This file contains the LBFGS algorithm and supporting routines
C
C     ****************
C     LBFGS SUBROUTINE
C     ****************
C
      SUBROUTINE LBFGS(N,M,X,F,G,DIAGCO,DIAG,EPS,XTOL,W,IFLAG)
      IMPLICIT NONE
      INTEGER N,M,IFLAG
      real(8) X(N),G(N),DIAG(N),W(N*(2*M+1)+2*M)
      real(8) F,EPS,EPS2,XTOL
      LOGICAL DIAGCO
C
C        LIMITED MEMORY BFGS METHOD FOR LARGE SCALE OPTIMIZATION
C                          JORGE NOCEDAL
C                        *** July 1990 ***
C
C 
C     This subroutine solves the unconstrained minimization problem
C 
C                      min F(x),    x= (x1,x2,...,xN),
C
C      using the limited memory BFGS method. The routine is especially
C      effective on problems involving a large number of variables. In
C      a typical iteration of this method an approximation Hk to the
C      inverse of the Hessian is obtained by applying M BFGS updates to
C      a diagonal matrix Hk0, using information from the previous M 
C      steps.
C      The user specifies the number M, which determines the amount of
C      storage required by the routine. The user may also provide the
C      diagonal matrices Hk0 if not satisfied with the default choice.
C      The algorithm is described in "On the limited memory BFGS method
C      for large scale optimization", by D. Liu and J. Nocedal,
C      Mathematical Programming B 45 (1989) 503-528.
C 
C      The user is required to calculate the function value F and its
C      gradient G. In order to allow the user complete control over
C      these computations, reverse  communication is used. The routine
C      must be called repeatedly under the control of the parameter
C      IFLAG. 
C
C      The steplength is determined at each iteration by means of the
C      line search routine MCVSRCH, which is a slight modification of
C      the routine CSRCH written by More' and Thuente.
C 
C      The calling statement is 
C 
C          CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG)
C 
C      where
C 
C     N       is an INTEGER variable that must be set by the user to the
C             number of variables. It is not altered by the routine.
C             Restriction: N>0.
C 
C     M       is an INTEGER variable that must be set by the user to
C             the number of corrections used in the BFGS update. It
C             is not altered by the routine. Values of M less than 3 are
C             not recommended; large values of M will result in excessive
C             computing time. 3<= M <=7 is recommended. Restriction: M>0.
C 
C     X       is a DOUBLE PRECISION array of length N. On initial entry
C             it must be set by the user to the values of the initial
C             estimate of the solution vector. On exit with IFLAG=0, it
C             contains the values of the variables at the best point
C             found (usually a solution).
C 
C     F       is a DOUBLE PRECISION variable. Before initial entry and on
C             a re-entry with IFLAG=1, it must be set by the user to
C             contain the value of the function F at the point X.
C 
C     G       is a DOUBLE PRECISION array of length N. Before initial
C             entry and on a re-entry with IFLAG=1, it must be set by
C             the user to contain the components of the gradient G at
C             the point X.
C 
C     DIAGCO  is a LOGICAL variable that must be set to .TRUE. if the
C             user  wishes to provide the diagonal matrix Hk0 at each
C             iteration. Otherwise it should be set to .FALSE., in which
C             case  LBFGS will use a default value described below. If
C             DIAGCO is set to .TRUE. the routine will return at each
C             iteration of the algorithm with IFLAG=2, and the diagonal
C              matrix Hk0  must be provided in the array DIAG.
C 
C 
C     DIAG    is a DOUBLE PRECISION array of length N. If DIAGCO=.TRUE.,
C             then on initial entry or on re-entry with IFLAG=2, DIAG
C             it must be set by the user to contain the values of the 
C             diagonal matrix Hk0.  Restriction: all elements of DIAG
C             must be positive.
C 
C     IPRINT  is an INTEGER array of length two which must be set by the
C             user.
C 
C             IPRINT(1) specifies the frequency of the output:
C                IPRINT(1) < 0 : no output is generated,
C                IPRINT(1) = 0 : output only at first and last iteration,
C                IPRINT(1) > 0 : output every IPRINT(1) iterations.
C 
C             IPRINT(2) specifies the type of output generated:
C                IPRINT(2) = 0 : iteration count, number of function 
C                                evaluations, function value, norm of the
C                                gradient, and steplength,
C                IPRINT(2) = 1 : same as IPRINT(2)=0, plus vector of
C                                variables and  gradient vector at the
C                                initial point,
C                IPRINT(2) = 2 : same as IPRINT(2)=1, plus vector of
C                                variables,
C                IPRINT(2) = 3 : same as IPRINT(2)=2, plus gradient vector.
C 
C 
C     EPS     is a positive DOUBLE PRECISION variable that must be set by
C             the user, and determines the accuracy with which the solution
C             is to be found. The subroutine terminates when
C
C                         ||G||/F < EPS max(1,||X||),
C
C             where ||.|| denotes the Euclidean norm.
C 
C     XTOL    is a  positive DOUBLE PRECISION variable that must be set by
C             the user to an estimate of the machine precision (e.g.
C             10**(-16) on a SUN station 3/60). The line search routine will
C             terminate if the relative width of the interval of uncertainty
C             is less than XTOL.
C 
C     W       is a DOUBLE PRECISION array of length N(2M+1)+2M used as
C             workspace for LBFGS. This array must not be altered by the
C             user.
C 
C     IFLAG   is an INTEGER variable that must be set to 0 on initial entry
C             to the subroutine. A return with IFLAG<0 indicates an error,
C             and IFLAG=0 indicates that the routine has terminated without
C             detecting errors. On a return with IFLAG=1, the user must
C             evaluate the function F and gradient G. On a return with
C             IFLAG=2, the user must provide the diagonal matrix Hk0.
C 
C             The following negative values of IFLAG, detecting an error,
C             are possible:
C 
C              IFLAG=-1  The line search routine MCSRCH failed. The
C                        parameter INFO provides more detailed information
C                        (see also the documentation of MCSRCH):
C
C                       INFO = 0  IMPROPER INPUT PARAMETERS.
C
C                       INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF
C                                 UNCERTAINTY IS AT MOST XTOL.
C
C                       INFO = 3  MORE THAN 20 FUNCTION EVALUATIONS WERE
C                                 REQUIRED AT THE PRESENT ITERATION.
C
C                       INFO = 4  THE STEP IS TOO SMALL.
C
C                       INFO = 5  THE STEP IS TOO LARGE.
C
C                       INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS. 
C                                 THERE MAY NOT BE A STEP WHICH SATISFIES
C                                 THE SUFFICIENT DECREASE AND CURVATURE
C                                 CONDITIONS. TOLERANCES MAY BE TOO SMALL.
C
C 
C              IFLAG=-2  The i-th diagonal element of the diagonal inverse
C                        Hessian approximation, given in DIAG, is not
C                        positive.
C           
C              IFLAG=-3  Improper input parameters for LBFGS (N or M are
C                        not positive).
C 
C
C
C    ON THE DRIVER:
C
C    The program that calls LBFGS must contain the declaration:
C
C                       EXTERNAL LB2
C
C    LB2 is a BLOCK DATA that defines the default values of several
C    parameters described in the COMMON section. 
C
C 
C 
C    COMMON:
C 
C     The subroutine contains one common area, which the user may wish to
C    reference:
C
         real(8) GTOL,STPMIN,STPMAX 
         COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
C 
C    MP  is an INTEGER variable with default value 6. It is used as the
C        unit number for the printing of the monitoring information
C        controlled by IPRINT.
C 
C    LP  is an INTEGER variable with default value 6. It is used as the
C        unit number for the printing of error messages. This printing
C        may be suppressed by setting LP to a non-positive value.
C 
C    GTOL is a DOUBLE PRECISION variable with default value 0.9, which
C        controls the accuracy of the line search routine MCSRCH. If the
C        function and gradient evaluations are inexpensive with respect
C        to the cost of the iteration (which is sometimes the case when
C        solving very large problems) it may be advantageous to set GTOL
C        to a small value. A typical small value is 0.1.  Restriction:
C        GTOL should be greater than 1.D-04.
C 
C    STPMIN and STPMAX are non-negative DOUBLE PRECISION variables which
C        specify lower and uper bounds for the step in the line search.
C        Their default values are 1.D-20 and 1.D+20, respectively. These
C        values need not be modified unless the exponents are too large
C        for the machine being used, or unless the problem is extremely
C        badly scaled (in which case the exponents should be increased).
C 
C
C  MACHINE DEPENDENCIES
C
C        The only variables that are machine-dependent are XTOL,
C        STPMIN and STPMAX.
C 
C
C  GENERAL INFORMATION
C 
C    Other routines called directly:  DAXPY, DDOT, LB1, MCSRCH
C 
C    Input/Output  :  No input; diagnostic messages on unit MP and
C                     error messages on unit LP.
C 
C 
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      real(8) ONE,ZERO,GNORM,DDOT,STP1,FTOL,
     .                 STP,YS,YY,SQ,YR,CC,BETA,XNORM
      INTEGER MP,LP,ITER,NFUN,POINT,ISPT,IYPT,MAXFEV,INFO,
     .        BOUND,NPT,CP,I,NFEV,INMC,IYCN,ISCN
      LOGICAL FINISH
C
      SAVE
      DATA ONE,ZERO/1.0D+0,0.0D+0/
C
C     INITIALIZE
C     ----------
C
      EPS2=0.01D0*EPS
      IF(IFLAG.EQ.0) GO TO 10
C      GO TO (172,100) IFLAG
      IF (IFLAG .EQ. 1) THEN
        GO TO 172
      ELSE IF (IFLAG .EQ. 2) THEN
        GO TO 100
      END IF
  10  ITER= 0
      IF(N.LE.0.OR.M.LE.0) GO TO 196
      IF(GTOL.LE.1.D-04) THEN
        GTOL=9.D-01
      ENDIF
      NFUN= 1
      POINT= 0
      FINISH= .FALSE.
      IF(DIAGCO) THEN
         DO 30 I=1,N
       IF (DIAG(I).LE.ZERO) GO TO 195
 30      CONTINUE
      ELSE
         DO 40 I=1,N
         DIAG(I)= 1.0D0
 40      CONTINUE
      ENDIF
C
C     THE WORK VECTOR W IS DIVIDED AS FOLLOWS:
C     ---------------------------------------
C     THE FIRST N LOCATIONS ARE USED TO STORE THE GRADIENT AND
C         OTHER TEMPORARY INFORMATION.
C     LOCATIONS (N+1)...(N+M) STORE THE SCALARS RHO.
C     LOCATIONS (N+M+1)...(N+2M) STORE THE NUMBERS ALPHA USED
C         IN THE FORMULA THAT COMPUTES H*G.
C     LOCATIONS (N+2M+1)...(N+2M+NM) STORE THE LAST M SEARCH
C         STEPS.
C     LOCATIONS (N+2M+NM+1)...(N+2M+2NM) STORE THE LAST M
C         GRADIENT DIFFERENCES.
C
C     THE SEARCH STEPS AND GRADIENT DIFFERENCES ARE STORED IN A
C     CIRCULAR ORDER CONTROLLED BY THE PARAMETER POINT.
C
      ISPT= N+2*M
      IYPT= ISPT+N*M     
      DO 50 I=1,N
      W(ISPT+I)= -G(I)*DIAG(I)
 50   CONTINUE
      GNORM= DSQRT(DDOT(N,G,1,G,1))
      STP1= ONE/GNORM
C
C     PARAMETERS FOR LINE SEARCH ROUTINE
C     
      FTOL= 1.0D-4
      MAXFEV= 20
      XNORM= DSQRT(DDOT(N,X,1,X,1))
      XNORM= DMAX1(1.0D0,XNORM)
      CC = GNORM / (XNORM*MAX(1.D0,F))
C
C Change on May 12, 1997
C at least M function evaluations before assuming convergence..
C
      IF (CC.LT.EPS.AND.NFUN.GE.M) FINISH=.TRUE.
      IF (CC.LT.EPS2) FINISH=.TRUE.
C
C    --------------------
C     MAIN ITERATION LOOP
C    --------------------
C
 80   ITER= ITER+1
      INFO=0
      BOUND=ITER-1
      IF(ITER.EQ.1) GO TO 165
      IF (ITER .GT. M)BOUND=M
C
         YS= DDOT(N,W(IYPT+NPT+1),1,W(ISPT+NPT+1),1)
      IF(.NOT.DIAGCO) THEN
         YY= DDOT(N,W(IYPT+NPT+1),1,W(IYPT+NPT+1),1)
         DO 90 I=1,N
         DIAG(I)= YS/YY
   90    CONTINUE
      ELSE
         IFLAG=2
         RETURN
      ENDIF
 100  CONTINUE
      IF(DIAGCO) THEN
        DO 110 I=1,N
        IF (DIAG(I).LE.ZERO) GO TO 195
 110    CONTINUE
      ENDIF
C
C     COMPUTE -H*G USING THE FORMULA GIVEN IN: Nocedal, J. 1980,
C     "Updating quasi-Newton matrices with limited storage",
C     Mathematics of Computation, Vol.24, No.151, pp. 773-782.
C     ---------------------------------------------------------
C
      CP= POINT
      IF (POINT.EQ.0) CP=M
      W(N+CP)= ONE/YS
      DO 112 I=1,N
      W(I)= -G(I)
 112  CONTINUE
      CP= POINT
      DO 125 I= 1,BOUND
         CP=CP-1
         IF (CP.EQ. -1)CP=M-1
         SQ= DDOT(N,W(ISPT+CP*N+1),1,W,1)
         INMC=N+M+CP+1
         IYCN=IYPT+CP*N
         W(INMC)= W(N+CP+1)*SQ
         CALL DAXPY(N,-W(INMC),W(IYCN+1),1,W,1)
 125  CONTINUE
C
      DO 130 I=1,N
      W(I)=DIAG(I)*W(I)
 130  CONTINUE
C
      DO 145 I=1,BOUND
         YR= DDOT(N,W(IYPT+CP*N+1),1,W,1)
         BETA= W(N+CP+1)*YR
         INMC=N+M+CP+1
         BETA= W(INMC)-BETA
         ISCN=ISPT+CP*N
         CALL DAXPY(N,BETA,W(ISCN+1),1,W,1)
         CP=CP+1
         IF (CP.EQ.M)CP=0
 145  CONTINUE
C
C     STORE THE NEW SEARCH DIRECTION
C     ------------------------------
C
       DO 160 I=1,N
       W(ISPT+POINT*N+I)= W(I)
 160   CONTINUE
C
C     OBTAIN THE ONE-DIMENSIONAL MINIMIZER OF THE FUNCTION 
C     BY USING THE LINE SEARCH ROUTINE MCSRCH
C     ----------------------------------------------------
 165  NFEV=0
      STP=ONE
      IF (ITER.EQ.1) STP=STP1
      DO 170 I=1,N
      W(I)=G(I)
 170  END DO ! OR CONTINUE
 172  CONTINUE
      CALL MCSRCH(N,X,F,G,W(ISPT+POINT*N+1),STP,FTOL,
     *            XTOL,MAXFEV,INFO,NFEV,DIAG)
      IF (INFO .EQ. -1) THEN
        IFLAG=1
        RETURN
      ENDIF
      IF (INFO .NE. 1) GO TO 190
      NFUN= NFUN + NFEV
C
C     COMPUTE THE NEW STEP AND GRADIENT CHANGE 
C     -----------------------------------------
C
      NPT=POINT*N
      DO 175 I=1,N
      W(ISPT+NPT+I)= STP*W(ISPT+NPT+I)
      W(IYPT+NPT+I)= G(I)-W(I)
 175  CONTINUE
      POINT=POINT+1
      IF (POINT.EQ.M)POINT=0
C
C     TERMINATION TEST
C     ----------------
C
      GNORM= DSQRT(DDOT(N,G,1,G,1))
      XNORM= DSQRT(DDOT(N,X,1,X,1))
      XNORM= DMAX1(1.0D0,XNORM)
      CC = GNORM / (XNORM*MAX(1.D0,F))
C
C Change on May 12, 1997
C at least M function evaluations before assuming convergence..
C
      IF (CC.LT.EPS.AND.NFUN.GE.M) FINISH=.TRUE.
      IF (CC.LT.EPS2) FINISH=.TRUE.
C
      IF (FINISH) THEN
         IFLAG=0
         RETURN
      ENDIF
      GO TO 80
C
C     ------------------------------------------------------------
C     END OF MAIN ITERATION LOOP. ERROR EXITS.
C     ------------------------------------------------------------
C
 190  IFLAG=-1
      RETURN
 195  IFLAG=-2
      RETURN
 196  IFLAG= -3
      RETURN
      END
C
C     LAST LINE OF SUBROUTINE LBFGS
C
C   ----------------------------------------------------------
C     DATA 
C   ----------------------------------------------------------
C
      BLOCK DATA LB2
      INTEGER LP,MP
      real(8) GTOL,STPMIN,STPMAX
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      DATA MP,LP,GTOL,STPMIN,STPMAX/6,6,9.0D-01,1.0D-20,1.0D+20/
      END
C
C
C   ----------------------------------------------------------
C
      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c

      implicit none

      integer i,incx,incy,ix,iy,m,mp1,n
      real(8) dx(n),dy(n),da,zero
      DATA ZERO/0.0D+0/
c
      if(n.le.0)return
      if (da .eq. zero) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
C
C
C   ----------------------------------------------------------
C
      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c

      implicit none

      integer i,incx,incy,ix,iy,m,mp1,n
      double precision dx(n),dy(n),dtemp
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
C    ------------------------------------------------------------------
C
C     **************************
C     LINE SEARCH ROUTINE MCSRCH
C     **************************
C
      SUBROUTINE MCSRCH(N,X,F,G,S,STP,FTOL,XTOL,MAXFEV,INFO,NFEV,WA)
      IMPLICIT NONE
      INTEGER N,LP,MP,MAXFEV,INFO,NFEV
      real(8) F,STP,FTOL,GTOL,XTOL,STPMIN,STPMAX
      real(8) X(N),G(N),S(N),WA(N)
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
      SAVE
C
C                     SUBROUTINE MCSRCH
C                
C     A slight modification of the subroutine CSRCH of More' and Thuente.
C     The changes are to allow reverse communication, and do not affect
C     the performance of the routine. 
C
C     THE PURPOSE OF MCSRCH IS TO FIND A STEP WHICH SATISFIES
C     A SUFFICIENT DECREASE CONDITION AND A CURVATURE CONDITION.
C
C     AT EACH STAGE THE SUBROUTINE UPDATES AN INTERVAL OF
C     UNCERTAINTY WITH ENDPOINTS STX AND STY. THE INTERVAL OF
C     UNCERTAINTY IS INITIALLY CHOSEN SO THAT IT CONTAINS A
C     MINIMIZER OF THE MODIFIED FUNCTION
C
C          F(X+STP*S) - F(X) - FTOL*STP*(GRADF(X)'S).
C
C     IF A STEP IS OBTAINED FOR WHICH THE MODIFIED FUNCTION
C     HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE DERIVATIVE,
C     THEN THE INTERVAL OF UNCERTAINTY IS CHOSEN SO THAT IT
C     CONTAINS A MINIMIZER OF F(X+STP*S).
C
C     THE ALGORITHM IS DESIGNED TO FIND A STEP WHICH SATISFIES
C     THE SUFFICIENT DECREASE CONDITION
C
C           F(X+STP*S) .LE. F(X) + FTOL*STP*(GRADF(X)'S),
C
C     AND THE CURVATURE CONDITION
C
C           ABS(GRADF(X+STP*S)'S)) .LE. GTOL*ABS(GRADF(X)'S).
C
C     IF FTOL IS LESS THAN GTOL AND IF, FOR EXAMPLE, THE FUNCTION
C     IS BOUNDED BELOW, THEN THERE IS ALWAYS A STEP WHICH SATISFIES
C     BOTH CONDITIONS. IF NO STEP CAN BE FOUND WHICH SATISFIES BOTH
C     CONDITIONS, THEN THE ALGORITHM USUALLY STOPS WHEN ROUNDING
C     ERRORS PREVENT FURTHER PROGRESS. IN THIS CASE STP ONLY
C     SATISFIES THE SUFFICIENT DECREASE CONDITION.
C
C     THE SUBROUTINE STATEMENT IS
C
C        SUBROUTINE MCSRCH(N,X,F,G,S,STP,FTOL,XTOL, MAXFEV,INFO,NFEV,WA)
C     WHERE
C
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
C         OF VARIABLES.
C
C       X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
C         BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
C         X + STP*S.
C
C       F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
C         AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + STP*S.
C
C       G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
C         GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
C         OF F AT X + STP*S.
C
C       S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
C         SEARCH DIRECTION.
C
C       STP IS A NONNEGATIVE VARIABLE. ON INPUT STP CONTAINS AN
C         INITIAL ESTIMATE OF A SATISFACTORY STEP. ON OUTPUT
C         STP CONTAINS THE FINAL ESTIMATE.
C
C       FTOL AND GTOL ARE NONNEGATIVE INPUT VARIABLES. (In this reverse
C         communication implementation GTOL is defined in a COMMON
C         statement.) TERMINATION OCCURS WHEN THE SUFFICIENT DECREASE
C         CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
C         SATISFIED.
C
C       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS
C         WHEN THE RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
C         IS AT MOST XTOL.
C
C       STPMIN AND STPMAX ARE NONNEGATIVE INPUT VARIABLES WHICH
C         SPECIFY LOWER AND UPPER BOUNDS FOR THE STEP. (In this reverse
C         communication implementatin they are defined in a COMMON
C         statement).
C
C       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION
C         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST
C         MAXFEV BY THE END OF AN ITERATION.
C
C       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
C
C         INFO = 0  IMPROPER INPUT PARAMETERS.
C
C         INFO =-1  A RETURN IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
C
C         INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
C                   DIRECTIONAL DERIVATIVE CONDITION HOLD.
C
C         INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
C                   IS AT MOST XTOL.
C
C         INFO = 3  NUMBER OF CALLS TO FCN HAS REACHED MAXFEV.
C
C         INFO = 4  THE STEP IS AT THE LOWER BOUND STPMIN.
C
C         INFO = 5  THE STEP IS AT THE UPPER BOUND STPMAX.
C
C         INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
C                   THERE MAY NOT BE A STEP WHICH SATISFIES THE
C                   SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
C                   TOLERANCES MAY BE TOO SMALL.
C
C       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF
C         CALLS TO FCN.
C
C       WA IS A WORK ARRAY OF LENGTH N.
C
C     SUBPROGRAMS CALLED
C
C       MCSTEP
C
C       FORTRAN-SUPPLIED...ABS,MAX,MIN
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
C     JORGE J. MORE', DAVID J. THUENTE
C
C     **********
      INTEGER INFOC,J
      LOGICAL BRACKT,STAGE1
      real(8) DG,DGM,DGINIT,DGTEST,DGX,DGXM,DGY,DGYM,
     *       FINIT,FTEST1,FM,FX,FXM,FY,FYM,P5,P66,STX,STY,
     *       STMIN,STMAX,WIDTH,WIDTH1,XTRAPF,ZERO
      DATA P5,P66,XTRAPF,ZERO /0.5D0,0.66D0,4.0D0,0.0D0/
      IF(INFO.EQ.-1) GO TO 45
      INFOC = 1
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (N .LE. 0 .OR. STP .LE. ZERO .OR. FTOL .LT. ZERO .OR.
     *    GTOL .LT. ZERO .OR. XTOL .LT. ZERO .OR. STPMIN .LT. ZERO
     *    .OR. STPMAX .LT. STPMIN .OR. MAXFEV .LE. 0) RETURN
C
C     COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
C     AND CHECK THAT S IS A DESCENT DIRECTION.
C
      DGINIT = ZERO
      DO 10 J = 1, N
         DGINIT = DGINIT + G(J)*S(J)
   10    CONTINUE
      IF (DGINIT .GE. ZERO) then
         RETURN
         ENDIF
C
C     INITIALIZE LOCAL VARIABLES.
C
      BRACKT = .FALSE.
      STAGE1 = .TRUE.
      NFEV = 0
      FINIT = F
      DGTEST = FTOL*DGINIT
      WIDTH = STPMAX - STPMIN
      WIDTH1 = WIDTH/P5
      DO 20 J = 1, N
         WA(J) = X(J)
   20    CONTINUE
C
C     THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
C     FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
C     THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
C     FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
C     THE INTERVAL OF UNCERTAINTY.
C     THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
C     FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
C
      STX = ZERO
      FX = FINIT
      DGX = DGINIT
      STY = ZERO
      FY = FINIT
      DGY = DGINIT
C
C     START OF ITERATION.
C
   30 CONTINUE
C
C        SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
C        TO THE PRESENT INTERVAL OF UNCERTAINTY.
C
         IF (BRACKT) THEN
            STMIN = MIN(STX,STY)
            STMAX = MAX(STX,STY)
         ELSE
            STMIN = STX
            STMAX = STP + XTRAPF*(STP - STX)
            END IF
C
C        FORCE THE STEP TO BE WITHIN THE BOUNDS STPMAX AND STPMIN.
C
         STP = MAX(STP,STPMIN)
         STP = MIN(STP,STPMAX)
C
C        IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
C        STP BE THE LOWEST POINT OBTAINED SO FAR.
C
         IF ((BRACKT .AND. (STP .LE. STMIN .OR. STP .GE. STMAX))
     *      .OR. NFEV .GE. MAXFEV-1 .OR. INFOC .EQ. 0
     *      .OR. (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX)) STP = STX
C
C        EVALUATE THE FUNCTION AND GRADIENT AT STP
C        AND COMPUTE THE DIRECTIONAL DERIVATIVE.
C        We return to main program to obtain F and G.
C
         DO 40 J = 1, N
            X(J) = WA(J) + STP*S(J)
   40       CONTINUE
         INFO=-1
         RETURN
C
   45    INFO=0
         NFEV = NFEV + 1
         DG = ZERO
         DO 50 J = 1, N
            DG = DG + G(J)*S(J)
   50       CONTINUE
         FTEST1 = FINIT + STP*DGTEST
C
C        TEST FOR CONVERGENCE.
C
         IF ((BRACKT .AND. (STP .LE. STMIN .OR. STP .GE. STMAX))
     *      .OR. INFOC .EQ. 0) INFO = 6
         IF (STP .EQ. STPMAX .AND.
     *       F .LE. FTEST1 .AND. DG .LE. DGTEST) INFO = 5
         IF (STP .EQ. STPMIN .AND.
     *       (F .GT. FTEST1 .OR. DG .GE. DGTEST)) INFO = 4
         IF (NFEV .GE. MAXFEV) INFO = 3
         IF (BRACKT .AND. STMAX-STMIN .LE. XTOL*STMAX) INFO = 2
         IF (F .LE. FTEST1 .AND. ABS(DG) .LE. GTOL*(-DGINIT)) INFO = 1
C
C        CHECK FOR TERMINATION.
C
         IF (INFO .NE. 0) RETURN
C
C        IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
C        FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
C
         IF (STAGE1 .AND. F .LE. FTEST1 .AND.
     *       DG .GE. MIN(FTOL,GTOL)*DGINIT) STAGE1 = .FALSE.
C
C        A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
C        WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
C        FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
C        DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
C        OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
C
         IF (STAGE1 .AND. F .LE. FX .AND. F .GT. FTEST1) THEN
C
C           DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
C
            FM = F - STP*DGTEST
            FXM = FX - STX*DGTEST
            FYM = FY - STY*DGTEST
            DGM = DG - DGTEST
            DGXM = DGX - DGTEST
            DGYM = DGY - DGTEST
C
C           CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
C           AND TO COMPUTE THE NEW STEP.
C
            CALL MCSTEP(STX,FXM,DGXM,STY,FYM,DGYM,STP,FM,DGM,
     *                 BRACKT,STMIN,STMAX,INFOC)
C
C           RESET THE FUNCTION AND GRADIENT VALUES FOR F.
C
            FX = FXM + STX*DGTEST
            FY = FYM + STY*DGTEST
            DGX = DGXM + DGTEST
            DGY = DGYM + DGTEST
         ELSE
C
C           CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
C           AND TO COMPUTE THE NEW STEP.
C
            CALL MCSTEP(STX,FX,DGX,STY,FY,DGY,STP,F,DG,
     *                 BRACKT,STMIN,STMAX,INFOC)
            END IF
C
C        FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
C        INTERVAL OF UNCERTAINTY.
C
         IF (BRACKT) THEN
            IF (ABS(STY-STX) .GE. P66*WIDTH1)
     *         STP = STX + P5*(STY - STX)
            WIDTH1 = WIDTH
            WIDTH = ABS(STY-STX)
            END IF
C
C        END OF ITERATION.
C
         GO TO 30
C
C     LAST LINE OF SUBROUTINE MCSRCH.
C
      END
      SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT,
     *                 STPMIN,STPMAX,INFO)
      IMPLICIT NONE
      INTEGER INFO
      real(8) STX,FX,DX,STY,FY,DY,STP,FP,DP,STPMIN,STPMAX
      LOGICAL BRACKT,BOUND
C
C     SUBROUTINE MCSTEP
C
C     THE PURPOSE OF MCSTEP IS TO COMPUTE A SAFEGUARDED STEP FOR
C     A LINESEARCH AND TO UPDATE AN INTERVAL OF UNCERTAINTY FOR
C     A MINIMIZER OF THE FUNCTION.
C
C     THE PARAMETER STX CONTAINS THE STEP WITH THE LEAST FUNCTION
C     VALUE. THE PARAMETER STP CONTAINS THE CURRENT STEP. IT IS
C     ASSUMED THAT THE DERIVATIVE AT STX IS NEGATIVE IN THE
C     DIRECTION OF THE STEP. IF BRACKT IS SET TRUE THEN A
C     MINIMIZER HAS BEEN BRACKETED IN AN INTERVAL OF UNCERTAINTY
C     WITH ENDPOINTS STX AND STY.
C
C     THE SUBROUTINE STATEMENT IS
C
C       SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT,
C                        STPMIN,STPMAX,INFO)
C
C     WHERE
C
C       STX, FX, AND DX ARE VARIABLES WHICH SPECIFY THE STEP,
C         THE FUNCTION, AND THE DERIVATIVE AT THE BEST STEP OBTAINED
C         SO FAR. THE DERIVATIVE MUST BE NEGATIVE IN THE DIRECTION
C         OF THE STEP, THAT IS, DX AND STP-STX MUST HAVE OPPOSITE
C         SIGNS. ON OUTPUT THESE PARAMETERS ARE UPDATED APPROPRIATELY.
C
C       STY, FY, AND DY ARE VARIABLES WHICH SPECIFY THE STEP,
C         THE FUNCTION, AND THE DERIVATIVE AT THE OTHER ENDPOINT OF
C         THE INTERVAL OF UNCERTAINTY. ON OUTPUT THESE PARAMETERS ARE
C         UPDATED APPROPRIATELY.
C
C       STP, FP, AND DP ARE VARIABLES WHICH SPECIFY THE STEP,
C         THE FUNCTION, AND THE DERIVATIVE AT THE CURRENT STEP.
C         IF BRACKT IS SET TRUE THEN ON INPUT STP MUST BE
C         BETWEEN STX AND STY. ON OUTPUT STP IS SET TO THE NEW STEP.
C
C       BRACKT IS A LOGICAL VARIABLE WHICH SPECIFIES IF A MINIMIZER
C         HAS BEEN BRACKETED. IF THE MINIMIZER HAS NOT BEEN BRACKETED
C         THEN ON INPUT BRACKT MUST BE SET FALSE. IF THE MINIMIZER
C         IS BRACKETED THEN ON OUTPUT BRACKT IS SET TRUE.
C
C       STPMIN AND STPMAX ARE INPUT VARIABLES WHICH SPECIFY LOWER
C         AND UPPER BOUNDS FOR THE STEP.
C
C       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
C         IF INFO = 1,2,3,4,5, THEN THE STEP HAS BEEN COMPUTED
C         ACCORDING TO ONE OF THE FIVE CASES BELOW. OTHERWISE
C         INFO = 0, AND THIS INDICATES IMPROPER INPUT PARAMETERS.
C
C     SUBPROGRAMS CALLED
C
C       FORTRAN-SUPPLIED ... ABS,MAX,MIN,SQRT
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
C     JORGE J. MORE', DAVID J. THUENTE
C
      real(8) GAMMA,P,Q,R,S,SGND,STPC,STPF,STPQ,THETA,ZERO
      DATA ZERO/0.0D+0/
      INFO = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF ((BRACKT .AND. (STP .LE. MIN(STX,STY) .OR.
     *    STP .GE. MAX(STX,STY))) .OR.
     *    DX*(STP-STX) .GE. ZERO .OR. STPMAX .LT. STPMIN) RETURN
C
C     DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
C
      SGND = DP*(DX/ABS(DX))
C
C     FIRST CASE. A HIGHER FUNCTION VALUE.
C     THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
C     TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
C     ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
C
      IF (FP .GT. FX) THEN
         INFO = 1
         BOUND = .TRUE.
         THETA = 3.D0*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
         GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
         IF (STP .LT. STX) GAMMA = -GAMMA
         P = (GAMMA - DX) + THETA
         Q = ((GAMMA - DX) + GAMMA) + DP
         R = P/Q
         STPC = STX + R*(STP - STX)
         STPQ = STX + ((DX/((FX-FP)/(STP-STX)+DX))/2.D0)*(STP - STX)
         IF (ABS(STPC-STX) .LT. ABS(STPQ-STX)) THEN
            STPF = STPC
         ELSE
           STPF = STPC + (STPQ - STPC)/2.D0
           END IF
         BRACKT = .TRUE.
C
C     SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
C     OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
C     STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
C     THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
C
      ELSE IF (SGND .LT. ZERO) THEN
         INFO = 2
         BOUND = .FALSE.
         THETA = 3.D0*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
         GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
         IF (STP .GT. STX) GAMMA = -GAMMA
         P = (GAMMA - DP) + THETA
         Q = ((GAMMA - DP) + GAMMA) + DX
         R = P/Q
         STPC = STP + R*(STX - STP)
         STPQ = STP + (DP/(DP-DX))*(STX - STP)
         IF (ABS(STPC-STP) .GT. ABS(STPQ-STP)) THEN
            STPF = STPC
         ELSE
            STPF = STPQ
            END IF
         BRACKT = .TRUE.
C
C     THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
C     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
C     THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
C     IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
C     IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
C     EITHER STPMIN OR STPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
C     COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
C     CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
C
      ELSE IF (ABS(DP) .LT. ABS(DX)) THEN
         INFO = 3
         BOUND = .TRUE.
         THETA = 3.D0*(FX - FP)/(STP - STX) + DX + DP
         S = MAX(ABS(THETA),ABS(DX),ABS(DP))
C
C        THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
C        TO INFINITY IN THE DIRECTION OF THE STEP.
C
         GAMMA = S*SQRT(MAX(ZERO,(THETA/S)**2 - (DX/S)*(DP/S)))
         IF (STP .GT. STX) GAMMA = -GAMMA
         P = (GAMMA - DP) + THETA
         Q = (GAMMA + (DX - DP)) + GAMMA
         R = P/Q
         IF (R .LT. ZERO .AND. GAMMA .NE. ZERO) THEN
            STPC = STP + R*(STX - STP)
         ELSE IF (STP .GT. STX) THEN
            STPC = STPMAX
         ELSE
            STPC = STPMIN
            END IF
         STPQ = STP + (DP/(DP-DX))*(STX - STP)
         IF (BRACKT) THEN
            IF (ABS(STP-STPC) .LT. ABS(STP-STPQ)) THEN
               STPF = STPC
            ELSE
               STPF = STPQ
               END IF
         ELSE
            IF (ABS(STP-STPC) .GT. ABS(STP-STPQ)) THEN
               STPF = STPC
            ELSE
               STPF = STPQ
               END IF
            END IF
C
C     FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
C     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
C     NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
C     IS EITHER STPMIN OR STPMAX, ELSE THE CUBIC STEP IS TAKEN.
C
      ELSE
         INFO = 4
         BOUND = .FALSE.
         IF (BRACKT) THEN
            THETA = 3.D0*(FP - FY)/(STY - STP) + DY + DP
            S = MAX(ABS(THETA),ABS(DY),ABS(DP))
            GAMMA = S*SQRT((THETA/S)**2 - (DY/S)*(DP/S))
            IF (STP .GT. STY) GAMMA = -GAMMA
            P = (GAMMA - DP) + THETA
            Q = ((GAMMA - DP) + GAMMA) + DY
            R = P/Q
            STPC = STP + R*(STY - STP)
            STPF = STPC
         ELSE IF (STP .GT. STX) THEN
            STPF = STPMAX
         ELSE
            STPF = STPMIN
            END IF
         END IF
C
C     UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
C     DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
C
      IF (FP .GT. FX) THEN
         STY = STP
         FY = FP
         DY = DP
      ELSE
         IF (SGND .LT. ZERO) THEN
            STY = STX
            FY = FX
            DY = DX
            END IF
         STX = STP
         FX = FP
         DX = DP
         END IF
C
C     COMPUTE THE NEW STEP AND SAFEGUARD IT.
C
      STPF = MIN(STPMAX,STPF)
      STPF = MAX(STPMIN,STPF)
      STP = STPF
      IF (BRACKT .AND. BOUND) THEN
         IF (STY .GT. STX) THEN
            STP = MIN(STX+0.66D0*(STY-STX),STP)
         ELSE
            STP = MAX(STX+0.66D0*(STY-STX),STP)
            END IF
         END IF
      RETURN
C
C     LAST LINE OF SUBROUTINE MCSTEP.
C
      END
      double precision FUNCTION DFMIN(AX,BX,FTOMIN,TOL,OBJ,IGAM,INOR,
     +     XGAM,XNOR,BOUND,icons,beta,info,nrr,nccov,ndcov,hess,
     +     itoev,iter_bf,iplus1,iplus2,ipedig,err)

      include 'parinclu.h'

      real(8) AX,BX,TOL,OBJ 
C***PURPOSE
C     An approximation to the point where  FTOMIN  attains a minimum  on
C     the interval (AX,BX) is determined as the value of the function
C     DFMIN.
C
C PARAMETERS
C
C INPUT
C
C  AX    (double precision)  left endpoint of initial interval
C  BX    (double precision) right endpoint of initial interval
C  FTOMIN     function subprogram which evaluates FTOMIN(X)  for any  X
C        in the interval  (AX,BX)
C  TOL   (double precision) desired length of the interval of uncertainty 
C        of the final result ( .ge. 0.0)
C
C
C OUTPUT
C
C DFMIN   abcissa approximating the minimizer of FTOMIN
C AX     lower bound for minimizer
C BX     upper bound for minimizer
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added onSeptember 16, 1997
C OBJ is the minimum value of FTOMIN
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C***DESCRIPTION
C
C     The method used is a combination of golden section search and
C     successive parabolic interpolation.  Convergence is never much 
C     slower than that for a Fibonacci search.  If F has a continuous 
C     second derivative which is positive at the minimum (which is not
C     at AX or BX), then convergence is superlinear, and usually of the 
C     order of about 1.324....
C
C     The function F is never evaluated at two points closer together
C     than EPS*ABS(DFMIN) + (TOL/3), where EPS is approximately the 
C     square root of the relative machine precision.  If F is a unimodal
C     function and the computed values of F are always unimodal when
C     separated by at least EPS*ABS(XSTAR) + (TOL/3), then DFMIN 
C     approximates the abcissa of the global minimum of F on the 
C     interval AX,BX with an error less than 3*EPS*ABS(DFMIN) + TOL.  
C     If F is not unimodal, then DFMIN may approximate a local, but 
C     perhaps non-global, minimum to the same accuracy.
C
C     This function subprogram is a slightly modified version of the
C     ALGOL 60 procedure LOCALMIN given in Richard Brent, Algorithms for
C     Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
C
C***REFERENCE(S)
C     Richard Brent, Algorithms for Minimization Without Derivatives,
C     Prentice-Hall, Inc. (1973).
C***ROUTINES CALLED   NONE
C***END PROLOGUE
      integer nrr,nccov,ndcov,itoev
      integer ICONS(0:NDIMAX),INFO(NRR,4)
      integer IGAM(0:NCCOV+NDCOV,3),INOR(0:NCCOV+NDCOV,3),err,iter_bf
      integer iplus1(nccov),iplus2(ndcov),IPEDIG(4,NDIMAX)
      real(8) HESS(NDIMAX,NDIMAX),BOUND(NCCOV+NDCOV,3)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) A,B,C,D,E,EPS,XM,P,Q,R,TOL1,TOL2,U,V,W
      real(8) FU,FV,FW,FX,X,BETA(NDIMAX)
      real(8) DABS,DSQRT,DSIGN
      EXTERNAL FTOMIN
      double precision FTOMIN
C
C  C is the squared inverse of the golden ratio
C
      C = 0.5D0*(3.D0 - DSQRT(5.0D0))
C
C  EPS is approximately the square root of the relative machine
C  precision.
C
      EPS = 1.0D00
   10 EPS = EPS/2.0D00
      TOL1 = 1.0D0 + EPS
      IF (TOL1 .GT. 1.0D00) GO TO 10
      EPS = DSQRT(EPS)
C
C  initialization
C
      A = AX
      B = BX
      V = A + C*(B - A)
      W = V
      X = V
      E = 0.0D0
      FX = FTOMIN(X,IGAM,INOR,XGAM,XNOR,BOUND,icons,beta,info,
     +     nrr,nccov,ndcov,hess,itoev,iter_bf,iplus1,iplus2,
     +     ipedig,err)
      if(err.gt.0)return
      FV = FX
      FW = FX
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added on September 16, 1997
C OBJ is the minimum value of FTOMIN
      OBJ = FX
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C  main loop starts here
C
   20 XM = 0.5D0*(A + B)
      TOL1 = EPS*DABS(X) + TOL/3.0D0
      TOL2 = 2.0D0*TOL1
C
C  check stopping criterion
C
      IF (DABS(X - XM) .LE. (TOL2 - 0.5D0*(B - A))) GO TO 90
C
C is golden-section necessary
C
      IF (DABS(E) .LE. TOL1) GO TO 40
C
C  fit parabola
C
      R = (X - W)*(FX - FV)
      Q = (X - V)*(FX - FW)
      P = (X - V)*Q - (X - W)*R
      Q = 2.0D00*(Q - R)
      IF (Q .GT. 0.0D0) P = -P
      Q =  DABS(Q)
      R = E
      E = D
C
C  is parabola acceptable
C
   30 IF (DABS(P) .GE. DABS(0.5D0*Q*R)) GO TO 40
      IF (P .LE. Q*(A - X)) GO TO 40
      IF (P .GE. Q*(B - X)) GO TO 40
C
C  a parabolic interpolation step
C
      D = P/Q
      U = X + D
C
C  F must not be evaluated too close to AX or BX
C
      IF ((U - A) .LT. TOL2) D = DSIGN(TOL1, XM - X)
      IF ((B - U) .LT. TOL2) D = DSIGN(TOL1, XM - X)
      GO TO 50
C
C  a golden-section step
C
   40 IF (X .GE. XM) E = A - X
      IF (X .LT. XM) E = B - X
      D = C*E
C
C  F must not be evaluated too close to X
C
   50 IF (DABS(D) .GE. TOL1) U = X + D
      IF (DABS(D) .LT. TOL1) U = X + DSIGN(TOL1, D)
      FU = FTOMIN(U,IGAM,INOR,XGAM,XNOR,BOUND,icons,beta,info,
     +     nrr,nccov,ndcov,hess,itoev,iter_bf,iplus1,iplus2,
     +     ipedig,err)
      if(err.gt.0)return
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added on September 16, 1997
C OBJ is the minimum value of FTOMIN
      IF (FU.LT.FX) OBJ = FU
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C  update  A, B, V, W, and X
C
      IF (FU .GT. FX) GO TO 60
      IF (U .GE. X) A = X
      IF (U .LT. X) B = X
      V = W
      FV = FW
      W = X
      FW = FX
      X = U
      FX = FU
      GO TO 20
   60 IF (U .LT. X) A = U
      IF (U .GE. X) B = U
      IF (FU .LE. FW) GO TO 70
      IF (W .EQ. X) GO TO 70
      IF (FU .LE. FV) GO TO 80
      IF (V .EQ. X) GO TO 80
      IF (V .EQ. W) GO TO 80
      GO TO 20
   70 V = W
      FV = FW
      W = U
      FW = FU
      GO TO 20
   80 V = U
      FV = FU
      GO TO 20
C
C  end of main loop
C
   90 DFMIN = X
      AX=A
      BX=B
      RETURN
      END
      SUBROUTINE JSORT(IX,N,IY,IW,ORD)
C
C     PURPOSE--THIS SUBROUTINE SORTS 
C              THE N ELEMENTS OF THE VECTOR IX
C              AND PUTS THE RESULTING N SORTED VALUES INTO THE
C              SINGLE PRECISION VECTOR IY.
C     INPUT  ARGUMENTS--IX      = THE  VECTOR OF
C                                OBSERVATIONS TO BE SORTED. 
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR IX. 
C                     --IW     = WORKING VECTOR OF SIZE AT LEAST 2*N
C                     --ORD    = CHARACTER 'A' FOR ASCENDING
C                                          'D' FOR DESCENDING  
C     OUTPUT ARGUMENTS--IY      = THE VECTOR
C                                INTO WHICH THE SORTED DATA VALUES
C                                FROM IX WILL BE PLACED.
C---------------------------------------------------------------------
C

      include 'parinclu.h'

      INTEGER I,J,K,L,M,N,JMI,JMK,MID,NM1,IP1,LMI
      INTEGER IW(NDIMAX),IX(NDIMAX),IY(NDIMAX),IT,IMED,IHOLD 
      CHARACTER ORD
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF (N.EQ.1)  GOTO  55
      IHOLD=IX(1)
      DO 60 I=2,N
        IF (IX(I).NE.IHOLD)  GOTO  90
   60 CONTINUE
      DO 61 I=1,N
        IY(I)=IX(I)
   61 CONTINUE
      GOTO 500
   55 IY(1)=IX(1)
      GOTO 500
   90 CONTINUE
C
C-----START POINT-----------------------------------------------------
C
C     COPY THE VECTOR X INTO THE VECTOR Y
      DO 100 I=1,N
        IY(I)=IX(I)
  100 CONTINUE
C
C     CHECK TO SEE IF THE INPUT VECTOR IS ALREADY SORTED
C
      NM1=N-1
      DO 200 I=1,NM1
        IP1=I+1
        IF (IY(I).LE.IY(IP1))  GOTO  200
        GOTO  250
  200 CONTINUE
      GOTO 500
  250 M=1 
      I=1 
      J=N 
  305 IF (I.GE.J)  GOTO  370
  310 K=I 
      MID=(I+J)/2
      IMED=IY(MID)
      IF (IY(I).LE.IMED)  GOTO  320 
      IY(MID)=IY(I)
      IY(I)=IMED
      IMED=IY(MID)
  320 L=J 
      IF (IY(J).GE.IMED)  GOTO  340 
      IY(MID)=IY(J)
      IY(J)=IMED
      IMED=IY(MID)
      IF (IY(I).LE.IMED)  GOTO  340 
      IY(MID)=IY(I)
      IY(I)=IMED
      IMED=IY(MID)
      GOTO  340
  330 IY(L)=IY(K)
      IY(K)=IT
  340 L=L-1
      IF (IY(L).GT.IMED)  GOTO  340 
      IT=IY(L)
  350 K=K+1
      IF (IY(K).LT.IMED)  GOTO  350 
      IF (K.LE.L)  GOTO  330
      LMI=L-I
      JMK=J-K
      IF (LMI.LE.JMK)  GOTO  360
      IW(M)=I
      IW(M+N)=L
      I=K 
      M=M+1
       GOTO 380
  360 IW(M)=K
      IW(M+N)=J
      J=L 
      M=M+1
       GOTO 380
  370 M=M-1
      IF (M.EQ.0) GOTO 500
      I=IW(M)
      J=IW(M+N)
  380 JMI=J-I
      IF (JMI.GE.11)  GOTO 310
      IF (I.EQ.1) GOTO 305
       I=I-1
  390  I=I+1
      IF (I.EQ.J) GOTO 370
      IMED=IY(I+1)
      IF (IY(I).LE.IMED)  GOTO 390 
      K=I 
  395 IY(K+1)=IY(K)
      K=K-1
      IF (IMED.LT.IY(K)) GOTO 395 
      IY(K+1)=IMED
      GOTO  390
C  REORDER IF DESCENDING ORDER
  500 IF (ORD.EQ.'D') THEN
        DO 510 I=1,N/2
	      IT= IY(I)
	      J=N-I+1
	      IY(I) = IY(J)
	      IY(J) = IT
  510   CONTINUE 
      ENDIF
      RETURN    
      END 
      SUBROUTINE XSORT(X,N,Y,IW,ORD)
C---------------------------------------------------------------------
C AS FOR JSORT BUT X AND Y ARE real(8) VECTORS
C---------------------------------------------------------------------
C

      include 'parinclu.h'

      INTEGER I,J,K,L,M,N,JMI,JMK,MID,NM1,IP1,LMI
      INTEGER IW(NDIMAX)
      real(8) X(NDIMAX),Y(NDIMAX),T,AMED,HOLD 
      CHARACTER ORD
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF (N.EQ.1)  GOTO  55
      HOLD=X(1)
      DO 60 I=2,N
        IF (X(I).NE.HOLD)  GOTO  90
   60 CONTINUE
      DO 61 I=1,N
        Y(I)=X(I)
   61 CONTINUE
      GOTO 500
   55 Y(1)=X(1)
      GOTO 500
   90 CONTINUE
C
C-----START POINT-----------------------------------------------------
C
C     COPY THE VECTOR X INTO THE VECTOR Y
      DO 100 I=1,N
        Y(I)=X(I)
  100 CONTINUE
C
C     CHECK TO SEE IF THE INPUT VECTOR IS ALREADY SORTED
C
      NM1=N-1
      DO 200 I=1,NM1
        IP1=I+1
        IF (Y(I).LE.Y(IP1))  GOTO  200
        GOTO  250
  200 CONTINUE
      GOTO 500
  250 M=1 
      I=1 
      J=N 
  305 IF (I.GE.J)  GOTO  370
  310 K=I 
      MID=(I+J)/2
      AMED=Y(MID)
      IF (Y(I).LE.AMED)  GOTO  320 
      Y(MID)=Y(I)
      Y(I)=AMED
      AMED=Y(MID)
  320 L=J 
      IF (Y(J).GE.AMED)  GOTO  340 
      Y(MID)=Y(J)
      Y(J)=AMED
      AMED=Y(MID)
      IF (Y(I).LE.AMED)  GOTO  340 
      Y(MID)=Y(I)
      Y(I)=AMED
      AMED=Y(MID)
      GOTO  340
  330 Y(L)=Y(K)
      Y(K)=T
  340 L=L-1
      IF (Y(L).GT.AMED)  GOTO  340 
      T=Y(L)
  350 K=K+1
      IF (Y(K).LT.AMED)  GOTO  350 
      IF (K.LE.L)  GOTO  330
      LMI=L-I
      JMK=J-K
      IF (LMI.LE.JMK)  GOTO  360
      IW(M)=I
      IW(M+N)=L
      I=K 
      M=M+1
       GOTO 380
  360 IW(M)=K
      IW(M+N)=J
      J=L 
      M=M+1
       GOTO 380
  370 M=M-1
      IF (M.EQ.0) GOTO 500
      I=IW(M)
      J=IW(M+N)
  380 JMI=J-I
      IF (JMI.GE.11)  GOTO 310
      IF (I.EQ.1) GOTO 305
       I=I-1
  390  I=I+1
      IF (I.EQ.J) GOTO 370
      AMED=Y(I+1)
      IF (Y(I).LE.AMED)  GOTO 390 
      K=I 
  395 Y(K+1)=Y(K)
      K=K-1
      IF (AMED.LT.Y(K)) GOTO 395 
      Y(K+1)=AMED
      GOTO  390
C  REORDER IF DESCENDING ORDER
  500 IF (ORD.EQ.'D') THEN
        DO 510 I=1,N/2
	    T= Y(I)
	    J=N-I+1
	    Y(I) = Y(J)
	    Y(J) = T
  510   CONTINUE 
	ENDIF
	RETURN    
      END 

C***********************************************
      real(8) FUNCTION GAMLOG(XVALUE)
C
C     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2
C
C     Calculation of the logarithm of the gamma function
C
      IMPLICIT NONE
      real(8) ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9),
     +          R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XVALUE,
     +          Y, ZERO
C
C     Coefficients of rational functions
C
      DATA R1/-2.66685 51149 5D0, -2.44387 53423 7D1,
     +        -2.19698 95892 8D1,  1.11667 54126 2D1,
     +         3.13060 54762 3D0,  6.07771 38777 1D-1,
     +         1.19400 90572 1D1,  3.14690 11574 9D1,
     +         1.52346 87407 0D1/
      DATA R2/-7.83359 29944 9D1, -1.42046 29668 8D2,
     +         1.37519 41641 6D2,  7.86994 92415 4D1,
     +         4.16438 92222 8D0,  4.70668 76606 0D1,
     +         3.13399 21589 4D2,  2.63505 07472 1D2,
     +         4.33400 02251 4D1/
      DATA R3/-2.12159 57232 3D5,  2.30661 51061 6D5,
     +         2.74647 64470 5D4, -4.02621 11997 5D4,
     +        -2.29660 72978 0D3, -1.16328 49500 4D5,
     +        -1.46025 93751 1D5, -2.42357 40962 9D4,
     +        -5.70691 00932 4D2/
      DATA R4/ 2.79195 31791 8525D-1, 4.91731 76105 05968D-1,
     +         6.92910 59929 1889D-2, 3.35034 38150 22304D0,
     +         6.01245 92597 64103D0/
C
C     Fixed constants
C
      DATA ALR2PI/9.18938 53320 4673D-1/, FOUR/4.D0/, HALF/0.5D0/,
     +     ONE/1.D0/, ONEP5/1.5D0/, TWELVE/12.D0/, ZERO/0.D0/
C
C     Machine-dependant constants.
C     A table of values is given at the top of page 399 of the paper.
C     These values are for the IEEE double-precision format for which
C     B = 2, t = 53 and U = 1023 in the notation of the paper.
C
      DATA XLGE/5.10D6/
C
      X = XVALUE
      GAMLOG = ZERO
C
C     Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined
C
      IF (X .LT. ONEP5) THEN
        IF (X .LT. HALF) THEN
          GAMLOG = -LOG(X)
          Y = X + ONE
C
C     Test whether X < machine epsilon
C
          IF (Y .EQ. ONE) RETURN
        ELSE
          GAMLOG = ZERO
          Y = X
          X = (X - HALF) - HALF
        END IF
        GAMLOG = GAMLOG + X * ((((R1(5)*Y + R1(4))*Y + R1(3))*Y
     +                + R1(2))*Y + R1(1)) / ((((Y + R1(9))*Y + R1(8))*Y
     +                + R1(7))*Y + R1(6))
        RETURN
      END IF
C
C     Calculation for 1.5 <= X < 4.0
C
      IF (X .LT. FOUR) THEN
        Y = (X - ONE) - ONE
        GAMLOG = Y * ((((R2(5)*X + R2(4))*X + R2(3))*X + R2(2))*X
     +              + R2(1)) / ((((X + R2(9))*X + R2(8))*X + R2(7))*X
     +              + R2(6))
        RETURN
      END IF
C
C     Calculation for 4.0 <= X < 12.0
C
      IF (X .LT. TWELVE) THEN
        GAMLOG = ((((R3(5)*X + R3(4))*X + R3(3))*X + R3(2))*X + R3(1)) /
     +            ((((X + R3(9))*X + R3(8))*X + R3(7))*X + R3(6))
        RETURN
      END IF
C
C     Calculation for X >= 12.0
C
      Y = LOG(X)
      GAMLOG = X * (Y - ONE) - HALF * Y + ALR2PI
      IF (X .GT. XLGE) RETURN
      X1 = ONE / X
      X2 = X1 * X1
      GAMLOG = GAMLOG + X1 * ((R4(3)*X2 + R4(2))*X2 + R4(1)) /
     +              ((X2 + R4(5))*X2 + R4(4))
      RETURN
      END
C**************************************************************************       
      FUNCTION DIGAMA(X)
      IMPLICIT NONE
C
C     ALGORITHM AS 103  APPL. STATIST. (1976) VOL.25, NO.3
C
C     Calculates DIGAMMA(X) = D( LOG( GAMMA(X))) / DX
C
      real(8) DIGAMA,ZERO, HALF, ONE
      real(8) R,S,C,S3,S4,S5,D1,Y,X 
C
C     Set constants, SN = Nth Stirling coefficient, D1 = DIGAMMA(1.0)
C
      DATA ZERO/0.0D0/, HALF/0.5D0/, ONE/1.0D0/  
      DATA S, C, S3, S4, S5, D1 /1.D-05, 8.5D0, 8.333333333D-02,
     *    8.3333333333D-03, 3.96825 3968D-03, -0.57721 56649D0/
C
C     Check argument is positive
C
      DIGAMA = ZERO
      Y = X
C
C     Use approximation if argument <= S
C
      IF (Y .LE. S) THEN
        DIGAMA = D1 - ONE / Y
        RETURN
      END IF
C
C     Reduce to DIGAMA(X + N) where (X + N) >= C
C
    1 IF (Y .GE. C) GO TO 2
      DIGAMA = DIGAMA - ONE/Y
      Y = Y + ONE
      GO TO 1
C
C     Use Stirling's (actually de Moivre's) expansion if argument > C
C
    2 R = ONE / Y
      DIGAMA = DIGAMA + DLOG(Y) - HALF*R
      R = R * R
      DIGAMA = DIGAMA - R*(S3 - R*(S4 - R*S5))
      RETURN
      END
C*******************************************************         
      function trigam(x)
      implicit none  
c
c        algorithm as121   Appl. Statist. (1978) vol 27, no. 1
c
c        calculates trigamma(x) = d**2(log(gamma(x))) / dx**2
c
      real(8) trigam,a, b, one, half, b2, b4, b6,b8, x, y, z, zero
      data a, b, one, half /1.0d-4, 5.0d0, 1.0d0, 0.5d0/
      data zero /0.0d0/
c
c        b2, b4, b6 and b8 are Bernoulli numbers
c
      data b2, b4, b6,b8
     */0.1666666667d0, -0.03333333333d0,
     *    0.02380952381d0, -0.03333333333d0/
c
c        check for positive value of x
c
      trigam = zero
      z = x
c
c        use small value approximation if x .le. a
c
      if (z .gt. a) goto 10
      trigam = one / (z * z)
      return
c
c        increase argument to (x+i) .ge. b
c
   10 if (z .ge. b) goto 20
      trigam = trigam + one / (z * z)
      z = z + one
      goto 10
c
c        apply asymptotic formula if argument .ge. b
c
   20 y = one / (z * z)
      trigam = trigam + half * y +
     * (one + y * (b2 + y * (b4 + y * (b6 + y * b8)))) / z
      return
      end
      SUBROUTINE CHOLESKY(A, LDA, N, INFO,ICODE,icons,err)
C   (INITIALLY CALLED DPOFA)
C***PURPOSE  Factor a real symmetric positive definite matrix.
C      Modified (vpd 16/12/95) to accept semi-positive definite matrix
C***LIBRARY   SLATEC (LINPACK) (with CHANGES)
C 
C***
C     CHOLESKY factors a double precision symmetric positive definite
C     matrix.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the symmetric matrix to be factored.  Only the
C                diagonal and upper triangle are used.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix  R  so that  A = TRANS(R)*R
C                where  TRANS(R)  is the transpose.
C                The strict lower triangle is unaltered.
C                If  INFO .NE. 0 , the factorization is not complete.
C
C        INFO    INTEGER
C                = 0  for normal return.
C                = K  signals an error condition.  The leading minor
C                     of order  K  is not positive definite.
C

	include 'parinclu.h'
c
      real(8) TOL                
      PARAMETER(TOL=1.D-10)      
C           
      INTEGER LDA,N,INFO,I,J,JJ,JM1,K,ICONS(0:NDIMAX),ICODE,err
      real(8)  A(LDA,*),DDOT,T,S
C***
         DO 30 J = 1, N
            INFO = J
            S = 0.0D0
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 20
            DO 10 K = 1, JM1
               IF (A(K,K).EQ.0.D0) THEN
                    A(K,J)=0.D0
               ELSE
                   T = A(K,J) - DDOT(K-1,A(1,K),1,A(1,J),1)
                   T = T/A(K,K)
                   A(K,J) = T
                   S = S + T*T
                 ENDIF
   10       CONTINUE
   20       CONTINUE
            S = A(J,J) - S
            IF (S .LE. -TOL) THEN
               err=5
               RETURN
             ELSE IF (S.LE.TOL) THEN
                A(J,J) = 0.D0
                IF (ICODE.EQ.1) THEN
                   JJ=0 
                   DO 25 I=1,ICONS(0)
                     IF (ICONS(I).EQ.J) JJ=1 
  25               CONTINUE
                   IF (JJ.EQ.0) THEN   
                     ICONS(0) = ICONS(0) + 1                
                     ICONS(ICONS(0))= J
                     DO 28 I=J+1,N
                       A(J,K)=0.D0
  28                 CONTINUE
                   ENDIF 
                ENDIF
             ELSE
                A(J,J) = SQRT(S)
            ENDIF 
30    CONTINUE
         INFO = 0
   40 CONTINUE
      RETURN
      END
       SUBROUTINE INVDET(A, LDA, N, DLDET, JOB)
C***(initially called DPODI)
C***PURPOSE  Compute the log-determinant and/or inverse of a certain real
C            symmetric positive definite matrix using the factors
C            computed by CHOLESKY.
C***LIBRARY   SLATEC (LINPACK) (with CHANGES)
C
C     INVDET computes the (non zero) logdeterminant and (generalized) inverse of a certain
C     double precision symmetric positive definite matrix (see below)
C     using the factor CHOLESKY.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output  A  from CHOLESKY
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        JOB     INTEGER
C                = 11   both determinant and inverse.
C                = 01   inverse only.
C                = 10   determinant only.
C
C     On Return
C
C        A     INVDET produces the upper half of INVERSE(A) .
C                Elements of  A  below the diagonal are unchanged.
C                If the units digit of JOB is zero,  A  is unchanged.
C
C        DLDET     DOUBLE PRECISION
C                determinant of  A if requested.
C                Otherwise not referenced.)
C

	include 'parinclu.h'

      INTEGER LDA,N,JOB, I,J,JM1,K,KP1 
      DOUBLE PRECISION A(LDA,*), DLDET,T
C
C     COMPUTE LOG DETERMINANT SKIPPING ZERO DIAGONALS
C
      IF (JOB.GE.10) THEN
         DLDET=0.0D0
         DO 50 I = 1, N
           IF (A(I,I).GT.0.D0) DLDET = DLDET+DLOG(A(I,I))
   50    CONTINUE
         DLDET = 2.D0 * DLDET
      ENDIF       
        IF (MOD(JOB,10) .EQ. 0) RETURN
C
C     COMPUTE INVERSE(R)
C
         DO 100 K = 1, N
            IF (A(K,K).NE.0.D0) THEN
               A(K,K) = 1.0D0/A(K,K)
               T = -A(K,K)
               CALL DSCAL(K-1,T,A(1,K),1)
               KP1 = K + 1
               IF (N .LT. KP1) GO TO 70
               DO 60 J = KP1, N
                 T = A(K,J)
                 A(K,J) = 0.0D0
                 CALL DAXPY(K,T,A(1,K),1,A(1,J),1)
   60         CONTINUE
   70       CONTINUE
              ELSE
                KP1 = K + 1
                IF (N .LT. KP1) GO TO 90
                DO 80 J = KP1, N
                  A(K,J) = 0.0D0
   80         CONTINUE  
             ENDIF
   90       CONTINUE

  100    CONTINUE
C
C        FORM  INVERSE(R) * TRANS(INVERSE(R))
C
         DO 130 J = 1, N
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = A(K,J)
               CALL DAXPY(K,T,A(1,J),1,A(1,K),1)
  110       CONTINUE
  120       CONTINUE
            T = A(J,J)
            CALL DSCAL(J,T,A(1,J),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
      SUBROUTINE DSCAL (N, DA, DX, INCX)
C***
C***PURPOSE  Multiply a vector by a constant.
C***LIBRARY   SLATEC (BLAS)
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DA  double precision scale factor
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C
C     --Output--
C       DX  double precision result (unchanged if N.LE.0)
C
C     Replace double precision DX by double precision DA*DX.
C     For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX),
C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
C

      implicit none

       DOUBLE PRECISION DA, DX(*)
      INTEGER I, INCX, IX, M, MP1, N
C 
      IF (N .LE. 0) RETURN
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        DX(IX) = DA*DX(IX)
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increment equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 M = MOD(N,5)
      IF (M .EQ. 0) GOTO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF (N .LT. 5) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I+1) = DA*DX(I+1)
        DX(I+2) = DA*DX(I+2)
        DX(I+3) = DA*DX(I+3)
        DX(I+4) = DA*DX(I+4)
   50 CONTINUE
      RETURN
      END

      subroutine cox(iconst,rconst,istime,squant,includ,info,itabl,
     +     xtabl,nrr,info2,itabl2,xtabl2,iplus1,iplus2,ipedig,nrr2,
     +     nccov,ndcov,nstime,nquant,igam,inor,xgam,xnor,bound,
     +     icons,beta,std,gradf,hess,xmom,surv,xlik,idf,km,resid,ut)
C//////////////////////////////////////////////////////////////////////*
C  COX           *                                    *  23 / 09 /1997 *
C                *                        Version 3.0 of March 1998    *
C                *            modified for R by J.K.Lindsey, June 1998 *
C//////////////////////////////////////////////////////////////////////*
c
      include 'parinclu.h'

      integer iconst(20)
      double precision rconst(2)

      integer ndcov,nccov,iter_bf,model,ut
      integer nrr,itabl(nrr,ndcov),err,nrr2
      integer nquant,nstime,nrule,nrelmat,nrelmat2,nani
      INTEGER I,J,JJ,NDIM,IFAIL,NSTATE,NBASE,NSURV,NCONS,NTOT
      INTEGER KITER,N,ITGAUSS,NCOVMOD,NSTRATA,NKAPLAN,ICRHO
      INTEGER NPEST,ITOEV,NSTATUS,NOPTION
      INTEGER NRAND,MAXRAND,ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,NALL_TIM
      INTEGER ISTIME(NSTIME),INCLUD(nccov+ndcov)
      integer INFO(NRR,4),itabl2(nrr2,ndcov),info2(nrr2,4)
      integer IPLUS1(nccov),IPLUS2(ndcov),IPEDIG(4,NDIMAX)
      INTEGER IUSER(3),NFAIL(NDIMAX),ICONS(0:NDIMAX),NCOL(MXEF)
      INTEGER IDF(2),IFIRST(MXEF),ILAST(MXEF),IANAL(MXEF)
      INTEGER NCOEF(0:MXEF),ICOEF(0:MXEF),INOR(0:NCCOV+NDCOV,3)
      INTEGER IRANK(NDIMAX),IGAM(0:NCCOV+NDCOV,3)
C
      real(8) F,W,OBJ,LOB,UPB,BEST,DFMIN
      real(8) DLDET,XCOEF(MXEF),EPS_BF
      real(8) BETA(NDIMAX),GRADF(NDIMAX)         
      real(8) XLIK(2),BOUND(NCCOV+NDCOV,3)
      real(8) STD(NDIMAX),STORBETA(NDIMAX,0:NPGAUSS)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) WGAUSS(5,5),XGAUSS(5,5),XMOM(0:3),XMOMP(2),W2
      real(8) OBJ0,XMUK,XMUKM1,SIGK,SIGKM1,R1,R2,VAL0,VAL1,GVAL
      real(8) SQUANT(NQUANT),xtabl(nrr,nccov),xtabl2(nrr2,nccov)
      real(8) HESS(NDIMAX,NDIMAX),valrho(MXSTRA),km(ut,4),resid(ut,3)
      real(8) surv(nstimax*nrr,3)
      double precision ftomin
      EXTERNAL  FTOMIN
C           
      COMMON/BL1/IFIRST,ILAST,NCOL,IRANK
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL5/NFAIL,NTOT
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL13/ICHECK,INTEGAM,NWITHIN,IWITHCOL,
     + NJOINT,NINTTDEP,NDIMB,NDENS,EPS_BF
      COMMON/BL14/NDIM
      COMMON/BL17/XCOEF,NCOEF,ICOEF
      common/bl21/model
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
C-----------------------------------------------------------------------
C   ROOTS USED IN THE GAUSS-HERMITE QUADRATURE  
C-----------------------------------------------------------------------
      DATA(WGAUSS(J,3),J=1,3)/
     + 0.417771379105166318,1.67108551642066683,0.417771379105166318/
      DATA(WGAUSS(J,4),J=1,4)/
     + 0.114993714684505977,1.13832042263099442,
     + 1.13832042263099487,0.114993714684505977/
      DATA(WGAUSS(J,5),J=1,5)/
     + 0.0282181455332160026,0.556661785214017657,
     + 1.33686841313653382,0.556661785214018656,0.02821814553321600266/
      DATA(XGAUSS(J,3),J=1,3)/
     + -1.73205080756887786,0.D0,1.73205080756887786/
      DATA(XGAUSS(J,4),J=1,4)/
     + -2.33441421833897689,-0.741963784302725915,
     + 0.741963784302725360,2.33441421833897689/
      DATA(XGAUSS(J,5),J=1,5)/
     + -2.85697001387280558,-1.35562617997426593,0.D0,
     + 1.35562617997426460,2.85697001387280558/

c      DATA((WGAUSS(J,I),J=1,I),I=3,5)/
c     + 0.417771379105166318,1.67108551642066683,0.417771379105166318,
c     + 0.114993714684505977,1.13832042263099442,
c     + 1.13832042263099487,0.114993714684505977,
c     + 0.0282181455332160026,0.556661785214017657,
c     + 1.33686841313653382,0.556661785214018656,0.02821814553321600266/
c      DATA((XGAUSS(J,I),J=1,I),I=3,5)/
c     + -1.73205080756887786,0.D0,1.73205080756887786,
c     + -2.33441421833897689,-0.741963784302725915,
c     + 0.741963784302725360,2.33441421833897689,
c     + -2.85697001387280558,-1.35562617997426593,0.D0,
c     + 1.35562617997426460,2.85697001387280558/

c step 0 : store constants

      ncons=iconst(1)
      nsurv=iconst(2)
      nstrata=iconst(3)
      nrelmat=iconst(5)
      integam=iconst(6)
      nwithin=iconst(7)
      ncovmod=iconst(8)
      nall_tim=iconst(9)
      nbase=iconst(10)
      icrho=iconst(11)
      ninttdep=iconst(12)
      njoint=iconst(13)
      npest=iconst(14)
      model=iconst(15)
      nrule=iconst(16)
      nkaplan=iconst(17)
      iter_bf=iconst(18)
      maxrand=nccov+ndcov
      nrelmat2=nrelmat

      eps_bf=rconst(2)

      err=0
      itoev=0
C 
C-----------------------------------------------------------------------
C STEP 1 : READ MODEL SPECIFICATION , INITIALIZATION
C          CHECK INPUT FILE, COMPUTES ELEMENTARY STATISTICS             
C          PREPARE WORKING FILE FOR SUBROUTINE FCOX 
C-----------------------------------------------------------------------
C
      CALL INIT(NSURV,NCONS,NSTATUS,itabl,xtabl,nrr,
     +     includ,istime,squant,valrho,igam,inor,xgam,icons,beta,info,
     +     nccov,ndcov,nstime,nquant,itabl2,xtabl2,ipedig,nrr2,err)
      if(err.gt.0)goto 9999
C-----------------------------------------------------------------------
C STEP 2 : FIND OR CHECK CONSTRAINTS                
C-----------------------------------------------------------------------
      IF (NCOVMOD.EQ.0) GOTO 80
C-----------------------------------------------------------------------
C IF NCONS = -1, LOOK FOR CONSTRAINTS               
C-----------------------------------------------------------------------
      IF (NCONS.EQ.-1) THEN     
       DO 10 I=1,NDIM          
            IF (NFAIL(I).NE.0) BETA(I) = 0.05 * MOD(I,5)
  10   CONTINUE                
       NOPTION=90
       CALL FCOX2(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,nrr,icons,
     +      XGAM,XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,
     +      ipedig,err,hess)
       if(err.gt.0)goto 9999
       NOPTION=0               
C       CALL CHOLESKY(HESS,NDIMAX,NDIM,J,0,icons)                  
C-----------------------------------------------------------------------
C IF NCONS = 2, CHECK THAT THE CONSTRAINTS SUPPLIED BY THE USER ARE     
C VALID     
C-----------------------------------------------------------------------
      ELSE IF (NCONS.EQ.2) THEN 
       DO 20 I=1,NDIM          
            IF (NFAIL(I).NE.0) BETA(I) = 0.05 * MOD(I,5)
  20    CONTINUE
       NOPTION=100               
       ICHECK = 1
       CALL FCOX2(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,nrr,icons,
     +      XGAM,XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,
     +      ipedig,err,hess)
       if(err.gt.0)goto 9999
       NOPTION=0               
      ENDIF 
C           
C-----------------------------------------------------------------------
C STEP 3 : CALL THE MAXIMIZATION SUBROUTINE
C-----------------------------------------------------------------------
   80 IFAIL = 0
      IUSER(3)=-1
      if(nkaplan.eq.1)goto 9998
C
C-----------------------------------------------------------------------
C COMPUTE FCOX WITH NO COVARIATE
C-----------------------------------------------------------------------
      DO 120 J=1,NCCOV+NDCOV
       IANAL(J)=0
  120 CONTINUE
      DO 15 I=1,NDIM            
        IF (NFAIL(I).NE.0) BETA(I) = 0.D0
  15  CONTINUE
      CALL FCOX(BETA,F,GRADF,NDIM,NSTATE,IUSER,nrr,icons,XGAM,XNOR,
     +     BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,ipedig,err)
      if(err.gt.0)goto 9999
      IDF(1) = 0              
      XLIK(1) = F      
      IF (NCOVMOD.EQ.0) THEN
        GOTO 750
      ENDIF
C-----------------------------------------------------------------------
C LAST ONE = FULL MODEL         
C-----------------------------------------------------------------------
      DO 200 J=1,NCCOV+NDCOV
       IANAL(J)=1
  200 CONTINUE
      IF (NPEST.EQ.0) THEN
        CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +       IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
        if(err.gt.0)goto 9999
      ELSE IF (NPEST.GE.1) THEN  
         JJ=0
         DO 202 J=1,MAXRAND
          IF (BOUND(J,1).GT.0.D0) JJ=J
  202   CONTINUE
        IF (NPEST.EQ.100) IUSER(3)=1
        LOB=BOUND(JJ,1)
        UPB=BOUND(JJ,2)
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C added on September 23, 1997
C OBJ is the minimum value of FTOMIN
        BEST=DFMIN(LOB,UPB,FTOMIN,BOUND(JJ,3),OBJ,IGAM,
     +       INOR,XGAM,XNOR,BOUND,icons,beta,info,nrr,nccov,ndcov,
     +       hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
       IF (NPEST.EQ.100) THEN 
         DO 690 J=1,NDIM
           STORBETA(J,0)=BETA(J) 
  690    CONTINUE
         OBJ0= OBJ
         XMUKM1=DLOG(BEST)
         SIGKM1= DABS(XMUKM1/10.D0)
       KITER=0 
       DO 700 ITGAUSS=1,NITER_GAUSS
         IF (KITER.EQ.1) GOTO 701
         DO 705 N=0,3
           XMOM(N)=0.D0
 705     CONTINUE
         XMUK=0.D0
         SIGK=0.D0
         DO 710 I=1,NPGAUSS
           VAL0=XMUKM1+ SIGKM1 * XGAUSS(I,NPGAUSS)
           VAL1 = 0.5D0 * XGAUSS(I,NPGAUSS) * XGAUSS(I,NPGAUSS)
           W= DEXP(VAL0)
           IF (IGAM(JJ,1).EQ.1) THEN
             XGAM(JJ) = W
           ELSE
             XNOR(JJ) = W
           ENDIF
           IF (ITGAUSS.GT.1) THEN
             DO 712 J=1,NDIM
               BETA(J)=STORBETA(J,I) 
  712        CONTINUE
           ENDIF
           CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +          IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +          hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
           DO 714 J=1,NDIM
             STORBETA(J,I)=BETA(J) 
  714      CONTINUE
           GVAL =  OBJ0 - OBJ
           R1= SIGKM1 * VAL0 * DEXP(VAL0 + VAL1 + GVAL)
           R2= R1 * VAL0
           XMUK = XMUK + WGAUSS(I,NPGAUSS) * R1
           SIGK = SIGK + WGAUSS(I,NPGAUSS) * R2
           DO 720 N=0,3
              W = SIGKM1*DEXP((N+1.D0) * VAL0 + VAL1 + GVAL)
              XMOM(N)=XMOM(N) + WGAUSS(I,NPGAUSS) * W
 720       CONTINUE
 710       CONTINUE
           XMUK =XMUK / XMOM(0)
           SIGK =SIGK / XMOM(0)
           SIGK = DSQRT(SIGK - XMUK * XMUK)
           DO 730 N=1,3
              XMOM(N)=XMOM(N)/XMOM(0)
 730       CONTINUE
           XMOM(3) = XMOM(3) - 3.D0 * XMOM(1)* XMOM(2)
     +          + 2.D0 * XMOM(1)**3
           XMOM(2) = DSQRT(XMOM(2) - XMOM(1) * XMOM(1))
           XMOM(3) = XMOM(3) / (XMOM(2)**3)
           W = DABS(XMOM(1) - XMOMP(1))/XMOM(1) 
           W2 = DABS(XMOM(2) - XMOMP(2))/XMOM(2)
           IF (MAX(W,W2).LT.1.D-3) THEN
               KITER=1
           ENDIF
           XMOMP(1) = XMOM(1)
           XMOMP(2) = XMOM(2) 
           XMUKM1=XMUK
           SIGKM1=SIGK
 700    CONTINUE
 701    DO 740 J=1,NDIM
           BETA(J)=STORBETA(J,0) 
  740    CONTINUE
         IF (IGAM(JJ,1).EQ.1) THEN
           XGAM(JJ)=BEST
         ELSE
           XNOR(JJ)=BEST
         ENDIF 
C
       ENDIF
       CALL OPTIMIZE(NDIM,F,OBJ,GRADF,BETA,EPS_BF,IUSER,
     +       IGAM,INOR,XGAM,XNOR,BOUND,icons,info,nrr,nccov,ndcov,
     +    hess,itoev,iter_bf,iplus1,iplus2,ipedig,err)
      ENDIF
C
  750 XLIK(2) = F
      IDF(2) = IUSER(1)
C-----------------------------------------------------------------------
C NSTD = 1 ==> COMPUTE STANDARD ERRORS              
C-----------------------------------------------------------------------
       ICHECK = 0
       NOPTION=0
       CALL FCOX2(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,nrr,icons,
     +      XGAM,XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,
     +      ipedig,err,hess)
       if(err.gt.0)goto 9999   
C
      IF (NSURV.NE.0) THEN          
        CALL BASELINE(NDIM,NBASE,NSURV,0 ,BETA,info,nrr,nccov,ndcov,
     +        nstime,nquant,SQUANT,ISTIME,NALL_TIM,km,resid,ut,info2,
     +        nrr2,iplus1,iplus2,surv,err) 
      ELSE IF (NBASE.NE.0) THEN     
        CALL BASELINE(NDIM,1,NSURV,0 ,BETA,info,nrr,nccov,ndcov,
     +        nstime,nquant,SQUANT,ISTIME,NALL_TIM,km,resid,ut,info2,
     +        nrr2,iplus1,iplus2,surv,err)
      ENDIF
      if(err.gt.0)goto 9999
 9998 IF (NKAPLAN.EQ.1)         
     +     CALL BASELINE(NDIM,0,NSURV,1 ,BETA,info,nrr,nccov,ndcov,
     +     nstime,nquant,SQUANT,ISTIME,NALL_TIM,km,resid,ut,info2,
     +     nrr2,iplus1,iplus2,surv,err) 
C
      if(npest.eq.100.and.kiter.eq.0)err=11
      iconst(2)=ndim
      iconst(3)=itoev
 9999 iconst(1)=err

      RETURN
c
C***********************************************************************
      END
      SUBROUTINE FCOX(BETA,F,GRADF,NDIM,NSTATE,IUSER,nrr,icons,XGAM,
     +     XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,iplus2,ipedig,
     +     err)
C***********************************************************************
C  FCOX          *                                    *  04 / 05 /1993 *
C***********************************************************************
C  COMPUTE THE VALUE OF - COX' PARTIAL LIKELIHOOD AND ITS VECTOR OF    *
C       DERIVATIVES                                                    *
C***********************************************************************
c
	include 'parinclu.h'
c
      real(8) CONST
C
      INTEGER NDIM,NSTATE,IUSER(3),nccov,ndcov,nrr
      INTEGER NRAND,MAXRAND,I,J,JJ,K,IDF,ICODE,IPRETIM                   
      INTEGER IA,IP,IM,IGPM,nani,nrec,err,NRULE,NRELMAT,NRELMAT2
      INTEGER IPLUS1(nccov),IPLUS2(ndcov),ICONS(0:NDIMAX)                 
      INTEGER IANAL(MXEF),IPRESTR,NSTRATA,INOR(0:NCCOV+NDCOV,3)
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)
      INTEGER IGAM(0:NCCOV+NDCOV,3),NCOEF(0:MXEF),ICOEF(0:MXEF)
      INTEGER ICRHO,IPEDIG(4,NDIMAX)
C           
      REAL XDATA(NRECMAX,MXEF_USED)
      real(8) CUM_RISK,F,U,UMINUS,W,WMINUS,XFAIL,XCOV,R,X,DM1(7)         
      real(8) BETA(NDIMAX),GRADF(NDIMAX),DC_RISK(NDIMAX)         
      real(8) GAMLOG,XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
      real(8) XCOEF(MXEF),BOUND(NCCOV+NDCOV,3)
C           
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL17/XCOEF,NCOEF,ICOEF
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA 
C
      EQUIVALENCE (IDATA(1,1),XDATA(1,1)) 
      DATA (DM1(I),I=1,7)/1.D0,1.33333333333333D0,2.D0,                 
     +  1.D0,1.3333333333D0,1.06666666667D0,1.4545454545455D0/  
        
C           
C-----------------------------------------------------------------------
C  INITIALIZATION               
C-----------------------------------------------------------------------
C
      F = 0.D0      
      XFAIL = 0.D0              
      IPRESTR=0                 
      CUM_RISK = 0.D0           
      DO 10 I = 1,NDIM          
       GRADF(I) = 0.D0          
       DC_RISK(I) = 0.D0        
   10 CONTINUE                  
      IDF=0 
      IPRETIM = 0               
C-----------------------------------------------------------------------
C  CONSTRAINT(S)                
C-----------------------------------------------------------------------
      DO 15 I = 1,ICONS(0)   
       BETA(ICONS(I)) = 0.D0    
   15 CONTINUE
C           
C-----------------------------------------------------------------------
C   RANDOM EFFECTS              
C-----------------------------------------------------------------------
      IF (NRAND.NE.0) THEN      
       DO 300 I = 1,MAXRAND     
       IF (IGAM(I,1).EQ.1) THEN 
             U = XGAM(I) * DLOG(XGAM(I)) - GAMLOG(XGAM(I))              
             DO 310 J = IGAM(I,2),IGAM(I,3)         
             W = DEXP(BETA(J))  
             F = F - U - XGAM(I) * (BETA(J) - W)    
             GRADF(J) = GRADF(J) - XGAM(I) * (1.D0 - W)                 
  310        CONTINUE           
       ELSE IF (INOR(I,1).EQ.1) THEN               
             CONST = -0.5D0 * DLOG(XNOR(I)) 
             IF (NRELMAT.EQ.0) THEN
               U = 1.D0 / XNOR(I)                
               DO 312 J = INOR(I,2),INOR(I,3)         
                  W = U * BETA(J)                   
                  F = F -CONST+ 0.5D0 * W * BETA(J)                   
                  GRADF(J) = GRADF(J) + W           
  312          CONTINUE         
             ELSE               
             U = 1.D0 / XNOR(NRELMAT2)              
             IF (NRULE.EQ.1.OR.NRULE.EQ.3) THEN     
                 DO 320 K=1,NANI 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LE.0) GOTO 320          
                   F = F -CONST                   
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<   
                   IA = IPEDIG(1,K)                 
                   IP = IPEDIG(3,K)                 
                   IM = IPEDIG(4,K)                 
                   X = BETA(IA) 
                   IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                
                   IF (IM.GT.0) X = X - 0.5D0 * BETA(IM)                
                   X = U * DM1(IPEDIG(2,K)) * X     
                   GRADF(IA) = GRADF(IA) + X        
                   IF (IP.GT.0) GRADF(IP) = GRADF(IP) - 0.5D0 * X       
                   IF (IM.GT.0) GRADF(IM) = GRADF(IM) - 0.5D0 * X       
 320             CONTINUE       
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C Modified on September 26, 1997
              J=I
              IF (NRULE.EQ.3) J=I+1
               DO 322 K=INOR(I,2),INOR(J,3)
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  F = F +  0.5 * BETA(K) * GRADF(K) 
 322           CONTINUE       
             ELSE IF (NRULE.EQ.2) THEN              
                 DO 330 K=1,NANI  
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LE.0) GOTO 330          
                   F= F - CONST
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
                   IA = IPEDIG(1,K)                 
                   IP = IPEDIG(3,K)                 
                   IGPM = IPEDIG(4,K)               
                   X = BETA(IA) 
                   IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                
                   IF (IGPM.GT.0) X = X - 0.25D0 * BETA(IGPM)           
                   X = U * DM1(IPEDIG(2,K)) * X     
                   GRADF(IA) = GRADF(IA) + X        
                   IF (IP.GT.0) GRADF(IP) = GRADF(IP) - 0.5D0 * X       
                   IF (IGPM.GT.0) GRADF(IGPM)=GRADF(IGPM) -0.25D0 * X   
 330             CONTINUE       
                 DO 332 K=INOR(I,2),INOR(I,3)  
                   F = F + 0.5 * BETA(K) * GRADF(K) 
 332             CONTINUE       
             ENDIF              
             ENDIF              
       ENDIF
  300   CONTINUE                
      ENDIF 
C
 100  continue
C           
C-----------------------------------------------------------------------
C   FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
      DO 190 I=1,NREC           
C           
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD WAS THE LAST RECORD WITH TIME = IPRETIM      
C   UPDATE F (COX PARTIAL LIKELIHOOD) AND GRADF (ITS GRADIENT)          
C   XFAIL IS THE NUMBER FAILING AT TIME IPRETIM     
C-----------------------------------------------------------------------
       IF ((INFO(I,1).NE.IPRETIM).AND.(IPRETIM.NE.0)) THEN              
       IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                  
            F = F + XFAIL * DLOG(CUM_RISK)          
            DO 20 J=1,NDIM      
             IF (DC_RISK(J).NE.0.D0)                
     +           GRADF(J) = GRADF(J) + XFAIL * DC_RISK(J) /  CUM_RISK   
   20       CONTINUE            
            XFAIL = 0.D0        
       ENDIF
       ENDIF
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD HAS A DIFFERENT STRATUM ID ==> NEW STRATUM   
C   INITIALIZE CUM_RISK (CUMULATIVE RISK FOR ALL ANIMALS) AND VECTOR    
C   DC_RISK (CUMULATED CONTRIBUTION FOR EACH LEVEL TO BE USED IN        
C   COMPUTING GRADF)            
C-----------------------------------------------------------------------
       IPRETIM =INFO(I,1)       
       ICODE = INFO(I,2)        
       IF (NSTRATA.GT.1) THEN   
       IF (INFO(I,3).NE.IPRESTR) THEN               
             IF (IPRESTR.NE.0) THEN                 
             CUM_RISK = 0.D0    
             DO 25 J = 1,NDIM   
             DC_RISK(J) = 0.D0  
   25          CONTINUE         
             ENDIF              
             IPRESTR=INFO(I,3)  
       ENDIF
       ENDIF
C-----------------------------------------------------------------------
C   U = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE               
C   UMINUS = SUM OF Z'*BETA FOR TIME-INDEPENDENT AFTER CHANGE           
C-----------------------------------------------------------------------
       U = 0.D0                 
       UMINUS = 0.D0            
C-----------------------------------------------------------------------
C        IPLUS1/2 = 0 FOR TIME-INDEPENDENT COVARIATES                   
C             ==> CONTRIBUTES TO U (AND TO UMINUS IF NOT LAST RECORD)   
C             EXCEPT IF LEFT TRUNCATED RECORD  (ONLY UMINUS)    
C        IPLUS1/2 = +1 FOR TIME-DEPENDENT (BEFORE CHANGE)               
C             ==> CONTRIBUTES TO U EXCEPT IF LEFT TRUNCATED RECORD      
C        IPLUS1/2 = -1 FOR TIME-DEPENDENT (AFTER CHANGE)                
C             ==> CONTRIBUTES TO UMINUS IF NOT LAST RECORD              
C     FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES
C     BETA = -999 = - INFINITY => EXP(Z'* BETA) WILL BE = 0             
C-----------------------------------------------------------------------
       K = 0
       DO 35 J=1,NCCOV
       IF (IANAL(J).NE.0) THEN 
            XCOV = XDATA(I,J)
            IF (IPLUS1(J).GT.0) THEN                
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             IF (ICODE.GE.-1) U = U + R             
            ELSE IF (IPLUS1(J).EQ.0) THEN           
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + R
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + R 
             IF (ICODE.LT.0) UMINUS= UMINUS + R     
            ELSE IF (ICODE.LT.0) THEN               
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             UMINUS = UMINUS + R
            ENDIF               
       ENDIF
   35     CONTINUE
       DO 36 J=1,ndcov
            JJ = J + nccov
       IF (IANAL(JJ).NE.0) THEN
            K = IDATA(I,JJ)
            IF (IPLUS2(J).GT.0) THEN
             IF (ICODE.GE.-1) U = U + BETA(K)       
            ELSE IF (IPLUS2(J).EQ.0) THEN
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + BETA(K)
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + BETA(K)
             IF (ICODE.LT.0) UMINUS = UMINUS + BETA(K)                  
            ELSE IF (ICODE.LT.0) THEN
             UMINUS = UMINUS + BETA(K)              
            ENDIF               
       ENDIF
   36     CONTINUE
C           
C-----------------------------------------------------------------------
C   DIVERGENCE ==> STOP         
C-----------------------------------------------------------------------
       IF (U.GT.100.D0) THEN    
       err=3
       RETURN 
       ENDIF
C           
C-----------------------------------------------------------------------
C   COMPUTE W = EXP(U) AND WMINUS = EXP(UMINUS) AND UPDATE CUM_RISK     
C            DEPENDING ON CODE  
C-----------------------------------------------------------------------
       W = 0.D0                 
       WMINUS = 0.D0            
       IF (U.GT.-20.D0) W = DEXP(U)                 
       IF (ICODE.GE.0) THEN     
       CUM_RISK = CUM_RISK + W  
       ELSE IF (ICODE.EQ.-1) THEN                   
       IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)  
       CUM_RISK = CUM_RISK + W - WMINUS             
       ELSE IF (ICODE.LE.-2) THEN                   
       IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)
       CUM_RISK = CUM_RISK - WMINUS                 
       ENDIF
C           
C-----------------------------------------------------------------------
C   UPDATE VECTOR DC_RISK DEPENDING ON CODE         
C   FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES      
C-----------------------------------------------------------------------
       K = 0
       DO 45 J=1,nccov
       IF (IANAL(J).NE.0) THEN 
            XCOV = XDATA(I,J)   
            IF (IPLUS1(J).GT.0) THEN                
             K = K+1            
             IF (ICODE.GE.-1) DC_RISK(K) = DC_RISK(K) + XCOV * W        
            ELSE IF (IPLUS1(J).EQ.0) THEN           
             K = K+1            
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c             DC_RISK(K) = DC_RISK(K) + XCOV *  W   
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) DC_RISK(K) = DC_RISK(K) + XCOV * W                
             IF (ICODE.LT.0) DC_RISK(K) = DC_RISK(K) - XCOV * WMINUS    
            ELSE IF (ICODE.LT.0) THEN               
             DC_RISK(K) = DC_RISK(K) - XCOV * WMINUS
            ENDIF               
       ENDIF
   45     CONTINUE
C           
       DO 46 J=1,ndcov
            JJ = J + nccov
       IF (IANAL(JJ).NE.0) THEN 
            K = IDATA(I,JJ)     
            IF (IPLUS2(J).GT.0) THEN                
             IF (ICODE.GE.-1) DC_RISK(K) = DC_RISK(K) + W               
            ELSE IF (IPLUS2(J).EQ.0) THEN   
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c             DC_RISK(K) = DC_RISK(K) + W   
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) DC_RISK(K) = DC_RISK(K) + W   
             IF (ICODE.LT.0) DC_RISK(K) = DC_RISK(K) - WMINUS           
            ELSE IF (ICODE.LT.0) THEN               
             DC_RISK(K) = DC_RISK(K) - WMINUS       
            ENDIF               
       ENDIF
   46     CONTINUE
C           
C-----------------------------------------------------------------------
C   IF UNCENSORED RECORD, UPDATE F AND GRADF        
C-----------------------------------------------------------------------
       K = 0
       IF (ICODE.GE.1) THEN     
       XFAIL = XFAIL + 1.D0     
       F = F - U
       DO 50 J=1,nccov
       IF (IANAL(J).NE.0) THEN 
            IF (IPLUS1(J).GE.0) THEN                
             K = K+1            
             GRADF(K) = GRADF(K) - XDATA(I,J)       
            ENDIF               
       ENDIF
   50     CONTINUE
       DO 51 J=1,ndcov
            JJ = J + nccov
       IF (IANAL(JJ).NE.0) THEN 
            IF (IPLUS2(J).GE.0) THEN                
             K = IDATA(I,JJ)    
             GRADF(K) = GRADF(K) - 1.D0             
            ENDIF               
       ENDIF
   51     CONTINUE              
       ENDIF
C           
  190 CONTINUE
C           
C-----------------------------------------------------------------------
C   LAST CONTRIBUTION TO F AND GRADF                
C-----------------------------------------------------------------------
      IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                   
       F = F + XFAIL * DLOG(CUM_RISK)               
       DO 200 J=1,NDIM          
       IF (DC_RISK(J).NE.0.D0)  
     +    GRADF(J) = GRADF(J) + XFAIL * DC_RISK(J) /  CUM_RISK          
  200   CONTINUE                
      ENDIF 
C           
C-----------------------------------------------------------------------
C   CONSTRAINTS ON GRADF + COUNT NON ZERO PARAMETERS
C-----------------------------------------------------------------------
      DO 250 I = 1,ICONS(0)     
       GRADF(ICONS(I)) = 0.D0   
  250 CONTINUE                  
      IDF=0 
      DO 260 J=1,NDIM           
       IF (GRADF(J).NE.0.D0) IDF = IDF + 1          
  260 CONTINUE                  
      IUSER(1) = IDF            
      IF (NSTATE.EQ.1) THEN     
       IUSER(2) = 1             
      ELSE  
       IUSER(2) = IUSER(2) + 1  
      ENDIF
C           
      RETURN
C***********************************************************************
      END   
      SUBROUTINE FCOX2(BETA,F,GRADF,STD,DLDET,NSTATUS,NOPTION,NDIM,nrr,
     +     icons,XGAM,XNOR,BOUND,IGAM,INOR,info,nccov,ndcov,iplus1,
     +     iplus2,ipedig,err,hess)
C***********************************************************************
C  FCOX2         *                                    *  04 / 05 /1996 *
C***********************************************************************
C  COMPUTE THE FINAL VALUE OF - COX' PARTIAL LIKELIHOOD, ITS VECTOR OF *
C       DERIVATIVES AND THE MATRIX OF SECOND DERIVATIVES TO COMPUTE    *
C       THE STANDARD ERRORS OF THE ESTIMATES                           *
C***********************************************************************
c
      include 'parinclu.h'
c
      real(8) CONST
C
      INTEGER NRAND,MAXRAND,NDIM,ICHECK,nccov,ndcov,nrr,err
      INTEGER I,J,JJ,K,L,LL,N,NREC,ICODE,IPRETIM,JOB
      INTEGER IPRESTR,NSTRATA,NRULE,NRELMAT,NANI,NRELMAT2
      INTEGER IA,IP,IM,IGPM,NSTATUS,NOPTION,NMAX
      INTEGER IPLUS1(nccov),IPLUS2(ndcov),ICONS(0:NDIMAX)
      INTEGER IPEDIG(4,NDIMAX),INOR(0:NCCOV+NDCOV,3)
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)   
      INTEGER IGAM(0:NCCOV+NDCOV,3),NCOEF(0:MXEF),ICOEF(0:MXEF)
C
      REAL XDATA(NRECMAX,MXEF_USED)
      real(8) XCOEF(MXEF)
      real(8) CUM_RISK,F,U,UMINUS,W,WMINUS,XFAIL,XCOV,XCOV2,R,X,DM1(7)  
      real(8) DLDET 
      real(8) BETA(NDIMAX),GRADF(NDIMAX),DC_RISK(NDIMAX)                 
      real(8) GAMLOG,HESS(NDIMAX,NDIMAX),STD(NDIMAX)      
      PARAMETER(NMAX=10)
      real(8) D2C_RISK(NDIMAX,NDIMAX),BOUND(NCCOV+NDCOV,3)
      real(8) XGAM(0:NCCOV+NDCOV),XNOR(0:NCCOV+NDCOV)
C
      COMMON/BL8/NRAND,MAXRAND
      COMMON/BL17/XCOEF,NCOEF,ICOEF
      COMMON/PARAM/NSTRATA
      COMMON/PEDIG/NRULE,NRELMAT,NANI,NRELMAT2
      COMMON/DATA1/NREC,IDATA               
C
      EQUIVALENCE (IDATA(1,1),XDATA(1,1)) 
      DATA (DM1(I),I=1,7)/1.D0,1.33333333333333D0,2.D0,                 
     +  1.D0,1.3333333333D0,1.06666666667D0,1.4545454545455D0/   
C           
C-----------------------------------------------------------------------
C  INITIALIZATION               
C-----------------------------------------------------------------------
C BRUCE EDIT     
      ICHECK = 0
      F = 0.D0                  
      XFAIL = 0.D0              
      IPRESTR=0                 
      CUM_RISK = 0.D0           
      DO 10 I = 1,NDIM          
       GRADF(I) = 0.D0          
       DC_RISK(I) = 0.D0        
       DO 8 J=1,NDIM            
       D2C_RISK(J,I) = 0.D0     
       HESS(J,I) = 0.D0      
    8   CONTINUE                
   10 CONTINUE                  
      IPRETIM = 0     
C-----------------------------------------------------------------------
C  CONSTRAINT(S)                
C-----------------------------------------------------------------------
      IF (ICHECK.NE.1) THEN     
       DO 15 I = 1,ICONS(0)     
       BETA(ICONS(I)) = 0.D0    
   15   CONTINUE                
      ENDIF 
C-----------------------------------------------------------------------
C   RANDOM EFFECTS              
C-----------------------------------------------------------------------
      IF (NRAND.NE.0) THEN      
       DO 300 I = 1,MAXRAND     
       IF (IGAM(I,1).EQ.1) THEN 
             U = XGAM(I) * DLOG(XGAM(I)) - GAMLOG(XGAM(I))              
             DO 310 J = IGAM(I,2),IGAM(I,3)         
             W = DEXP(BETA(J))  
             F = F - U - XGAM(I) * (BETA(J) - W)    
             GRADF(J) = GRADF(J) - XGAM(I) * (1.D0 - W)                 
             HESS(J,J) = HESS(J,J) + XGAM(I) * W                  
  310        CONTINUE           
       ELSE IF (INOR(I,1).EQ.1) THEN
             CONST = -0.5D0 * DLOG(XNOR(I))               
             IF (NRELMAT.EQ.0) THEN                 
             U = 1.D0 / XNOR(I) 
             DO 312 J = INOR(I,2),INOR(I,3)         
                  W = U * BETA(J)                   
                  F = F - CONST + 0.5D0 * W * BETA(J)                   
                  GRADF(J) = GRADF(J) + W           
                  HESS(J,J) = HESS(J,J) + U   
  312          CONTINUE         
             ELSE               
             U = 1.D0 / XNOR(NRELMAT2)              
             IF (NRULE.EQ.1.OR.NRULE.EQ.3) THEN     
             DO 320 K=1,NANI 
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LE.0) GOTO 320          
                   F = F - CONST
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<     
               IA = IPEDIG(1,K)                 
               IP = IPEDIG(3,K)                 
               IM = IPEDIG(4,K)                 
               X = BETA(IA) 
               IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                
               IF (IM.GT.0) X = X - 0.5D0 * BETA(IM)                
               W = U * DM1(IPEDIG(2,K))         
               X = W * X    
               GRADF(IA) = GRADF(IA) + X        
               HESS(IA,IA)= HESS(IA,IA) + W                   
               IF (IP.GT.0) THEN                
                 GRADF(IP) = GRADF(IP) - 0.5D0 * X
                 HESS(IA,IP) = HESS(IA,IP) - 0.5D0 * W          
                 HESS(IP,IA) = HESS(IP,IA) - 0.5D0 * W          
                 HESS(IP,IP) = HESS(IP,IP) + 0.25D0 * W         
               ENDIF        
               IF (IM.GT.0) THEN                
                 GRADF(IM) = GRADF(IM) - 0.5D0 * X
                 HESS(IA,IM) = HESS(IA,IM) - 0.5D0 * W          
                 HESS(IM,IA) = HESS(IM,IA) - 0.5D0 * W          
                 HESS(IM,IM) = HESS(IM,IM) + 0.25D0 * W         
                 IF (IP.GT.0) THEN                
                   HESS(IP,IM) = HESS(IP,IM) + 0.25D0 * W         
                   HESS(IM,IP) = HESS(IM,IP) + 0.25D0 * W         
                 ENDIF        
               ENDIF        
  320        CONTINUE       
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C Modified on September 26, 1997
              J=I
              IF (NRULE.EQ.3) J=I+1
               DO 322 K=INOR(I,2),INOR(J,3)
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                  F = F +  0.5 * BETA(K) * GRADF(K) 
  322          CONTINUE       
             ELSE IF (NRULE.EQ.2) THEN              
                 DO 330 K=1,NANI
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C This line was added on October 21, 1996
                   IF (IPEDIG(2,K).LE.0) GOTO 330          
                   F = F - CONST
C <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<      
                   IA = IPEDIG(1,K)                 
                   IP = IPEDIG(3,K)                 
                   IGPM = IPEDIG(4,K)               
                   X = BETA(IA) 
                   IF (IP.GT.0) X = X - 0.5D0 * BETA(IP)                
                   IF (IGPM.GT.0) X = X - 0.25D0 * BETA(IGPM)           
                   W = U * DM1(IPEDIG(2,K))         
                   X = W * X    
                   GRADF(IA) = GRADF(IA) + X        
                   HESS(IA,IA)= HESS(IA,IA) + W                   
                   IF (IP.GT.0) THEN                
                   GRADF(IP) = GRADF(IP) - 0.5D0 * X
                   HESS(IA,IP) = HESS(IA,IP) - 0.5D0 * W          
                   HESS(IP,IA) = HESS(IP,IA) - 0.5D0 * W          
                   HESS(IP,IP) = HESS(IP,IP) + 0.25D0 * W         
                   ENDIF        
                   IF (IGPM.GT.0) THEN              
                   GRADF(IGPM)=GRADF(IGPM) -0.25D0 * X                  
                   HESS(IA,IGPM)=HESS(IA,IGPM) -0.25D0 * W        
                   HESS(IGPM,IA)=HESS(IGPM,IA) -0.25D0 * W        
                   HESS(IGPM,IGPM)=HESS(IGPM,IGPM)+0.0625D0*W     
                   IF (IP.GT.0) THEN                
                   HESS(IP,IGPM)=HESS(IP,IGPM) +0.125D0 * W       
                   HESS(IGPM,IP)=HESS(IGPM,IP) +0.125D0 * W       
                   ENDIF        
                   ENDIF        
  330            CONTINUE       
                 DO 332 K=INOR(I,2),INOR(I,3) 
                   F = F + 0.5 * BETA(K) * GRADF(K) 
  332            CONTINUE       
               ENDIF              
             ENDIF              
       ENDIF
  300   CONTINUE                
      ENDIF
 100  continue
C           
C-----------------------------------------------------------------------
C   FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
      DO 190 I=1,NREC           
C           
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD WAS THE LAST RECORD WITH TIME = IPRETIM      
C   UPDATE F (COX PARTIAL LIKELIHOOD) AND GRADF (ITS GRADIENT)          
C   XFAIL IS THE NUMBER FAILING AT TIME IPRETIM     
C-----------------------------------------------------------------------
       IF ((INFO(I,1).NE.IPRETIM).AND.(IPRETIM.NE.0)) THEN              
       IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                  
            F = F + XFAIL * DLOG(CUM_RISK)          
            DO 20 J=1,NDIM      
             IF (DC_RISK(J).NE.0.D0) THEN           
             GRADF(J) = GRADF(J) + XFAIL * DC_RISK(J) / CUM_RISK        
             DO 18 K=1,NDIM     
                  IF (DC_RISK(K).NE.0.D0)           
     +            HESS(K,J)= HESS(K,J) + (XFAIL / CUM_RISK) *     
     +            (D2C_RISK(K,J) -(DC_RISK(J) * DC_RISK(K)) / CUM_RISK) 
   18           CONTINUE        
             ENDIF              
   20       CONTINUE            
            XFAIL = 0.D0        
       ENDIF
       ENDIF
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD HAS A DIFFERENT STRATUM ID ==> NEW STRATUM   
C   INITIALIZE CUM_RISK (CUMULATIVE RISK FOR ALL ANIMALS) AND VECTOR    
C   DC_RISK (CUMULATED CONTRIBUTION FOR EACH LEVEL TO BE USED IN        
C   COMPUTING GRADF) + D2C_RISK (TO BE USED IN COMPUTING HESSIAN)       
C-----------------------------------------------------------------------
       IPRETIM =INFO(I,1)       
       ICODE = INFO(I,2)        
       IF (NSTRATA.GT.1) THEN   
       IF (INFO(I,3).NE.IPRESTR) THEN               
             IF (IPRESTR.NE.0) THEN                 
             CUM_RISK = 0.D0    
             DO 25 J = 1,NDIM   
             DC_RISK(J) = 0.D0  
             DO 24 K=1,NDIM     
                   D2C_RISK(K,J) = 0.D0             
   24            CONTINUE       
   25          CONTINUE         
             ENDIF              
             IPRESTR=INFO(I,3)  
       ENDIF
       ENDIF
C-----------------------------------------------------------------------
C   U = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE               
C   UMINUS = SUM OF Z'*BETA FOR TIME-INDEPENDENT AFTER CHANGE           
C-----------------------------------------------------------------------
       U = 0.D0                 
       UMINUS = 0.D0            
C-----------------------------------------------------------------------
C        IPLUS1/2 = 0 FOR TIME-INDEPENDENT COVARIATES                   
C             ==> CONTRIBUTES TO U (AND TO UMINUS IF NOT LAST RECORD)   
C        IPLUS1/2 = +1 FOR TIME-DEPENDENT (BEFORE CHANGE)               
C             ==> CONTRIBUTES TO U EXCEPT IF LEFT TRUNCATED RECORD      
C        IPLUS1/2 = -1 FOR TIME-DEPENDENT (AFTER CHANGE)                
C             ==> CONTRIBUTES TO UMINUS IF NOT LAST RECORD              
C     FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES    
C     BETA = -999 = - INFINITY => EXP(Z'* BETA) WILL BE = 0             
C-----------------------------------------------------------------------
       K = 0
       DO 35 J=1,nccov
            XCOV = XDATA(I,J)
            IF (IPLUS1(J).GT.0) THEN
             K = K+1
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             IF (ICODE.GE.-1) U = U + R             
            ELSE IF (IPLUS1(J).EQ.0) THEN           
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + R
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + R                   
             IF (ICODE.LT.0) UMINUS= UMINUS + R     
            ELSE IF (ICODE.LT.0) THEN               
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             UMINUS = UMINUS + R
            ENDIF               
   35     CONTINUE              
       DO 36 J=1,ndcov
            JJ = J + nccov
            K = IDATA(I,JJ)
            IF (IPLUS2(J).GT.0) THEN                
             IF (ICODE.GE.-1) U = U + BETA(K)       
            ELSE IF (IPLUS2(J).EQ.0) THEN           
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + BETA(K)
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + BETA(K)            
             IF (ICODE.LT.0) UMINUS = UMINUS + BETA(K)                  
            ELSE IF (ICODE.LT.0) THEN               
             UMINUS = UMINUS + BETA(K)              
            ENDIF               
   36     CONTINUE              
C           
C-----------------------------------------------------------------------
C   DIVERGENCE ==> STOP         
C-----------------------------------------------------------------------
       IF (U.GT.100.D0) THEN
       err=3
       RETURN 
       ENDIF
C           
C-----------------------------------------------------------------------
C   COMPUTE W = EXP(U) AND WMINUS = EXP(UMINUS) AND UPDATE CUM_RISK     
C            DEPENDING ON CODE  
C-----------------------------------------------------------------------
       W = 0.D0                 
       WMINUS = 0.D0            
C           
       IF (U.GT.-20.D0) W = DEXP(U)                 
       IF (ICODE.GE.0) THEN     
             CUM_RISK = CUM_RISK + W                
       ELSE IF (ICODE.EQ.-1) THEN                   
             IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)                
             CUM_RISK = CUM_RISK + W - WMINUS       
       ELSE IF (ICODE.LE.-2) THEN                   
             IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)                
             CUM_RISK = CUM_RISK - WMINUS           
       ENDIF
C           
C-----------------------------------------------------------------------
C   UPDATE VECTOR DC_RISK AND MATRIX D2C_RISK DEPENDING ON CODE         
C   FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES      
C-----------------------------------------------------------------------
       K = 0
       DO 45 J=1,nccov
            XCOV = XDATA(I,J)   
            IF (IPLUS1(J).GT.0) THEN                
             K = K+1            
             IF (ICODE.GE.-1) THEN                  
             DC_RISK(K) = DC_RISK(K) + XCOV * W     
             N = 0              
             DO 145 L=1,nccov
                  XCOV2 = XDATA(I,L)                
                  IF (IPLUS1(L).GE.0) THEN          
                   N = N+1      
                   D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV * XCOV2 * W     
                  ENDIF         
  145          CONTINUE         
             DO 245 L=1,ndcov
                  LL = L + nccov
                  IF (IPLUS2(L).GE.0) THEN          
                   N = IDATA(I,LL)                  
                   D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV * W             
                  ENDIF         
  245          CONTINUE         
             ENDIF              
            ELSE IF (IPLUS1(J).EQ.0) THEN           
             K = K+1            
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c was added on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.EQ.-2) GOTO 498
             DC_RISK(K) = DC_RISK(K) + XCOV * W     
             N = 0              
             DO 345 L=1,nccov
             XCOV2 = XDATA(I,L) 
             IF (IPLUS1(L).GE.0) THEN               
                  N = N+1       
                  D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV * XCOV2 * W      
             ENDIF              
  345         CONTINUE          
             DO 445 L=1,ndcov
             LL = L + nccov
             IF (IPLUS2(L).GE.0) THEN               
                  N = IDATA(I,LL)                   
                  D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV * W              
             ENDIF              
  445         CONTINUE          
  498        IF (ICODE.LT.0) THEN                   
             DC_RISK(K) = DC_RISK(K) - XCOV * WMINUS
             N = 0              
             DO 545 L=1,nccov
                  IF (IPLUS1(L).GE.0) N=N+1         
                  IF (IPLUS1(L).LE.0) THEN          
                   XCOV2 = XDATA(I,L)               
                   D2C_RISK(K,N) =D2C_RISK(K,N) -XCOV * XCOV2 * WMINUS  
                  ENDIF         
  545           CONTINUE        
             DO 645 L=1,ndcov
                  LL = L + nccov
                  IF (IPLUS2(L).LE.0) THEN          
                   N = IDATA(I,LL)                  
                   D2C_RISK(K,N) = D2C_RISK(K,N) - XCOV * WMINUS        
                  ENDIF         
  645           CONTINUE        
             ENDIF              
            ELSE IF (ICODE.LT.0) THEN               
             DC_RISK(K) = DC_RISK(K) - XCOV * WMINUS
             N = 0              
             DO 745 L=1,nccov
             IF (IPLUS1(L).GE.0) N=N+1              
             IF (IPLUS1(L).LE.0) THEN               
                  XCOV2 = XDATA(I,L)                
                  D2C_RISK(K,N) = D2C_RISK(K,N) - XCOV * XCOV2 * WMINUS 
             ENDIF              
  745         CONTINUE          
             DO 845 L=1,ndcov
             LL = L + nccov
             IF (IPLUS2(L).LE.0) THEN               
                  N = IDATA(I,LL)                   
                  D2C_RISK(K,N) = D2C_RISK(K,N) - XCOV * WMINUS         
             ENDIF              
  845         CONTINUE          
            ENDIF               
   45     CONTINUE              
C           
       DO 46 J=1,ndcov
            JJ = J + nccov
            K = IDATA(I,JJ)     
            IF (IPLUS2(J).GT.0) THEN                
             IF (ICODE.GE.-1) THEN                  
             DC_RISK(K) = DC_RISK(K) + W            
             N = 0              
             DO 146 L=1,nccov
                  IF (IPLUS1(L).GE.0) N=N+1         
                  IF (IPLUS1(L).GE.0) THEN          
                   XCOV2 = XDATA(I,L)               
                   D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV2 * W            
                  ENDIF         
  146           CONTINUE        
             DO 246 L=1,ndcov
                  LL = L + nccov
                  IF (IPLUS2(L).GE.0) THEN          
                   N = IDATA(I,LL)                  
                   D2C_RISK(K,N) = D2C_RISK(K,N) + W
                  ENDIF         
  246          CONTINUE         
             ENDIF              
            ELSE IF (IPLUS2(J).EQ.0) THEN           
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement was added on October 16, 1996
c  for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.EQ.-2) GOTO 499
             DC_RISK(K) = DC_RISK(K) + W            
             N = 0              
             DO 346 L=1,nccov
             XCOV2 = XDATA(I,L) 
             IF (IPLUS1(L).GE.0) THEN               
                  N = N+1
                  D2C_RISK(K,N) = D2C_RISK(K,N) + XCOV2 * W             
             ENDIF              
  346         CONTINUE          
             DO 446 L=1,ndcov
             LL = L + nccov
             IF (IPLUS2(L).GE.0) THEN               
                   N = IDATA(I,LL)                  
                   D2C_RISK(K,N) = D2C_RISK(K,N) + W
             ENDIF              
  446        CONTINUE          
  499        IF (ICODE.LT.0) THEN                   
             DC_RISK(K) = DC_RISK(K) - WMINUS       
             N = 0              
             DO 546 L=1,nccov
                  IF (IPLUS1(L).GE.0) N=N+1         
                  IF (IPLUS1(L).LE.0) THEN          
                   XCOV2 = XDATA(I,L)               
                   D2C_RISK(K,N) =D2C_RISK(K,N) - XCOV2 * WMINUS        
                  ENDIF         
  546           CONTINUE        
             DO 646 L=1,ndcov
                  LL = L + nccov
                  IF (IPLUS2(L).LE.0) THEN          
                   N = IDATA(I,LL)                  
                   D2C_RISK(K,N) = D2C_RISK(K,N) - WMINUS               
                  ENDIF         
  646           CONTINUE        
             ENDIF              
            ELSE IF (ICODE.LT.0) THEN               
             DC_RISK(K) = DC_RISK(K) - WMINUS       
             N = 0              
             DO 746 L=1,nccov
             IF (IPLUS1(L).GE.0) N=N+1              
             IF (IPLUS1(L).LE.0) THEN               
                  XCOV2 = XDATA(I,L)                
                  D2C_RISK(K,N) = D2C_RISK(K,N) - XCOV2 * WMINUS        
             ENDIF              
  746         CONTINUE          
             DO 846 L=1,ndcov
             LL = L + nccov
             IF (IPLUS2(L).LE.0) THEN               
                  N = IDATA(I,LL)                   
                  D2C_RISK(K,N) = D2C_RISK(K,N) - WMINUS                
             ENDIF              
  846         CONTINUE          
            ENDIF               
   46     CONTINUE              
C           
C-----------------------------------------------------------------------
C   IF UNCENSORED RECORD, UPDATE F AND GRADF        
C-----------------------------------------------------------------------
       K = 0
       IF (ICODE.GE.1) THEN     
       XFAIL = XFAIL + 1.D0     
       F = F - U                
       DO 50 J=1,nccov
            IF (IPLUS1(J).GE.0) THEN                
             K = K+1            
             GRADF(K) = GRADF(K) - XDATA(I,J)       
            ENDIF               
   50     CONTINUE              
       DO 51 J=1,ndcov
            JJ = J + nccov
            IF (IPLUS2(J).GE.0) THEN                
             K = IDATA(I,JJ)    
             GRADF(K) = GRADF(K) - 1.D0             
            ENDIF               
   51     CONTINUE              
       ENDIF
C           
  190 CONTINUE                  
c      IF (NSTOR.NE.1) GOTO 100  
            
c  199 IF (NSTOR.NE.1) CLOSE(4)  
C           
C-----------------------------------------------------------------------
C   LAST CONTRIBUTION TO F AND GRADF                
C-----------------------------------------------------------------------
      IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                   
       F = F + XFAIL * DLOG(CUM_RISK)               
       DO 200 J=1,NDIM          
       IF (DC_RISK(J).NE.0.D0) THEN                 
            GRADF(J) = GRADF(J) + XFAIL * DC_RISK(J) / CUM_RISK         
            DO 205 K=1,NDIM     
             IF (DC_RISK(K).NE.0.D0)                
     +         HESS(K,J)= HESS(K,J) + (XFAIL / CUM_RISK) *        
     +          (D2C_RISK(K,J) -(DC_RISK(J) * DC_RISK(K)) / CUM_RISK)   
  205       CONTINUE            
       ENDIF
  200   CONTINUE                
      ENDIF 
C           
C           
C-----------------------------------------------------------------------
C   CONSTRAINTS ON GRADF AND HESSIAN                
C-----------------------------------------------------------------------
       ICODE=0
       IF (NOPTION.GE.90) THEN
C---------------------------------------------------------------------- 
C  NUMERICAL FACTORIZATION TO LOOK FOR CONSTRAINTS  
C---------------------------------------------------------------------- 
         ICODE=1
         CALL CHOLESKY(HESS,NDIMAX,NDIM,J,ICODE,icons,err)
         if(err.gt.0)return
         IF (NOPTION.EQ.90) RETURN
       ENDIF     
C
      IF (ICHECK.NE.1) THEN     
       DO 210 I = 1,ICONS(0)    
       GRADF(ICONS(I)) = 0.D0   
       DO 209 J=1,NDIM          
             HESS(J,ICONS(I))=0.D0               
             HESS(ICONS(I),J)=0.D0               
  209     CONTINUE              
  210   CONTINUE                
      ENDIF    
C---------------------------------------------------------------------- 
C   END OF THE CONSTRUCTION OF THE HESSIAN          
C----------------------------------------------------------------------
c
c print when debugging (delete the condition NDIM.lt.-1)
c
      IF (NDIM.lt.-1) THEN 
c      IF (NDIM.ne.-1) THEN
      DO 750 J=1,NDIM
        DO 752 I=J,NDIM
           HESS(I,J)=HESS(J,I)
  752   CONTINUE
  750 CONTINUE
c      
       DLDET=0.D0
       ENDIF
C---------------------------------------------------------------------- 
C  NUMERICAL FACTORIZATION             
C----------------------------------------------------------------------
       CALL CHOLESKY(HESS,NDIMAX,NDIM,J,ICODE,icons,err)
       if(err.gt.0)return
C
C---------------------------------------------------------------------- 
C  IF NOPTION = 1 ==> COMPUTATION OF THE LOG-DETERMINANT              
C---------------------------------------------------------------------- 
      IF (NOPTION.EQ.1.OR.ICHECK.GT.0) THEN
	JOB=10
	CALL INVDET(HESS,NDIMAX,NDIM,DLDET,JOB)
      ENDIF
C----------------------------------------------------------------------- 
C  IF NOPTION = 0 ==> COMPUTATION OF THE STANDARD DEVIATIONS                  
C----------------------------------------------------------------------- 
      IF (NOPTION.EQ.0.OR.ICHECK.GT.0) THEN
        JOB=1
        CALL INVDET(HESS,NDIMAX,NDIM,DLDET,JOB)
      DO 754 J=1,NDIM
        DO 756 I=J+1,NDIM
           HESS(I,J)=HESS(J,I)
 756    CONTINUE
 754  CONTINUE
        DO 760 I=1,NDIM
          STD(I) = HESS(I,I)
          IF (STD(I).GT.1.D-10) THEN
             STD(I) = DSQRT(STD(I))
          ELSE IF (STD(I).LT.-1.D-6) THEN
             err=2
             return
          ELSE
            STD(I)=0.D0
          ENDIF
  760   CONTINUE 
      ENDIF                  
C           
      RETURN
C***********************************************************************
      END
      SUBROUTINE BASELINE(NDIM,NBASE,NSURV,NKAPLAN,BETA,info,nrr,nccov,
     +     ndcov,nstime,nquant,SQUANT,ISTIME,NALL_TIM,km,resid,ut,
     +     info2,nrr2,iplus1,iplus2,surv,err)
C***********************************************************************
C  BASELINE      *                                    *  11 / 10 /1993 *
C***********************************************************************
C  COMPUTE THE BASELINE CUMLATIVE HAZARD XLAMB0(T) AND THE SURVIVOR    *
C     FUNCTION S0(T) USING BRESLOW'S FORMULA (BRESLOW, 1974)           *
C***********************************************************************
c
	include 'parinclu.h'
c
      INTEGER NDIM,NSTIME,NALL_TIM,NQUANT,NSURV,NBASE,NKAPLAN,err
      INTEGER IDICHO,ILT,ILT2,IHT,KLT,KLT2,IDIF,IMID,IS,IQ,IT,JST       
      INTEGER IANI,IANIPREV,NEXTJUMP,nrr,ut,nn,nrr2,nccov,ndcov
      INTEGER I,J,JJ,K,K2,KK,N,NREC,ICODE,ISTR
      INTEGER IPRESTR,NSTRATA,NSTR,IPRETIM,ISUP
      INTEGER ICRHO,IC_PREV,NS_PREV
      INTEGER IPLUS1(nccov),IPLUS2(ndcov),IANAL(MXEF)   
      INTEGER NCOEF(0:MXEF),ICOEF(0:MXEF)
C           
      INTEGER IREST(NSTIMAX),IRESQ(NSTIMAX)     
      INTEGER INFO(NRR,4),IDATA(NRECMAX,MXEF_USED)   
      INTEGER IRANGE(2,0:MXSTRA),ISTIME(NSTIMAX)     
      INTEGER INFO2(nrr2,4),IDATA2(MXEF_USED,NRECMAX2)
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c added on October 9, 1997
      INTEGER NRESI
      real(8) km(ut,4),resid(ut,3),xic,xmart,xdev
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C           
      REAL XDATA(NRECMAX,MXEF_USED),XDATA2(MXEF_USED,NRECMAX2)
      real(8) surv(nstimax*nrr,3)
C 
      real(8) XCOEF(MXEF)
      real(8) XJUMP(NTIMMAX),XLAMB0(NTIMMAX),S0(NTIMMAX),VARS0(NTIMMAX)  
      real(8) LOGT(NTIMMAX),LOGLOGS0(NTIMMAX),SQUANT(NSTIMAX)   
      real(8) CUM_RISK,CUM_RIS2,CUM_RSKP,U,UMINUS,W,WMINUS,XFAIL,XCOV,R  
      real(8) XSTIME,XL_PREV,SX,SX2,REST(NSTIMAX)    
      real(8) BETA(NDIMAX)   
C           
      COMMON/BL3/NSTRATA,ICRHO,IANAL
      COMMON/BL6/XJUMP          
      COMMON/BL17/XCOEF,NCOEF,ICOEF
      COMMON/DATA1/NREC,IDATA                   
      COMMON/DATA2/IDATA2               
C
      EQUIVALENCE (IDATA(1,1),XDATA(1,1))           
      EQUIVALENCE (IDATA2(1,1),XDATA2(1,1))         
C           
C-----------------------------------------------------------------------
C  INITIALIZATION               
C-----------------------------------------------------------------------
C           
      XFAIL = 0.D0              
      IPRESTR=0                 
      CUM_RISK = 0.D0           
      IPRETIM = 0               
      IT = 0
      IRANGE(2,0) = 0

 100  continue
C           
C-----------------------------------------------------------------------
C   FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
      DO 190 I=1,NREC           
C           
C-----------------------------------------------------------------------
C   IF FIRST RECORD :           
C       IRANGE(1,*) = STRATUM LABEL = INFO(I,3)     
C       IRANGE(2,*) = INDEX OF LARGEST TIME OF S0(T)
C-----------------------------------------------------------------------
       IF (IPRETIM.EQ.0) THEN   
       ISTR=1                   
       IRANGE(1,ISTR)= INFO(I,3)
       ENDIF
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD WAS THE LAST RECORD WITH TIME = IPRETIM      
C   UPDATE XLAMB0(T) AND XJUMP(T)                   
C   XFAIL IS THE NUMBER FAILING AT TIME IPRETIM     
C-----------------------------------------------------------------------
       IF ((INFO(I,1).NE.IPRETIM).AND.(IPRETIM.NE.0)) THEN              
       IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                  
            IT=IT+1             
            IF (IT.GT.NTIMMAX) THEN
             err=4
             RETURN               
            ENDIF               
            XJUMP(IT)= IPRETIM  
            XLAMB0(IT) = XFAIL / CUM_RISK           
            IF (NKAPLAN.EQ.1) THEN                  
             S0(IT) = 1.D0 - XLAMB0(IT)             
             IF (CUM_RISK.NE.XFAIL)                 
     +        VARS0(IT) = XFAIL / ((CUM_RISK - XFAIL)*CUM_RISK)         
            ENDIF               
            XFAIL = 0.D0        
       ENDIF
       ENDIF
C-----------------------------------------------------------------------
C   IF THE PREVIOUS RECORD HAS A DIFFERENT STRATUM ID ==> NEW STRATUM   
C   SET TO ZERO THE LAST (=TIME 0) CONTRIBUTION TO XLAMB0               
C   INITIALIZE CUM_RISK (CUMULATIVE RISK FOR ALL ANIMALS)               
C-----------------------------------------------------------------------
       IPRETIM =INFO(I,1)       
       ICODE = INFO(I,2)        
       IF (NSTRATA.GT.1) THEN   
       IF (INFO(I,3).NE.IPRESTR) THEN               
             IF (IPRESTR.NE.0) THEN                 
             IT = IT + 1        
             IF (IT.GT.NTIMMAX) THEN
                  err=4
                  RETURN          
             ENDIF              
             XJUMP(IT)= 0       
             XLAMB0(IT) = 0.D0  
             S0(IT) = 0.D0      
             VARS0(IT) = 0.D0   
             IRANGE(2,ISTR)= IT 
C           
             CUM_RISK = 0.D0    
             ISTR= ISTR + 1
             IRANGE(1,ISTR)= INFO(I,3)              
             ENDIF              
             IPRESTR=INFO(I,3)  
       ENDIF
       ENDIF
       IF (NKAPLAN.EQ.1) THEN   
       IF (ICODE.GE.0) CUM_RISK= CUM_RISK + 1.D0    
       IF (ICODE.EQ.-2) CUM_RISK= CUM_RISK - 1.D0   
       GOTO 189                 
       ENDIF
C-----------------------------------------------------------------------
C   U = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE               
C   UMINUS = SUM OF Z'*BETA FOR TIME-INDEPENDENT AFTER CHANGE           
C-----------------------------------------------------------------------
       U = 0.D0                 
       UMINUS = 0.D0            
C-----------------------------------------------------------------------
C        IPLUS1/2 = 0 FOR TIME-INDEPENDENT COVARIATES                   
C             ==> CONTRIBUTES TO U (AND TO UMINUS IF NOT LAST RECORD)   
C        IPLUS1/2 = +1 FOR TIME-DEPENDENT (BEFORE CHANGE)               
C             ==> CONTRIBUTES TO U EXCEPT IF LEFT TRUNCATED RECORD      
C        IPLUS1/2 = -1 FOR TIME-DEPENDENT (AFTER CHANGE)                
C             ==> CONTRIBUTES TO UMINUS IF NOT LAST RECORD              
C     FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES    
C     BETA = -999 = - INFINITY => EXP(Z'* BETA) WILL BE = 0             
C-----------------------------------------------------------------------
       K = 0
       DO 35 J=1,ndcov
       IF (IANAL(J).NE.0) THEN 
            XCOV = XDATA(I,J)   
            IF (IPLUS1(J).GT.0) THEN                
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             IF (ICODE.GE.-1) U = U + R             
            ELSE IF (IPLUS1(J).EQ.0) THEN           
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + R
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + R
             IF (ICODE.LT.0) UMINUS= UMINUS + R     
            ELSE IF (ICODE.LT.0) THEN               
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             UMINUS = UMINUS + R
            ENDIF               
       ENDIF
   35     CONTINUE              
       DO 36 J=1,ndcov
       JJ = J + nccov
       IF (IANAL(JJ).NE.0) THEN 
            K = IDATA(I,JJ)     
            IF (IPLUS2(J).GT.0) THEN                
             IF (ICODE.GE.-1) U = U + BETA(K)       
            ELSE IF (IPLUS2(J).EQ.0) THEN           
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c The following statement :
c            U = U + BETA(K)
c was corrected on October 16, 1996 for left-truncated records
c <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
             IF (ICODE.NE.-2) U = U + BETA(K)             
             IF (ICODE.LT.0) UMINUS = UMINUS + BETA(K)                  
            ELSE IF (ICODE.LT.0) THEN               
             UMINUS = UMINUS + BETA(K)              
            ENDIF               
       ENDIF
   36     CONTINUE              
C           
C-----------------------------------------------------------------------
C   COMPUTE W = EXP(U) AND WMINUS = EXP(UMINUS) AND UPDATE CUM_RISK     
C            DEPENDING ON CODE  
C-----------------------------------------------------------------------
       W = 0.D0                 
       WMINUS = 0.D0            
       IF (U.GT.-20.D0) W = DEXP(U)                 
       IF (ICODE.GE.0) THEN     
       CUM_RISK = CUM_RISK + W  
       ELSE IF (ICODE.EQ.-1) THEN                   
       IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)  
       CUM_RISK = CUM_RISK + W - WMINUS             
       ELSE IF (ICODE.LE.-2) THEN                   
       IF (UMINUS.GT.-20.D0) WMINUS = DEXP(UMINUS)  
       CUM_RISK = CUM_RISK - WMINUS                 
       ENDIF
C           
  189   IF (ICODE.GE.1) XFAIL = XFAIL + 1.D0        
C           
  190 CONTINUE                  
c      IF (NSTOR.NE.1) GOTO 100  
c  199 IF (NSTOR.NE.1) CLOSE(4)  
C           
C-----------------------------------------------------------------------
C   LAST "TRUE" CONTRIBUTION TO XLAMB0(T)           
C-----------------------------------------------------------------------
      IF ((CUM_RISK.GT.1.D-10).AND.(XFAIL.NE.0)) THEN                   
       IT=IT+1                  
       IF (IT.GT.NTIMMAX) THEN
       err=4
       RETURN 
       ENDIF
       XJUMP(IT)= IPRETIM       
       XLAMB0(IT) = XFAIL / CUM_RISK                
       IF (NKAPLAN.EQ.1) THEN   
       S0(IT) = 1.D0 - XLAMB0(IT)                   
       VARS0(IT) = XFAIL / ((CUM_RISK - XFAIL)*CUM_RISK)                
       ENDIF
      ENDIF 
C-----------------------------------------------------------------------
C   SET TO ZERO THE LAST (=TIME 0) CONTRIBUTION TO XLAMB0               
C-----------------------------------------------------------------------
      IT=IT+1                   
      IF (IT.GT.NTIMMAX) THEN
       err=4
       RETURN 
      ENDIF 
      XJUMP(IT)= 0              
      XLAMB0(IT) = 0.D0         
      S0(IT) = 0.D0             
      VARS0(IT) = 0.D0          
      IRANGE(2,ISTR) = IT       
C           
C-----------------------------------------------------------------------
C   CUMULATE THE CONTRIBUTIONS TO XLAMB0 AND COMPUTE S0(T)              
C   IF KAPLAN-MEIER (NKAPLAN=1) MULTIPLY CONTRIBUTIONS TO S0,           
C          AND COMPUTES NELSON'S ESTIMATE AND A 95% INTERVAL            
C          FOR S0(T) (KALBFLEISH AND PRENTICE P13-15)                   
C   PRINT IF NBASE IS NOT 0     
C-----------------------------------------------------------------------
      nn=0
      DO 250 I=1,ISTR
       K = IRANGE(2,I)          
       K2 = IRANGE(2,I-1)+1
       S0(K) = DEXP(-XLAMB0(K)) 
       N = 0
       DO 200 J=K-1,K2,-1       
       IF (XJUMP(J).LT.1.D-4) GOTO 200              
       N = N+1                  
       XLAMB0(J) = XLAMB0(J) + XLAMB0(J+1)          
       LOGT(J)= DLOG(XJUMP(J))  
       LOGLOGS0(J)= DLOG(XLAMB0(J))                 
       IF (NKAPLAN.NE.1) THEN   
             S0(J) = DEXP(-XLAMB0(J))               
             IF (NBASE.NE.0) THEN                   
             KK = INT(XJUMP(J)+0.05)
             km(n+nn,1)=kk
             km(n+nn,2)=xlamb0(j)
             km(n+nn,3)=s0(j)
             ENDIF              
       ELSE 
             IF (S0(J+1).LT.1.D-8) GOTO 200         
             KK = INT(XJUMP(J)+0.05)                
             S0(J) = S0(J) * S0(J+1)                
             IF (S0(J).GT.1.D-8) THEN               
             U = DLOG(S0(J))    
             VARS0(J) = VARS0(J) + VARS0(J+1)       
             R = DSQRT(VARS0(J)) / DABS(U)          
             LOGLOGS0(J)= DLOG(-U)                  
             W = DEXP(DEXP(-1.96D0* R)* U)          
             U = DEXP(DEXP(1.96D0* R)* U)           
             ELSE               
             U = 0.D0           
             W = 0.D0           
             ENDIF
             km(n+nn,1)=kk
             km(n+nn,2)=s0(j)
             km(n+nn,3)=vars0(j)
             km(n+nn,4)=xlamb0(j)
       ENDIF
  200     CONTINUE
       nn=nn+irange(2,i)-irange(2,i-1)
        
  250 CONTINUE       
      IF (NKAPLAN.EQ.1) RETURN  
C-----------------------------------------------------------------------
C   FUTURE RECORDS :                  
C-----------------------------------------------------------------------
      IF (NSURV.EQ.1) THEN
C-----------------------------------------------------------------------
C  RETURN IF NO INTEREST IN THE SURVIVOR CURVE OF PARTICULAR ANIMALS    
C-----------------------------------------------------------------------
        IF (NALL_TIM.EQ.0.AND.NQUANT.EQ.0.AND.NSTIME.EQ.0) RETURN         
C-----------------------------------------------------------------------
C   COMPUTATION OF (PART OF) THE SURVIVOR CURVE FOR ANIMALS IN FILE 3   
C-----------------------------------------------------------------------
      IANIPREV=-999             
      IT = 0
 300  continue
C
C-----------------------------------------------------------------------
C      FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
      nn=0
       DO 390 I=1,NRr2         
       IANI=INFO2(I,4)          
       IF (IANI.NE.IANIPREV) THEN                   
            NSTR = MAX(1,INFO2(I,3))             
            ILT = IRANGE(2,NSTR)                 
            IHT = IRANGE(2,NSTR-1)+1             
            CUM_RSKP = 0.D0     
            XL_PREV = 0.D0      
            ISUP = 0            
            IS = 1              
            IQ = 1              
            IF (IANIPREV.NE.-999) THEN
             DO 270 J = 1,IT
                nn=nn+1
                surv(nn,1)=IANIPREV
                surv(nn,2)=irest(j)
                surv(nn,3)=rest(j)
             REST(J)=0.D0       
  270         CONTINUE          
             DO 275 J = 1,NQUANT
                 nn=nn+1
                 surv(nn,1)=IANIPREV
                 surv(nn,2)=iresq(j)
                 surv(nn,3)=squant(j)
             IRESQ(J)=0         
  275         CONTINUE          
             IT = 0             
            ENDIF               
            IANIPREV = IANI     
       ENDIF
c       NEXTJUMP =INFO2(I,1)
       NEXTJUMP =MIN(INFO2(I,1),INT(XJUMP(IHT)+0.5))
       ICODE = INFO2(I,2)       
C-----------------------------------------------------------------------
C   U = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE               
C-----------------------------------------------------------------------
       U = 0.D0                 
C-----------------------------------------------------------------------
C        IPLUS1/2 = 0 FOR TIME-INDEPENDENT COVARIATES                   
C        IPLUS1/2 = +1 FOR TIME-DEPENDENT (BEFORE CHANGE)               
C             ==> CONTRIBUTES TO U                  
C        IPLUS1/2 = -1 FOR TIME-DEPENDENT (AFTER CHANGE)                
C             ==> NOT RELEVANT HERE                 
C     FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES    
C     BETA = -999 = - INFINITY => EXP(Z'* BETA) WILL BE = 0             
C-----------------------------------------------------------------------
       K = 0
       DO 305 J=1,nccov
       IF (IANAL(J).NE.0) THEN 
             XCOV = XDATA2(J,I) 
             IF (IPLUS1(J).GE.0) THEN               
             K = K+1            
             R = BETA(K) * XCOV 
             IF (BETA(K).LE.-999.D0) R = -999.9D0   
             U = U + R          
             ENDIF              
       ENDIF
  305     CONTINUE              
       DO 306 J=1,ndcov
            JJ = J + nccov
            IF (IANAL(JJ).NE.0) THEN                
             K = IDATA2(JJ,I)   
             IF (IPLUS2(J).GE.0) U = U + BETA(K)    
            ENDIF               
  306     CONTINUE              
C           
C-----------------------------------------------------------------------
C  COMPUTE W = EXP(U)           
C-----------------------------------------------------------------------
       W = 0.D0                 
       IF (U.GT.-20.D0) W = DEXP(U)                 
C           
C-----------------------------------------------------------------------
C  IF THERE ARE SPECIFIC TIMES IN VECTOR ISTIME FOR WHICH S(T) IS TO    
C        BE COMPUTED (I.E., IF NSTIME>0)            
C    ==> CHECK WHETHER THE FIRST ONE (ISTIME(IS)) IS LESS THAN NEXTJUMP 
C        COMPUTE  S(MIN( ISTIME(IS), NEXTJUMP))     
C-----------------------------------------------------------------------
  310     IF ((NSTIME.EQ.0).OR.(IS.GT.NSTIME)) GOTO 320                 
       IF (ISTIME(IS).LT.NEXTJUMP) THEN             
             XSTIME = DBLE(ISTIME(IS))              
             JST = 1            
C           
C-----------------------------------------------------------------------
C   XSTIME = NEXT ELEMENT IN VECTOR ISTIME          
C   SEARCH FOR XSTIME IN VECTOR XJUMP = SEARCH FOR THE INDEX IND FOR    
C   WHICH XJUMP(IND) IS AS LARGE AS POSSIBLE BUT < OR = TO XSTIME       
C   USE DICHOTOMOUS SEARCH FUNCTION IDICHO. IF IDICHO() = KLT = 0,      
C   XSTIME IS LARGER THAN THE LARGEST TIME FOR THIS STRATUM             
C    => XLAMB0(XSTIME) LARGER THAN OR EQUAL (INDICATED BY ISUP=1)       
C   TO XLAMB0(KLT) OTHERWISE XLAMB0(XSTIME) = XLAMB0(KLT)               
C-----------------------------------------------------------------------
             KLT = IDICHO(ILT,IHT,XSTIME)           
             IF (KLT.EQ.0) THEN 
             ISUP = 1           
             CUM_RISK = CUM_RSKP + W * (XLAMB0(IHT) - XL_PREV)          
             SX = DEXP( - CUM_RISK)                 
             ELSE               
             CUM_RISK = CUM_RSKP + W * (XLAMB0(KLT) - XL_PREV)          
             SX = DEXP( - CUM_RISK)                 
             ENDIF              
             goto 325           
       Endif
  320        XSTIME = DBLE(NEXTJUMP)                
             JST = 0            
             IF (ISTIME(IS).EQ.NEXTJUMP) JST=2      
C           
C-----------------------------------------------------------------------
C   AS BEFORE WITH XSTIME = NEXTJUMP                
C-----------------------------------------------------------------------
             KLT = IDICHO(ILT,IHT,XSTIME)           
             IF (KLT.EQ.0) THEN 
             ISUP = 1           
             CUM_RISK = CUM_RSKP + W * (XLAMB0(IHT) - XL_PREV)          
             SX = DEXP( - CUM_RISK)                 
             ELSE               
             CUM_RISK = CUM_RSKP + W * (XLAMB0(KLT) - XL_PREV)          
             SX = DEXP( - CUM_RISK)                 
             ENDIF              
C-----------------------------------------------------------------------
C  IF NQUANT>0 AND IQ <=NQUANT, THERE ARE QUANTILES TO BE COMPUTED      
C  CHECK WHETHER SX IS SMALLER THAN SQUANT(IQ)      
C  IF THE ANSWER IS YES, SEARCH FOR THE TIME XSTIME2<= XSTIME SUCH THAT 
C  S(XSTIME2-1) > SQUANT(IQ) AND S(XTIME2)<= SQUANT(IQ)                 
C-----------------------------------------------------------------------
  325     ILT2 = ILT            
       KLT2 = KLT               
       IMID = KLT2              
       IF ((NQUANT.EQ.0).OR.(IQ.GT.NQUANT)) GOTO 340
       IF (SX.LE.SQUANT(IQ)) THEN                   
             IDIF = ILT2- KLT2  
             K = INT(XJUMP(IMID)+0.5)               
             IF (IDIF.LE.1) THEN
             IRESQ(IQ) = K      
             IQ = IQ + 1        
             GOTO 325           
             ENDIF              
  330        IDIF = ILT2 - KLT2 
             IF (IDIF.LE.1) THEN
             IF (SX2.GT.SQUANT(IQ)) IMID = IMID-1   
             K = INT(XJUMP(IMID)+0.5)               
             IRESQ(IQ) = K      
             IQ = IQ + 1        
             GOTO 325           
             ELSE               
             IMID = ILT2 - IDIF/2                   
             CUM_RIS2 = CUM_RSKP + W * (XLAMB0(IMID) - XL_PREV)         
             SX2 = DEXP( - CUM_RIS2)                
             IF (SX2.LE.SQUANT(IQ)) THEN            
                  KLT2 = IMID   
                  GOTO 330      
             ELSE               
                  ILT2 = IMID   
                  GOTO 330      
             ENDIF              
             ENDIF              
       ELSE IF (ISUP.EQ.1) THEN 
             K = INT(XJUMP(IHT)+0.5)
             IRESQ(IQ) = K      
             IQ = NQUANT + 1    
       ENDIF
C           
  340     IF (JST.GE.1) THEN    
            IF (ISUP.EQ.1) THEN 
             K = INT(XJUMP(IHT)+0.5)
             IS = NSTIME        
            ENDIF               
            IT = IT + 1         
            IREST(IT) = ISTIME(IS)                  
            REST(IT) = SX       
            IS = IS + 1         
            ILT = KLT           
            IF (JST.EQ.2.AND.ISUP.NE.1) THEN        
             CUM_RSKP = CUM_RISK
             XL_PREV = XLAMB0(ILT)                  
             GOTO 390           
            ELSE                
             GOTO 310           
            ENDIF               
       ELSE 
            IF (NALL_TIM.EQ.1) THEN                 
             IF (ISUP.EQ.1) THEN
             K = INT(XJUMP(IHT)+0.5)
             ENDIF              
             IT = IT + 1        
             IREST(IT) = NEXTJUMP                   
             REST(IT) = SX      
            ENDIF               
            ILT = KLT           
            CUM_RSKP = CUM_RISK 
            XL_PREV = XLAMB0(ILT)                   
       ENDIF
C           
  390   CONTINUE
C
       DO 370 J = 1,IT
          nn=nn+1
          surv(nn,1)=IANI
          surv(nn,2)=irest(j)
          surv(nn,3)=rest(j)
  370   CONTINUE
       DO 375 J = 1,NQUANT
          nn=nn+1
          surv(nn,1)=IANI
          surv(nn,2)=iresq(j)
          surv(nn,3)=squant(j)
  375   CONTINUE
C-----------------------------------------------------------------------
C   RESIDUALS :                  
C-----------------------------------------------------------------------
      ELSE
C-----------------------------------------------------------------------
C   COMPUTATION OF GENERALIZED RESIDUALS FOR ANIMALS IN FILE 3   
C   READS BLOCK ON DISK IF NSTOR = 1                
C-----------------------------------------------------------------------
      IANIPREV=-999             
      IS = 0
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c added on October 9, 1997
      NRESI=0
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C-----------------------------------------------------------------------
C   FOR EACH ELEMENTARY RECORD I :                  
C-----------------------------------------------------------------------
       DO 590 I=1,NREC
         IANI=INFO2(I,4)
         IF (IANI.NE.IANIPREV) THEN
            NSTR = MAX(1,INFO2(I,3))
            ILT = IRANGE(2,NSTR)                 
            IHT = IRANGE(2,NSTR-1)+1
            IF (IANIPREV.NE.-999.AND.IS.NE.1) THEN
                XIC = DBLE(IC_PREV)
                XMART = XIC -CUM_RSKP
                XDEV = -2.D0 * (XMART +XIC * DLOG(XIC-XMART))
                XDEV=DSQRT(XDEV)
                IF (XMART.LT.0.D0) XDEV=-XDEV
                resid(i,1)=CUM_RSKP
                resid(i,2)=XMART
                resid(i,3)=XDEV
            ENDIF               
            CUM_RSKP = 0.D0     
            XL_PREV = 0.D0      
            ISUP = 0            
            IS = 0             
            IANIPREV = IANI    
         ELSE IF (IS.EQ.1) THEN
           GOTO 590 
         ENDIF
         ICODE = INFO2(I,2)
         IF (ICODE.EQ.-2) THEN
           IS=1
           GOTO 590
         ENDIF
         NEXTJUMP =INFO2(I,1)
C-----------------------------------------------------------------------
C   U = SUM OF Z'*BETA FOR TIME-INDEPENDENT BEFORE CHANGE               
C-----------------------------------------------------------------------
         U = 0.D0            
C-----------------------------------------------------------------------
C        IPLUS1/2 = 0 FOR TIME-INDEPENDENT COVARIATES                   
C        IPLUS1/2 = +1 FOR TIME-DEPENDENT (BEFORE CHANGE)               
C             ==> CONTRIBUTES TO U                  
C        IPLUS1/2 = -1 FOR TIME-DEPENDENT (AFTER CHANGE)                
C             ==> NOT RELEVANT HERE                 
C     FIRST, IR1 CONTINUOUS COVARIATES, THEN IR2 DISCRETE COVARIATES    
C     BETA = -999 = - INFINITY => EXP(Z'* BETA) WILL BE = 0             
C-----------------------------------------------------------------------
         K = 0                 
         DO 505 J=1,nccov
           IF (IANAL(J).NE.0) THEN                
             XCOV = XDATA2(J,I)
             IF (IPLUS1(J).GE.0) THEN
               K = K+1            
               R = BETA(K) * XCOV 
               IF (BETA(K).LE.-999.D0) R = -999.9D0   
               U = U + R          
             ENDIF              
           ENDIF             
  505    CONTINUE             
         DO 506 J=1,ndcov
            JJ = J + nccov
            IF (IANAL(JJ).NE.0) THEN                
              K = IDATA2(JJ,I)  
              IF (IPLUS2(J).GE.0) U = U + BETA(K)   
            ENDIF
  506    CONTINUE              
C           
C-----------------------------------------------------------------------
C  COMPUTE W = EXP(U)           
C-----------------------------------------------------------------------
         W = 0.D0            
         IF (U.GT.-20.D0) W = DEXP(U)            
C           
C-----------------------------------------------------------------------
C   COMPUTE CONTRIBUTION BETWEEN PREVIOUS TIME AND NEXTJUMP 
C-----------------------------------------------------------------------
         XSTIME = DBLE(NEXTJUMP)                
         IPRETIM = NEXTJUMP                
C           
         KLT = IDICHO(ILT,IHT,XSTIME)           
         IF (KLT.EQ.0) THEN 
           ISUP = 1           
           CUM_RISK = CUM_RSKP + W * (XLAMB0(IHT) - XL_PREV)          
         ELSE               
           CUM_RISK = CUM_RSKP + W * (XLAMB0(KLT) - XL_PREV)          
         ENDIF              
C-----------------------------------------------------------------------
         ILT = KLT           
         CUM_RSKP = CUM_RISK
         XL_PREV = XLAMB0(ILT)                  
         IC_PREV = ICODE                  
         NS_PREV = NSTR
C           
  590   CONTINUE
        IF (IANIPREV.NE.-999.AND.IS.NE.1) THEN
            XIC = DBLE(IC_PREV)
            XMART = XIC -CUM_RSKP
            XDEV = -2.D0 * (XMART +XIC * DLOG(XIC-XMART))
            XDEV=DSQRT(XDEV)
            IF (XMART.LT.0.D0) XDEV=-XDEV
            resid(nrec,1)=CUM_RSKP
            resid(nrec,2)=XMART
            resid(nrec,3)=XDEV
        ENDIF
      ENDIF 
C           
      RETURN
C***********************************************************************
      END   
      FUNCTION IDICHO(ILT,IHT,XSTIME)               
C***********************************************************************
C  IDICO         *                                    *  14 / 10 /1993 *
C***********************************************************************
C   DICHOTOMOUS SEARCH IN A LIST GIVEN THE INDICES OF THE LOWEST (ILT) *
C   AND THE LARGEST (IHT) ELEMENT OF THE LIST                          *
C   NOTE : HERE ILT > IHT BECAUSE XJUMP IS SORTED IN DESCENDING ORDER  *
C   SEARCH FOR XSTIME IN VECTOR XJUMP = SEARCH FOR THE INDEX IND FOR   *
C   WHICH XJUMP(IND) IS AS LARGE AS POSSIBLE BUT < OR = TO XSTIME      *
C***********************************************************************
c
	include 'parinclu.h'
c
	INTEGER IDICHO,ILT,IHT,KLT,KHT,KDIF,KMID
      real(8) XJUMP(NTIMMAX),XSTIME                  
C           
      COMMON/BL6/XJUMP          
C           
      IF (XJUMP(IHT).LT.XSTIME) THEN                
C-----------------------------------------------------------------------
C   XSTIME IS LARGER THAN THE LARGEST TIME FOR THIS STRATUM             
C    => S(XSTIME) SMALLER THAN OR EQUAL TO S0(IHT)  
C      (THIS IS INDICATED BY GIVING A VALUE OF 0 TO IDICHO              
C-----------------------------------------------------------------------
       IDICHO = 0               
       RETURN                   
      ELSE IF (XJUMP(ILT).EQ.XSTIME) THEN           
C-----------------------------------------------------------------------
C    => PARTICULAR CASES        
C-----------------------------------------------------------------------
       IDICHO = ILT             
       RETURN                   
      ELSE IF (XJUMP(IHT).EQ.XSTIME) THEN           
       IDICHO = IHT             
       RETURN                   
      ELSE  
C-----------------------------------------------------------------------
C   XSTIME IS BETWEEN XJUMP(ILT) (CURRENT TIME) AND XJUMP(IHT)          
C    => DIVIDE INTERVAL BY 2 AND CHECK FOR THE VALUE OF XJUMP()         
C-----------------------------------------------------------------------
       KLT = ILT                
       KHT = IHT                
    5   KDIF = KLT - KHT        
       IF (KDIF.LE.1) THEN      
C-----------------------------------------------------------------------
C   THERE IS NO OTHER JUMP BETWEEN KLT AND KHT      
C    =>  S0(XSTIME)  = S0(KLT)  
C-----------------------------------------------------------------------
       IDICHO = KLT             
       RETURN                   
       ELSE 
       KMID = KHT + KDIF/2      
C-----------------------------------------------------------------------
C   THREE POSSIBILITIES : XJUMP(KMID) = XSTIME ==> S0(XSTIME)= S0(KMID) 
C          XJUMP(KMID) < XSTIME ==> REPLACE KLT BY KMID                 
C          XJUMP(KMID) > XSTIME ==> REPLACE KHT BY KMID                 
C-----------------------------------------------------------------------
       IF (XJUMP(KMID).EQ.XSTIME) THEN              
             IDICHO = KMID      
             RETURN             
       ELSE IF (XJUMP(KMID).LT.XSTIME) THEN         
             KLT = KMID         
             GOTO 5             
       ELSE 
             KHT = KMID         
             GOTO 5             
       ENDIF
       ENDIF
      ENDIF 
C           
C***********************************************************************
      END   
