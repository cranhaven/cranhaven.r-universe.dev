      subroutine itscale5(SXT,ngroups,ntraits,const,
     & prior,prob,entropy,niter,tol,denom) 
C Implements the Improved Iterative Scaling algorithm of
C Della Pietra et al. (1997). Inducing features of random
C fields. IEEE Transactions Pattern Analysis and Machine
C Intelligence 19:1-13.
C Author: Bill Shipley. Ported to R by Etienne Laliberte.
C SXT is a Groups (rows) X Traits (columns) matrix
C const is a vector of the constraint values (means, variances)
C prior is the prior distribution
C prob is the return vector of the maximum entropy
C entropy is the maximum entropy
C probabilities
C niter is the number of iterations required
C tol is the convergence tolerance value
C tolerance is mean square difference
C denom are final moments
      double precision SXT(ngroups,ntraits),const(ntraits)
      double precision prob(ngroups),prob2(ngroups),prior(ngroups)
      double precision gamma1(ntraits),total,test1,tol
      double precision Csums(ntraits),denom(ntraits),unstand(ngroups)
      double precision entropy
* 2023-10-28_fix this: 
* itscale5.f:71:13:
* 
*    71 |         diff=abs(prob2(i)-prob(i))
*       |             1
* Warning: Possible change of value in conversion from 
* REAL(8) to REAL(4) at (1) [-Wconversion]
      double precision diff
* END FIX
      integer niter
      if(ngroups.eq.0)then
       call rexit('Error in itscale5: number of states = 0')
      endif 
C SET INITIAL PROBS FROM PRIOR ...
      do i=1,ngroups
       prob(i)=prior(i)
       prob2(i)=prior(i)
      enddo
C sum each trait value over all species
      do i=1,ntraits
       Csums(i)=0.0
       do j=1,ngroups
        Csums(i)=Csums(i)+SXT(j,i)
       enddo
      enddo
      niter=0
C loop begins...
      test1=1.D10
101   if(test1.gt.tol) then
       niter=niter+1
       do i=1,ntraits
        denom(i)=0.
        gamma1(i)=0.
        do j=1,ngroups
         denom(i)=denom(i)+prob(j)*SXT(j,i)
        enddo
        if(denom(i).eq.0.or.const(i).eq.0.or.Csums(i).eq.0)then
         call rexit('Error in itscale5: NAs in gamma values')
        endif 
        gamma1(i)=log(const(i)/denom(i))/Csums(i)
       enddo
       total=0.0
       do i=1,ngroups
        unstand(i)=0.0
        do j=1,ntraits
         unstand(i)=unstand(i)+gamma1(j)*SXT(i,j)
        enddo
        unstand(i)=exp(unstand(i))*prob(i)
        total=total+unstand(i)
       enddo
       test1=0.0
       if(total.eq.0)then
        call rexit('Error in itscale5: NAs in prob')
       endif 
       test1=0.
       do i=1,ngroups
        prob2(i)=unstand(i)/total
        diff=abs(prob2(i)-prob(i))
C test1 is used to determine convergence.  If the greatest
C absolute difference between prob estimates in any state
C across iterations is less that the tolerance, then stop
        if(test1.lt.diff) then
         test1=diff
        endif
        prob(i)=prob2(i)
       enddo
c THE TEST CRITERION IS test1
       goto 101
      endif
C exit from loop and calculate maximum entropy
      entropy=0.0
      do i=1,ngroups
       if(prob(i).gt.0)entropy=entropy+prob(i)*log(prob(i))
      enddo
      entropy=-1*entropy
      return
      end
        
