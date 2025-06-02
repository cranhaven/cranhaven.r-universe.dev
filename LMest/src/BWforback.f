      subroutine BWforback(TT, k, FFM, piv, P, lk, Pp1, Pp2)

      integer TT,k,u
      double precision piv(k),P(k,k),mm
      double precision Qf(k,TT),FFM(k,TT)
      double precision lk,norm,tol,pot
      double precision q(k),ff(k),q0(k)
      double precision Pp1(k,TT),Pp2(k,k,TT),qf0(k),qb(k),qb0(k)
      integer j,t

c Perform forward and backward recursions and Baum and Welch



c preliminaries
      tol = 0.1; pot = 100
      tol = tol**pot

c compute probabilities
c first time occasion
      ff = max(FFM(:,1),tol)
      q = piv*ff
      mm = sum(q)
      q = q/mm
      norm = log(mm)
      Qf(:,1) = q
c following occasions
      do t = 2,TT
          ff = max(FFM(:,t),tol)
          mm = 0
          q0 = q
          do j = 1,k
            q(j) = ff(j)*sum(q0*P(:,j))
          end do
          mm = sum(q)
          q = q/mm
          norm = norm+log(mm)
          Qf(:,t) = q
      end do

c compute log-likelihood
      lk = log(sum(q))+norm

c for the last time occasion
      qb = 1
      Pp1(:,TT) = Qf(:,TT)/sum(Qf(:,TT))
      ff = max(FFM(:,TT),tol)
      qf0 = Qf(:,TT-1)
      do u = 1,k
          Pp2(u,:,TT) = qf0(u)*P(u,:)*ff
      end do
      Pp2(:,:,TT) = Pp2(:,:,TT)/sum(Pp2(:,:,TT))

c iterate in backward order
      do t=TT-1,1,-1
c updatre backward vector
          qb0 = qb
      	  qb = 0
      	  do u = 1,k
            qb(u) = sum(qb0*ff*P(u,:))
          end do
          mm = sum(qb)
          qb = qb/mm

c compute first order posterior probabilities
          Pp1(:,t) = Qf(:,t)*qb
          Pp1(:,t) = Pp1(:,t)/sum(Pp1(:,t))

c compute second order probabilities
          if(t>1) then
            ff = max(FFM(:,t),tol)
            qf0 = Qf(:,t-1)
            do u = 1,k
            Pp2(u,:,t) = qf0(u)*P(u,:)*ff*qb
            end do
            Pp2(:,:,t) = Pp2(:,:,t)/sum(Pp2(:,:,t))
            end if


      end do

      end
