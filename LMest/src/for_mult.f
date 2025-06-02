      subroutine for_mult(TT,r,k,ns,l,S,Psi,Piv,PI,Phi,LL)

      integer t,TT,j,r,i,ns,l,c,k,S(ns,TT,r)
      double precision Psi(l,k,r),Phi(ns,k,TT)
      double precision Piv(ns,k),LL(ns,k,TT)
      double precision PI(k,k,ns,TT)

c compute response probabilities
      Phi = 1
      do t = 1,TT
      do j = 1,r
      Phi(:,:,t) = Phi(:,:,t)*Psi(S(:,t,j)+1,:,j)
      end do
      end do
c compute forward probabilities
      LL = 0
      LL(:,:,1) = Phi(:,:,1)*Piv
      do t=2,TT
      do i=1,ns
      do c=1,k
      LL(i,c,t) = sum(LL(i,:,t-1)*PI(:,c,i,t))
      end do
      end do
       LL(:,:,t) = LL(:,:,t)*Phi(:,:,t)
      end do

      end



