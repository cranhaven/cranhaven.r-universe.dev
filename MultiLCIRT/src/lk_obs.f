      subroutine lk_obs(jj,k,ns,S,l,Phi,Psi)

      integer j,jj,c,k,ns,S(ns,k)
      double precision Phi(l,jj,k),Psi(ns,k)
      
      do j = 1,jj
        do c = 1,k
          Psi(:,c) = Psi(:,c)*Phi(S(:,j)+1,j,c)
        end do
      end do

      end 



