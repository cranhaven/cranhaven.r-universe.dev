      subroutine back(TT,r,k,ns,l,S,Psi,Piv,PI,Phi,LL,U,V)

      integer t,TT,i,ns,c,k
      double precision Phi(ns,k,TT)
      double precision LL(ns,k,TT),PI(k,k,ns,TT)
      double precision U(k,k,ns,TT),V(ns,k,TT),pv(ns)
      double precision M(ns,k),M0(ns,k),pm(k)
      l = l
      piv = piv
      psi = psi
      r = r
      s = s

c compute marginal probabilities
      pv = 0
      do i=1,ns
        pv(i) = sum(LL(i,:,TT))
      end do
c compute backward probabilities
c last time occasion
      M = 1
      do i=1,ns
        V(i,:,TT) = LL(i,:,TT)/pv(i)
        pm = Phi(i,:,TT)
        do c=1,k
          U(c,:,i,TT) = LL(i,c,TT-1)*(PI(c,:,i,TT)*pm)/pv(i)
        end do
      end do
c for the previous occasions
      do t=TT-1,1,-1
        M0 = M
        do i=1,ns
          do c=1,k
            M(i,c) = sum(M0(i,:)*PI(c,:,i,t+1)*Phi(i,:,t+1))
          end do
          V(i,:,t) = LL(i,:,t)*M(i,:)/pv(i)
          if(t>1) then
            pm = Phi(i,:,t)*M(i,:)
            do c=1,k
             U(c,:,i,t) = LL(i,c,t-1)*(PI(c,:,i,t)*pm)/pv(i)
            end do
          end if
        end do
      end do

      end



