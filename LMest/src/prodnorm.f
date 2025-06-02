      subroutine prodnorm(A,B,C,n,k,D)

c declare arguments in input
      integer n,k,i
      double precision A(n,k),B(n,k),C(k,k),D(k,k),Tmp(k,k)

c check each row
      do i = 1,n
        Tmp = spread(A(i,:),dim=2,ncopies=k)
        Tmp = Tmp*spread(B(i,:),dim=1,ncopies=k)
        Tmp = Tmp*C
        D = D+Tmp/sum(Tmp)
      end do

      end
