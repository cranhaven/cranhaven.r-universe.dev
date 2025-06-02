      subroutine prodnormw(A,B,C,n,k,D,yv)

c declare arguments in input
      integer n,k,i
      double precision A(n,k),B(n,k),C(k,k),D(k,k),Tmp(k,k),yv(n)

c check each row
      do i = 1,n
        Tmp = spread(A(i,:),dim=2,ncopies=k)
        Tmp = Tmp*spread(B(i,:),dim=1,ncopies=k)
        Tmp = Tmp*C
c        Tmp2 = Tmp/sum(Tmp)
c        Tmp3 = Tmp2*yv(i)
        D = D+Tmp/sum(Tmp)*yv(i)
      end do

      end
