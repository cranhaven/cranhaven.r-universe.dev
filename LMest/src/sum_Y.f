      subroutine sum_Y(Ydis,Y,label,ndis,ns,k)

      integer i,ndis,ns,k,label(ns),j
      double precision Ydis(ndis,k),Y(ns,k)

      do i = 1,ns
      j = label(i)
       Ydis(j,:) = Ydis(j,:)+Y(i,:)
      end do
      end
