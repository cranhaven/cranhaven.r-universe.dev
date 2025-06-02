      subroutine prob_multilogif(Xdis,be,label,Pdis,P,k,ndis,ns,ncov)

      integer i,k,ncov,ndis,ns,label(ns)
      double precision Xdis(k,ncov,ndis),be(ncov),Pdis(ndis,k),P(ns,k)

      do i = 1,ndis
          do j = 1,k
          Pdis(i,j) = exp(sum(Xdis(j,:,i)*be))
          end do
          Pdis(i,:) = Pdis(i,:)/sum(Pdis(i,:))
      end do
      do i = 1,ns
          P(i,:) = Pdis(label(i),:)
      end do

      end
