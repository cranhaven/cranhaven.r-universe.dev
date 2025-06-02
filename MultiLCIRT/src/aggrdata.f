      subroutine aggrdata(data0,r,c,ndis,datadis,freq,label)
      
c version that include the vector B for particular states (0,1,2)
c in input also the density function FFM

c declare arguments in input
      integer h,i,j,r,c,ndis,freq(r),label(r)
      double precision data0(r,c),datadis(r,c),uf

c check each row
      ndis = 0
      label = 0
      do h = 1,r
        if(label(h) == 0) then
          ndis = ndis+1
          datadis(ndis,:) = data0(h,:)
          label(h) = ndis
          freq(ndis) = 1
          if(h<r) then
            do i = (h+1),r
              uf = sum(abs(datadis(ndis,:)-data0(i,:)))
      	      if(uf==0) then
      	        label(i) = ndis
      	        freq(ndis) = freq(ndis)+1
      	      end if
            end do
          end if
        end if
      end do

      end
