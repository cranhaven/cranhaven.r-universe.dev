c     Sequential strong screening

      subroutine seq_str_screen(x,y,n,p,lambda,lambda0,b0,act,nact)
      integer n,p
      integer nact,act(1:p)
      integer i,j
      double complex x(1:n,1:p),y(1:n),b0(1:p),tmp,r(1:n)
      double precision lambda,lambda0,gap
      nact = 0
      do 36 j=1,p
         act(j) = 0
 36   continue

      do 44 i=1,n
         tmp = 0
         do 42 j=1,p
            tmp = tmp + x(i,j) * b0(j)
 42      continue
         r(i) = y(i) - tmp
 44   continue
      
      gap = 2*lambda - lambda0

      do 59 j=1,p
         tmp = 0
         do 52 i=1,n
            tmp = tmp + conjg(x(i,j)) * r(i)
 52      continue
         tmp = tmp/n

         if (abs(tmp) .GE. gap) then
            nact = nact + 1
            act(nact) = j
         endif
 59   continue

      return
      end


c     Complex Lasso with warm start and active set screening

      subroutine classocd_warm_screen(x,y,n,p,lambda,lambda0,b0,b)
      integer n,p
      integer i,j,k,it,nact,act(1:p)
      double complex x(1:n, 1:p),xj(1:n),y(1:n),b0(1:p),b(1:p),bo(1:p)
      double complex r(1:n),rj(1:n)
      double precision lambda, lambda0
      double complex ro(1:n)
      double precision eo,en,ed
      double complex sxr

      call seq_str_screen(x,y,n,p,lambda,lambda0,b0,act,nact)

      do 81 j=1,p
         bo(j) = 0
 81   continue

      do 86 k=1,nact
         j=act(k)
         bo(j) = b0(j)
 86   continue

      eo = 0
      do 96 i = 1,n
         ro(i) = y(i)
         do 94 k=1,nact
            j = act(k)
            ro(i) = ro(i) - x(i,j) * bo(j)
 94      continue
         eo = eo + abs(ro(i)) * abs(ro(i))
 96   continue

      eo = eo/n

      do 150 it = 1,1000
         if(eo .GT. 0.001) then
            do 104 i = 1,n
               r(i) = ro(i)
 104        continue

            do 109 k=1,nact
               j=act(k)
               b(j) = bo(j)
 109        continue

            do 126 k = 1,nact
               j=act(k)
               do 116 i = 1,n
                  xj(i) = x(i,j)
                  rj(i) = r(i) + xj(i) * b(j)
 116           continue
               
               sxr = 0
               call soft_threshold(rj, xj, n, lambda, sxr)
               b(j) = sxr
               do 123 i = 1,n
                  r(i) = rj(i) - xj(i) * b(j)
 123           continue

               bo(j) = b(j)
 126        continue

            en = 0
            do 136 i = 1,n
               ro(i) = y(i)
               do 134 k=1,nact
                  j=act(k)
                  ro(i) = ro(i) - x(i,j) * bo(j)
 134           continue
               en = en + abs(ro(i)) * abs(ro(i))
 136        continue

            en = en/n
            ed = abs(en - eo)

            if(ed .LE. 0.0001) then
               exit
            else
               eo = en
            endif
            
         else
            exit
         endif
 150  continue

      return
      end
         
         

      

      
