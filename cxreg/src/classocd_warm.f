
c     Complex Lasso with warm start
      
      subroutine classocd_warm(x,y,n,p,lambda,b0,b)
      integer n,p
      integer i,j,k,it
      double complex x(1:n, 1:p),xj(1:n),y(1:n),b0(1:p),b(1:p),bo(1:p)
      double complex r(1:n),rj(1:n)
      double precision lambda
      double complex ro(1:n)
      double precision eo,en,ed
      double complex sxr
              
      do 41 j=1,p
         bo(j) = b0(j)
 41   continue

      eo = 0
      do 50 i = 1,n
         ro(i) = y(i)
         do 48 j = 1,p
            ro(i) = ro(i) - x(i,j) * bo(j)
 48      continue
         eo = eo + abs(ro(i)) * abs(ro(i))
 50   continue
      
      eo = eo/n

      do 101 it = 1,1000
         if(eo .GT. 0.001) then
            do 58 i = 1,n
               r(i) = ro(i)
 58         continue

            do 62 j = 1,p
               b(j) = bo(j)
 62         continue

            do 77 j = 1,p               
               do 68 i = 1,n
                  xj(i) = x(i,j)
                  rj(i) = r(i) + xj(i) * b(j)
 68            continue
               sxr = 0
               call soft_threshold(rj, xj, n, lambda, sxr)
               b(j) = sxr
               do 74 i = 1,n
                  r(i) = rj(i) - xj(i) * b(j)
 74            continue

               bo(j) = b(j)
 77         continue


            en = 0
            do 87 i = 1,n
               ro(i) = y(i)
               do 85 j = 1,p
                  ro(i) = ro(i) - x(i,j) * bo(j)
 85            continue
               en = en + abs(ro(i)) * abs(ro(i))
 87         continue

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
 101  continue
      
      return
      end



