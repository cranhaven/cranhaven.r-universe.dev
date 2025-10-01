c     Complex Lasso (Unscaled) with Cov Update
      subroutine classocd_cov_noscale(xx,xy,p,lambda,lambda0,
     + b0,b,maxiter,tol,screen)
      
      integer p
      integer i,j,k,it,nact,act(1:p),maxiter
      double complex xx(1:p,1:p),xy(1:p),b0(1:p),b(1:p),bo(1:p)
      double complex xr(1:p),v(1:p),xro(1:p),tmp
      double precision lambda, lambda0
      double precision eo,en,ed,tol
      logical screen

      if(screen) then
         call screen_cov(xx,xy,p,lambda,lambda0,b0,act,nact)
      else
         nact = p
         do 57 j=1,p
            act(j)=j
 57      continue
      endif
      
      
      
      do 64 j=1,p
         bo(j) = 0
 64   continue
      
      do 69 k=1,nact
         j=act(k)
         bo(j) = b0(j)
 69   continue

      eo = 0
      
      do 80 i = 1,p
         xro(i) = xy(i)
         do 78 k = 1, nact
            j = act(k)
            xro(i) = xro(i) - xx(i,j) * bo(j)
 78      continue
         eo = eo + abs(xro(i)) * abs(xro(i))
 80   continue

      
      eo = eo/p
      do 134 it = 1, maxiter
         if(eo .GT. tol) then
            do 88 i = 1,p
               xr(i) = xro(i)
 88         continue

            do 93 k = 1,nact
               j = act(k)
               b(j) = bo(j)
 93         continue

            do 108 k = 1,nact
               j = act(k)
               do 99 i = 1,p
                  v(i) = xr(i) + xx(i,j) * b(j)
 99            continue
               tmp = v(j)
               call soft_threshold_cov(tmp, lambda)
               b(j) = tmp / xx(j,j)
               do 105 i = 1,p
                  xr(i) = v(i) - xx(i,j) * b(j)
 105           continue

               bo(j) = b(j)
 108        continue

            en = 0

            do 119 i = 1,p
               xro(i) = xy(i)
               do 117 k = 1,nact
                  j = act(k)
                  xro(i) = xro(i) - xx(i,j) * bo(j)
 117           continue
               en = en + abs(xro(i)) * abs(xro(i))
 119        continue

            en = en/p
            ed = abs(en - eo)

            if(ed .LE. tol) then
               exit
            else
               eo = en
            endif

         else
            exit
         endif
         
 134  continue

      return
      end
         
   
