c     cglasso without covariate scaling/with covariance adapted penalty
      subroutine cglassocd_noscale(s,p,lambda,theta,
     + w,w0,w_init,maxiter,tol,h,final_cycle)
      
      integer p,maxiter,h
      integer i,j,k,it,in,jn
      double precision lambda,lambda0,tol
      double complex tmp
      double complex s(1:p,1:p)
      double complex w(1:p,1:p),wo(1:p,1:p),w0(1:p,1:p)
      double complex sc(1:(p-1)),s12(1:(p-1)),w11(1:(p-1),1:(p-1))
      double complex w11_sc(1:(p-1),1:(p-1)), s12_sc(1:(p-1))
      double complex theta(1:p,1:p)
      double precision eo, e
      double complex b(1:(p-1)), b_mat(1:(p-1),1:p),b_init(1:(p-1))
      logical w_init,final_cycle,screen
      external classocd_cov

      screen = .FALSE.
      

      if(.NOT. w_init) then
         do 34 i=1,p
            do 33 j=1,p
               wo(i,j)=s(i,j)
               w(i,j)=s(i,j)
 33         continue
 34      continue
      else
         do 41 i=1,p
            do 40 j=1,p
               wo(i,j)=w0(i,j)
               w(i,j)=w0(i,j)
 40         continue
 41      continue
      endif

      do 48 i=1,p
         do 47 j=1,p
            theta(i,j)=cmplx(0,0)
 47      continue
 48   continue
      
      eo = 10000.0
      final_cycle = .FALSE.

      do 59 i=1,(p-1)
         do 58 j=1,p
            b_mat(i,j)=cmplx(0,0)
 58      continue
 59   continue
      
      do 164 it = 1,maxiter
         
         do 140 k=1,p
            in=1
            do 76 i=1,p
               if(i .EQ. k) then
                  go to 76
               endif
               s12(in)=s(i,k)
               jn=1
               do 75 j=1,p
                  if(j .EQ. k) then
                     go to 75
                  endif
                  w11(in,jn)=wo(i,j)
                  jn=jn+1
 75            continue
               in=in+1
 76         continue

c     Set a dummy lambda0 since active set screening is off
            
            lambda0 = 1
            do 93 j=1,(p-1)
               b_init(j) = b_mat(j,k)
               b(j) = cmplx(0,0)
 93         continue
            call classocd_cov_noscale(w11,s12,p-1,lambda,lambda0,
     +       b_init,b,maxiter,tol,screen)

            do 101 j=1,(p-1)
               b_mat(j,k) = b(j)
 101        continue

            in = 1
            do 115 i=1,p
               if(i .EQ. k) then
                  go to 115
               endif
               w(i,k) = cmplx(0,0)
               do 112 j=1,(p-1)
                  w(i,k) = w(i,k) + w11(in,j)*b(j)
 112           continue
               w(k,i) = conjg(w(i,k))
               in=in+1
 115        continue

            if(final_cycle) then
               h=1
               tmp = cmplx(0,0)
               in=1
               do 126 i=1,p
                  if(i .EQ. k) then
                     go to 126
                  endif
                  tmp = tmp + w(k,i) * b(in)
                  in=in+1
 126           continue

               theta(k,k) = cmplx(1,0) / (w(k,k)-tmp)

               in=1
               do 138 i=1,p
                  if(i .EQ. k) then
                     go to 138
                  endif
                  theta(i,k) = -theta(k,k) * b(in)
                  theta(k,i) = conjg(theta(i,k))
                  in=in+1
 138           continue
            endif
 140     continue

         e = 0.0

         do 147 i=1,p
            do 146 j=1,p
               e=e+(abs(w(i,j)-wo(i,j)))**2
 146        continue
 147     continue
         e = sqrt(e)
         if(final_cycle) then
            go to 166
         endif

         if(abs(e-eo)<tol) then
            final_cycle = .TRUE.
         else
            do 160 i=1,p
               do 159 j=1,p
                  wo(i,j)=w(i,j)
 159           continue
 160        continue
            eo=e
         endif

 164  continue

 166  continue

      return
      end
            
                     
