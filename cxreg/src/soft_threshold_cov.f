c    Soft threshold function for covariance updated version     
      subroutine soft_threshold_cov(x, lambda)
      double precision lambda
      double complex x
      if ((abs(x) - lambda) .GT. 0) then
         x = (1 - (lambda / abs(x))) * x
      else
         x = 0
      endif
      return
      end


c     Sequential strong screening
      subroutine screen_cov(xx,xy,p,lambda,lambda0,b0,act,nact)
      integer p, nact, act(1:p)
      integer j,k
      double complex xx(1:p,1:p),xy(1:p),b0(1:p),tmp
      double precision lambda,lambda0,gap
      nact = 0
      do 22 j = 1,p
         act(j) = 0
 22   continue
      gap = 2*lambda - lambda0
      do 33 j = 1,p
         tmp = 0
         do 28 k = 1,p
            tmp = tmp + xx(j,k) * b0(k)
 28      continue
         if (abs(xy(j) - tmp) .GE. gap) then
            nact = nact + 1
            act(nact) = j
         endif
 33   continue
      return
      end
