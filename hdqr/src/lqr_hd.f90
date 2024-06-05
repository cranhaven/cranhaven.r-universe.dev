! --------------------------------------------------------------------------
! dcsvm_unif.f90: the algorithm for the sparse SVM using uniform kernel convolution. 
! --------------------------------------------------------------------------
!
! USAGE:
! 
! call dcsvm_unif (alpha, lam2, hval, nobs, nvars, x, y, jd, pfncol, pf, & 
! & pf2, dfmax, pmax, nlam, flmin, ulam, eps, isd, maxit, istrong, nalam, & 
! & b0, beta, ibeta, nbeta, alam, npass, jerr, istrong) 
!
! INPUT ARGUMENTS:
! 
!    alpha = regularization parameter for the elastic net
!    lam2 = regularization parameter for the elastic net (no effect if alpha is not NULL)
!    hval = bandwidth parameter in the smoothing kernel
!    nobs = number of observations
!    nvars = number of predictor variables
!    x(nobs, nvars) = matrix of predictors, of dimension N * p; each row is an observation vector.
!    y(nobs) = response variable. This argument should be a two-level factor {-1, 1} 
!            for classification.
!    jd(jd(1)+1) = predictor variable deletion flag
!                  jd(1) = 0  => use all variables
!                  jd(1) != 0 => do not use variables jd(2)...jd(jd(1)+1)
!    pfncol = user controls whether to use same L1 weights for all lambda values
!             1 => same weights
!             nlambda => individual weights for each lambda
!    pf(nvars, pfncol) = relative L1 penalties for each predictor variable
!                pfncol == 1 => same weights for all the lambda
!                pfncol == nlambda => pf(j, l): weight of jth variable for lth lambda
!                pf(j, l) = 0 => jth variable unpenalized 
!    dfmax = limit the maximum number of variables in the model.
!            (one of the stopping criterion)
!    pmax = limit the maximum number of variables ever to be nonzero. 
!           For example once beta enters the model, no matter how many 
!           times it exits or re-enters model through the path, it will 
!           be counted only once. 
!    nlam = the number of lambda values
!    flmin = user control of lambda values (>=0)
!            flmin < 1.0 => minimum lambda = flmin*(largest lambda value)
!            flmin >= 1.0 => use supplied lambda values (see below)
!    ulam(nlam) = user supplied lambda values (ignored if flmin < 1.0)
!    eps = convergence threshold for coordinate majorization descent. 
!          Each inner coordinate majorization descent loop continues 
!          until the relative change in any coefficient is less than eps.
!    isd = standarization flag:
!          isd = 0 => regression on original predictor variables
!          isd = 1 => regression on standardized predictor variables
!          Note: output solutions always reference original
!                variables locations and scales.
!    maxit = maximum number of outer-loop iterations allowed at fixed lambda value. 
!            (suggested values, maxit = 100000)
!    istrong = whether to adopt the strong rule to accelerate the algorithm.
! 
! OUTPUT:
! 
!    nalam = actual number of lambda values (solutions)
!    b0(nalam) = intercept values for each solution
!    beta(pmax, nalam) = compressed coefficient values for each solution
!    ibeta(pmax) = pointers to compressed coefficients
!    nbeta(nalam) = number of compressed coefficients for each solution
!    alam(nalam) = lambda values corresponding to each solution
!    npass = actual number of passes over the data for all lambda values
!    jerr = error flag:
!           jerr  = 0 => no error
!           jerr > 0 => fatal error - no output returned
!                    jerr < 7777 => memory allocation error
!                    jerr = 7777 => all used predictors have zero variance
!                    jerr = 10000 => maxval(vp) <= 0.0
!           jerr < 0 => non fatal error - partial output:
!                    Solutions for larger lambdas (1:(k-1)) returned.
!                    jerr = -k => convergence for kth lambda value not reached
!                           after maxit (see above) iterations.
!                    jerr = -10000-k => number of non zero coefficients along path
!                           exceeds pmax (see above) at kth lambda value.
! 
! LICENSE: GNU GPL (version 2 or later)
! 
! AUTHORS:

 
     SUBROUTINE lqr_hd (alpha, lam2, hval, nobs, nvars, x, y, tau, jd, & 
      & pfncol, pf, pf2, dfmax, pmax, nlam, flmin, ulam, eps, isd, maxit, &
      & nalam, b0, beta, ibeta, nbeta, alam, npass, jerr, sigma, is_exact) 

         IMPLICIT NONE
!------- arg types ----------------------------------------------
         INTEGER :: nobs, nvars, nlam, nalam, maxit, dfmax, pmax, pfncol, nwdel, nhval(nlam)
         INTEGER :: isd, npass(nlam), jerr, jd (*), ibeta (pmax), nbeta(nlam), ndel, is_exact
         DOUBLE PRECISION :: alpha, lam2, hval, flmin, eps, tau
         DOUBLE PRECISION :: x (nobs, nvars), y (nobs), sigma
         DOUBLE PRECISION :: pf (nvars, pfncol), pf2 (nvars)
         DOUBLE PRECISION :: ulam (nlam), alam (nlam), projeps, hvaleps
         DOUBLE PRECISION :: beta (pmax, nlam), b0 (nlam), KKTeps, wdelta
         
!------- local declarations -------------------------------------
         INTEGER :: j, l, nk, ju (nvars)
         DOUBLE PRECISION :: xmean (nvars), xnorm (nvars), maj (nvars), mval
         
!------- preliminary step ---------------------------------------
         CALL chkvars (nobs, nvars, x, ju)
         IF (jd(1) > 0) ju(jd(2:(jd(1)+1))) = 0
         IF (Maxval (ju) <= 0) THEN
            jerr = 7777
            RETURN
         ENDIF
         IF (Maxval (pf) <= 0.0D0) THEN
            jerr = 10000
            RETURN
         ENDIF
         IF (Maxval (pf2) <= 0.0D0) THEN
            jerr = 10000
            RETURN
         ENDIF
         pf = Max (0.0D0, pf)
         pf2 = Max (0.0D0, pf2)
         
!------- first standardize the data -----------------------------
         CALL Standard (nobs, nvars, x, ju, isd, xmean, xnorm, maj)

         CALL lqr_path (alpha, lam2, hval, maj, mval, nobs, nvars, &
         & x, y, tau, ju, pfncol, pf, pf2, dfmax, pmax, nlam, flmin, ulam, eps, &
         & maxit, nalam, b0, beta, ibeta, nbeta, alam, npass, jerr, sigma, is_exact)
         IF (jerr > 0) RETURN  ! check error after calling function

!------- organize beta afterward --------------------------------
         DO l = 1, nalam
            nk = nbeta(l)
            IF (isd == 1) THEN
               DO j = 1, nk
                  beta(j, l) = beta(j, l) / xnorm(ibeta(j))
               ENDDO
            ENDIF
            b0(l) = b0(l) - Dot_product (beta(1:nk, l), &
           & xmean(ibeta(1:nk)))
         ENDDO
         RETURN
      END SUBROUTINE lqr_hd

      SUBROUTINE lqr_path (alpha, lam2, hval, maj, mval, nobs, nvars, &
      & x, y, tau, ju, pfncol, pf, pf2, dfmax, pmax, nlam, flmin, ulam, eps, &
      & maxit, nalam, b0, beta, m, nbeta, alam, npass, jerr, sigma, is_exact)
         
         ! use sorts
         IMPLICIT NONE
!------- arg types -----------------------------------------------
         DOUBLE PRECISION, PARAMETER :: BIG = 9.9E30
         DOUBLE PRECISION, PARAMETER :: MFL = 1.0E-6
         DOUBLE PRECISION, PARAMETER :: Pi = 3.141592654
         INTEGER, PARAMETER :: MNLAM = 6
         INTEGER :: nobs, nvars, dfmax, pmax, nlam
         INTEGER :: pfncol, maxit, nalam, npass(nlam), jerr
         INTEGER :: ju (nvars), m (pmax), nbeta (nlam), is_exact
         DOUBLE PRECISION :: alpha, lam2, hval, flmin, eps, tau
         DOUBLE PRECISION :: x (nobs, nvars), y (nobs), sigma
         DOUBLE PRECISION :: pf (nvars, pfncol), pf2 (nvars), ulam (nlam)
         DOUBLE PRECISION :: beta (pmax, nlam), b0 (nlam)
         DOUBLE PRECISION :: alam (nlam), maj (nvars), mval
!------- local declarations -------------------------------------
         DOUBLE PRECISION :: d, dif, oldb, u, v, al, alf, hinv
         DOUBLE PRECISION :: dl (nobs), r (nobs), onemh, oneph
         DOUBLE PRECISION :: b (0:nvars), oldbeta (0:nvars)
         INTEGER :: i, k, j, l, ctr, ni, me, mnl, mm (nvars), pfl
!------- local declarations for the projection -----------------
         ! INTEGER, PARAMETER :: hval_len 
         INTEGER :: jx, hval_id, tt, hval_len=4, mproj = 1, mp
         DOUBLE PRECISION :: ga (nvars), vl (nvars), al0, maj0(nvars)
         DOUBLE PRECISION :: ka(nobs), bb, obj1, obj0, b00, dif0, hvaleps
         INTEGER :: eset(nvars), sset(nobs), si, ss (nobs)
         DOUBLE PRECISION :: xb, xk2, mb, mb0, theta(nobs), delta
         DOUBLE PRECISION :: ytmp(nobs), quantile, d_kkt, dif_kkt, st
         DOUBLE PRECISION :: sx(nvars), uo, obj, ab, KKTeps, projeps
         DOUBLE PRECISION :: ap(nvars), plm(nvars)
!------- some initial setup ------------------------------------- 
         al = 0.0D0
         r = y
         b = 0.0D0
         oldbeta = 0.0D0
         m = 0
         mm = 0
         npass = 0
         ni = 0
         alf = 0.01D0
         ga = 0.0D0
         vl = 0.0D0
         maj0 = maj 
         xb = 0.0D0
         mb = 0.0D0
         mb0 = 0.0D0
         xk2 = 0.0D0
         delta = hval
         theta =0.0D0
         obj0 = 0.0D0
         obj1 = 0.0D0
         b00 = 0.0D0
         ka = 0.0D0
         bb = 0.0D0
         ab = 0.0D0
         KKTeps = 1e-3   
         ss = 0
         projeps = nobs * eps
         hvaleps = 1e-6
!---------- lambda loop -----------------------------------------
         mnl = Min (MNLAM, nlam)
         IF (flmin < 1.0D0) THEN
            flmin = Max (MFL, flmin)
            alf = flmin ** (1.0D0 / (nlam - 1.0D0))
         ENDIF
         !find the quantile 
         CALL objfun(b(0), bb, ab, ka, y, 0.0D0, 0.0D0, nobs, nvars, tau, obj0)
         CALL opt_int(-1.0D2, 1.0D2, nobs, nvars, ab, ka, bb, y, 0.0D0, 0.0D0, &
            & tau, obj1, b00)
         quantile = b00

         vl = 0.0D0
         CALL lqr_drv (nobs, nvars, x, y, tau, y-quantile, vl, 1/1.0D-9, -1.0D-9, 1.0D-9)
         ga = Abs (vl)

         IF (pfncol == 1) THEN 
            pfl = 1
            DO j = 1, nvars
               IF (ju(j) /= 0) THEN
                  IF (pf(j, pfl) > 0.0D0) THEN
                     al0 = Max (al0, ga(j) / pf(j, pfl))
                  ENDIF
               ENDIF
            ENDDO
         ENDIF 

         loop_lambda: DO l = 1, nlam
            IF (pfncol /= 1) THEN 
               pfl = l          
               al0 = 0.0D0
               DO j = 1, nvars
                  IF (ju(j) /= 0) THEN
                     IF (pf(j, pfl) > 0.0D0) THEN
                        al0 = Max (al0, ga(j) / pf(j, pfl))
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF 
            hval = delta
            hval_id = 0
         
            IF (flmin >= 1.0D0) THEN
               al = ulam(l)
            ELSE
               IF (l > 2) THEN
                  al = al * alf
               ELSE IF (l == 1) THEN
                  al = BIG
               ELSE IF (l == 2) THEN
                  al = al0 * alf
               ENDIF
            ENDIF
  
            IF (al > al0) THEN 
               b(1:nvars) = 0.0D0
               b(0) = quantile
               r = y - b(0)
               beta(:, l) = b(1:nvars)
               nbeta(l) = 0
               b0(l) = b(0)
               alam(l) = al
               nalam = l
               CYCLE
            ENDIF
            
            IF (alpha /= -1.0) THEN
               lam2 = al * (1 - alpha) * 0.5D0 / alpha
            ENDIF

            loop_hval: DO
               hval_id = hval_id + 1
               hinv = 1.0D0 / hval
               mval = hinv * 0.50D0
               maj = maj0 * mval
               onemh = - hval
               oneph = hval
                 
               oldbeta(0) = b(0)
               IF (ni > 0) oldbeta(m(1:ni)) = b(m(1:ni))
                  
   !---------- middle loop -----------------------------------------
               loop_middle: DO 
                  npass(l) = npass(l) + 1
                  dif = 0.0D0
                     
   !---------- 1. update beta_j first ( in middle loop) ------------
                  loop_middle_update_betaj: DO k = 1, nvars
                     IF (ju(k) /= 0) THEN
                        oldb = b(k)                
                        u = 0.0D0
                        plm(k) = pf2(k) * lam2 + maj(k)
                        ap(k) = al * pf(k, pfl)

                        DO i = 1, nobs   
                           IF (r(i) < onemh) THEN
                              dl(i) =  -(tau-1.0D0)
                           ELSEIF (r(i) > oneph) THEN
                              dl(i) = -tau
                           ELSE
                              dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                           ENDIF
                           u = u + dl(i) * x(i, k)
                        ENDDO 

                        u = maj(k) * b(k) - u / nobs 
                        v = Abs (u) - ap(k)
                    
                        IF (v > 0.0D0) THEN
                           b(k) = Sign (v, u) / plm(k)
                        ELSE
                           b(k) = 0.0D0
                        ENDIF

                        d = b(k) - oldb
                        IF (Abs (d) > 0.0D0) THEN
                           dif = Max (dif, d * d) 
                           r = r - x(:, k) * d
                           IF (mm(k) == 0) THEN
                              ni = ni + 1
                              IF (ni > pmax) EXIT
                              mm(k) = ni
                              m(ni) = k 
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO loop_middle_update_betaj
   !---------- 2. update intercept ( in middle loop) ---------------
                  IF (ni > pmax) EXIT  
                     d = 0.0D0
                     DO i = 1, nobs   
                        IF (r(i) < onemh) THEN
                           dl(i) =  -(tau-1.0D0)
                        ELSEIF (r(i) > oneph) THEN
                           dl(i) = -tau
                        ELSE
                           dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                        ENDIF
                        d = d + dl(i)
                     ENDDO 
                     d = - d / nobs /mval

                  IF (d /= 0.0D0) THEN                
                     b(0) = b(0) +  d
                     r = r - d
                     dif = Max (dif, d * d) !!!
                  ENDIF

                  IF (dif < eps) EXIT
                  IF (SUM(npass) > maxit) THEN
                     jerr = -1
                     RETURN
                  ENDIF
     
   !---------- inner loop ------------------------------------------
                  loop_inner: DO 
                     npass(l) = npass(l) + 1      
                     dif = 0.0D0
                        
   !---------- 1. update beta_j first ( in inner loop) -------------
                     DO j = 1, ni
                        k = m(j)
                        oldb = b(k)                         
                        u = 0.0D0
                        DO i = 1, nobs   
                           IF (r(i) < onemh) THEN
                              dl(i) =  -(tau-1.0D0)
                           ELSEIF (r(i) > oneph) THEN
                              dl(i) = -tau
                           ELSE
                              dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                           ENDIF
                           u = u + dl(i) * x(i, k)
                        ENDDO         
                        u = maj(k) * b(k) - u / nobs
                        v = Abs (u) - ap(k)
                          
                        IF (v > 0.0D0) THEN
                           b(k) = Sign (v, u) / plm(k)
                        ELSE
                           b(k) = 0.0D0
                        ENDIF
                              
                        d = b(k) - oldb
                        IF (Abs(d) > 0.0D0) THEN
                           dif = Max (dif, d * d) !!!
                           r = r - x(:, k) * d
                        ENDIF                       
                     ENDDO
                    
   !---------- 2. update intercept ( in inner loop) ----------------
                     d = 0.0D0
                     DO i = 1, nobs   
                        IF (r(i) < onemh) THEN
                           dl(i) =  -(tau-1.0D0)
                        ELSEIF (r(i) > oneph) THEN
                           dl(i) = -tau
                        ELSE
                           dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                        ENDIF
                        d = d + dl(i)
                     ENDDO 
              
                     d = - d / nobs/ mval
           
                     IF (d /= 0.0D0) THEN
                        b(0) = b(0) + d
                        r = r - d
                        dif = Max (dif, d * d) 
                     ENDIF
                       
                     IF (dif < eps) EXIT
                     IF (SUM(npass) > maxit) THEN
                        jerr = -1
                        RETURN
                     ENDIF
                  ENDDO loop_inner

               ENDDO loop_middle

               ab = SUM(abs(b(1:nvars)))
               ka = y-r-b(0)
               bb = Dot_product(b(1:nvars), b(1:nvars))
               CALL objfun(b(0), bb, ab, ka, y, al, lam2, nobs, nvars, tau, obj0)
               CALL opt_int(-1.0D2, 1.0D2, nobs, nvars, ab, ka, bb, y, al, lam2, &
                  & tau, obj1, b00)

               IF (obj1 < obj0) THEN
                 dif0 = b00 - b(0)
                 r = r - dif0
                 b(0) = b00
                 dif = Max (dif, dif0 * dif0)
               ENDIF

               IF (ni > pmax) EXIT
               dif = Maxval(Abs(oldbeta(m(1:ni)) - b(m(1:ni))))
               ! check KKT
               d_kkt = 0.0D0
               dif_kkt= 0.0D0
               CALL lqr_drv (nobs, nvars, x, y, tau, r, vl, 1/1.0D-12, -1.0D-12, 1.0D-12)
               DO j = 1, nobs
                  IF (Abs(b(j)) > 0) THEN
                     u = vl(j) + lam2 * pf2(j) * b(j)
                     v = Abs(u) - ap(j)
                     d_kkt = sign(v, u)
                  ELSE 
                     v = Abs(vl(j)) - ap(j)
                     IF(v > 0.0D0) d_kkt = v
                  ENDIF
                  IF (d_kkt /= 0.0D0) dif_kkt = Max(dif_kkt, d_kkt * d_kkt)
               ENDDO

               uo = Max(ulam(l), 1.0D0)
               IF (dif_kkt * dif_kkt/ uo / uo < KKTeps) THEN
                 IF (dif * dif < hvaleps) EXIT
               ENDIF 
               
               IF (is_exact .EQ. 0) THEN
                 EXIT
               ELSEIF (hval < 0.125**2) THEN
                  eset = 0
                  si = 0 
                  sset = 0
                  ss = 0
                  theta = 0.0D0
                  projection: DO mp = 1, mproj
                     DO i = 1, nobs
                       IF(Abs(r(i)) < hval) THEN
                           IF(ss(i) == 0) THEN
                              si = si + 1
                              sset(si) = i
                              ss(i) = 1
                           ENDIF
                       ENDIF
                     ENDDO
                     DO k = 1, nvars
                       IF (Abs(b(k)) < hval) THEN
                         eset(k) = 1
                       ENDIF
                     ENDDO
       
                     loop_proj_middle: DO 
                        npass(l) = npass(l) + 1
                        dif = 0.0D0                   
      !---------- 1. update beta_j first ( in middle loop) ------------
                        loop_proj_middle_update_betaj: DO k = 1, nvars

                           IF (ju(k) /= 0) THEN
                              oldb = b(k)
                              IF(eset(k) == 1) THEN 
                                 b(k) = 0.0D0
                              ELSE   
            
                                 u = 0.0D0

                                 DO i = 1, nobs   
                                    IF (r(i) < onemh) THEN
                                       dl(i) =  -(tau-1.0D0)
                                    ELSEIF (r(i) > oneph) THEN
                                       dl(i) = -tau
                                    ELSE
                                       dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                                    ENDIF
                                    u = u + dl(i) * x(i, k)
                                 ENDDO 

                                 mb = 0.0D0
                                 IF (si>0) THEN
                                    sx(k) = DOT_PRODUCT(x(sset(1:si),k),x(sset(1:si),k))
                                    mb = DOT_PRODUCT(x(sset(1:si),k),  theta(1:si) + &
                                       & sigma * r(sset(1:si))) + sigma * sx(k) * b(k)
                                 ENDIF
                                 u = mb + maj(k) * b(k) - u / nobs 
                                 v = Abs (u) - ap(k)
                             
                                 IF (v > 0.0D0) THEN
                                    IF(si > 0) THEN
                                       b(k) = Sign (v, u) / (plm(k) + &
                                          & sigma * sx(k)) 
                                    ELSE
                                       b(k) = Sign (v, u) / plm(k) 
                                    ENDIF
                                 ELSE
                                    b(k) = 0.0D0
                                 ENDIF
                            
                              ENDIF
                              d = b(k) - oldb
                              IF (Abs (d) > 0.0D0) THEN
                                 dif = Max (dif, d * d) 
                                 r = r - x(:, k) * d
                                 IF (mm(k) == 0) THEN
                                    ni = ni + 1
                                    IF (ni > pmax) EXIT
                                    mm(k) = ni
                                    m(ni) = k 
                                 ENDIF
                              ENDIF
                           ENDIF
                           
                        ENDDO loop_proj_middle_update_betaj

      !---------- 2. update intercept ( in middle loop) ---------------
                        IF (ni > pmax) EXIT
                           d = 0.0D0
                           DO i = 1, nobs   
                              IF (r(i) < onemh) THEN
                                 dl(i) =  -(tau-1.0D0)
                              ELSEIF (r(i) > oneph) THEN
                                 dl(i) = -tau
                              ELSE
                                 dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                              ENDIF
                              d = d + dl(i)
                           ENDDO 

                           d = - d / nobs 
                           IF (si > 0)  THEN 
                              d = d + SUM(theta(1:si) + sigma * r(sset(1:si))) 
                              d = d / (sigma * si + mval)
                           ELSE 
                              d = d/mval
                           ENDIF
                   
                        IF (d /= 0.0D0) THEN                
                           b(0) = b(0) +  d
                           r = r - d
                           dif = Max (dif, d * d) 
                        ENDIF

      !---------- 3. update theta ( in middle loop) ---------------
                        IF (si > 0) theta(1:si) = theta(1:si) + sigma * r(sset(1:si))
                        IF (dif < eps) EXIT
                        IF (SUM(npass) > maxit) THEN
                           jerr = -1
                           RETURN
                        ENDIF

      !---------- inner loop ------------------------------------------
                        loop_proj_inner: DO 
                           npass(l) = npass(l) + 1      
                           dif = 0.0D0
                           
      !---------- 1. update beta_j first ( in inner loop) -------------
                           DO j = 1, ni
                              k = m(j)
                              oldb = b(k)
                              IF(eset(k) == 1) THEN 
                                 b(k) = 0.0D0
                              ELSE
                                 u = 0.0D0
                                 DO i = 1, nobs   
                                    IF (r(i) < onemh) THEN
                                       dl(i) =  -(tau-1.0D0)
                                    ELSEIF (r(i) > oneph) THEN
                                       dl(i) = -tau
                                    ELSE
                                       dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                                    ENDIF
                                    u = u + dl(i) * x(i, k)
                                 ENDDO 
                                 mb = 0.0D0
                                 IF (si > 0) THEN
                                    mb = DOT_PRODUCT(x(sset(1:si),k), theta(1:si)+ &
                                       & sigma * r(sset(1:si))) + sigma * sx(k) * b(k)
                                 ENDIF              
                                 u = mb + maj(k) * b(k) - u / nobs
                                 v = Abs (u) - ap(k)
                                   
                                 IF (v > 0.0D0) THEN
                                    IF(si > 0) THEN
                                       b(k) = Sign (v, u) / (plm(k) + &
                                          & sigma * sx(k)) 
                                    ELSE
                                       b(k) = Sign (v, u) / plm(k) 
                                    ENDIF
                                 ELSE
                                    b(k) = 0.0D0
                                 ENDIF
                            
                              ENDIF
                              d = b(k) - oldb
                              IF (Abs(d) > 0.0D0) THEN
                                 dif = Max (dif, d * d) 
                                 r = r - x(:, k) * d
                              ENDIF                       
                           ENDDO
                          
      !---------- 2. update intercept ( in inner loop) ----------------
                           d = 0.0D0
                           DO i = 1, nobs   
                              IF (r(i) < onemh) THEN
                                 dl(i) =  -(tau-1.0D0)
                              ELSEIF (r(i) > oneph) THEN
                                 dl(i) = -tau
                              ELSE
                                 dl(i) = -r(i) * 0.5D0 * hinv - tau + 0.50D0
                              ENDIF
                              d = d + dl(i)
                           ENDDO 
                    
                           d = - d / nobs
                           IF (si > 0) THEN 
                              d = d + SUM(theta(1:si) + sigma * r(sset(1:si))) 
                              d = d / (sigma * si + mval) 
                           ELSE
                              d = d / mval
                           ENDIF

                           IF (d /= 0.0D0) THEN                
                              b(0) = b(0) +  d
                              r = r - d
                              dif = Max (dif, d * d) 
                           ENDIF
                      
           !---------- 3. update theta ( in inner loop) ----------------
                           IF (si > 0) theta(1:si) = theta(1:si) + sigma * r(sset(1:si))
                        
                           IF (dif < projeps) EXIT
                           IF (SUM(npass) > maxit) THEN
                              jerr = -1
                              RETURN
                           ENDIF
                        ENDDO loop_proj_inner
                     ENDDO loop_proj_middle                  
                  ENDDO projection
               ENDIF 
              
               IF(hval_id .EQ. hval_len) EXIT
               hval = 0.125D0 * hval
            ENDDO loop_hval
            ! CALL INTPR("l", -1, l, 1)
            ! CALL INTPR("h_id", -1, hval_id, 1)
!---------- final update variable save results ------------------
            IF (ni > pmax) THEN
               jerr = - 10000 - l
               EXIT
            ENDIF
            IF (ni > 0) beta(1:ni, l) = b(m(1:ni))
            nbeta(l) = ni
            b0(l) = b(0)
            alam(l) = al
            nalam = l
            IF (l < mnl) CYCLE
            IF (flmin >= 1.0D0) CYCLE
            me = Count (beta(1:ni, l) /= 0.0D0)
            IF (me > dfmax) EXIT
         ENDDO loop_lambda
         RETURN
      END SUBROUTINE lqr_path
      
      SUBROUTINE lqr_drv (nobs, nvars, x, y, tau, r, vl, hinv, onemh, oneph)
         IMPLICIT NONE
         INTEGER :: nobs, nvars, i
         DOUBLE PRECISION :: x (nobs, nvars), y (nobs), tau
         DOUBLE PRECISION :: ninv, hinv, onemh, oneph
         DOUBLE PRECISION :: r (nobs), vl (nvars), dl (nobs), dly (nobs)
         vl = 0.0D0
         dl = 0.0D0
         ninv = 1.0D0 / nobs
         DO i = 1, nobs
            IF (r(i) < onemh) THEN
               dl(i) =  -(tau-1.0D0)
            ELSEIF (r(i) > oneph) THEN
               dl(i) = -tau
            ELSE
               dl(i) = - tau + 0.50D0
            ENDIF
         ENDDO
         DO i = 1, nvars
            vl(i) = Dot_product(dl, x(1:nobs, i)) * ninv
         ENDDO
      END SUBROUTINE lqr_drv