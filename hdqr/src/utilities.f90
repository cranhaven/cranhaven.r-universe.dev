
! ----------------------------------------------------------------------------
SUBROUTINE KKTcheck(nobs, ninv, r, ya, KKTtol, KKTvals)
  INTEGER :: nobs, j
  DOUBLE PRECISION :: ninv, r(nobs), ya(nobs), KKTtol, KKTvals, KKTval(nobs)

  KKTval = 0.0D0
  DO j = 1, nobs
    IF (r(j) < 1.0D0 - KKTtol) THEN
      KKTval(j) = Abs(ninv - ya(j))
    ELSEIF (r(j) > 1.0D0 + KKTtol) THEN
      KKTval(j) = Abs(ya(j))
    ELSE
      KKTval(j) = Abs(0.5D0 * ninv - ya(j))
    ENDIF
  ENDDO
  KKTvals = Sum(KKTval * KKTval)
END SUBROUTINE KKTcheck


! ----------------------------------------------------------------------------
SUBROUTINE KKTcheckloo(nobs, ninv, n, loor, ya, KKTtol, KKTvals)
  INTEGER :: nobs, n, j
  DOUBLE PRECISION :: ninv, loor(nobs), ya(nobs), KKTtol, KKTvals, KKTval(nobs)

  KKTval = 0.0D0
  DO j = 1, nobs
    IF (j .EQ. n) THEN
      KKTval(j) = Abs(ya(j))
    ELSEIF (loor(j) < 1.0D0 - KKTtol) THEN
      KKTval(j) = Abs(ninv - ya(j))
    ELSEIF (loor(j) > 1.0D0 + KKTtol) THEN
      KKTval(j) = Abs(ya(j))
    ELSE
      KKTval(j) = Abs(0.5D0 * ninv - ya(j))
    ENDIF
  ENDDO
  KKTvals = Sum(KKTval * KKTval)
END SUBROUTINE KKTcheckloo


! ----------------------------------------------------------------------------
SUBROUTINE KKTcheckloo2(nobs, ninv, n, loor, ya, KKTtol, KKTvals)
  INTEGER :: nobs, n, j
  DOUBLE PRECISION :: ninv, loor(nobs), ya(nobs), KKTtol, KKTvals, KKTval(nobs)

  KKTval = 0.0D0
  DO j = 1, nobs
    IF (j .EQ. n) THEN
      KKTval(j) = Abs(ya(j))
    ELSEIF (loor(j) < 1.0D0 - KKTtol) THEN
      KKTval(j) = Abs(ninv - ya(j))
    ELSEIF (loor(j) > 1.0D0 + KKTtol) THEN
      KKTval(j) = Abs(ya(j))
    ELSE
      KKTval(j) = Abs(0.5D0 * ninv - ya(j))
    ENDIF
  ENDDO
  KKTvals = Sum(KKTval * KKTval)
  CALL DBLEPR("KKTval", -1, KKTval, nobs)
END SUBROUTINE KKTcheckloo2


! --------------------------------------------------------------------------
! chkvars: An auxiliary function for variable check.
! --------------------------------------------------------------------------
!
! USAGE:
! 
! call chkvars (nobs, nvars, x, ju)
! 
! INPUT ARGUMENTS:
! 
!    nobs = number of observations
!    nvars = number of predictor variables
!    x(nobs, nvars) = matrix of predictors, of dimension N * p; each row is an observation vector.
!    y(no) = response variable. This argument should be a two-level factor {-1, 1} 
!            for classification.
!    
! OUTPUT:
!
!    ju(nvars) = flag of predictor variables
!                ju(j) = 0 => this predictor has zero variance
!                ju(j) = 1 => this predictor does not have zero variance
!
! --------------------------------------------------
SUBROUTINE chkvars (nobs, nvars, x, ju)
! --------------------------------------------------
      IMPLICIT NONE
    ! - - - arg types - - -
      INTEGER :: nobs
      INTEGER :: nvars
      INTEGER :: ju (nvars)
      DOUBLE PRECISION :: x (nobs, nvars)
    ! - - - local declarations - - -
      INTEGER :: i
      INTEGER :: j
      DOUBLE PRECISION :: t
! - - - begin - - -
      DO j = 1, nvars
         ju (j) = 0
         t = x (1, j)
         DO i = 2, nobs
            IF (x(i, j) /= t) THEN
               ju (j) = 1
               EXIT
            END IF
         END DO
      END DO
END SUBROUTINE chkvars

! --------------------------------------------------------------------------
! standard: An auxiliary function for standardize x matrix.
! --------------------------------------------------------------------------
!
! USAGE:
! 
! call standard (nobs,nvars,x,ju,isd,xmean,xnorm,maj)   
! 
! INPUT ARGUMENTS:
! 
!    nobs = number of observations
!    nvars = number of predictor variables
!    x(nobs, nvars) = matrix of predictors, of dimension N * p; each row is an observation vector.
!    ju(nvars) = flag of predictor variables
!                ju(j) = 0 => this predictor has zero variance
!                ju(j) = 1 => this predictor does not have zero variance
!    isd = standarization flag:
!          isd = 0 => do not standardize predictor variables
!          isd = 1 => standardize predictor variables
!          NOTE: no matter isd is 1 or 0, matrix x is always centered by column. That is, col.mean(x) = 0.
!    
! OUTPUT:
!
!    x(nobs, nvars) = standarized matrix x
!    xmean(nvars) = column mean of x matrix
!    xnorm(nvars) = column standard deviation of x matrix
!    maj(nvars) = column variance of x matrix
!
! --------------------------------------------------
SUBROUTINE standard(nobs,nvars,x,ju,isd,xmean,xnorm,maj)     
! --------------------------------------------------
    IMPLICIT NONE
    ! - - - arg types - - -
    INTEGER :: nobs
    INTEGER :: nvars
    INTEGER :: isd
    INTEGER :: ju(nvars)
    DOUBLE PRECISION :: x(nobs,nvars)
    DOUBLE PRECISION :: xmean(nvars)
    DOUBLE PRECISION :: xnorm(nvars)
    DOUBLE PRECISION :: maj(nvars)
    ! - - - local declarations - - -
    INTEGER:: j
! - - - begin - - -                                
    DO j = 1,nvars                                  
        IF (ju(j) == 1) THEN                         
            xmean(j) = sum(x(:,j)) / nobs     !mean                        
            x(:,j) = x(:,j) - xmean(j)    
            maj(j) = dot_product(x(:,j),x(:,j))/nobs                                              
              IF (isd == 1) THEN
                xnorm(j) = sqrt(maj(j))    !standard deviation               
                x(:,j) = x(:,j)/xnorm(j)
                maj(j) = 1.0D0
            ENDIF                                                        
        ENDIF                                     
    ENDDO                             
END SUBROUTINE standard
! imported from R source code
! --------------------------------------------------
SUBROUTINE pnorm(qval, pval)
! --------------------------------------------------
      IMPLICIT NONE

      INTEGER :: ii
      DOUBLE PRECISION :: qval, pval
      DOUBLE PRECISION :: y, xsq, xnum, xden, tmp, del
      DOUBLE PRECISION, PARAMETER :: eps = 1.0D-16
      DOUBLE PRECISION, PARAMETER :: a(5) = (/ &
        & 2.2352520354606839287, &
        & 161.02823106855587881, &
        & 1067.6894854603709582, &
        & 18154.981253343561249, &
        & 0.065682337918207449113 &
        & /)
      DOUBLE PRECISION, PARAMETER :: b(4) = (/ &
        & 47.20258190468824187, & 
        & 976.09855173777669322, & 
        & 10260.932208618978205, & 
        & 45507.789335026729956 & 
        & /)
      DOUBLE PRECISION, PARAMETER :: c(8) = (/ &
        & 0.39894151208813466764, &
        & 8.8831497943883759412, &
        & 93.506656132177855979, &
        & 597.27027639480026226, &
        & 2494.5375852903726711, &
        & 6848.1904505362823326, &
        & 11602.651437647350124, &
        & 9842.7148383839780218  &
        & /)
      DOUBLE PRECISION :: clast = 1.0765576773720192317D-8
      DOUBLE PRECISION, PARAMETER :: d(8) = (/ &
        & 22.266688044328115691, &
        & 235.38790178262499861, &
        & 1519.377599407554805, &
        & 6485.558298266760755, &
        & 18615.571640885098091, &
        & 34900.952721145977266, &
        & 38912.003286093271411, &
        & 19685.429676859990727 &
        & /)

      y = Abs(qval)
      del = 0.0
      IF (y .LE. 0.67448975) THEN
        IF (y > eps) THEN
          xsq = qval * qval
          xnum = a(5) * xsq
          xden = xsq;
          DO ii = 1, 3
            xnum = (xnum + a(ii)) * xsq
            xden = (xden + b(ii)) * xsq
          ENDDO
        ELSE
          xnum = 0.0
          xden = 0.0
        ENDIF
        tmp = qval * (xnum + a(4)) / (xden + b(4))
        pval = 0.5 + tmp
      ELSEIF (y .LE. 5.656854) THEN
        xnum = clast * y
        xden = y
        DO ii = 1, 7
          xnum = (xnum + c(ii)) * y
          xden = (xden + d(ii)) * y
        ENDDO
        tmp = (xnum + c(8)) / (xden + d(8))
        xsq = Aint(qval * 16) / 16
        del = (qval - xsq) * (qval + xsq)
        pval = Exp(-xsq * xsq * 0.5) * Exp(-del * 0.5) * tmp
        if (qval > 0.0) pval = 1.0 - pval
      ELSE
        IF (qval > 0.0) pval = 1.0
        IF (qval < 0.0) pval = 0.0
      ENDIF
END SUBROUTINE pnorm      

SUBROUTINE objfun(intcpt, bb, ab, ka, y, lam1, lam2, nobs, nvars, tau, objval) !change
  IMPLICIT NONE
  INTEGER :: nobs, j, nvars
  DOUBLE PRECISION :: intcpt, bb, ka (nobs), y (nobs), lam1, objval, lam2
  DOUBLE PRECISION :: fh (nobs), xi (nobs), xi_tmp, tau, del, ttau, ab
  xi = 0.0D0
  !check loss
  DO j = 1, nobs
    fh(j) = ka(j) + intcpt
    xi_tmp = y(j) - fh(j)
    ttau = tau-1.0D0
    IF (xi_tmp < 0.0D0) THEN
      xi_tmp = xi_tmp * ttau
    ELSE 
      xi_tmp = xi_tmp * tau
    END IF
    xi(j) = xi_tmp
  ENDDO
  objval = (lam2/2.0D0) * bb + Sum(xi)/(nobs) + lam1 * ab
END SUBROUTINE objfun

! This code is partially modified from fmim.c from in the source code of R
SUBROUTINE opt_int(lmin, lmax, nobs, nvars, ab, ka, bb, y, lam1, lam2, &
  & tau, objval, lhat)
  IMPLICIT NONE
  INTEGER :: nobs, nvars
  DOUBLE PRECISION :: lmin, lmax, lhat, objval, ka (nobs), bb, y (nobs), lam1, tau
  DOUBLE PRECISION :: a, b, d, e, p, q, r, u, v, w, x, del, ab, lam2
  DOUBLE PRECISION :: t2, fu, fv, fw, fx, xm, tol, tol1, tol3
  REAL(KIND = SELECTED_REAL_KIND(10, 99)) :: eps
  DOUBLE PRECISION, PARAMETER :: gold = (3.0D0 - Sqrt(5.0D0)) * 0.5D0

  eps = 3.14 !any double precision value
  eps = Epsilon(eps)
  tol = eps ** 0.25D0
  tol1 = eps + 1.0D0
  eps = Sqrt(eps)

  a = lmin
  b = lmax
  v = a + gold * (b - a)
  w = v
  x = v

  d = 0.0D0
  e = 0.0D0
  objval = 0.0D0
  CALL objfun(x, bb, ab, ka, y, lam1, lam2, nobs, nvars, tau, objval)
  fx = objval
  fv = fx
  fw = fx
  tol3 = tol / 3.0D0

  main_loop: DO
    xm = (a + b) * 0.5D0
    tol1 = eps * Abs(x) + tol3
    t2 = tol1 * 2.0D0

    IF (Abs(x - xm) .LE. t2 - (b - a) * 0.5D0) EXIT
    p = 0.0D0
    q = 0.0D0
    r = 0.0D0
    IF (Abs(e) > tol1) THEN
      r = (x - w) * (fx - fv)
      q = (x - v) * (fx - fw)
      p = (x - v) * q - (x - w) * r
      q = (q - r) * 2.0D0
      IF (q > 0.0D0) THEN
        p = -p
      ELSE
        q = -q
      ENDIF
      r = e
      e = d
    ENDIF

    IF ((Abs(p) .GE. Abs(q * 0.5D0 * r)) .OR. (p .LE. q * (a - x)) .OR. &
      &  (p .GE. q * (b - x))) THEN
      IF (x < xm) THEN
        e = b - x
      ELSE
        e = a - x
      ENDIF
      d = gold * e
    ELSE
      d = p / q
      u = x + d
      IF (u - a < t2 .OR. b - u < t2) THEN
        d = tol1
        IF (x .GE. xm) d = -d
      ENDIF
    ENDIF

    IF (Abs(d) .GE. tol1) THEN
      u = x + d
    ELSEIF (d > 0.0D0) THEN
      u = x + tol1
    ELSE
      u = x - tol1
    ENDIF

    objval = 0.0D0
    CALL objfun(u, bb, ab, ka, y, lam1, lam2, nobs, nvars, tau, objval)
    fu = objval

    IF (fu .LE. fx) THEN
      IF (u < x) THEN
        b = x
      ELSE
        a = x
      ENDIF
      v = w
      w = x
      x = u
      fv = fw
      fw = fx
      fx = fu
    ELSE
      IF (u < x) THEN
        a = u
      ELSE
        b = u
      ENDIF

      IF ((fu .LE. fw) .OR. (w .EQ. x)) THEN
        v = w
        fv = fw
        w = u
        fw = fu
      ELSEIF ((fu .LE. fv) .OR. (v .EQ. x) .OR. (v .EQ. w)) THEN
        v = u
        fv = fu
      ENDIF
    ENDIF

  ENDDO main_loop
  lhat = x
  objval = 0.0D0
  CALL objfun(x, bb, ab, ka, y, lam1, lam2, nobs, nvars, tau, objval)

END SUBROUTINE opt_int






