FUNCTION kprimecdf( x, q, r, a1, TOL, MAXITER, ier )
    !-----------------------------------------------------------------------
    !     Poitevineau, J. and Lecoutre, B. (2010) Statistical distributions for bayesian 
    !         experimental data analysis fortran functions 1. continuous distributions,
    !         url = https://eris62.eu
    !
    !     Calculates the probability that a random variable distributed
    !     according to the K' distribution with Q and R degrees of
    !     freedom, A1 centrality parameter, is less than or equal to X
    !
    !     X     - Input . Value of the variable                   - Real
    !     Q     - Input . First degrees of freedom       (Q >  0) - Real
    !     R     - Input . Second   "    "     "          (R >  0) - Real
    !     A1    - Input . Eccentricity parameter                  - Real
    !     TOL   - Input . Maximum absolute error required on      - Real
    !                     kprimecdf (stopping criteria)
    !                     (eps < TOL < 1 where eps is machine
    !                     epsilon; see parameter statement below)
    !     MAXITER - Input . Maximum number of iterations            - Integer
    !     IER     - Output. Return code :                           - Integer
    !                     0 = normal
    !                    -1 = no more evolution of the sum but
    !                         required accuracy not reached yet
    !                         (then kprimecdf = value at last iteration)
    !                     1 = invalid input argument
    !                         (then kprimecdf = zero)
    !                     2 = maximum number of iterations reached
    !                         (then kprimecdf = value at last iteration)
    !                     3 = cannot be computed
    !                         (then kprimecdf = zero)
    !                     4 = error in auxiliary function
    !                         (betacdf, lprimecdf or tcdf)
    !                     5 = result out of limits (i.e. <0 or >1)
    !                     7 = 2 + 5 both codes apply
    !
    !     External functions called:
    !       BETACDF  LPRIMECDF  TCDF
    !       DLGAMA (Log(Gamma(.)) if not in FORTRAN library
    !
    !     Fortran functions called:
    !       ABS  EXP  LOG  MAX  MOD  INT  (and DLGAMA if available)
    !
    !     Uses "method 2", see Benton & Krishnamoorthy (2003),
    !     Computational Statistics & Data Analysis, 43, 249-267.
    !     N.B. The mode is taken as the starting point for iterations
    !     (forward and backward).
    !     Starting index is modified if worthwile (see parameter betaratio)
    !     To deal with the case where k=0 and r<=1, xgamf is updated at the
    !     end of the iteration loop rather than at the beginning.
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER        :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: kprimecdf

   !  Arguments
   REAL(PR), INTENT(in)     :: x, q, r, a1, TOL
   INTEGER, INTENT(in)      :: MAXITER
   INTEGER, INTENT(out)     :: ier

   !  local declarations
   REAL(PR), EXTERNAL :: betacdf, lprimecdf, tcdf

   REAL(PR), PARAMETER :: zero =0.0D0, half=0.5D0, one=1.0D0, two=2.0D0
   REAL(PR), PARAMETER :: dlg15=-0.120782237635245204D0  ! =log(gamma(1.5))
   REAL(PR), PARAMETER :: bel  =1.0D6, qlimit=2.0D6, rlimit=2.0D6
   REAL(PR), PARAMETER :: eps  =0.223D-15, explower=-706.893D0
   REAL(PR), PARAMETER :: betaratio=0.01D0
   INTEGER, PARAMETER  :: kmin=10    ! When starting point of iterations is below
                                     ! this limit "method 2" is not judged worthwile

   !     These constants are machine dependent:
   !     eps = machine epsilon
   !           (the smallest real such that 1.0 + eps > 1.0)
   !     explower = minimum valid argument for the exponential function
   REAL(PR) :: a2, aqa, aqal, beta0, betak, dj2, q2, q2l, qqal
   REAL(PR) :: r2, r2l, r2l1mx, sumg, xarg, xl, xx
   INTEGER :: iok, j, jm, k, kit
   LOGICAL :: xneg
   REAL(PR) :: betab(0:1), betaf(0:1), gcoefb(0:1), gcoeff(0:1)
   REAL(PR) :: cdf(0:1), xgamb(0:1), xgamf(0:1)
   REAL(PR) :: prv(0:1)

   !-----------------------------------------------------------------
   kprimecdf = zero
   ier = 0

   !  Test for valid input arguments
   if ( q <= zero .or. r <= zero .or. TOL >= one .or. TOL <= eps ) then
      ier = 1
      return
   end if

   !  Case x = 0
   if ( abs(x) < eps ) then
      cdf(0) = zero
      cdf(1) = zero
      xneg = .false.
      goto 10
   end if

   !  If a1 is close to zero use approximation (exact if a1 = 0)
   if ( abs(a1) < eps ) then
      kprimecdf = tcdf( x, r, bel, ier )
      if ( ier /= 0 ) ier = 4
      return
   end if

   !  If q or r is large enough use limiting distribution
   if ( q > qlimit ) then
      kprimecdf = one - lprimecdf( a1, r, x, TOL, MAXITER, ier ) ! noncentral t
      if ( ier /= 0 ) ier = 4
      return
   else if ( r > rlimit ) then
      kprimecdf = lprimecdf( x, q, a1, TOL, MAXITER, ier )
      if ( ier /= 0 ) ier = 4
      return
   end if

   !  Define usefull parameters
   xx = x
   xarg = x*x
   xarg = xarg / (xarg+r)
   q2 = q*half
   r2 = r*half
   a2 = a1*a1

   !  Case xarg close to one (x tends to +/- infinity)
   if ( abs(xarg-one) < eps+eps ) then
      if ( x > zero ) kprimecdf = one
      return
   end if

   !  Case a1 < 0 : change sign of x :
   !  Pr( K'(a1,1) < x ) = 1 - Pr( K'(-a1,1) < -x )
   if ( a1 < zero ) xx = -xx
   xneg = xx < zero          ! When a1 and x are not of same sign, the series is alternate
                             ! and this is flagged by xneg

   !  General case (iterations)
   !  When a1 > 0:
   !     1) if xx > 0:
   !        Pr( K'<xx ) =  Pr( K'<0 ) + Pr( 0<K'<xx )
   !     2) if xx < 0:
   !        Pr( K'<xx ) =  Pr( K'<0 ) - Pr( xx<K'<0 )
   !  First, calculate  Pr( Min(0,xx)<K'<Max(0,xx) )
   aqa = a2/(q+a2)
   aqal = log(aqa)
   qqal = q2*log(one-aqa)
   q2l = log_gamma(q2)
   r2l = log_gamma(r2)
   r2l1mx = r2*log(one-xarg)
   xl = log(xarg)
   k = max( 0, int( a2-(a2+a2)/q ) )   ! Mode of neg. bin. distribution
   if ( k < kmin ) k = 0               ! k is not large enough to be worthwile
   k = (k/2)*2                         ! Make k an even number

   ! To deal with the case where k=0 and r<=1, xgamf is updated at
   ! end of iteration loop, while xgamb is updated at beginning of loop
   dj2 = k*half
   betaf(0) = betacdf( xarg, dj2+half, r2, ier )
   if ( ier /= 0 ) then
      ier = 4
      return
   end if
   if ( k > 0 ) then   ! Remember k is even
      beta0 = betacdf( xarg, half, r2, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      if ( betaf(0) < beta0*betaratio ) then ! Beta at k is small, so k is moved to
         k = INT(xarg*k)                     ! somewhere between modes of g_j's and H_j's
         k = (k/2)*2                         ! Make k an even number
         dj2 = k*half
         betaf(0) = betacdf( xarg, dj2+half, r2, ier )
         if ( ier /= 0 ) then
            ier = 4
            return
         end if
      end if
      if ( k > 0 ) then
         gcoeff(0) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
         if ( gcoeff(0) >= explower ) then
            gcoeff(0) = exp(gcoeff(0))*half
         else
            gcoeff(0) = zero
         end if
         dj2 = (k+1)*half
         betaf(1) = betacdf( xarg, dj2+half, r2, ier )
         if ( ier /= 0 ) then
            ier = 4
            return
         end if
         gcoeff(1) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
         if ( gcoeff(1) >= explower ) then
            gcoeff(1) = exp(gcoeff(1))*half
         else
            gcoeff(1) = zero
         end if
         xgamf(0) = (k+1)*half*xl+r2l1mx+log_gamma(r2+(k+1)*half)-r2l-log_gamma((k+3)*half)
         xgamf(1) = (k+2)*half*xl+r2l1mx+log_gamma(r2+k*half+one)-r2l-log_gamma((k+4)*half)
         xgamb(0) = xgamf(0)
         xgamb(1) = k*half*xl+r2l1mx+log_gamma(r2+k*half)-r2l-log_gamma((k+2)*half)
         if ( xgamf(0) >= explower .and. xgamf(1) >= explower .and.   &
              xgamb(1) >= explower ) then
            xgamf(0) = exp(xgamf(0))
            xgamf(1) = exp(xgamf(1))
            xgamb(0) = xgamf(0)
            xgamb(1) = exp(xgamb(1))
         else         ! xgamf/b at k too small for recurrence to hold
            k = 0     ! thus start at k=0
         end if
      end if
   else
      beta0 = betaf(0)
   end if
   if ( k > 0 ) then
      gcoefb(0) = gcoeff(0)
      betab(0) = betaf(0)
      dj2 = (k-1)*half
      gcoefb(1) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
      if ( gcoefb(1) >= explower ) then
         gcoefb(1) = exp(gcoefb(1))*half
      else
         gcoefb(1) = zero
      end if
      betab(1) = betaf(1) + xgamb(1)
   else
      gcoeff(0) = ((one-aqa)**q2)*half
      gcoeff(1) = qqal - q2l - dlg15 + half*aqal + log_gamma(q2+half)
      if ( gcoeff(1) >= explower ) then
         gcoeff(1) = exp(gcoeff(1))*half
      else
         gcoeff(1) = zero
      end if
      betaf(0) = beta0
      betaf(1) = betacdf( xarg, one, r2, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      xgamf(0) = r2l1mx+log_gamma(r2+half)-r2l-dlg15+half*xl
      xgamf(1) = (one-xarg)**r2*xarg*r2
      xgamb(0) = xgamf(0)
      xgamb(1) = (one-xarg)**r2
      if ( xgamf(0) >= explower ) then
         xgamf(0) = exp(xgamf(0))
         xgamb(0) = xgamf(0)
      else         ! xgamf/b at 0 too small for recurrence to hold
         ier = 3   ! thus quit
         return
      end if
      gcoefb(0) = zero
      gcoefb(1) = zero
      betab(0) = zero
      betab(1) = zero
   end if
   betak = betaf(0)
   cdf(0) = gcoeff(0)*betaf(0)     ! sum of even terms (positive)
   cdf(1) = gcoeff(1)*betaf(1) + & ! sum of odd terms (possibly negative)
            gcoefb(1)*betab(1)     ! (negative when xx < 0)
   sumg = gcoeff(0) + gcoeff(1) + gcoefb(1)
   prv = -one

   !  Iteration loops
   !  Do forward and backward computations k times or until convergence
   kit = min( k, MAXITER )
   do j = 2, kit
      jm = mod(k+j,2)
      betaf(jm) = max( betaf(jm)-xgamf(jm), zero )
      gcoeff(jm) = gcoeff(jm)*(k+j-two+q)*aqa/(k+j)
      xgamb(jm) = xgamb(jm)*(k-j+3)/((k-j+1+r)*xarg)
      betab(jm) = betab(jm) + xgamb(jm)
      gcoefb(jm) = gcoefb(jm)*(k-j+two)/(aqa*(k-j+q))

      cdf(jm) = cdf(jm) + gcoeff(jm)*betaf(jm) + gcoefb(jm)*betab(jm)

      sumg = sumg + gcoeff(jm) + gcoefb(jm)
      if ( (one-sumg)*beta0 <= TOL ) goto 10
      !  Looking for no more evolution of the sum
      if ( cdf(0) == prv(0) .and. cdf(1) == prv(1) ) then
         ier = -1
         goto 10
      end if
      prv(jm) = cdf(jm)
      xgamf(jm) = xgamf(jm)*xarg*(k+j-1+r)/(k+j+1)
   end do

   !  Do forward computations until convergence
   do j = max(kit,1)+1, MAXITER
      jm = mod(k+j,2)
      betaf(jm) = max( betaf(jm)-xgamf(jm), zero )
      gcoeff(jm) = gcoeff(jm)*(k+j-two+q)*aqa/(k+j)

      cdf(jm) = cdf(jm) + gcoeff(jm)*betaf(jm)

      sumg = sumg + gcoeff(jm)
      if ( (one-sumg)*betaf(jm) <= TOL ) goto 10
      !  Looking for no more evolution of the sum
      if ( cdf(0) == prv(0) .and. cdf(1) == prv(1) ) then
         ier = -1
         goto 10
      end if
      prv(jm) = cdf(jm)
      xgamf(jm) = xgamf(jm)*xarg*(k+j-1+r)/(k+j+1)
   end do

   !  Maximum number of iterations is reached

   ier = 2

   10 continue
   !  The end: add Pr( K'<0 )
   !  Pr( K'(q,r,abs(a1)) < 0 ) = Pr( t(q) < -abs(a1) )

   if ( xneg ) then
      kprimecdf = tcdf( -abs(a1), q, bel, iok ) + cdf(1) - cdf(0)
   else
      kprimecdf = tcdf( -abs(a1), q, bel, iok ) + cdf(1) + cdf(0)
   end if
   if ( iok /= 0 ) then
      ier = 4
      return
   end if
   if ( a1 < zero ) kprimecdf = one - kprimecdf

   if ( ier == -1 ) then
      ! See if the "reverse" problem is OK
      call kprimebis( a1, r, q, x, TOL, MAXITER, iok, xx )
      if ( iok == 0 ) then
         kprimecdf = one - xx
         ier = 0
      end if
   end if

      ! Check out of limits
   if ( kprimecdf < zero ) then
      if ( kprimecdf >= -TOL ) then
         kprimecdf = zero
      else
         ier = 5 + ier
      end if
   else if ( kprimecdf > one ) then
      if ( kprimecdf <= one+TOL ) then
         kprimecdf = one
      else
         ier = 5 + ier
      end if
   end if

END FUNCTION kprimecdf


SUBROUTINE kprimebis( x, q, r, a1, TOL, MAXITER, ier, result )
    !     Returns in result the K' cdf
    !     This is the subroutine version of the Kprimecdf function used
    !     for the reverse problem when ier=-1 in the Kprimecdf function
    !     (Same arguments as in kprimecdf function)
   IMPLICIT NONE
   INTEGER, PARAMETER        :: PR=KIND(1.0D0)

   !  Arguments
   REAL(PR), INTENT(in) :: x, q, r, a1, TOL
   REAL(PR), INTENT(out) :: result
   INTEGER, INTENT(in) :: MAXITER
   INTEGER, INTENT(out) :: ier

   !  local declarations
   REAL(PR), EXTERNAL :: betacdf, lprimecdf, tcdf

   REAL(PR), PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0
   REAL(PR), PARAMETER :: dlg15=-0.120782237635245204D0  ! =log(gamma(1.5))
   REAL(PR), PARAMETER :: bel=1.0D6, qlimit=2.0D6, rlimit=2.0D6
   REAL(PR), PARAMETER :: eps=0.223D-15, explower=-706.893D0
   REAL(PR), PARAMETER :: betaratio=0.01D0
   INTEGER, PARAMETER :: kmin=10    ! When starting point of iterations is below
                                    ! this limit "method 2" is not judged worthwile

   !     These constants are machine dependent:
   !     eps = machine epsilon
   !           (the smallest real such that 1.0 + eps > 1.0)
   !     explower = minimum valid argument for the exponential function
   REAL(PR) :: a2, aqa, aqal, beta0, betak, dj2, q2, q2l, qqal
   REAL(PR) :: r2, r2l, r2l1mx, sumg, xarg, xl, xx
   INTEGER :: iok, j, jm, k, kit
   LOGICAL :: xneg
   REAL(PR) :: betab(0:1), betaf(0:1), gcoefb(0:1), gcoeff(0:1)
   REAL(PR) :: cdf(0:1), xgamb(0:1), xgamf(0:1)
   REAL(PR) :: prv(0:1)

   !-----------------------------------------------------------------
   xx = zero
   ier = 0

   !  Test for valid input arguments
   if ( q <= zero .or. r <= zero .or. TOL >= one .or. TOL <= eps ) then
      ier = 1
      return
   end if

   !  Case x = 0
   if ( abs(x) < eps ) then
      cdf(0) = zero
      cdf(1) = zero
      xneg = .false.
      goto 10
   end if

   !  If a1 is close to zero use approximation (exact if a1 = 0)
   if ( abs(a1) < eps ) then
      result = tcdf( x, r, bel, ier )
      if ( ier /= 0 ) ier = 4
      return
   end if

   !  If q or r is large enough use limiting distribution
   if ( q > qlimit ) then
      result = one - lprimecdf( a1, r, x, TOL, MAXITER, ier ) ! noncentral t
      if ( ier /= 0 ) ier = 4
      return
   else if ( r > rlimit ) then
      result = lprimecdf( x, q, a1, TOL, MAXITER, ier )
      if ( ier /= 0 ) ier = 4
      return
   end if

   !  Define usefull parameters
   xx = x
   xarg = x*x
   xarg = xarg / (xarg+r)
   q2 = q*half
   r2 = r*half
   a2 = a1*a1

   !  Case xarg close to one (x tends to +/- infinity)
   if ( abs(xarg-one) < eps+eps ) then
      if ( x > zero ) result = one
      return
   end if

   !  Case a1 < 0 : change sign of x :
   !  Pr( K'(a1,1) < x ) = 1 - Pr( K'(-a1,1) < -x )
   if ( a1 < zero ) xx = -xx
   xneg = xx < zero          ! When a1 and x are not of same sign, the series is alternate
                             ! and this is flagged by xneg

   !  General case (iterations)
   !  When a1 > 0:
   !     1) if xx > 0:
   !        Pr( K'<xx ) =  Pr( K'<0 ) + Pr( 0<K'<xx )
   !     2) if xx < 0:
   !        Pr( K'<xx ) =  Pr( K'<0 ) - Pr( xx<K'<0 )

   !  First, calculate  Pr( Min(0,xx)<K'<Max(0,xx) )
   aqa = a2/(q+a2)
   aqal = log(aqa)
   qqal = q2*log(one-aqa)
   q2l = log_gamma(q2)
   r2l = log_gamma(r2)
   r2l1mx = r2*log(one-xarg)
   xl = log(xarg)
   k = max( 0, int( a2-(a2+a2)/q ) )   ! Mode of neg. bin. distribution
   if ( k < kmin ) k = 0               ! k is not large enough to be worthwile
   k = (k/2)*2                         ! Make k an even number

   ! To deal with the case where k=0 and r<=1, xgamf is updated at
   ! end of iteration loop, while xgamb is updated at beginning of loop
   dj2 = k*half
   betaf(0) = betacdf( xarg, dj2+half, r2, ier )
   if ( ier /= 0 ) then
      ier = 4
      return
   end if
   if ( k > 0 ) then   ! Remember k is even
      beta0 = betacdf( xarg, half, r2, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      if ( betaf(0) < beta0*betaratio ) then ! Beta at k is small, so k is moved to
         k = INT(xarg*k)                     ! somewhere between modes of g_j's and H_j's
         k = (k/2)*2                         ! Make k an even number
         dj2 = k*half
         betaf(0) = betacdf( xarg, dj2+half, r2, ier )
         if ( ier /= 0 ) then
            ier = 4
            return
         end if
      end if
      if ( k > 0 ) then
         gcoeff(0) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
         if ( gcoeff(0) >= explower ) then
            gcoeff(0) = exp(gcoeff(0))*half
         else
            gcoeff(0) = zero
         end if
         dj2 = (k+1)*half
         betaf(1) = betacdf( xarg, dj2+half, r2, ier )
         if ( ier /= 0 ) then
            ier = 4
            return
         end if
         gcoeff(1) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
         if ( gcoeff(1) >= explower ) then
            gcoeff(1) = exp(gcoeff(1))*half
         else
            gcoeff(1) = zero
         end if
         xgamf(0) = (k+1)*half*xl+r2l1mx+log_gamma(r2+(k+1)*half)-r2l-log_gamma((k+3)*half)
         xgamf(1) = (k+2)*half*xl+r2l1mx+log_gamma(r2+k*half+one)-r2l-log_gamma((k+4)*half)
         xgamb(0) = xgamf(0)
         xgamb(1) = k*half*xl+r2l1mx+log_gamma(r2+k*half)-r2l-log_gamma((k+2)*half)
         if ( xgamf(0) >= explower .and. xgamf(1) >= explower .and.   &
              xgamb(1) >= explower ) then
            xgamf(0) = exp(xgamf(0))
            xgamf(1) = exp(xgamf(1))
            xgamb(0) = xgamf(0)
            xgamb(1) = exp(xgamb(1))
         else         ! xgamf/b at k too small for recurrence to hold
            k = 0     ! thus start at k=0
         end if
      end if
   else
      beta0 = betaf(0)
   end if
   if ( k > 0 ) then
      gcoefb(0) = gcoeff(0)
      betab(0) = betaf(0)
      dj2 = (k-1)*half
      gcoefb(1) = qqal - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
      if ( gcoefb(1) >= explower ) then
         gcoefb(1) = exp(gcoefb(1))*half
      else
         gcoefb(1) = zero
      end if
      betab(1) = betaf(1) + xgamb(1)
   else
      gcoeff(0) = ((one-aqa)**q2)*half
      gcoeff(1) = qqal - q2l - dlg15 + half*aqal + log_gamma(q2+half)
      if ( gcoeff(1) >= explower ) then
         gcoeff(1) = exp(gcoeff(1))*half
      else
         gcoeff(1) = zero
      end if
      betaf(0) = beta0
      betaf(1) = betacdf( xarg, one, r2, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      xgamf(0) = r2l1mx+log_gamma(r2+half)-r2l-dlg15+half*xl
      xgamf(1) = (one-xarg)**r2*xarg*r2
      xgamb(0) = xgamf(0)
      xgamb(1) = (one-xarg)**r2
      if ( xgamf(0) >= explower ) then
         xgamf(0) = exp(xgamf(0))
         xgamb(0) = xgamf(0)
      else         ! xgamf/b at 0 too small for recurrence to hold
         ier = 3   ! thus quit
         return
      end if
      gcoefb(0) = zero
      gcoefb(1) = zero
      betab(0) = zero
      betab(1) = zero
   end if
   betak = betaf(0)
   cdf(0) = gcoeff(0)*betaf(0)     ! sum of even terms (positive)
   cdf(1) = gcoeff(1)*betaf(1) + & ! sum of odd terms (possibly negative)
            gcoefb(1)*betab(1)     ! (negative when xx < 0)
   sumg = gcoeff(0) + gcoeff(1) + gcoefb(1)
   prv = -one

   !  Iteration loops
   !  Do forward and backward computations k times or until convergence
   kit = min( k, MAXITER )
   do j = 2, kit
      jm = mod(k+j,2)
      betaf(jm) = max( betaf(jm)-xgamf(jm), zero )
      gcoeff(jm) = gcoeff(jm)*(k+j-two+q)*aqa/(k+j)
      xgamb(jm) = xgamb(jm)*(k-j+3)/((k-j+1+r)*xarg)
      betab(jm) = betab(jm) + xgamb(jm)
      gcoefb(jm) = gcoefb(jm)*(k-j+two)/(aqa*(k-j+q))

      cdf(jm) = cdf(jm) + gcoeff(jm)*betaf(jm) + gcoefb(jm)*betab(jm)

      sumg = sumg + gcoeff(jm) + gcoefb(jm)
      if ( (one-sumg)*beta0 <= TOL ) goto 10
      !  Looking for no more evolution of the sum
      if ( cdf(0) == prv(0) .and. cdf(1) == prv(1) ) then
         ier = -1
         goto 10
      end if
      prv(jm) = cdf(jm)
      xgamf(jm) = xgamf(jm)*xarg*(k+j-1+r)/(k+j+1)
   end do

   !  Do forward computations until convergence
   do j = max(kit,1)+1, MAXITER
      jm = mod(k+j,2)
      betaf(jm) = max( betaf(jm)-xgamf(jm), zero )
      gcoeff(jm) = gcoeff(jm)*(k+j-two+q)*aqa/(k+j)

      cdf(jm) = cdf(jm) + gcoeff(jm)*betaf(jm)

      sumg = sumg + gcoeff(jm)
      if ( (one-sumg)*betaf(jm) <= TOL ) goto 10
      !  Looking for no more evolution of the sum
      if ( cdf(0) == prv(0) .and. cdf(1) == prv(1) ) then
         ier = -1
         goto 10
      end if
      prv(jm) = cdf(jm)
      xgamf(jm) = xgamf(jm)*xarg*(k+j-1+r)/(k+j+1)
   end do

   !  Maximum number of iterations is reached
   ier = 2

   10 continue
   !  The end: add Pr( K'<0 )
   !  Pr( K'(q,r,abs(a1)) < 0 ) = Pr( t(q) < -abs(a1) )

   if ( xneg ) then
      result = tcdf( -abs(a1), q, bel, iok ) + cdf(1) - cdf(0)
   else
      result = tcdf( -abs(a1), q, bel, iok ) + cdf(1) + cdf(0)
   end if
   if ( iok /= 0 ) then
      ier = 4
      return
   end if
   if ( a1 < zero ) result = one - result

END SUBROUTINE kprimebis
