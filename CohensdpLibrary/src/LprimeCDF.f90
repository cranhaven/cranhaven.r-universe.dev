FUNCTION lprimecdf( x, q, a, TOL, MAXITER, ier )
    !-----------------------------------------------------------------------
    !     Poitevineau, J. and Lecoutre, B. (2010) Statistical distributions for bayesian 
    !         experimental data analysis fortran functions 1. continuous distributions,
    !         url = https://eris62.eu
    !
    !     Calculates the probability that a random variable distributed
    !     according to the Lambda' distribution with Q degrees of freedom,
    !     A centrality parameter, is less than or equal to X
    !
    !     X     - Input . Value of the variable                   - Real
    !     Q     - Input . Degrees of freedom             (Q >  0) - Real
    !     A     - Input . Eccentricity parameter                  - Real
    !     TOL - Input . Maximum absolute error required on      - Real
    !                     lprimecdf (stopping criterion)
    !                     (eps < TOL < 1 where eps is machine
    !                     epsilon; see parameter statement below)
    !     MAXITER- Input . Maximum number of iterations            - Integer
    !     IER   - Output. Return code :                           - Integer
    !                     0 = normal
    !                     1 = invalid input argument
    !                         (then lprimecdf = zero)
    !                     2 = maximum number of iterations reached
    !                         (then lprimecdf = value at last iteration,
    !                                    or zero)
    !                     3 = required accuracy cannot be reached
    !                         (then lprimecdf = value at last iteration)
    !                     4 = error in auxiliary function
    !                         (chi2cdf or tcdf)
    !                     7 = result out of limits (i.e. <0 or >1)
    !
    !     External functions called:
    !       CHI2CDF  NCDF  TCDF
    !       DLGAMA (Log(Gamma(.)) if not in FORTRAN library
    !
    !     Fortran functions called:
    !       ABS  EXP  LOG  NINT  (and DLGAMA if available)
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER  :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: lprimecdf

   !  Arguments
   REAL(PR), INTENT(in):: x, q, a, TOL
   INTEGER, INTENT(in) :: MAXITER
   INTEGER, INTENT(out):: ier

   !  local declarations
   REAL(PR), EXTERNAL  :: chi2cdf, ncdf, tcdf

   REAL(PR), PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0
   REAL(PR), PARAMETER :: onep5=1.5D0, two=2.0D0, three=3.0D0, four=4.0D0
   REAL(PR), PARAMETER :: dlg15=-0.120782237635245204D0  ! =log(gamma(1.5))
   REAL(PR), PARAMETER :: twol=0.6931471805599453D0      ! =log(2)
   REAL(PR), PARAMETER :: bel=1.0D6, qlimit=1.0D5, cinf=1.0D-20
   REAL(PR), PARAMETER :: dflimit=1.0D6

   REAL(PR), PARAMETER :: eps=0.223D-15, tinyr=1.0D-307, explower=-706.893D0
   REAL(PR), PARAMETER :: relerr=1.0D-14       ! Relative error assumed
                                               ! in recurrence calculations
   REAL(PR), PARAMETER :: xp=tinyr/relerr
   INTEGER, PARAMETER  :: jmax=200

   !     dflimit = limit for approximation in chi2cdf function
   !     These constants are machine dependent:
   !     eps   = machine epsilon
   !             (the smallest real such that 1.0 + eps > 1.0)
   !     tinyr = the smallest positive real
   !     explower = minimum valid argument for the exponential function
   !             (i.e. log(tinyr))
   REAL(PR) :: aqal, a2, dj, dj2, erp, err, ga, g0,  &
               kgj, kgl,                             &
               qqal, q2, q2l,                        &
               sum, sumg, sumgk, sumneg,             &
               xarg, xl, x2, xx
   INTEGER  :: iok, j, ja, jjj, jm, jz, j0
   LOGICAL  :: xneg
   REAL(PR) :: gl(0:1), kx(0:1), rl(0:1)

   !-----------------------------------------------------------------
   lprimecdf = zero
   ier = 0

   !  Test for valid input arguments
   if ( q <= zero .or. TOL >= one .or. TOL <= eps ) then
      ier = 1
      return
   end if

   !  Case x = 0
   if ( abs(x) < eps ) then
      sum = zero
      sumneg = zero
      xneg = .false.
      goto 10
   end if

   !  If a is close to zero or q is large enough use approximation
   !  (exact if a = 0)
   if ( abs(a) < eps ) then
      lprimecdf = ncdf( x )
      return
   else if ( q > qlimit ) then
      g0 = one - one/(four*q)
      xl = g0*a                    ! mean
      x2 = one + (one-g0*g0)*a*a   ! variance
      lprimecdf = ncdf( (x-xl)/sqrt(x2) )
      return
   end if

   !  Define usefull parameters
   xx   = x
   xarg = x*x
   x2   = half * xarg
   xl   = log( xarg )
   q2   = q * half
   a2   = a*a

   !  Case a < 0 : change sign of x :
   !  Pr( L'(a,1) < x ) = 1 - Pr( L'(-a,1) < -x )

   if ( a < zero ) xx = -xx
   xneg = xx < zero
   !  General case (iterations)
   !  kx's are decreasing for all j's
   !  gl's are decreasing for j >= jjj = 2*(a2/2 - a2/q - 1)
   !  kgj, sumg, sumgk are only used for stopping rule
   !  Logs are used to avoid underflows

   err  = TOL * half
   erp  = err / relerr
   qqal = q2 * log( q/(q+a2) )
   aqal = log( a2/(q+a2) )
   q2l  = log_gamma( q2 )
   jjj  = 2 * nint( a2*half - a2/q - half )
   jjj  = max( jjj, 0 )

   !  Examine rate of convergence:
   !  find starting index for iterations (j0)
   !  cinf is a limit under which sum of g coefficients is
   !  considered as negligible
   !  g0 is gl at j0
   j0 = 0
   g0 = qqal - twol
   if ( jjj >= jmax ) then
      if ( g0 < log(cinf) ) then
         dj2 = half * jjj
         ga  = qqal - twol - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
         if ( ga < explower )  goto 5   !  Cannot be computed
         if ( exp(ga) < cinf ) goto 5   !  Cannot be computed
         ja = 0
         jz = jjj
         ga = g0
         do
            j0  = (ja+jz) / 2
            dj2 = half * j0
            g0  = qqal - twol - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
            if ( g0 >= explower ) then
               if ( (j0+1)*exp(g0) < cinf ) then
                  ja = j0
                  ga = g0
               else
                  jz = j0
               end if
            else
               ja = j0
               ga = g0
            end if
            if ( ja+1 >= jz ) exit
         end do
         j0 = ja
         g0 = ga
      end if
   end if
5  continue

   !  Initialize

   !  When j0 > 0, it is assumed sumg at j0 is negligible,
   !  and the upper bound is taken for kgj:
   !  kgj = sum( j*g*kx ) <= sum( j*g )  (as kx <= 1)
   !                      <= exp(g0)*sum( j )    (as j <= j0 < jjj)
   !                      <= exp(g0)*j0*(j0+1)/2
   kgj = zero
   if ( j0 > 0 ) then
      jm = mod(j0,2)
      dj = j0
      if ( g0 >= explower ) kgj = (dj+one) * dj * half * exp(g0)
      gl(jm) = g0
      kx(jm) = chi2cdf( xarg, dj+one, dflimit, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      rl(jm) = (dj+one)*half*log(x2) - x2 - log_gamma((dj+three)*half)
      jm  = abs( jm-1 )
      dj  = j0 + 1
      dj2 = dj * half
      gl(jm) = qqal - twol - q2l + log_gamma(dj2+q2) - log_gamma(dj2+one) + dj2*aqal
      kx(jm) = chi2cdf( xarg, dj+one, dflimit, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      rl(jm) = (dj+one)*half*log(x2) - x2 - log_gamma((dj+three)*half)
   else
      gl(0) = qqal - twol
      gl(1) = gl(0) - q2l - dlg15 + half*aqal + log_gamma(q2+half)
      kx(0) = chi2cdf( xarg, one, dflimit, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      kx(1) = chi2cdf( xarg, two, dflimit, ier )
      if ( ier /= 0 ) then
         ier = 4
         return
      end if
      rl(0) = half*log(x2) - ( x2 + log_gamma(onep5) )
      rl(1) = log(x2) - ( x2 + log_gamma(two) )
   end if
   sum   = zero
   sumneg= zero
   sumg  = zero
   sumgk = zero

   !  Iteration loop
   do j = j0, j0+MAXITER
      dj = j
      jm = mod(j,2)

      if ( kx(jm) > zero ) then
         kgl = log(kx(jm)) + gl(jm)
         if ( kgl >= explower ) then
            kgl   = exp(kgl)
            sumgk = sumgk + kgl
            !  When x<0 the signs of the alternating series are reversed
            !  so that sum will be added to the t in the final step
            !  rather than substracted
            if ( xneg .and. jm == 0 ) then
               sumneg = sumneg + kgl  ! sum of negative terms
            else
               sum = sum + kgl
            end if
            kgj = kgj + kgl*dj
            !  Check loss of accuracy
            if ( kgj >= erp ) then
               ier = 3
               goto 10
            end if
         end if
      end if

      !  Check accuracy (stopping rule)
      !  xp is used to prevent possible underflow
      if ( gl(jm) >= explower ) sumg = sumg + exp(gl(jm))
      if ( kgj >= xp ) then
         if ( relerr*kgj+kx(jm)*(one-sumg) < err ) goto 10
      else
         if (            kx(jm)*(one-sumg) < err ) goto 10
      end if

      !  Prepare next iteration
      dj2    = dj * half
      gl(jm) = gl(jm) + aqal + log(dj2+q2) - log(dj2+one)
      if ( rl(jm) >= explower ) kx(jm) = kx(jm) - exp( rl(jm) )
      rl(jm) = rl(jm) - log(dj+three) + xl

   end do

   !  Maximum number of iterations is reached
   ier = 2

   !  The end
10 continue
   !  Pr( L'(q,abs(a)) < 0 ) = Pr( t(q) < -abs(a) )
   lprimecdf = sum - sumneg + tcdf( -abs(a), q, bel, iok )
   if ( iok /= 0 ) then
      ier = 4
      return
   end if
   if ( a < zero ) lprimecdf = one - lprimecdf
   if ( lprimecdf < zero ) then
      if ( lprimecdf >= -TOL ) then
         lprimecdf = zero
      else
         ier = 7
      end if
   else if ( lprimecdf > one ) then
      if ( lprimecdf <= one+TOL ) then
         lprimecdf = one
      else
         ier = 7
      end if
   end if

END FUNCTION lprimecdf
