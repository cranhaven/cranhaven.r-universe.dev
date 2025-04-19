FUNCTION tcdf( x, p, plim, ier )
    !-----------------------------------------------------------------------
    !     Computes the probability that a random variable distributed
    !     according to the Student's t distribution with P degrees of
    !     freedom, is less than or equal to X.
    !
    !     X     - Input . Value of the variable                   - Real
    !     P     - Input . Degrees of freedom              (P > 0) - Real
    !     PLIM  - Input . Limit for P over which the distribution - Real
    !                     is approximated by a normal distr.
    !     IER   - Output. Return code :                           - Integer
    !                     0 = normal
    !                     1 = invalid input argument
    !                         (then tcdf = -1.)
    !                     2 = maximum number of iterations reached
    !                         (then tcdf = value at last iteration,
    !                                        or zero)
    !                     3 = Beta out of limits
    !                         (betacdf < -EPS or betacdf > 1+EPS)
    !
    !     External functions called:
    !       BETACDF  NCDF
    !
    !     NOTE : the error on tcdf is supposed to be minimal.
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER        :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: tcdf

   !  Arguments
   REAL(PR), INTENT(in) :: x, p, plim
   INTEGER, INTENT(out) :: ier

   !  local declarations
   REAL(PR), EXTERNAL :: betacdf, ncdf
   REAL(PR), PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0
   REAL(PR) :: t, xx

   !  Test for valid input arguments
   if ( p <= zero ) then
      ier  = 1
      tcdf = -one
      return
   end if

   if ( p <= plim ) then
      xx = x * x
      t  = betacdf( xx/(xx+p), half, p*half, ier )
      if ( x >= zero ) then
         tcdf = half + t*half
      else
         tcdf = half - t*half
      end if
   else
      ier  = 0
      tcdf = ncdf( x )
   end if

END FUNCTION tcdf
