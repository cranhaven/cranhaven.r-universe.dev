FUNCTION    dtrinv( func, f, linf, lsup, xinf, xsup, ex, sx, TOL, MAXITER, ier )
    !-----------------------------------------------------------------------
    !     Computes the inverse of a continuous distribution function.
    !     That is, given the cumulative distribution function FUNC of
    !     the random variable X, a probability F, an absolute error
    !     TOL on probability, it returns a value x such that:
    !     F - TOL <= FUNC(x) <= F + TOL  with FUNC(x) = Proba( X < x )
    !
    !     FUNC  - Input . The name of the user supplied Fortran   - Real
    !                     function (real type)
    !                     computing the cumulative distribution
    !                     function.
    !                     This function must be in the form:
    !                     FUNCTION FUNC( X, IOK )
    !                     where X, double precision input, is the
    !                     value for which the function is to be
    !                     evaluated, and IOK a positive integral
    !                     return code with 0 coding for no error
    !     F     - Input . Probability                             - Real
    !     LINF  - Input . Must be set to .TRUE. if the random     - Logical
    !                     variable has a non-infinite lower bound
    !     LSUP  - Input . As LINF but for an upper bound          - Logical
    !     XINF  - Input . A lower bound  (used as the solution    - Real
    !                     when  F < TOL).
    !                     If LINF was set to .FALSE. then XINF
    !                     must represent -infinity (e.g.-1.e300)
    !     XSUP  - Input . As XINF but for the upper bound         - Real
    !                     (must be greater than XINF)
    !     EX    - Input . A central value of the variable         - Real
    !                     (e.g. the mean). It is used to compute
    !                     the initial solution
    !     SX    - Input . A non-zero scale index of the variable  - Real
    !                     in same units as the variable (e.g. the
    !                     standard deviation).
    !                     May be positive or negative: the sign
    !                     codes the type of the initial solution:
    !                     if negative, it is EX; if positive, from
    !                     a normal (EX,SX**2) distribution
    !     TOL   - Input . Maximum absolute error on probablity    - Real
    !                     (1.e-12 < TOL < 1)
    !     MAXITER -Input. Maximum number of iterations            - Integer
    !     IER   - Output. Return code:                            - Integer
    !                     0 = no error
    !                     1 = invalid F, TOL, SX, XINF, XSUP
    !                         on input (then DTRINV returns zero)
    !                     2 = max. number of iterations reached
    !                         or degeneration
    !                         (then DTRINV returns the value
    !                         at last iteration)
    !                     <0= -IOK from FUNC: error in FUNC
    !                         (then DTRINV returns zero)
    !
    !     External functions called:
    !       FUNC (user supplied)
    !     Fortran functions called:
    !       ABS  LOG  SQRT
    !     J. Poitevineau (CNRS-URA1201)  June 03, 1985
    !     Modif. 30/09/94 (protections)
    !-----------------------------------------------------------------------
    !     Algorithm:
    !     Initial solution: EX if SX < 0
    !                     : from a normal (EX,SX**2) distribution if SX > 0.
    !     Then, if no bounds exist, some empirical ones are found by
    !     incrementing (or decrementing) EX by Abs(SX) as long as needed
    !     and reasonable (with a maximum of 100 times).
    !     Then the bounds are iteratively modified by a 2-step process
    !     (let Li and Ui be the lower and upper bounds at iteration no. i):
    !        step1: solution Xi = linear interpolation on interval (Li,Ui)
    !        step2: solution Yi = middle of (Xi,Ui) (or (Li,Xi) according
    !                             to whether FUNC(Xi) < F or > F),
    !               Li+1 = Li (or Yi),  Ui+1 = Yi (or Ui)
    !
    !     Of course, at each step, whenever FUNC is called, the result is
    !     compared to F and the process is stopped if the solution fits.
    !
    !     DTRINV can cope with distributions, though definite, having some
    !     infinite parameters (variance, and mean possibly); e.g. a
    !     Fisher-Snedecor F with 1 and 1 degrees of freedom. In such cases
    !     SX should be set to a very large negative value, e.g. -1.0e30.
    !     If the mean is also infinite, EX should be set to such a value
    !     too, but with the same sign as the infinite mean.
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER    :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: dtrinv

   !  Arguments
   REAL(PR), EXTERNAL    :: func
   REAL(PR), INTENT(in)  :: f
   LOGICAL,  INTENT(in)  :: linf, lsup
   REAL(PR), INTENT(in)  :: xinf, xsup, ex, sx, TOL
   INTEGER,  INTENT(in)  :: MAXITER
   INTEGER,  INTENT(out) :: ier

   !  Local declarations
   REAL(PR), PARAMETER  :: zero=0.0D0, half=0.5D0, one=1.0D0
   REAL(PR)             :: asx, c, t, t2, xa, xm, xmx, xz, y, ya, yz
   INTEGER              :: iok, it

   !--------------------------------------------------------------------
   !  Test for valid input arguments
   if ( f < zero .or. f > one .or. sx == zero .or. xinf >= xsup  &
      .or. TOL >= one .or. TOL <= 1.0D-12 ) then
        if ( f < zero .or. f > one ) then
            ier = 11
            dtrinv = f
            return
        end if
        if ( sx == zero ) then
            ier = 12
            dtrinv = sx
            return
        end if
        if ( xinf >= xsup ) then
            ier = 13
            dtrinv = xinf
            return
        end if
        if ( TOL >= one .or. TOL <= 1.0D-12 ) then
            ier = 14
            dtrinv = TOL
            return
        end if
        dtrinv = zero
        ier    = 1
        return
   end if

   ier = 0

   !  f near 0 or 1
   if ( f <= TOL ) then
      dtrinv = xinf
      return
   end if
   if ( f >= one-TOL ) then
      dtrinv = xsup
      return
   end if

   ! Compute initial solution xm
   asx = abs(sx)
   xm  = ex
   if ( sx > zero ) then
      !  From normal distribution, mean ex & std.dev sx
      !  (from IBM-SSP subroutine NDTRI)
      c = f
      if ( c > half ) c = one - c
      t2 = log( one/(c*c) )
      t  = sqrt(t2)
      xm = t - (2.515517D0+0.802853D0*t+0.010328D0*t2)/            &
               (1.0D0+1.432788D0*t+0.189269D0*t2+0.001308D0*t*t2)
      if ( f <= half ) xm = -xm
      xm = xm*asx + ex
   end if
   if ( xm < xinf ) xm = xinf
   if ( xm > xsup ) xm = xsup
   y = func( xm, iok )
   if ( iok /= 0 ) goto 30
   if ( abs(f-y) <= TOL ) then
      dtrinv = xm
      return
   end if

   !  Get lower (xa) and upper (xz) bounds for x
   if ( f < y ) then
      !  Get an upper bound
      xz = xm
      yz = y
      if ( linf ) then
            ! Check limit
         xa = xinf
         ya = func( xa, iok )
         if ( iok /= 0 ) goto 30
         if ( abs(f-ya) <= TOL ) then
            dtrinv = xa
            return
         end if
         if ( f < ya ) then
            ier    = 2
            dtrinv = xa
            return
         end if
      else
         !  Find a lower bound and possibly adjust the upper bound
         xmx = ex
         do it = 1, 100
            xmx = xmx - asx
            ya  = func( xmx, iok )
            if ( iok /= 0 ) goto 30
            if ( abs(f-ya) <= TOL ) then
               dtrinv = xmx
               return
            else if ( ya < f ) then
               xa = xmx
               goto 15
            end if
            xz = xmx
            yz = ya
         end do
         !  Empirical lower bound not found, take limit
         xa = xinf
         ya = zero
15       continue
      end if
   else
      !  Get a lower bound
      xa = xm
      ya = y
      if ( lsup ) then
            ! Check limit
         xz = xsup
         yz = func( xz, iok )
         if ( iok /= 0 ) goto 30
         if ( abs(f-yz) <= TOL ) then
            dtrinv = xz
            return
         end if
         if ( f > yz ) then
            ier    = 2
            dtrinv = xz
            return
         end if
      else
         !  Find an upper bound and possibly adjust the lower bound
         xmx = ex
         do it = 1, 100
            xmx = xmx + asx
            yz  = func( xmx, iok )
            if ( iok /= 0 ) goto 30
            if ( abs(f-yz) <= TOL ) then
               dtrinv = xmx
               return
            else if ( yz > f ) then
               xz = xmx
               goto 25
            end if
            xa = xmx
            ya = yz
         end do
         !  Empirical upper bound not found, take limit
         xz = xsup
         yz = one
25       continue
      end if
   end if

   ! Iteration loop
   xmx = xa

   do it = 1, MAXITER
      !  1st step: solution xm = linear interpolation on (xa, xz)

      !  Note that there's no protection for yz=ya
      !  in the following statement because it should not occur...
      xm = xa + (xz-xa)*(f-ya)/(yz-ya)

      !  Protection against degenerated cases
      if ( xm == xmx ) then
         ier    = 2
         dtrinv = xm
         return
      end if
      xmx = xm
      y  = func( xm, iok )
      if ( iok /= 0 ) goto 30
      if ( abs(f-y) <= TOL ) then
         dtrinv = xm
         return
      end if
      if ( f < y ) then
         xz = xm
         yz = y
      else
         xa = xm
         ya = y
      end if

      !  2nd step: solution xm = middle of updated (xa, xz)
      xm = (xa+xz) * half
      y  = func( xm, iok )
      if ( iok /= 0 ) goto 30
      if ( abs(f-y) <= TOL ) then
         dtrinv = xm
         return
      end if
      if ( f < y ) then
         xz = xm
         yz = y
      else
         xa = xm
         ya = y
      end if

   end do

   !  Maximum number of iterations is reached
   ier    = 2
   dtrinv = xm
   return

   !  Error in FUNC function
30 continue
   ier    = -iok
   dtrinv = zero

END FUNCTION dtrinv
