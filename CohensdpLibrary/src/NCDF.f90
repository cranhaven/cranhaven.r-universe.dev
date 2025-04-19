FUNCTION ncdf( x )
    !-----------------------------------------------------------------------
    !     Returns the probability that a random variable distributed
    !     according to the Normal distribution with zero mean and unit
    !     variance, is less than or equal to x.
    !
    !     X    - Input . Argument                                 - Real
    !
    !     Fortran  functions called:
    !        ABS
    !
    !     Calculation is based upon the error function, using Chebyshev
    !     approximation over the interval (0, 6.09).
    !
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER   :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: ncdf

   !  Arguments
   REAL(PR), INTENT(in) :: x

   !  Local declarations
   REAL(PR), PARAMETER :: zero=0.0D0, half=0.5D0, one=1.0D0
   REAL(PR), PARAMETER :: b=6.09D0
   REAL(PR), PARAMETER :: epsl=1.0D-15
   REAL(PR), PARAMETER :: sq2=0.70710678118654748D0  ! = 1/sqrt(2)
   INTEGER, PARAMETER :: m=43

   REAL(PR) :: ax, d, dd, sv, y, y2, z
   INTEGER :: j
   !  Chebyshev coefficients for the error function:
   REAL(PR) :: c(m)=(/                                          &
                0.1635454272828649D+01,   0.3340345052068802D+00, &
               -0.2550158252490571D+00, 0.1576322031081386D+00, &
               -0.7292374105725899D-01, 0.1844981586051652D-01, &
                0.5451830906274930D-02,-0.9378172345987997D-02, &
                0.5461774234132427D-02,-0.1369434835987556D-02, &
               -0.4791673249620534D-03, 0.6448576838517482D-03, &
               -0.2787148092871001D-03, 0.1375372712760661D-04, &
                0.5421909937317043D-04,-0.3178226588449582D-04, &
                0.4982720716690025D-05, 0.3910358552338792D-05, &
               -0.2794516331451236D-05, 0.5298695171317318D-06, &
                0.2745633118680364D-06,-0.2071635016913643D-06, &
                0.3687897944251272D-07, 0.1967578278235985D-07, &
               -0.1324655993569593D-07, 0.1759666490397310D-08, &
                0.1371495927659758D-08,-0.7222588259009134D-09, &
                0.4293513190125905D-10, 0.8604672660411214D-10, &
               -0.3240778980162313D-10,-0.1531203125775596D-11, &
                0.4585991808162509D-11,-0.1112803317375455D-11, &
               -0.2501311672927168D-12, 0.1996065316425628D-12, &
               -0.2333998072735281D-13,-0.1670821608490672D-13, &
                0.6679068635371678D-14, 0.2100528246172614D-15, &
               -0.7805650505799361D-15,-0.2386236005000803D-15, &
               -0.1792153097738274D-15 /)

   !--------------------------------------------------------------------

   if ( abs(x) < epsl ) then
      ncdf = half
      return
   end if

   z  = x * sq2
   ax = abs( z )
   if ( ax < b ) then
      d  = zero
      dd = zero
      y  = (ax+ax-b)/b
      y2 = y + y
      do j = m, 2, -1
         sv = d
         d  = y2*d - dd + c(j)
         dd = sv
      end do
      ncdf = half + half*(y*d-dd+half*c(1))
   else
      ncdf = one
   end if
   if ( x < zero ) ncdf = one - ncdf

END FUNCTION ncdf
