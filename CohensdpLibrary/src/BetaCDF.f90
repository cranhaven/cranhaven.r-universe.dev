FUNCTION betacdf( x, p, q, ier )
    !-----------------------------------------------------------------------
    !     Returns the incomplete beta function
    !
    !     X     - Input . Value of the variable (0 <= X <= 1)     - Real
    !     P     - Input . First parameter  (P > 0)                - Real
    !     Q     - Input . Second parameter (Q > 0)                - Real
    !     IER   - Output. Return code :                           - Integer
    !                     0 = normal
    !                     1 = invalid input argument
    !                     3 = result out of limits
    !                         (betacdf < 0 or betacdf > 1)
    !
    !     Uses subroutine bratio:
    !     Armido DiDinato, Alfred Morris.
    !     Algorithm 708: Significant Digit Computation of the
    !     Incomplete Beta Function Ratios.
    !     ACM Transactions on Mathematical Software,
    !     Volume 18, Number 3, September 1993, pages 360-373.
    !-----------------------------------------------------------------------

   IMPLICIT NONE
   INTEGER, PARAMETER        :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: betacdf

   !  Arguments
   REAL(PR), INTENT(in) :: x, p, q
   INTEGER, INTENT(out) :: ier

   !  Local declarations
   REAL(PR), PARAMETER :: zero=0.0D0, one=1.0D0
   REAL(PR) :: w, w1, y

   !--------------------------------------------------------------------
   !  Test for valid input arguments
   if ( p <= zero .or. q <= zero ) then
      ier  = 1
      betacdf = -one
      return
   else if ( x < zero ) then
      ier  = 1
      betacdf = zero
      return
   else if ( x > one ) then
      ier  = 1
      betacdf = one
      return
   end if

   y = one - x
   call bratio( p, q, x, y, w, w1, ier )
   betacdf = w

   !  Final check
   if ( ier /= 0 ) then
      ier = 1
   else if ( betacdf < zero .or. betacdf > one ) then
      ier = 3
   end if

END FUNCTION betacdf


subroutine bratio( a, b, x, y, w, w1, ierr )
    !*****************************************************************************80
    !
    !! BRATIO evaluates the incomplete beta function Ix(A,B).
    !
    !  Discussion:
    !
    !    It is assumed that X <= 1
    !    and Y = 1 - X.  BRATIO assigns W and W1 the values
    !
    !                      W  = ix(a,b)
    !                      W1 = 1 - ix(a,b)
    !
    !     ierr is a variable that reports the status of the results.
    !     if no input errors are detected then ierr is set to 0 and
    !     w and w1 are computed. otherwise, if an error is detected,
    !     then w and w1 are assigned the value 0 and ierr is set to
    !     one of the following values ...
    !
    !        ierr = 1  if a or b is negative
    !        ierr = 2  if a = b = 0
    !        ierr = 3  if x < 0 or x .gt. 1
    !        ierr = 4  if y < 0 or y .gt. 1
    !        ierr = 5  if x + y /= 1
    !        ierr = 6  if x = a = 0
    !        ierr = 7  if y = b = 0
    !
    !  Modified:
    !
    !    17 May 2007
    !
    !  Author:
    !
    !    Armido DiDinato, Alfred Morris
    !
    !  Reference:
    !
    !    Armido DiDinato, Alfred Morris,
    !    Algorithm 708:
    !    Significant Digit Computation of the
    !    Incomplete Beta Function Ratios,
    !    ACM Transactions on Mathematical Software,
    !    Volume 18, Number 3, September 1993, pages 360-373.
    !
    !  Parameters:
    !
    !    Input, real A, B, the parameters of the function.  A and B should
    !    be nonnegative.
    !
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0

  real(PR) a
  real(PR) a0
  real(PR) apser
  real(PR) b
  real(PR) b0
  real(PR) basym
  real(PR) bfrac
  real(PR) bpser
  real(PR) bup
  real(PR) eps
  real(PR) fpser
  integer ierr
  integer ierr1
  integer ind
  real(PR) lambda
  integer n
  real(PR) t
  real(PR) w
  real(PR) w1
  real(PR) x
  real(PR) x0
  real(PR) y
  real(PR) y0
  real(PR) z

  eps = epsilon ( one )

  w = zero
  w1 = zero
  if (a < zero .or. b < zero) go to 300
  if (a == zero .and. b == zero) go to 310
  if (x < zero .or. x .gt. one) go to 320
      if (y < zero .or. y .gt. one) go to 330

  z = ((x + y) - half) - half

  if ( abs(z) .gt. 3.0D0*eps) then
    ierr = 5
    return
  end if

  ierr = 0

  if (x == zero) go to 200
  if (y == zero) go to 210
  if (a == zero) go to 211
  if (b == zero) go to 201

      eps = max ( eps, 1.D-15 )
      if ( max ( a, b ) < 1.D-3*eps) go to 230

      ind = 0
      a0 = a
      b0 = b
      x0 = x
      y0 = y
      if ( min ( a0, b0 ) .gt. one) go to 30
!
!  procedure for a0 <= 1 or b0 <= 1
!
      if (x <= half) go to 10
      ind = 1
      a0 = b
      b0 = a
      x0 = y
      y0 = x

   10 if (b0 < min ( eps, eps * a0 ) ) go to 80
      if (a0 < min ( eps, eps * b0 ) .and. b0*x0 <= one) go to 90
      if ( max ( a0, b0 ) .gt. one) go to 20
      if (a0 .ge. min ( 0.2D0, b0 ) ) go to 100
      if (x0**a0 <= 0.9D0) go to 100
      if (x0 .ge. 0.3D0) go to 110
      n = 20
      go to 130

   20 if (b0 <= one) go to 100
      if (x0 .ge. 0.3D0) go to 110
      if (x0 .ge. 0.1D0) go to 21
      if ((x0*b0)**a0 <= 0.7D0) go to 100
   21 if (b0 .gt. 15.0D0) go to 131
      n = 20
      go to 130
!
!  procedure for a0 .gt. 1 and b0 .gt. 1
!
   30 if (a .gt. b) go to 31
         lambda = a - (a + b)*x
         go to 32
   31 lambda = (a + b)*y - b

   32 if (lambda .ge. zero) go to 40
      ind = 1
      a0 = b
      b0 = a
      x0 = y
      y0 = x
      lambda = abs(lambda)

   40 if (b0 < 40.0D0 .and. b0*x0 <= 0.7D0) go to 100
      if (b0 < 40.0D0) go to 140
      if (a0 .gt. b0) go to 50
         if (a0 <= 100.0D0) go to 120
         if (lambda .gt. 0.03D0*a0) go to 120
         go to 180
   50 if (b0 <= 100.0D0) go to 120
      if (lambda .gt. 0.03D0*b0) go to 120
      go to 180
!
!  evaluation of the appropriate algorithm
!
   80 continue
      w = fpser(a0, b0, x0, eps)
      w1 = half + (half - w)
      go to 220

   90 continue
      w1 = apser(a0, b0, x0, eps)
      w = half + (half - w1)
      go to 220

  100 continue
      w = bpser(a0, b0, x0, eps)
      w1 = half + (half - w)
      go to 220

  110 continue
      w1 = bpser(b0, a0, y0, eps)
      w = half + (half - w1)
      go to 220

  120 continue
      w = bfrac(a0, b0, x0, y0, lambda, 15.0D0*eps)
      w1 = half + (half - w)
      go to 220

  130 continue
      w1 = bup(b0, a0, y0, x0, n, eps)
      b0 = b0 + n
  131 call bgrat ( b0, a0, y0, x0, w1, 15.0D0*eps, ierr1 )
      w = half + (half - w1)
      go to 220

  140 n = INT(b0)
      b0 = b0 - n

      if (b0 == zero) then
         n = n - 1
         b0 = one
      end if

      w = bup(b0, a0, y0, x0, n, eps)
      if (x0 .gt. 0.7D0) go to 150
      w = w + bpser(a0, b0, x0, eps)
      w1 = half + (half - w)
      go to 220

  150 if ( 15.0D0 < a0 ) go to 151
         n = 20
         w = w + bup(a0, b0, x0, y0, n, eps)
         a0 = a0 + n
  151 call bgrat ( a0, b0, x0, y0, w, 15.0D0*eps, ierr1 )
      w1 = half + (half - w)
      go to 220

  180 w = basym(a0, b0, lambda, 100.0D0*eps)
      w1 = half + (half - w)
      go to 220
!
!  termination of the procedure
!
  200 if (a == zero) go to 350
  201 w = zero
      w1 = one
      return

  210 if (b == zero) go to 360
  211 w = one
      w1 = zero
      return

  220 if (ind == 0) return
      t = w
      w = w1
      w1 = t
      return
!
!  procedure for a and b < 1.e-3*eps
!
  230 w = b/(a + b)
      w1 = a/(a + b)
      return
!
!  Error return
!
  300 ierr = 1
      return
  310 ierr = 2
      return
  320 ierr = 3
      return
  330 ierr = 4
      return

  350 ierr = 6
      return
  360 ierr = 7

  return
end subroutine bratio


function algdiv ( a, b )
    !*****************************************************************************80
    !
    !! ALGDIV computes ln(gamma(b)/gamma(a+b)) when 8 <= B.
    !
    !  Discussion:
    !
    !    In this algorithm, del(x) is the function defined by
    !    ln(gamma(x)) = (x - 0.5)*ln(x) - x + 0.5*ln(2*pi) + del(x).
    !
    !  Modified:
    !
    !    17 May 2007
    !
    !  Author:
    !
    !    Armido DiDinato, Alfred Morris
    !
    !  Reference:
    !
    !    Armido DiDinato, Alfred Morris,
    !    Algorithm 708:
    !    Significant Digit Computation of the
    !    Incomplete Beta Function Ratios,
    !    ACM Transactions on Mathematical Software,
    !    Volume 18, Number 3, September 1993, pages 360-373.
    !
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: half=0.5D0, one=1.0D0

  real(PR) a
  real(PR) algdiv
  real(PR) alnrel
  real(PR) b
  real(PR) c
  real(PR), parameter :: c0 = 0.833333333333333D-01
  real(PR), parameter :: c1 = -0.277777777760991D-02
  real(PR), parameter :: c2 = 0.793650666825390D-03
  real(PR), parameter :: c3 = -0.595202931351870D-03
  real(PR), parameter :: c4 = 0.837308034031215D-03
  real(PR), parameter :: c5 = -0.165322962780713D-02
  real(PR) d
  real(PR) h
  real(PR) s11
  real(PR) s3
  real(PR) s5
  real(PR) s7
  real(PR) s9
  real(PR) t
  real(PR) u
  real(PR) v
  real(PR) w
  real(PR) x
  real(PR) x2

  if ( b < a ) then
    h = b / a
    c = one / (one + h)
    x = h / (one + h)
    d = a + (b - half)
  else
    h = a / b
    c = h / ( one + h )
    x = one / ( one + h )
    d = b + ( a - half )
  end if
!
!  set sn = (1 - x**n) / ( 1 - x )
!
  x2 = x * x
  s3 = one + ( x + x2)
  s5 = one + ( x + x2 * s3 )
  s7 = one + ( x + x2 * s5 )
  s9 = one + ( x + x2 * s7 )
  s11 = one + ( x + x2 * s9 )
!
!  Set w = del(b) - del(a + b)
!
  t = (one/b)**2
  w = ((((c5*s11*t + c4*s9)*t + c3*s7)*t + c2*s5)*t + c1*s3)*t + c0
  w = w * ( c / b )
!
!  Combine the results.
!
  u = d * alnrel ( a / b )
  v = a*( log ( b ) - one )

  if ( v < u ) then
    algdiv = ( w - v ) - u
  else
    algdiv = ( w - u ) - v
  end if

  return
end
function alnrel ( a )

!*****************************************************************************80
!
!! ALNREL evaluates the function ln(1 + a).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: onedble=1.0D0
  real(PR), parameter :: one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) alnrel
  real(PR), parameter :: p1 = -0.129418923021993D+01
  real(PR), parameter :: p2 = 0.405303492862024D0
  real(PR), parameter :: p3 = -0.178874546012214D-01
  real(PR), parameter :: q1 = -0.162752256355323D+01
  real(PR), parameter :: q2 = 0.747811014037616D0
  real(PR), parameter :: q3 = -0.845104217945565D-01
  real(PR) t
  real(PR) t2
  real(PR) w
  real(PR) x

  if ( abs ( a ) <= 0.375D0 ) then

    t = a / (a + two)
    t2 = t * t
    w = (((p3*t2 + p2)*t2 + p1)*t2 + one) / &
        (((q3*t2 + q2)*t2 + q1)*t2 + one)
    alnrel = two * t * w

  else

    x = real ( onedble + a, kind(PR) )
    alnrel = log ( x )

  end if

  return
end
function apser ( a, b, x, eps )

!*****************************************************************************80
!
!! APSER yields the incomplete beta ratio i(sub(1-x))(b,a) for
!     a <= min(eps,eps*b), b*x <= 1, and x <= 0.5. used when
!     a is very small. use only if above inequalities are satisfied.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, one=1.0D0

  real(PR) a
  real(PR) aj
  real(PR) apser
  real(PR) b
  real(PR) bx
  real(PR) c
  real(PR) eps
  real(PR), parameter :: g = 0.577215664901533D0
  real(PR) j
  real(PR) psi
  real(PR) s
  real(PR) t
  real(PR) tol
  real(PR) x

  bx = b * x
  t = x - bx

  if ( b * eps <= 2.D-2 ) then
    c = log(x) + psi(b) + g + t
  else
    c = log(bx) + g + t
  end if

  tol = 5.0D0*eps*abs(c)
  j = one
  s = zero

  do

    j = j + one
    t = t*(x - bx/j)
    aj = t/j
    s = s + aj

    if ( abs ( aj ) <= tol ) then
      exit
    end if

  end do

  apser = -a*(c + s)

  return
end
function basym ( a, b, lambda, eps )

!*****************************************************************************80
!
!! BASYM uses an asymptotic expansion for Ix(A,B) for large A and B.
!
!     lambda = (a + b)*y - b  and eps is the tolerance used.
!     it is assumed that lambda is nonnegative and that
!     a and b are greater than or equal to 15.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) a0(21)
  real(PR) b
  real(PR) b0(21)
  real(PR) bcorr
  real(PR) basym
  real(PR) bsum
  real(PR) c(21)
  real(PR) d(21)
  real(PR) dsum
  real(PR), parameter :: e0 = 1.12837916709551D0
  real(PR), parameter :: e1 = 0.353553390593274D0
  real(PR) eps
!!real(PR) erfc1
  real(PR) f
  real(PR) h
  real(PR) h2
  real(PR) hn
  integer i
  integer im1
  integer imj
  integer j
  real(PR) j0
  real(PR) j1
  real(PR) lambda
  integer m
  integer mm1
  integer mmj
  integer n
  integer np1
  integer, parameter :: num = 20
  real(PR) r
  real(PR) r0
  real(PR) r1
  real(PR) rlog1
  real(PR) s
  real(PR) sum2
  real(PR) t
  real(PR) t0
  real(PR) t1
  real(PR) u
  real(PR) w
  real(PR) w0
  real(PR) z
  real(PR) z0
  real(PR) z2
  real(PR) zn
  real(PR) znm1
!
!  num is the maximum value that n can take in the do loop
!  ending at statement 50. it is required that num be even.
!  the arrays a0, b0, c, d have dimension num + 1.
!
!------------------------
!     e0 = 2/sqrt(pi)
!     e1 = 2**(-3/2)
!------------------------

  basym = zero

  if ( a < b ) then
    h = a/b
    r0 = one/(one + h)
    r1 = (b - a)/b
    w0 = one/sqrt(a*(one + h))
  else
    h = b/a
    r0 = one/(one + h)
    r1 = (b - a)/a
    w0 = one/sqrt(b*(one + h))
  end if

  f = a*rlog1(-lambda/a) + b*rlog1(lambda/b)
  t = exp(-f)

  if ( t == zero ) then
    return
  end if

  z0 = sqrt(f)
  z = half*(z0/e1)
  z2 = f + f

  a0(1) = (two/3.0D0)*r1
  c(1) = - half*a0(1)
  d(1) = - c(1)
  j0 = (half/e0)*exp(z0*z0)*erfc(z0)
  j1 = e1
  sum2 = j0 + d(1)*w0*j1

  s = one
  h2 = h*h
  hn = one
  w = w0
  znm1 = z
  zn = z2

  do n = 2, num, 2

    hn = h2*hn
    a0(n) = two*r0*(one + h*hn)/(n + two)
    np1 = n + 1
    s = s + hn
    a0(np1) = two*r1*s/(n + 3.0D0)

    do i = n, np1

      r = -half*(i + one)
      b0(1) = r*a0(1)

      do m = 2, i

        bsum = zero
        mm1 = m - 1

        do j = 1, mm1
          mmj = m - j
          bsum = bsum + (j*r - mmj)*a0(j)*b0(mmj)
        end do

        b0(m) = r*a0(m) + bsum/m

      end do

      c(i) = b0(i)/(i + one)

      dsum = zero
      im1 = i - 1

      do j = 1, im1
        imj = i - j
        dsum = dsum + d(imj)*c(j)
      end do

      d(i) = -(dsum + c(i))

    end do

    j0 = e1*znm1 + (n - one)*j0
    j1 = e1*zn + n*j1
    znm1 = z2*znm1
    zn = z2*zn
    w = w0*w
    t0 = d(n)*w*j0
    w = w0*w
    t1 = d(np1)*w*j1
    sum2 = sum2 + (t0 + t1)

    if ((abs(t0) + abs(t1)) <= eps * sum2 ) then
      exit
    end if

  end do

  u = exp ( -bcorr(a,b) )
  basym = e0 * t * u * sum2

  return
end
function bcorr ( a0, b0 )

!*****************************************************************************80
!
!! BCORR evaluates  del(a0) + del(b0) - del(a0 + b0)  where
!     ln(gamma(a)) = (a - 0.5)*ln(a) - a + 0.5*ln(2*pi) + del(a).
!     it is assumed that a0 .ge. 8 and b0 .ge. 8.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: one=1.0D0

  real(PR) a
  real(PR) a0
  real(PR) b
  real(PR) b0
  real(PR) bcorr
  real(PR) c
  real(PR), parameter :: c0 = 0.833333333333333D-01
  real(PR), parameter :: c1 = -0.277777777760991D-02
  real(PR), parameter :: c2 = 0.793650666825390D-03
  real(PR), parameter :: c3 = -0.595202931351870D-03
  real(PR), parameter :: c4 = 0.837308034031215D-03
  real(PR), parameter :: c5 = -0.165322962780713D-02
  real(PR) h
  real(PR) s11
  real(PR) s3
  real(PR) s5
  real(PR) s7
  real(PR) s9
  real(PR) t
  real(PR) w
  real(PR) x
  real(PR) x2

  a = min ( a0, b0 )
  b = max ( a0, b0 )
  h = a/b
  c = h/(one + h)
  x = one/(one + h)
  x2 = x*x
!
!  Set sn = (1 - x**n)/(1 - x)
!
  s3 = one + (x + x2)
  s5 = one + (x + x2*s3)
  s7 = one + (x + x2*s5)
  s9 = one + (x + x2*s7)
  s11 = one + (x + x2*s9)
!
!  Set w = del(b) - del(a + b)
!
  t = (one/b)**2
  w = ((((c5*s11*t + c4*s9)*t + c3*s7)*t + c2*s5)*t + c1*s3)*t + c0
  w = w*(c/b)
!
!  Compute  del(a) + w
!
  t = (one/a)**2
  bcorr = (((((c5*t + c4)*t + c3)*t + c2)*t + c1)*t + c0)/a + w

  return
end
function betaln ( a0, b0 )

!*****************************************************************************80
!
!! BETALN evaluates the logarithm of the Beta function.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
!  Local Parameters:
!
!    Local, real E, the value of Log ( 2 * PI ) / 2.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) a0
  real(PR) algdiv
  real(PR) alnrel
  real(PR) b
  real(PR) b0
  real(PR) bcorr
  real(PR) betaln
  real(PR) c
  real(PR), parameter :: e = 0.918938533204673D0
!!real(PR) log_gamma
  real(PR) gsumln
  real(PR) h
  integer i
  integer n
  real(PR) u
  real(PR) v
  real(PR) w
  real(PR) z

  a = min ( a0, b0 )
  b = max ( a0, b0 )
  if (a .ge. 8.0D0) go to 60
  if (a .ge. one) go to 20
!
!  a < 1
!
  if ( b < 8.0D0 ) then
    betaln = log_gamma ( a ) + ( log_gamma ( b ) - log_gamma ( a + b ) )
    return
  else
    betaln = log_gamma ( a ) + algdiv ( a, b )
    return
  end if
!
!  procedure when 1 <= a < 8
!
   20 if (a .gt. two) go to 30
      if (b .gt. two) go to 21
         betaln = log_gamma(a) + log_gamma(b) - gsumln(a,b)
         return
   21 w = zero
      if (b < 8.0D0) go to 40
         betaln = log_gamma(a) + algdiv(a,b)
         return
!
! reduction of a when b <= 1000
!
   30 if (b .gt. 1000.0D0) go to 50
      n = INT(a - one)
      w = one
      do i = 1,n
         a = a - one
         h = a/b
         w = w * (h/(one + h))
      end do
      w = log ( w )
      if (b < 8.0D0) go to 40
      betaln = w + log_gamma(a) + algdiv(a,b)
      return
!
!  Reduction of b when b < 8
!
   40 n = INT(b - one)
      z = one
      do i = 1,n
         b = b - one
         z = z * (b/(a + b))
      end do
      betaln = w + log ( z ) + (log_gamma(a) + (log_gamma(b) - gsumln(a,b)))
      return
!
!  reduction of a when b .gt. 1000
!
   50 n = INT(a - one)
      w = one
      do i = 1,n
         a = a - one
         w = w * (a/(one + a/b))
      end do
      betaln = ( log ( w ) - n* log ( b ) ) + (log_gamma(a) + algdiv(a,b))
      return
!
!  procedure when a .ge. 8
!
   60 w = bcorr(a,b)
      h = a/b
      c = h/(one + h)
      u = -(a - half) * log ( c )
      v = b*alnrel(h)
      if (u <= v) go to 61
         betaln = (((-half* log ( b ) + e) + w) - v) - u
         return
   61 betaln = (((-half* log ( b ) + e) + w) - u) - v

  return
end
function bfrac ( a, b, x, y, lambda, eps )

!*****************************************************************************80
!
!! BFRAC uses a continued fraction expansion for ix(a,b) when a,b .gt. 1.
!
!     it is assumed that  lambda = (a + b)*y - b.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) alpha
  real(PR) an
  real(PR) anp1
  real(PR) b
  real(PR) beta
  real(PR) bfrac
  real(PR) bn
  real(PR) bnp1
  real(PR) brcomp
  real(PR) c
  real(PR) c0
  real(PR) c1
  real(PR) e
  real(PR) eps
  real(PR) lambda
  real(PR) n
  real(PR) p
  real(PR) r
  real(PR) r0
  real(PR) s
  real(PR) t
  real(PR) w
  real(PR) x
  real(PR) y
  real(PR) yp1

  bfrac = brcomp ( a, b, x, y )

  if ( bfrac == zero ) then
    return
  end if

  c = one + lambda
  c0 = b / a
  c1 = one + one / a
  yp1 = y + one

  n = zero
  p = one
  s = a + one
  an = zero
  bn = one
  anp1 = one
  bnp1 = c / c1
  r = c1 / c
!
!  Continued fraction calculation.
!
  do

    n = n + one
    t = n / a
    w = n * ( b - n ) * x
    e = a / s
    alpha = ( p * ( p + c0 ) * e * e ) * ( w * x )
    e = ( one + t ) / ( c1 + t + t )
    beta = n + w / s + e * ( c + n * yp1 )
    p = one + t
    s = s + two
!
!  Update AN, BN, ANP1, and BNP1.
!
    t = alpha * an + beta * anp1
    an = anp1
    anp1 = t
    t = alpha * bn + beta * bnp1
    bn = bnp1
    bnp1 = t
    r0 = r
    r = anp1 / bnp1

    if ( abs ( r - r0 ) <= eps * r ) then
      exit
    end if
!
!  Rescale AN, BN, ANP1, and BNP1.
!
    an = an / bnp1
    bn = bn / bnp1
    anp1 = r
    bnp1 = one

  end do
!
!  Termination.
!
  bfrac = bfrac * r

  return
end
subroutine bgrat ( a, b, x, y, w, eps, ierr )

!*****************************************************************************80
!
!! BGRAT uses an asymptotic expansion for Ix(a,b) when A is larger than B.
!
!  Discussion:
!
!    The result of the expansion is added to w.  It is assumed
!    that a .ge. 15 and b <= 1.  eps is the tolerance used.
!    ierr is a variable that reports the status of the results.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) algdiv
  real(PR) alnrel
  real(PR) b
  real(PR) bm1
  real(PR) bp2n
  real(PR) c(30)
  real(PR) cn
  real(PR) coef
  real(PR) d(30)
  real(PR) dj
  real(PR) eps
  real(PR) gam1
  integer i
  integer ierr
  real(PR) j
  real(PR) l
  real(PR) lnx
  integer n
  real(PR) n2
  integer nm1
  real(PR) nu
  real(PR) p
  real(PR) q
  real(PR) r
  real(PR) s
  real(PR) sum1
  real(PR) t
  real(PR) t2
  real(PR) u
  real(PR) v
  real(PR) w
  real(PR) x
  real(PR) y
  real(PR) z

  bm1 = (b - half) - half
  nu = a + half*bm1
  if (y .gt. 0.375D0) go to 10
  lnx = alnrel(-y)
  go to 11
10 continue
  lnx = log ( x )
11 continue
  z = -nu*lnx
  if (b*z == zero) go to 100
!
!  computation of the expansion
!  set r = exp(-z)*z**b/gamma(b)
!
      r = b*(one + gam1(b))*exp ( b * log ( z ) )
      r = r*exp(a*lnx)*exp(half*bm1*lnx)
      u = algdiv(b,a) + b* log ( nu )
      u = r*exp(-u)
      if (u == zero) go to 100
      call grat1(b,z,r,p,q,eps)

      v = 0.25D0*(one/nu)**2
      t2 = 0.25D0*lnx*lnx
      l = w/u
      j = q/r
      sum1 = j
      t = one
      cn = one
      n2 = zero
      do n = 1,30
         bp2n = b + n2
         j = (bp2n*(bp2n + one)*j + (z + bp2n + one)*t)*v
         n2 = n2 + two
         t = t*t2
         cn = cn/(n2*(n2 + one))
         c(n) = cn
         s = zero
         if (n == 1) go to 21
            nm1 = n - 1
            coef = b - n
            do i = 1,nm1
               s = s + coef*c(i)*d(n-i)
               coef = coef + b
            end do
   21    d(n) = bm1*cn + s/n
         dj = d(n)*j
         sum1 = sum1 + dj

         if ( sum1 <= zero ) then
           go to 100
         end if

         if (abs(dj) <= eps*(sum1 + l)) then
           go to 30
         end if

      end do
!
!  Add the results to w
!
   30 ierr = 0
      w = w + u * sum1
      return
!
!  the expansion cannot be computed
!
  100 ierr = 1

  return
end
function bpser ( a, b, x, eps )

!*****************************************************************************80
!
!! BPSER uses the power series expansion for evaluating ix(a,b) when b <= 1
!     or b*x <= 0.7.  eps is the tolerance used.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: onedble=1.0D0
  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0

  real(PR) a
  real(PR) a0
  real(PR) algdiv
  real(PR) apb
  real(PR) b
  real(PR) b0
  real(PR) betaln
  real(PR) bpser
  real(PR) c
  real(PR) eps
  real(PR) gam1
  real(PR) gamln1
  integer i
  integer m
  real(PR) n
  real(PR) sum1
  real(PR) t
  real(PR) tol
  real(PR) u
  real(PR) w
  real(PR) x
  real(PR) z

  bpser = zero
  if ( x == zero ) then
    return
  end if
!
!  compute the factor x**a/(a*beta(a,b))
!
      a0 = min ( a, b )
      if (a0 < one) go to 10
         z = a* log ( x ) - betaln(a,b)
         bpser = exp(z)/a
         go to 70
   10 b0 = max ( a, b )
      if (b0 .ge. 8.0D0) go to 60
      if (b0 .gt. one) go to 40
!
!  procedure for a0 < 1 and b0 <= 1
!
      bpser = x**a
      if (bpser == zero) return

      apb = a + b
      if (apb .gt. one) go to 20
         z = one + gam1(apb)
         go to 30
   20 u = real(a + b - onedble, kind(PR)) 
      z = (one + gam1(u))/apb

   30 c = (one + gam1(a))*(one + gam1(b))/z
      bpser = bpser*c*(b/apb)
      go to 70
!
!  procedure for a0 < 1 and 1 < b0 < 8
!
   40 u = gamln1(a0)
      m = INT(b0 - one)
      if (m < 1) go to 50
      c = one
      do i = 1,m
         b0 = b0 - one
         c = c*(b0/(a0 + b0))
      end do
      u = log ( c ) + u

   50 z = a* log ( x ) - u
      b0 = b0 - one
      apb = a0 + b0
      if (apb .gt. one) go to 51
         t = one + gam1(apb)
         go to 52
   51 u = real(a0 + b0 - onedble, kind(PR)) 
      t = (one + gam1(u))/apb
   52 bpser = exp(z)*(a0/a)*(one + gam1(b0))/t
      go to 70
!
!  procedure for a0 < 1 and b0 .ge. 8
!
   60 u = gamln1(a0) + algdiv(a0,b0)
      z = a* log ( x ) - u
      bpser = (a0/a)*exp(z)
   70 if (bpser == zero .or. a <= 0.1D0*eps) return
!
!  Compute the series
!
  sum1 = zero
  n = zero
  c = one
  tol = eps/a

  do
    n = n + one
    c = c*(half + (half - b/n))*x
    w = c/(a + n)
    sum1 = sum1 + w
    if ( abs ( w ) <= tol ) then
      exit
    end if
  end do

  bpser = bpser*(one + a*sum1)

  return
end
function brcmp1 ( mu, a, b, x, y )

!*****************************************************************************80
!
!! BRCMP1 evaluates exp(mu) * (x**a*y**b/beta(a,b)).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: onedble=1.0D0
  real(PR), parameter :: zero=0.0D0, one=1.0D0

  real(PR) a
  real(PR) a0
  real(PR) algdiv
  real(PR) alnrel
  real(PR) apb
  real(PR) b
  real(PR) b0
  real(PR) bcorr
  real(PR) betaln
  real(PR) brcmp1
  real(PR) c
  real(PR), parameter :: const = 0.398942280401433D0
  real(PR) e
  real(PR) esum
  real(PR) gam1
  real(PR) gamln1
  real(PR) h
  integer i
  real(PR) lambda
  real(PR) lnx
  real(PR) lny
  integer mu
  integer n
  real(PR) rlog1
  real(PR) t
  real(PR) u
  real(PR) v
  real(PR) x
  real(PR) x0
  real(PR) y
  real(PR) y0
  real(PR) z

  a0 = min ( a, b )
      if (a0 .ge. 8.0D0) go to 100

      if (x .gt. 0.375D0) go to 10
         lnx = log ( x )
         lny = alnrel(-x)
         go to 20
   10 if (y .gt. 0.375D0) go to 11
         lnx = alnrel(-y)
         lny = log ( y )
         go to 20
   11 lnx = log ( x )
      lny = log ( y )

   20 z = a*lnx + b*lny
      if (a0 < one) go to 30
      z = z - betaln(a,b)
      brcmp1 = esum(mu,z)
      return
!-----------------------------------------------------------------------
!  procedure for a < 1 or b < 1
!-----------------------------------------------------------------------
   30 b0 = max ( a, b )
      if (b0 .ge. 8.0D0) go to 80
      if (b0 .gt. one) go to 60
!
!  algorithm for b0 <= 1
!
      brcmp1 = esum(mu,z)
      if (brcmp1 == zero) return

      apb = a + b
      if ( one < apb ) go to 40
         z = one + gam1(apb)
         go to 50
   40 u = real(a + b - onedble, kind(PR)) 
      z = (one + gam1(u))/apb

   50 c = (one + gam1(a))*(one + gam1(b))/z
      brcmp1 = brcmp1*(a0*c)/(one + a0/b0)
      return
!
!  algorithm for 1 < b0 < 8
!
   60 u = gamln1(a0)
      n = INT(b0 - one)
      if (n < 1) go to 70
      c = one
      do 61 i = 1,n
         b0 = b0 - one
         c = c*(b0/(a0 + b0))
   61 continue
      u = log ( c ) + u

   70 z = z - u
      b0 = b0 - one
      apb = a0 + b0
      if (apb .gt. one) go to 71
         t = one + gam1(apb)
         go to 72
   71 u = real(a0 + b0 - onedble, kind(PR)) 
      t = (one + gam1(u))/apb
   72 brcmp1 = a0*esum(mu,z)*(one + gam1(b0))/t
      return
!
!  algorithm for b0 .ge. 8
!
   80 u = gamln1(a0) + algdiv(a0,b0)
      brcmp1 = a0*esum(mu,z - u)
      return
!-----------------------------------------------------------------------
!  procedure for a .ge. 8 and b .ge. 8
!-----------------------------------------------------------------------
  100 if (a .gt. b) go to 101
         h = a/b
         x0 = h/(one + h)
         y0 = one/(one + h)
         lambda = a - (a + b)*x
         go to 110
  101 h = b/a
      x0 = one/(one + h)
      y0 = h/(one + h)
      lambda = (a + b)*y - b

  110 e = -lambda/a
      if (abs(e) .gt. 0.6D0) go to 111
         u = rlog1(e)
         go to 120
  111 u = e - log ( x / x0 )

  120 e = lambda/b
      if (abs(e) .gt. 0.6D0) go to 121
         v = rlog1(e)
         go to 130
  121 v = e - log ( y / y0 )

  130 z = esum(mu,-(a*u + b*v))
      brcmp1 = const*sqrt(b*x0)*z*exp(-bcorr(a,b))

  return
end
function brcomp ( a, b, x, y )

!*****************************************************************************80
!
!! BRCOMP evaluates X**a * y**b / beta(a,b).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: onedble=1.0D0
  real(PR), parameter :: zero=0.0D0, one=1.0D0

  real(PR) a
  real(PR) a0
  real(PR) algdiv
  real(PR) alnrel
  real(PR) apb
  real(PR) b
  real(PR) b0
  real(PR) bcorr
  real(PR) betaln
  real(PR) brcomp
  real(PR) c
  real(PR), parameter :: const = 0.398942280401433D0
  real(PR) e
  real(PR) gam1
  real(PR) gamln1
  real(PR) h
  integer i
  real(PR) lambda
  real(PR) lnx
  real(PR) lny
  integer n
  real(PR) rlog1
  real(PR) t
  real(PR) u
  real(PR) v
  real(PR) x
  real(PR) x0
  real(PR) y
  real(PR) y0
  real(PR) z

  brcomp = zero

  if ( x == zero .or. y == zero ) then
    return
  end if

  a0 = min ( a, b )

      if ( 8.0 <= a0 ) go to 100

      if (x .gt. 0.375D0) go to 10
         lnx = log ( x )
         lny = alnrel(-x)
         go to 20
   10 if (y .gt. 0.375D0) go to 11
         lnx = alnrel(-y)
         lny = log ( y )
         go to 20
   11 lnx = log ( x )
      lny = log ( y )

   20 z = a*lnx + b*lny

      if ( one <= a ) then
        z = z - betaln(a,b)
        brcomp = exp(z)
        return
      end if
!-----------------------------------------------------------------------
!  procedure for a < 1 or b < 1
!-----------------------------------------------------------------------
      b0 = max ( a, b )
      if (b0 .ge. 8.0D0) go to 80
      if ( one < b0 ) go to 60
!
!  algorithm for b0 <= 1
!
      brcomp = exp(z)
      if (brcomp == zero) return

      apb = a + b
      if (apb .gt. one) go to 40
         z = one + gam1(apb)
         go to 50
   40 u = real(a + b - onedble, kind(PR)) 
      z = (one + gam1(u))/apb

   50 c = (one + gam1(a))*(one + gam1(b))/z
      brcomp = brcomp*(a0*c)/(one + a0/b0)
      return
!
!  algorithm for 1 < b0 < 8
!
   60 u = gamln1(a0)
      n = INT(b0 - one)
      if (n < 1) go to 70
      c = one
      do i = 1,n
         b0 = b0 - one
         c = c*(b0/(a0 + b0))
      end do
      u = log ( c ) + u

   70 z = z - u
      b0 = b0 - one
      apb = a0 + b0
      if (apb .gt. one) go to 71
         t = one + gam1(apb)
         go to 72
   71 u = real(a0 + b0 - onedble, kind(PR)) 
      t = (one + gam1(u))/apb
   72 brcomp = a0*exp(z)*(one + gam1(b0))/t
      return
!
!  algorithm for b0 .ge. 8
!
   80 u = gamln1(a0) + algdiv(a0,b0)
      brcomp = a0*exp(z - u)
      return
!-----------------------------------------------------------------------
!  procedure for a .ge. 8 and b .ge. 8
!-----------------------------------------------------------------------
  100 if (a .gt. b) go to 101
         h = a/b
         x0 = h/(one + h)
         y0 = one/(one + h)
         lambda = a - (a + b)*x
         go to 110
  101 h = b/a
      x0 = one/(one + h)
      y0 = h/(one + h)
      lambda = (a + b)*y - b

  110 e = -lambda/a
      if (abs(e) .gt. 0.6D0) go to 111
         u = rlog1(e)
         go to 120
  111 u = e - log ( x / x0 )

  120 e = lambda/b
      if (abs(e) .gt. 0.6D0) go to 121
         v = rlog1(e)
         go to 130
  121 v = e - log ( y / y0 )

  130 z = exp(-(a*u + b*v))
      brcomp = const*sqrt(b*x0)*z*exp(-bcorr(a,b))

  return
end
function bup ( a, b, x, y, n, eps )

!*****************************************************************************80
!
!! BUP evaluates ix(a,b) - ix(a+n,b) where n is a positive integer.
!
!     eps is the tolerance used.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, one=1.0D0

  real(PR) a
  real(PR) ap1
  real(PR) apb
  real(PR) b
  real(PR) brcmp1
  real(PR) bup
  real(PR) d
  real(PR) eps
  real(PR) exparg
  integer i
  integer k
  integer kp1
  real(PR) l
  integer mu
  integer n
  integer nm1
  real(PR) r
  real(PR) t
  real(PR) w
  real(PR) x
  real(PR) y
!
!  obtain the scaling factor exp(-mu) and
!  exp(mu)*(x**a*y**b/beta(a,b))/a
!
  apb = a + b
  ap1 = a + one
  mu = 0
  d = one
  if (n == 1 .or. a < one) go to 10
      if (apb < 1.1D0*ap1) go to 10
         mu = INT(abs(exparg(1)))
         k = INT(exparg(0))
         if (k < mu) mu = k
         t = mu
         d = exp(-t)

10 continue

  bup = brcmp1(mu,a,b,x,y)/a
      if (n == 1 .or. bup == zero) return
      nm1 = n - 1
      w = d
!
!  let k be the index of the maximum term
!
      k = 0
      if (b <= one) go to 40
      if ( 1.D-4 < y ) go to 20
         k = nm1
         go to 30
   20 r = (b - one)*x/y - a
      if (r < one) go to 40
      k = nm1
      t = nm1
      if (r < t) k = INT(r)
!
!  add the increasing terms of the series
!
   30 do i = 1,k
         l = i - 1
         d = ((apb + l)/(ap1 + l))*x*d
         w = w + d
      end do
      if (k == nm1) go to 50
!
!  add the remaining terms of the series
!
   40 kp1 = k + 1
      do i = kp1,nm1
         l = i - 1
         d = ((apb + l)/(ap1 + l))*x*d
         w = w + d
         if (d <= eps*w) go to 50
      end do
!
!  terminate the procedure
!
   50 bup = bup*w

  return
end
function esum ( mu, x )

!*****************************************************************************80
!
!! ESUM evaluates exp(mu + x).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0

  real(PR) esum
  integer mu
  real(PR) w
  real(PR) x

  if (x .gt. zero) go to 10

      if (mu < 0) go to 20
         w = mu + x
         if (w .gt. zero) go to 20
         esum = exp(w)
         return

   10 if (mu .gt. 0) go to 20
         w = mu + x
         if (w < zero) go to 20
         esum = exp(w)
         return

   20 w = mu

  esum = exp(w)*exp(x)

  return
end
function exparg ( l )

!*****************************************************************************80
!
!! EXPARG reports the largest safe arguments for EXP(X).
!
!    if l = 0 then  exparg(l) = the largest positive w for which
!    exp(w) can be computed.
!
!    if l is nonzero then  exparg(l) = the largest negative w for
!    which the computed value of exp(w) is nonzero.
!
!    Only an approximate value for exparg(l) is needed.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR) exparg
  integer l

   if (l == 0) then
      exparg =  706.893D0   ! log(huge(real*8)), approx.
   else
      exparg = -706.893D0   ! log(tiny(real*8)), approx.
   end if

end
function fpser ( a, b, x, eps )

!*****************************************************************************80
!
!! FPSER evaluates Ix(A,B) for small B and X <= 0.5.
!
!          for b < min(eps,eps*a) and x <= 0.5.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, one=1.0D0

  real(PR) a
  real(PR) an
  real(PR) b
  real(PR) c
  real(PR) eps
  real(PR) exparg
  real(PR) fpser
  real(PR) s
  real(PR) t
  real(PR) tol
  real(PR) x
!
!  Set FPSER = X**A.
!
  fpser = one

  if ( 1.0D-03 * eps < a ) then
    fpser = zero
    t = a * log ( x )
    if ( t < exparg ( 1 ) ) then
      return
    end if
    fpser = exp ( t )
  end if
!
!  Note that 1/b(a,b) = b
!
  fpser = ( b / a ) * fpser
  tol = eps/a
  an = a + one
  t = x
  s = t/an

  do

    an = an + one
    t = x * t
    c = t / an
    s = s + c

    if ( abs ( c ) <= tol ) then
      exit
    end if

  end do

  fpser = fpser * ( one + a * s )

  return
end
function gam1 ( a )

!*****************************************************************************80
!
!! GAM1 computes 1/gamma(a+1) - 1  for -0.5 <= a <= 1.5
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0

  real(PR) a
  real(PR) bot
  real(PR) d
  real(PR) gam1
  real(PR) p(7)
  real(PR) q(5)
  real(PR) r(9)
  real(PR) s1
  real(PR) s2
  real(PR) t
  real(PR) top
  real(PR) w

      data p(1)/ .577215664901533D0/, p(2)/-.409078193005776D0/, &
           p(3)/-.230975380857675D0/, p(4)/ .597275330452234D-01/, &
           p(5)/ .766968181649490D-02/, p(6)/-.514889771323592D-02/, &
           p(7)/ .589597428611429D-03/

      data q(1)/ .100000000000000D+01/, q(2)/ .427569613095214D0/, &
           q(3)/ .158451672430138D0/, q(4)/ .261132021441447D-01/, &
           q(5)/ .423244297896961D-02/

      data r(1)/-.422784335098468D0/, r(2)/-.771330383816272D0/, &
           r(3)/-.244757765222226D0/, r(4)/ .118378989872749D0/, &
           r(5)/ .930357293360349D-03/, r(6)/-.118290993445146D-01/, &
           r(7)/ .223047661158249D-02/, r(8)/ .266505979058923D-03/, &
           r(9)/-.132674909766242D-03/

      data s1  / .273076135303957D0/, s2    / .559398236957378D-01/

      t = a
      d = a - half
      if (d .gt. zero) t = d - half
      if (t .lt. 0) goto 30
      if (t .eq. 0) goto 10
      if (t .gt. 0) goto 20

   10 gam1 = zero
      return

   20 top = (((((p(7)*t + p(6))*t + p(5))*t + p(4))*t + p(3))*t &
                        + p(2))*t + p(1)
      bot = (((q(5)*t + q(4))*t + q(3))*t + q(2))*t + one
      w = top/bot
      if (d .gt. zero) go to 21
         gam1 = a*w
         return
   21 gam1 = (t/a)*((w - half) - half)
      return

   30 top = (((((((r(9)*t + r(8))*t + r(7))*t + r(6))*t + r(5))*t &
                          + r(4))*t + r(3))*t + r(2))*t + r(1)
      bot = (s2*t + s1)*t + one
      w = top/bot
      if ( zero < d ) go to 31
         gam1 = a*((w + half) + half)
         return
   31 gam1 = t*w/a

  return
end
function gamln1 ( a )

!*****************************************************************************80
!
!! GAMLN1 evaluates ln(gamma(1 + a)) for -0.2 <= A <= 1.25
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: half=0.5D0, one=1.0D0

  real(PR) a
  real(PR) gamln1
  real(PR) p0
  real(PR) p1
  real(PR) p2
  real(PR) p3
  real(PR) p4
  real(PR) p5
  real(PR) p6
  real(PR) q1
  real(PR) q2
  real(PR) q3
  real(PR) q4
  real(PR) q5
  real(PR) q6
  real(PR) r0
  real(PR) r1
  real(PR) r2
  real(PR) r3
  real(PR) r4
  real(PR) r5
  real(PR) s1
  real(PR) s2
  real(PR) s3
  real(PR) s4
  real(PR) s5
  real(PR) w
  real(PR) x

      data p0/ .577215664901533D0/, p1/ .844203922187225D0/, &
           p2/-.168860593646662D0/, p3/-.780427615533591D0/, &
           p4/-.402055799310489D0/, p5/-.673562214325671D-01/, &
           p6/-.271935708322958D-02/
      data q1/ .288743195473681D+01/, q2/ .312755088914843D+01/, &
           q3/ .156875193295039D+01/, q4/ .361951990101499D0/, &
           q5/ .325038868253937D-01/, q6/ .667465618796164D-03/

      data r0/.422784335098467D0/,    r1/.848044614534529D0/, &
           r2/.565221050691933D0/,    r3/.156513060486551D0/, &
           r4/.170502484022650D-01/, r5/.497958207639485D-03/
      data s1/.124313399877507D+01/, s2/.548042109832463D0/, &
           s3/.101552187439830D0/,    s4/.713309612391000D-02/, &
           s5/.116165475989616D-03/

  if ( a < 0.6 ) then
      w = ((((((p6*a + p5)*a + p4)*a + p3)*a + p2)*a + p1)*a + p0)/ &
          ((((((q6*a + q5)*a + q4)*a + q3)*a + q2)*a + q1)*a + one)
      gamln1 = -a*w
  else
    x = (a - half) - half
      w = (((((r5*x + r4)*x + r3)*x + r2)*x + r1)*x + r0)/ &
          (((((s5*x + s4)*x + s3)*x + s2)*x + s1)*x + one)
      gamln1 = x*w
  end if

  return
end
subroutine grat1 ( a, x, r, p, q, eps )

!*****************************************************************************80
!
!! GRAT1 evaluates the incomplete Gamma ratio functions P(A,X) and Q(A,X).
!
!  Discussion:
!
!    It is assumed that a <= 1.  eps is the tolerance to be used.
!    the input argument r has the value e**(-x)*x**a/gamma(a).
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0

  real(PR) a
  real(PR) a2n
  real(PR) a2nm1
  real(PR) am0
  real(PR) an
  real(PR) an0
  real(PR) b2n
  real(PR) b2nm1
  real(PR) c
  real(PR) cma
  real(PR) eps
!!real(PR) erf
!!real(PR) erfc1
  real(PR) g
  real(PR) gam1
  real(PR) h
  real(PR) j
  real(PR) l
  real(PR) p
  real(PR) q
  real(PR) r
  real(PR) rexp
  real(PR) sum2
  real(PR) t
  real(PR) tol
  real(PR) w
  real(PR) x
  real(PR) z

  if (a*x == zero) go to 130
  if (a == half) go to 120
  if (x < 1.1) go to 10
  go to 50
!
!  Taylor series for p(a,x)/x**a
!
10 continue

      an = 3.0D0
      c = x
      sum2 = x/(a + 3.0D0)
      tol = 0.1D0*eps/(a + one)

      do
        an = an + one
        c = -c*(x/an)
        t = c/(a + an)
        sum2 = sum2 + t
        if ( abs ( t ) <= tol ) then
          exit
        end if
      end do

      j = a*x*((sum2/6.0D0 - half/(a + two))*x + one/(a + one))

      z = a * log ( x )
      h = gam1(a)
      g = one + h
      if (x < 0.25D0) go to 20
         if (a < x/2.59D0) go to 40
         go to 30
   20 if (z .gt. -0.13394D0) go to 40

   30 w = exp(z)
      p = w*g*(half + (half - j))
      q = half + (half - p)
      return

   40 l = rexp(z)
      w = half + (half + l)
      q = (w*j - l)*g - h
      if (q < zero) go to 110
      p = half + (half - q)
      return
!
!  continued fraction expansion
!
   50 a2nm1 = one
      a2n = one
      b2nm1 = x
      b2n = x + (one - a)
      c = one
   51    a2nm1 = x*a2n + c*a2nm1
         b2nm1 = x*b2n + c*b2nm1
         am0 = a2nm1/b2nm1
         c = c + one
         cma = c - a
         a2n = a2nm1 + cma*a2n
         b2n = b2nm1 + cma*b2n
         an0 = a2n/b2n
         if (abs(an0 - am0) .ge. eps*an0) go to 51
      q = r*an0
      p = half + (half - q)
      return
!
!  special cases
!
  100 p = zero
      q = one
      return

  110 p = one
      q = zero
      return

  120 if (x .ge. 0.25D0) go to 121
      p = erf(sqrt(x))
      q = half + (half - p)
      return
  121 q = erfc(sqrt(x))
      p = half + (half - q)
      return

  130 if (x <= a) go to 100
      go to 110

end
function gsumln ( a, b )

!*****************************************************************************80
!
!! GSUMLN evaluates the function Log ( Gamma ( A + B ) ) in a special range.
!
!          for 1 <= a <= 2  and  1 <= b <= 2
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: twodble=2.0D0
  real(PR), parameter :: one=1.0D0

  real(PR) a
  real(PR) alnrel
  real(PR) b
  real(PR) gamln1
  real(PR) gsumln
  real(PR) x

  x = real ( a + b - twodble, kind(PR) ) 

  if ( x <= 0.25D0 ) then
    gsumln = gamln1 ( one + x )
  else if (x <= 1.25D0 ) then
    gsumln = gamln1 ( x ) + alnrel ( x )
  else
    gsumln = gamln1 ( x - one ) + log ( x * ( one + x ) )
  end if

  return
end
function ipmpar ( i )

!*****************************************************************************80
!
!! IPMPAR provides the integer machine constants for the computer
!     that is used. it is assumed that the argument i is an integer
!     having one of the values 1-10. ipmpar(i) has the value ...
!
!  integers.
!
!     assume integers are represented in the n-digit, base-a form
!
!               sign ( x(n-1)*a**(n-1) + ... + x(1)*a + x(0) )
!
!               where 0 <= x(i) < a for i=0,...,n-1.
!
!     ipmpar(1) = a, the base.
!
!     ipmpar(2) = n, the number of base-a digits.
!
!     ipmpar(3) = a**n - 1, the largest magnitude.
!
!  floating-point numbers.
!
!     it is assumed that the single and double precision floating
!     point arithmetics have the same base, say b, and that the
!     nonzero numbers are represented in the form
!
!               sign (b**e) * (x(1)/b + ... + x(m)/b**m)
!
!               where x(i) = 0,1,...,b-1 for i=1,...,m,
!               x(1) .ge. 1, and emin <= e <= emax.
!
!     ipmpar(4) = b, the base.
!
!  single-precision
!
!     ipmpar(5) = m, the number of base-b digits.
!
!     ipmpar(6) = emin, the smallest exponent e.
!
!     ipmpar(7) = emax, the largest exponent e.
!
!  double-precision
!
!     ipmpar(8) = m, the number of base-b digits.
!
!     ipmpar(9) = emin, the smallest exponent e.
!
!     ipmpar(10) = emax, the largest exponent e.
!
!     to define this function for the computer being used, activate
!     the data statments for the computer by removing the c from
!     column 1. (all the other data statements should have c in
!     column 1.)
!
!     ipmpar is an adaptation of the function i1mach, written by
!     p.a. fox, a.d. hall, and n.l. schryer (bell laboratories).
!     ipmpar was formed by a.h. morris (nswc). the constants are
!     from bell laboratories, nswc, and other sources.
!
      integer imach(10)
  integer ipmpar
!
!     machine constants for the ibm pc.
!
!!    data imach( 1) /     2 /
!!    data imach( 2) /    31 /
!!    data imach( 3) / 2147483647 /
!!    data imach( 4) /     2 /
!!    data imach( 5) /    24 /
!!    data imach( 6) /  -125 /
!!    data imach( 7) /   128 /
!!    data imach( 8) /    53 /
!!    data imach( 9) / -1021 /
!!    data imach(10) /  1024 /

      imach( 1) = 2
      imach( 2) = digits(0)
      imach( 3) = huge(0)
      imach( 4) = 2
      imach( 5) = digits(0.0)
      imach( 6) = minexponent(0.0)
      imach( 7) = maxexponent(0.0)
      imach( 8) = digits(0.0D0)
      imach( 9) = minexponent(0.0D0)
      imach(10) = maxexponent(0.0D0)
!
!     machine constants for amdahl machines.
!
!     data imach( 1) /   2 /
!     data imach( 2) /  31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /  16 /
!     data imach( 5) /   6 /
!     data imach( 6) / -64 /
!     data imach( 7) /  63 /
!     data imach( 8) /  14 /
!     data imach( 9) / -64 /
!     data imach(10) /  63 /
!
!     machine constants for the at&t 3b series, at&t
!     pc 7300, and at&t 6300.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the burroughs 1700 system.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   33 /
!     data imach( 3) / 8589934591 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -256 /
!     data imach( 7) /  255 /
!     data imach( 8) /   60 /
!     data imach( 9) / -256 /
!     data imach(10) /  255 /
!
!     machine constants for the burroughs 5700 system.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /    8 /
!     data imach( 5) /   13 /
!     data imach( 6) /  -50 /
!     data imach( 7) /   76 /
!     data imach( 8) /   26 /
!     data imach( 9) /  -50 /
!     data imach(10) /   76 /
!
!     machine constants for the burroughs 6700/7700 systems.
!
!     data imach( 1) /      2 /
!     data imach( 2) /     39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /      8 /
!     data imach( 5) /     13 /
!     data imach( 6) /    -50 /
!     data imach( 7) /     76 /
!     data imach( 8) /     26 /
!     data imach( 9) / -32754 /
!     data imach(10) /  32780 /
!
!     machine constants for the cdc 6000/7000 series
!     60 bit arithmetic, and the cdc cyber 995 64 bit
!     arithmetic (nos operating system).
!
!     data imach( 1) /    2 /
!     data imach( 2) /   48 /
!     data imach( 3) / 281474976710655 /
!     data imach( 4) /    2 /
!     data imach( 5) /   48 /
!     data imach( 6) / -974 /
!     data imach( 7) / 1070 /
!     data imach( 8) /   95 /
!     data imach( 9) / -926 /
!     data imach(10) / 1070 /
!
!     machine constants for the cdc cyber 995 64 bit
!     arithmetic (nos/ve operating system).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    48 /
!     data imach( 6) / -4096 /
!     data imach( 7) /  4095 /
!     data imach( 8) /    96 /
!     data imach( 9) / -4096 /
!     data imach(10) /  4095 /
!
!     machine constants for the cray 1, xmp, 2, and 3.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    47 /
!     data imach( 6) / -8189 /
!     data imach( 7) /  8190 /
!     data imach( 8) /    94 /
!     data imach( 9) / -8099 /
!     data imach(10) /  8190 /
!
!     machine constants for the data general eclipse s/200.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     machine constants for the harris 220.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   23 /
!     data imach( 3) / 8388607 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   38 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     machine constants for the honeywell 600/6000
!     and dps 8/70 series.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   63 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     machine constants for the hp 2100
!     3 word double precision option with ftn4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   39 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     machine constants for the hp 2100
!     4 word double precision option with ftn4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   55 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     machine constants for the hp 9000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -126 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the ibm 360/370 series,
!     the icl 2900, the itel as/6, the xerox sigma
!     5/7/9 and the sel systems 85/86.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     machine constants for the macintosh ii - absoft
!     macfortran ii.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the microvax - vms fortran.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     machine constants for the pdp-10 (ka processor).
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   54 /
!     data imach( 9) / -101 /
!     data imach(10) /  127 /
!
!     machine constants for the pdp-10 (ki processor).
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   62 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     machine constants for the pdp-11 fortran supporting
!     32-bit integer arithmetic.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     machine constants for the sequent balance 8000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the silicon graphics iris-4d
!     series (mips r3000 processor).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the sun 3.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     machine constants for the univac 1100 series.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   60 /
!     data imach( 9) /-1024 /
!     data imach(10) / 1023 /
!
!     machine constants for the vax 11/780.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
      ipmpar = imach(i)

  return
end
function psi ( xx )

!*****************************************************************************80
!
!! PSI evaluates the Psi or Digamma function.
!
!     psi(xx) is assigned the value 0 when the digamma function cannot
!     be computed.
!
!     the main computation involves evaluation of rational chebyshev
!     approximations published in math. comp. 27, 123-127(1973) by
!     cody, strecok and thacher.
!
!     psi was written at argonne national laboratory for the funpack
!     package of special function subroutines. psi was modified by
!     a.h. morris (nswc).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
!  Local parameters:
!
!    Local, double precision DX0, zero of PSI.
!
!    Local, real PIOV4 = pi/4.
!
!    Local, real XMAX1, the smallest positive floating point constant
!    with entirely integer representation.  Also used as negative of
!    lower bound on acceptable negative arguments and as the positive
!    argument beyond which PSI may be represented as log(x).
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0

  real(PR) aug
  real(PR) den
  real(PR), parameter :: dx0 = 1.461632144968362341262659542325721325D0
  integer i
  integer ipmpar
  integer m
  integer n
  integer nq
  real(PR) p1(7)
  real(PR) p2(4)
  real(PR), parameter :: piov4 = 0.785398163397448D0
  real(PR) psi
  real(PR) q1(6)
  real(PR) q2(4)
  real(PR) sgn
  real(PR) upper
  real(PR) w
  real(PR) x
  real(PR) xmax1
  real(PR) xmx0
  real(PR) xsmall
  real(PR) xx
  real(PR) z
!
!  coefficients for rational approximation of
!  psi(x) / (x - x0),  0.5 <= x <= 3.0
!
      data p1(1)/.895385022981970D-02/, p1(2)/.477762828042627D+01/, &
           p1(3)/.142441585084029D+03/, p1(4)/.118645200713425D+04/, &
           p1(5)/.363351846806499D+04/, p1(6)/.413810161269013D+04/, &
           p1(7)/.130560269827897D+04/
      data q1(1)/.448452573429826D+02/, q1(2)/.520752771467162D+03/, &
           q1(3)/.221000799247830D+04/, q1(4)/.364127349079381D+04/, &
           q1(5)/.190831076596300D+04/, q1(6)/.691091682714533D-05/
!
!  coefficients for rational approximation of
!  psi(x) - ln(x) + 1 / (2*x),  x .gt. 3.0
!
      data p2(1)/-.212940445131011D+01/, p2(2)/-.701677227766759D+01/, &
           p2(3)/-.448616543918019D+01/, p2(4)/-.648157123766197D0/
      data q2(1)/ .322703493791143D+02/, q2(2)/ .892920700481861D+02/, &
           q2(3)/ .546117738103215D+02/, q2(4)/ .777788548522962D+01/
!
!  machine dependent constants ...
!
!  xsmall = absolute argument below which pi*cotan(pi*x)
!  may be represented by 1/x.
!
      xmax1 = ipmpar(3)
      xmax1 =  min ( xmax1, one / epsilon ( xmax1 ) )
      xsmall = 1.0D-9

      x = xx
      aug = zero
      if (x .ge. half) go to 200
!
!  x < 0.5,  use reflection formula
!  psi(1-x) = psi(x) + pi * cotan(pi*x)
!
      if ( xsmall < abs(x) ) go to 100
      if (x == zero) go to 400
!
!  0 < abs(x) <= xsmall.  use 1/x as a substitute
!  for  pi*cotan(pi*x)
!
      aug = -one / x
      go to 150
!
!  reduction of argument for cotan
!
  100 w = - x
      sgn = piov4
      if ( zero < w ) go to 120
      w = - w
      sgn = -sgn
!
!  Error exit if x <= -xmax1
!
  120 if (w .ge. xmax1) go to 400
      nq = int(w)
      w = w - float(nq)
      nq = int(w*4.0D0)
      w = 4.0D0 * (w - float(nq) * .25D0)
!
!  w is now related to the fractional part of  4.0 * x.
!  adjust argument to correspond to values in first
!  quadrant and determine sign
!
      n = nq / 2
      if ((n+n) /= nq) then
        w = one - w
      end if
      z = piov4 * w
      m = n / 2
      if ((m+m) /= n) then
        sgn = - sgn
      end if
!
!  Determine final value for  -pi*cotan(pi*x)
!
      n = (nq + 1) / 2
      m = n / 2
      m = m + m
      if (m /= n) go to 140
!
!  Check for singularity
!
      if (z == zero) go to 400
!
!  use cos/sin as a substitute for cotan, and
!  sin/cos as a substitute for tan
!
      aug = sgn * ((cos(z) / sin(z)) * 4.0D0)
      go to 150
  140 aug = sgn * ((sin(z) / cos(z)) * 4.0D0)
  150 x = one - x
  200 if (x .gt. 3.0D0) go to 300
!
!  0.5 <= x <= 3.0
!
      den = x
      upper = p1(1) * x

      do i = 1, 5
         den = (den + q1(i)) * x
         upper = (upper + p1(i+1)) * x
      end do

      den = (upper + p1(7)) / (den + q1(6))
      xmx0 = real(x  - dx0, kind(1.0D0))
      psi = den * xmx0 + aug
      return
!
!  if x .ge. xmax1, psi = ln(x)
!
  300 if (x .ge. xmax1) go to 350
!
!  3.0 < x < xmax1
!
      w = one / (x * x)
      den = w
      upper = p2(1) * w

      do i = 1, 3
         den = (den + q2(i)) * w
         upper = (upper + p2(i+1)) * w
      end do

      aug = upper / (den + q2(4)) - half / x + aug
  350 psi = aug + log ( x )
      return
!
!  error return
!
  400 psi = zero

  return
end
function rexp ( x )

!*****************************************************************************80
!
!! REXP evaluates the function Exp(X) - 1.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0

  real(PR), parameter :: p1 = 0.914041914819518D-09
  real(PR), parameter :: p2 = 0.238082361044469D-01
  real(PR), parameter :: q1 = -0.499999999085958D0
  real(PR), parameter :: q2 = 0.107141568980644D0
  real(PR), parameter :: q3 = -0.119041179760821D-01
  real(PR), parameter :: q4 = 0.595130811860248D-03
  real(PR) rexp
  real(PR) w
  real(PR) x

  if ( abs ( x ) <= 0.15D0 ) then

    rexp = x * (((  &
              p2    &
        * x + p1 )  &
        * x + one ) &
      / ((((  q4    &
        * x + q3 )  &
        * x + q2 )  &
        * x + q1 )  &
        * x + one ))

  else if ( x < zero ) then

    w = exp ( x )
    rexp = ( w - half ) - half

  else

    w = exp ( x )
    rexp = w * ( half + ( half - one / w ) )

  end if

  return
end
function rlog1 ( x )

!*****************************************************************************80
!
!! RLOG1 evaluates the function X - Log ( 1 + X ).
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the
!    Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1993, pages 360-373.
!
  implicit none
  INTEGER, PARAMETER        :: PR=KIND(1.0D0)

  real(PR), parameter :: zero=0.0D0, half=0.5D0, one=1.0D0, two=2.0D0

  real(PR), parameter :: a = 0.566749439387324D-01
  real(PR), parameter :: b = 0.456512608815524D-01
  real(PR) h
  real(PR), parameter :: p0 = 0.333333333333333D0
  real(PR), parameter :: p1 = -0.224696413112536D0
  real(PR), parameter :: p2 = 0.620886815375787D-02
  real(PR), parameter :: q1 = -0.127408923933623D+01
  real(PR), parameter :: q2 = 0.354508718369557
  real(PR) r
  real(PR) rlog1
  real(PR) t
  real(PR) w
  real(PR) w1
  real(PR) x

  if ( x < -0.39D0 ) then

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  else if ( x < -0.18D0 ) then

    h = ( x + 0.3D0 ) / 0.7D0
    w1 = a - h * 0.3D0
    r = h / ( h + two )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + one )
    rlog1 = two * t * ( one / ( one - r ) - r * w ) + w1

  else if ( x <= 0.18D0 ) then

    h = x
    w1 = zero
    r = h / ( h + two )
    t = r * r
    w = (( p2 * t + p1 ) * t + p0 ) / (( q2 * t + q1 ) * t + one )
    rlog1 = two * t * ( one / ( one - r ) - r * w ) + w1

  else if ( x <= 0.57D0 ) then

    h = 0.75D0 * x - 0.25D0
    w1 = b + h / 3.0D0
    r = h / ( h + two )
    t = r * r
    w = (( p2 * t + p1 ) * t + p0 ) / (( q2 * t + q1 ) * t + one )
    rlog1 = two * t * ( one / ( one - r ) - r * w ) + w1

  else

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  end if

  return
end
