FUNCTION tprimecdf( x, q, a, TOL, MAXITER, ier )
    !-----------------------------------------------------------------------
    !     Poitevineau, J. and Lecoutre, B. (2010) Statistical distributions for bayesian 
    !         experimental data analysis fortran functions 1. continuous distributions,
    !         url = https://eris62.eu
    !
    !     Computes the probability that a random variable distributed
    !     according to the noncentral t distribution with Q degrees of
    !     freedom, A centrality parameter, is less than or equal to X
    !     Note: P( t'q (x) < a ) = P( L'q (a) > x )
    !
    !     X     - Input . Value of the variable                   - Real
    !     Q     - Input . Degrees of freedom             (Q >  0) - Real
    !     A     - Input . Eccentricity parameter                  - Real
    !     TOL   - Input . Maximum absolute error required on      - Real
    !                     tnccdf (stopping criterion)
    !                     (eps < TOL < 1 where eps is machine
    !                     epsilon; see parameter statement below)
    !     MAXITER- Input . Maximum number of iterations            - Integer
    !     IER   - Output. Return code :                           - Integer
    !                     0 = normal
    !                     1 = invalid input argument
    !                         (then tnccdf = one)
    !                     2 = maximum number of iterations reached
    !                         (then tnccdf = value at last iteration,
    !                                    or one)
    !                     3 = required accuracy cannot be reached
    !                         (then tnccdf = value at last iteration)
    !                     4 = error in auxiliary function (chi2cdf or tcdf)
    !                     7 = result out of limits (i.e. <0 or >1)
    !
    !     External functions called:
    !       LPRIMECDF
    !*********************************************************************************************!
    !**                                                                                         **!
    !** This function was renamed by Denis Cousineau, 4/4/2022.                                 **!
    !**                                                                                         **!
    !*********************************************************************************************!

   IMPLICIT NONE
   INTEGER, PARAMETER        :: PR=KIND(1.0D0)

   !  Function
   REAL(PR) :: tprimecdf

   !  Arguments
   REAL(PR), INTENT(in) :: x, q, a, TOL
   INTEGER, INTENT(in)  :: MAXITER
   INTEGER, INTENT(out) :: ier

   !  local declarations
   REAL(PR), EXTERNAL :: lprimecdf
   REAL(PR), PARAMETER :: one=1.0D0

   !-----------------------------------------------------------------
   ier = 0

   tprimecdf = one - lprimecdf( a, q, x, TOL, MAXITER, ier )

END FUNCTION tprimecdf
