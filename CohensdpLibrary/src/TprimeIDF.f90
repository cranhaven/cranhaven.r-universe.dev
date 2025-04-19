FUNCTION tprimeidf( p, q, a1, delta, maxitr, ier )

    !-----------------------------------------------------------------------
    !     Calculates the quantile of a random variable distributed
    !     according to the t' distribution with Q degrees of freedom,
    !     A centrality parameter, that is the X such that F( X | q, a) = p
    !
    !     P     - Input . Value of the quantile          (0<P<1)  - Real
    !     Q     - Input . Degrees of freedom             (Q >  0) - Real
    !     A1    - Input . Eccentricity parameter                  - Real
    !     DELTA - Input . Maximum absolute error required on      - Real
    !                     kprimecdf (stopping criteria)
    !                     (eps < DELTA < 1 where eps is machine
    !                     epsilon; see parameter statement below)
    !     MAXITR- Input . Maximum number of iterations            - Integer
    !     IER   - Output. Return code :                           - Integer
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
    !       LPRIMECDF
    !     Fortran functions called:
    !       ABS  LOG  SQRT  log_gamma
    !
    !*********************************************************************************************!
    !**                                                                                         **!
    !** This function was added by Denis Cousineau, 28 november 2020.                           **!
    !** It is just a wrapper to the generic function dtrinv.f90 made by J. Poitevineau.         **!
    !**                                                                                         **!
    !*********************************************************************************************!

    IMPLICIT NONE
    INTEGER, PARAMETER        :: PR=KIND(1.0D0)

    !  Function
    !  --------
    REAL(PR) :: tprimeidf

    !  Arguments
    !  ---------
    REAL(PR),     INTENT(in)  :: p, q, a1, delta
    INTEGER,      INTENT(in)  :: maxitr
    INTEGER,      INTENT(out) :: ier

    !  Local declarations
    !  ------------------
    REAL(PR) :: k
    REAL(PR), EXTERNAL :: tprimecdf, dtrinv
    ier = 0

    k = sqrt(q/2) * Exp(log_gamma((q-1.0D0)/2.0D0) - log_gamma(q/2.0D0) ) 

    tprimeidf = dtrinv( func, p,                             &
                .FALSE., .FALSE., -1.0D6, 1.0D6,          &
                a1 * k,                                   &
                sqrt( q*(1.0D0 + a1**2)/(q-2.0D0) - a1**2 * k**2 ), &
                +1.0D-6, 1000,                            &
                ier                                       &
            )

CONTAINS

    FUNCTION func(x, iok)
        REAL(PR), INTENT(in) :: x
        INTEGER,  INTENT(out):: iok
        REAL(PR), EXTERNAL   :: lprimecdf
        REAL(PR) :: func
        func = tprimecdf(x, q, a1, delta, maxitr, iok)
    END FUNCTION func

END FUNCTION tprimeidf


