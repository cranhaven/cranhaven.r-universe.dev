FUNCTION FBdeltafromobsdpobsridf( q, n, obsdp, obsr, TOL, MAXITER, ier  )

    !! This distribution is based on a fiducial-Bayesian approach
    !! in which the mixing parameter K is based on an observed r rather than the population rho.

    !-----------------------------------------------------------------------
    !     Calculates the probability that a random variable distributed
    !     according to the K' distribution with Q and R degrees of
    !     freedom, A1 centrality parameter, is less than or equal to X
    !
    !     q     - Input . p value of the desired quantile (0<P<1) - Real
    !     n     - Input . sample size                    (n >  0) - Real
    !     obsdp - Input . Eccentricity parameter                  - Real
    !     obsr  - Input . Observed correlation                    - Real
    !     TOL   - Input . Maximum absolute error required on      - Real
    !                     kprimecdf (stopping criteria)
    !                     (eps < DELTA < 1 where eps is machine
    !                     epsilon; see parameter statement below)
    !     MAXITR- Input . Maximum number of iterations            - Integer
    !     IER   - Output. Return code :                           - Integer
    !         (dtrinv)    0 = normal
    !                    -1 = no more evolution of the sum but required accuracy not reached yet  (then fct = value at last iteration)
    !                     1 = invalid input argument (then fct = zero)
    !                     2 = maximum number of iterations reached (then fct = value at last iteration)
    !                     3 = cannot be computed (then fct = zero)
    !                     4 = error in auxiliary function
    !                     5 = result out of limits (i.e. <0 or >1)
    !                     7 = 2 + 5 both codes apply
    !
    !     External functions called:
    !       FBdeltafromobsdpobsrcdf, dtrinv
    !     Fortran functions called:
    !       ABS  LOG  SQRT  DLGAMA
    !*********************************************************************************************!
    !**                                                                                         **!
    !** This function was created by Denis Cousineau, 7 august 2022.                            **!
    !** Avec l'assistance de sa muse, Elysabeth Aguila                                          **!
    !** It is a wrapper to the generic function dtrinv.f90 made by J. Poitevineau.              **!
    !**                                                                                         **!
    !*********************************************************************************************!

    IMPLICIT NONE
    INTEGER, PARAMETER        :: PR=KIND(1.0D0)

    !  Function
    REAL(PR)                  :: FBdeltafromobsdpobsridf

    !  Arguments
    REAL(PR), INTENT(in)      :: q, n, obsdp, obsr, TOL
    INTEGER,  INTENT(in)      :: MAXITER
    INTEGER,  INTENT(out)     :: ier

    !  Local declarations
    REAL(PR), EXTERNAL        :: FBdeltafromobsdpobsrcdf, dtrinv
    ier = 0
    FBdeltafromobsdpobsridf = dtrinv( func, q,          &
                .FALSE., .FALSE., -1.0D2, +1.0D2,     &
                obsdp, -0.5D0,                        & ! negative to start with EX
                TOL*10, 100,                          &
                ier                                   &
            )

CONTAINS
    FUNCTION func(x, iok)
        REAL(PR), INTENT(in) :: x
        INTEGER,  INTENT(out):: iok
        REAL(PR), EXTERNAL   :: deltafromobsdpobsrcdf
        REAL(PR)             :: func
        iok = 0
        func = FBdeltafromobsdpobsrcdf( x, n, obsdp, obsr, TOL, MAXITER, iok )
    END FUNCTION func

END FUNCTION FBdeltafromobsdpobsridf


