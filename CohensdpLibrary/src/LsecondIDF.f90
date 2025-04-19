FUNCTION lsecondIDF ( q, n, d, rho, TOL, MAXITER, ier )
    ! ***************************************************************************
    ! Nonstandard noncentral t" distribution
    !   This distribution is the exact distribution of the Cohen's d_p
    !   in 2-repeated-measure samples from a Binormal population with 
    !   common variance sigma (compound symmetry).
    ! Parameters are n        (sample size), 
    !                d        (observed difference in the sample), 
    !                rho      (correlatoin in the population)
    !                q        (probability whose quantile is searched for)
    !                MAXITER  (maximum iterations, often 15 are enough)
    !                TOL      (a tolerance level to decide to quit)
    !                ier      (unused)
    ! Ouptput:
    !       the quantile of q
    ! Requires 
    !       mydtrinv
    !
    ! Denis Cousineau, 12/juillet/2022.
    !
    ! ***************************************************************************
    IMPLICIT NONE
    INTEGER, PARAMETER         :: PR=KIND(1.0D0)

    !  Function
    REAL(KIND=PR)              :: lsecondIDF

    !  Arguments
    REAL(KIND=PR), INTENT(IN)  :: q, n, d, rho
    INTEGER, INTENT(IN)        :: MAXITER  ! max iterations, suggested 5000
    REAL(PR), INTENT(IN)       :: TOL      ! precision to exit, suggested 10D-5
    INTEGER, INTENT(OUT)       :: ier      

    !  Local declarations
    REAL(PR), PARAMETER        :: ONE=1.0D0, TWO=2.0D0

    !  Local declarations
    REAL(KIND=PR), EXTERNAL :: lsecondCDF, mydtrinv

    ier = 0
    lsecondIDF = mydtrinv( func, q,                     & ! func must a one-argument function
                 .FALSE., .FALSE., -1.0D6, 1.0D6,       & ! no bounds
                 d,                                     & ! approximate mean
                 ONE/TWO,                               & ! approximate standard deviation
                 TOL, MAXITER,  ier                     & ! etc.
            )

CONTAINS

    FUNCTION func(x, iok)
        REAL(KIND=PR), INTENT(in) :: x
        INTEGER,       INTENT(out):: iok
        REAL(KIND=PR), EXTERNAL   :: nsnctCDF
        REAL(KIND=PR) :: func
        func = lsecondCDF(x, n, d, rho, TOL, MAXITER, iok )
    END FUNCTION func

END FUNCTION lsecondIDF


