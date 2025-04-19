FUNCTION lsecondPDF ( delta, n, d, rho, TOL, MAXITER, ier )
    ! ***************************************************************************
    ! Nonstandard noncentral Lambda" distribution
    !   This distribution is the exact distribution of the Cohen's delta
    !   in the 2-repeated-measure samples from a Binormal population with 
    !   common variance sigma.
    ! Parameters are delta    (putative parameter whose density is required)
    !                n        (sample size), 
    !                d        (observed difference in the sample), 
    !                rho      (correlation in the population)
    !                TOL      (a tolerance level to decide to quit)
    !                MAXITER  (maximum iterations, often 15 are enough)
    !                ier      (unused)
    ! Outputs the density of delta given the population parameter rho and observed d_p
    !
    ! Requires GAMMA, lprimePDF
    !
    ! Denis Cousineau 12/juillet/2022.
    !
    ! ***************************************************************************
    IMPLICIT NONE
    INTEGER,  PARAMETER   :: PR = KIND(1.0D0)

    REAL(PR), INTENT(IN)  :: delta, n, d, rho
    INTEGER,  INTENT(IN)  :: MAXITER      ! max iterations, suggested 5000
    INTEGER,  INTENT(OUT) :: ier          ! exit code
    REAL(PR), INTENT(IN)  :: TOL          ! precision to exit, suggested 1.0D-7
    REAL(PR)              :: lsecondPDF  
    
    REAL(PR), EXTERNAL    :: lprimePDF
    REAL(PR), PARAMETER   :: HALF=0.5D0, ONE=1.0D0, TWO = 2.0D0, FOUR =  4.0D0

    !  Local declarations
    REAL(PR)              :: q, b0, total, step, previousstep, ck, ek
    INTEGER               :: k, jer

    jer = 0
    step  = 0.0D0
    previousstep = 0.0D0
    total  = 0.0D0

    b0 = sqrt(n/(TWO*(ONE-rho))) 
    q  = n - ONE

    k = 0
    DO WHILE ( k .LE. MAXITER )
        previousstep = step
!        ck = EXP( LOG_GAMMA(q/TWO+k) - LOG_GAMMA(q/TWO) - LOG_GAMMA(k+ONE)) * rho**(TWO*k) * & 
!            (ONE-rho**TWO)**(q/TWO)
        ck =  EXP( LOG_GAMMA(q/TWO+k) - LOG_GAMMA(q/TWO) - LOG_GAMMA(k+ONE) + (TWO*k)*LOG(rho) + & 
                   (q/TWO) * LOG(ONE-rho**TWO) &
              )
        ek = sqrt(1/(1-rho**TWO)) * sqrt(q/(q + TWO*k))
    
        step = ck/(1/ek) * lprimePDF(b0 * delta, TWO*q+FOUR*k, b0*d / ek, TOL, MAXITER, jer)

        total = total +  step
        k = k + 1
        ! exit if too many iterations or if step is decreasing and goes below TOL
        IF (( k > MAXITER).or.((step < previousstep).AND.(step < TOL))) GOTO 10

    ENDDO

10  ier = 0
    lsecondPDF = b0 * total

END FUNCTION lsecondPDF
