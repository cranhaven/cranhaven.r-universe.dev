FUNCTION lsecondCDF( delta, n, d, rho, TOL, MAXITER, ier )
    ! ***************************************************************************
    ! Nonstandard noncentral Lambda" distribution
    !   This distribution is the exact distribution of the Cohen's delta
    !   in the 2-repeated-measure samples from a Binormal population with 
    !   common variance sigma.
    ! Parameters are n        (sample size), 
    !                d        (observed difference in the sample), 
    !                rho      (correlation in the population)
    !                d        (putative parameter whose probability is required)
    !                MAXITER  (maximum iterations, often 15 are enough)
    !                TOL      (a tolerance level to decide to quit)
    !                ier      (unused)
    ! Outputs the probability of delta given the population parameter rho and observed d_p
    !
    ! Requires hypergeometric1F1, LOG_GAMMA, EXP
    !
    ! Because there are many large numbers, this is far from trivial to have
    !    a robust program...
    !
    ! Denis Cousineau 12/juillet/2022.
    !
    ! ***************************************************************************
    IMPLICIT NONE
    INTEGER, PARAMETER      :: PR=KIND(1.0D0)

    REAL(PR), intent(in)    :: delta, n, d, rho
    integer                 :: MAXITER  ! max iterations, suggested 5000
    REAL(PR), INTENT(IN)    :: TOL      ! precision to exit, suggested 10D-5
    INTEGER, INTENT(OUT)    :: ier
    REAL(PR)                :: lsecondCDF

    REAL(PR), EXTERNAL      :: lprimecdf
    REAL(PR), PARAMETER     :: HALF=0.5D0,ONE=1.0D0,TWO=2.0D0,FOUR=4.0D0,SEIZE=16.0D0

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

        step = ck * lprimecdf(b0 * delta, TWO*q+FOUR*k, b0*d / ek, TOL, MAXITER, jer)

        total = total +  step
        k = k + 1
        ! exit if too many iterations or if step is decreasing and goes below TOL
        IF (( k > MAXITER).or.((step < previousstep).AND.(step < TOL))) GOTO 10

    ENDDO

10  ier = 0
    lsecondCDF =  total

END FUNCTION lsecondCDF

