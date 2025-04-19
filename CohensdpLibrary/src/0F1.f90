! I can't believe that there is no simple implementation of the 0F1
! hypergeometric function (confluent hypergeometric limit function)
! So let's write one...

! Denis Cousineau 4/4/2022.
!
FUNCTION hyg0f1 ( parama, paramz )
    IMPLICIT NONE

    INTEGER, PARAMETER    :: PR=KIND(1.0D0)
    REAL(PR), intent(in)  :: parama, paramz
    REAL(PR)              :: hyg0f1       ! only doubles can be transfered 

    REAL(PR), PARAMETER   :: TOL=1.0D-15, MAXITER = 1000
    REAL(PR), PARAMETER   :: ZERO=0.0D0,ONE=1.0D0
    REAL(PR) serie, lstep, lprod, lfact
    INTEGER  iter

    serie = ONE ! start of the serie
    iter  = 0
    lprod  = LOG(ONE)
    lfact  = LOG(ONE)

    ! DO WHILE (.TRUE.)
        ! iter = iter + 1
        ! IF (iter .GE. MAXITER) GOTO 10

        ! prod = prod * (parama + REAL(iter, KIND=PR) - ONE)
        ! fact = fact * iter
        ! step = paramz**(iter) / ( REAL(fact, KIND=PR) * prod)
        ! serie= serie + step
        ! IF (step < tol) GOTO 10
    ! ENDDO
    ! we use logs because there are frequent overflow
    DO WHILE (.TRUE.)
        iter = iter + 1
        IF (iter .GE. MAXITER) GOTO 10
        lprod = lprod + LOG(parama + REAL(iter, KIND=PR) - ONE) 
        lfact = lfact + LOG( REAL(iter, KIND=PR) )
        lstep = iter * LOG(paramz) - lfact - lprod  
        serie = serie + EXP(lstep)
        IF ( EXP(lstep) < tol ) GOTO 10
    ENDDO

10  hyg0f1 = serie

END FUNCTION hyg0f1