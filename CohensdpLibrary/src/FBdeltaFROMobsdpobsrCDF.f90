FUNCTION FBdeltafromobsdpobsrcdf( delta, n, obsdp, obsr, TOL, MAXITER, ier )

    !! This distribution is based on a fiducial-Bayesian approach
    !! in which the mixing parameter K is based on an observed r rather than the population rho.

    !-----------------------------------------------------------------------
    !
    !     Calculates the probability that a random variable dp distributed
    !     according to the L" distribution with parameters 
    !           2(n-1)                          degree of freedom,
    !           sqrt(n/(2(1-rho))) delta        non-centrality parameter
    !     where rho is distributed according to the K' distribution with parameters
    !           n-2, n-1                        degrees of freedom and
    !           sqrt(n-2) obsr/sqrt(1-obsr^2)   non-centrality parameter
    !     is less than or equal to x. This distribution is a mixture across all rhos
    !     given one observed r. It is expressed as
    !           PDF(x) = NIntegrate[PDF[rhoDistribution[n, obsr], rho] *
    !                               PDF[rmdeltaDistribution[n, obsdp, rho], x], 
    !                               {rho, -1, 1}]
    !     where
    !            PDF[rhoDistribution[n, obsr], rho]        is the rho predictive distribution given one observed r
    !                   = sqrt(n-1)/(1-rho^2)^(3/2) * PDF[K'(n-2, n-1, sqrt(n-2) obsr/sqrt(1-obsr^2)][sqrt(n-1) rho/sqrt(1-rho^2)]
    !            PDF[rmdeltaDistribution[n, obsdp, rho], delta]   is the repeated measure sampling distribution of delta (x) given obsdp, rho
    !                   = PDF[L'( 2(n-1), sqrt(n/(2(1-rho)) obsdp][sqrt(n/(2(1-rho)))*x]
    !
    !     delta     - Input . delta value where to compute the pdf     - Real
    !     n         - Input . Sample size (N >  0)                     - Real
    !     obsdp     - Input . for eccentricity parameter of L'         - Real
    !     obsr      - Input . for eccentricity parameter of K'         - Real
    !     TOL       - Input . Maximum absolute error required on       - Real
    !                         kprimepdf (stopping criteria)
    !                         (eps < TOL < 1 where eps is machine epsilon)
    !     MAXITER   - Input . Maximum number of iterations            - Integer
    !     ier       - input . unused
    !
    !     External functions called:
    !         KPRIMEPDF  FBlsecondCDF
    !     Fortran functions called:
    !         ABS SQRT 
    !
    !*********************************************************************************************!
    !**                                                                                         **!
    !** This function was created by Denis Cousineau, 7 august 2022.                            **!
    !** Avec l'assistance de sa muse, Elysabeth Aguila                                          **!
    !** It uses functions from B. Lecoutre & J. Poitevineau.                                    **!
    !**                                                                                         **!
    !*********************************************************************************************!

    IMPLICIT NONE

    !  Function
    INTEGER, PARAMETER        :: PR=KIND(1.0D0)
    REAL(PR)                  :: FBdeltafromobsdpobsrcdf

    !  Arguments
    REAL(PR), INTENT(in)      :: delta, n, obsdp, obsr, TOL
    INTEGER,  INTENT(in)      :: MAXITER
    INTEGER,  INTENT(out)     :: ier
    REAL(PR), PARAMETER       :: SML=1.0D-6, ONE=1.0D0, TWO=2.0D0, ONEnHALF=1.5D0

    !  Local declarations
    REAL(PR)                  :: s
    REAL(PR), EXTERNAL        :: kprimepdf, FBlsecondCDF

    ier = 0
    ! NOTE: the bounds cannot be exactly -1 and +1 so removed SML
    s = qromb( fct, -1.0D0 + SML, +1.0D0 - SML )

    ! s is the solution
    FBdeltafromobsdpobsrcdf = s

CONTAINS

    FUNCTION fct(rho)
        REAL(PR), INTENT(in)      :: rho
        REAL(PR), EXTERNAL        :: kprimepdf, FBlsecondCDF
        REAL(PR)                  :: fct

        REAL(PR)                  :: k1, scaledrho, scaledr   !temporary variables
        INTEGER                   :: iok                ! unused
!            PDF[rhoDistribution[n, obsr], rho]        is the rho predictive distribution given one observed r
!                   = sqrt(n-1)/(1-rho^2)^(3/2) * PDF[K'(n-2, n-1, sqrt(n-2) obsr/sqrt(1-obsr^2)][sqrt(n-1) rho/sqrt(1-rho^2)]
!                   =           k1              * PDF[K'(n-2, n-1,        ncK                   ][        scaledr            ]
!            PDF[L"[n, delta, rho], dp]   is the repeated measure sampling distribution of delta (x) given obsdp, rho

        k1        = sqrt(n-ONE) / ((ONE-rho**2)**(ONEnHALF)) 
        scaledrho = sqrt(n-ONE) * rho  / sqrt(ONE-rho**2)
        scaledr   = sqrt(n-TWO) * obsr / sqrt(ONE-obsr**2)

        fct = k1 * kprimepdf(scaledrho, n-TWO, n-ONE, scaledr, TOL, MAXITER, iok)  &
            * FBlsecondCDF(delta, n, obsdp, obsr, rho, TOL, MAXITER )

    END FUNCTION fct

    ! The following are taken from Press, Teukolsky, Vetterling, and Flannery
    ! Numerical Receipes in Fortran 77, 1992.
    FUNCTION qromb(fct, a, b)
        REAL(PR), EXTERNAL             :: fct
        REAL(PR), INTENT(in)           :: a, b
        REAL(PR)                       :: qromb

        REAL(PR)                       :: EPS 
        INTEGER, PARAMETER             :: JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1
        INTEGER                        :: j
        REAL(PR)                       :: dqromb
        REAL(PR), DIMENSION(JMAXP)     :: h, s 

        EPS = TOL * 5.0D0

        h(1) = 1.0D0
        do j = 1, JMAX
            call trapzd(fct, a, b, s(j), j)
            if (j .ge. K) then
                call polint(h(j-KM), s(j-KM), K, 0.0D0, qromb, dqromb)
!                if (abs(dqromb) .le. EPS*abs(qromb)) return
                if (abs(dqromb) .le. EPS ) return ! not the relative error, but the absolute error
            endif
            s(j+1) = s(j)
            h(j+1) = 0.25*h(j)
        enddo
        qromb = -43.0D0 ! something went wrong e.g., exceeded JMAX steps
        return
    END FUNCTION qromb

    SUBROUTINE trapzd(fct, a, b, s, n)
        integer, intent(in)         :: n
        real(PR), external          :: fct
        real(PR), intent(in)        :: a, b
        real(PR), intent(inout)     :: s

        integer                     :: it, j
        real(PR)                    :: del, fsum, tnm, x

        if (n .eq. 1) then
            s = 0.5*(b-a)*(fct(a)+fct(b))
        else
            it  = 2**(n-2)
            tnm = it
            del = (b-a)/tnm 
            x   = a+0.5*del
            fsum = 0.
            do j = 1, it
                fsum = fsum + fct(x)
                x   = x + del
            enddo
            s = 0.5*(s+(b-a)*fsum/tnm) 
        endif
        return
    END SUBROUTINE trapzd

    SUBROUTINE polint(xa, ya, n, x, y, dy)
        INTEGER, INTENT(in)        :: n
        REAL(PR), INTENT(in)       :: xa(n), ya(n), x
        REAL(PR), INTENT(out)      :: y, dy

        INTEGER, PARAMETER         :: NMAX=10
        INTEGER                    :: i, m, ns
        REAL(PR)                   :: den, dif, dift, ho, hp, w, c(NMAX), d(NMAX)

        ns = 1
        dif = abs(x-xa(1))
        do i = 1, n 
            dift = abs(x-xa(i))
            if (dift .lt. dif) then
                ns = i
                dif = dift
            endif
            c(i) = ya(i) 
            d(i) = ya(i)
        enddo
        y = ya(ns) 
        ns = ns-1
        do m = 1, n-1 
            do i = 1, n-m 
                ho   = xa(i)-x
                hp   = xa(i+m)-x
                w    = c(i+1)-d(i)
                den  = ho-hp
                if (den .eq. 0.) then
                    y  = -45.0D0
                    dy = -45.0D0
                    return ! pause "failure in polint"
                endif
                den  = w/den
                d(i) = hp*den 
                c(i) = ho*den
            enddo
            if (2*ns .lt. n-m) then 
                dy = c(ns+1)
            else
                dy = d(ns)
                ns = ns - 1
            endif
            y = y + dy
        enddo
        return
    END SUBROUTINE polint

END FUNCTION FBdeltafromobsdpobsrcdf
