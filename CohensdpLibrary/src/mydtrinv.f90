FUNCTION mydtrinv ( func, f, linf, lsup, xinf, xsup, ex, sx, TOL, MAXITER, ier )
    !******************************************************************************/
    ! this function is a replacement for dtrinv, too slow...
    ! f       ex      ex+sx
    ! target, startg, startd )
    !
    ! linf, lsup, xinf, xsup : is there a lower (upper) limit, and if yes, what is it
    !
    !******************************************************************************/
    !
    !  Purpose:
    !    mydtrinv computes the quantile of a distribution function func;
    !    it uses a simple binary search over the cdf of the cumulative distribution.
    !
    !  Licensing:
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Created:
    !    16 March 2020; fortran-version 4 december 2020; updated 4/4/2022.
    !
    !  Author:   Denis Cousineau.
    !
    !  Parameters:
    !    func, f (the quantile, between 0 and 1)
    !    linf, lsup, xinf, xsup : limits?
    !    ex, sx: expected and standard deviation of the distribution
    !    TOL, MAXITER: the precision and maximum number of steps
    !    ier: error code (unused)
    !******************************************************************************/

    IMPLICIT NONE
    INTEGER, PARAMETER    :: PR=KIND(1.0D0)

    !  Function
    REAL(KIND=PR) :: mydtrinv

    !  Arguments
    REAL(KIND=PR), EXTERNAL    :: func
    REAL(KIND=PR), INTENT(in)  :: f
    LOGICAL,       INTENT(in)  :: linf, lsup
    REAL(KIND=PR), INTENT(in)  :: xinf, xsup, ex, sx, TOL
    INTEGER,       INTENT(in)  :: MAXITER
    INTEGER,       INTENT(out) :: ier

    ! Local variables
    REAL(KIND=PR)              :: fg, fd, mdl, ptg, ptd, newf
    INTEGER                    :: i, iok

    ptg=ex
    ptd=ex

    ! look for two points on either side of target f
    if ( func(ex, iok) > f ) then
        do i = 1, 10
            ptg = ptg - sx
            if (linf .EQV. .TRUE.) then
                if (ptg .LT. xinf) ptg = xinf
            end if
            if (func(ptg, iok) < f) exit
        end do
    else
        do i = 1, 10
            ptd = ptd + sx
            if (lsup .EQV. .TRUE.) then
                if (ptd .GT. xsup) ptd = xsup
            end if
            if (func(ptd, iok) > f) exit
        end do
    end if

    fg = func(ptg, iok);
    fd = func(ptd, iok);

    do i = 1, MAXITER
        if (fd - fg < TOL ) exit

        mdl  = (ptd + ptg)/2.0D0
        newf = func(mdl, iok)
        if (newf < f) then
            ptg = mdl
            fg = newf
        else 
            ptd = mdl
            fd = newf
        end if 
    end do

    if (i == MAXITER) then
        ier = iok - 1000
    else
        ier = iok
    end if

    mydtrinv = mdl
    
END FUNCTION mydtrinv
