/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *******************************
 * limits for time index types *
 *******************************
 */


#include "compiler.h"
#include "tlimits.h"
#include "datetime.h"
#include "calendar.h"


TIND__ATTRIBUTE_FUNCTION
SEXP
tlimits(SEXP stype)
{
    char type = *CHAR(STRING_ELT(stype, 0));
    SEXP sy;

    if ((type == 't') || (type == 'h')) {
        sy = PROTECT(allocVector(REALSXP, 2));
        double *y = REAL(sy);
        y[0] = (type == 't') ? VALID_T_MIN : 0.;
        y[1] = (type == 't') ? VALID_T_MAX : 86400.;
    } else {
        sy = PROTECT(allocVector(INTSXP, 2));
        int *y = INTEGER(sy);
        switch (type) {
            case 'y': y[0] = VALID_Y_MIN; y[1] = VALID_Y_MAX; break;
            case 'q': y[0] = VALID_Q_MIN; y[1] = VALID_Q_MAX; break;
            case 'm': y[0] = VALID_M_MIN; y[1] = VALID_M_MAX; break;
            case 'w': y[0] = VALID_W_MIN; y[1] = VALID_W_MAX; break;
            case 'd': y[0] = VALID_D_MIN; y[1] = VALID_D_MAX; break;
            default: break;
        }
    }

    UNPROTECT(1);
    return sy;
}


