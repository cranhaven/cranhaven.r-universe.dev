#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


SEXP as_scalar_logical(SEXP x)
{
    return ScalarLogical(asLogical(x));
}


SEXP as_scalar_integer(SEXP x)
{
    return ScalarInteger(asInteger(x));
}


SEXP as_scalar_real(SEXP x)
{
    return ScalarReal(asReal(x));
}


SEXP as_scalar_complex(SEXP x)
{
    return ScalarComplex(asComplex(x));
}


SEXP as_scalar_number(SEXP x, SEXP strict)
{
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP: case REALSXP: return as_scalar_real(x);
    default:
        if (asLogical(strict)) {
            Rcomplex value = asComplex(x);
            if (ISNAN(value.r) || ISNAN(value.i))
                return ScalarReal(NA_REAL);
            else if (value.i == 0)
                return ScalarReal(value.r);
            else return ScalarComplex(value);
        }
        return as_scalar_complex(x);
    }
}


SEXP as_scalar_string(SEXP x)
{
    return ScalarString(asChar(x));
}


SEXP as_scalar(SEXP x)
{
    switch(TYPEOF(x)) {
    case LGLSXP: return as_scalar_logical(x);
    case INTSXP: return as_scalar_integer(x);
    case REALSXP: return as_scalar_real(x);
    case CPLXSXP: return as_scalar_complex(x);
    default: return as_scalar_string(x);
    }
}





SEXP as_numbers(SEXP x, SEXP strict)
{
    SEXP value;
    switch(TYPEOF(x)) {
    case LGLSXP: case INTSXP: value = PROTECT(coerceVector(x, REALSXP)); break;
    case REALSXP: case CPLXSXP: value = PROTECT(Rf_duplicate(x)); break;
    default: value = PROTECT(coerceVector(x, CPLXSXP)); break;
    }
    if (ATTRIB(value) != R_NilValue) {
        SET_ATTRIB(value, R_NilValue);
        if (OBJECT(value)) SET_OBJECT(value, 0);
        if (IS_S4_OBJECT(value)) UNSET_S4_OBJECT(value);
    }
    if (TYPEOF(value) == CPLXSXP && asLogical(strict)) {
        int coerce = 1;
        Rcomplex c, *cvalue = COMPLEX(value);
        R_xlen_t i, len = xlength(value);
        for (i = 0; i < len; i++) {
            c = cvalue[i];
            if (!(ISNAN(c.r) || ISNAN(c.i) || c.i == 0)) {
                coerce = 0;
                break;
            }
        }
        if (coerce) {
            UNPROTECT(1);
            return coerceVector(value, REALSXP);
        }
    }
    UNPROTECT(1);
    return value;
}





SEXP hypot_vectorized(SEXP x, SEXP y)
{
    SEXP value;
    R_xlen_t lenx, leny, len, i;
    lenx = xlength(x);
    leny = xlength(y);
    if (lenx == 0 || leny == 0) return allocVector(REALSXP, 0);

    x = PROTECT(coerceVector(x, REALSXP));
    y = PROTECT(coerceVector(y, REALSXP));

    len = fmax2(lenx, leny);
    value = PROTECT(allocVector(REALSXP, len));
    double *rx = REAL(x), *ry = REAL(y), *rvalue = REAL(value);

    for (i = 0; i < len; i++) {
        rvalue[i] = hypot(rx[i % lenx], ry[i % leny]);
    }
    UNPROTECT(3);
    return value;
}





static const R_CallMethodDef callRoutines[] = {
    {"as.scalar.logical", (DL_FUNC) &as_scalar_logical, 1},
    {"as.scalar.integer", (DL_FUNC) &as_scalar_integer, 1},
    {"as.scalar.real"   , (DL_FUNC) &as_scalar_real   , 1},
    {"as.scalar.complex", (DL_FUNC) &as_scalar_complex, 1},
    {"as.scalar.number" , (DL_FUNC) &as_scalar_number , 2},
    {"as.scalar.string" , (DL_FUNC) &as_scalar_string , 1},
    {"as.scalar"        , (DL_FUNC) &as_scalar        , 1},
    {"as.numbers"       , (DL_FUNC) &as_numbers       , 2},
    {"hypot"            , (DL_FUNC) &hypot_vectorized , 2},
    {NULL, NULL, 0}
};


void R_init_essentials(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callRoutines, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
