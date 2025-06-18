#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h> 

#include <Imath/ImathConfig.h>
#include <Imath/ImathMatrix.h>
#include <Imath/ImathVec.h>

extern "C" {
    
SEXP C_print_imath_version(void) {
    const char version[] = IMATH_VERSION_STRING;
    Rprintf("Imath version %s", version);
    return(R_NilValue);
}

/**
 * Rotates a 3D point using Imath's matrix operations
 * 
 * @param point An R numeric vector with 3 elements (x, y, z)
 * @param angles An R numeric vector with 3 elements (rotation angles in radians)
 * @return The rotated point as an R numeric vector
 */
SEXP imath_rotate_point(SEXP point, SEXP angles) {
    // Validate inputs
    if (TYPEOF(point) != REALSXP || LENGTH(point) != 3) {
        Rf_error("'point' must be a numeric vector of length 3");
    }
    if (TYPEOF(angles) != REALSXP || LENGTH(angles) != 3) {
        Rf_error("'angles' must be a numeric vector of length 3");
    }
    
    // Extract point coordinates
    double *point_ptr = REAL(point);
    Imath::V3f p((float)point_ptr[0], (float)point_ptr[1], (float)point_ptr[2]);
    
    // Extract rotation angles
    double *angles_ptr = REAL(angles);
    Imath::V3f rot((float)angles_ptr[0], (float)angles_ptr[1], (float)angles_ptr[2]);
    
    // Create identity matrix
    Imath::M44f M;
    M.makeIdentity();
    
    // Create rotation matrix
    Imath::M44f R;
    R.makeIdentity();
    R.rotate(rot);
    
    // Apply rotation
    M = R * M;
    
    // Transform the point
    Imath::V3f result;
    M.multVecMatrix(p, result);
    
    // Create and populate result vector
    SEXP r_result = PROTECT(Rf_allocVector(REALSXP, 3));
    REAL(r_result)[0] = (double)result.x;
    REAL(r_result)[1] = (double)result.y;
    REAL(r_result)[2] = (double)result.z;
    
    // Add names to result vector for R
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, Rf_mkChar("x"));
    SET_STRING_ELT(names, 1, Rf_mkChar("y"));
    SET_STRING_ELT(names, 2, Rf_mkChar("z"));
    Rf_setAttrib(r_result, R_NamesSymbol, names);
    
    UNPROTECT(2);
    return r_result;
}

//=== registration ===========================================================

static const R_CallMethodDef CallEntries[] = {
    {"imath_rotate_point", (DL_FUNC) &imath_rotate_point, 2},
    {"C_print_imath_version",   (DL_FUNC) &C_print_imath_version,   0},
    {NULL, NULL, 0}
};

void R_init_libimathwrapper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

}
