// File: src/libdeflate_wrapper.c

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h> 
#include <R_ext/Memory.h> 
#include <libdeflate.h>

//=== finalizers =============================================================

static void compressor_finalizer(SEXP ptr) {
    struct libdeflate_compressor *c = (struct libdeflate_compressor*) R_ExternalPtrAddr(ptr);
    if (c) libdeflate_free_compressor(c);
    R_ClearExternalPtr(ptr);
}

static void decompressor_finalizer(SEXP ptr) {
    struct libdeflate_decompressor *d = (struct libdeflate_decompressor*) R_ExternalPtrAddr(ptr);
    if (d) libdeflate_free_decompressor(d);
    R_ClearExternalPtr(ptr);
}

//=== compressor wrappers ====================================================

// .Call("C_alloc_compressor", integer level)
SEXP C_alloc_compressor(SEXP level_SEXP) {
    int level = INTEGER(level_SEXP)[0];
    struct libdeflate_compressor *c = libdeflate_alloc_compressor(level);
    if (!c) Rf_error("libdeflate_alloc_compressor(%d) failed", level);

    SEXP ext = PROTECT(R_MakeExternalPtr(c, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext, compressor_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

// .Call("C_deflate_compress", extptr, raw_input)
SEXP C_deflate_compress(SEXP compressor_SEXP, SEXP input_raw) {
    struct libdeflate_compressor *c = R_ExternalPtrAddr(compressor_SEXP);
    if (!c) Rf_error("Invalid compressor");

    R_xlen_t in_len = XLENGTH(input_raw);
    const void *in_ptr = RAW(input_raw);

    size_t bound = libdeflate_deflate_compress_bound(c, (size_t)in_len);
    void *tmp = R_alloc(bound, 1);
    size_t actual = libdeflate_deflate_compress(c, in_ptr, (size_t)in_len, tmp, bound);
    if (actual == 0) Rf_error("compression failed");

    SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)actual));
    memcpy(RAW(out), tmp, actual);
    UNPROTECT(1);
    return out;
}

//=== decompressor wrappers ==================================================

// .Call("C_alloc_decompressor")
SEXP C_alloc_decompressor(SEXP dummy) {
    struct libdeflate_decompressor *d = libdeflate_alloc_decompressor();
    if (!d) Rf_error("libdeflate_alloc_decompressor() failed");

    SEXP ext = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext, decompressor_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

// .Call("C_deflate_decompress", extptr, raw_input, integer expected_output_length)
SEXP C_deflate_decompress(SEXP decompressor_SEXP, SEXP input_raw, SEXP out_len_SEXP) {
    struct libdeflate_decompressor *d = (struct libdeflate_decompressor*) R_ExternalPtrAddr(decompressor_SEXP);
    if (!d) Rf_error("Invalid decompressor external pointer");

    const void *in_ptr = RAW(input_raw);
    size_t in_len = (size_t) XLENGTH(input_raw);
    size_t out_len_expected = (size_t) INTEGER(out_len_SEXP)[0];

    SEXP out_raw = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)out_len_expected));
    void *out_ptr = RAW(out_raw);
    size_t actual_out;
    enum libdeflate_result res = libdeflate_deflate_decompress(
        d, in_ptr, in_len, out_ptr, out_len_expected, &actual_out
    );

    if (res != LIBDEFLATE_SUCCESS)
        Rf_error("Decompression failed (code %d)", res);

    if (actual_out < out_len_expected) {
        R_xlen_t new_len = (R_xlen_t) actual_out;
        Rf_xlengthgets(out_raw, new_len);  // adjust user‐visible length
    }

    UNPROTECT(1);
    return out_raw;
}

//=== registration ===========================================================

static const R_CallMethodDef CallEntries[] = {
    {"C_alloc_compressor",   (DL_FUNC) &C_alloc_compressor,   1},
    {"C_deflate_compress",   (DL_FUNC) &C_deflate_compress,   2},
    {"C_alloc_decompressor", (DL_FUNC) &C_alloc_decompressor, 1},
    {"C_deflate_decompress", (DL_FUNC) &C_deflate_decompress, 3},
    {NULL, NULL, 0}
};

void R_init_libdeflatewrapper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
