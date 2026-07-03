// SPDX-License-Identifier: MIT
// R <-> nirs4all-datasets C ABI glue. Each .Call entry drives the stable n4ds_ JSON
// surface: a per-call context carries the error buffer; on a non-OK status the message
// is raised as an R error; owned result strings are copied into an R string and freed
// with n4ds_string_free.
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>

#include "nirs4all_datasets.h"

// Read element 0 of an R character vector, or NULL for NULL / NA / empty.
static const char *opt_cstr(SEXP s) {
    if (s == R_NilValue || TYPEOF(s) != STRSXP || LENGTH(s) == 0)
        return NULL;
    SEXP e = STRING_ELT(s, 0);
    if (e == NA_STRING)
        return NULL;
    return CHAR(e);
}

static const char *req_cstr(SEXP s, const char *what) {
    const char *p = opt_cstr(s);
    if (p == NULL)
        Rf_error("%s must be a non-NA character scalar", what);
    return p;
}

// The three data calls share the (ctx, const char*, const char*, char**) shape.
typedef enum n4ds_status_t (*json_fn)(struct n4ds_context_t *, const char *, const char *, char **);

static SEXP call_json(json_fn fn, SEXP arg1, SEXP arg2, const char *n1, const char *n2) {
    const char *a1 = req_cstr(arg1, n1);
    const char *a2 = req_cstr(arg2, n2);
    struct n4ds_context_t *ctx = NULL;
    if (n4ds_context_create(&ctx) != N4DS_OK)
        Rf_error("n4ds: failed to create context");
    char *out = NULL;
    enum n4ds_status_t st = fn(ctx, a1, a2, &out);
    if (st != N4DS_OK) {
        const char *msg = n4ds_context_last_error(ctx);
        char buf[1024];
        snprintf(buf, sizeof(buf), "%s", msg ? msg : "unknown error");
        n4ds_context_destroy(ctx);
        Rf_error("n4ds error: %s", buf);
    }
    SEXP res = PROTECT(Rf_mkString(out ? out : ""));
    n4ds_string_free(out);
    n4ds_context_destroy(ctx);
    UNPROTECT(1);
    return res;
}

SEXP r_n4ds_resolve(SEXP index_json, SEXP dataset_id) {
    return call_json(n4ds_resolve, index_json, dataset_id, "index_json", "dataset_id");
}

SEXP r_n4ds_fetch(SEXP resolved_json, SEXP opts_json) {
    return call_json(n4ds_fetch, resolved_json, opts_json, "resolved_json", "opts_json");
}

SEXP r_n4ds_verify_cached(SEXP resolved_json, SEXP dir) {
    return call_json(n4ds_verify_cached, resolved_json, dir, "resolved_json", "dir");
}

SEXP r_n4ds_abi_version(void) {
    char *v = n4ds_abi_version();
    SEXP res = PROTECT(Rf_mkString(v ? v : ""));
    n4ds_string_free(v);
    UNPROTECT(1);
    return res;
}

static const R_CallMethodDef call_methods[] = {
    {"r_n4ds_resolve", (DL_FUNC)&r_n4ds_resolve, 2},
    {"r_n4ds_fetch", (DL_FUNC)&r_n4ds_fetch, 2},
    {"r_n4ds_verify_cached", (DL_FUNC)&r_n4ds_verify_cached, 2},
    {"r_n4ds_abi_version", (DL_FUNC)&r_n4ds_abi_version, 0},
    {NULL, NULL, 0}};

void R_init_nirs4alldatasets(DllInfo *info) {
    R_registerRoutines(info, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
