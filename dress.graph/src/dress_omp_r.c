/*
 * dress_omp_r.c -- R bridge for OpenMP-parallel DRESS
 *
 * Identical to the CPU bridge but calls dress_fit_omp and
 * dress_delta_fit_omp_strided instead of the sequential versions.
 */

#include <R.h>
#include <Rinternals.h>

#include "dress/dress.h"
#include "dress/omp/dress_omp.h"

/* ------------------------------------------------------------------ */
/*  dress_fit_omp                                                      */
/* ------------------------------------------------------------------ */
SEXP C_dress_fit_omp(SEXP n_vertices_,
                     SEXP sources_,
                     SEXP targets_,
                     SEXP weights_,
                     SEXP node_weights_,
                     SEXP variant_,
                     SEXP max_iterations_,
                     SEXP epsilon_,
                     SEXP precompute_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int variant        = INTEGER(variant_)[0];
    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];
    int precompute     = INTEGER(precompute_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    double *W = NULL;
    double *NW = NULL;

    if (!U || !V) { free(U); free(V); error("dress_fit_omp: malloc failed"); }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_fit_omp: malloc failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
            free(U); free(V); if(W) free(W);
            error("dress_fit_omp: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        if (!NW) { free(U); free(V); if(W) free(W); error("dress_fit_omp: malloc failed"); }
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) error("dress_fit_omp: dress_init_graph returned NULL");

    int iterations = 0;
    double delta = 0.0;
    dress_fit_omp(g, max_iterations, epsilon, &iterations, &delta);

    SEXP result = PROTECT(allocVector(VECSXP, g->NW ? 8 : 7));
    SEXP names  = PROTECT(allocVector(STRSXP, g->NW ? 8 : 7));
    SET_STRING_ELT(names, 0, mkChar("sources"));
    SET_STRING_ELT(names, 1, mkChar("targets"));
    SET_STRING_ELT(names, 2, mkChar("edge_dress"));
    SET_STRING_ELT(names, 3, mkChar("edge_weight"));
    SET_STRING_ELT(names, 4, mkChar("vertex_dress"));
    SET_STRING_ELT(names, 5, mkChar("iterations"));
    SET_STRING_ELT(names, 6, mkChar("delta"));
    if (g->NW) SET_STRING_ELT(names, 7, mkChar("vertex_weights"));
    setAttrib(result, R_NamesSymbol, names);

    SEXP r_s = PROTECT(allocVector(INTSXP, E));
    SEXP r_t = PROTECT(allocVector(INTSXP, E));
    SEXP r_ed = PROTECT(allocVector(REALSXP, E));
    SEXP r_ew = PROTECT(allocVector(REALSXP, E));
    SEXP r_nd = PROTECT(allocVector(REALSXP, N));
    SEXP r_it = PROTECT(ScalarInteger(iterations));
    SEXP r_dl = PROTECT(ScalarReal(delta));

    memcpy(INTEGER(r_s), g->U, E * sizeof(int));
    memcpy(INTEGER(r_t), g->V, E * sizeof(int));
    memcpy(REAL(r_ed), g->edge_dress, E * sizeof(double));
    memcpy(REAL(r_ew), g->edge_weight, E * sizeof(double));
    memcpy(REAL(r_nd), g->vertex_dress, N * sizeof(double));

    SET_VECTOR_ELT(result, 0, r_s);
    SET_VECTOR_ELT(result, 1, r_t);
    SET_VECTOR_ELT(result, 2, r_ed);
    SET_VECTOR_ELT(result, 3, r_ew);
    SET_VECTOR_ELT(result, 4, r_nd);
    SET_VECTOR_ELT(result, 5, r_it);
    SET_VECTOR_ELT(result, 6, r_dl);

    if (g->NW) {
        SEXP r_nw = PROTECT(allocVector(REALSXP, N));
        memcpy(REAL(r_nw), g->NW, N * sizeof(double));
        SET_VECTOR_ELT(result, 7, r_nw);
        UNPROTECT(1);
    }

    dress_free_graph(g);
    UNPROTECT(9);
    return result;
}

/* ------------------------------------------------------------------ */
/*  dress_delta_fit_omp                                                */
/* ------------------------------------------------------------------ */

static SEXP build_histogram_df_omp(const dress_hist_pair_t *hist, int hist_size) {
    SEXP df = PROTECT(allocVector(VECSXP, 2));
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SEXP values = PROTECT(allocVector(REALSXP, hist_size));
    SEXP counts = PROTECT(allocVector(REALSXP, hist_size));
    SEXP row_names = PROTECT(allocVector(INTSXP, 2));
    SEXP class_name = PROTECT(mkString("data.frame"));

    SET_STRING_ELT(names, 0, mkChar("value"));
    SET_STRING_ELT(names, 1, mkChar("count"));
    setAttrib(df, R_NamesSymbol, names);

    for (int i = 0; i < hist_size; i++) {
        REAL(values)[i] = hist[i].value;
        REAL(counts)[i] = (double)hist[i].count;
    }

    SET_VECTOR_ELT(df, 0, values);
    SET_VECTOR_ELT(df, 1, counts);

    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -hist_size;
    setAttrib(df, R_RowNamesSymbol, row_names);
    setAttrib(df, R_ClassSymbol, class_name);

    UNPROTECT(6);
    return df;
}

SEXP C_delta_dress_fit_omp(SEXP n_vertices_,
                           SEXP sources_, SEXP targets_,
                           SEXP weights_, SEXP node_weights_,
                           SEXP k_, SEXP variant_,
                           SEXP max_iterations_, SEXP epsilon_,
                           SEXP n_samples_, SEXP seed_,
                           SEXP precompute_, SEXP keep_multisets_,
                           SEXP compute_histogram_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int k  = INTEGER(k_)[0];
    int variant        = INTEGER(variant_)[0];
    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];
    int n_samples      = INTEGER(n_samples_)[0];
    unsigned int seed  = (unsigned int)INTEGER(seed_)[0];
    int precompute     = INTEGER(precompute_)[0];
    int keep_ms        = INTEGER(keep_multisets_)[0];
    int compute_hist   = INTEGER(compute_histogram_)[0];
    int offset         = 0;
    int stride         = 1;

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    double *W = NULL, *NW = NULL;
    if (!U || !V) { free(U); free(V); error("dress_delta_fit_omp: malloc failed"); }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        memcpy(W, REAL(weights_), E * sizeof(double));
    }
    if (node_weights_ != R_NilValue) {
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) error("dress_delta_fit_omp: dress_init_graph returned NULL");

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;

    dress_hist_pair_t *hist = dress_delta_fit_omp_strided(
        g, k, max_iterations, epsilon,
        n_samples, seed,
        compute_hist ? &hist_size : NULL,
        keep_ms, keep_ms ? &ms_ptr : NULL,
        &num_sub, offset, stride);

    SEXP result = PROTECT(allocVector(VECSXP, 3));
    SEXP rnames = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(rnames, 0, mkChar("histogram"));
    SET_STRING_ELT(rnames, 1, mkChar("multisets"));
    SET_STRING_ELT(rnames, 2, mkChar("num_subgraphs"));
    setAttrib(result, R_NamesSymbol, rnames);

    SET_VECTOR_ELT(result, 0,
                   hist ? build_histogram_df_omp(hist, hist_size) : R_NilValue);
    SET_VECTOR_ELT(result, 2, ScalarReal((double)num_sub));

    if (keep_ms && ms_ptr && num_sub > 0) {
        R_xlen_t total = (R_xlen_t)num_sub * E;
        SEXP r_ms = PROTECT(allocVector(REALSXP, total));
        memcpy(REAL(r_ms), ms_ptr, total * sizeof(double));

        SEXP dim = PROTECT(allocVector(INTSXP, 2));
        INTEGER(dim)[0] = E;
        INTEGER(dim)[1] = (int)num_sub;
        setAttrib(r_ms, R_DimSymbol, dim);

        SET_VECTOR_ELT(result, 1, r_ms);
        UNPROTECT(2);
        free(ms_ptr);
    } else {
        SET_VECTOR_ELT(result, 1, R_NilValue);
    }

    if (hist) free(hist);
    dress_free_graph(g);
    UNPROTECT(2);
    return result;
}

/* ------------------------------------------------------------------ */
/*  dress_nabla_fit_omp                                                */
/* ------------------------------------------------------------------ */
SEXP C_nabla_dress_fit_omp(SEXP n_vertices_,
                           SEXP sources_, SEXP targets_,
                           SEXP weights_, SEXP node_weights_,
                           SEXP k_, SEXP variant_,
                           SEXP max_iterations_, SEXP epsilon_,
                           SEXP n_samples_, SEXP seed_,
                           SEXP precompute_, SEXP keep_multisets_,
                           SEXP compute_histogram_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int k  = INTEGER(k_)[0];
    int variant        = INTEGER(variant_)[0];
    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];
    int n_samples      = INTEGER(n_samples_)[0];
    unsigned int seed  = (unsigned int)INTEGER(seed_)[0];
    int precompute     = INTEGER(precompute_)[0];
    int keep_ms        = INTEGER(keep_multisets_)[0];
    int compute_hist   = INTEGER(compute_histogram_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    double *W = NULL, *NW = NULL;
    if (!U || !V) { free(U); free(V); error("dress_nabla_fit_omp: malloc failed"); }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        memcpy(W, REAL(weights_), E * sizeof(double));
    }
    if (node_weights_ != R_NilValue) {
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) error("dress_nabla_fit_omp: dress_init_graph returned NULL");

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;

    dress_hist_pair_t *hist = dress_nabla_fit_omp(
        g, k, max_iterations, epsilon,
        n_samples, seed,
        compute_hist ? &hist_size : NULL,
        keep_ms, keep_ms ? &ms_ptr : NULL,
        &num_tuples);

    SEXP result = PROTECT(allocVector(VECSXP, 3));
    SEXP rnames = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(rnames, 0, mkChar("histogram"));
    SET_STRING_ELT(rnames, 1, mkChar("multisets"));
    SET_STRING_ELT(rnames, 2, mkChar("num_tuples"));
    setAttrib(result, R_NamesSymbol, rnames);

    SET_VECTOR_ELT(result, 0,
                   hist ? build_histogram_df_omp(hist, hist_size) : R_NilValue);
    SET_VECTOR_ELT(result, 2, ScalarReal((double)num_tuples));

    if (keep_ms && ms_ptr && num_tuples > 0) {
        R_xlen_t total = (R_xlen_t)num_tuples * E;
        SEXP r_ms = PROTECT(allocVector(REALSXP, total));
        memcpy(REAL(r_ms), ms_ptr, total * sizeof(double));

        SEXP dim = PROTECT(allocVector(INTSXP, 2));
        INTEGER(dim)[0] = E;
        INTEGER(dim)[1] = (int)num_tuples;
        setAttrib(r_ms, R_DimSymbol, dim);

        SET_VECTOR_ELT(result, 1, r_ms);
        UNPROTECT(2);
        free(ms_ptr);
    } else {
        SET_VECTOR_ELT(result, 1, R_NilValue);
    }

    if (hist) free(hist);
    dress_free_graph(g);
    UNPROTECT(2);
    return result;
}
