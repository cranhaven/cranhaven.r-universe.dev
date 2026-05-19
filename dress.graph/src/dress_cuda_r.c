/*
 * dress_cuda_r.c -- R bridge for the CUDA-accelerated DRESS library.
 *
 * Same API as dress_r.c but calls dress_fit_cuda / dress_delta_fit_cuda.
 * Exposed via the `cuda` environment in R:
 *
 *   library(dress)
 *   cuda$dress_fit(4, sources, targets)
 *
 * Only compiled when DRESS_CUDA is defined (requires CUDA toolkit).
 */

#ifdef DRESS_CUDA

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "dress/dress.h"
#include "dress/cuda/dress_cuda.h"

static SEXP build_histogram_df(const dress_hist_pair_t *hist, int hist_size) {
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

/* ------------------------------------------------------------------ */
/*  dress_fit  (CUDA)                                                  */
/* ------------------------------------------------------------------ */
SEXP C_dress_fit_cuda(SEXP n_vertices_,
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

    if (!U || !V) {
        free(U); free(V);
        error("dress_fit_cuda: memory allocation failed");
    }

    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_fit_cuda: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
            free(U); free(V); if(W) free(W);
            error("dress_fit_cuda: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant,
                                         precompute);
    if (!g) {
        error("dress_fit_cuda: dress_init_graph returned NULL");
    }

    int iterations = 0;
    double delta = 0.0;
    dress_fit_cuda(g, max_iterations, epsilon, &iterations, &delta);

    /* Build result list — identical structure to C_dress_fit */
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

    SEXP r_sources    = PROTECT(allocVector(INTSXP,  g->E));
    SEXP r_targets    = PROTECT(allocVector(INTSXP,  g->E));
    SEXP r_edge_dress = PROTECT(allocVector(REALSXP, g->E));
    SEXP r_edge_wt    = PROTECT(allocVector(REALSXP, g->E));
    SEXP r_node_dress = PROTECT(allocVector(REALSXP, g->N));
    SEXP r_iters      = PROTECT(ScalarInteger(iterations));
    SEXP r_delta      = PROTECT(ScalarReal(delta));

    memcpy(INTEGER(r_sources),    g->U,          g->E * sizeof(int));
    memcpy(INTEGER(r_targets),    g->V,          g->E * sizeof(int));
    memcpy(REAL(r_edge_dress),    g->edge_dress, g->E * sizeof(double));
    memcpy(REAL(r_edge_wt),       g->edge_weight,g->E * sizeof(double));
    memcpy(REAL(r_node_dress),    g->vertex_dress, g->N * sizeof(double));

    SET_VECTOR_ELT(result, 0, r_sources);
    SET_VECTOR_ELT(result, 1, r_targets);
    SET_VECTOR_ELT(result, 2, r_edge_dress);
    SET_VECTOR_ELT(result, 3, r_edge_wt);
    SET_VECTOR_ELT(result, 4, r_node_dress);
    SET_VECTOR_ELT(result, 5, r_iters);
    SET_VECTOR_ELT(result, 6, r_delta);

    if (g->NW) {
        SEXP r_node_wt = PROTECT(allocVector(REALSXP, g->N));
        memcpy(REAL(r_node_wt), g->NW, g->N * sizeof(double));
        SET_VECTOR_ELT(result, 7, r_node_wt);
        UNPROTECT(1);
    }

    dress_free_graph(g);
    UNPROTECT(9);
    return result;
}

/* ------------------------------------------------------------------ */
/*  dress_delta_fit  (CUDA)                                            */
/* ------------------------------------------------------------------ */
SEXP C_delta_dress_fit_cuda(SEXP n_vertices_,
                            SEXP sources_,
                            SEXP targets_,
                            SEXP weights_,
                            SEXP node_weights_,
                            SEXP k_,
                            SEXP variant_,
                            SEXP max_iterations_,
                            SEXP epsilon_,
                            SEXP n_samples_,
                            SEXP seed_,
                            SEXP precompute_,
                            SEXP keep_multisets_,
                            SEXP compute_histogram_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int k              = INTEGER(k_)[0];
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
    if (!U || !V) {
        free(U); free(V);
        error("dress_delta_fit_cuda: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_delta_fit_cuda: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_delta_fit_cuda: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_delta_fit_cuda: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;
    dress_hist_pair_t *hist = dress_delta_fit_cuda_strided(g, k, max_iterations, epsilon,
                                                           n_samples, seed,
                                                           compute_hist ? &hist_size : NULL,
                                                           keep_ms,
                                                           keep_ms ? &ms_ptr : NULL,
                                                           keep_ms ? &num_sub : NULL,
                                                           offset, stride);

    int n_fields = keep_ms ? 3 : 1;
    SEXP result = PROTECT(allocVector(VECSXP, n_fields));
    SEXP names  = PROTECT(allocVector(STRSXP, n_fields));

    SET_STRING_ELT(names, 0, mkChar("histogram"));
    if (keep_ms) {
        SET_STRING_ELT(names, 1, mkChar("multisets"));
        SET_STRING_ELT(names, 2, mkChar("num_subgraphs"));
    }
    setAttrib(result, R_NamesSymbol, names);

    SEXP r_hist = PROTECT(build_histogram_df(hist, hist_size));
    SET_VECTOR_ELT(result, 0, r_hist);
    UNPROTECT(1); /* r_hist */

    if (keep_ms) {
        if (ms_ptr && num_sub > 0) {
            SEXP r_ms = PROTECT(allocMatrix(REALSXP, (int)num_sub, E));
            for (int64_t s = 0; s < num_sub; s++)
                for (int e = 0; e < E; e++)
                    REAL(r_ms)[e * (int)num_sub + (int)s] = ms_ptr[s * E + e];
            SET_VECTOR_ELT(result, 1, r_ms);
            UNPROTECT(1);
            free(ms_ptr);
        } else {
            SET_VECTOR_ELT(result, 1, allocMatrix(REALSXP, 0, 0));
        }
        SET_VECTOR_ELT(result, 2, ScalarInteger((int)num_sub));
    }

    free(hist);
    dress_free_graph(g);
    UNPROTECT(3);
    return result;
}

/* ------------------------------------------------------------------ */
/*  dress_nabla_fit  (CUDA)                                            */
/* ------------------------------------------------------------------ */
SEXP C_nabla_dress_fit_cuda(SEXP n_vertices_,
                            SEXP sources_,
                            SEXP targets_,
                            SEXP weights_,
                            SEXP node_weights_,
                            SEXP k_,
                            SEXP variant_,
                            SEXP max_iterations_,
                            SEXP epsilon_,
                            SEXP n_samples_,
                            SEXP seed_,
                            SEXP precompute_,
                            SEXP keep_multisets_,
                            SEXP compute_histogram_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int k              = INTEGER(k_)[0];
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
    if (!U || !V) {
        free(U); free(V);
        error("dress_nabla_fit_cuda: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_nabla_fit_cuda: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_nabla_fit_cuda: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_nabla_fit_cuda: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;
    dress_hist_pair_t *hist = dress_nabla_fit_cuda(g, k, max_iterations, epsilon,
                                                   n_samples, seed,
                                                   compute_hist ? &hist_size : NULL,
                                                   keep_ms,
                                                   keep_ms ? &ms_ptr : NULL,
                                                   keep_ms ? &num_tuples : NULL);

    int n_fields = keep_ms ? 3 : 1;
    SEXP result = PROTECT(allocVector(VECSXP, n_fields));
    SEXP names  = PROTECT(allocVector(STRSXP, n_fields));

    SET_STRING_ELT(names, 0, mkChar("histogram"));
    if (keep_ms) {
        SET_STRING_ELT(names, 1, mkChar("multisets"));
        SET_STRING_ELT(names, 2, mkChar("num_tuples"));
    }
    setAttrib(result, R_NamesSymbol, names);

    SEXP r_hist = PROTECT(build_histogram_df(hist, hist_size));
    SET_VECTOR_ELT(result, 0, r_hist);
    UNPROTECT(1);

    if (keep_ms) {
        if (ms_ptr && num_tuples > 0) {
            SEXP r_ms = PROTECT(allocMatrix(REALSXP, (int)num_tuples, E));
            for (int64_t s = 0; s < num_tuples; s++)
                for (int e = 0; e < E; e++)
                    REAL(r_ms)[e * (int)num_tuples + (int)s] = ms_ptr[s * E + e];
            SET_VECTOR_ELT(result, 1, r_ms);
            UNPROTECT(1);
            free(ms_ptr);
        } else {
            SET_VECTOR_ELT(result, 1, allocMatrix(REALSXP, 0, 0));
        }
        SET_VECTOR_ELT(result, 2, ScalarInteger((int)num_tuples));
    }

    free(hist);
    dress_free_graph(g);
    UNPROTECT(3);
    return result;
}

/* ------------------------------------------------------------------ */
/*  dress_fit_cuda_obj — CUDA fit on persistent graph pointer          */
/* ------------------------------------------------------------------ */
SEXP C_dress_fit_cuda_obj(SEXP ptr_,
                          SEXP max_iterations_,
                          SEXP epsilon_) {

    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr_);
    if (!g) error("dress_fit_cuda_obj: graph already closed");

    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];

    int iterations = 0;
    double delta = 0.0;
    dress_fit_cuda(g, max_iterations, epsilon, &iterations, &delta);

    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SEXP names  = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("iterations"));
    SET_STRING_ELT(names, 1, mkChar("delta"));
    setAttrib(result, R_NamesSymbol, names);
    SET_VECTOR_ELT(result, 0, ScalarInteger(iterations));
    SET_VECTOR_ELT(result, 1, ScalarReal(delta));
    UNPROTECT(2);
    return result;
}

#endif /* DRESS_CUDA */

/* Prevent -Wempty-translation-unit when CUDA is unavailable. */
typedef int dress_cuda_r_unused_t;
