/*
 * dress_r.c -- R bridge for the DRESS library
 *
 * Provides .Call-compatible C functions that wrap the DRESS C API.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "dress/dress.h"

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
/*  dress_fit                                                          */
/* ------------------------------------------------------------------ */
SEXP C_dress_fit(SEXP n_vertices_,
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

    /* Allocate copies of sources/targets/weights with malloc
       (dress_init_graph takes ownership and frees them). */
    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    double *W = NULL;
    double *NW = NULL;

    if (!U || !V) {
        free(U); free(V);
        error("dress_fit: memory allocation failed");
    }

    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_fit: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
            free(U); free(V); if(W) free(W);
            error("dress_fit: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        if (!NW) { free(U); free(V); if(W) free(W); error("dress_fit: memory allocation failed"); }
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant,
                                         precompute);
    if (!g) {
        error("dress_fit: dress_init_graph returned NULL");
    }

    int iterations = 0;
    double delta = 0.0;
    dress_fit(g, max_iterations, epsilon, &iterations, &delta);

    /* Build result list */
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
/*  dress_version                                                      */
/* ------------------------------------------------------------------ */
SEXP C_dress_version(void) {
    return ScalarString(mkChar("0.8.3"));
}

/* ------------------------------------------------------------------ */
/*  dress_delta_fit                                                    */
/* ------------------------------------------------------------------ */
SEXP C_delta_dress_fit(SEXP n_vertices_,
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

    /* Allocate copies (dress_init_graph takes ownership). */
    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_delta_fit: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_delta_fit: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_delta_fit: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_delta_fit: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;
    dress_hist_pair_t *hist = dress_delta_fit_strided(g, k, max_iterations, epsilon,
                                                      n_samples, seed,
                                                      compute_hist ? &hist_size : NULL,
                                                      keep_ms,
                                                      keep_ms ? &ms_ptr : NULL,
                                                      keep_ms ? &num_sub : NULL,
                                                      offset, stride);

    /* Build result list */
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

    /* multisets and num_subgraphs (when requested) */
    if (keep_ms) {
        if (ms_ptr && num_sub > 0) {
            /* R matrix is column-major: matrix[row, col] = data[col * nrow + row]
               C multisets are row-major: ms_ptr[s * E + e]
               R matrix(num_sub, E): result[s, e] = data[(e-1)*num_sub + (s-1)]
               So we transpose during copy. */
            SEXP r_ms = PROTECT(allocMatrix(REALSXP, (int)num_sub, E));
            for (int64_t s = 0; s < num_sub; s++)
                for (int e = 0; e < E; e++)
                    REAL(r_ms)[e * (int)num_sub + (int)s] = ms_ptr[s * E + e];
            SET_VECTOR_ELT(result, 1, r_ms);
            UNPROTECT(1); /* r_ms */
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

SEXP C_nabla_dress_fit(SEXP n_vertices_,
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

    /* Allocate copies (dress_init_graph takes ownership). */
    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_nabla_fit: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_nabla_fit: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_nabla_fit: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_nabla_fit: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;
    dress_hist_pair_t *hist = dress_nabla_fit(g, k, max_iterations, epsilon,
                                              n_samples, seed,
                                              compute_hist ? &hist_size : NULL,
                                              keep_ms,
                                              keep_ms ? &ms_ptr : NULL,
                                              keep_ms ? &num_tuples : NULL);

    /* Build result list */
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
    UNPROTECT(1); /* r_hist */

    /* multisets and num_tuples (when requested) */
    if (keep_ms) {
        if (ms_ptr && num_tuples > 0) {
            SEXP r_ms = PROTECT(allocMatrix(REALSXP, (int)num_tuples, E));
            for (int64_t s = 0; s < num_tuples; s++)
                for (int e = 0; e < E; e++)
                    REAL(r_ms)[e * (int)num_tuples + (int)s] = ms_ptr[s * E + e];
            SET_VECTOR_ELT(result, 1, r_ms);
            UNPROTECT(1); /* r_ms */
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

#ifdef DRESS_CUDA
/* CUDA bridge (defined in dress_cuda_r.c) */
extern SEXP C_dress_fit_cuda(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_delta_dress_fit_cuda(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nabla_dress_fit_cuda(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_dress_fit_cuda_obj(SEXP, SEXP, SEXP);
#endif

/* OMP bridge (defined in dress_omp_r.c) */
extern SEXP C_dress_fit_omp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_delta_dress_fit_omp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nabla_dress_fit_omp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#ifdef DRESS_MPI
/* MPI bridge (defined in dress_mpi_r.c) */
extern SEXP C_delta_dress_fit_mpi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nabla_dress_fit_mpi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
#ifdef DRESS_CUDA
extern SEXP C_delta_dress_fit_mpi_cuda(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nabla_dress_fit_mpi_cuda(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
#endif
#ifdef _OPENMP
extern SEXP C_delta_dress_fit_mpi_omp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nabla_dress_fit_mpi_omp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
#endif
#endif

/* ------------------------------------------------------------------ */
/*  Persistent DRESS object (external pointer)                         */
/* ------------------------------------------------------------------ */

static void dress_graph_finalizer(SEXP ptr) {
    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr);
    if (g) {
        dress_free_graph(g);
        R_ClearExternalPtr(ptr);
    }
}

/* Create and return an external pointer wrapping a C dress_graph_t. */
SEXP C_dress_init(SEXP n_vertices_,
                  SEXP sources_,
                  SEXP targets_,
                  SEXP weights_,
                  SEXP node_weights_,
                  SEXP variant_,
                  SEXP precompute_) {

    int N  = INTEGER(n_vertices_)[0];
    int E  = LENGTH(sources_);
    int variant    = INTEGER(variant_)[0];
    int precompute = INTEGER(precompute_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    double *W = NULL;
    double *NW = NULL;

    if (!U || !V) {
        free(U); free(V);
        error("dress_init: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    if (weights_ != R_NilValue) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_init: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
            free(U); free(V); if(W) free(W);
            error("dress_init: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant,
                                         precompute);
    if (!g) {
        error("dress_init: dress_init_graph returned NULL");
    }

    SEXP ptr = PROTECT(R_MakeExternalPtr(g, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, dress_graph_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

/* Fit an already-initialized graph. Returns list(iterations, delta). */
SEXP C_dress_fit_obj(SEXP ptr_,
                     SEXP max_iterations_,
                     SEXP epsilon_) {

    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr_);
    if (!g) error("dress_fit_obj: graph already closed");

    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];

    int iterations = 0;
    double delta = 0.0;
    dress_fit(g, max_iterations, epsilon, &iterations, &delta);

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

/* Query DRESS value for an edge (existing or virtual). */
SEXP C_dress_get_obj(SEXP ptr_,
                     SEXP u_,
                     SEXP v_,
                     SEXP max_iterations_,
                     SEXP epsilon_,
                     SEXP edge_weight_) {

    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr_);
    if (!g) error("dress_get_obj: graph already closed");

    int u = INTEGER(u_)[0];
    int v = INTEGER(v_)[0];
    int max_iterations = INTEGER(max_iterations_)[0];
    double epsilon     = REAL(epsilon_)[0];
    double edge_weight = REAL(edge_weight_)[0];

    double val = dress_get(g, u, v, max_iterations, epsilon, edge_weight);
    return ScalarReal(val);
}

/* Extract current results without freeing. */
SEXP C_dress_result(SEXP ptr_) {
    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr_);
    if (!g) error("dress_result: graph already closed");

    SEXP result = PROTECT(allocVector(VECSXP, 5));
    SEXP names  = PROTECT(allocVector(STRSXP, 5));

    SET_STRING_ELT(names, 0, mkChar("sources"));
    SET_STRING_ELT(names, 1, mkChar("targets"));
    SET_STRING_ELT(names, 2, mkChar("edge_dress"));
    SET_STRING_ELT(names, 3, mkChar("edge_weight"));
    SET_STRING_ELT(names, 4, mkChar("vertex_dress"));
    setAttrib(result, R_NamesSymbol, names);

    SEXP r_sources    = PROTECT(allocVector(INTSXP,  g->E));
    SEXP r_targets    = PROTECT(allocVector(INTSXP,  g->E));
    SEXP r_edge_dress = PROTECT(allocVector(REALSXP, g->E));
    SEXP r_edge_wt    = PROTECT(allocVector(REALSXP, g->E));
    SEXP r_node_dress = PROTECT(allocVector(REALSXP, g->N));

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

    UNPROTECT(7);
    return result;
}

/* Explicitly free the underlying C graph. */
SEXP C_dress_close(SEXP ptr_) {
    p_dress_graph_t g = (p_dress_graph_t) R_ExternalPtrAddr(ptr_);
    if (g) {
        dress_free_graph(g);
        R_ClearExternalPtr(ptr_);
    }
    return R_NilValue;
}

/* ------------------------------------------------------------------ */
/*  Registration table                                                 */
/* ------------------------------------------------------------------ */
static const R_CallMethodDef callMethods[] = {
    {"C_dress_fit",            (DL_FUNC) &C_dress_fit,            9},
    {"C_delta_dress_fit",      (DL_FUNC) &C_delta_dress_fit,      14},
    {"C_nabla_dress_fit",      (DL_FUNC) &C_nabla_dress_fit,      14},
    {"C_dress_version",        (DL_FUNC) &C_dress_version,        0},
    {"C_dress_init",           (DL_FUNC) &C_dress_init,           7},
    {"C_dress_fit_obj",        (DL_FUNC) &C_dress_fit_obj,        3},
    {"C_dress_get_obj",        (DL_FUNC) &C_dress_get_obj,        6},
    {"C_dress_result",         (DL_FUNC) &C_dress_result,         1},
    {"C_dress_close",          (DL_FUNC) &C_dress_close,          1},
    {"C_dress_fit_omp",        (DL_FUNC) &C_dress_fit_omp,        9},
    {"C_delta_dress_fit_omp",  (DL_FUNC) &C_delta_dress_fit_omp,  14},
    {"C_nabla_dress_fit_omp",  (DL_FUNC) &C_nabla_dress_fit_omp,  14},
#ifdef DRESS_CUDA
    {"C_dress_fit_cuda",       (DL_FUNC) &C_dress_fit_cuda,       9},
    {"C_delta_dress_fit_cuda", (DL_FUNC) &C_delta_dress_fit_cuda, 14},
    {"C_nabla_dress_fit_cuda", (DL_FUNC) &C_nabla_dress_fit_cuda, 14},
    {"C_dress_fit_cuda_obj",   (DL_FUNC) &C_dress_fit_cuda_obj,   3},
#endif
#ifdef DRESS_MPI
    {"C_delta_dress_fit_mpi",  (DL_FUNC) &C_delta_dress_fit_mpi,  12},
    {"C_nabla_dress_fit_mpi",  (DL_FUNC) &C_nabla_dress_fit_mpi,  15},
#ifdef DRESS_CUDA
    {"C_delta_dress_fit_mpi_cuda", (DL_FUNC) &C_delta_dress_fit_mpi_cuda, 12},
    {"C_nabla_dress_fit_mpi_cuda", (DL_FUNC) &C_nabla_dress_fit_mpi_cuda, 15},
#endif
#ifdef _OPENMP
    {"C_delta_dress_fit_mpi_omp",  (DL_FUNC) &C_delta_dress_fit_mpi_omp,  12},
    {"C_nabla_dress_fit_mpi_omp",  (DL_FUNC) &C_nabla_dress_fit_mpi_omp,  15},
#endif
#endif
    {NULL, NULL, 0}
};

void R_init_dress_graph(DllInfo *dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
