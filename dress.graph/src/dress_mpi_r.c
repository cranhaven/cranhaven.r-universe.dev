/*
 * dress_mpi_r.c -- R bridge for the MPI-distributed DRESS library.
 *
 * Same API as dress_r.c (dress_delta_fit) but calls the C MPI backend.
 * Exposed via the `mpi` environment in R:
 *
 *   library(dress)
 *   mpi$dress_delta_fit(4, sources, targets, k = 2L)
 *
 * Only compiled when DRESS_MPI is defined (requires MPI library).
 */

#ifdef DRESS_MPI

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "dress/dress.h"
#include "dress/mpi/dress_mpi.h"

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
/*  dress_delta_fit_mpi                                                */
/* ------------------------------------------------------------------ */
SEXP C_delta_dress_fit_mpi(SEXP n_vertices_,
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
                           SEXP compute_histogram_,
                           SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    /* Allocate copies (dress_init_graph takes ownership). */
    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_delta_fit_mpi: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_delta_fit_mpi: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_delta_fit_mpi: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_delta_fit_mpi: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;
    dress_hist_pair_t *hist = dress_delta_fit_mpi_fcomm(g, k, max_iterations, epsilon,
                                                        n_samples, seed,
                                                        compute_hist ? &hist_size : NULL,
                                                        keep_ms,
                                                        keep_ms ? &ms_ptr : NULL,
                                                        keep_ms ? &num_sub : NULL,
                                                        comm_f);

    /* Build result list: histogram [+ multisets + num_subgraphs] */
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

/* ------------------------------------------------------------------ */
/*  dress_nabla_fit_mpi                                                */
/* ------------------------------------------------------------------ */
SEXP C_nabla_dress_fit_mpi(SEXP n_vertices_,
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
                           SEXP compute_histogram_,
                           SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    /* Allocate copies (dress_init_graph takes ownership). */
    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_nabla_fit_mpi: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_nabla_fit_mpi: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_nabla_fit_mpi: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_nabla_fit_mpi: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;
    dress_hist_pair_t *hist = dress_nabla_fit_mpi_fcomm(g, k, max_iterations, epsilon,
                                                        n_samples, seed,
                                                        compute_hist ? &hist_size : NULL,
                                                        keep_ms,
                                                        keep_ms ? &ms_ptr : NULL,
                                                        keep_ms ? &num_tuples : NULL,
                                                        comm_f);

    /* Build result list: histogram [+ multisets + num_tuples] */
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

/* ------------------------------------------------------------------ */
/*  dress_delta_fit_mpi_cuda                                           */
/* ------------------------------------------------------------------ */
SEXP C_delta_dress_fit_mpi_cuda(SEXP n_vertices_,
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
                                SEXP compute_histogram_,
                                SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_delta_fit_mpi_cuda: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_delta_fit_mpi_cuda: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_delta_fit_mpi_cuda: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_delta_fit_mpi_cuda: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;
    dress_hist_pair_t *hist = dress_delta_fit_mpi_cuda_fcomm(g, k, max_iterations, epsilon,
                                                             n_samples, seed,
                                                             compute_hist ? &hist_size : NULL,
                                                             keep_ms,
                                                             keep_ms ? &ms_ptr : NULL,
                                                             keep_ms ? &num_sub : NULL,
                                                             comm_f);

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
/*  dress_nabla_fit_mpi_cuda                                           */
/* ------------------------------------------------------------------ */
SEXP C_nabla_dress_fit_mpi_cuda(SEXP n_vertices_,
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
                                SEXP compute_histogram_,
                                SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) {
        free(U); free(V);
        error("dress_nabla_fit_mpi_cuda: memory allocation failed");
    }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        if (!W) { free(U); free(V); error("dress_nabla_fit_mpi_cuda: memory allocation failed"); }
        memcpy(W, REAL(weights_), E * sizeof(double));
    }

    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        if (LENGTH(node_weights_) != N) {
             free(U); free(V); if(W) free(W);
             error("dress_nabla_fit_mpi_cuda: vertex_weights length mismatch");
        }
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) {
        error("dress_nabla_fit_mpi_cuda: dress_init_graph returned NULL");
    }

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;
    dress_hist_pair_t *hist = dress_nabla_fit_mpi_cuda_fcomm(g, k, max_iterations, epsilon,
                                                             n_samples, seed,
                                                             compute_hist ? &hist_size : NULL,
                                                             keep_ms,
                                                             keep_ms ? &ms_ptr : NULL,
                                                             keep_ms ? &num_tuples : NULL,
                                                             comm_f);

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

#endif /* DRESS_CUDA */

/* ------------------------------------------------------------------ */
/*  dress_delta_fit_mpi_omp                                            */
/* ------------------------------------------------------------------ */
#ifdef _OPENMP

#include "dress/omp/dress_omp.h"

SEXP C_delta_dress_fit_mpi_omp(SEXP n_vertices_,
                               SEXP sources_, SEXP targets_,
                               SEXP weights_, SEXP node_weights_,
                               SEXP k_, SEXP variant_,
                               SEXP max_iterations_, SEXP epsilon_,
                               SEXP n_samples_, SEXP seed_,
                               SEXP precompute_, SEXP keep_multisets_,
                               SEXP compute_histogram_,
                               SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) { free(U); free(V); error("dress_delta_fit_mpi_omp: malloc failed"); }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        memcpy(W, REAL(weights_), E * sizeof(double));
    }
    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) error("dress_delta_fit_mpi_omp: dress_init_graph returned NULL");

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_sub = 0;
    dress_hist_pair_t *hist = dress_delta_fit_mpi_omp_fcomm(
        g, k, max_iterations, epsilon,
        n_samples, seed,
        compute_hist ? &hist_size : NULL,
        keep_ms, keep_ms ? &ms_ptr : NULL,
        keep_ms ? &num_sub : NULL, comm_f);

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
    UNPROTECT(1);

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
/*  dress_nabla_fit_mpi_omp                                            */
/* ------------------------------------------------------------------ */
SEXP C_nabla_dress_fit_mpi_omp(SEXP n_vertices_,
                               SEXP sources_, SEXP targets_,
                               SEXP weights_, SEXP node_weights_,
                               SEXP k_, SEXP variant_,
                               SEXP max_iterations_, SEXP epsilon_,
                               SEXP n_samples_, SEXP seed_,
                               SEXP precompute_, SEXP keep_multisets_,
                               SEXP compute_histogram_,
                               SEXP comm_f_) {

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
    int comm_f         = INTEGER(comm_f_)[0];

    int *U = (int *)malloc(E * sizeof(int));
    int *V = (int *)malloc(E * sizeof(int));
    if (!U || !V) { free(U); free(V); error("dress_nabla_fit_mpi_omp: malloc failed"); }
    memcpy(U, INTEGER(sources_), E * sizeof(int));
    memcpy(V, INTEGER(targets_), E * sizeof(int));

    double *W = NULL;
    if (!isNull(weights_)) {
        W = (double *)malloc(E * sizeof(double));
        memcpy(W, REAL(weights_), E * sizeof(double));
    }
    double *NW = NULL;
    if (node_weights_ != R_NilValue) {
        NW = (double *)malloc(N * sizeof(double));
        memcpy(NW, REAL(node_weights_), N * sizeof(double));
    }

    p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW,
                                         (dress_variant_t)variant, precompute);
    if (!g) error("dress_nabla_fit_mpi_omp: dress_init_graph returned NULL");

    int hist_size = 0;
    double *ms_ptr = NULL;
    int64_t num_tuples = 0;
    dress_hist_pair_t *hist = dress_nabla_fit_mpi_omp_fcomm(
        g, k, max_iterations, epsilon,
        n_samples, seed,
        compute_hist ? &hist_size : NULL,
        keep_ms, keep_ms ? &ms_ptr : NULL,
        keep_ms ? &num_tuples : NULL, comm_f);

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

#endif /* _OPENMP */

#endif /* DRESS_MPI */

/* Prevent -Wempty-translation-unit when MPI/CUDA is unavailable. */
typedef int dress_mpi_r_unused_t;
