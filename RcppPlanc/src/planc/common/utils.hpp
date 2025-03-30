#pragma once
/* Copyright 2016 Ramakrishnan Kannan */
// utility functions

#include <stack>
#include "utils.h"

extern "C" {
#include "hw_detect.h"
}

static uint64_t powersof10[16] = {
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000
};

static std::stack<std::chrono::steady_clock::time_point> tictoc_stack;
static std::stack<double> tictoc_stack_omp_clock;


/// start the timer. easy to call as tic(); some code; double t=toc();
void tic();

/***
 * Returns the time taken between the most recent tic() to itself.
 * @return time in seconds.
*/
double toc();

template<class T>
void fixNumericalError(T* X, const double prec,
                       const double repl) {
    (*X).for_each(
        [&](typename T::elem_type&val) { val = (val < prec) ? repl : val; });
}

template<class T>
void fixAbsNumericalError(T* X, const double prec,
                          const double repl) {
    (*X).for_each([&](typename T::elem_type&val) {
        val = (std::abs(val) < prec) ? repl : val;
    });
}

template<class T>
void fixDecimalPlaces(T* X, const int places) {
    (*X).for_each([&](typename T::elem_type&val) {
        val = floorf(val * powersof10[places]) / powersof10[places];
    });
}

/*
 * Returns the nth prime number.
 * There are totally 10000 prime numbers within 104000;
 */
int random_sieve(int nthprime);

template<class T>
void absmat(T* X) {
    arma::uvec negativeIdx = find((*X) < 0);
    (*X)(negativeIdx) = (*X)(negativeIdx) * -1;
}

template<class T>
void makeSparse(const double sparsity, T (*X)) {
    // make a matrix sparse
#ifndef USING_R
    srand(RAND_SEED_SPARSE);
#endif
#pragma omp parallel for default(none) shared(sparsity, X)
    for (int j = 0; j < X->n_cols; j++) {
        for (arma::uword i = 0; i < X->n_rows; i++) {
            if (arma::randu() > sparsity) (*X)(i, j) = 0;
        }
    }
}

void randNMF(arma::uword m, arma::uword n, arma::uword k, double sparsity,
             const arma::mat* A);

void randNMF(arma::uword m, arma::uword n, arma::uword k, double sparsity,
             const arma::sp_mat* A);

template<class T>
void printVector(std::vector<T>&x);

std::vector<std::vector<size_t>> cartesian_product(
    std::vector<std::vector<size_t>>&v);

/*
 * can be called by external people for sparse input matrix.
 */
template<class INPUTTYPE, class LRTYPE>
double computeObjectiveError(const INPUTTYPE&A, const LRTYPE&W,
                             const LRTYPE&H);

/*
 * This is an sgemm wrapper for armadillo matrices
 * Something is going crazy with armadillo
 */

void cblas_sgemm(const arma::mat&A, const arma::mat&B, double* C);

/**
 * Generates the same low rank matrix. Matrix columns are seeded to
 * easily create different row slices of the matrix. The lowrank matrix
 * X is expected to be of size n x k where k << n.
 *
 * @param[in] row_start is the starting row of the block to be generated
 * @param[in] nrows is the number of rows to be generated
 * @param[in] k is the lowrank (length of each row vector)
 * @param[in] X is the reference to the matrix to populate
 * @param[in] trans is a flag to indicate that Xt is sent in.
 * @param[in] mseed is the seed for the first column of the matrix
 */
void gen_discard(arma::uword row_start, arma::uword nrows, arma::uword k,
                 arma::mat&X, bool trans, int mseed = 7907);

/*
 * Read in a dense matrix
 */
void read_input_matrix(arma::mat&A, std::string fname);

/*
 * Read in a sparse matrix
 */
void read_input_matrix(arma::sp_mat&A, std::string fname);

/*
 * Generate random dense matrix
 */
void generate_rand_matrix(arma::mat&A, const std::string&rtype,
                          arma::uword m, arma::uword n, arma::uword k, double density, bool symm_flag = false,
                          bool adjrand = false, int kalpha = 1, int kbeta = 0);

/*
 * Generate random sparse matrix
 */
void generate_rand_matrix(arma::sp_mat&A, const std::string&rtype,
                          arma::uword m, arma::uword n, arma::uword k, double density, bool symm_flag = false,
                          bool adjrand = false, int kalpha = 5, int kbeta = 10);

int debug_hook();


template<class INPUTTYPE, class LRTYPE>
double computeObjectiveError(const INPUTTYPE&A, const LRTYPE&W,
                             const LRTYPE&H) {
    // 1. over all nnz (a_ij - w_i h_j)^2
    // 2. over all nnz (w_i h_j)^2
    // 3. Compute R of W ahd L of H through QR
    // 4. use sgemm to compute RL
    // 5. use slange to compute ||RL||_F^2
    // 6. return nnzsse+nnzwh-||RL||_F^2
    arma::uword k = W.n_cols;
    arma::uword m = A.n_rows;
    arma::uword n = A.n_cols;
    tic();
    double nnzsse = 0;
    double nnzwh = 0;
    LRTYPE Rw(k, k);
    LRTYPE Rh(k, k);
    LRTYPE Qw(m, k);
    LRTYPE Qh(n, k);
    LRTYPE RwRh(k, k);
#pragma omp parallel for reduction(+ : nnzsse, nnzwh) default(none)
    for (arma::uword jj = 1; jj <= A.n_cols; jj++) {
        arma::uword startIdx = A.col_ptrs[jj - 1];
        arma::uword endIdx = A.col_ptrs[jj];
        arma::uword col = jj - 1;
        double nnzssecol = 0;
        double nnzwhcol = 0;
        for (arma::uword ii = startIdx; ii < endIdx; ii++) {
            arma::uword row = A.row_indices[ii];
            double tempsum = 0;
            for (arma::uword kk = 0; kk < k; kk++) {
                tempsum += (W(row, kk) * H(col, kk));
            }
            nnzwhcol += tempsum * tempsum;
            nnzssecol += (A.values[ii] - tempsum) * (A.values[ii] - tempsum);
        }
        nnzsse += nnzssecol;
        nnzwh += nnzwhcol;
    }
    qr_econ(Qw, Rw, W);
    qr_econ(Qh, Rh, H);
    RwRh = Rw * Rh.t();
    double normWH = arma::norm(RwRh, "fro");
    Rw.clear();
    Rh.clear();
    Qw.clear();
    Qh.clear();
    RwRh.clear();
#ifdef _VERBOSE
    INFO << "error compute time " << toc() << std::endl;
#endif // _VERBOSE
    double fastErr = sqrt(nnzsse + (normWH * normWH - nnzwh));
    return (fastErr);
}
