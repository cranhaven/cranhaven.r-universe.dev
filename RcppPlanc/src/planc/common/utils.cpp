//
// Created by andrew on 11/28/2023.
//
#include "utils.hpp"
#include <chrono>
#include <utility>
#include <vector>
#include <random>

void tic() { tictoc_stack.push(std::chrono::steady_clock::now()); }

double toc() {
    const auto time_span =
            std::chrono::duration_cast<std::chrono::duration<double>>(
                std::chrono::steady_clock::now() - tictoc_stack.top());
    const double rc = time_span.count();
    tictoc_stack.pop();
    return rc;
}

int random_sieve(const int nthprime) {
    int i, m, k;

    constexpr int nlimit = 104000;

    const auto mark = static_cast<int *>(calloc(nlimit, sizeof(int)));

    /* Calculate limit for k */
    const int klimit = static_cast<int>(sqrt(static_cast<double>(nlimit) + 1));

    /* Mark the composites */
    /* Special case */
    mark[1] = -1;

    /* Set k=1. Loop until k >= sqrt(n) */
    for (k = 3; k <= klimit; k = m) {
        /* Find first non-composite in list > k */
        for (m = k + 1; m < nlimit; m++)
            if (!mark[m]) break;

        /* Mark the numbers 2m, 3m, 4m, ... */
        for (i = m * 2; i < nlimit; i += m) mark[i] = -1;
    }

    /* Now display results - all unmarked numbers are prime */
    int rcprime = -1;
    for (k = 0, i = 1; i < nlimit; i++) {
        if (!mark[i]) {
            k++;
            if (k == nthprime + 1) {
                rcprime = i;
                break;
            }
        }
    }
    free(mark);
    return rcprime;
}


void randNMF(const arma::uword m, const arma::uword n, const arma::uword k, const double sparsity,
             const arma::mat* A) {
#ifndef USING_R
    srand(RAND_SEED);
#endif
    arma::mat W = 10 * arma::randu<arma::mat>(m, k);
    arma::mat H = 10 * arma::randu<arma::mat>(n, k);
    if (sparsity < 1) {
        makeSparse<arma::mat>(sparsity, &W);
        makeSparse<arma::mat>(sparsity, &H);
    }
    arma::mat temp = ceil(W * trans(H));
    A = &temp;
}

void randNMF(const arma::uword m, const arma::uword n, const arma::uword k, const double sparsity,
             const arma::sp_mat* A) {
    const auto temp = arma::sprandu<arma::sp_mat>(m, n, sparsity);
    A = &temp;
}

template<class T>
void printVector(const std::vector<T>&x) {
    for (int i = 0; i < x.size(); i++) {
        INFO << x[i] << ' ';
    }
    INFO << std::endl;
}

std::vector<std::vector<size_t>> cartesian_product(
    const std::vector<std::vector<size_t>>&v) {
    std::vector<std::vector<size_t>> s = {{}};
    for (auto&u: v) {
        std::vector<std::vector<size_t>> r;
        for (auto y: u) {
            for (auto&x: s) {
                r.push_back(x);
                r.back().push_back(y);
            }
        }
        s.swap(r);
    }
    return s;
}

void cblas_sgemm(const arma::mat&A, const arma::mat&B, double* C) {
    const arma::uword m = A.n_rows;
    const arma::uword n = B.n_cols;
    const arma::uword k = A.n_cols;
    constexpr double alpha = 1.0;
    constexpr double beta = 0.0;
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, static_cast<int32_t>(m), static_cast<int32_t>(n),
                static_cast<int32_t>(k), alpha,
                A.memptr(), static_cast<int32_t>(m), B.memptr(), static_cast<int32_t>(k), beta, C,
                static_cast<int32_t>(m));
}

void gen_discard(const arma::uword row_start, const arma::uword nrows, const arma::uword k,
                 arma::mat&X, const bool trans, const int mseed) {
    for (unsigned int j = 0; j < k; ++j) {
        std::mt19937 gen(mseed + j);
        gen.discard(row_start);
        for (unsigned int i = 0; i < nrows; ++i) {
            if (trans) {
                X(j, i) = static_cast<double>(gen()) / std::mt19937::max();
            }
            else {
                X(i, j) = static_cast<double>(gen()) / std::mt19937::max();
            }
        }
    }
}

void read_input_matrix(arma::mat&A, std::string fname) {
    A.load(std::move(fname));
}

void read_input_matrix(arma::sp_mat&A, std::string fname) {
    A.load(std::move(fname), arma::coord_ascii);
}

void generate_rand_matrix(arma::mat&A, const std::string&rtype,
                          const arma::uword m, const arma::uword n, const arma::uword k, double density,
                          const bool symm_flag,
                          const bool adjrand, const int kalpha, const int kbeta) {
    if (rtype == "uniform") {
        if (symm_flag) {
            A = arma::randu<arma::mat>(m, n);
            A = 0.5 * (A + A.t());
        }
        else {
            A = arma::randu<arma::mat>(m, n);
        }
    }
    else if (rtype == "normal") {
        if (symm_flag) {
            A = arma::randn<arma::mat>(m, n);
            A = 0.5 * (A + A.t());
        }
        else {
            A = arma::randn<arma::mat>(m, n);
        }
        A.elem(find(A < 0)).zeros();
    }
    else {
        if (symm_flag) {
            arma::mat Htrue = arma::zeros<arma::mat>(n, k);
            gen_discard(0, n, k, Htrue, false, HTRUE_SEED);
            A = Htrue * Htrue.t();

            // Free auxiliary variables
            Htrue.clear();
        }
        else {
            arma::mat Wtrue = arma::zeros<arma::mat>(m, k);
            gen_discard(0, m, k, Wtrue, false, WTRUE_SEED);
            arma::mat Htrue = arma::zeros<arma::mat>(k, n);
            gen_discard(0, n, k, Htrue, true, HTRUE_SEED);
            A = Wtrue * Htrue;

            // Free auxiliary variables
            Wtrue.clear();
            Htrue.clear();
        }
    }
    if (adjrand) {
        A = kalpha * A + kbeta;
        A = ceil(A);
    }
}

void generate_rand_matrix(arma::sp_mat&A, const std::string&rtype,
                          arma::uword m, arma::uword n, arma::uword k, double density, bool symm_flag,
                          bool adjrand, int kalpha, int kbeta) {
    if (rtype == "uniform") {
        if (symm_flag) {
            double dens = 0.5 * density;
            A = arma::sprandu<arma::sp_mat>(m, n, dens);
            A = 0.5 * (A + A.t());
        }
        else {
            A = arma::sprandu<arma::sp_mat>(m, n, density);
            INFO << size(nonzeros(A)) << std::endl;
        }
    }
    else if (rtype == "normal") {
        if (symm_flag) {
            double dens = 0.5 * density;
            A = arma::sprandn<arma::sp_mat>(m, n, dens);
            A = 0.5 * (A + A.t());
        }
        else {
            A = arma::sprandn<arma::sp_mat>(m, n, density);
        }
    }
    else if (rtype == "lowrank") {
        if (symm_flag) {
            double dens = 0.5 * density;
            auto mask = arma::sprandu<arma::sp_mat>(m, n, dens);
            mask = 0.5 * (mask + mask.t());
            mask = spones(mask);
            arma::mat Htrue = arma::zeros(n, k);
            gen_discard(0, n, k, Htrue, false, HTRUE_SEED);
            A = arma::sp_mat(mask % (Htrue * Htrue.t()));

            // Free auxiliary space
            Htrue.clear();
            mask.clear();
        }
        else {
            auto mask = arma::sprandu<arma::sp_mat>(m, n, density);
            mask = spones(mask);
            arma::mat Wtrue = arma::zeros(m, k);
            gen_discard(0, m, k, Wtrue, false, WTRUE_SEED);
            arma::mat Htrue = arma::zeros(k, n);
            gen_discard(0, n, k, Htrue, true, HTRUE_SEED);
            A = arma::sp_mat(mask % (Wtrue * Htrue));

            // Free auxiliary space
            Wtrue.clear();
            Htrue.clear();
            mask.clear();
        }
    }
    // Adjust and project non-zeros
    arma::sp_mat::iterator start_it = A.begin();
    arma::sp_mat::iterator end_it = A.end();
    for (arma::sp_mat::iterator it = start_it; it != end_it; ++it) {
        double curVal = *it;
        if (adjrand) {
            *it = ceil(kalpha * curVal + kbeta);
        }
        if (*it < 0) *it = kbeta;
    }
}

int debug_hook() {
    int i = 0;
    while (i < 1) {
    }
    return 0;
}
