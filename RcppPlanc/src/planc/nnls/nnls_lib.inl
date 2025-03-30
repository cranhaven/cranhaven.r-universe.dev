#pragma once

#include "nnls_lib.hpp"
#include "utils.hpp"
#include "bppnnls.hpp"

template<typename T, typename eT>
arma::mat planc::nnlslib<T, eT>::runbppnnls(const arma::mat &C, const T &B, const int& ncores) {
    arma::uword m_n = B.n_cols;
    arma::uword m_k = C.n_cols;
    arma::mat CtC = C.t() * C;
    arma::mat outmat = arma::zeros<arma::mat>(m_k, m_n);
    arma::mat *outmatptr;
    outmatptr = &outmat;
    arma::uword ONE_THREAD_MATRIX_SIZE = chunk_size_dense<double>(m_k);
    unsigned int numChunks = m_n / ONE_THREAD_MATRIX_SIZE;
    if (numChunks*ONE_THREAD_MATRIX_SIZE < m_n) numChunks++;
#pragma omp parallel for schedule(dynamic) default(none) shared(numChunks, ONE_THREAD_MATRIX_SIZE, m_n, outmatptr, C, B, CtC) num_threads(ncores)
     for (unsigned int i = 0; i < numChunks; i++) {
         unsigned int spanStart = i * ONE_THREAD_MATRIX_SIZE;
         unsigned int spanEnd = (i + 1) * ONE_THREAD_MATRIX_SIZE - 1;
         if (spanEnd > m_n - 1) spanEnd = m_n - 1;
         // double start = omp_get_wtime();
         arma::mat CtBChunk = C.t() * B.cols(spanStart, spanEnd);
         BPPNNLS<arma::mat, arma::vec> solveProblem(CtC, CtBChunk, true);
         solveProblem.solveNNLS();
         (*outmatptr).cols(spanStart, spanEnd) = solveProblem.getSolutionMatrix();
     }
     return outmat;
 }

template<typename T, typename eT>
arma::mat planc::nnlslib<T, eT>::bppnnls_prod(const arma::mat &CtC, const arma::mat &CtB, const int& nCores) {
        arma::uword n = CtB.n_cols;
        arma::uword k = CtC.n_cols;
        arma::uword ONE_THREAD_MATRIX_SIZE = chunk_size_dense<double>(k);
        arma::mat outmat = arma::zeros<arma::mat>(k, n);
        arma::mat* outmatptr = &outmat;
        unsigned int numChunks = n / ONE_THREAD_MATRIX_SIZE;
        if (numChunks*ONE_THREAD_MATRIX_SIZE < n) numChunks++;
#pragma omp parallel for schedule(dynamic) default(none) shared(numChunks, CtB, ONE_THREAD_MATRIX_SIZE, outmatptr, CtC, n) num_threads(nCores)
        for (unsigned int i = 0; i < numChunks; i++) {
            unsigned int spanStart = i * ONE_THREAD_MATRIX_SIZE;
            unsigned int spanEnd = (i + 1) * ONE_THREAD_MATRIX_SIZE - 1;
            if (spanEnd > n - 1) spanEnd = n - 1;
            arma::mat CtBChunk = CtB.cols(spanStart, spanEnd);
            BPPNNLS<arma::mat, arma::vec> solveProblem(CtC, CtBChunk, true);
            solveProblem.solveNNLS();
            (*outmatptr).cols(spanStart, spanEnd) = solveProblem.getSolutionMatrix();
        }
        return outmat;
    }