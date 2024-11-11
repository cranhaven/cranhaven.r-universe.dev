#include "trans_rank.h"

void  trans_rank(const matrix & A, const matrix & B, int N, int M,
                 matrixI & idx, vector & mass, bool & a_sort) {
  if (N != M) {
    Rcpp::stop("Number of atoms of A and B must match for ranks method!");
  }
  idx.resize(N, 2);
  mass.resize(N);
  mass.fill(1.0/double(N));
  matrixI ranksB(B.rows(), M);
  rank_mat(B, ranksB);
  vector meanB = ranksB.colwise().sum().cast<double>();
  // meanB /= double(M); // add in normalization if indices get too large
  std::vector<size_t> idx_B = sort_indexes(meanB);
  std::vector<size_t> idx_A(N);
    
  if (!a_sort) {
    // Rcpp::Rcout << "a not sorted\n";
    matrixI ranksA(A.rows(), N);
    rank_mat(A, ranksA);
    vector meanA = ranksA.colwise().sum().cast<double>();
    // meanA /= double(N); // add in normalization if indices get too large
    sort_indexes(meanA, idx_A);
  } else {
    // Rcpp::Rcout << "a sorted\n";
    std::iota (idx_A.begin(), idx_A.end(), 0);
  }
  idx.col(1) = vectorI::LinSpaced(N,0,N-1);
  for ( int n = 0; n < N; n++ ) {
    idx(idx_B[n],0) = idx_A[n];
    // Rcpp::Rcout << idx_A[n] << ", ";
  }
}

