#include "trans_hilbert.h"

void trans_hilbert(const matrix & A, const matrix & B, int N, int M,
                       matrixI & idx, vector &  mass, bool & a_sort)
{
  int K = A.rows();
  if(N != M) {
    Rcpp::stop("Number of atoms of A and B must match for current implementation of Hilbert sort!");
  }
  idx.resize(N, 2);
  mass.resize(N);
  mass.fill(1.0/double(N));
  mass.fill(1.0/double(N));
  std::vector<int> idx_A(N);
  std::vector<int> idx_B(N);

  if ( a_sort ) {
    std::iota (std::begin(idx_A), std::end(idx_A), 0);
  } else {
    hilbert_sort_cgal_fun(A.data(), K, N, &idx_A[0] );

    // a_sort = true;
  }
  hilbert_sort_cgal_fun( B.data(), K, N, &idx_B[0] );
  idx.col(1) = vectorI::LinSpaced(N,0,N-1);
  for ( int n = 0; n < N; n++ ) {
    idx(idx_B[n],0) = idx_A[n];
  }
}
