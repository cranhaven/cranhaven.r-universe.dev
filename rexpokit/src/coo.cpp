#include <Rcpp.h>


extern "C" {
// Sparsity of densely stored square matrix
int sparse_count_zeros(const int n, double *x)
{
  int count = 0;
  int i, j;
  
  for (j=0; j<n; j++)
  {
    for (i=0; i<n; i++)
    {
      if (x[i + n*j] == 0.)
        count++;
    }
  }
  
  return count;
}

// Reimplentation of SparseM's dense-to-coo converter
SEXP R_rexpokit_as_coo(SEXP x_)
{
  Rcpp::NumericMatrix x(x_);
  
  int ct = 0, i, j;
  const int n = x.nrow();
  const int sparsity = sparse_count_zeros(n, x.begin());
  const int len = n*n - sparsity;
  
  Rcpp::NumericMatrix ret(len, 3);
  
  for (i=0; i<n; i++)
  {
    for (j=0; j<n; j++)
    {
      if (x(i, j) != 0.)
      {
        ret(ct, 0) = i + 1;       // ia
        ret(ct, 1) = j + 1;       // ja
        ret(ct, 2) = x(i, j);     // a
        
        ct++;
      }
    }
  }
  
  
  return ret;
}
}
