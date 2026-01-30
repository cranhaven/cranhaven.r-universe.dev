/*******************************************************************************
 *
 * Text Distance in C++ 
 *
 *******************************************************************************
 * Initialization
 ******************************************************************************/
#include <Rcpp.h>
#include "distance.h"
using namespace Rcpp;
/*******************************************************************************
 * Internal Functions
 ******************************************************************************/
/*******************************************************************************
 * Calculate the text distance between two dense text vectors
 ******************************************************************************/
// [[Rcpp::export]]
double textDist_cpp(NumericVector x, NumericVector y, double zeroes=.5) {
  double den=sum(x+y);
  if (den==0) return zeroes;
  return sum(abs(x-y))/den;
}
/*******************************************************************************
 * Calculate the text distance between two sparse text vectors via structs
 ******************************************************************************/
double dist_sparse_cpp(const SparseVec& x,
                       const SparseVec& y, 
                       double zeroes) {
  size_t i = 0, j = 0;
  double num = 0.0;
  double den = x.total + y.total;
  if (den == 0.0) return zeroes;
  
  while (i < x.size && j < y.size) {
    int xi = x.rows[i];
    int yj = y.rows[j];
    if (xi == yj) {
      double vx = x.values[i];
      double vy = y.values[j];
      num += std::abs(vx - vy);
      ++i; ++j;
    } else if (xi < yj) {
      num += x.values[i++];
    } else {
      num += y.values[j++];
    }
  }
  while (i < x.size) num += x.values[i++];
  while (j < y.size) num += y.values[j++];
  
  return num / den;
}
/*******************************************************************************
 * Calculate the text distance between two sparse text vectors
 ******************************************************************************/
// [[Rcpp::export]]
double textDist_sparse_cpp(IntegerVector xi,
                           NumericVector xx,
                           IntegerVector yi,
                           NumericVector yx,
                           double zeroes=.5) {
  
  // Create SparseVec x
  SparseVec x;
  x.rows   = xi.begin();
  x.values = xx.begin();
  x.size   = xi.size();
  x.total  = sum_doubles(xx.begin(), xx.size());
  
  // Create SparseVec y
  SparseVec y;
  y.rows   = yi.begin();
  y.values = yx.begin();
  y.size   = yi.size();
  y.total  = sum_doubles(yx.begin(), yx.size());
  
  // Calculate distance
  return dist_sparse_cpp(x,y,zeroes);
}
/*******************************************************************************
 * Calculate the text distance between two dense matrices of the same size, 
 * where we calculate the text distance between corresponding columns
 ******************************************************************************/
// [[Rcpp::export]]
NumericVector textDistM_cpp(const NumericMatrix& x, 
                            const NumericMatrix& y, 
                            double zeroes = 0.5) {
  int nr = x.nrow();
  int nc = x.ncol();
  NumericVector out(nc);
  
  for (int j = 0; j < nc; ++j) {
    double num = 0.0, den = 0.0;
    for (int i = 0; i < nr; ++i) {
      double a = x(i, j);
      double b = y(i, j);
      num += std::abs(a - b);
      den += a + b;
    }
    out[j] = (den == 0.0) ? zeroes : num / den;
  }
  
  return out;
}

/*******************************************************************************
 * Calculate the text distance between two sparse matrices of the same size, 
 * where we calculate the text distance between corresponding columns
 ******************************************************************************/
// [[Rcpp::export]]
NumericVector textDistM_sparse_cpp(
    IntegerVector Mi_x, IntegerVector Mp_x, NumericVector Mx_x,
    IntegerVector Mi_y, IntegerVector Mp_y, NumericVector Mx_y,
    double zeroes = 0.5) {
  
  int ncol = Mp_x.size() - 1;
  NumericVector out(ncol);
  
  for (int col = 0; col < ncol; ++col) {
    
    // Column col of matrix X
    int start_x = Mp_x[col];
    int end_x = Mp_x[col + 1];
    SparseVec x_vec {
      Mi_x.begin() + start_x,
      Mx_x.begin() + start_x,
      static_cast<size_t>(end_x - start_x),
      sum_doubles(Mx_x.begin() + start_x, (end_x - start_x))
    };
    
    // Column col of matrix Y
    int start_y = Mp_y[col];
    int end_y = Mp_y[col + 1];
    SparseVec y_vec {
      Mi_y.begin() + start_y,
      Mx_y.begin() + start_y,
      static_cast<size_t>(end_y - start_y),
      sum_doubles(Mx_y.begin() + start_y, (end_y - start_y))
    };
    
    // Compute distance
    out[col] = dist_sparse_cpp(x_vec, y_vec, zeroes);
  }
  
  return out;
}
/*******************************************************************************
 * Calculate the text distance between all combinations of columns of one
 * sparse matrix
 ******************************************************************************/
// [[Rcpp::export]]
List textDistMatrix_cpp(
    IntegerVector Mi, IntegerVector Mp, NumericVector Mx,
    double zeroes = 0.5
) {
  int ncol = Mp.size() - 1;
  int ncomb = (ncol * (ncol - 1)) / 2;
  
  NumericVector out(ncomb, 0.0);
  std::vector<int> row1(ncomb);
  std::vector<int> row2(ncomb);
  
  int idx = 0; // index into out and row1/row2
  
  // Loop over all pairs of columns
  for (int d1 = 0; d1 < ncol; ++d1) {
    int start1 = Mp[d1];
    int end1   = Mp[d1 + 1];
    size_t sz1 = static_cast<size_t>(end1 - start1);
    SparseVec col1 {
      Mi.begin() + start1,
      Mx.begin() + start1,
      sz1,
      sum_doubles(Mx.begin() + start1, sz1)
    };
    
    for (int d2 = d1 + 1; d2 < ncol; ++d2) {
      int start2 = Mp[d2];
      int end2   = Mp[d2 + 1];
      size_t sz2 = static_cast<size_t>(end2 - start2);
      SparseVec col2 {
        Mi.begin() + start2,
        Mx.begin() + start2,
        sz2,
        sum_doubles(Mx.begin() + start2, sz2)
      };
      
      // Compute distance using the central function
      out[idx] = dist_sparse_cpp(col1, col2, zeroes);
      row1[idx] = d1;
      row2[idx] = d2;
      ++idx;
    }
  }
  
  IntegerMatrix comb(2, ncomb);
  for (int i = 0; i < ncomb; ++i) {
    comb(0, i) = row1[i];
    comb(1, i) = row2[i];
  }
  
  return List::create(
    Named("out") = out,
    Named("comb") = comb
  );
}
