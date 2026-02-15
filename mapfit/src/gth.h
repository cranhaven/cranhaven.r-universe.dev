#ifndef MAPFIT_GTH_H
#define MAPFIT_GTH_H

#include <Rcpp.h>
#include "traits.h"
#include "blas.h"

#define MX(i,j) (valueX[((i)-1)+ld*((j)-1)])
#define vx(i) (valueV[(i)-1])

template <typename Tv, typename Tm>
void gth(Tm& X, Tv& v) {
  int n = dense_matrix_traits<Tm>::nrow(X);
  double* valueX = dense_matrix_traits<Tm>::value(X);
  const int ld = dense_matrix_traits<Tm>::ld(X);
  double* valueV = stride_vector_traits<Tv>::value(v);
  
  double tmp;
  for (int l=n; l>=2; l--) {
    tmp = 0.0;
    for (int j=1; j<=l-1; j++) {
      tmp += MX(l,j);
    }
    for (int j=1; j<=l-1; j++) {
      for (int i=1; i<=l-1; i++) {
        if (i != j) {
          MX(i,j) += MX(l,j) * MX(i,l) / tmp;
        }
      }
    }
    for (int i=1; i<=l-1; i++) {
      MX(i,l) /= tmp;
      MX(l,i) = 0.0;
    }
    MX(l,l) = -1.0;
  }
  
  vx(1) = 1.0;
  tmp = vx(1);
  for (int l=2; l<=n; l++) {
    vx(l) = 0.0;
    for (int i=1; i<=l-1; i++) {
      vx(l) += vx(i) * MX(i,l);
    }
    tmp += vx(l);
  }
  for (int i=1; i<=n; i++) {
    vx(i) /= tmp;
  }
}

template <typename Tm, typename Tv>
void markov_gth(const Tm& Q, Tv& x) {
  const int n = stride_vector_traits<Tv>::size(x);
  Rcpp::NumericMatrix A(n);
  mcopy(Q, A);
  gth(A, x);
}

template <typename Tm1, typename Tm2, typename Tv>
void map_gth(const Tm1& D0, const Tm2& D1, Tv& x) {
  const int n = stride_vector_traits<Tv>::size(x);
  Rcpp::NumericMatrix A(n);
  Rcpp::NumericMatrix B(n);
  mcopy(D0, A);
  mcopy(D1, B);
  axpy(1.0, B, A);
  gth(A, x);
}

#endif
