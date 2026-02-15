#ifndef MAPFIT_BLAS_H
#define MAPFIT_BLAS_H

#include <Rcpp.h>

#include "traits.h"
#include "f77blas.h"

// blas interface level 1

template<typename T1, typename T2>
double dot(const T1& x, const T2& y) {
  using traits1 = vector_traits<T1>;
  using traits2 = vector_traits<T2>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  const int inc2 = traits2::inc(y);
  return f77ddot(n, traits1::value(x), inc1, traits2::value(y), inc2);
}

template<typename T>
double asum(const T& x) {
  using traits1 = vector_traits<T>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  return f77dasum(n, traits1::value(x), inc1);
}

/**
 * Return the C-style index 
 */
template<typename T>
int iamax(const T& x) {
  using traits1 = vector_traits<T>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  return f77idamax(n, traits1::value(x), inc1);
}

template<typename T1, typename T2>
void copy(const T1& x, T2& y) {
  using traits1 = vector_traits<T1>;
  using traits2 = vector_traits<T2>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  const int inc2 = traits2::inc(y);
  f77dcopy(n, traits1::value(x), inc1, traits2::value(y), inc2);
}

template<typename T>
void scal(double alpha, T& x) {
  using traits1 = vector_traits<T>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  f77dscal(n, alpha, traits1::value(x), inc1);
}

template<typename T1, typename T2>
void axpy(double alpha, const T1& x, T2& y) {
  using traits1 = vector_traits<T1>;
  using traits2 = vector_traits<T2>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  const int inc2 = traits2::inc(y);
  f77daxpy(n, alpha, traits1::value(x), inc1, traits2::value(y), inc2);
}

template<typename T>
void fill(T& x, double v) {
  using traits1 = vector_traits<T>;
  const int n = traits1::size(x);
  const int inc1 = traits1::inc(x);
  double* p = traits1::value(x);
  for (int i=0; i<n; i++, p+=inc1) {
    *p = v;
  }
}

// level 2

// mcopy: matrix copy

namespace _mcopy_ {

template<typename T1, typename T2>
void mcopy(const T1& A, T2& B, DenseMatrixT, DenseMatrixT) {
  copy(A, B);
}

template<typename T1, typename T2>
void mcopy(const T1& A, T2& B, CSRMatrixT, DenseMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  double* valueB = traits2::value(B);
  const int ld = traits2::ld(B);
  
  fill(B, 0.0);
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      valueB[j*ld+i] = valueA[z];
    }
  }
}

template<typename T1, typename T2>
void mcopy(const T1& A, T2& B, CSCMatrixT, DenseMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  double* valueB = traits2::value(B);
  const int ld = traits2::ld(B);
  
  fill(B, 0.0);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      valueB[j*ld+i] = valueA[z];
    }
  }
}

template<typename T1, typename T2>
void mcopy(const T1& A, T2& B, COOMatrixT, DenseMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  double* valueB = traits2::value(B);
  const int ld = traits2::ld(B);
  
  fill(B, 0.0);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    valueB[j*ld+i] = valueA[z];
  }
}

}

template<typename T1>
void mcopy(const T1& A, T1& B) {
  copy(A, B);
}

template<typename T1, typename T2>
void mcopy(const T1& A, T2& B) {
  _mcopy_::mcopy(
    A, B,
    typename matrix_category<T1>::type{},
    typename matrix_category<T2>::type{});
}

// gemv

namespace _gemv_ {

template<typename T1, typename T2, typename T3>
void gemv(TRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int ld = traits1::ld(A);
  const int inc1 = traits2::inc(x);
  const int inc2 = traits3::inc(y);
  f77dgemv('T', m, n, alpha, traits1::value(A), ld,
           traits2::value(x), inc1, beta, traits3::value(y), inc2);
}

template<typename T1, typename T2, typename T3>
void gemv(NOTRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int ld = traits1::ld(A);
  const int inc1 = traits2::inc(x);
  const int inc2 = traits3::inc(y);
  f77dgemv('N', m, n, alpha, traits1::value(A), ld,
           traits2::value(x), inc1, beta, traits3::value(y), inc2);
}

template <typename T1, typename T2, typename T3>
void gemv(NOTRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, CSRMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int base = traits1::base(A);
  const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      valueY[i*incy] += alpha * valueA[z] * valueX[j*incx];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemv(TRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, CSRMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int base = traits1::base(A);
  const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      valueY[j*incy] += alpha * valueA[z] * valueX[i*incx];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemv(NOTRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, CSCMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int base = traits1::base(A);
  // const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      valueY[i*incy] += alpha * valueA[z] * valueX[j*incx];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemv(TRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, CSCMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  const int base = traits1::base(A);
  // const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      valueY[j*incy] += alpha * valueA[z] * valueX[i*incx];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemv(NOTRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, COOMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    valueY[i*incy] += alpha * valueA[z] * valueX[j*incx];
  }
}

template <typename T1, typename T2, typename T3>
void gemv(TRANS, double alpha, const T1& A, const T2& x, double beta, T3& y, COOMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = vector_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  scal(beta, y);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    valueY[j*incy] += alpha * valueA[z] * valueX[i*incx];
  }
}

}

template<typename T0, typename T1, typename T2, typename T3>
void gemv(const T0& tr, double alpha, const T1& A, const T2& x, double beta, T3& y) {
  _gemv_::gemv(tr, alpha, A, x, beta, y,
               typename matrix_category<T1>::type{});
}

// ger

namespace _ger_ {

template<typename T1, typename T2, typename T3>
void ger(double alpha, const T1& x, const T2& y, T3& A, DenseMatrixT) {
  using traits1 = vector_traits<T1>;
  using traits2 = vector_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int inc1 = traits1::inc(x);
  const int inc2 = traits2::inc(y);
  const int m = traits3::nrow(A);
  const int n = traits3::ncol(A);
  const int ld = traits3::ld(A);
  f77dger(m, n, alpha, traits1::value(x), inc1, traits2::value(y), inc2,
          traits3::value(A), ld);
}

template <typename T1, typename T2, typename T3>
void ger(double alpha, const T1& x, const T2& y, T3& A, CSRMatrixT) {
  using traits1 = csr_matrix_traits<T3>;
  using traits2 = vector_traits<T1>;
  using traits3 = vector_traits<T2>;
  const int base = traits1::base(A);
  const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  const double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      valueA[z] += alpha * valueX[i*incx] * valueY[j*incy];
    }
  }
}

template <typename T1, typename T2, typename T3>
void ger(double alpha, const T1& x, const T2& y, T3& A, CSCMatrixT) {
  using traits1 = csc_matrix_traits<T3>;
  using traits2 = vector_traits<T1>;
  using traits3 = vector_traits<T2>;
  const int base = traits1::base(A);
  // const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  const double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      valueA[z] += alpha * valueX[i*incx] * valueY[j*incy];
    }
  }
}

template <typename T1, typename T2, typename T3>
void ger(double alpha, const T1& x, const T2& y, T3& A, COOMatrixT) {
  using traits1 = coo_matrix_traits<T3>;
  using traits2 = vector_traits<T1>;
  using traits3 = vector_traits<T2>;
  // const int m = traits1::nrow(A);
  // const int n = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  const double* valueX = traits2::value(x);
  const int incx = traits2::inc(x);
  const double* valueY = traits3::value(y);
  const int incy = traits3::inc(y);
  
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    valueA[z] += alpha * valueX[i*incx] * valueY[j*incy];
  }
}

}

template<typename T1, typename T2, typename T3>
void ger(NOTRANS, double alpha, const T1& x, const T2& y, T3& A) {
  _ger_::ger(alpha, x, y, A,
             typename matrix_category<T3>::type{});
}

template<typename T1, typename T2, typename T3>
void ger(TRANS, double alpha, const T1& x, const T2& y, T3& A) {
  _ger_::ger(alpha, y, x, A,
             typename matrix_category<T3>::type{});
}

namespace _gemm_ {

template<typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int k = traits1::ncol(A);
  const int ld1 = traits1::ld(A);
  const int ld2 = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  const int ld3 = traits3::ld(C);
  f77dgemm('N', 'N', m, n, k, alpha, traits1::value(A), ld1,
           traits2::value(B), ld2, beta, traits3::value(C), ld3);
}

template<typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int k = traits1::nrow(A);
  const int ld1 = traits1::ld(A);
  const int ld2 = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  const int ld3 = traits3::ld(C);
  f77dgemm('T', 'N', m, n, k, alpha, traits1::value(A), ld1,
           traits2::value(B), ld2, beta, traits3::value(C), ld3);
}

template<typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int k = traits1::ncol(A);
  const int ld1 = traits1::ld(A);
  const int ld2 = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  const int ld3 = traits3::ld(C);
  f77dgemm('N', 'T', m, n, k, alpha, traits1::value(A), ld1,
           traits2::value(B), ld2, beta, traits3::value(C), ld3);
}

template<typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int k = traits1::nrow(A);
  const int ld1 = traits1::ld(A);
  const int ld2 = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  const int ld3 = traits3::ld(C);
  f77dgemm('T', 'T', m, n, k, alpha, traits1::value(A), ld1,
           traits2::value(B), ld2, beta, traits3::value(C), ld3);
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSRMatrixT, DenseMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
        Cptr[i] += alpha * valueA[z] * Bptr[j];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSRMatrixT, DenseMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const int k = traits2::nrow(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<k; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
        Cptr[j] += alpha * valueA[z] * Bptr[i];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSRMatrixT, DenseMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
        Cptr[i] += alpha * valueA[z] * Bptr[j*ldb];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSRMatrixT, DenseMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const double* valueA = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  const int k = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  //    const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<k; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
        Cptr[j] += alpha * valueA[z] * Bptr[i*ldb];
      }
    }
  }
}

// dcsrmm2

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSRMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csr_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  const int k = traits2::nrow(B);
  const int base = traits2::base(B);
  const double* valueB = traits2::value(B);
  const int* rowptr = traits2::rowptr(B);
  const int* colind = traits2::colind(B);
  const int m = traits3::nrow(C);
  //    const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<k; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
        Cptr[j*ldc] += alpha * Aptr[i*lda] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSRMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csr_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  const int k = traits2::nrow(B);
  const int base = traits2::base(B);
  const double* valueB = traits2::value(B);
  const int* rowptr = traits2::rowptr(B);
  const int* colind = traits2::colind(B);
  const int m = traits3::nrow(C);
  //    const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<k; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
        Cptr[j*ldc] += alpha * Aptr[i] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSRMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csr_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  const int n = traits2::nrow(B);
  const int base = traits2::base(B);
  const double* valueB = traits2::value(B);
  const int* rowptr = traits2::rowptr(B);
  const int* colind = traits2::colind(B);
  const int m = traits3::nrow(C);
  //    const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<n; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
        Cptr[i*ldc] += alpha * Aptr[j*lda] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSRMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csr_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  const int n = traits2::nrow(B);
  const int base = traits2::base(B);
  const double* valueB = traits2::value(B);
  const int* rowptr = traits2::rowptr(B);
  const int* colind = traits2::colind(B);
  const int m = traits3::nrow(C);
  //    const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int i=0; i<n; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
        Cptr[i*ldc] += alpha * Aptr[j] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSCMatrixT, DenseMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<k; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
        Cptr[i] += alpha * valueA[z] * Bptr[j];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSCMatrixT, DenseMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<m; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
        Cptr[j] += alpha * valueA[z] * Bptr[i];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSCMatrixT, DenseMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<k; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
        Cptr[i] += alpha * valueA[z] * Bptr[j*ldb];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, CSCMatrixT, DenseMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int base = traits1::base(A);
  const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<m; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Bptr = valueB;
      double* Cptr = valueC;
      for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
        Cptr[j] += alpha * valueA[z] * Bptr[i*ldb];
      }
    }
  }
}

// dcsrmm2

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSCMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csc_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int* colptr = traits2::colptr(B);
  const int* rowind = traits2::rowind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
        Cptr[j*ldc] += alpha * Aptr[i*lda] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSCMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csc_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int* colptr = traits2::colptr(B);
  const int* rowind = traits2::rowind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
        Cptr[j*ldc] += alpha * Aptr[i] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSCMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csc_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int* colptr = traits2::colptr(B);
  const int* rowind = traits2::rowind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<k; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
        Cptr[i*ldc] += alpha * Aptr[j*lda] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, CSCMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = csc_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int* colptr = traits2::colptr(B);
  const int* rowind = traits2::rowind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int j=0; j<k; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      const double* Aptr = valueA;
      double* Cptr = valueC;
      for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
        Cptr[i*ldc] += alpha * Aptr[j] * valueB[z];
      }
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, COOMatrixT, DenseMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int k = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Bptr = valueB;
    double* Cptr = valueC;
    for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
      Cptr[i] += alpha * valueA[z] * Bptr[j];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, COOMatrixT, DenseMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Bptr = valueB;
    double* Cptr = valueC;
    for (int v=0; v<n; v++, Bptr+=ldb, Cptr+=ldc) {
      Cptr[j] += alpha * valueA[z] * Bptr[i];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, COOMatrixT, DenseMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int k = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Bptr = valueB;
    double* Cptr = valueC;
    for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
      Cptr[i] += alpha * valueA[z] * Bptr[j*ldb];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, COOMatrixT, DenseMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = dense_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const double* valueA = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  const int base = traits1::base(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const double* valueB = traits2::value(B);
  const int ldb = traits2::ld(B);
  // const int m = traits3::nrow(C);
  const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Bptr = valueB;
    double* Cptr = valueC;
    for (int v=0; v<n; v++, Bptr+=1, Cptr+=ldc) {
      Cptr[j] += alpha * valueA[z] * Bptr[i*ldb];
    }
  }
}

// dcoomm2

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, COOMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = coo_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const int nnz = traits2::nnz(B);
  const double* valueB = traits2::value(B);
  const int* rowind = traits2::rowind(B);
  const int* colind = traits2::colind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Aptr = valueA;
    double* Cptr = valueC;
    for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
      Cptr[j*ldc] += alpha * Aptr[i*lda] * valueB[z];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, NOTRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, COOMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = coo_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int k = traits2::nrow(B);
  // const int n = traits2::ncol(B);
  const int nnz = traits2::nnz(B);
  const double* valueB = traits2::value(B);
  const int* rowind = traits2::rowind(B);
  const int* colind = traits2::colind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Aptr = valueA;
    double* Cptr = valueC;
    for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
      Cptr[j*ldc] += alpha * Aptr[i] * valueB[z];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(NOTRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, COOMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = coo_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int m = traits1::nrow(A);
  // const int k = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const int nnz = traits2::nnz(B);
  const double* valueB = traits2::value(B);
  const int* rowind = traits2::rowind(B);
  const int* colind = traits2::colind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Aptr = valueA;
    double* Cptr = valueC;
    for (int v=0; v<m; v++, Aptr+=1, Cptr+=1) {
      Cptr[i*ldc] += alpha * Aptr[j*lda] * valueB[z];
    }
  }
}

template <typename T1, typename T2, typename T3>
void gemm(TRANS, TRANS, double alpha, const T1& A, const T2& B, double beta, T3& C, DenseMatrixT, COOMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = coo_matrix_traits<T2>;
  using traits3 = dense_matrix_traits<T3>;
  // const int k = traits1::nrow(A);
  // const int m = traits1::ncol(A);
  const double* valueA = traits1::value(A);
  const int lda = traits1::ld(A);
  // const int n = traits2::nrow(B);
  // const int k = traits2::ncol(B);
  const int nnz = traits2::nnz(B);
  const double* valueB = traits2::value(B);
  const int* rowind = traits2::rowind(B);
  const int* colind = traits2::colind(B);
  const int base = traits2::base(B);
  const int m = traits3::nrow(C);
  // const int n = traits3::ncol(C);
  double* valueC = traits3::value(C);
  const int ldc = traits3::ld(C);
  
  scal(beta, C);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    const double* Aptr = valueA;
    double* Cptr = valueC;
    for (int v=0; v<m; v++, Aptr+=lda, Cptr+=1) {
      Cptr[i*ldc] += alpha * Aptr[j] * valueB[z];
    }
  }
}

}

template<typename T0, typename T1, typename T2, typename T3, typename T4>
void gemm(const T0& tr1, const T1& tr2, double alpha, const T2& A, const T3& B,
          double beta, T4& C) {
  _gemm_::gemm(tr1, tr2, alpha, A, B, beta, C,
               typename matrix_category<T2>::type{},
               typename matrix_category<T3>::type{});
}

// lapack

/*
 DGESV
 
 Description:
 
 A X = B
 
 The above linear equation is solved by LU decomposition
 
 Arguments:
 n: the number of rows of A
 nrhs: the number of columns of B
 A: matrix A. The result is the matrix that is decomposed by LU
 lda: leading dimension of A
 ipiv: substitued indexes
 B: matrix B. The result is X.
 ldb: leading dimension of B
 
 */

namespace _gesv_ {

// template<typename T1, typename T2>
// int gesv(double alpha, T1& A, T2& B, DenseMatrixT) {
//   using traits1 = dense_matrix_traits<T1>;
//   using traits2 = dense_matrix_traits<T2>;
//   const int n = traits1::nrow(A);
//   double* valueA = traits1::value(A);
//   const int lda = traits1::ld(A);
//   double* valueB = traits2::value(B);
//   const int nrhs = traits2::ncol(B);
//   const int ldb = traits2::ld(B);
//   
//   std::vector<int> ipiv(n);
//   int info;
//   
//   scal(alpha, A);
//   info = f77dgesv(n, nrhs, valueA, lda, &ipiv[0], valueB, ldb);
//   return info;
// }

template<typename T1, typename T2, typename T3>
int gesv(TRANS, double alpha, const T1& A, const T2& B, T3& C, DenseMatrixT) {
  const int n = dense_matrix_traits<T1>::ncol(A);
  auto tmpA = Rcpp::NumericMatrix(n, n);
  auto tmpB = Rcpp::NumericVector(n);
  mcopy(A, tmpA);
  copy(B, tmpB);
  scal(alpha, tmpA);
  Rcpp::Function solve("solve");
  Rcpp::Function t("t");
  Rcpp::NumericVector x = solve(t(tmpA), Rcpp::Named("b", tmpB));
  copy(x, C);
  return 0;
}

template<typename T1, typename T2, typename T3>
int gesv(TRANS, double alpha, const T1& A, const T2& B, T3& C, CSRMatrixT) {
  const int n = csr_matrix_traits<T1>::ncol(A);
  auto tmpA = Rcpp::NumericMatrix(n, n);
  auto tmpB = Rcpp::NumericVector(n);
  mcopy(A, tmpA);
  copy(B, tmpB);
  scal(alpha, tmpA);
  Rcpp::Function solve("solve");
  Rcpp::Function t("t");
  Rcpp::NumericVector x = solve(t(tmpA), Rcpp::Named("b", tmpB));
  copy(x, C);
  return 0;
}

template<typename T1, typename T2, typename T3>
int gesv(TRANS, double alpha, const T1& A, const T2& B, T3& C, CSCMatrixT) {
  const int n = csc_matrix_traits<T1>::ncol(A);
  auto tmpA = Rcpp::NumericMatrix(n, n);
  auto tmpB = Rcpp::NumericVector(n);
  mcopy(A, tmpA);
  copy(B, tmpB);
  scal(alpha, tmpA);
  Rcpp::Function solve("solve");
  Rcpp::Function t("t");
  Rcpp::NumericVector x = solve(t(tmpA), Rcpp::Named("b", tmpB));
  copy(x, C);
  return 0;
}

template<typename T1, typename T2, typename T3>
int gesv(TRANS, double alpha, const T1& A, const T2& B, T3& C, COOMatrixT) {
  const int n = coo_matrix_traits<T1>::ncol(A);
  auto tmpA = Rcpp::NumericMatrix(n, n);
  auto tmpB = Rcpp::NumericVector(n);
  mcopy(A, tmpA);
  copy(B, tmpB);
  scal(alpha, tmpA);
  Rcpp::Function solve("solve");
  Rcpp::Function t("t");
  Rcpp::NumericVector x = solve(t(tmpA), Rcpp::Named("b", tmpB));
  copy(x, C);
  return 0;
}

}

template<typename T1, typename T2, typename T3, typename T0>
int gesv(const T0& tr, double alpha, const T1& A, const T2& B, T3& C) {
  return _gesv_::gesv(tr, alpha, A, B, C,
              typename matrix_category<T1>::type{});
}

#endif
