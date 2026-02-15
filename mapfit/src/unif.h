#ifndef MAPFIT_UNIF_H
#define MAPFIT_UNIF_H

#include "traits.h"
#include "blas.h"

namespace _unif_ {

template<typename T1>
double unif(T1& A, double ufactor, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int ld = traits1::ld(A);
  double* value = traits1::value(A);
  // assume m == n
  double maxv = std::abs(value[0]);
  int maxi = 0;
  for (int i=1; i<n; i++) {
    double tmp = std::abs(value[i*ld+i]);
    if (maxv < tmp) {
      maxi = i;
      maxv = tmp;
    }
  }
  double qv = maxv * ufactor;
  scal(1/qv, A);
  for (int i=0; i<n; i++) {
    value[i*ld+i] += 1.0;
  }
  return qv;
}

template<typename T1>
double unif(T1& A, double ufactor, CSRMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  double* value = traits1::value(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  // assume m == n
  double maxv = 0.0;
  int maxi = 0;
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      if (i == j) {
        double tmp = std::abs(value[z]);
        if (tmp > maxv) {
          maxi = i;
          maxv = tmp;
        }
        break;
      } else if (i < j) {
        break;
      }
    }
  }
  double qv = maxv * ufactor;
  for (int i=0; i<n; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      value[z] /= qv;
      if (i == j) {
        value[z] += 1.0;
      }
    }
  }
  return qv;
}

template<typename T1>
double unif(T1& A, double ufactor, CSCMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  double* value = traits1::value(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  // assume m == n
  double maxv = 0.0;
  int maxi = 0;
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      if (i == j) {
        double tmp = std::abs(value[z]);
        if (tmp > maxv) {
          maxi = i;
          maxv = tmp;
        }
        break;
      } else if (i > j) {
        break;
      }
    }
  }
  double qv = maxv * ufactor;
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      value[z] /= qv;
      if (i == j) {
        value[z] += 1.0;
      }
    }
  }
  return qv;
}

template<typename T1>
double unif(T1& A, double ufactor, COOMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const int base = traits1::base(A);
  double* value = traits1::value(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  // assume m == n
  double maxv = 0.0;
  int maxi = 0;
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    if (i == j) {
      double tmp = std::abs(value[z]);
      if (tmp > maxv) {
        maxi = i;
        maxv = tmp;
      }
    }
  }
  double qv = maxv * ufactor;
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    value[z] /= qv;
    if (i == j) {
      value[z] += 1.0;
    }
  }
  return qv;
}

}

template<typename T1>
double unif(T1& A, double ufactor) {
  return _unif_::unif(A, ufactor, typename matrix_category<T1>::type{});
}

template<typename T1, typename T2>
double unif(T1& A, const T2& diag_index, double ufactor) {
  using trait1 = vector_traits<T1>;
  using trait2 = vector_traits<T2,int>;
  double* value = trait1::value(A);
  const int n = trait2::size(diag_index);

  double maxv = 0.0;
  for (int i=0; i<n; i++) {
    int z = diag_index[i];
    if (z >= 0) {
      double tmp = std::abs(value[z]);
      if (tmp > maxv) {
        maxv = tmp;
      }
    }
  }
  double qv = maxv * ufactor;
  scal(1.0/qv, A);
  for (int i=0; i<n; i++) {
    int z = diag_index[i];
    if (z >= 0) {
      value[z] += 1.0;
    }
  }
  return qv;
}

// diag: return an index vector for diag elements (C-style index)

namespace _diag_ {

template<typename T1, typename T2>
void diag(const T1& A, T2& diag_index, DenseMatrixT) {
  using traits1 = dense_matrix_traits<T1>;
  using traits2 = vector_traits<T2,int>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int ld = traits1::ld(A);
  int* index = traits2::value(diag_index);

  // assume m == n
  int z = 0;
  for (int i=0; i<n; i++) {
    index[i] = z;
    z += ld+1;
  }
}

template<typename T1, typename T2>
void diag(const T1& A, T2& diag_index, CSRMatrixT) {
  using traits1 = csr_matrix_traits<T1>;
  using traits2 = vector_traits<T2,int>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  const int* rowptr = traits1::rowptr(A);
  const int* colind = traits1::colind(A);
  int* index = traits2::value(diag_index);

  // assume m == n
  for (int i=0; i<m; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      if (i == j) {
        index[i] = z;
        break;
      } else if (i < j) {
        index[i] = -1;
        break;
      }
    }
  }
}

template<typename T1, typename T2>
void diag(const T1& A, T2& diag_index, CSCMatrixT) {
  using traits1 = csc_matrix_traits<T1>;
  using traits2 = vector_traits<T2,int>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int base = traits1::base(A);
  const int* colptr = traits1::colptr(A);
  const int* rowind = traits1::rowind(A);
  int* index = traits2::value(diag_index);

  // assume m == n
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      if (i == j) {
        index[j] = z;
        break;
      } else if (i > j) {
        index[j] = -1;
        break;
      }
    }
  }
}

template<typename T1, typename T2>
void diag(const T1& A, T2& diag_index, COOMatrixT) {
  using traits1 = coo_matrix_traits<T1>;
  using traits2 = vector_traits<T2,int>;
  const int m = traits1::nrow(A);
  const int n = traits1::ncol(A);
  const int nnz = traits1::nnz(A);
  const int base = traits1::base(A);
  const int* rowind = traits1::rowind(A);
  const int* colind = traits1::colind(A);
  int* index = traits2::value(diag_index);

  // assume m == n
  for (int i=0; i<n; i++) {
    index[i] = -1;
  }
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    if (i == j) {
      index[i] = z;
    }
  }
}

}

template<typename T1, typename T2>
void diag(const T1& A, T2& diag_index) {
  _diag_::diag(A, diag_index, typename matrix_category<T1>::type{});
}

#endif
