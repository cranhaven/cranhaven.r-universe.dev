#ifndef MAPFIT_PHASE_CF1
#define MAPFIT_PHASE_CF1

#include <Rcpp.h>
#include "phase_gen.h"

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const GPH<G1,G2,G0>& gph, CF1<T1,GPHT>& cf1) {
  const int n = gph.size();
  const double* Q = vector_traits<G2>::value(gph.Q);
  const int* diag = stride_vector_traits<G0,int>::value(gph.diag);
  double* rate = stride_vector_traits<T1>::value(cf1.rate);
  copy(gph.alpha, cf1.alpha);
  for (int i=0; i<n; i++) {
    rate[i] = -Q[diag[i]];
  }
}

namespace _phcopy_ {

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const CF1<T1,GPHT>& cf1, GPH<G1,G2,G0>& gph, DenseMatrixT) {
  const int n = gph.size();
  double* Q = dense_matrix_traits<G2>::value(gph.Q);
  const int ld = dense_matrix_traits<G2>::value(gph.Q);
  double* xi = stride_vector_traits<G1>::value(gph.xi);
  const double* rate = stride_vector_traits<T1>::value(cf1.rate);
  copy(cf1.alpha, gph.alpha);
  for (int i=0; i<n-1; i++) {
    Q[i*ld+i] = -rate[i];
    Q[(i+1)*ld+i] = rate[i];
  }
  Q[(n-1)*ld+(n-1)] = -rate[n-1];
  xi[n-1] = rate[n-1];
}

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const CF1<T1,GPHT>& cf1, GPH<G1,G2,G0>& gph, CSRMatrixT) {
  const int n = gph.size();
  double* Q = csr_matrix_traits<G2>::value(gph.Q);
  const int* rowptr = csr_matrix_traits<G2>::rowptr(gph.Q);
  const int* colind = csr_matrix_traits<G2>::colind(gph.Q);
  const int base = csr_matrix_traits<G2>::base(gph.Q);
  double* xi = stride_vector_traits<G1>::value(gph.xi);
  const double* rate = stride_vector_traits<T1>::value(cf1.rate);
  copy(cf1.alpha, gph.alpha);
  for (int i=0; i<n; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      if (i == j) {
        Q[z] = -rate[i];
      } else if (i == j-1) {
        Q[z] = rate[i];
      }
    }
  }
  xi[n-1] = rate[n-1];
}

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const CF1<T1,GPHT>& cf1, GPH<G1,G2,G0>& gph, CSCMatrixT) {
  const int n = gph.size();
  double* Q = csc_matrix_traits<G2>::value(gph.Q);
  const int* colptr = csc_matrix_traits<G2>::colptr(gph.Q);
  const int* rowind = csc_matrix_traits<G2>::rowind(gph.Q);
  const int base = csc_matrix_traits<G2>::base(gph.Q);
  double* xi = stride_vector_traits<G1>::value(gph.xi);
  const double* rate = stride_vector_traits<T1>::value(cf1.rate);
  copy(cf1.alpha, gph.alpha);
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      if (i == j) {
        Q[z] = -rate[i];
      } else if (i == j-1) {
        Q[z] = rate[i];
      }
    }
  }
  xi[n-1] = rate[n-1];
}

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const CF1<T1,GPHT>& cf1, GPH<G1,G2,G0>& gph, COOMatrixT) {
  const int n = gph.size();
  double* Q = coo_matrix_traits<G2>::value(gph.Q);
  const int* rowind = coo_matrix_traits<G2>::rowind(gph.Q);
  const int* colind = coo_matrix_traits<G2>::colind(gph.Q);
  const int base = coo_matrix_traits<G2>::base(gph.Q);
  const int nnz = coo_matrix_traits<G2>::nnz(gph.Q);
  double* xi = stride_vector_traits<G1>::value(gph.xi);
  const double* rate = stride_vector_traits<T1>::value(cf1.rate);
  copy(cf1.alpha, gph.alpha);
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    if (i == j) {
      Q[z] = -rate[i];
    } else if (i == j-1) {
      Q[z] = rate[i];
    }
  }
  xi[n-1] = rate[n-1];
}

}

template <typename G1, typename G2, typename G0, typename T1, typename GPHT>
void phcopy(const CF1<T1,GPHT>& cf1, GPH<G1,G2,G0>& gph) {
  _phcopy_::phcopy(cf1, gph, typename matrix_category<G2>::type{});
}

template <typename T1, typename GPHT, typename DataT, typename EresT, typename OptionT, typename WorkSpace>
double estep(
    const CF1<T1,GPHT>& model,
    const DataT& data,
    EresT& eres,
    OptionT& options,
    WorkSpace& work) noexcept {
  
  return estep(model.gph, data, eres, options, work);
}

inline
  void cf1swap(double* a0, double* r0, double* a1, double* r1) {
    double w = *r1 / *r0;
    *a0 += (1.0 - w) * *a1;
    *a1 *= w;
    double tmp = *r1;
    *r1 = *r0;
    *r0 = tmp;
  }

template <typename T1, typename T2>
void cf1sort(T1& _alpha, T2& _rate) {
  double* alpha = stride_vector_traits<T1>::value(_alpha);
  double* rate = stride_vector_traits<T2>::value(_rate);
  const int n = stride_vector_traits<T1>::size(_alpha);
  for (int i=0; i<n-1; i++) {
    if (rate[i] > rate[i+1]) {
      cf1swap(&alpha[i], &rate[i], &alpha[i+1], &rate[i+1]);
      for (int j=i; j>=1; j--) {
        if (rate[j-1] <= rate[j]) {
          break;
        }
        cf1swap(&alpha[j-1], &rate[j-1], &alpha[j], &rate[j]);
      }
    }
  }
}

template <typename T1, typename GPHT>
void cf1sort(CF1<T1,GPHT>& cf1) {
  cf1sort(cf1.alpha, cf1.rate);
}

template <typename EresT, typename T1, typename G1, typename G2, typename G0, typename OptionT>
void mstep(const EresT& eres, CF1<T1,GPH<G1,G2,G0>>& model, OptionT& options) noexcept {
  _mstep_::mstep(eres, model.gph, options, typename matrix_category<G2>::type{});
  phcopy(model.gph, model);
  cf1sort(model);
  phcopy(model, model.gph);
  // unif
  copy(model.gph.Q, model.gph.P);
  double qv = unif(model.gph.P, model.gph.diag, options.ufactor);
  model.gph.qv = qv;
}

template <typename EresT, typename T1, typename G1, typename G2, typename G0, typename OptionT>
void mstep(const EresT& eres, CF1<T1,GPHPoi<GPH<G1,G2,G0>>>& model, OptionT& options) noexcept {
  _mstep_::mstep(eres, model.gph.gph, options, typename matrix_category<G2>::type{});
  phcopy(model.gph.gph, model);
  cf1sort(model);
  phcopy(model, model.gph.gph);
  // unif
  copy(model.gph.gph.Q, model.gph.gph.P);
  double qv = unif(model.gph.gph.P, model.gph.gph.diag, options.ufactor);
  model.gph.gph.qv = qv;
  model.gph.omega = eres.etotal;
}

#endif

