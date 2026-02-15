#ifndef MAPFIT_MAP_ERHMM_H
#define MAPFIT_MAP_ERHMM_H

#include <Rcpp.h>
#include "traits.h"
#include "map_data.h"
#include "map_models.h"
#include "gamma.h"
#include "blas.h"
#include "gth.h"

#define TDAT(k) (tdat[(k)-1])

template <typename Mv, typename Mi, typename Dv, typename Ev, typename Em, typename Mm,
          typename OptionT, typename WorkSpace>
double estep(
    const ErlangHMM<Mv,Mi,Mm>& model,
    const MAPTimeSample<Dv>& data,
    ErlangHMMEres<Ev,Em>& eres,
    OptionT,
    WorkSpace& work) {

  const int n = model.size();
  const int m = data.size();
  const double* tdat = stride_vector_traits<Dv,double>::value(data.time);

  std::vector<double> tmpv(n);
  std::vector<double> hew(n);
  std::vector<std::vector<double>> vf(m+2, std::vector<double>(n));
  std::vector<std::vector<double>> vb(m+2, std::vector<double>(n));
  std::vector<std::vector<double>> erl(m+2, std::vector<double>(n));

  // set erlang
  const double* rate = stride_vector_traits<Mv>::value(model.rate);
  const int* shape = stride_vector_traits<Mi,int>::value(model.shape);
  for (int k=1; k<=m; k++) {
    for (int i=0; i<n; i++) {
      erl[k][i] = gam::erlang_pdf(shape[i], rate[i], TDAT(k));
    }
  }
  fill(eres.eb, 0.0);
  fill(eres.ew0, 0.0);
  fill(eres.ew1, 0.0);
  fill(eres.en, 0.0);

  double scale;
  double llf = 0.0;

  copy(model.alpha, vf[0]);
  copy(model.xi, vb[m+1]);
  for (int i=0; i<n; i++) {
    vf[1][i] = erl[1][i] * vf[0][i];
  }
  scale = asum(vf[1]);
  scal(1.0/scale, vf[1]);

  for (int k=2; k<=m; k++) {
    gemv(TRANS{}, 1.0, model.P, vf[k-1], 0.0, vf[k]);
    for (int i=0; i<n; i++) {
      vf[k][i] *= erl[k][i];
    }
    scale = asum(vf[k]);
    scal(1.0/scale, vf[k]);
  }

  for (int k=m; k>=1; k--) {
    gemv(NOTRANS{}, 1.0, model.P, vb[k+1], 0.0, vb[k]);
    for (int i=0; i<n; i++) {
      vb[k][i] *= erl[k][i];
    }
    scale = asum(vb[k]);
    scal(1.0/scale, vb[k]);
    llf += log(scale);
  }
  
  for (int k=1; k<=m; k++) {
    gemv(NOTRANS{}, 1.0, model.P, vb[k+1], 0.0, tmpv);
    scale = dot(vf[k], tmpv);
    ger(NOTRANS{}, 1.0/scale, vf[k], vb[k+1], eres.en);
    copy(vf[k], hew);
    for (int i=0; i<n; i++) {
      hew[i] *= tmpv[i];
    }
    axpy(1.0/scale, hew, eres.ew0);
    axpy(TDAT(k)/scale, hew, eres.ew1);
  }
  
  const double* alpha = vector_traits<Mv>::value(model.alpha);
  const double* P = vector_traits<Mm>::value(model.P);
  double* eb = stride_vector_traits<Ev>::value(eres.eb);
  double* en = vector_traits<Em>::value(eres.en);
  
  copy(vb[1], eres.eb);
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
  }
  scale = asum(eres.eb);
  scal(1.0/scale, eres.eb);
  llf += log(scale);
  
  for (int i=0; i<vector_traits<Em>::size(eres.en); i++) {
    en[i] *= P[i];
  }
  return llf;
}

namespace _mstep_ {

template <typename Ev, typename Em, typename Tv, typename Ti, typename Tm,
          typename OptionT>
void mstep(const ErlangHMMEres<Ev,Em>& eres,
           ErlangHMM<Tv,Ti,Tm>& model, OptionT, DenseMatrixT) {
  const int n = model.size();
  const double* eb = stride_vector_traits<Ev>::value(eres.eb);
  const double* ew0 = stride_vector_traits<Ev>::value(eres.ew0);
  const double* ew1 = stride_vector_traits<Ev>::value(eres.ew1);
  const double* en = dense_matrix_traits<Tm>::value(eres.en);
  const int ld = dense_matrix_traits<Tm>::ld(eres.en);
  double* rate = stride_vector_traits<Tv>::value(model.rate);
  const int* shape = stride_vector_traits<Ti,int>::value(model.shape);
  double* P = dense_matrix_traits<Tm>::value(model.P);
  const int ldp = dense_matrix_traits<Tm>::ld(model.P);

  std::vector<double> tmpv(n, 0.0);
  
  for (int j=0; j<n; j++) {
    for (int i=0; i<n; i++) {
      tmpv[i] += en[j*ld+i];
    }
  }
  for (int j=0; j<n; j++) {
    for (int i=0; i<n; i++) {
      P[j*ldp+i] = en[j*ld+i] / tmpv[i];
    }
  }
  
  copy(eres.eb, model.alpha);
  for (int i=0; i<n; i++) {
    rate[i] = ew0[i] * shape[i] / ew1[i];
  }
}

}

template <typename Ev, typename Em, typename Tv, typename Ti, typename Tm,
          typename OptionT>
void mstep(const ErlangHMMEres<Ev,Em>& eres,
           ErlangHMM<Tv,Ti,Tm>& model,
           OptionT& options) {
  _mstep_::mstep(eres, model, options, typename matrix_category<Tm>::type{});
  if (options.stationary) {
    markov_gth(model.P, model.alpha);
  }
}

#endif
