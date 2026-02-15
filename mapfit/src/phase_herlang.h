#ifndef MAPFIT_PHASE_ERLANG_H
#define MAPFIT_PHASE_ERLANG_H

#include <Rcpp.h>
#include "traits.h"
#include "gamma.h"
#include "blas.h"
#include "phase_data.h"
#include "phase_models.h"

/**
 Description: estep for Erlang-PH with weighted time and group/truncated data
 
 alpha    (in): initial vector
 shape    (in): shape parameter vector
 rate     (in): rate parameter vector
 tdat     (in): interarrival time
 wdat     (in): weights for interarrivals
 gdat     (in): # of arrivals (-1 means NA)
 gdatlast (in): # of arrivals in [lasttime, infinity] (-1 means NA)
 idat     (in): indicator whether an arrival occurs at the last instant
 etotal  (out): expected # of arrivals
 eb      (out): expected # of starts
 ew      (out): expected sojourn time?
 
 return value -> llf (log-likelihood)
 */

#define TDAT(k) (tdat[(k)-1])
#define WDAT(k) (wdat[(k)-1])
#define GDAT(k) (gdat[(k)-1])
#define IDAT(k) (idat[(k)-1])

template <typename T1, typename T2, typename T3, typename T4, typename T5, typename OptionT, typename WorkSpace>
double estep(
    const HErlang<T1,T2>& model,
    const PHWeightSample<T3,T4>& data,
    HErlangEres<T5>& eres,
    OptionT,
    WorkSpace& work) {

  const int n = model.size();
  const int m = data.size();
  const double* alpha = stride_vector_traits<T1,double>::value(model.alpha);
  const double* rate = stride_vector_traits<T1,double>::value(model.rate);
  const int* shape = stride_vector_traits<T2,int>::value(model.shape);
  const double* tdat = stride_vector_traits<T3,double>::value(data.time);
  const double* wdat = stride_vector_traits<T4,double>::value(data.weights);
  
  // work
  // std::vector<std::vector<double>> perl0(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> perl1(m+1, std::vector<double>(n));
  std::vector<std::vector<double>>& perl0(work.perl0);
  std::vector<std::vector<double>>& perl1(work.perl1);
  
  double scale, tmp;
  double llf = 0.0;
  
  // set Erlang pdf
  tmp = 0.0;
  for (int k=1; k<=m; k++) {
    tmp += TDAT(k);
    for (int i=0; i<n; i++) {
      perl0[k][i] = gam::erlang_pdf(shape[i], rate[i], tmp);
      perl1[k][i] = tmp * perl0[k][i];
    }
  }
  
  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ew, 0.0);
  for (int k=1; k<=m; k++) {
    scale = dot(model.alpha, perl0[k]);
    axpy(WDAT(k)/scale, perl0[k], eres.eb);
    axpy(WDAT(k)/scale, perl1[k], eres.ew);
    llf += WDAT(k) * log(scale);
    eres.etotal += WDAT(k);
  }

  double* eb = stride_vector_traits<T5>::value(eres.eb);
  double* ew = stride_vector_traits<T5>::value(eres.ew);
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ew[i] *= alpha[i];
  }
  return llf;
}

template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename OptionT, typename WorkSpace>
double estep(
    const HErlang<T1,T2>& model,
    const PHGroupSample<T3,T4,T5>& data,
    HErlangEres<T6>& eres,
    OptionT,
    WorkSpace& work) {
  
  int n = model.size();
  int m = data.size();
  const double* alpha = stride_vector_traits<T1,double>::value(model.alpha);
  const double* rate = stride_vector_traits<T1,double>::value(model.rate);
  const int* shape = stride_vector_traits<T2,int>::value(model.shape);
  const double* tdat = stride_vector_traits<T3,double>::value(data.time);
  const int* gdat = stride_vector_traits<T4,int>::value(data.counts);
  const int* idat = stride_vector_traits<T5,int>::value(data.indicators);
  int gdatlast = data.last;

  // workspace
  std::vector<double> tmpv0(n);
  std::vector<double> tmpv1(n);
  // std::vector<std::vector<double>> perl0(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> perl1(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> cerl0(m+2, std::vector<double>(n));
  // std::vector<std::vector<double>> cerl1(m+2, std::vector<double>(n));
  std::vector<std::vector<double>>& perl0(work.perl0);
  std::vector<std::vector<double>>& perl1(work.perl1);
  std::vector<std::vector<double>>& cerl0(work.cerl0);
  std::vector<std::vector<double>>& cerl1(work.cerl1);
  
  // set Erlang pdf and cdf
  double tmp = 0.0;
  fill(cerl0[0], 0.0);
  fill(cerl1[0], 0.0);
  for (int k=1; k<=m; k++) {
    tmp += TDAT(k);
    for (int i=0; i<n; i++) {
      perl0[k][i] = gam::erlang_pdf(shape[i], rate[i], tmp);
      perl1[k][i] = tmp * perl0[k][i];
      cerl0[k][i] = gam::erlang_cdf(shape[i], rate[i], tmp);
      cerl1[k][i] = (shape[i] / rate[i]) * gam::erlang_cdf(shape[i]+1, rate[i], tmp);
    }
  }
  fill(cerl0[m+1], 1.0);
  for (int i=0; i<n; i++) {
    cerl1[m+1][i] = shape[i] / rate[i];
  }
  
  // estep
  double scale;
  double nn = 0.0;
  double uu = 0.0;
  double llf = 0.0;
  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ew, 0.0);
  for (int k=1; k<=m; k++) {
    if (GDAT(k) >= 0 && TDAT(k) != 0.0) {
      // tmpv0 = cerl0[k] - cerl0[k-1];
      // tmpv1 = cerl1[k] - cerl1[k-1];
      copy(cerl0[k], tmpv0);
      copy(cerl1[k], tmpv1);
      axpy(-1.0, cerl0[k-1], tmpv0);
      axpy(-1.0, cerl1[k-1], tmpv1);
      scale = dot(model.alpha, tmpv0);
      nn += GDAT(k);
      uu += scale;
      axpy(GDAT(k)/scale, tmpv0, eres.eb);
      axpy(GDAT(k)/scale, tmpv1, eres.ew);
      llf += GDAT(k) * log(scale) - gam::lfact(GDAT(k));
    }
    if (IDAT(k) == 1) {
      scale = dot(model.alpha, perl0[k]);
      nn += 1.0;
      axpy(1.0/scale, perl0[k], eres.eb);
      axpy(1.0/scale, perl1[k], eres.ew);
      llf += log(scale);
    }
  }
  if (gdatlast >= 0) {
    // tmpv0 = cerl0[m+1] - cerl0[m];
    // tmpv1 = cerl1[m+1] - cerl1[m];
    copy(cerl0[m+1], tmpv0);
    copy(cerl1[m+1], tmpv1);
    axpy(-1.0, cerl0[m], tmpv0);
    axpy(-1.0, cerl1[m], tmpv1);
    scale = dot(model.alpha, tmpv0);
    nn += gdatlast;
    uu += scale;
    axpy(gdatlast/scale, tmpv0, eres.eb);
    axpy(gdatlast/scale, tmpv1, eres.ew);
    llf += gdatlast * log(scale) - gam::lfact(gdatlast);
  }
  
  for (int k=1; k<=m; k++) {
    if (GDAT(k) == -1) {
      // tmpv0 = cerl0[k] - cerl0[k-1];
      // tmpv1 = cerl1[k] - cerl1[k-1];
      copy(cerl0[k], tmpv0);
      copy(cerl1[k], tmpv1);
      axpy(-1.0, cerl0[k-1], tmpv0);
      axpy(-1.0, cerl1[k-1], tmpv1);
      axpy(nn/uu, tmpv0, eres.eb);
      axpy(nn/uu, tmpv1, eres.ew);
    }
  }
  if (gdatlast == -1) {
    // tmpv0 = cerl0[m+1] - cerl0[m];
    // tmpv1 = cerl1[m+1] - cerl1[m];
    copy(cerl0[m+1], tmpv0);
    copy(cerl1[m+1], tmpv1);
    axpy(-1.0, cerl0[m], tmpv0);
    axpy(-1.0, cerl1[m], tmpv1);
    axpy(nn/uu, tmpv0, eres.eb);
    axpy(nn/uu, tmpv1, eres.ew);
  }
  llf += gam::lgamma(nn + 1.0) - nn * log(uu);
  
  double* eb = stride_vector_traits<T6>::value(eres.eb);
  double* ew = stride_vector_traits<T6>::value(eres.ew);
  eres.etotal = nn / uu;
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ew[i] *= alpha[i];
  }
  return llf;
}

template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename OptionT, typename WorkSpace>
double estep(
    const HErlangPoi<T1,T2>& model,
    const PHGroupSample<T3,T4,T5>& data,
    HErlangEres<T6>& eres,
    OptionT,
    WorkSpace& work) {
  
  int n = model.size();
  int m = data.size();
  const double* alpha = stride_vector_traits<T1,double>::value(model.alpha);
  const double* rate = stride_vector_traits<T1,double>::value(model.rate);
  const int* shape = stride_vector_traits<T2,int>::value(model.shape);
  const double* tdat = stride_vector_traits<T3,double>::value(data.time);
  const int* gdat = stride_vector_traits<T4,int>::value(data.counts);
  const int* idat = stride_vector_traits<T5,int>::value(data.indicators);
  int gdatlast = data.last;
  double omega = model.omega;
  
  // workspace
  std::vector<double> tmpv0(n);
  std::vector<double> tmpv1(n);
  // std::vector<std::vector<double>> perl0(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> perl1(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> cerl0(m+2, std::vector<double>(n));
  // std::vector<std::vector<double>> cerl1(m+2, std::vector<double>(n));
  std::vector<std::vector<double>>& perl0(work.perl0);
  std::vector<std::vector<double>>& perl1(work.perl1);
  std::vector<std::vector<double>>& cerl0(work.cerl0);
  std::vector<std::vector<double>>& cerl1(work.cerl1);
  
    // workspace

  // set Erlang pdf and cdf
  double tmp = 0.0;
  fill(cerl0[0], 0.0);
  fill(cerl1[0], 0.0);
  for (int k=1; k<=m; k++) {
    tmp += TDAT(k);
    for (int i=0; i<n; i++) {
      perl0[k][i] = gam::erlang_pdf(shape[i], rate[i], tmp);
      perl1[k][i] = tmp * perl0[k][i];
      cerl0[k][i] = gam::erlang_cdf(shape[i], rate[i], tmp);
      cerl1[k][i] = (shape[i] / rate[i]) * gam::erlang_cdf(shape[i]+1, rate[i], tmp);
    }
  }
  fill(cerl0[m+1], 1.0);
  for (int i=0; i<n; i++) {
    cerl1[m+1][i] = shape[i] / rate[i];
  }

  // estep
  double scale;
  double nn = 0.0;
  double uu = 0.0;
  double llf = 0.0;
  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ew, 0.0);
  for (int k=1; k<=m; k++) {
    if (GDAT(k) >= 0 && TDAT(k) != 0.0) {
      // tmpv0 = cerl0[k] - cerl0[k-1];
      // tmpv1 = cerl1[k] - cerl1[k-1];
      copy(cerl0[k], tmpv0);
      copy(cerl1[k], tmpv1);
      axpy(-1.0, cerl0[k-1], tmpv0);
      axpy(-1.0, cerl1[k-1], tmpv1);
      scale = dot(model.alpha, tmpv0);
      nn += GDAT(k);
      uu += scale;
      axpy(GDAT(k)/scale, tmpv0, eres.eb);
      axpy(GDAT(k)/scale, tmpv1, eres.ew);
      llf += GDAT(k) * log(scale) - gam::lfact(GDAT(k));
    }
    if (IDAT(k) == 1) {
      scale = dot(model.alpha, perl0[k]);
      nn += 1.0;
      axpy(1.0/scale, perl0[k], eres.eb);
      axpy(1.0/scale, perl1[k], eres.ew);
      llf += log(scale);
    }
  }
  if (gdatlast >= 0) {
    // tmpv0 = cerl0[m+1] - cerl0[m];
    // tmpv1 = cerl1[m+1] - cerl1[m];
    copy(cerl0[m+1], tmpv0);
    copy(cerl1[m+1], tmpv1);
    axpy(-1.0, cerl0[m], tmpv0);
    axpy(-1.0, cerl1[m], tmpv1);
    scale = dot(model.alpha, tmpv0);
    nn += gdatlast;
    uu += scale;
    axpy(gdatlast/scale, tmpv0, eres.eb);
    axpy(gdatlast/scale, tmpv1, eres.ew);
    llf += gdatlast * log(scale) - gam::lfact(gdatlast);
  }

  for (int k=1; k<=m; k++) {
    if (GDAT(k) == -1) {
      // tmpv0 = cerl0[k] - cerl0[k-1];
      // tmpv1 = cerl1[k] - cerl1[k-1];
      copy(cerl0[k], tmpv0);
      copy(cerl1[k], tmpv1);
      axpy(-1.0, cerl0[k-1], tmpv0);
      axpy(-1.0, cerl1[k-1], tmpv1);
      axpy(omega, tmpv0, eres.eb);
      axpy(omega, tmpv1, eres.ew);
    }
  }
  if (gdatlast == -1) {
    // tmpv0 = cerl0[m+1] - cerl0[m];
    // tmpv1 = cerl1[m+1] - cerl1[m];
    copy(cerl0[m+1], tmpv0);
    copy(cerl1[m+1], tmpv1);
    axpy(-1.0, cerl0[m], tmpv0);
    axpy(-1.0, cerl1[m], tmpv1);
    axpy(omega, tmpv0, eres.eb);
    axpy(omega, tmpv1, eres.ew);
  }
  llf += nn * log(omega) - omega * uu;

  double* eb = stride_vector_traits<T6>::value(eres.eb);
  double* ew = stride_vector_traits<T6>::value(eres.ew);
  eres.etotal = nn + omega * (1.0 - uu);
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ew[i] *= alpha[i];
  }
  return llf;
}

template <typename V, typename I, typename V2, typename OptionT>
void mstep(const HErlangEres<V2>& eres,
           HErlang<V,I>& model,
           OptionT) {

  using trait2 = stride_vector_traits<V>;
  using trait3 = stride_vector_traits<I,int>;
  using trait4 = stride_vector_traits<V2>;
  double* alpha = trait2::value(model.alpha);
  double* rate = trait2::value(model.rate);
  int* shape = trait3::value(model.shape);
  const double* eb = trait4::value(eres.eb);
  const double* ew = trait4::value(eres.ew);

  int n = model.size();
  copy(eres.eb, model.alpha);
  scal(1.0/eres.etotal, model.alpha);
  for (int i=0; i<n; i++) {
    rate[i] = shape[i] * eb[i] / ew[i];
  }
}

template <typename V, typename I, typename V2, typename OptionT>
void mstep(const HErlangEres<V2>& eres,
           HErlangPoi<V,I>& model,
           OptionT) {

  using trait2 = stride_vector_traits<V>;
  using trait3 = stride_vector_traits<I,int>;
  using trait4 = stride_vector_traits<V2>;
  double* alpha = trait2::value(model.alpha);
  double* rate = trait2::value(model.rate);
  int* shape = trait3::value(model.shape);
  const double* eb = trait4::value(eres.eb);
  const double* ew = trait4::value(eres.ew);

  int n = model.size();
  copy(eres.eb, model.alpha);
  scal(1.0/eres.etotal, model.alpha);
  model.omega = eres.etotal;
  for (int i=0; i<n; i++) {
    rate[i] = shape[i] * eb[i] / ew[i];
  }
}

#endif
