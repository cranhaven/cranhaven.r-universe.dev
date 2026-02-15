#ifndef MAPFIT_PHASE_GPH
#define MAPFIT_PHASE_GPH

#include <Rcpp.h>
#include "traits.h"
#include "poisson.h"
#include "gamma.h"
#include "blas.h"
#include "unif.h"
#include "phase_data.h"
#include "phase_models.h"

/**
 Description: estep for PH with weighted time and group/truncated data
 
 alpha      (in): initial vector
 baralpha   (in): baralpha = alpha (-Q)^-1
 xi         (in): exit vector
 one        (in): one vector
 Q          (in): infinitesimal generator
 P          (in): uniformed generator
 qv         (in): uniformization constant
 tdat       (in): interarrival time
 wdat       (in): weights for interarrivals
 gdat       (in): # of arrivals (-1 means NA)
 gdatlast   (in): # of arrivals in [lasttime, infinity] (-1 means NA)
 idat       (in): indicator whether an arrival occurs at the last instant
 etotal    (out): expected # of arrivals
 eb        (out): expected # of starts
 ey        (out): expected # of exits
 ez        (out): expected sojourn time
 en        (out): expected # of phase transitions
 poi_eps    (in): eps for poisson prob (optional)
 atol       (in): tolerance error in uniformization (optional)
 
 return value -> llf (log-likelihood)
 
 */

#define TDAT(k) (tdat[(k)-1])
#define WDAT(k) (wdat[(k)-1])
#define GDAT(k) (gdat[(k)-1])
#define IDAT(k) (idat[(k)-1])

template <typename T0, typename T1, typename T2, typename T4, typename T5, typename T6, typename T7,
          typename OptionT, typename WorkSpace>
double estep(
    const GPH<T1,T2,T0>& model,
    const PHWeightSample<T4,T5>& data,
    GPHEres<T6,T7>& eres,
    OptionT& options,
    WorkSpace& work) noexcept {
  
  const int m = data.size();
  const double* tdat = vector_traits<T4,double>::value(data.time);
  const double* wdat = vector_traits<T5,double>::value(data.weights);
  double tmax = data.maxtime;
  
  int n = model.size();
  double qv = model.qv;
  
  // alloc
  int right = poi::rightbound(qv*tmax, options.poisson_eps) + 1;
  std::vector<double> prob(right+1);
  std::vector<std::vector<double>> vx(right+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vf(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vb(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vc(m+1, std::vector<double>(n));
  std::vector<double> blf(m+1);
  std::vector<double> tmpv(n);
  std::vector<double> xtmp(n);

  std::vector<std::vector<double>>& vf(work.vf);
  std::vector<std::vector<double>>& vb(work.vb);
  std::vector<std::vector<double>>& vc(work.vc);
  
  double scale;
  double llf = 0.0;
  double tllf = 0.0;
  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ey, 0.0);
  fill(eres.ez, 0.0);
  fill(eres.en, 0.0);
  
  // forward & backward
  copy(model.alpha, vf[0]);
  copy(model.xi, vb[0]);
  for (int k=1; k<=m; k++) {
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);
    
    // mexp::mexp_unifvec(sci::mat::T, P, qv, r, poi, weight, vf[k-1], vf[k], 0.0);
    fill(vf[k], 0.0);
    copy(vf[k-1], xtmp);
    axpy(prob[0], xtmp, vf[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.P, xtmp, 0.0, tmpv);
      copy(tmpv, xtmp);
      axpy(prob[u], xtmp, vf[k]);
    }
    scal(1.0/weight, vf[k]);
    scale = dot(vf[k], model.xi);
    scal(1.0/scale, vf[k]);
    axpy(WDAT(k), vf[k], eres.ey);

    blf[k] = scale;
    // mexp::mexp_unifvec(sci::mat::N, P, qv, r, poi, weight, vb[k-1], vb[k], 0.0);
    fill(vb[k], 0.0);
    copy(vb[k-1], xtmp);
    axpy(prob[0], xtmp, vb[k]);
    for (int u=1; u<=right; u++) {
      gemv(NOTRANS{}, 1.0, model.P, xtmp, 0.0, tmpv);
      copy(tmpv, xtmp);
      axpy(prob[u], xtmp, vb[k]);
    }
    scal(1.0/weight, vb[k]);
    scale = dot(model.alpha, vb[k]);
    scal(1.0/scale, vb[k]);
    axpy(WDAT(k), vb[k], eres.eb);
    
    eres.etotal += WDAT(k);
    tllf += log(blf[k]);
    llf += WDAT(k) * tllf;
  }
  
  fill(vc[m], 0.0);
  axpy(WDAT(m)/blf[m], model.alpha, vc[m]);
  for (int k=m-1; k>=1; k--) {
    int right = poi::rightbound(qv*TDAT(k+1), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k+1), 0, right, prob);
    
    // mexp::mexp_unifvec(sci::mat::T, P, qv, r, poi, weight, vc[k+1], vc[k], 0.0);
    fill(vc[k], 0.0);
    copy(vc[k+1], xtmp);
    axpy(prob[0], xtmp, vc[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.P, xtmp, 0.0, tmpv);
      copy(tmpv, xtmp);
      axpy(prob[u], xtmp, vc[k]);
    }
    scal(1.0/(weight*blf[k]), vc[k]);
    axpy(WDAT(k)/blf[k], model.alpha, vc[k]);
  }
  
  for (int k=1; k<=m; k++) {
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);
    
    // mexp::mexpc_unif(sci::mat::T, sci::mat::N, P, qv, r, poi, weight,
    //                  vc[k], vb[k-1], vb[k-1], en);
    fill(vx[right], 0.0);
    axpy(prob[right], vb[k-1], vx[right]);
    for (int l=right-1; l>=1; l--) {
      gemv(NOTRANS{}, 1.0, model.P, vx[l+1], 0.0, vx[l]);
      axpy(prob[l], vb[k-1], vx[l]);
    }
    
    ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[1], eres.en);
    for (int l=1; l<=right-1; l++) {
      gemv(TRANS{}, 1.0, model.P, vc[k], 0.0, tmpv);
      copy(tmpv, vc[k]);
      ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[l+1], eres.en);
    }
  }
  
  const double* alpha = vector_traits<T1>::value(model.alpha);
  const double* Q = vector_traits<T2>::value(model.Q);
  const double* xi = vector_traits<T1>::value(model.xi);
  const int* diag = vector_traits<T0,int>::value(model.diag);
  double* eb = vector_traits<T6>::value(eres.eb);
  double* ey = vector_traits<T6>::value(eres.ey);
  double* ez = vector_traits<T6>::value(eres.ez);
  double* en = vector_traits<T7>::value(eres.en);
  
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ey[i] *= xi[i];
    ez[i] = en[diag[i]];
  }
  for (int i=0; i<vector_traits<T7>::size(eres.en); i++) {
    en[i] *= Q[i];
  }

  return llf;
}

template <typename T0, typename T1, typename T2, typename T4,
          typename T5, typename T6, typename T7, typename T8,
          typename OptionT, typename WorkSpace>
double estep(
    const GPH<T1,T2,T0>& model,
    const PHGroupSample<T4,T5,T6>& data,
    GPHEres<T7,T8>& eres,
    OptionT& options,
    WorkSpace& work) noexcept {
  
  const int m = data.size();
  const double* tdat = stride_vector_traits<T4,double>::value(data.time);
  const int* gdat = stride_vector_traits<T5,int>::value(data.counts);
  const int* idat = stride_vector_traits<T6,int>::value(data.indicators);
  const int gdatlast = data.last;
  const double tmax = data.maxtime;
  
  int n = model.size();
  double qv = model.qv;
  std::vector<double> baralpha(n);
  std::vector<double> vone(n, 1.0);
  gesv(TRANS{}, -1.0, model.Q, model.alpha, baralpha);
  
  // work
  int right = poi::rightbound(qv*tmax, options.poisson_eps) + 1;
  std::vector<double> prob(right+1);
  std::vector<std::vector<double>> vx(right+1, std::vector<double>(n, 0));
  std::vector<double> tmpvf(n);
  std::vector<double> tmpvb(n);
  std::vector<double> tmpv(n);
  // std::vector<std::vector<double>> barvf(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> barvb(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vb(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vc(m+1, std::vector<double>(n));
  std::vector<double> wg(m+2);
  std::vector<double> wp(m+2);

  std::vector<std::vector<double>>& barvf(work.barvf);
  std::vector<std::vector<double>>& barvb(work.barvb);
  std::vector<std::vector<double>>& vb(work.vb);
  std::vector<std::vector<double>>& vc(work.vc);

  double scale;
  double llf = 0.0;
  double tllf = 0.0;
  int nn = 0;
  double uu = 0.0;

  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ey, 0.0);
  fill(eres.ez, 0.0);
  fill(eres.en, 0.0);
  
  // forward & backward
  copy(baralpha, barvf[0]);
  copy(vone, barvb[0]);
  copy(model.xi, vb[0]);
  for (int k=1; k<=m; k++) {
    // barvf[k] = barvf[k-1] * exp(T * tdat[k])
    // barvb[k] = exp(T * tdat[k]) * barvb[k-1]

    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);

    fill(barvf[k], 0.0);
    fill(barvb[k], 0.0);
    copy(barvf[k-1], tmpvf);
    copy(barvb[k-1], tmpvb);
    axpy(prob[0], tmpvf, barvf[k]);
    axpy(prob[0], tmpvb, barvb[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.P, tmpvf, 0.0, tmpv);
      copy(tmpv, tmpvf);
      gemv(NOTRANS{}, 1.0, model.P, tmpvb, 0.0, tmpv);
      copy(tmpv, tmpvb);
      axpy(prob[u], tmpvf, barvf[k]);
      axpy(prob[u], tmpvb, barvb[k]);
    }
    scal(1.0/weight, barvf[k]);
    scal(1.0/weight, barvb[k]);

    // vb[k] = (-ph.T) * barvb[k]
    gemv(NOTRANS{}, -1.0, model.Q, barvb[k], 0.0, vb[k]);

    // tmpvf = barvf[k-1] - barvf[k]
    // tmpvb = barvb[k-1] - barvb[k]
    copy(barvf[k-1], tmpvf);
    copy(barvb[k-1], tmpvb);
    axpy(-1.0, barvf[k], tmpvf);
    axpy(-1.0, barvb[k], tmpvb);

    if (GDAT(k) >= 0 && TDAT(k) > 0.0) {
      double tmp = dot(model.alpha, tmpvb);
      llf += GDAT(k) * log(tmp) - gam::lfact(GDAT(k));
      nn += GDAT(k);
      uu += tmp;
      wg[k] = GDAT(k) / tmp;
      axpy(wg[k], tmpvb, eres.eb);
      axpy(wg[k], tmpvf, eres.ey);
    }
    if (IDAT(k) == 1) {
      double tmp = dot(model.alpha, vb[k]);
      llf += log(tmp);
      nn += 1;
      wp[k] = 1 / tmp;
      axpy(wp[k], vb[k], eres.eb);
      gemv(TRANS{}, -wp[k], model.Q, barvf[k], 1.0, eres.ey);
    } else {
      wp[k] = 0.0;
    }
    
  }
  // for the interval [t_m, infinity)
  if (gdatlast >= 0) {
    double tmp = dot(model.alpha, barvb[m]);
    llf += gdatlast * log(tmp) - gam::lfact(gdatlast);
    nn += gdatlast;
    uu += tmp;
    wg[m+1] = gdatlast / tmp;
    axpy(wg[m+1], barvb[m], eres.eb);
    axpy(wg[m+1], barvf[m], eres.ey);
  }
  for (int k=1; k<=m; k++) {
    if (GDAT(k) == -1) {
      wg[k] = nn / uu;
      copy(barvf[k-1], tmpvf);
      copy(barvb[k-1], tmpvb);
      axpy(-1.0, barvf[k], tmpvf);
      axpy(-1.0, barvb[k], tmpvb);
      axpy(wg[k], tmpvb, eres.eb);
      axpy(wg[k], tmpvf, eres.ey);
    }
  }
  if (gdatlast == -1) {
    wg[m+1] = nn / uu;
    axpy(wg[m+1], barvb[m], eres.eb);
    axpy(wg[m+1], barvf[m], eres.ey);
  }
  llf += gam::lfact(nn) - nn * log(uu);

  // compute vectors and convolution

  fill(vc[m], 0.0);
  axpy(wg[m+1] - wg[m], baralpha, vc[m]);
  if (IDAT(m) == 1) {
    axpy(wp[m], model.alpha, vc[m]);
  }
  for (int k=m-1; k>=1; k--) {
    // vc[k] = vc[k+1] * exp(T * tdat[k+1]) + (wg[k+1] - wg[k]) * baralpha + I(idat[k]==1) (wp[k] * alpha)
    int right = poi::rightbound(qv*TDAT(k+1), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k+1), 0, right, prob);

    fill(vc[k], 0.0);
    copy(vc[k+1], tmpvf);
    axpy(prob[0], tmpvf, vc[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.P, tmpvf, 0.0, tmpv);
      copy(tmpv, tmpvf);
      axpy(prob[u], tmpvf, vc[k]);
    }
    scal(1.0/weight, vc[k]);
    axpy(wg[k+1]-wg[k], baralpha, vc[k]);
    if (IDAT(k) == 1) {
      axpy(wp[k], model.alpha, vc[k]);
    }
  }

  for (int k=1; k<=m; k++) {
    // compute convolution integral
    // int_0^tdat[k] exp(T* s) * vb[k-1] * vc[k] * exp(T(tdat[k]-s)) ds
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);

    fill(vx[right], 0.0);
    axpy(prob[right], vb[k-1], vx[right]);
    for (int l=right-1; l>=1; l--) {
      gemv(NOTRANS{}, 1.0, model.P, vx[l+1], 0.0, vx[l]);
      axpy(prob[l], vb[k-1], vx[l]);
    }

    ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[1], eres.en);
    for (int l=1; l<=right-1; l++) {
      gemv(TRANS{}, 1.0, model.P, vc[k], 0.0, tmpv);
      copy(tmpv, vc[k]);
      ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[l+1], eres.en);
    }
    ger(NOTRANS{}, wg[k+1]-wg[k], baralpha, barvb[k], eres.en);
  }
  ger(NOTRANS{}, wg[1], baralpha, barvb[0], eres.en);

  const double* alpha = vector_traits<T1>::value(model.alpha);
  const double* Q = vector_traits<T2>::value(model.Q);
  const double* xi = vector_traits<T1>::value(model.xi);
  const int* diag = vector_traits<T0,int>::value(model.diag);
  double* eb = vector_traits<T7>::value(eres.eb);
  double* ey = vector_traits<T7>::value(eres.ey);
  double* ez = vector_traits<T7>::value(eres.ez);
  double* en = vector_traits<T8>::value(eres.en);

  eres.etotal = nn / uu;
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ey[i] *= xi[i];
    ez[i] = en[diag[i]];
  }
  for (int i=0; i<vector_traits<T8>::size(eres.en); i++) {
    en[i] *= Q[i];
  }
  return llf;
}

template <typename T0, typename T1, typename T2, typename T4,
          typename T5, typename T6, typename T7, typename T8,
          typename OptionT, typename WorkSpace>
double estep(
    const GPHPoi<GPH<T1,T2,T0>>& model,
    const PHGroupSample<T4,T5,T6>& data,
    GPHEres<T7,T8>& eres,
    OptionT& options,
    WorkSpace& work) noexcept {
  
  const int m = data.size();
  const double* tdat = stride_vector_traits<T4,double>::value(data.time);
  const int* gdat = stride_vector_traits<T5,int>::value(data.counts);
  const int* idat = stride_vector_traits<T6,int>::value(data.indicators);
  const int gdatlast = data.last;
  const double tmax = data.maxtime;
  
  int n = model.gph.size();
  double qv = model.gph.qv;
  double omega = model.omega;
  std::vector<double> baralpha(n);
  std::vector<double> vone(n, 1.0);
  gesv(TRANS{}, -1.0, model.gph.Q, model.gph.alpha, baralpha);
  
  // work
  int right = poi::rightbound(qv*tmax, options.poisson_eps) + 1;
  std::vector<double> prob(right+1);
  std::vector<std::vector<double>> vx(right+1, std::vector<double>(n, 0));
  std::vector<double> tmpvf(n);
  std::vector<double> tmpvb(n);
  std::vector<double> tmpv(n);
  // std::vector<std::vector<double>> barvf(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> barvb(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vb(m+1, std::vector<double>(n));
  // std::vector<std::vector<double>> vc(m+1, std::vector<double>(n));
  std::vector<double> wg(m+2);
  std::vector<double> wp(m+2);
  
  std::vector<std::vector<double>>& barvf(work.barvf);
  std::vector<std::vector<double>>& barvb(work.barvb);
  std::vector<std::vector<double>>& vb(work.vb);
  std::vector<std::vector<double>>& vc(work.vc);

  double scale;
  double llf = 0.0;
  double tllf = 0.0;
  int nn = 0;
  double uu = 0.0;
  
  eres.etotal = 0.0;
  fill(eres.eb, 0.0);
  fill(eres.ey, 0.0);
  fill(eres.ez, 0.0);
  fill(eres.en, 0.0);
  
  // forward & backward
  copy(baralpha, barvf[0]);
  copy(vone, barvb[0]);
  copy(model.gph.xi, vb[0]);
  for (int k=1; k<=m; k++) {
    // barvf[k] = barvf[k-1] * exp(T * tdat[k])
    // barvb[k] = exp(T * tdat[k]) * barvb[k-1]
    
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);
    
    fill(barvf[k], 0.0);
    fill(barvb[k], 0.0);
    copy(barvf[k-1], tmpvf);
    copy(barvb[k-1], tmpvb);
    axpy(prob[0], tmpvf, barvf[k]);
    axpy(prob[0], tmpvb, barvb[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.gph.P, tmpvf, 0.0, tmpv);
      copy(tmpv, tmpvf);
      gemv(NOTRANS{}, 1.0, model.gph.P, tmpvb, 0.0, tmpv);
      copy(tmpv, tmpvb);
      axpy(prob[u], tmpvf, barvf[k]);
      axpy(prob[u], tmpvb, barvb[k]);
    }
    scal(1.0/weight, barvf[k]);
    scal(1.0/weight, barvb[k]);
    
    // vb[k] = (-ph.T) * barvb[k]
    gemv(NOTRANS{}, -1.0, model.gph.Q, barvb[k], 0.0, vb[k]);
    
    // tmpvf = barvf[k-1] - barvf[k]
    // tmpvb = barvb[k-1] - barvb[k]
    copy(barvf[k-1], tmpvf);
    copy(barvb[k-1], tmpvb);
    axpy(-1.0, barvf[k], tmpvf);
    axpy(-1.0, barvb[k], tmpvb);
    
    if (GDAT(k) >= 0 && TDAT(k) > 0.0) {
      double tmp = dot(model.gph.alpha, tmpvb);
      llf += GDAT(k) * log(tmp) - gam::lfact(GDAT(k));
      nn += GDAT(k);
      uu += tmp;
      wg[k] = GDAT(k) / tmp;
      axpy(wg[k], tmpvb, eres.eb);
      axpy(wg[k], tmpvf, eres.ey);
    }
    if (IDAT(k) == 1) {
      double tmp = dot(model.gph.alpha, vb[k]);
      llf += log(tmp);
      nn += 1;
      wp[k] = 1 / tmp;
      axpy(wp[k], vb[k], eres.eb);
      gemv(TRANS{}, -wp[k], model.gph.Q, barvf[k], 1.0, eres.ey);
    } else {
      wp[k] = 0.0;
    }
    
  }
  // for the interval [t_m, infinity)
  if (gdatlast >= 0) {
    double tmp = dot(model.gph.alpha, barvb[m]);
    llf += gdatlast * log(tmp) - gam::lfact(gdatlast);
    nn += gdatlast;
    uu += tmp;
    wg[m+1] = gdatlast / tmp;
    axpy(wg[m+1], barvb[m], eres.eb);
    axpy(wg[m+1], barvf[m], eres.ey);
  }
  for (int k=1; k<=m; k++) {
    if (GDAT(k) == -1) {
      wg[k] = omega;
      copy(barvf[k-1], tmpvf);
      copy(barvb[k-1], tmpvb);
      axpy(-1.0, barvf[k], tmpvf);
      axpy(-1.0, barvb[k], tmpvb);
      axpy(wg[k], tmpvb, eres.eb);
      axpy(wg[k], tmpvf, eres.ey);
    }
  }
  if (gdatlast == -1) {
    wg[m+1] = omega;
    axpy(wg[m+1], barvb[m], eres.eb);
    axpy(wg[m+1], barvf[m], eres.ey);
  }
  llf += nn * log(omega) - omega * uu;
  
  // compute vectors and convolution
  
  fill(vc[m], 0.0);
  axpy(wg[m+1] - wg[m], baralpha, vc[m]);
  if (IDAT(m) == 1) {
    axpy(wp[m], model.gph.alpha, vc[m]);
  }
  for (int k=m-1; k>=1; k--) {
    // vc[k] = vc[k+1] * exp(T * tdat[k+1]) + (wg[k+1] - wg[k]) * baralpha + I(idat[k]==1) (wp[k] * alpha)
    int right = poi::rightbound(qv*TDAT(k+1), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k+1), 0, right, prob);
    
    fill(vc[k], 0.0);
    copy(vc[k+1], tmpvf);
    axpy(prob[0], tmpvf, vc[k]);
    for (int u=1; u<=right; u++) {
      gemv(TRANS{}, 1.0, model.gph.P, tmpvf, 0.0, tmpv);
      copy(tmpv, tmpvf);
      axpy(prob[u], tmpvf, vc[k]);
    }
    scal(1.0/weight, vc[k]);
    axpy(wg[k+1]-wg[k], baralpha, vc[k]);
    if (IDAT(k) == 1) {
      axpy(wp[k], model.gph.alpha, vc[k]);
    }
  }
  
  for (int k=1; k<=m; k++) {
    // compute convolution integral
    // int_0^tdat[k] exp(T* s) * vb[k-1] * vc[k] * exp(T(tdat[k]-s)) ds
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, right, prob);
    
    fill(vx[right], 0.0);
    axpy(prob[right], vb[k-1], vx[right]);
    for (int l=right-1; l>=1; l--) {
      gemv(NOTRANS{}, 1.0, model.gph.P, vx[l+1], 0.0, vx[l]);
      axpy(prob[l], vb[k-1], vx[l]);
    }
    
    ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[1], eres.en);
    for (int l=1; l<=right-1; l++) {
      gemv(TRANS{}, 1.0, model.gph.P, vc[k], 0.0, tmpv);
      copy(tmpv, vc[k]);
      ger(NOTRANS{}, 1.0/(qv*weight), vc[k], vx[l+1], eres.en);
    }
    ger(NOTRANS{}, wg[k+1]-wg[k], baralpha, barvb[k], eres.en);
  }
  ger(NOTRANS{}, wg[1], baralpha, barvb[0], eres.en);
  
  const double* alpha = vector_traits<T1>::value(model.gph.alpha);
  const double* Q = vector_traits<T2>::value(model.gph.Q);
  const double* xi = vector_traits<T1>::value(model.gph.xi);
  const int* diag = vector_traits<T0,int>::value(model.gph.diag);
  double* eb = vector_traits<T7>::value(eres.eb);
  double* ey = vector_traits<T7>::value(eres.ey);
  double* ez = vector_traits<T7>::value(eres.ez);
  double* en = vector_traits<T8>::value(eres.en);
  
  eres.etotal = nn + omega * (1.0 - uu);
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ey[i] *= xi[i];
    ez[i] = en[diag[i]];
  }
  for (int i=0; i<vector_traits<T8>::size(eres.en); i++) {
    en[i] *= Q[i];
  }
  return llf;
}

namespace _mstep_ {

template <typename T0, typename T1, typename T2, typename T3, typename T4,
          typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPH<T3,T4,T0>& model,
           OptionT& options, DenseMatrixT) noexcept{
  const int n = model.size();
  const double* eb = vector_traits<T1>::value(eres.eb);
  const double* ey = vector_traits<T1>::value(eres.ey);
  const double* ez = vector_traits<T1>::value(eres.ez);
  const double* en = dense_matrix_traits<T2>::value(eres.en);
  const int ld = dense_matrix_traits<T2>::ld(eres.en);
  double* alpha = vector_traits<T3>::value(model.alpha);
  double* xi = vector_traits<T3>::value(model.xi);
  double* Q = dense_matrix_traits<T4>::value(model.Q);
  const int ldq = dense_matrix_traits<T4>::ld(model.Q);
  const int* d = vector_traits<T0,int>::value(model.diag);
  std::vector<double> tmpv(n, 0.0);
  
  for (int j=0; j<n; j++) {
    for (int i=0; i<n; i++) {
      if (i != j) {
        Q[j*ldq+i] = en[j*ld+i] / ez[i];
        tmpv[i] += Q[j*ldq+i];
      }
    }
  }
  for (int i=0; i<n; i++) {
    alpha[i] = eb[i] / eres.etotal;
    xi[i] = ey[i] / ez[i];
    tmpv[i] += xi[i];
    Q[d[i]] = -tmpv[i];
  }
}

template <typename T0, typename T1, typename T2, typename T3, typename T4,
          typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPH<T3,T4,T0>& model,
           OptionT& options, CSRMatrixT) {
  const int n = model.size();
  const double* eb = vector_traits<T1>::value(eres.eb);
  const double* ey = vector_traits<T1>::value(eres.ey);
  const double* ez = vector_traits<T1>::value(eres.ez);
  const double* en = csr_matrix_traits<T2>::value(eres.en);
  
  double* alpha = vector_traits<T3>::value(model.alpha);
  double* xi = vector_traits<T3>::value(model.xi);
  double* Q = csr_matrix_traits<T4>::value(model.Q);
  const int* rowptr = csr_matrix_traits<T4>::rowptr(model.Q);
  const int* colind = csr_matrix_traits<T4>::colind(model.Q);
  const int base = csr_matrix_traits<T4>::base(model.Q);
  const int* d = vector_traits<T0,int>::value(model.diag);
  std::vector<double> tmpv(n, 0.0);
  
  for (int i=0; i<n; i++) {
    for (int z=rowptr[i]-base; z<rowptr[i+1]-base; z++) {
      int j = colind[z] - base;
      if (i != j) {
        Q[z] = en[z] / ez[i];
        tmpv[i] += Q[z];
      }
    }
  }
  for (int i=0; i<n; i++) {
    alpha[i] = eb[i] / eres.etotal;
    xi[i] = ey[i] / ez[i];
    tmpv[i] += xi[i];
    Q[d[i]] = -tmpv[i];
  }
}

template <typename T0, typename T1, typename T2, typename T3, typename T4,
          typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPH<T3,T4,T0>& model,
           OptionT& options, CSCMatrixT) {
  const int n = model.size();
  const double* eb = vector_traits<T1>::value(eres.eb);
  const double* ey = vector_traits<T1>::value(eres.ey);
  const double* ez = vector_traits<T1>::value(eres.ez);
  const double* en = csc_matrix_traits<T2>::value(eres.en);

  double* alpha = vector_traits<T3>::value(model.alpha);
  double* xi = vector_traits<T3>::value(model.xi);
  double* Q = csc_matrix_traits<T4>::value(model.Q);
  const int* colptr = csc_matrix_traits<T4>::colptr(model.Q);
  const int* rowind = csc_matrix_traits<T4>::rowind(model.Q);
  const int base = csc_matrix_traits<T4>::base(model.Q);
  const int* d = vector_traits<T0,int>::value(model.diag);
  std::vector<double> tmpv(n, 0.0);
  
  for (int j=0; j<n; j++) {
    for (int z=colptr[j]-base; z<colptr[j+1]-base; z++) {
      int i = rowind[z] - base;
      if (i != j) {
        Q[z] = en[z] / ez[i];
        tmpv[i] += Q[z];
      }
    }
  }
  for (int i=0; i<n; i++) {
    alpha[i] = eb[i] / eres.etotal;
    xi[i] = ey[i] / ez[i];
    tmpv[i] += xi[i];
    Q[d[i]] = -tmpv[i];
  }
}

template <typename T0, typename T1, typename T2, typename T3, typename T4,
          typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPH<T3,T4,T0>& model,
           OptionT& options, COOMatrixT) {
  const int n = model.size();
  const double* eb = vector_traits<T1>::value(eres.eb);
  const double* ey = vector_traits<T1>::value(eres.ey);
  const double* ez = vector_traits<T1>::value(eres.ez);
  const double* en = coo_matrix_traits<T2>::value(eres.en);
  
  double* alpha = vector_traits<T3>::value(model.alpha);
  double* xi = vector_traits<T3>::value(model.xi);
  double* Q = coo_matrix_traits<T4>::value(model.Q);
  const int* rowind = coo_matrix_traits<T4>::rowind(model.Q);
  const int* colind = coo_matrix_traits<T4>::colind(model.Q);
  const int base = coo_matrix_traits<T4>::base(model.Q);
  const int nnz = coo_matrix_traits<T4>::nnz(model.Q);
  const int* d = vector_traits<T0,int>::value(model.diag);
  std::vector<double> tmpv(n, 0.0);
  
  for (int z=0; z<nnz; z++) {
    int i = rowind[z] - base;
    int j = colind[z] - base;
    if (i != j) {
      Q[z] = en[z] / ez[i];
      tmpv[i] += Q[z];
    }
  }
  for (int i=0; i<n; i++) {
    alpha[i] = eb[i] / eres.etotal;
    xi[i] = ey[i] / ez[i];
    tmpv[i] += xi[i];
    Q[d[i]] = -tmpv[i];
  }
}

}

template <typename T0, typename T1, typename T2, typename T3, typename T4,
          typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPH<T3,T4,T0>& model,
           OptionT& options) {
  _mstep_::mstep(eres, model, options, typename matrix_category<T4>::type{});
  // unif
  copy(model.Q, model.P);
  double qv = unif(model.P, model.diag, options.ufactor);
  model.qv = qv;
}

template <typename T1, typename T2, typename GPHT, typename OptionT>
void mstep(const GPHEres<T1,T2>& eres,
           GPHPoi<GPHT>& model,
           OptionT& options) noexcept {
  mstep(eres, model.gph, options);
  model.omega = eres.etotal;
}

#endif

