#ifndef MAPFIT_GMMPP
#define MAPFIT_GMMPP

#include <Rcpp.h>
#include "traits.h"
#include "gamma.h"
#include "blas.h"
#include "unif.h"
#include "gauss_inte.h"
#include "map_models.h"
#include "map_data.h"
#include "map_gen.h"

#define TDAT(k) (tdat[(k)-1])
#define GDAT(k) (gdat[(k)-1])
#define IDAT(k) (idat[(k)-1])

double xifunc0(int n, double t, double u, double mi, double mj, double ri, double rj) {
  return exp(n * log(ri*u + rj*(t-u)) - gam::lgamma(n+1.0) - mi*u - mj*(t-u));
}

template <typename Tv1>
double gam_inte(int n, double t, double mi, double mj, double ri, double rj,
                const Tv1& x, const Tv1& w, Tv1& fx, Tv1& fv) {
  const int divide = stride_vector_traits<Tv1>::size(x);
  double s = gauss_inte::fx(x, 0.0, t, fx);
  for (int i=0; i<divide; i++) {
    fv[i] = xifunc0(n, t, fx[i], mi, mj, ri, rj);
  }
  return gauss_inte::fv(w, s, fv);
}

template <typename Tv1>
double psi_inte(int n, double t, double mi, double mj, double ri, double rj,
                const Tv1& x, const Tv1& w, Tv1& fx, Tv1& fv) {
  const int divide = stride_vector_traits<Tv1>::size(x);
  double s = gauss_inte::fx(x, 0.0, t, fx);
  for (int i=0; i<divide; i++) {
    fv[i] = fx[i] * xifunc0(n, t, fx[i], mi, mj, ri, rj);
  }
  return gauss_inte::fv(w, s, fv);
}

template <typename MatT, typename Tv>
void makeG(int num, double t, const MatT& D0, const MatT& D1, MatT& G,
           const Tv& x, const Tv& w, Tv& fx, Tv& fv) {
  const int m = dense_matrix_traits<MatT>::nrow(D0);
  const int n = dense_matrix_traits<MatT>::ncol(D0);
  const double* d0 = dense_matrix_traits<MatT>::value(D0);
  const double* d1 = dense_matrix_traits<MatT>::value(D1);
  const int ld0 = dense_matrix_traits<MatT>::ld(D0);
  const int ld1 = dense_matrix_traits<MatT>::ld(D1);
  double* g = dense_matrix_traits<MatT>::value(G);
  const int ldg = dense_matrix_traits<MatT>::ld(G);
  
  double dij, dji;
  for (int j=0; j<n; j++) {
    for (int i=0; i<m; i++) {
      if (i == j) {
        dij = 1.0;
        dji = 1.0;
      } else {
        dij = d0[i+j*ld0];
        dji = d0[j+i*ld0];
      }
      g[i+j*ldg] = dij * gam_inte(num, t, -d0[i+i*ld0], -d0[j+j*ld0], d1[i+i*ld1], d1[j+j*ld1], x, w, fx, fv);
    }
  }
}
  
template <typename MatT, typename Tv>
void makeGPsi(int num, double t, const MatT& D0, const MatT& D1,
              MatT& G, MatT& PsiT1, MatT& PsiT2, MatT& PsiN1, MatT& PsiN2,
              const Tv& x, const Tv& w, Tv& fx, Tv& fv) {
  const int m = dense_matrix_traits<MatT>::nrow(D0);
  const int n = dense_matrix_traits<MatT>::ncol(D0);
  const double* d0 = dense_matrix_traits<MatT>::value(D0);
  const double* d1 = dense_matrix_traits<MatT>::value(D1);
  const int ld0 = dense_matrix_traits<MatT>::ld(D0);
  const int ld1 = dense_matrix_traits<MatT>::ld(D1);

  double* g = dense_matrix_traits<MatT>::value(G);
  const int ldg = dense_matrix_traits<MatT>::ld(G);
  double* psit1 = dense_matrix_traits<MatT>::value(PsiT1);
  const int lpt1 = dense_matrix_traits<MatT>::ld(PsiT1);
  double* psit2 = dense_matrix_traits<MatT>::value(PsiT2);
  const int lpt2 = dense_matrix_traits<MatT>::ld(PsiT2);
  double* psin1 = dense_matrix_traits<MatT>::value(PsiN1);
  const int lpn1 = dense_matrix_traits<MatT>::ld(PsiN1);
  double* psin2 = dense_matrix_traits<MatT>::value(PsiN2);
  const int lpn2 = dense_matrix_traits<MatT>::ld(PsiN2);
  
  double dij, dji;
  for (int j=0; j<n; j++) {
    for (int i=0; i<m; i++) {
      if (i == j) {
        dij = 1.0;
        dji = 1.0;
      } else {
        dij = d0[i+j*ld0];
        dji = d0[j+i*ld0];
      }
      g[i+j*ldg] = dij * gam_inte(num, t, -d0[i+i*ld0], -d0[j+j*ld0], d1[i+i*ld1], d1[j+j*ld1], x, w, fx, fv);
      double tmp = psi_inte(num, t, -d0[i+i*ld0], -d0[j+j*ld0], d1[i+i*ld1], d1[j+j*ld1], x, w, fx, fv);
      psit1[i+j*lpt1] = dij * tmp;
      psit2[j+i*lpt2] = dji * tmp;
      if (num != 0) {
        double tmp = psi_inte(num-1, t, -d0[i+i*ld0], -d0[j+j*ld0], d1[i+i*ld1], d1[j+j*ld1], x, w, fx, fv);
        psin1[i+j*lpn1] = d1[i+i*ld1] * dij * tmp;
        psin2[j+i*lpn2] = d1[i+i*ld1] * dji * tmp;
      }
    }
  }
}

template <typename Tv1, typename Tv2, typename Tv3>
void mulplus(const Tv1& x, const Tv2& y, Tv3& z) {
  const int n = stride_vector_traits<Tv1>::size(x);
  const double* xval = stride_vector_traits<Tv1>::value(x);
  const double* yval = stride_vector_traits<Tv2>::value(y);
  double* zval = vector_traits<Tv3>::value(z);

  for (int i=0; i<n; i++) {
    zval[i] += xval[i] * yval[i];
  }
}

template <typename Tv1, typename Tv2, typename Tv3>
void mmulplus(const Tv1& x, const Tv2& y, Tv3& z) {
  const int m = dense_matrix_traits<Tv1>::nrow(x);
  const int n = dense_matrix_traits<Tv1>::ncol(x);
  const double* xval = dense_matrix_traits<Tv1>::value(x);
  const int ldx = dense_matrix_traits<Tv1>::ld(x);
  const double* yval = dense_matrix_traits<Tv2>::value(y);
  const int ldy = dense_matrix_traits<Tv2>::ld(y);
  double* zval = dense_matrix_traits<Tv3>::value(z);
  const int ldz = dense_matrix_traits<Tv3>::ld(z);

  for (int j=0; j<n; j++) {
    for (int i=0; i<m; i++) {
      zval[i+ldz*j] += xval[i+ldx*j] * yval[i+ldy*j];
    }
  }
}

template <typename Tv1, typename Tv2, typename Tv3>
void xmulplus(const Tv1& x, const Tv2& y, Tv3& z) {
  const int n = stride_vector_traits<Tv1>::size(x);
  const double* xval = stride_vector_traits<Tv1>::value(x);
  const double* yval = stride_vector_traits<Tv2>::value(y);
  double* zval = dense_matrix_traits<Tv3>::value(z);
  const int ldz = dense_matrix_traits<Tv3>::ld(z);
  
  for (int i=0; i<n; i++) {
    zval[i+ldz*i] += xval[i] * yval[i];
  }
}

template <typename Mv, typename Mm, typename Mi,
          typename Gt, typename Gc, typename Gi,
          typename Ev, typename Em,
          typename OptionT, typename WorkSpace>
double estep(
    const GMMPP<MAP<Mv,Mm,Mi>>& model,
    const MAPGroupSample<Gt,Gc,Gi>& data,
    MAPEres<Ev,Em>& eres,
    OptionT& options,
    WorkSpace& work) {
  
  const int m = data.size();
  const double* tdat = stride_vector_traits<Gt,double>::value(data.time);
  const int* gdat = stride_vector_traits<Gc,int>::value(data.counts);
  const int* idat = stride_vector_traits<Gi,int>::value(data.indicators);
  const double tmax = data.maxtime;
  const int nmax = data.maxcount;
  
  int n = model.size();

  // alloc
  using vec1 = std::vector<double>;
  using vec2 = std::vector<std::vector<double>>;
  vec1 tmpv(n);
  vec1 tmpv2(n);
  vec1 tmpb(n);
  vec2 xi(nmax+1, vec1(n));
  vec2 vf(m+2, vec1(n));
  vec2 vb(m+2, vec1(n));

  vec1 inte_x(options.inte_divide);
  vec1 inte_w(options.inte_divide);
  vec1 inte_fx(options.inte_divide);
  vec1 inte_fv(options.inte_divide);
  gauss_inte::w(inte_x, inte_w, options.inte_eps);

  Em& G(work.G);
  Em& Psi1T(work.Psi1T);
  Em& Psi2T(work.Psi2T);
  Em& Psi1N(work.Psi1N);
  Em& Psi2N(work.Psi2N);
  Em& tmpm(work.tmpm);
  
  double scale;
  double llf = 0.0;
  
  fill(eres.eb, 0.0);
  fill(eres.ez, 0.0);
  fill(eres.en0, 0.0);
  fill(eres.en1, 0.0);
  
  copy(model.map.xi, vb[m+1]);
  for (int k=m; k>=1; k--) {
    if (IDAT(k) == 1) {
      gemv(NOTRANS{}, 1.0, model.map.D1, vb[k+1], 0.0, tmpv);
    } else {
      copy(vb[k+1], tmpv);
    }
    makeG(GDAT(k), TDAT(k), model.map.D0, model.map.D1, G, inte_x, inte_w, inte_fx, inte_fv);
    gemv(NOTRANS{}, 1.0, G, tmpv, 0.0, vb[k]);
    scale = asum(vb[k]);
    scal(1.0/scale, vb[k]);
    llf += log(scale);
  }
  copy(vb[1], eres.eb);

  copy(model.map.alpha, vf[0]);
  for (int k=1; k<=m; k++) {
    makeGPsi(GDAT(k), TDAT(k), model.map.D0, model.map.D1,
             G, Psi1T, Psi2T, Psi1N, Psi2N,
             inte_x, inte_w, inte_fx, inte_fv);
    gemv(TRANS{}, 1.0, G, vf[k-1], 0.0, tmpv);
    if (IDAT(k) == 1) {
      gemv(TRANS{}, 1.0, model.map.D1, tmpv, 0.0, vf[k]);
    } else {
      copy(tmpv, vf[k]);
    }

    // for ez, en0, en1
    if (IDAT(k) == 1) {
      gemv(NOTRANS{}, 1.0, model.map.D1, vb[k+1], 0.0, tmpb);
    } else {
      copy(vb[k+1], tmpb);
    }
    scale = dot(tmpv, tmpb);

    // en0
    fill(tmpm, 0.0);
    ger(NOTRANS{}, 1.0/scale, vf[k-1], tmpb, tmpm);
    mmulplus(G, tmpm, eres.en0);

    // ez
    gemv(TRANS{}, 1.0/scale, Psi2T, vf[k-1], 0.0, tmpv2);
    mulplus(tmpv2, tmpb, eres.ez);
    gemv(NOTRANS{}, 1.0/scale, Psi1T, tmpb, 0.0, tmpv2);
    mulplus(tmpv2, vf[k-1], eres.ez);

    // en1
    if (GDAT(k) != 0) {
      gemv(TRANS{}, 1.0/scale, Psi2N, vf[k-1], 0.0, tmpv2);
      xmulplus(tmpv2, tmpb, eres.en1);
      gemv(NOTRANS{}, 1.0/scale, Psi1N, tmpb, 0.0, tmpv2);
      xmulplus(tmpv2, vf[k-1], eres.en1);
    }

    scale = asum(vf[k]);
    scal(1.0/scale, vf[k]);
  }

  const double* alpha = stride_vector_traits<Mv>::value(model.map.alpha);
  double* eb = stride_vector_traits<Ev>::value(eres.eb);
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
  }
  scale = asum(eres.eb);
  scal(1.0/scale, eres.eb);
  llf += log(scale);
  return llf;
}

template <typename Ev, typename Em,
          typename MAPT,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           GMMPP<MAPT>& model,
           OptionT& options) {
  mstep(eres, model.map, options);
}

#endif

