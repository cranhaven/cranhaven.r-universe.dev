#ifndef MAPFIT_MAP_GEN
#define MAPFIT_MAP_GEN

#include <Rcpp.h>
#include "traits.h"
#include "poisson.h"
#include "gamma.h"
#include "blas.h"
#include "gth.h"
#include "unif.h"
#include "map_data.h"
#include "map_models.h"

#define TDAT(k) (tdat[(k)-1])
#define GDAT(k) (gdat[(k)-1])
#define IDAT(k) (idat[(k)-1])
#define MAX(a,b) ((a) > (b) ? (a) : (b))

template <typename Mv, typename Mm, typename Mi,
          typename Gt, typename Gc, typename Gi,
          typename Ev, typename Em,
          typename OptionT, typename WorkSpace>
double estep(
    const MAP<Mv,Mm,Mi>& model,
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
  double qv = model.qv;

  // alloc
  using vec1 = std::vector<double>;
  using vec2 = std::vector<std::vector<double>>;
  using vec3 = std::vector<std::vector<std::vector<double>>>;
  int right = poi::rightbound(qv*tmax, options.poisson_eps) + 1;
  vec1 prob(MAX(right+1,nmax+1));
  vec1 tmpv(n);
  vec1 tmpvf(n);
  vec1 tmpvb(n);
  vec2 xi(nmax+1, vec1(n));
  vec2 vf(m+2, vec1(n));
  vec2 vb(m+2, vec1(n));
  vec3 vc(right+1, vec2(nmax+1, vec1(n)));
  
  Em& H0(work.H0);
  Em& H1(work.H1);
  
  double scale;
  double llf = 0.0;

  fill(eres.eb, 0.0);
  fill(eres.ez, 0.0);
  fill(eres.en0, 0.0);
  fill(eres.en1, 0.0);
  
  copy(model.xi, vb[m+1]);
  for (int k=m; k>=1; k--) {
    int gk = GDAT(k);
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, MAX(right, gk+1), prob);

    if (IDAT(k) == 1) {
      gemv(NOTRANS{}, 1.0, model.D1, vb[k+1], 0.0, tmpvb);
    } else {
      copy(vb[k+1], tmpvb);
    }

    { // vb[k] = exp(D(g_k)*t_k) * vb[k+1]
      fill(vb[k], 0.0);
      for (int j=0; j<gk; j++) {
        fill(xi[j], 0.0);
      }
      copy(tmpvb, xi[gk]);
      axpy(prob[0], xi[0], vb[k]);
      for (int l=1; l<=right; l++) {
        for (int j=0; j<=gk; j++) {
          gemv(NOTRANS{}, 1.0, model.P0, xi[j], 0.0, tmpv);
          if (j != gk) {
            gemv(NOTRANS{}, 1.0, model.P1, xi[j+1], 1.0, tmpv);
          }
          copy(tmpv, xi[j]);
        }
        axpy(prob[l], xi[0], vb[k]);
      }
      scal(1.0/weight, vb[k]);
    }

    scale = asum(vb[k]);
    scal(1.0/scale, vb[k]);
    llf += log(scale);
  }
  copy(vb[1], eres.eb);

  copy(model.alpha, vf[0]);
  for (int k=1; k<=m; k++) {
    int gk = GDAT(k);
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, MAX(right, gk+1), prob);
    
    // mapblas::mexp_unifvec_nforward(P0, P1, qv, r.end, poi, weight,
    //                                gdat(k), vf[k-1], tmpv, work);
    { // tmpvf = vf[k-1] * exp(D(g_k)*t_k)
      fill(tmpvf, 0.0);
      copy(vf[k-1], xi[0]);
      for (int j=1; j<=gk; j++) {
        fill(xi[j], 0.0);
      }
      axpy(prob[0], xi[gk], tmpvf);
      for (int l=1; l<=right; l++) {
        for (int j=gk; j>=0; j--) {
          gemv(TRANS{}, 1.0, model.P0, xi[j], 0.0, tmpv);
          if (j != 0) {
            gemv(TRANS{}, 1.0, model.P1, xi[j-1], 1.0, tmpv);
          }
          copy(tmpv, xi[j]);
        }
        axpy(prob[l], xi[gk], tmpvf);
      }
      scal(1.0/weight, tmpvf);
    }
    
    if (IDAT(k) == 1) {
      gemv(TRANS{}, 1.0, model.D1, tmpvf, 0.0, vf[k]);
    } else {
      copy(tmpvf, vf[k]);
    }

    scale = asum(vf[k]);
    scal(1.0/scale, vf[k]);
  }

  for (int k=1; k<=m; k++) {
    int gk = GDAT(k);
    int right = poi::rightbound(qv*TDAT(k), options.poisson_eps) + 1;
    double weight = poi::pmf(qv*TDAT(k), 0, MAX(right, gk+1), prob);
    
    if (IDAT(k) == 1) {
      gemv(NOTRANS{}, 1.0, model.D1, vb[k+1], 0.0, tmpvb);
    } else {
      copy(vb[k+1], tmpvb);
    }
    
    { // unifvec_nforward(P0, P1, qv, right, poi, weight, gdat(k), vf[k-1], tmpv, tmpz, h0, h1)
      for (int l=0; l<=right; l++) {
        for (int j=0; j<=gk; j++) {
          fill(vc[l][j], 0.0);
        }
      }
      
      axpy(prob[right], tmpvb, vc[right][gk]);
      for (int j=0; j<=gk-1; j++) {
        fill(vc[right][j], 0.0);
      }
      
      for (int l=right-1; l>=1; l--) {
        for (int j=0; j<=gk; j++) {
          gemv(NOTRANS{}, 1.0, model.P0, vc[l+1][j], 0.0, vc[l][j]);
          if (j != gk) {
            gemv(NOTRANS{}, 1.0, model.P1, vc[l+1][j+1], 1.0, vc[l][j]);
          }
        }
        axpy(prob[l], tmpvb, vc[l][gk]);
      }
      
      // compute H
      fill(H0, 0.0);
      fill(H1, 0.0);
      copy(vf[k-1], xi[0]);
      for (int j=1; j<=gk; j++) {
        fill(xi[j], 0.0);
      }
      fill(tmpvf, 0.0);
      axpy(prob[0], xi[gk], tmpvf);
      for (int j=0; j<=gk; j++) {
        ger(NOTRANS{}, 1.0/(qv*weight), xi[j], vc[1][j], H0);
        if (j != gk) {
          ger(NOTRANS{}, 1.0/(qv*weight), xi[j], vc[1][j+1], H1);
        }
      }
      for (int l=1; l<=right-1; l++) {
        for (int j=gk; j>=0; j--) {
          copy(xi[j], tmpv);
          gemv(TRANS{}, 1.0, model.P0, tmpv, 0.0, xi[j]);
          if (j != 0) {
            gemv(TRANS{}, 1.0, model.P1, xi[j-1], 1.0, xi[j]);
          }
        }
        axpy(prob[l], xi[gk], tmpvf);
        for (int j=0; j<=gk; j++) {
          ger(NOTRANS{}, 1.0/(qv*weight), xi[j], vc[l+1][j], H0);
          if (j != gk) {
            ger(NOTRANS{}, 1.0/(qv*weight), xi[j], vc[l+1][j+1], H1);
          }
        }
      }
      scal(1.0/weight, tmpvf);
    }
    
    scale = dot(tmpvf, tmpvb);
    axpy(1.0/scale, H0, eres.en0);
    axpy(1.0/scale, H1, eres.en1);
    if (IDAT(k) == 1) {
      ger(NOTRANS{}, 1.0/scale, tmpvf, vb[k+1], eres.en1);
    }
  }

  const double* alpha = stride_vector_traits<Mv>::value(model.alpha);
  const double* D0 = vector_traits<Mm>::value(model.D0);
  const double* D1 = vector_traits<Mm>::value(model.D1);
  const int* diag = stride_vector_traits<Mi,int>::value(model.diag);
  double* eb = stride_vector_traits<Ev>::value(eres.eb);
  double* ez = stride_vector_traits<Ev>::value(eres.ez);
  double* en0 = vector_traits<Em>::value(eres.en0);
  double* en1 = vector_traits<Em>::value(eres.en1);

  // eres.etotal = nn / uu;
  for (int i=0; i<n; i++) {
    eb[i] *= alpha[i];
    ez[i] = en0[diag[i]];
  }
  const int nz0 = vector_traits<Mm>::size(model.D0);
  const int nz1 = vector_traits<Mm>::size(model.D0);
  for (int i=0; i<nz0; i++) {
    en0[i] *= D0[i];
  }
  for (int i=0; i<nz1; i++) {
    en1[i] *= D1[i];
  }
  scale = asum(eres.eb);
  scal(1.0/scale, eres.eb);
  llf += log(scale);
  // for (int i=0; i<vector_traits<T8>::size(eres.en); i++) {
  //   en[i] *= Q[i];
  // }
  return llf;
}

namespace _mstep_ {

template <typename Ev, typename Em,
          typename Mv, typename Mm, typename Mi,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           MAP<Mv,Mm,Mi>& model,
           OptionT& options, DenseMatrixT) {
  const int n = model.size();
  const double* eb = stride_vector_traits<Ev>::value(eres.eb);
  const double* ez = stride_vector_traits<Ev>::value(eres.ez);
  const double* en0 = dense_matrix_traits<Em>::value(eres.en0);
  const double* en1 = dense_matrix_traits<Em>::value(eres.en1);
  const int ld0 = dense_matrix_traits<Em>::ld(eres.en0);
  const int ld1 = dense_matrix_traits<Em>::ld(eres.en1);
  double* alpha = stride_vector_traits<Mv>::value(model.alpha);
  double* D0 = dense_matrix_traits<Mm>::value(model.D0);
  double* D1 = dense_matrix_traits<Mm>::value(model.D1);
  const int ldd0 = dense_matrix_traits<Mm>::ld(model.D0);
  const int ldd1 = dense_matrix_traits<Mm>::ld(model.D1);
  const int* diag = stride_vector_traits<Mi,int>::value(model.diag);

  std::vector<double> tmpv(n, 0.0);
  
  for (int j=0; j<n; j++) {
    for (int i=0; i<n; i++) {
      if (i != j) {
        D0[j*ldd0+i] = en0[j*ld0+i] / ez[i];
        tmpv[i] += D0[j*ldd0+i];
      }
      D1[j*ldd1+i] = en1[j*ld1+i] / ez[i];
      tmpv[i] += D1[j*ldd1+i];
    }
  }
  for (int i=0; i<n; i++) {
    D0[diag[i]] = -tmpv[i];
  }
}

template <typename Ev, typename Em,
          typename Mv, typename Mm, typename Mi,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           MAP<Mv,Mm,Mi>& model,
           OptionT& options, CSRMatrixT) {
  const int n = model.size();
  const double* eb = stride_vector_traits<Ev>::value(eres.eb);
  const double* ez = stride_vector_traits<Ev>::value(eres.ez);
  const double* en0 = csr_matrix_traits<Em>::value(eres.en0);
  const int* rowptr0 = csr_matrix_traits<Em>::rowptr(eres.en0);
  const int* colind0 = csr_matrix_traits<Em>::colind(eres.en0);
  const int base0 = csr_matrix_traits<Em>::base(eres.en0);
  const double* en1 = csr_matrix_traits<Em>::value(eres.en1);
  const int* rowptr1 = csr_matrix_traits<Em>::rowptr(eres.en1);
  const int* colind1 = csr_matrix_traits<Em>::colind(eres.en1);
  const int base1 = csr_matrix_traits<Em>::base(eres.en1);
  
  double* alpha = stride_vector_traits<Mv>::value(model.alpha);
  double* D0 = csr_matrix_traits<Mm>::value(model.D0);
  double* D1 = csr_matrix_traits<Mm>::value(model.D1);
  const int* diag = stride_vector_traits<Mi,int>::value(model.diag);
  
  std::vector<double> tmpv(n, 0.0);
  
  for (int i=0; i<n; i++) {
    for (int z=rowptr0[i]-base0; z<rowptr0[i+1]-base0; z++) {
      int j = colind0[z] - base0;
      if (i != j) {
        D0[z] = en0[z] / ez[i];
        tmpv[i] += D0[z];
      }
    }
    for (int z=rowptr1[i]-base1; z<rowptr1[i+1]-base1; z++) {
      int j = colind1[z] - base1;
      D1[z] = en1[z] / ez[i];
      tmpv[i] += D1[z];
    }
  }
  for (int i=0; i<n; i++) {
    D0[diag[i]] = -tmpv[i];
  }
}

template <typename Ev, typename Em,
          typename Mv, typename Mm, typename Mi,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           MAP<Mv,Mm,Mi>& model,
           OptionT& options, CSCMatrixT) {
  const int n = model.size();
  const double* eb = stride_vector_traits<Ev>::value(eres.eb);
  const double* ez = stride_vector_traits<Ev>::value(eres.ez);
  const double* en0 = csc_matrix_traits<Em>::value(eres.en0);
  const int* colptr0 = csc_matrix_traits<Em>::colptr(eres.en0);
  const int* rowind0 = csc_matrix_traits<Em>::rowind(eres.en0);
  const int base0 = csc_matrix_traits<Em>::base(eres.en0);
  const double* en1 = csc_matrix_traits<Em>::value(eres.en1);
  const int* colptr1 = csc_matrix_traits<Em>::colptr(eres.en1);
  const int* rowind1 = csc_matrix_traits<Em>::rowind(eres.en1);
  const int base1 = csc_matrix_traits<Em>::base(eres.en1);
  
  double* alpha = stride_vector_traits<Mv>::value(model.alpha);
  double* D0 = csc_matrix_traits<Mm>::value(model.D0);
  double* D1 = csc_matrix_traits<Mm>::value(model.D1);
  const int* diag = stride_vector_traits<Mi,int>::value(model.diag);
  
  std::vector<double> tmpv(n, 0.0);
  
  for (int j=0; j<n; j++) {
    for (int z=colptr0[j]-base0; z<colptr0[j+1]-base0; z++) {
      int i = rowind0[z] - base0;
      if (i != j) {
        D0[z] = en0[z] / ez[i];
        tmpv[i] += D0[z];
      }
    }
    for (int z=colptr1[j]-base1; z<colptr1[j+1]-base1; z++) {
      int i = rowind1[z] - base1;
      D1[z] = en1[z] / ez[i];
      tmpv[i] += D1[z];
    }
  }
  for (int i=0; i<n; i++) {
    D0[diag[i]] = -tmpv[i];
  }
}

template <typename Ev, typename Em,
          typename Mv, typename Mm, typename Mi,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           MAP<Mv,Mm,Mi>& model,
           OptionT& options, COOMatrixT) {
  const int n = model.size();
  const double* eb = stride_vector_traits<Ev>::value(eres.eb);
  const double* ez = stride_vector_traits<Ev>::value(eres.ez);
  const double* en0 = coo_matrix_traits<Em>::value(eres.en0);
  const int* rowind0 = coo_matrix_traits<Em>::rowind(eres.en0);
  const int* colind0 = coo_matrix_traits<Em>::colind(eres.en0);
  const int base0 = coo_matrix_traits<Em>::base(eres.en0);
  const int nnz0 = coo_matrix_traits<Em>::nnz(eres.en0);
  const double* en1 = coo_matrix_traits<Em>::value(eres.en1);
  const int* rowind1 = coo_matrix_traits<Em>::rowind(eres.en1);
  const int* colind1 = coo_matrix_traits<Em>::colind(eres.en1);
  const int base1 = coo_matrix_traits<Em>::base(eres.en1);
  const int nnz1 = coo_matrix_traits<Em>::nnz(eres.en1);
  
  double* alpha = stride_vector_traits<Mv>::value(model.alpha);
  double* D0 = coo_matrix_traits<Mm>::value(model.D0);
  double* D1 = coo_matrix_traits<Mm>::value(model.D1);
  const int* diag = stride_vector_traits<Mi,int>::value(model.diag);
  
  std::vector<double> tmpv(n, 0.0);

  for (int z=0; z<nnz0; z++) {
    int i = rowind0[z] - base0;
    int j = colind0[z] - base0;
    if (i != j) {
      D0[z] = en0[z] / ez[i];
      tmpv[i] += D0[z];
    }
  }
  for (int z=0; z<nnz1; z++) {
    int i = rowind1[z] - base1;
    int j = colind1[z] - base1;
    D1[z] = en1[z] / ez[i];
    tmpv[i] += D1[z];
  }
  for (int i=0; i<n; i++) {
    D0[diag[i]] = -tmpv[i];
  }
}

}

template <typename Ev, typename Em,
          typename Mv, typename Mm, typename Mi,
          typename OptionT>
void mstep(const MAPEres<Ev,Em>& eres,
           MAP<Mv,Mm,Mi>& model,
           OptionT& options) {
  _mstep_::mstep(eres, model, options, typename matrix_category<Mm>::type{});
  // unif
  copy(model.D0, model.P0);
  copy(model.D1, model.P1);
  double qv = unif(model.P0, model.diag, options.ufactor);
  scal(1.0/qv, model.P1);
  model.qv = qv;
  // stationary
  if (options.stationary) {
    map_gth(model.D0, model.D1, model.alpha);
  }
}

#endif

