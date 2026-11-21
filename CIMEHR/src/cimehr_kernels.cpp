#include <Rcpp.h>
#include <vector>
#include <cmath>
#if defined(__clang__)
#pragma clang fp contract(off)
#elif defined(__GNUC__)
#pragma GCC optimize("fp-contract=off")
#endif

using namespace Rcpp;

// Output: G rows, indexed by group.
// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_group_sum(NumericMatrix M, IntegerVector g, int G) {
  const R_xlen_t n = M.nrow();
  const int p = M.ncol();
  if (g.size() != n) stop("length of 'g' must equal the number of rows of 'M'");
  if (G < 0) stop("'G' must be non-negative");
  for (R_xlen_t i = 0; i < n; ++i)
    if (g[i] == NA_INTEGER || g[i] < 1 || g[i] > G) stop("group index out of range");
  NumericMatrix out(G, p);
  const int *gi = g.begin();
  for (int j = 0; j < p; ++j) {
    const double *x = M.begin() + j * n;
    double *o = out.begin() + (R_xlen_t)j * G;
    for (R_xlen_t i = 0; i < n; ++i) o[gi[i] - 1] += x[i];
  }
  return out;
}

static void cimehr_rev_cumsum(const double *v, int m, double *r) {
  long double s = 0.0;
  for (int k = m - 1; k >= 0; --k) {
    s += v[k];
    r[k] = (double)s;
  }
  if (!ISNAN((double)s)) return;
  bool nan = false, na = false;
  for (int k = m - 1; k >= 0; --k) {
    if (!nan) nan = ISNAN(v[k]);
    if (nan && !na) na = R_IsNA(v[k]);
    if (na || nan) r[k] = na ? NA_REAL : R_NaN;
  }
}

// S[k, ] = sum over rows i with fri[i] <= k <= lri[i] of wM[i, ].
// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_window_risk_sum(NumericMatrix wM, IntegerVector lri, IntegerVector fri, int K) {
  const R_xlen_t n = wM.nrow();
  const int p = wM.ncol();
  if (lri.size() != n || fri.size() != n)
    stop("lengths of 'lri' and 'fri' must equal the number of rows of 'wM'");
  if (K < 0) stop("'K' must be non-negative");
  for (R_xlen_t i = 0; i < n; ++i) {
    if (lri[i] == NA_INTEGER || lri[i] < 1 || lri[i] > K) stop("risk index out of range");
    if (fri[i] == NA_INTEGER || fri[i] < 1 || fri[i] > K + 1) stop("risk index out of range");
  }
  NumericMatrix S(K, p);
  std::vector<double> out(K), late(K + 1), ro(K), rl(K + 1);
  const int *lr = lri.begin(), *fr = fri.begin();
  for (int j = 0; j < p; ++j) {
    std::fill(out.begin(), out.end(), 0.0);
    std::fill(late.begin(), late.end(), 0.0);
    const double *x = wM.begin() + j * n;
    for (R_xlen_t i = 0; i < n; ++i) out[lr[i] - 1] += x[i];
    for (R_xlen_t i = 0; i < n; ++i) late[fr[i] - 1] += x[i];
    cimehr_rev_cumsum(out.data(), K, ro.data());
    cimehr_rev_cumsum(late.data(), K + 1, rl.data());
    double *o = S.begin() + (R_xlen_t)j * K;
    for (int k = 0; k < K; ++k) o[k] = ro[k] - rl[k + 1];
  }
  return S;
}

namespace {
struct cimehr_dd {
  double hi, lo;
  cimehr_dd(double x=0.0, double y=0.0) : hi(x), lo(y) {}
};
cimehr_dd dd_add(cimehr_dd a, cimehr_dd b) {
  double s=a.hi+b.hi, v=s-a.hi;
  double e=(a.hi-(s-v))+(b.hi-v)+a.lo+b.lo;
  double h=s+e;
  return cimehr_dd(h,e-(h-s));
}
cimehr_dd dd_neg(cimehr_dd a) { return cimehr_dd(-a.hi,-a.lo); }
cimehr_dd dd_sub(cimehr_dd a,cimehr_dd b) { return dd_add(a,dd_neg(b)); }
cimehr_dd dd_mul(cimehr_dd a,cimehr_dd b) {
  double p=a.hi*b.hi;
  double e=std::fma(a.hi,b.hi,-p)+a.hi*b.lo+a.lo*b.hi+a.lo*b.lo;
  double h=p+e;
  return cimehr_dd(h,e-(h-p));
}
cimehr_dd dd_div(cimehr_dd a,cimehr_dd b) {
  double q=a.hi/b.hi;
  cimehr_dd r=dd_sub(a,dd_mul(b,cimehr_dd(q)));
  return dd_add(cimehr_dd(q),cimehr_dd((r.hi+r.lo)/b.hi));
}
double dd_value(cimehr_dd a) { return a.hi+a.lo; }
}

// [[Rcpp::export(rng = false)]]
NumericVector cimehr_stable_matvec_cpp(NumericMatrix M, NumericVector x) {
  if(M.ncol()!=x.size())stop("Stable matrix-vector dimensions do not agree.");
  NumericVector out(M.nrow());
  for(int i=0;i<M.nrow();++i) {
    cimehr_dd sum;
    for(int j=0;j<M.ncol();++j)sum=dd_add(sum,dd_mul(cimehr_dd(M(i,j)),cimehr_dd(x[j])));
    out[i]=dd_value(sum);
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_stable_right_product_cpp(NumericMatrix M, NumericMatrix B) {
  if(M.ncol()!=B.nrow())stop("Stable matrix-product dimensions do not agree.");
  NumericMatrix out(M.nrow(),B.ncol());
  for(int j=0;j<B.ncol();++j)for(int i=0;i<M.nrow();++i) {
    cimehr_dd sum;
    for(int k=0;k<M.ncol();++k)sum=dd_add(sum,dd_mul(cimehr_dd(M(i,k)),cimehr_dd(B(k,j))));
    out(i,j)=dd_value(sum);
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
List cimehr_stage3_center_cpp(NumericMatrix D0, NumericMatrix D1,
    NumericVector risk_weight, NumericVector case_weight, IntegerVector last,
    IntegerVector first, IntegerVector subject, IntegerVector grid, NumericVector time, int K) {
  int n=D0.nrow(),p=D0.ncol(),m=subject.size();
  if(D1.nrow()!=n||D1.ncol()!=p||risk_weight.size()!=n||case_weight.size()!=n||
     last.size()!=n||first.size()!=n||grid.size()!=m||time.size()!=m||K<1)
    stop("Invalid stable Stage-3 centering dimensions.");
  int anchor=0;while(anchor<n&&!(last[anchor]>0&&last[anchor]>=first[anchor]&&risk_weight[anchor]>0))++anchor;
  if(anchor==n)stop("Stable Stage-3 centering has no active risk mass.");
  NumericVector a0(p),a1(p),den(K),den_low(K);
  NumericMatrix c0(K,p),c1(K,p),shift0(K,p),shift1(K,p),shift0_low(K,p),shift1_low(K,p),D(m,p),H(m,p),center(m,p);
  for(int j=0;j<p;++j){a0[j]=D0(anchor,j);a1[j]=D1(anchor,j);}
  std::vector<cimehr_dd> changes(K+1),mass(K),running0(K),running1(K);
  for(int i=0;i<n;++i)if(last[i]>0&&last[i]>=first[i]) {
    if(first[i]<1||last[i]>K)stop("Stable Stage-3 risk index out of range.");
    cimehr_dd w=dd_mul(cimehr_dd(risk_weight[i]),cimehr_dd(case_weight[i]));
    changes[first[i]-1]=dd_add(changes[first[i]-1],w);
    changes[last[i]]=dd_sub(changes[last[i]],w);
  }
  cimehr_dd total;
  for(int k=0;k<K;++k){total=dd_add(total,changes[k]);mass[k]=total;den[k]=total.hi;den_low[k]=total.lo;}
  for(int j=0;j<p;++j) {
    std::fill(running0.begin(),running0.end(),cimehr_dd());
    std::fill(running1.begin(),running1.end(),cimehr_dd());
    std::vector<cimehr_dd> add0(K+1),add1(K+1);
    for(int i=0;i<n;++i)if(last[i]>0&&last[i]>=first[i]) {
      cimehr_dd w=dd_mul(cimehr_dd(risk_weight[i]),cimehr_dd(case_weight[i]));
      cimehr_dd v0=dd_mul(w,dd_sub(cimehr_dd(D0(i,j)),cimehr_dd(a0[j])));
      cimehr_dd v1=dd_mul(w,dd_sub(cimehr_dd(D1(i,j)),cimehr_dd(a1[j])));
      add0[first[i]-1]=dd_add(add0[first[i]-1],v0);add0[last[i]]=dd_sub(add0[last[i]],v0);
      add1[first[i]-1]=dd_add(add1[first[i]-1],v1);add1[last[i]]=dd_sub(add1[last[i]],v1);
    }
    cimehr_dd s0,s1;
    for(int k=0;k<K;++k) {
      s0=dd_add(s0,add0[k]);s1=dd_add(s1,add1[k]);
      running0[k]=den[k]>0?dd_div(s0,mass[k]):cimehr_dd();
      running1[k]=den[k]>0?dd_div(s1,mass[k]):cimehr_dd();
      shift0(k,j)=running0[k].hi;shift1(k,j)=running1[k].hi;
      shift0_low(k,j)=running0[k].lo;shift1_low(k,j)=running1[k].lo;
      c0(k,j)=den[k]>0?dd_value(dd_add(cimehr_dd(a0[j]),running0[k])):0;
      c1(k,j)=den[k]>0?dd_value(dd_add(cimehr_dd(a1[j]),running1[k])):0;
    }
    for(int e=0;e<m;++e) {
      int i=subject[e]-1,k=grid[e]-1;
      if(i<0||i>=n||k<0||k>=K||den[k]<=0)stop("Stable Stage-3 event index or risk mass is invalid.");
      cimehr_dd d=dd_add(cimehr_dd(D0(i,j)),dd_mul(cimehr_dd(time[e]),cimehr_dd(D1(i,j))));
      cimehr_dd h0=dd_sub(dd_sub(cimehr_dd(D0(i,j)),cimehr_dd(a0[j])),running0[k]);
      cimehr_dd h1=dd_sub(dd_sub(cimehr_dd(D1(i,j)),cimehr_dd(a1[j])),running1[k]);
      cimehr_dd h=dd_add(h0,dd_mul(cimehr_dd(time[e]),h1));
      D(e,j)=dd_value(d);H(e,j)=dd_value(h);center(e,j)=dd_value(dd_sub(d,h));
    }
  }
  return List::create(_["D"]=D,_["H"]=H,_["center"]=center,_["center0"]=c0,
    _["center1"]=c1,_["shift0"]=shift0,_["shift1"]=shift1,
    _["shift0_low"]=shift0_low,_["shift1_low"]=shift1_low,
    _["anchor0"]=a0,_["anchor1"]=a1,_["den"]=den,_["den_low"]=den_low);
}

// [[Rcpp::export(rng = false)]]
List cimehr_stage3_accumulate_cpp(NumericMatrix D0, NumericMatrix D1, NumericMatrix H,
    NumericVector residual, NumericVector cw, NumericVector risk_weight,
    IntegerVector subject, IntegerVector grid, NumericVector time, IntegerVector last,
    IntegerVector first, NumericVector den, NumericMatrix shift0, NumericMatrix shift1,
    NumericVector anchor0, NumericVector anchor1, NumericVector den_low,
    NumericMatrix shift0_low, NumericMatrix shift1_low) {
  int n=D0.nrow(),p=D0.ncol(),m=H.nrow(),K=den.size();
  if(D1.nrow()!=n||D1.ncol()!=p||H.ncol()!=p||residual.size()!=m||cw.size()!=m||
    subject.size()!=m||grid.size()!=m||time.size()!=m||risk_weight.size()!=n||
    first.size()!=n||last.size()!=n||den_low.size()!=K||shift0.nrow()!=K||shift0.ncol()!=p||
    shift1.nrow()!=K||shift1.ncol()!=p||shift0_low.nrow()!=K||shift0_low.ncol()!=p||
    shift1_low.nrow()!=K||shift1_low.ncol()!=p||anchor0.size()!=p||anchor1.size()!=p)
    stop("Stage-3 accumulation dimensions do not match.");
  std::vector<cimehr_dd> sum0(K),sum1(K),step0(K),step1(K),cum0(K+1),cum1(K+1);
  for(int e=0;e<m;++e) {
    int k=grid[e]-1;if(k<0||k>=K)stop("Stage-3 event grid is out of range.");
    cimehr_dd r=dd_mul(cimehr_dd(cw[e]),cimehr_dd(residual[e]));
    sum0[k]=dd_add(sum0[k],r);sum1[k]=dd_add(sum1[k],dd_mul(r,cimehr_dd(time[e])));
  }
  NumericVector dA0(K),dA1(K);
  for(int k=0;k<K;++k) {
    cimehr_dd a=den[k]>0?dd_div(sum0[k],cimehr_dd(den[k],den_low[k])):cimehr_dd();
    cimehr_dd b=den[k]>0?dd_div(sum1[k],cimehr_dd(den[k],den_low[k])):cimehr_dd();
    step0[k]=a;step1[k]=b;
    dA0[k]=dd_value(a);dA1[k]=dd_value(b);cum0[k+1]=dd_add(cum0[k],a);cum1[k+1]=dd_add(cum1[k],b);
  }
  NumericMatrix event_score(n,p),integral(n,p),score(n,p);
  for(int j=0;j<p;++j) {
    std::vector<cimehr_dd> grouped(n),cent(K+1);
    for(int e=0;e<m;++e) {
      int i=subject[e]-1;if(i<0||i>=n)stop("Stage-3 subject index is out of range.");
      cimehr_dd r=dd_mul(cimehr_dd(cw[e]),cimehr_dd(residual[e]));
      grouped[i]=dd_add(grouped[i],dd_mul(cimehr_dd(H(e,j)),r));
    }
    for(int k=0;k<K;++k)cent[k+1]=dd_add(cent[k],dd_add(
      dd_mul(cimehr_dd(shift0(k,j),shift0_low(k,j)),step0[k]),
      dd_mul(cimehr_dd(shift1(k,j),shift1_low(k,j)),step1[k])));
    for(int i=0;i<n;++i) {
      int end=last[i],start=first[i]-1;cimehr_dd ri;
      if(start<0||start>K||end<0||end>K)stop("Stage-3 risk interval is out of range.");
      if(end>=start&&end>0) {
        ri=dd_sub(dd_add(dd_mul(dd_sub(cimehr_dd(D0(i,j)),cimehr_dd(anchor0[j])),dd_sub(cum0[end],cum0[start])),
          dd_mul(dd_sub(cimehr_dd(D1(i,j)),cimehr_dd(anchor1[j])),dd_sub(cum1[end],cum1[start]))),dd_sub(cent[end],cent[start]));
      }
      event_score(i,j)=dd_value(grouped[i]);integral(i,j)=dd_value(ri);
      score(i,j)=dd_value(dd_sub(grouped[i],dd_mul(cimehr_dd(risk_weight[i]),ri)));
    }
  }
  return List::create(_["event_score"]=event_score,_["risk_integral"]=integral,
    _["score"]=score,_["dA0"]=dA0,_["dA1"]=dA1);
}

// [[Rcpp::export(rng = false)]]
List cimehr_stage3_residual_cpp(NumericMatrix D, NumericMatrix H, NumericVector y,
    NumericVector coefficient, NumericVector weight) {
  int n=D.nrow(),p=D.ncol();NumericVector residual(n),moment(p),scale(p),relative(p);
  if(H.nrow()!=n||H.ncol()!=p||y.size()!=n||coefficient.size()!=p||weight.size()!=n)
    stop("Stage-3 residual dimensions do not match.");
  std::vector<cimehr_dd> sums(p),bounds(p);
  for(int i=0;i<n;++i) {
    cimehr_dd fitted;long double size=std::fabs((long double)y[i]);
    for(int j=0;j<p;++j) {fitted=dd_add(fitted,dd_mul(cimehr_dd(D(i,j)),cimehr_dd(coefficient[j])));
      size+=std::fabs((long double)D(i,j)*(long double)coefficient[j]);}
    cimehr_dd r=dd_sub(cimehr_dd(y[i]),fitted);residual[i]=dd_value(r);
    for(int j=0;j<p;++j) {
      cimehr_dd wh=dd_mul(cimehr_dd(weight[i]),cimehr_dd(H(i,j)));
      sums[j]=dd_add(sums[j],dd_mul(wh,r));
      bounds[j]=dd_add(bounds[j],cimehr_dd((double)(std::fabs((long double)weight[i]*H(i,j))*size)));
    }
  }
  for(int j=0;j<p;++j){moment[j]=dd_value(sums[j]);scale[j]=dd_value(bounds[j]);
    relative[j]=scale[j]>0?std::fabs(moment[j])/scale[j]:std::fabs(moment[j]);}
  return List::create(_["residual"]=residual,_["moment"]=moment,_["scale"]=scale,_["relative"]=relative);
}

// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_stable_matrix_sum_cpp(List values) {
  if(!values.size()) stop("At least one matrix is required.");
  NumericMatrix first=values[0];int nr=first.nrow(),nc=first.ncol();
  std::vector<NumericMatrix> matrices;
  for(int k=0;k<values.size();++k) {
    NumericMatrix value=values[k];
    if(value.nrow()!=nr||value.ncol()!=nc) stop("Matrix-sum dimensions do not match.");
    matrices.push_back(value);
  }
  NumericMatrix out(nr,nc);
  for(int j=0;j<nc;++j)for(int i=0;i<nr;++i) {
    cimehr_dd sum;
    for(int k=0;k<values.size();++k)sum=dd_add(sum,cimehr_dd(matrices[k](i,j)));
    out(i,j)=dd_value(sum);
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_stable_scale_add_cpp(NumericMatrix A, NumericVector x,
    NumericMatrix B, NumericVector y) {
  int n=A.nrow(),p=A.ncol();
  if(B.nrow()!=n||B.ncol()!=p||x.size()!=n||y.size()!=n)
    stop("Scaled matrix-sum dimensions do not match.");
  NumericMatrix out(n,p);
  for(int j=0;j<p;++j)for(int i=0;i<n;++i)
    out(i,j)=dd_value(dd_add(dd_mul(cimehr_dd(A(i,j)),cimehr_dd(x[i])),
      dd_mul(cimehr_dd(B(i,j)),cimehr_dd(y[i]))));
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_stage3_kappa_score_cpp(NumericMatrix G, NumericMatrix H,
    NumericVector residual, NumericVector coefficient, IntegerVector subject,
    NumericVector risk_weight, NumericMatrix G_integral) {
  int ne=G.nrow(),p=G.ncol(),n=G_integral.nrow();
  if(H.nrow()!=ne||H.ncol()!=p||residual.size()!=ne||coefficient.size()!=p||
    subject.size()!=ne||risk_weight.size()!=n||G_integral.ncol()!=p)
    stop("Stage-3 derivative dimensions do not match.");
  std::vector<cimehr_dd> psi(ne);
  for(int e=0;e<ne;++e)for(int j=0;j<p;++j)
    psi[e]=dd_add(psi[e],dd_mul(cimehr_dd(G(e,j)),cimehr_dd(coefficient[j])));
  NumericMatrix out(n,p);
  for(int j=0;j<p;++j) {
    std::vector<cimehr_dd> sums(n);
    for(int e=0;e<ne;++e) {
      int i=subject[e]-1;if(i<0||i>=n)stop("Stage-3 subject index is out of range.");
      sums[i]=dd_add(sums[i],dd_sub(dd_mul(cimehr_dd(G(e,j)),cimehr_dd(residual[e])),
        dd_mul(cimehr_dd(H(e,j)),psi[e])));
    }
    for(int i=0;i<n;++i)out(i,j)=dd_value(dd_sub(sums[i],
      dd_mul(cimehr_dd(risk_weight[i]),cimehr_dd(G_integral(i,j)))));
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
NumericMatrix cimehr_stage3_random_integral_cpp(NumericMatrix W0, NumericMatrix W1,
    NumericVector dA0, NumericVector dA1, IntegerVector last, IntegerVector first) {
  int n=W0.nrow(),p=W0.ncol(),K=dA0.size();
  if(W1.nrow()!=n||W1.ncol()!=p||dA1.size()!=K||last.size()!=n||first.size()!=n)
    stop("Stage-3 random-integral dimensions do not match.");
  std::vector<cimehr_dd> a0(K+1),a1(K+1);
  for(int k=0;k<K;++k){a0[k+1]=dd_add(a0[k],cimehr_dd(dA0[k]));a1[k+1]=dd_add(a1[k],cimehr_dd(dA1[k]));}
  NumericMatrix out(n,p);
  for(int i=0;i<n;++i) {
    int start=first[i]-1,end=last[i];
    if(start<0||end>K||start>K||end<0)stop("Stage-3 risk interval is out of range.");
    if(end>=start)for(int j=0;j<p;++j)out(i,j)=dd_value(dd_add(
      dd_mul(cimehr_dd(W0(i,j)),dd_sub(a0[end],a0[start])),
      dd_mul(cimehr_dd(W1(i,j)),dd_sub(a1[end],a1[start]))));
  }
  return out;
}
