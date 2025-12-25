#include <Rcpp.h>
using namespace Rcpp;
using std::abs;
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericVector local_linear_mean_est_faster(
    NumericMatrix yyij,
    IntegerMatrix ttij,
    IntegerVector nobs,
    IntegerVector alltimepoints,
    const int hh){
  
  const int nind=ttij.nrow();
  const int ntimepoints=alltimepoints.length();
  
  const double omega=0.001;
  const double hh_double=double(hh)*omega;
  
  NumericVector mu(ntimepoints);
  
  int tt,ttdiff,ii,jj,ttidx,idx;
  double ttdiff_double;
  double U0,U1,V0,V1,V2;
  
  NumericVector allkvalues(2*hh+1);
  NumericVector allktvalues(2*hh+1);
  NumericVector allkttvalues(2*hh+1);
  NumericVector allttdiff(2*hh+1);
  
  for(tt=-hh;tt<hh;tt++){
    ttdiff_double=double(tt)*omega;
    allttdiff[tt+hh]=ttdiff_double;
    allkvalues[tt+hh]=0.75*(1.0-(ttdiff_double/hh_double)*(ttdiff_double/hh_double)); //hht_double
    allktvalues[tt+hh]=allkvalues[tt+hh]*ttdiff_double;
    allkttvalues[tt+hh]=allktvalues[tt+hh]*ttdiff_double;
  }
  
  for(ttidx=0;ttidx<ntimepoints;++ttidx){
    tt=alltimepoints(ttidx);
    U0=0.0;U1=0.0;V0=0.0;V1=0.0;V2=0.0;
    for(ii=0;ii<nind;++ii){
      for(jj=0;jj<nobs(ii);++jj){
        ttdiff=ttij(ii,jj)-tt;
        if(ttdiff<=-hh)continue;
        if(ttdiff>=hh)break;
        idx=hh+ttdiff;
        V0=V0+allkvalues[idx];
        V1=V1+allktvalues[idx];
        V2=V2+allkttvalues[idx];
        U0=U0+allkvalues[idx]*yyij(ii,jj);
        U1=U1+allktvalues[idx]*yyij(ii,jj);
      }
    }
    mu(ttidx)=(V2*U0-V1*U1)/(V2*V0-V1*V1);
  }
  return mu;
}
