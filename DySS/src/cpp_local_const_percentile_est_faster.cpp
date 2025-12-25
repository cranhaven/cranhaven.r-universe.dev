#include <Rcpp.h>
using namespace Rcpp;
using std::abs;
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix local_const_percentile_est_faster(
    NumericMatrix yyij_eva,
    IntegerMatrix ttij_eva,
    IntegerVector nobs_eva,
    NumericVector yy_ref,
    IntegerVector tt_ref,
    IntegerVector starting_idx,
    IntegerVector ending_idx,
    NumericVector upper_line,
    const int ntimepoints,
    const int hh_t,
    const double hh_y){
  
  const int nrow_eva=ttij_eva.nrow();
  
  NumericMatrix zzij_eva(nrow_eva,ttij_eva.ncol());
  std::fill(zzij_eva.begin(),zzij_eva.end(),NumericVector::get_na());
  
  const double omega=0.001;
  // const double hh_t_double=double(hh_t)*omega;
  
  int tt,ttdiff,ii,jj,idx,iiref;
  double ttdiff_double,yydiff,yy,kery;
  double U0,V0;
  
  NumericVector allkvalues(2*hh_t+1);
  NumericVector allttdiff(2*hh_t+1);
  
  for(tt=-hh_t;tt<hh_t;tt++){
    ttdiff_double=double(tt)*omega;
    allttdiff[tt+hh_t]=ttdiff_double;
    allkvalues[tt+hh_t]=1.0-double(tt*tt)/double(hh_t*hh_t); //hh_t_double
  }
  
  for(ii=0;ii<nrow_eva;++ii){
    if(ii%50==0)Rcpp::Rcout<<ii<<" ";
    for(jj=0;jj<nobs_eva(ii);++jj){
      tt=ttij_eva(ii,jj);
      yy=yyij_eva(ii,jj);
      U0=0.0,V0=0.0;
      if(yy<upper_line(tt-1)){
        for(iiref=starting_idx(tt-1)-1;iiref<=ending_idx(tt-1)-1;iiref++){
          ttdiff=tt_ref(iiref)-tt;
          //if(ttdiff<=-hh_t)continue;
          //if(ttdiff>=hh_t)break;
          yydiff=yy-yy_ref(iiref);
          kery=R::pnorm5(yydiff,0.0,hh_y,1,0);
          idx=hh_t+ttdiff;
          U0=U0+allkvalues[idx];
          V0=V0+allkvalues[idx]*kery;
        }
        //zzij_eva(ii,jj)=V0/U0;
        zzij_eva(ii,jj)=R::qnorm5(V0/U0,0.0,1.0,1,0);
      }else{
        for(iiref=starting_idx(tt-1)-1;iiref<=ending_idx(tt-1)-1;iiref++){
          ttdiff=tt_ref(iiref)-tt;
          //if(ttdiff<=-hh_t)continue;
          //if(ttdiff>=hh_t)break;
          yydiff=yy_ref(iiref)-yy;
          kery=R::pnorm5(yydiff,0.0,hh_y,1,0);
          idx=hh_t+ttdiff;
          U0=U0+allkvalues[idx];
          V0=V0+allkvalues[idx]*kery;
        }
        zzij_eva(ii,jj)=-R::qnorm5(V0/U0,0.0,1.0,1,0);
      }
    }
  }
  return zzij_eva;
}
