#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
NumericMatrix varcov_mat_est(arma::cube eps_cube,
                             IntegerVector tt_matrix,
                             IntegerVector nobs,
                             IntegerVector alltimepoints,
                             int hh){
  int numrow=eps_cube.n_rows;
  int numdim=eps_cube.n_slices;
  int ntimepoints=alltimepoints.length();
  
  int tt,tt1,tt2,ttidx,ttidx1,ttidx2,ii,jj,jj1,jj2,ll,ll1,ll2;
  int diff,diff1,diff2;
  double kterm1,kterm2,kterm;
  double U,V;
  NumericVector allkvalues(2*hh+1);
  NumericMatrix varcov_est(ntimepoints*numdim,ntimepoints*numdim);

  for(tt=-hh;tt<=hh;++tt){
    allkvalues(tt+hh)=1.0-(double(tt)/double(hh))*(double(tt)/double(hh));//hh_double
  }
  
  for(ttidx1=0;ttidx1<ntimepoints;ttidx1++){
    tt1=alltimepoints(ttidx1);
    for(ttidx2=0;ttidx2<ntimepoints;ttidx2++){
      tt2=alltimepoints(ttidx2);
      for(ll1=0;ll1<numdim;ll1++){
        for(ll2=0;ll2<=ll1;ll2++){
          U=0.0;V=0.0;
          for(ii=0;ii<numrow;ii++){
            for(jj1=0;jj1<nobs(ii);jj1++){
              diff1=tt_matrix(ii,jj1)-tt1;
              if(diff1>=hh||diff1<=-hh)continue;
              kterm1=allkvalues(hh+diff1);
              for(jj2=0;jj2<nobs(ii);jj2++){
                diff2=tt_matrix(ii,jj2)-tt2;
                if(diff2>=hh||diff2<=-hh)continue;
                if(ll1==ll2&&jj1==jj2)continue;
                kterm2=allkvalues(hh+diff2);
                kterm=kterm1*kterm2;
                U=U+kterm;
                V=V+kterm*eps_cube(ii,jj1,ll1)*eps_cube(ii,jj2,ll2);
              }
            }
          }
          varcov_est(ttidx1*numdim+ll1,ttidx2*numdim+ll2)=V/U;
          varcov_est(ttidx2*numdim+ll2,ttidx1*numdim+ll1)=varcov_est(ttidx1*numdim+ll1,ttidx2*numdim+ll2);
        }
      }
    }
  }
  for(ttidx=0;ttidx<ntimepoints;ttidx++){
    tt=alltimepoints(ttidx);
    for(ll=0;ll<numdim;ll++){
      U=0.0;V=0.0;
      for(ii=0;ii<numrow;ii++){
        for(jj=0;jj<nobs(ii);jj++){
          diff=tt_matrix(ii,jj)-tt;
          if(diff>=hh||diff<=-hh)continue;
          kterm=allkvalues(hh+diff);
          U=U+kterm;
          V=V+kterm*eps_cube(ii,jj,ll)*eps_cube(ii,jj,ll);
        }
      }
      varcov_est(ttidx*numdim+ll,ttidx*numdim+ll)=V/U;
    }
  }
  
  return(varcov_est);
}
