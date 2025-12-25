#include <Rcpp.h>
using namespace Rcpp;
using std::abs;
//' @keywords internal
//' @noRd
//[[Rcpp::export]]
NumericMatrix local_const_cov_est_faster(
    NumericMatrix epsij,
    IntegerMatrix ttij,
    IntegerVector nobs,
    IntegerVector alltimepoints,
    const int hh){
  
  const int ntimepoints=alltimepoints.length();
  const double omega=0.001;
  const double hh_double=double(hh)*omega;
  
  int tt,ss,ssidx,ttidx,ttdiff,ssdiff,ttdiff_idx,ssdiff_idx,ii,jj,jjs,jjt;
  double ttdiff_double; //yy
  
  NumericVector allkvalues(2*hh+1);
  NumericVector allttdiff(2*hh+1);
  NumericMatrix allkmatrix(2*hh+1,2*hh+1);
  
  for(tt=-hh;tt<=hh;++tt){
    ttdiff_double=double(tt)*omega;
    allttdiff(tt+hh)=ttdiff_double;
    allkvalues(tt+hh)=0.75*(1.0-(ttdiff_double/hh_double)*(ttdiff_double/hh_double));//hh_double
  }
  
  for(ssdiff=-hh;ssdiff<hh;++ssdiff){
    ssdiff_idx=hh+ssdiff;
    for(ttdiff=-hh;ttdiff<hh;++ttdiff){
      ttdiff_idx=hh+ttdiff;
      allkmatrix(ssdiff_idx,ttdiff_idx)=allkvalues(ssdiff_idx)*allkvalues(ttdiff_idx);
    }
  }
  
  const int numrow=ttij.nrow();
  
  IntegerMatrix starting_idx(ntimepoints,numrow);
  IntegerMatrix ending_idx(ntimepoints,numrow);
  for(ttidx=0;ttidx<ntimepoints;++ttidx){
    tt=alltimepoints(ttidx);
    for(ii=0;ii<numrow;++ii){
      //-- calculate starting_idx
      if(ttij(ii,nobs(ii)-1)<=tt-hh){
        starting_idx(ttidx,ii)=nobs(ii);
      }else{
        for(jj=0;jj<nobs(ii);++jj){
          if(ttij(ii,jj)>tt-hh){
            starting_idx(ttidx,ii)=jj;
            break;
          }
        }
      }
      //-- calculate ending_idx
      if(ttij(ii,0)>=tt+hh){
        ending_idx(ttidx,ii)=-1;
      }else{
        for(jj=nobs(ii)-1;jj>-1;--jj){
          if(ttij(ii,jj)<tt+hh){
            ending_idx(ttidx,ii)=jj;
            break;
          }
        }
      }
    }
  }
  
  double kterm,epsterm;
  double numerator,denominator;
  NumericMatrix COV(ntimepoints,ntimepoints);
  
  for(ssidx=0;ssidx<ntimepoints;++ssidx){
    if(ssidx%100==0)Rcpp::Rcout<<ssidx<<" ";
    ss=alltimepoints(ssidx);
    for(ttidx=0;ttidx<ssidx;++ttidx){
      tt=alltimepoints(ttidx);
      numerator=0.0;denominator=0.0;
      for(ii=0;ii<numrow;ii++){
        for(jjs=starting_idx(ssidx,ii);jjs<=ending_idx(ssidx,ii);++jjs){
          ssdiff=ttij(ii,jjs)-ss;
          //if(ssdiff>=hh)break;
          //if(ssdiff<=-hh)continue;
          ssdiff_idx=hh+ssdiff;
          for(jjt=starting_idx(ttidx,ii);jjt<=ending_idx(ttidx,ii);++jjt){
            if(jjs==jjt)continue;
            ttdiff=ttij(ii,jjt)-tt;
            //if(ttdiff>=hh)break;
            //if(ttdiff<=-hh)continue;
            ttdiff_idx=hh+ttdiff;
            kterm=allkmatrix(ssdiff_idx,ttdiff_idx);
            epsterm=epsij(ii,jjs)*epsij(ii,jjt);
            denominator=denominator+kterm;
            numerator=numerator+kterm*epsterm;
          }
        }
      }
      COV(ssidx,ttidx)=numerator/denominator;
      COV(ttidx,ssidx)=COV(ssidx,ttidx);
    }
  }
  return COV;
}
