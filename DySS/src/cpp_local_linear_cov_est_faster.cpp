#include <Rcpp.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::export]]
NumericMatrix local_linear_cov_est_faster(
    NumericMatrix epsij,
    IntegerMatrix ttij,
    IntegerVector nobs,
    IntegerVector alltimepoints,
    const int hh){
  
  const int ntimepoints=alltimepoints.length();
  const double omega=0.001;
  const double hh_double=double(hh)*omega;
  
  int tt,ss,ssidx,ttidx,ttdiff,ssdiff,ttdiff_idx,ssdiff_idx,ii,jj,jjs,jjt;
  double S00,S10,S01,S20,S11,S02,R00,R10,R01,A1,A2,A3,B;
  double ttdiff_double,ssdiff_double; //yy
  
  NumericVector allkvalues(2*hh+1);
  NumericVector allktvalues(2*hh+1);
  NumericVector allkttvalues(2*hh+1);
  NumericVector allttdiff(2*hh+1);
  
  NumericMatrix allkmatrix(2*hh+1,2*hh+1);
  NumericMatrix allksmatrix(2*hh+1,2*hh+1);
  NumericMatrix allktmatrix(2*hh+1,2*hh+1);
  NumericMatrix allkssmatrix(2*hh+1,2*hh+1);
  NumericMatrix allkstmatrix(2*hh+1,2*hh+1);
  NumericMatrix allkttmatrix(2*hh+1,2*hh+1);
  
  for(tt=-hh;tt<hh;++tt){
    ttdiff_double=double(tt)*omega;
    allttdiff(tt+hh)=ttdiff_double;
    allkvalues(tt+hh)=0.75*(1.0-(ttdiff_double/hh_double)*(ttdiff_double/hh_double));//hh_double
    allktvalues[tt+hh]=allkvalues[tt+hh]*ttdiff_double;
    allkttvalues[tt+hh]=allktvalues[tt+hh]*ttdiff_double;
  }
  
  for(ssdiff=-hh;ssdiff<hh;++ssdiff){
    ssdiff_double=double(ssdiff)*omega;
    ssdiff_idx=hh+ssdiff;
    for(ttdiff=-hh;ttdiff<hh;++ttdiff){
      ttdiff_double=double(ttdiff)*omega;
      ttdiff_idx=hh+ttdiff;
      allkmatrix(ssdiff_idx,ttdiff_idx)=allkvalues(ssdiff_idx)*allkvalues(ttdiff_idx);
      allksmatrix(ssdiff_idx,ttdiff_idx)=allkmatrix(ssdiff_idx,ttdiff_idx)*ssdiff_double;
      allktmatrix(ssdiff_idx,ttdiff_idx)=allkmatrix(ssdiff_idx,ttdiff_idx)*ttdiff_double;
      allkssmatrix(ssdiff_idx,ttdiff_idx)=allkmatrix(ssdiff_idx,ttdiff_idx)*ssdiff_double*ssdiff_double;
      allkstmatrix(ssdiff_idx,ttdiff_idx)=allkmatrix(ssdiff_idx,ttdiff_idx)*ssdiff_double*ttdiff_double;
      allkttmatrix(ssdiff_idx,ttdiff_idx)=allkmatrix(ssdiff_idx,ttdiff_idx)*ttdiff_double*ttdiff_double;
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
  
  double epsterm;
  //double numerator,denominator;
  NumericMatrix COV(ntimepoints,ntimepoints);
  
  for(ssidx=0;ssidx<ntimepoints;++ssidx){
    if(ssidx%100==0)Rcpp::Rcout<<ssidx<<" ";
    ss=alltimepoints(ssidx);
    for(ttidx=0;ttidx<ssidx;++ttidx){
      tt=alltimepoints(ttidx);
      S00=0.0;S10=0.0;S01=0.0;S20=0.0;S11=0.0;S02=0.0;R00=0.0;R10=0.0;R01=0.0;
      //numerator=0.0;denominator=0.0;
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
            
            epsterm=epsij(ii,jjs)*epsij(ii,jjt);
            
            S00=S00+allkmatrix(ssdiff_idx,ttdiff_idx);
            S10=S10+allksmatrix(ssdiff_idx,ttdiff_idx);
            S01=S01+allktmatrix(ssdiff_idx,ttdiff_idx);
            S20=S20+allkssmatrix(ssdiff_idx,ttdiff_idx);
            S11=S11+allkstmatrix(ssdiff_idx,ttdiff_idx);
            S02=S02+allkttmatrix(ssdiff_idx,ttdiff_idx);
            
            R00=R00+epsterm*allkmatrix(ssdiff_idx,ttdiff_idx);
            R10=R10+epsterm*allksmatrix(ssdiff_idx,ttdiff_idx);
            R01=R01+epsterm*allktmatrix(ssdiff_idx,ttdiff_idx);
          }
        }
      }
      A1=S20*S02-S11*S11;
      A2=S10*S02-S01*S11;
      A3=S01*S20-S10*S11;
      B=A1*S00-A2*S10-A3*S01;
      
      COV(ssidx,ttidx)=(A1*R00-A2*R10-A3*R01)/B;
      COV(ttidx,ssidx)=COV(ssidx,ttidx);
    }
  }
  return COV;
}
