#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
List risk_estimate_beta_recurrent(arma::cube xxij,
                                  arma::imat ttij,
                                  arma::ivec nobs,
                                  arma::ivec ot,
                                  arma::cube xfij,
                                  arma::imat ft,
                                  arma::ivec nevent,
                                  int hh,
                                  int niter=200){
  
  int tt,ii,jj,ii2,jj2,ttdiff,kk,mm;
  double hh_double,tt_double,kvalue,eps;
  
  const int nind=xxij.n_rows;
  const int nmaxobs=xxij.n_cols;
  const int nmaxevent=xfij.n_cols;
  const int ndim=xxij.n_slices;
  
  arma::field<arma::vec> F_xxij(nind,nmaxobs);
  arma::field<arma::vec> F_xfij(nind,nmaxevent);
  for(ii=0;ii<nind;ii++){
    for(jj=0;jj<nobs(ii);jj++){
      F_xxij(ii,jj)=arma::vectorise(xxij.tube(ii,jj));
    }
    for(jj=0;jj<nevent(ii);jj++){
      F_xfij(ii,jj)=arma::vectorise(xfij.tube(ii,jj));
    }
  }
  
  arma::vec allkvalues(2*hh+1);
  hh_double=double(hh)*0.001;
  for(ii=0;ii<2*hh+1;ii++){
    tt_double=double(ii-hh)*0.001;
    allkvalues(ii)=0.75*(1.0-(tt_double/hh_double)*(tt_double/hh_double))/hh_double;
  }
  
  arma::mat F_expbetaxxij(nind,nmaxobs);
  arma::mat F_expbetaxxij_xxij1(nind,nmaxobs);
  arma::mat F_expbetaxxij_xxij2(nind,nmaxobs);
  
  double linear_term;
  double quad_term;
  
  arma::vec beta_est(ndim,arma::fill::zeros);
  arma::vec beta_old(ndim,arma::fill::zeros);
  
  double denominator;
  double numerator1,numerator2;
  double fraction1,fraction2;
  arma::vec temp_vec(ndim);
  
  for(mm=0;mm<niter;mm++){
    beta_old=beta_est;
    for(kk=0;kk<ndim;kk++){
      for(ii=0;ii<nind;ii++){
        for(jj=0;jj<nobs(ii);jj++){
          temp_vec=F_xxij(ii,jj);
          F_expbetaxxij(ii,jj)=std::exp(arma::dot(temp_vec,beta_est));
          F_expbetaxxij_xxij1(ii,jj)=xxij(ii,jj,kk)*F_expbetaxxij(ii,jj);
          F_expbetaxxij_xxij2(ii,jj)=xxij(ii,jj,kk)*xxij(ii,jj,kk)*F_expbetaxxij(ii,jj);
        }
      }
      linear_term=0.0;
      quad_term=0.0;
      for(ii=0;ii<nind;ii++){
        for(jj=0;jj<nevent(ii);jj++){
          // if(!delta(ii,jj))continue;
          tt=ft(ii,jj);
          denominator=0.0;
          numerator1=0.0;
          numerator2=0.0;
          
          for(ii2=0;ii2<nind;ii2++){
            if(ttij(ii2,nobs(ii2)-1)<tt)continue;
            for(jj2=0;jj2<nobs(ii2);jj2++){
              ttdiff=ttij(ii2,jj2)-tt;
              if(std::abs(ttdiff)>=hh)continue;
              kvalue=allkvalues(ttdiff+hh);
              denominator=denominator+F_expbetaxxij(ii2,jj2)*kvalue;
              numerator1=numerator1+F_expbetaxxij_xxij1(ii2,jj2)*kvalue;
              numerator2=numerator2+F_expbetaxxij_xxij2(ii2,jj2)*kvalue;
            }
          }
          fraction1=numerator1/denominator;
          fraction2=numerator2/denominator;
          linear_term=linear_term+xfij(ii,jj,kk)-fraction1;
          quad_term=quad_term+fraction2-fraction1*fraction1;
        }
      }
      beta_est(kk)=beta_est(kk)+linear_term/quad_term;
    }
    eps=arma::sum(arma::abs(beta_est-beta_old));
    if(eps<1e-8)break;
  }
  
  List result(2);
  result(0)=beta_est;
  result(1)=mm;
  return(result);
}

