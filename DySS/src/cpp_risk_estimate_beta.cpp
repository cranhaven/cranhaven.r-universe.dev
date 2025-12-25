#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
arma::vec risk_estimate_beta(arma::cube yyijk,
                             arma::imat ttij,
                             arma::ivec nobs,
                             arma::ivec st,
                             arma::ivec ot,
                             LogicalVector delta,
                             arma::mat yfij,
                             int hh,
                             int niter=200){
  
  int tt,ii,jj,ii2,jj2,ttdiff,kk,mm;
  double hh_double,tt_double,kvalue,eps;
  
  const int nind=yyijk.n_rows;
  const int nmaxobs=yyijk.n_cols;
  const int ndim=yyijk.n_slices;
  
  arma::field<arma::vec> F_yyijk(nind,nmaxobs);
  for(ii=0;ii<nind;ii++){
    for(jj=0;jj<nobs(ii);jj++){
      F_yyijk(ii,jj)=arma::vectorise(yyijk.tube(ii,jj));
    }
  }
  
  arma::vec allkvalues(2*hh+1);
  hh_double=double(hh);
  for(ii=0;ii<2*hh+1;ii++){
    tt_double=double(ii-hh);
    allkvalues(ii)=0.75*(1.0-(tt_double/hh_double)*(tt_double/hh_double));
  }
  
  arma::mat F_expbetayyijk(nind,nmaxobs);
  arma::mat F_expbetayyijk_yyijk1(nind,nmaxobs);
  arma::mat F_expbetayyijk_yyijk2(nind,nmaxobs);
  
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
          temp_vec=F_yyijk(ii,jj);
          F_expbetayyijk(ii,jj)=std::exp(arma::dot(temp_vec,beta_est));
          F_expbetayyijk_yyijk1(ii,jj)=yyijk(ii,jj,kk)*F_expbetayyijk(ii,jj);
          F_expbetayyijk_yyijk2(ii,jj)=yyijk(ii,jj,kk)*yyijk(ii,jj,kk)*F_expbetayyijk(ii,jj);
        }
      }
      linear_term=0.0;
      quad_term=0.0;
      for(ii=0;ii<nind;ii++){
        if(!delta(ii))continue;
        tt=ot(ii);
        denominator=0.0;
        numerator1=0.0;
        numerator2=0.0;
        
        for(ii2=0;ii2<nind;ii2++){
          // if(ttij(ii2,nobs(ii2)-1)<tt)continue;
          if(ot(ii2)<tt)continue;
          if(st(ii2)>tt)continue;
          for(jj2=0;jj2<nobs(ii2);jj2++){
            ttdiff=ttij(ii2,jj2)-tt;
            if(std::abs(ttdiff)>=hh)continue;
            kvalue=allkvalues(ttdiff+hh);
            denominator=denominator+F_expbetayyijk(ii2,jj2)*kvalue;
            numerator1=numerator1+F_expbetayyijk_yyijk1(ii2,jj2)*kvalue;
            numerator2=numerator2+F_expbetayyijk_yyijk2(ii2,jj2)*kvalue;
          }
        }
        fraction1=numerator1/denominator;
        fraction2=numerator2/denominator;
        linear_term=linear_term+yfij(ii,kk)-fraction1;
        quad_term=quad_term+fraction2-fraction1*fraction1;
      }
      beta_est(kk)=beta_est(kk)+linear_term/quad_term;
    }
    eps=arma::sum(arma::abs(beta_est-beta_old));
    if(eps<1e-8)break;
  }
  
  return(beta_est);
}

