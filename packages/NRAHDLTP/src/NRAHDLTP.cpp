// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
//#include <Rcpp.h>
#include "RcppArmadillo.h"
//#include <math.h>//for "pow", "sqrt"

using namespace Rcpp;
using namespace arma;

#include "RcppArmadilloExtensions/sample.h"
//
//Test Proposed by Bai and Saranadasa (1996)
// [[Rcpp::export]]
double ts_bs1996_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1); arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1); arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2-2; double invtau=double(n1*n2)/(n1+n2);
  double stat=arma::as_scalar((mu1-mu2).t()*(mu1-mu2));
  arma::mat z=join_horiz(z1,z2);
  arma::mat S;
  if(p<=n)
  {
    S=(z*z.t())/n;//tr(Sigmahat)
  }
  else
  {
    S=(z.t()*z)/n;
  }
  double trSn=arma::trace(S);//tr(Sigmahat^2)
  double trSn2=arma::accu(S%S);//tr(Sigmahat^2)
  double Bn2=double(n*n)/((n+2)*(n-1))*(trSn2-trSn*trSn/n);//unbiased estimator for B
  double statn=(invtau*stat-trSn)/sqrt(2*double(n+1)/n*Bn2);
  return statn;
}

//Test proposed by Chen and Qin (2010)
// [[Rcpp::export]]
arma::vec tsbf_cq2010_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  arma::mat y1xy1t=y1.t()*y1;
  double sumx1tx1inej=arma::accu(y1xy1t)-arma::trace(y1xy1t);
  arma::mat y2xy2t=y2.t()*y2;
  double sumx2tx2inej=arma::accu(y2xy2t)-arma::trace(y2xy2t);
  arma::mat y1xy2t=y1.t()*y2;
  double sumx1tx2=arma::accu(y1xy2t);
  double Tn=sumx1tx1inej/(n1*(n1-1))+sumx2tx2inej/(n2*(n2-1))-2*sumx1tx2/(n1*n2);

  double tempsum=0.0;
  arma::colvec y1sum=arma::sum(y1,1);//add columns
  //constants for speed up
  int n1_1=n1-1;
  int n2_1=n2-1;
  int n1_2=n1-2;
  int n2_2=n2-2;

  for(int j=0;j<n1;j++)
  {
    arma::colvec y1_j=y1.col(j);
    for(int k=j+1;k<n1;k++)
    {
      arma::colvec y1_k=y1.col(k);
      arma::colvec meanxiexjk = (y1sum-y1_j-y1_k)/n1_2;
      tempsum+=arma::as_scalar(y1_k.t()*(y1_j-meanxiexjk)*y1_j.t()*(y1_k-meanxiexjk));
    }
  }
  double trsigma12hat = (tempsum+tempsum)/(n1*(n1_1));

  tempsum=0.0;
  arma::colvec y2sum=arma::sum(y2,1);//add columns
  for(int j=0;j<n2;j++)
  {
    arma::colvec y2_j=y2.col(j);
    for(int k=j+1;k<n2;k++)
    {
      arma::colvec y2_k=y2.col(k);
      arma::colvec meanxiexjk = (y2sum-y2_j-y2_k)/n2_2;
      tempsum+=arma::as_scalar(y2_k.t()*(y2_j-meanxiexjk)*y2_j.t()*(y2_k-meanxiexjk));
    }
  }
  double trsigma22hat = (tempsum+tempsum)/(n2*(n2_1));

  tempsum=0.0;
  for(int l=0;l<n1;l++)
  {
    arma::colvec y1_l=y1.col(l);
    arma::colvec meanx1exl = (y1sum-y1_l)/n1_1;
    for(int k=0;k<n2;k++)
    {
      arma::colvec y2_k=y2.col(k);
      arma::colvec meanx2exk = (y2sum-y2_k)/n2_1;
      tempsum+=arma::as_scalar(y2_k.t()*(y1_l-meanx1exl)*y1_l.t()*(y2_k-meanx2exk));
    }
  }
  double trsigma1sigma2hat=tempsum/(n1*n2);
  double sigman12hat=2.0/(n1*(n1_1))*trsigma12hat+2.0/(n2*(n2_1))*trsigma22hat+4.0/(n1*n2)*trsigma1sigma2hat;
  double stat=Tn/sqrt(sigman12hat);//Qn
  arma::vec stats(2);
  stats(0)=stat;
  stats(1)=Tn;
  return stats;
}


// Test proposed by Zhang et al. (2020)
// [[Rcpp::export]]
arma::vec ts_zgzc2020_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1); arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1); arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2;
  double stat=double(n1*n2)/n*arma::as_scalar((mu1-mu2).t()*(mu1-mu2));
  double uA,uB,uA0;
  arma::mat z=join_horiz(z1,z2);
  arma::mat S;
  if(p<=n){
    S=(z*z.t())/(n-2);//tr(Sigmahat)
  }
  else{
    S=(z.t()*z)/(n-2);
  }
  double B=arma::accu(S%S);//tr(Sigmahat^2)
  double A=arma::trace(S);
  uA0 = A;
  uA=double((n-1)*(n-2))/(n*(n-3))*(A*A-2*B/(n-1));//unbiased estimator for A^2
  uB=double((n-2)*(n-2))/(n*(n-3))*(B-A*A/(n-2));//unbiased estimator for B
  double beta=uB/uA0;double df=uA/uB;
  double statn=stat/beta;//normalizing
  double statstd = (stat - beta*df)/sqrt(2*beta*beta*df);
  arma::vec stats(5);
  stats(0)=stat;
  stats(1)=statn;
  stats(2)=beta;
  stats(3)=df;
  stats(4)=statstd;
  return stats;
}//end of function
//

// Test proposed by Zhang et al.(2021).
// [[Rcpp::export]]
arma::vec tsbf_zzgz2021_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1); arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1); arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2;
  double stat=double(n1*n2)/n*arma::as_scalar((mu1-mu2).t()*(mu1-mu2));
  double uA,uB,uA0;
  arma::mat S1,S2;
  double B1,B2,B12;
  if(p<n){
    S1=(z1*z1.t())/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2*z2.t())/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1*z1.t()*z2*z2.t())/((n1-1)*(n2-1));
  }else{
    S1=(z1.t()*z1)/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2.t()*z2)/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1.t()*z2*z2.t()*z1)/((n1-1)*(n2-1));
  }
  double A1=arma::trace(S1);double A2=arma::trace(S2);
  uA0=(n2*A1+n1*A2)/n;//ptr(Sigma), same for the naive/br method
  double A12=A1*A2;
  double uA1=double(n1*(n1-1))/((n1+1)*(n1-2))*(A1*A1-2*B1/n1);//unbiased estimator for A1^2
  double uB1=double((n1-1)*(n1-1))/((n1+1)*(n1-2))*(B1-A1*A1/(n1-1));//unbiased estimator for B1

  double uA2=double(n2*(n2-1))/((n2+1)*(n2-2))*(A2*A2-2*B2/n2);//unbiased estimator for A2^2
  double uB2=double((n2-1)*(n2-1))/((n2+1)*(n2-2))*(B2-A2*A2/(n2-1));//unbiased estimator for B2

  uA=(double(n2*n2)/(n*n)*uA1+2*double(n1*n2)/(n*n)*A12+double(n1*n1)/(n*n)*uA2);//tr(Sigma)^2
  uB=(double(n2*n2)/(n*n)*uB1+2*double(n1*n2)/(n*n)*B12+double(n1*n1)/(n*n)*uB2);//tr(Sigma^2)

  double beta=uB/uA0;double df=uA/uB;
  double statn=stat/beta;//normalizing
  double statstd = (stat - beta*df)/sqrt(2*beta*beta*df);
  arma::vec stats(5);
  stats(0)=stat;
  stats(1)=statn;
  stats(2)=beta;
  stats(3)=df;
  stats(4)=statstd;
  return stats;
}//end of function
//

//Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec ts_zz2022_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1);
  arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1);
  arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2;
  double invtau=double(n1*n2)/n;
  double stat=arma::as_scalar((mu1-mu2).t()*(mu1-mu2));
  arma::mat z=join_horiz(z1,z2);
  arma::mat S;
  if(p<=n)
  {
    S=(z*z.t())/(n-2);//tr(Sigmahat)
  }
  else
  {
    S=(z.t()*z)/(n-2);
  }
  double trSn=arma::trace(S);//tr(Sigmahat^2)
  double trSn2=arma::accu(S%S);//tr(Sigmahat^2) ##element-wise multiplication
  double trSn3=arma::trace(S*S*S);//tr(Sigmahat^3)
  double htrSn2 = pow((n-2),2)*(trSn2-trSn*trSn/(n-2))/(n-3)/n;
  double htrSn3 = pow((n-2),4)*(trSn3-3*trSn*trSn2/(n-2)+2*pow(trSn,3)/pow((n-2),2))/(n*n-n-6)/(n*(n-4));

  double statn=invtau*stat-trSn;
  double beta0 = -(n-1)*htrSn2*htrSn2/htrSn3/(n-3);
  double beta1 = (n-3)*htrSn3/htrSn2/(n-2);
  double d = (n-1)*(n-2)*pow(htrSn2,3)/pow(htrSn3,2)/pow((n-3),2);
  double statstd = statn/sqrt(2*(n-1)*htrSn2/(n-2));

  arma::vec stats(5);
  stats(0)=statn;
  stats(1)=beta0;
  stats(2)=beta1;
  stats(3)=d;
  stats(4)=statstd;
  return stats;
}

// Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec tsbf_zz2022_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1);
  arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1);
  arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2;
  arma::mat S1,S2;
  double B1,B2,B12,D12,D21;
  if(p<n)
  {
    S1=(z1*z1.t())/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2*z2.t())/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1*z1.t()*z2*z2.t())/((n1-1)*(n2-1));
    D12 = arma::trace(z1*z1.t()*z1*z1.t()*z2*z2.t())/((n1-1)*(n1-1)*(n2-1));
    D21 = arma::trace(z1*z1.t()*z2*z2.t()*z2*z2.t())/((n1-1)*(n2-1)*(n2-1));
  }
  else
  {
    S1=(z1.t()*z1)/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2.t()*z2)/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1.t()*z2*z2.t()*z1)/((n1-1)*(n2-1));
    D12 = arma::trace(z1.t()*z1*z1.t()*z2*z2.t()*z1)/((n1-1)*(n1-1)*(n2-1));
    D21 = arma::trace(z1.t()*z2*z2.t()*z2*z2.t()*z1)/((n1-1)*(n2-1)*(n2-1));
  }

  double A1=arma::trace(S1); //tr(hSigma1)
  double A2=arma::trace(S2); //tr(hSigma2)
  double stat= arma::as_scalar((mu1-mu2).t()*(mu1-mu2))-(A1/n1+A2/n2);


  //uB1, uB2 are unbiased estimators of B1, B2
  double uB1=double((n1-1)*(n1-1))/((n1+1)*(n1-2))*(B1-A1*A1/(n1-1));
  double uB2=double((n2-1)*(n2-1))/((n2+1)*(n2-2))*(B2-A2*A2/(n2-1));

  double K2 = 2*(uB1/(n1*(n1-1))+2*B12/(n1*n2)+uB2/(n2*(n2-1)));

  //C1 is tr(hSigma1^3); C2 is tr(Sigma2^3)
  double C1 = arma::trace(S1*S1*S1);
  double C2 = arma::trace(S2*S2*S2);


  //coefficients of uC1 and uC2

  double c1 = pow((n1-1),4)/(n1*n1+n1-6)/(n1*n1-2*n1-3);
  double c2 = pow((n2-1),4)/(n2*n2+n2-6)/(n2*n2-2*n2-3);


  //uC1, uC2 are unbiased estimatros of C1, C2
  double uC1 = c1*(C1-3*A1*B1/(n1-1)+2*pow(A1,3)/((n1-1)*(n1-1)));
  double uC2 = c2*(C2-3*A2*B2/(n2-1)+2*pow(A2,3)/((n2-1)*(n2-1)));

  double uD12 = (n1-1)*((n1-1)*D12-B12*A1)/(n1-2)/(n1+1);
  double uD21 = (n2-1)*((n2-1)*D21-B12*A2)/(n2-2)/(n2+1);

  double K3 = 8*((n1-2)*uC1/pow(n1*(n1-1),2)+3*uD12/(n1*n1*n2)+3*uD21/(n1*n2*n2)+(n2-2)*uC2/pow(n2*(n2-1),2));

  double beta0 = -2*K2*K2/K3;
  double beta1 = K3/(4*K2);
  double d = 8*pow(K2,3)/(K3*K3);

  double statstd = stat/sqrt(K2);
  arma::vec stats(5);
  stats(0)=stat;
  stats(1)=beta0;
  stats(2)=beta1;
  stats(3)=d;
  stats(4)=statstd;
  return stats;
}//end of function
//

// Test proposed by Zhu et al.(2023).
// [[Rcpp::export]]
arma::vec tsbf_zwz2023_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;int n2 = y2.n_cols;
  int p = y1.n_rows;
  arma::colvec mu1=arma::mean(y1, 1);
  arma::mat z1=y1-arma::repmat(mu1,1,n1);
  arma::colvec mu2=arma::mean(y2, 1);
  arma::mat z2=y2-arma::repmat(mu2,1,n2);
  int n=n1+n2;
  arma::mat S1,S2;
  double B1,B2,B12;
  if(p<n)
  {
    S1=(z1*z1.t())/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2*z2.t())/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1*z1.t()*z2*z2.t())/((n1-1)*(n2-1));
  }
  else
  {
    S1=(z1.t()*z1)/(n1-1); B1=arma::accu(S1%S1);
    S2=(z2.t()*z2)/(n2-1); B2=arma::accu(S2%S2);
    B12=arma::trace(z1.t()*z2*z2.t()*z1)/((n1-1)*(n2-1));
  }
  double A1=arma::trace(S1);
  double A2=arma::trace(S2);
  double stat= arma::as_scalar((mu1-mu2).t()*(mu1-mu2))/(A1/n1+A2/n2);
  double A12=A1*A2;
  double uA1=double(n1*(n1-1))/((n1+1)*(n1-2))*(A1*A1-2*B1/n1);//unbiased estimator for A1^2
  double uB1=double((n1-1)*(n1-1))/((n1+1)*(n1-2))*(B1-A1*A1/(n1-1));//unbiased estimator for B1

  double uA2=double(n2*(n2-1))/((n2+1)*(n2-2))*(A2*A2-2*B2/n2);//unbiased estimator for A2^2
  double uB2=double((n2-1)*(n2-1))/((n2+1)*(n2-2))*(B2-A2*A2/(n2-1));//unbiased estimator for B2

  double uA,uB,uC;
  uA=uA1/(n1*n1)+2*A12/(n1*n2)+uA2/(n2*n2);
  uB=uB1/(n1*n1)+2*B12/(n1*n2)+uB2/(n2*n2);
  double d1 = uA/uB;
  uC=uB1/(n1*n1*(n1-1))+uB2/(n2*n2*(n2-1));
  double d2 = uA/uC;

  arma::vec stats(3);
  stats(0)=stat;
  stats(1)=d1;
  stats(2)=d2;
  return stats;
}//end of function
////////////////////////////////////
///////////////////////////////////
//Two-sample scale-invariant tests
// Test proposed by Srivastava and Du (2008)
// [[Rcpp::export]]
double ts_sd2008_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;
  int n2 = y2.n_cols;
  int n = n1+n2-2;
  int p = y1.n_rows;
  arma::colvec bary1=arma::mean(y1, 1);
  arma::colvec bary2=arma::mean(y2, 1);

  arma::mat x1=y1.each_col()-bary1;
  arma::mat x2=y2.each_col()-bary2;

  arma::vec diagSvec1=arma::var(x1, 0, 1); //variance of x1
  arma::vec diagSvec2=arma::var(x2, 0, 1); //variance of x2

  arma::mat x=join_horiz(x1,x2);

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1*(n1-1)+diagSvec2*(n2-1))/n); //D^{-1/2}

  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10,-10))).fill(pow(10,-10));
  arma::vec meandiff=(bary1-bary2);
  meandiff.each_col()/=sqrtdiagSvec; //D^{-1/2}(bary1-bary2)
  double Tnp=double(n1*n2)/(n1+n2)*arma::dot(meandiff,meandiff)/p;
  x.each_col()/=sqrtdiagSvec;
  arma::mat R;
  R=(x*x.t())/n;//tr(Sigmahat)

  double trR2=arma::accu(R%R);
  double cpn=1+trR2/pow(sqrt(p),3);

  double TSD = (p*Tnp-n*p/(n-2))/sqrt(2*(trR2-p*p/n)*cpn);
  return TSD;
}

// Test proposed by Srivastava et al.(2013)
// [[Rcpp::export]]
arma::vec tsbf_skk2013_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;
  int n2 = y2.n_cols;
  int n = n1+n2;
  int p = y1.n_rows;
  arma::colvec bary1=arma::mean(y1, 1);
  arma::colvec bary2=arma::mean(y2, 1);

  arma::mat x1=y1.each_col()-bary1;
  arma::mat x2=y2.each_col()-bary2;

  arma::vec diagSvec1=arma::var(x1, 0, 1); //variance of x1
  arma::vec diagSvec2=arma::var(x2, 0, 1); //variance of x2

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1*n2+diagSvec2*n1)/n); //D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10,-10))).fill(pow(10,-10));
  arma::vec meandiff=(bary1-bary2);
  meandiff.each_col()/=sqrtdiagSvec; //D^{-1/2}(bary1-bary2)
  double Tnp = double(n1*n2)/n*arma::dot(meandiff,meandiff)/p;
  arma::mat w1 = x1.each_col()/sqrtdiagSvec;
  arma::mat w2 = x2.each_col()/sqrtdiagSvec;
  arma::mat R1;
  arma::mat R2;
  double B1, B2, B12;
  if(n1<p || n2<p){
    R1=(w1.t()*w1)/(n1-1);//tr(Sigmahat)
    R2=(w2.t()*w2)/(n2-1);
    B1=arma::accu(R1%R1);
    B2=arma::accu(R2%R2);
    B12=arma::trace(w2.t()*w1*w1.t()*w2)/((n1-1)*(n2-1));
  }else{
    R1=(w1*w1.t())/(n1-1);//tr(Sigmahat)
    R2=(w2*w2.t())/(n2-1);
    B1=arma::accu(R1%R1);
    B2=arma::accu(R2%R2);
    B12=arma::trace(w1*w1.t()*w2*w2.t())/((n1-1)*(n2-1));
  }

  arma::vec vecw1 = arma::vectorise(w1);
  arma::vec vecw2 = arma::vectorise(w2);
  double trinvDS1=arma::dot(vecw1,vecw1)/(n1-1);//=arma::trace(invDs*S1);
  double trinvDS2=arma::dot(vecw2,vecw2)/(n2-1);//=arma::trace(invDs*S2);

  double trR2=(n2*n2*B1+n1*n1*B2+2*n1*n2*B12)/(n*n);

  double cpn=1+trR2/pow(sqrt(p),3);

  double sigma2 = ((n2*n2*trinvDS1*trinvDS1)/(n1-1)+(n1*n1*trinvDS2*trinvDS2)/(n2-1))/(n*n);
  sigma2 = 2*(trR2-sigma2);

  double TSKK = (p*Tnp-p)/sqrt(sigma2*cpn);
  arma::vec values(3);
  values(0) = TSKK;
  values(1) = sigma2;
  values(2) = cpn;
  return values;
}


// Test proposed by Zhang et al. (2020)
// [[Rcpp::export]]
arma::vec ts_zzz2020_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;
  int n2 = y2.n_cols;
  int n = n1+n2-2;
  int p = y1.n_rows;
  arma::colvec bary1=arma::mean(y1, 1);
  arma::colvec bary2=arma::mean(y2, 1);

  arma::mat x1=y1.each_col()-bary1;
  arma::mat x2=y2.each_col()-bary2;

  arma::vec diagSvec1=arma::var(x1, 0, 1); //variance of x1
  arma::vec diagSvec2=arma::var(x2, 0, 1); //variance of x2

  arma::mat x=join_horiz(x1,x2);

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1*(n1-1)+diagSvec2*(n2-1))/n); //D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10,-10))).fill(pow(10,-10));
  arma::vec meandiff=(bary1-bary2);
  meandiff.each_col()/=sqrtdiagSvec; //D^{-1/2}(bary1-bary2)
  double Tnp=double(n1*n2)/(n1+n2)*arma::dot(meandiff,meandiff)/p;
  x.each_col()/=sqrtdiagSvec;
  arma::mat R;
  R=(x*x.t())/n;//tr(Sigmahat)

  double trR2=arma::accu(R%R);
  double trR2hat = pow(n,2)*(trR2-pow(trace(R),2)/n)/(n1+n2)/(n-1);
  double dhat = p*p/trR2hat;

  arma::vec values(2);
  values(0) = Tnp;
  values(1) = dhat;
  return values;
}



// Test proposed by Zhang et al. (2023)

// [[Rcpp::export]]
arma::vec tsbf_zzz2023_cpp(const arma::mat & y1, const arma::mat & y2)
{
  int n1 = y1.n_cols;
  int n2 = y2.n_cols;
  int n = n1+n2;
  int p = y1.n_rows;
  arma::colvec bary1=arma::mean(y1, 1);
  arma::colvec bary2=arma::mean(y2, 1);

  arma::mat x1=y1.each_col()-bary1;
  arma::mat x2=y2.each_col()-bary2;

  arma::vec diagSvec1=arma::var(x1, 0, 1); //variance of x1
  arma::vec diagSvec2=arma::var(x2, 0, 1); //variance of x2

  //arma::mat x=join_horiz(x1,x2);

  arma::vec sqrtdiagSvec = arma::sqrt((diagSvec1*n2+diagSvec2*n1)/n); //D^{-1/2}
  sqrtdiagSvec.elem(find(sqrtdiagSvec < pow(10,-10))).fill(pow(10,-10));
  arma::vec meandiff=(bary1-bary2);
  meandiff.each_col()/=sqrtdiagSvec; //D^{-1/2}(bary1-bary2)
  double Tnp=double(n1*n2)/n*arma::dot(meandiff,meandiff)/p;
  //double trR2=arma::trace(S*invDs*S*invDs);
  arma::mat w1 = x1.each_col()/sqrtdiagSvec;
  arma::mat w2 = x2.each_col()/sqrtdiagSvec;
  arma::mat R1;
  arma::mat R2;
  double B1, B2, B12;
  if(n1<p || n2<p){
    R1=(w1.t()*w1)/(n1-1);//tr(Sigmahat)
    R2=(w2.t()*w2)/(n2-1);
    B1=arma::accu(R1%R1);
    B2=arma::accu(R2%R2);
    B12=arma::trace(w2.t()*w1*w1.t()*w2)/((n1-1)*(n2-1));
  }else{
    R1=(w1*w1.t())/(n1-1);//tr(Sigmahat)
    R2=(w2*w2.t())/(n2-1);
    B1=arma::accu(R1%R1);
    B2=arma::accu(R2%R2);
    B12=arma::trace(w1*w1.t()*w2*w2.t())/((n1-1)*(n2-1));
  }


  double trR12 = (n1-1)*(n1-1)*(B1-pow(trace(R1),2)/(n1-1))/(n1-2)/(n1+1);
  double trR22 = (n2-1)*(n2-1)*(B2-pow(trace(R2),2)/(n2-1))/(n2-2)/(n2+1);


  double trR2=(n2*n2*B1+n1*n1*B2+2*n1*n2*B12)/(n*n);

  double cpn=1+trR2/pow(sqrt(p),3);

  double trRn2 = (n2*n2*trR12+n1*n1*trR22+2*n1*n2*B12)/(n*n);

  double dhat = p*p/trRn2;

  arma::vec values(3);
  values(0) = Tnp;
  values(1) = dhat;
  values(2) = cpn;
  return values;
}



//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
///One-way MANOVA
// Test proposed by Schott (2007)
// [[Rcpp::export]]

arma::vec ks_s2007_cpp(List Y, const arma::vec & n , int p){
  int g = Y.length(); //number of classes
  int h = g-1;
  int ss = sum(n);
  int e = ss-g;
  arma::mat ybar(p,g);
  arma::colvec poolybar(p);
  poolybar.zeros();
  arma::mat E(p,p);
  E.zeros();
  for(int i=0; i<g; ++i){
    arma::mat y = Y[i];
    arma::colvec mu = arma::mean(y, 1);
    ybar.col(i) = mu;
    poolybar += n(i)*mu;
    arma::mat z=y-arma::repmat(mu,1,n(i));
    E += z*z.t();
  }
  poolybar = poolybar/ss;
  arma::mat H(p,p);
  H.zeros();
  for(int i=0; i<g; ++i){
    H += n(i)*(ybar.col(i)-poolybar)*(ybar.col(i)-poolybar).t();
  }
  double tnp = (trace(H)/h-trace(E)/e)/sqrt(ss-1);
  double a = (trace(E*E)-pow(trace(E),2)/e)/(e+2)/(e-1);
  double sigmahat2 = 2*a/e/h;
  double sigmahat = sqrt(sigmahat2);

  arma::vec stats(2);
  stats(0)=tnp;
  stats(1)=sigmahat;
  return stats;
}


/////////////////////////////////////////////////
/////////////////////////////////////////////////
///General linear hypothesis testing (GLHT) problem

//Test proposed by Fujikoshi et al. (2004)
// [[Rcpp::export]]
double glht_fhw2004_cpp(const arma::mat & Y, const arma::mat & X, const arma::mat & C){
  int n = Y.n_rows;
  int p = Y.n_cols;
  int k = X.n_cols;
  int q = rank(C);
  arma::mat XtXinv = inv_sympd(X.t()*X);
  arma::mat H = X*XtXinv*C.t()*inv_sympd(C*XtXinv*C.t())*C*XtXinv*X.t();
  arma::mat Sh = Y.t()*H*Y;
  arma::mat P = X*XtXinv*X.t();
  arma::mat I(n,n);
  I.eye();
  arma::mat Se = Y.t()*(I-P)*Y;
  double trSe2 = trace(Se*Se);
  double sigmaD = sqrt(2*q*(trSe2/pow(n-k,2)-pow(trace(Se),2)/pow(n-k,3))/p)/(trace(Se)/(n-k)/p);
  double TFHW = sqrt(p)*((n-k)*trace(Sh)/trace(Se)-q)/sigmaD;
  return TFHW;
}



// Test proposed by Srivastava and Fujikoshi (2006)
// [[Rcpp::export]]
double glht_sf2006_cpp(const arma::mat & Y, const arma::mat & X, const arma::mat & C){
  int n = Y.n_rows;
  int p = Y.n_cols;
  int k = X.n_cols;
  int q = rank(C);
  arma::mat XtXinv = inv_sympd(X.t()*X);
  arma::mat H = X*XtXinv*C.t()*inv_sympd(C*XtXinv*C.t())*C*XtXinv*X.t();
  arma::mat Sh = Y.t()*H*Y;
  arma::mat P = X*XtXinv*X.t();
  arma::mat I(n,n);
  I.eye();
  arma::mat Se = Y.t()*(I-P)*Y;
  double trSe2 = trace(Se*Se);
  double a2 = (trSe2-pow(trace(Se),2)/(n-k))/(n-k-1)/(n-k+2)/p;
  double TSF = (trace(Sh)/sqrt(p)-q*trace(Se)/sqrt(n-k)/sqrt((n-k)*p))/sqrt(2*q*a2*(1+q/(n-k)));
  return TSF;
}


// Test proposed by Yamada and Srivastava (2012)
// [[Rcpp::export]]
arma::vec glht_ys2012_cpp(const arma::mat & Y, const arma::mat & X, const arma::mat & C){
  int n = Y.n_rows;
  int p = Y.n_cols;
  int k = X.n_cols;
  int q = rank(C);
  arma::mat XtXinv = inv_sympd(X.t()*X);
  arma::mat H = X*XtXinv*C.t()*inv_sympd(C*XtXinv*C.t())*C*XtXinv*X.t();
  arma::mat Sh = Y.t()*H*Y;
  arma::mat P = X*XtXinv*X.t();
  arma::mat I(n,n);
  I.eye();
  arma::mat Se = Y.t()*(I-P)*Y;
  arma::mat Sigma = Se/(n-k);
  arma::mat invD = diagmat(1/Sigma.diag());
  double Tnp = trace(Sh*invD)/p/q;
  double trRhat2 = trace(invD*Sigma*invD*Sigma);
  double cpn = 1+trRhat2/sqrt(pow(p,3));
  double TYS = (p*q*Tnp - (n-k)*p*q/(n-k-2))/sqrt(2*q*(trRhat2-p*p/(n-k))*cpn);
  arma::vec values(2);
  values(0) = TYS;
  values(1) = cpn;
  return values;

}


//Test Proposed by Zhou et al. (2017)
// [[Rcpp::export]]
arma::vec glhtbf_zgz2017_cpp(List Y, const arma::mat & tG, const arma::vec & n , int p){
  int k = Y.length(); //number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat H = tG.t()*inv_sympd(tG*D*tG.t())*tG;
  arma::mat hatM(p,k);
  double trOmegan=0;
  arma::vec A(k);
  arma::vec B(k);
  arma::vec Q(k); Q.zeros();
  arma::mat Bij(k,k);
  if(p<ss){
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi*zi.t()/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      for(int f=0; f<n(i); ++f){
        double cXtcX = arma::as_scalar(zi.col(f).t()*zi.col(f));
        Q(i) +=cXtcX*cXtcX;
      }
      Q(i) = Q(i)/(n(i)-1);
      trOmegan += H(i,i)*A(i)/n(i);
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi*zi.t()*zj*zj.t())/((n(i)-1)*(n(j)-1));//tr(hbSigma1*hbSigma2)
      }}}
  else{
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi.t()*zi/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      for(int f=0; f<n(i); ++f){
        double cXtcX = arma::as_scalar(zi.col(f).t()*zi.col(f));
        Q(i) +=cXtcX*cXtcX;
      }
      Q(i) = Q(i)/(n(i)-1);
      trOmegan += H(i,i)*A(i)/n(i);
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi.t()*zj*zj.t()*zi)/((n(i)-1)*(n(j)-1));
      }
    }
  }
  double Tnp = trace(hatM*H*hatM.t())-trOmegan;

  arma::vec uB(k); //unbiased estimates of tr(bSigma_i^2)
  uB.zeros();
  double K2s1 = 0;
  arma::mat K2s2mat(k,k);
  for(int i=0;i<k;++i){
    uB(i) = (n(i)-1)*((n(i)-1)*(n(i)-2)*B(i)+A(i)*A(i)-n(i)*Q(i))/(n(i)*(n(i)-2)*(n(i)-3));
    K2s1 += H(i,i)*H(i,i)*uB(i)/(n(i)*(n(i)-1));
    for(int j=0;j<k;++j){
      K2s2mat(i,j) = H(i,j)*H(i,j)*Bij(i,j)/(n(i)*n(j));
    }
  }

  double K2s2 = accu(K2s2mat)-sum(K2s2mat.diag());
  double sigma2 = 2*(K2s1+K2s2);

  arma::vec stats(2);
  stats(0)=Tnp;
  stats(1)=sigma2;
  return stats;
}


// Test Proposed by Zhang et al. (2017)
// [[Rcpp::export]]
arma::vec glht_zgz2017_cpp(List Y, const arma::mat & tG, const arma::vec & n , int p){
  int k = Y.length(); //number of classes
  int q = rank(tG);
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat H = tG.t()*inv_sympd(tG*D*tG.t())*tG;
  arma::mat hatM(p,k);
  arma::mat S(p,p);
  S.zeros();
  for(int i=0; i<k; ++i){
    arma::mat y = Y[i];
    arma::colvec mu = arma::mean(y, 1);
    hatM.col(i) = mu;
    arma::mat z=y-arma::repmat(mu,1,n(i));
    S += z*z.t();
  }
  S = S/(ss-k);
  double Tnp = trace(hatM*H*hatM.t());

  double trSn=arma::trace(S);//tr(Sigmahat^2)
  double trSn2=arma::accu(S%S);//tr(Sigmahat^2) ##element-wise multiplication
  double uA = (ss-k)*(ss-k+1)*(pow(trSn,2)-2*trSn2/(ss-k+1))/(ss-k-1)/(ss-k+2);
  double uB = pow(ss-k,2)*(trSn2-pow(trSn,2)/(ss-k))/(ss-k-1)/(ss-k+2);

  double beta = uB/trSn;
  double d = q*uA/uB;
  arma::vec stats(4);
  stats(0)=Tnp;
  stats(1)=beta;
  stats(2)=d;
  return stats;
}


// Test proposed by Zhang et al.(2022)
// [[Rcpp::export]]
arma::vec glhtbf_zzg2022_cpp(List Y, const arma::mat & tG, const arma::vec & n , int p){
  int k = Y.length(); //number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat D2 = diagmat(1/sqrt(n));
  arma::mat H = tG.t()*inv_sympd(tG*D*tG.t())*tG;
  arma::mat hatM(p,k);
  arma::mat Amat = D2*H*D2;
  arma::vec A(k);
  arma::vec B(k);
  arma::mat Bij(k,k);
  if(p<ss){
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi*zi.t()/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi*zi.t()*zj*zj.t())/((n(i)-1)*(n(j)-1));//tr(hbSigma1*hbSigma2)
      }}}
  else{
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi.t()*zi/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi.t()*zj*zj.t()*zi)/((n(i)-1)*(n(j)-1));
      }
    }
  }
  double Tnp = trace(hatM*H*hatM.t());

  arma::vec uB(k); //unbiased estimates of tr(bSigma_i^2)
  arma::vec uC(k); //unbiased estimates of tr^2(bSigma_i)
  arma::mat S1(k,k);
  arma::mat S2(k,k);
  double trOmegan = 0;
  double tr2Omegans1 = 0;
  double trOmegan2s1 = 0;
  for(int i=0;i<k;++i){
    uB(i) = (n(i)-1)*(n(i)-1)*(B(i)-A(i)*A(i)/(n(i)-1))/((n(i)-2)*(n(i)+1));
    uC(i) = n(i)*(n(i)-1)*(A(i)*A(i)-2*B(i)/n(i))/((n(i)-2)*(n(i)+1));
    trOmegan += Amat(i,i)*A(i);
    tr2Omegans1 += Amat(i,i)*Amat(i,i)*uC(i);
    trOmegan2s1 += Amat(i,i)*Amat(i,i)*uB(i);
    for(int j=0;j<k;++j){
      S1(i,j) = Amat(i,i)*Amat(j,j)*A(i)*A(j);
      S2(i,j) = Amat(i,j)*Amat(i,j)*Bij(i,j);
    }
  }
  double tr2Omegan = tr2Omegans1 + accu(S1)-sum(S1.diag());
  double trOmegan2 = trOmegan2s1 + accu(S2)-sum(S2.diag());
  double beta = trOmegan2/trOmegan;
  double d = tr2Omegan/trOmegan2;

  arma::vec stats(3);
  stats(0)=Tnp;
  stats(1)=beta;
  stats(2)=d;
  return stats;
}

//Test proposed by Zhu and Zhang (2022)
// [[Rcpp::export]]
arma::vec glht_zz2022_cpp(List Y, const arma::mat & tG, const arma::vec & n , int p){
  int k = Y.length(); //number of classes
  int q = rank(tG);
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat H = tG.t()*inv_sympd(tG*D*tG.t())*tG;
  arma::mat hatM(p,k);
  arma::mat z;
  arma::mat S;
  for(int i=0; i<k; ++i){
    arma::mat y = Y[i];
    arma::colvec mu = arma::mean(y, 1);
    hatM.col(i) = mu;
    z.insert_cols(0,y-arma::repmat(mu,1,n(i)));
  }

  if(p<ss){
    S= z*z.t()/(ss-k);
  }else{
    S=z.t()*z/(ss-k);
  }

  double Tnp = trace(hatM*H*hatM.t())-q*trace(S);

  double trSn=arma::trace(S);//tr(Sigmahat^2)
  double trSn2=arma::accu(S%S);//tr(Sigmahat^2) ##element-wise multiplication
  double trSn3=arma::trace(S*S*S);//tr(Sigmahat^3)
  double htrSn2 = pow((ss-k),2)*(trSn2-trSn*trSn/(ss-k))/(ss-k-1)/(ss-k+2);
  double htrSn3 = pow((ss-k),4)*(trSn3-3*trSn*trSn2/(ss-k)+2*pow(trSn,3)/pow((ss-k),2))/(pow(ss-k,2)+2*(ss-k)-3)/(pow(ss-k,2)-4);


  double beta0 = -q*(ss-k+q)*htrSn2*htrSn2/htrSn3/(ss-k-q);
  double beta1 = (ss-k-q)*htrSn3/htrSn2/(ss-k);
  double d = q*(ss-k)*(ss-k+q)*pow(htrSn2,3)/pow(htrSn3,2)/pow((ss-k-q),2);
  arma::vec stats(4);
  stats(0)=Tnp;
  stats(1)=beta0;
  stats(2)=beta1;
  stats(3)=d;
  return stats;
}

//Test proposed by Zhang and Zhu (2022)
// [[Rcpp::export]]
arma::vec glhtbf_zz2022_cpp(List Y, const arma::mat & tG, const arma::vec & n , int p){
  int k = Y.length(); //number of classes
  int ss = sum(n);
  arma::mat D = diagmat(1/n);
  arma::mat H = tG.t()*inv_sympd(tG*D*tG.t())*tG;
  arma::mat hatM(p,k);
  double trOmegan=0;
  arma::vec A(k);
  arma::vec B(k);
  arma::vec C(k);
  arma::mat Bij(k,k);
  arma::mat Dij(k,k);
  if(p<ss){
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi*zi.t()/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      C(i) = arma::trace(Si*Si*Si); //tr(hbSigma_i^3)
      trOmegan += H(i,i)*A(i)/n(i);
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi*zi.t()*zj*zj.t())/((n(i)-1)*(n(j)-1));
        Dij(i,j) = arma::trace(zi*zi.t()*zi*zi.t()*zj*zj.t())/((n(i)-1)*(n(i)-1)*(n(j)-1));
      }}}
  else{
    for(int i=0; i<k; ++i){
      arma::mat yi = Y[i];
      arma::colvec mui = arma::mean(yi, 1);
      hatM.col(i) = mui;
      arma::mat zi=yi-arma::repmat(mui,1,n(i));
      arma::mat Si = zi.t()*zi/(n[i]-1);
      A(i) = arma::trace(Si); //tr(hbSigma_i)
      B(i) = arma::accu(Si%Si); //tr(hbSigma_i^2)
      C(i) = arma::trace(Si*Si*Si); //tr(hbSigma_i^3)
      trOmegan += H(i,i)*A(i)/n(i);
      for(int j=0;j<k; ++j){
        arma::mat yj = Y[j];
        arma::colvec muj = arma::mean(yj, 1);
        arma::mat zj=yj-arma::repmat(muj,1,n(j));
        Bij(i,j) = arma::trace(zi.t()*zj*zj.t()*zi)/((n(i)-1)*(n(j)-1));
        Dij(i,j) = arma::trace(zi.t()*zi*zi.t()*zj*zj.t()*zi)/((n(i)-1)*(n(i)-1)*(n(j)-1));
      }
    }
  }
  double Tnp = trace(hatM*H*hatM.t())-trOmegan;

  arma::vec uB(k); //unbiased estimates of tr(bSigma_i^2)
  uB.zeros();
  arma::vec uC(k); //unbiased estimates of tr(bSigma_i^3)
  uC.zeros();
  double K2s1 = 0;
  double K3s1 = 0;
  double K3s3 = 0;
  for(int i=0;i<k;++i){
    double c = pow((n(i)-1),4)/((n(i)*n(i)+n(i)-6)*(n(i)*n(i)-2*n(i)-3));
    uB(i) = (n(i)-1)*(n(i)-1)*(B(i)-A(i)*A(i)/(n(i)-1))/((n(i)-2)*(n(i)+1));
    uC(i) = c*(C(i)-3*A(i)*B(i)/(n(i)-1)+2*pow(A(i),3)/((n(i)-1)*(n(i)-1)));
    K2s1 += H(i,i)*H(i,i)*uB(i)/(n(i)*(n(i)-1));
    K3s1 += pow(H(i,i),3)*(n(i)-2)*uC(i)/(n(i)*n(i)*(n(i)-1)*(n(i)-1));
  }
  arma::mat K2s2mat(k,k);
  arma::mat K3s2mat(k,k);

  for(int i=0;i<k;++i){
    for(int j=0;j<k;++j){
      double Cij = (n(i)-1)*((n(i)-1)*Dij(i,j)-Bij(i,j)*A(i))/((n(i)-2)*(n(i)+1));
      K2s2mat(i,j) = H(i,j)*H(i,j)*Bij(i,j)/(n(i)*n(j));
      K3s2mat(i,j) = H(i,i)*H(i,j)*H(i,j)*Cij/(n(i)*n(i)*n(j));
    }
  }
  double K2s2 = accu(K2s2mat)-sum(K2s2mat.diag());
  double K3s2 = accu(K3s2mat)-sum(K3s2mat.diag());

  for(int i=2; i<k;++i){
    for(int j=1; j<i; ++j){
      for(int r=0; r<j; ++r){
        arma::mat yi = Y[i];
        arma::mat yj = Y[j];
        arma::mat yr = Y[r];
        arma::mat zi=yi-arma::repmat(hatM.col(i),1,n(i));
        arma::mat zj=yj-arma::repmat(hatM.col(j),1,n(j));
        arma::mat zr=yr-arma::repmat(hatM.col(r),1,n(r));
        if(p<ss){
          K3s3 += H(i,j)*H(j,r)*H(r,i)*arma::trace(zi*zi.t()*zj*zj.t()*zr*zr.t())/((n(i)-1)*(n(j)-1)*(n(r)-1))/(n(i)*n(j)*n(r));
        }
        else{
          K3s3 += H(i,j)*H(j,r)*H(r,i)*arma::trace(zi.t()*zj*zj.t()*zr*zr.t()*zi)/((n(i)-1)*(n(j)-1)*(n(r)-1))/(n(i)*n(j)*n(r));
        }
      }
    }}
  double K2 = 2*(K2s1+K2s2);
  double K3 = 8*(K3s1+3*K3s2+6*K3s3);

  double beta0 = -2*K2*K2/K3;
  double beta1 = K3/(4*K2);
  double d = 8*K2*K2*K2/(K3*K3);
  arma::vec stats(4);
  stats(0)=Tnp;
  stats(1)=beta0;
  stats(2)=beta1;
  stats(3)=d;
  return stats;
}

// Test proposed by Zhu et al. (2022)
// [[Rcpp::export]]
arma::vec glht_zzz2022_cpp(const arma::mat & Y, const arma::mat & X, const arma::mat & C){
  int n = Y.n_rows;
  int p = Y.n_cols;
  int k = X.n_cols;
  int q = rank(C);
  arma::mat XtXinv = inv_sympd(X.t()*X);
  arma::mat H = X*XtXinv*C.t()*inv_sympd(C*XtXinv*C.t())*C*XtXinv*X.t();
  arma::mat Sh = Y.t()*H*Y;
  arma::mat P = X*XtXinv*X.t();
  arma::mat I(n,n);
  I.eye();
  arma::mat Se = Y.t()*(I-P)*Y;
  arma::mat Sigma = Se/(n-k);
  arma::mat invD = diagmat(1/Sigma.diag());
  double Tnp = trace(Sh*invD)/p/q;

  double trRhat2 = trace(invD*Sigma*invD*Sigma);
  double trR2  = (n-k)*(n-k)*(trRhat2-pow(p,2)/(n-k))/(n-k-1)/(n-k+2);
  double hatd = p*p*q/trR2;
  arma::vec values(2);
  values(0) = Tnp;
  values(1) = hatd;
  return values;
}
