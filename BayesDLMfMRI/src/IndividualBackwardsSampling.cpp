// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
# include <RcppArmadillo.h>
#include <RcppDist.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]

using namespace Rcpp ;
// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export(name=".Individual_Backwards_Sampling")]]

Rcpp::List Individual_Backwards_Sampling(const arma::mat & ffd1, const arma::mat & Cova, const arma::mat & m0In,
                                       const arma::mat & c0In, const arma::mat & S0In, const arma::mat & beta0In,
                                       const double nt0In, const int NIn, const int Nsimu, const int CUTpos){

  arma::mat Y1 = ffd1;
  arma::mat X1 = Cova;
  arma::mat m0 = m0In;
  arma::mat c0 = c0In;
  arma::mat S0 = S0In;
  arma::mat beta0 = beta0In;
  double nt0 = nt0In;
  int    N   = NIn;

  //NUMBER OF VOXELS INSIDE THE CLUSTER
  int  q1 = m0.n_cols;

  //NUMBER OF COVARIATES IN THE MODEL
  int  p1 = m0.n_rows;


  arma::mat m1(p1, q1);
  arma::mat   S1(q1, q1);


  //NUMBER OF CURVES DRAWING FROM OUR ALGORITHM
  int   nsimu = Nsimu;

  //CUT POSITION FOR THE FUNCTIONAL TEST
  int cutpos = CUTpos;

  //AUXILIARY VECTORS AND MATRICES
arma::cube   Theta_mean(p1, q1, N);
//arma::cube   Theta_simu(p1, q1, N);
arma::cube   C0_out(p1, p1, N);
arma::cube   S0_out(q1, q1, N);
arma::rowvec nt1(N);
arma::cube   Indi_joint(N, p1, nsimu);
arma::cube   Indi_margin(N, p1, nsimu);
arma::cube   Indi_lt(N, p1, nsimu);
arma::cube   Theta_Margin_out(N, p1, nsimu);
arma::cube   Theta_Mean_out(N, p1, nsimu);

Indi_joint.ones();
Indi_margin.ones();
Indi_lt.ones();
Theta_mean.zeros();
//Theta_simu.zeros();
Theta_Margin_out.zeros();
Theta_Mean_out.zeros();


for(int i = 0; i<N; i++){
arma::rowvec F1 = X1.row(i);
arma::mat    a1  = m0;
arma::mat    R1  = beta0*c0*beta0;
arma::rowvec f1  = F1*m0;
double       Q1  = arma::as_scalar(F1*c0*F1.t()) + 1.0;
arma::rowvec e1  = Y1.row(i) - f1;
arma::colvec A1  = R1*F1.t()/Q1;
int          n1  = nt0 + 1;
arma::mat    S1  = (nt0*S0 + e1.t()*e1/Q1)/n1;
arma::mat    C1  = R1 - A1*A1.t()*Q1;
arma::mat    m1  = a1 + A1*e1;



nt0 = n1;
m0  = m1;
c0  = C1;
S0  = S1;

Theta_mean.slice(i) = m0;
C0_out.slice(i) = c0;
S0_out.slice(i) = S0;
nt1(i) = nt0;

}





for(int j = 0; j < nsimu; j++){
   arma::mat CT = C0_out.slice(N-1);
   arma::mat mT = Theta_mean.slice(N-1);
   arma::mat S0T = S0_out.slice(N-1);
   arma::mat Sigma_simu = riwish(q1 + nt1(N-1) - 1, nt1(N-1)*S0T);
   arma::mat Z1 = arma::randn(p1, q1);
   arma::mat theta_T = m0 +  arma::chol(CT)*Z1*arma::chol(Sigma_simu);

   for(int i = N-1; i >= cutpos; i--){
      arma::mat Z2 = arma::randn(p1, q1);
      arma::mat C11  = C0_out.slice(i-1);
      arma::mat M11  = Theta_mean.slice(i-1);
      arma::mat C_k1 = C11 - C11*inv(beta0*C11*beta0)*C11;
      arma::mat M_k1 = M11 + C11*inv(beta0*C11*beta0)*(theta_T - M11);
      arma::mat theta_2 = M_k1 + arma::chol(C_k1)*Z2*arma::chol(Sigma_simu);
      arma::colvec theta_lt = mean(theta_2, 1);



      for(int k = 0; k<p1; k++){
       if(any(theta_2.row(k)<0)){Indi_joint(i, k, j) = 0;}
       if(theta_2(k,0)<0){Indi_margin(i, k, j) = 0;}
       if(theta_lt(k)<0){Indi_lt(i, k, j) = 0;}

       Theta_Margin_out(i, k, j)  = theta_2(k, 0);
       Theta_Mean_out(i, k, j) = theta_lt(k);
      }



   }


}



arma::mat eviden_joint  = sum(Indi_joint, 0);
arma::mat eviden_margin = sum(Indi_margin, 0);
arma::mat eviden_lt     = sum(Indi_lt,  0);


eviden_joint.elem( find(eviden_joint<N) ).zeros();
eviden_joint.elem( find(eviden_joint==N) ).ones();

eviden_margin.elem( find(eviden_margin<N) ).zeros();
eviden_margin.elem( find(eviden_margin==N) ).ones();

eviden_lt.elem( find(eviden_lt<N) ).zeros();
eviden_lt.elem( find(eviden_lt==N) ).ones();

arma::rowvec Eviden_Joint(p1);
arma::rowvec Eviden_Margin(p1);
arma::rowvec Eviden_Lt(p1);


for(int jj = 0; jj<p1; jj++){
 Eviden_Joint(jj) = mean(eviden_joint.row(jj));
 Eviden_Margin(jj) =    mean(eviden_margin.row(jj));
 Eviden_Lt(jj)     =    mean(eviden_lt.row(jj));
}



return Rcpp::List::create(Rcpp::Named("Eviden_joint") = Eviden_Joint,
Rcpp::Named("Eviden_margin") = Eviden_Margin,
Rcpp::Named("eviden_lt") = Eviden_Lt,
Rcpp::Named("Online_theta") =Theta_Margin_out,
Rcpp::Named("Online_theta_mean") =Theta_Mean_out);

}
