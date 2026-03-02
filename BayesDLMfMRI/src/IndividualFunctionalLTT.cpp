// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
# include <RcppArmadillo.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;
// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export(name=".Individual_FunctionalTestLT")]]

Rcpp::List Individual_FunctionalTestLT(const arma::mat & ffd1, const arma::mat & Cova, const arma::mat & m0In, 
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
  
  //COPY THE PRIOR HYPERPARAMETERS FOR THE APPLICATION OF OUR ALGORIHTM 
  arma::colvec m01c(p1);
  arma::mat c01c(p1, p1);
  double ntCut = 0;
  double sigma_Y0 =0;
  
  //NUMBER OF CURVES DRAWING FROM OUR ALGORITHM
  int   nsimu = Nsimu;
  
  //CUT POSITION FOR THE FUNCTIONAL TEST
  int cutpos = CUTpos;
  
  //AUXILIARY VECTORS AND MATRICES
  arma::rowvec m0_mean(p1);
  arma::rowvec Sigma_mean(p1);
  arma::colvec theta_simu(p1);
  arma::rowvec Y_simu(nsimu);
  arma::mat    Y_simu2(N-cutpos, nsimu);
  arma::mat    Theta_online_curve2(N-cutpos, p1);
  arma::cube   Theta_online_curves(N-cutpos, p1, nsimu);
  LogicalMatrix res2(nsimu, p1);
  arma::rowvec evidenTheta(p1);
  
  //VECTORS TO ALLOCATE THE APPROXIMATION ERROR (MEASURE OF FITNESS)
  arma::rowvec Efit(N-cutpos);
  arma::rowvec Efit2(nsimu);
  
  
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
    
    //THE POSTERIOR PARAMETERS AT TIME cutpos ARE STORED FOR THE SIMULATION OF THE ON-LINE THETA TRAJECTORIES
    if(i==cutpos-1){
      for(int j=0; j<p1; j++){m01c[j] = mean(m0.row(j));}
      c01c = c0;
      double aux11 = 0;
      for(int k=0; k<q1; k++){aux11 += arma::as_scalar(sum(S0.col(k)));}
      sigma_Y0 = aux11/pow(q1, 2);
      ntCut = nt0;
    }
    
    
    //IN THIS BLOCK ARE SIMULATED nsimu BOLD RESPONSES
    if(i>=(cutpos)){
      double aux1 = 0;
      for(int k=0; k<q1; k++){aux1 += arma::as_scalar(sum(S0.col(k)));}
      for(int j=0; j<p1; j++){m0_mean[j] = mean(m0.row(j));} 
      double sigma_Y = sqrt(aux1)/q1;
      for(int j=0; j<p1; j++){Sigma_mean[j] = sqrt(arma::as_scalar(aux1*c0(j,j))/pow(q1, 2));}
      for(int i=0; i<nsimu; i++){
        for(int j=0; j<p1; j++){theta_simu[j]= R::rnorm(arma::as_scalar(m0_mean[j]), arma::as_scalar(Sigma_mean[j]));} 
        Y_simu[i] = arma::as_scalar(F1*theta_simu) + R::rnorm(0, sigma_Y);
      }
      Y_simu2.row(i-cutpos) =  Y_simu;
    }
    //END BLOCK
    
  }
  
  
  
  
  //BEGIN(SIMULATE THE ON-LINE THETA TRAJECTORIES)
  for(int j=0; j < nsimu; j++){
    arma::colvec m01  = m01c;
    arma::mat c01  = c01c;
    double S01  = sigma_Y0;
    double    nt01 = ntCut;
    
    for(int i = cutpos; i<N; i++){
      arma::rowvec F1 = X1.row(i);
      arma::colvec a11  = m01;
      arma::mat    R11  = beta0*c01*beta0;
      arma::rowvec f11  = F1*m01;
      double       Q11  = arma::as_scalar(F1*c01*F1.t()) + 1.0;
      double       e11  = arma::as_scalar(Y_simu2(i-cutpos,j) - f11);
      arma::colvec A11  = R11*F1.t()/Q11;
      int          n11  = nt01 + 1;
      double       S11  = (nt01*S01 + e11*e11/Q11)/n11;
      arma::mat    C11  = R11 - A11*A11.t()*Q11;
      arma::colvec m11  = a11 + A11*e11;
      
      nt01 = n11;
      m01  = m11;
      c01  = C11;
      S01  = S11;

      Efit(i-cutpos) = std::abs(arma::as_scalar(Y1(i,0) - f11))/std::abs(arma::as_scalar(Y1(i,0)));
      
      Theta_online_curve2.row(i-cutpos) = m11.t();
    }
    
    Efit2(j) = median(Efit);
    Theta_online_curves.slice(j) = Theta_online_curve2;
    
    for(int k=0; k<p1; k++){res2(j,k) = any(Theta_online_curve2.col(k)<0);}
    
  }
  //END(SIMULATE THE ON-LINE THETA TRAJECTORIES)
  
  //COMPUTE THE FITNESS MEASURE
  double FitnessV = median(Efit2);
  
  //COMPUTE THE FUNCTIONAL EVIDENCE FOR A POSSIBLE BRAIN ACTIVATION
  for(int k=0; k<p1; k++){evidenTheta[k] = 1- mean(res2(_, k) );}
  
  
  return Rcpp::List::create(Rcpp::Named("Eviden")= evidenTheta,
  Rcpp::Named("Online_theta")=Theta_online_curves,
  Rcpp::Named("Y_simu")=Y_simu2,
  Rcpp::Named("FitnessV") =FitnessV);
  
  //return Rcpp::List::create(Rcpp::Named("Eviden")= evidenTheta);
  
}



