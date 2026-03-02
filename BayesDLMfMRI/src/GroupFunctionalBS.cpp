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
// returning the result of an operation on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export(name=".Group_Functional_Backwards_Sampling")]]

Rcpp::List Group_Functional_Backwards_Sampling(const arma::mat & ffd1, const arma::mat & Cova, const double m0In,
                                     const arma::mat & c0In, const double S0In, const arma::mat & beta0In,
                                     const double nt0In, const int flag1, const int NIn, const int NS, const int Nsimu, const int CUTpos){
  
  arma::mat Y1 = ffd1;
  arma::mat X1 = Cova;
  double m01 = m0In;
  arma::mat c00 = c0In;
  double S01 = S0In;
  arma::mat beta0 = beta0In;
  double nt00 = nt0In;
  int flag = flag1;
  int  N   = NIn;
  int  Ns  = NS;
  //int  j  = Rcpp::as<int>(Serie);
  
  
  //NUMEBER OF CURVES DRAWING FROM OUR ALGORITM. -FOR THE FUNCTIONAL TEST-
  int   nsimu = Nsimu;
  //NUMBER OF VOXELS THAT CONFORM THE CLUSTER
  int  q1 = Y1.n_cols;
  //NUMBER OF COVARIATES IN THE MODEL
  int  p1 = X1.n_cols;
  //CUT POSITION FOR THE FUNCTIONAL TEST
  int cutpos = CUTpos;
  
  
  
  //COPY THE PRIOR HYPERPARAMETERS FOR THE APPLICATION OF OUR ALGORIHTM
  arma::mat c01c(p1, p1);
  c01c.zeros();
  
  
  //TAKING OFF THE COLUMNS WITH NULL VALUES (flag==1 MEANS THAT THERE IS ANY COLUMN WITH ZERO VALUES)
  if(flag==1){
    for(int i=0; i<q1; i++){
      if(any(Y1.col(i)==0)){Y1.shed_col(i);
        i=i-1;
        q1=q1-1;}
    }
  }
  
  
  
  //CREATING THE MATRIX S0 WITH DIMENSIONS ACCORDING TO THE VALUE OF q1
  arma::mat S00(q1, q1);
  S00.eye();
  S00 = S01*S00;
  //CREATING THE MATRIX m0 WITH DIMENSIONS ACCORDING TO THE VALUE OF q1
  arma::mat m00(p1, q1);
  m00.ones();
  m00 = m01*m00;
  
  //DEFINING AUXILIAR VECTORS AND MATRICES
  arma::uvec colAux(q1);
  arma::uvec rowAux(N);
  arma::mat Y_final(N, q1);
  
  
  //DEFINING AUXILIAR MATRICES TO ALLOCATE THE MATRICES RESULTING FROM THE GROUP ANALYSIS
  arma::colvec nt0aux(Ns);
  arma::mat m0aux(p1*Ns, q1);
  arma::mat c0aux(p1*Ns, p1);
  arma::mat S0aux(q1*Ns, q1);
  
  arma::mat S011c(q1, q1);
  arma::mat c011c(p1, p1);
  arma::mat m011c(p1, q1);
  
  
  //AUXILIARY VECTORS AND MATRICES
  arma::cube   Indi_joint(N, p1, nsimu);
  arma::cube   Indi_margin(N, p1, nsimu);
  arma::cube   Indi_lt(N, p1, nsimu);
  arma::cube   Theta_Margin_out(N, p1, nsimu);
  arma::cube   Theta_Mean_out(N, p1, nsimu);
  Indi_joint.ones();
  Indi_margin.ones();
  Indi_lt.ones();
  arma::cube   Theta_out(N-cutpos, p1, nsimu);
  arma::cube   Theta_mean(p1, q1, N);
  arma::cube   C0_out(p1, p1, N);
  arma::cube   S0_out(q1, q1, N);
  arma::rowvec nt1(N);
  
  Indi_joint.ones();
  Indi_margin.ones();
  Indi_lt.ones();
  Theta_mean.zeros();
  Theta_Margin_out.zeros();
  Theta_Mean_out.zeros();
  
  //FILL UP THE MATRICES AND VECTOR HYPERPARAMETERS WITH THE PRIOR VALUES
  for(int j=0; j<Ns; j++){
    nt0aux[j] = nt00;
    m0aux.submat(j*p1, 0, p1*(j+1)-1, q1-1) = m00;
    c0aux.submat(j*p1, 0, p1*(j+1)-1, p1-1) = c00;
    S0aux.submat(j*q1, 0, q1*(j+1)-1, q1-1) = S00;
  }
  
  
  
  //FILLING ONE OF THE AUXILIARY VECTORS WITH THE POSITIONS RELATED TO THE CLUSTER SIZE (q1)
  for(int k=0; k<q1; k++){colAux[k] = k;}
  
  
  for(int i = 0; i<N; i++){
    
    S011c.zeros();
    c011c.zeros();
    m011c.zeros();
    int    n1 = 0;
    
    //FITTING THE MULTIVARIATE DLM FOR EVERY SUBJECT IN THE SAMPLE (Ns SUBJECTS)
    for(int j=0; j<Ns; j++){
      
      double    nt0 = nt0aux[j];
      arma::mat m0  = m0aux.submat(j*p1, 0, p1*(j+1)-1, q1-1);
      arma::mat c0  = c0aux.submat(j*p1, 0, p1*(j+1)-1, p1-1);
      arma::mat S0  = S0aux.submat(j*q1, 0, q1*(j+1)-1, q1-1);
      
      //FILLING ONE OF THE AUXILIARY VECTORS WITH THE POSITIONS RELATED TO THE BOLD RESPONSES OBSERVATIONS FOR THE SUBJECT j (N OBSERVATIONS).
      for(int k2=0 ; k2< N; k2++){rowAux[k2] = j*N + k2;}
      //TAKING THE APPROPIATE SUBMATRIX (OR NEIGHBORHOOD VOXEL SERIES FOR THE SUBJECT j) USING THE AUXILIARY VECTORS
      Y_final   = Y1.submat(rowAux, colAux);
      //STANDARDIZING THE DOLB RESPONSES
      for(int ii=0; ii<q1; ii++){Y_final.col(ii)   = (Y_final.col(ii) - arma::mean(Y_final.col(ii)))/arma::stddev(Y_final.col(ii));}
      
      c011c = c011c + c0;
      m011c = m011c + m0;
      
      
      arma::rowvec F1 = X1.row(i);
      arma::mat    a1  = m0;
      arma::mat    R1  = beta0*c0*beta0;
      arma::rowvec f1  = F1*m0;
      double       Q1  = arma::as_scalar(F1*c0*F1.t()) + 1.0;
      arma::rowvec e1  = Y_final.row(i) - f1;
      arma::colvec A1  = R1*F1.t()/Q1;
                   n1  = nt0 + 1;
      arma::mat    S1  = (nt0*S0 + e1.t()*e1/Q1)/n1;
      arma::mat    C1  = R1 - A1*A1.t()*Q1;
      arma::mat    m1  = a1 + A1*e1;
      
      nt0aux[j] = n1;
      m0aux.submat(j*p1, 0, p1*(j+1)-1, q1-1)  = m1;
      c0aux.submat(j*p1, 0, p1*(j+1)-1, p1-1)  = C1;
      S0aux.submat(j*q1, 0, q1*(j+1)-1, q1-1)  = S1;
      
      S011c = S011c +  S1;
      
      
    }
    
    
    c011c = c011c/pow(Ns, 2);
    S011c = S011c/pow(Ns, 2);
    m011c = m011c/Ns;
    

    Theta_mean.slice(i) = m011c;
    C0_out.slice(i) = c011c;
    S0_out.slice(i) = S011c;
    nt1(i) = n1;
  }
  
  
  for(int j = 0; j < nsimu; j++){
    arma::mat CT = C0_out.slice(N-1);
    arma::mat mT = Theta_mean.slice(N-1);
    arma::mat S0T = S0_out.slice(N-1);
    arma::mat Sigma_simu = riwish(q1 + nt1(N-1) - 1, nt1(N-1)*S0T);
    arma::mat Z1 = arma::randn(p1, q1);
    arma::mat theta_T = m011c +  arma::chol(CT)*Z1*arma::chol(Sigma_simu);
    
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



