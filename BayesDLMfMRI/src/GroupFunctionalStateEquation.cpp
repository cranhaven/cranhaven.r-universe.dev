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
// [[Rcpp::export(name=".Group_Functional_Equation")]]

Rcpp::List Group_Functional_Equation(const arma::mat & ffd1, const arma::mat & Cova, const double m0In,
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
  arma::cube   Indi_joint(N-cutpos, p1, nsimu);
  arma::cube   Indi_margin(N-cutpos, p1, nsimu);
  arma::cube   Indi_lt(N-cutpos, p1, nsimu);
  Indi_joint.ones();
  Indi_margin.ones();
  Indi_lt.ones();
  arma::cube   Theta_out(N-cutpos, p1, nsimu);
  arma::cube   Theta_mean(p1, q1, N);
  arma::cube   C0_out(p1, p1, N);
  arma::cube   S0_out(q1, q1, N);



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
  int          n1  = nt0 + 1;
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


  //IN THIS BLOCK ON-LINE TRAJECTORIES ARE SIMULATED
  if(i>=(cutpos)){
  arma::mat Wt = beta0*c011c*beta0 - c011c;
  arma::mat Wt_chol = arma::chol(Wt);
  arma::mat S1_chol = arma::chol(S011c);

    for(int k = 0; k<nsimu; k++){
         arma::mat Z1 = arma::randn(p1, q1);
         arma::mat Omega  = Wt_chol.t()*Z1*S1_chol.t();
         arma::mat theta_t = m011c +  Omega;
         arma::colvec theta_lt = mean(theta_t, 1);
       for(int j = 0; j<p1; j++){
         if(any(theta_t.row(j)<0)){Indi_joint(i-cutpos, j, k) = 0;}
         if(theta_t(j,0)<0){Indi_margin(i-cutpos, j, k) = 0;}
         if(theta_lt(j)<0){Indi_lt(i-cutpos, j, k) = 0;}
         Theta_out(i-cutpos, j, k ) = theta_t(j, 0);

      }
    }
  }
  //END BLOCK


  Theta_mean.slice(i) = m011c;
  C0_out.slice(i) = c011c;
  S0_out.slice(i) = S011c;

  }


  arma::mat eviden_joint  = sum(Indi_joint, 0);
  arma::mat eviden_margin = sum(Indi_margin, 0);
  arma::mat eviden_lt     = sum(Indi_lt,  0);


  eviden_joint.elem( find(eviden_joint<N-cutpos) ).zeros();
  eviden_joint.elem( find(eviden_joint==N-cutpos) ).ones();

  eviden_margin.elem( find(eviden_margin<N-cutpos) ).zeros();
  eviden_margin.elem( find(eviden_margin==N-cutpos) ).ones();

  eviden_lt.elem( find(eviden_lt<N-cutpos) ).zeros();
  eviden_lt.elem( find(eviden_lt==N-cutpos) ).ones();

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
  Rcpp::Named("Theta.simu") =Theta_out,
  Rcpp::Named("Theta.fit") =Theta_mean,
  Rcpp::Named("C.fitt") = C0_out,
  Rcpp::Named("S.fit") = S0_out);

}



