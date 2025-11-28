#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;
using namespace std;

const double log2pi = log(2.0 * M_PI);

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::rowvec colmeanNA(arma::mat Y){
  int P = Y.n_cols;
  arma::rowvec colmean = arma::zeros<arma::rowvec>(P);
  for (int i=0; i < P; ++i){
    arma::vec ycol = Y.col(i);
    arma::uvec finiteind = find_finite(ycol);
    arma::vec yy = ycol(finiteind);
    colmean(i) = mean(yy);
  }
  return colmean;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
int sample_index(const int size,
                 const NumericVector prob = NumericVector::create()){
  //sample one number from 1:size
  arma::vec sequence = arma::linspace<arma::vec>(1, size, size);
  arma::vec out = Rcpp::RcppArmadillo::sample(sequence, size, false, prob);
  return out(0);
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat mvrnormArma(const int n,
                      const arma::vec mu,
                      const arma::mat Sigma) {
  //returns random multivariate normal vectors with mean mu and covariance Sigma
  //input : integer n for the number of vectors you'd like to draw
  //      : vector mu for the mean
  //      : matrix Sigma for the covariance - needs to be psd
  int ncols = Sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(Sigma);
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
double dmvnrm_arma(const arma::rowvec x,
                   const arma::rowvec mean,
                   const arma::mat sigma,
                   const bool logd = false) {
  //returns the density of a multivariate normal vector
  //input : a rowvector x whose density you'd like to know
  //      : a rowvector mean for the mean of mvn
  //      : a matrix Sigma for covariance, needs to be psd
  //      : a boolean logd, true if you like the log density
  int xdim = x.n_cols;
  if(xdim==0){return 0;}
  double out;
  arma::mat rooti = arma::trans(arma::inv(trimatu(arma::chol(sigma))));
  double rootisum = arma::sum(log(rooti.diag()));
  double constants = -(static_cast<double>(xdim)/2.0) * log2pi;
  arma::vec z = rooti*arma::trans(x-mean);
  out = constants - 0.5*arma::sum(z%z)+rootisum;
  if (logd == false){
    out = exp(out);
  }
  return(out);
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat em_with_zero_mean_c(arma::mat y,
                              const int maxit){
  int orig_p = y.n_cols;
  arma::vec vars = arma::zeros<arma::vec>(orig_p);
  for (int i=0; i < orig_p; ++i){
    arma::vec ycol = y.col(i);
    arma::uvec finiteind = find_finite(ycol);
    arma::vec yy = ycol(finiteind);
    vars(i) = sum((yy-mean(yy))%(yy-mean(yy)));
  }
  arma::uvec valid_ind = find(vars>1e-6);
  y = y.cols(valid_ind);
  int p = y.n_cols;
  int n = y.n_rows;
  arma::rowvec mu = arma::zeros<arma::rowvec>(p);
  arma::mat y_imputed = y;
  for (int j = 0; j < p; ++j){
    arma::uvec colind = arma::zeros<arma::uvec>(1);
    colind(0) = j;
    arma::uvec nawhere = find_nonfinite(y_imputed.col(j));
    arma::uvec nonnawhere = find_finite(y_imputed.col(j));
    arma::vec tempcolmean = mean(y_imputed(nonnawhere, colind), 0);
    y_imputed(nawhere, colind).fill(tempcolmean(0));
  }
  arma::mat oldSigma = y_imputed.t() * y_imputed / n;
  arma::mat Sigma = oldSigma;
  double diff = 1;
  int it = 1;
  while (diff>0.001 && it < maxit){
    arma::mat bias = arma::zeros<arma::mat>(p,p);
    for (int i=0; i<n; ++i){
      arma::rowvec tempdat = y.row(i);
      arma::uvec ind = find_finite(tempdat);
      arma::uvec nind = find_nonfinite(tempdat);
      if (0 < ind.size() && ind.size() < p){
        //MAKE THIS PART FASTER
        bias(nind, nind) += Sigma(nind, nind) - Sigma(nind, ind) * (Sigma(ind, ind).i()) * Sigma(ind, nind);
        arma::uvec rowind = arma::zeros<arma::uvec>(1);
        rowind(0) = i;
        arma::mat yvec = y(rowind, ind);
        //MAKE THIS PART FASTER
        y_imputed(rowind, nind) = (Sigma(nind, ind)*(Sigma(ind, ind).i())*y(rowind, ind).t()).t();
      }
    }
    Sigma = (y_imputed.t() * y_imputed + bias)/n;
    arma::mat diffmat = (Sigma-oldSigma);
    arma::mat diffsq = diffmat%diffmat;
    diff = accu(diffsq);
    oldSigma = Sigma;
    it = it + 1;
  }
  arma::mat finalSigma = arma::zeros<arma::mat>(orig_p, orig_p);
  finalSigma.submat(valid_ind, valid_ind.t()) = Sigma;
  /*
  return Rcpp::List::create(
    Rcpp::Named("Sigma") = finalSigma,
  Rcpp::Named("iteration") = it
  )*/
  return finalSigma;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::cube rinvwish_c(const int n,
                      const int v,
                      const arma::mat S){
  //draw a matrix from inverse wishart distribution with parameters S and v
  RNGScope scope;
  int p = S.n_rows;
  arma::mat L = chol(inv_sympd(S), "lower");
  arma::cube sims(p, p, n, arma::fill::zeros);
  for(int j = 0; j < n; j++){
    arma::mat A(p,p, arma::fill::zeros);
    for(int i = 0; i < p; i++){
      int df = v - (i + 1) + 1; //zero-indexing
      A(i,i) = sqrt(R::rchisq(df));
    }
    for(int row = 1; row < p; row++){
      for(int col = 0; col < row; col++){
        A(row, col) = R::rnorm(0,1);
      }
    }
    arma::mat LA_inv = inv(trimatl(trimatl(L) * trimatl(A)));
    sims.slice(j) = LA_inv.t() * LA_inv;
  }
  return(sims);
}



// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec get_target_c(const arma::vec X,
                       const arma::mat Y,
                       const double sigmabeta,
                       const arma::mat Sigma,
                       const arma::vec gam,
                       const arma::vec beta){
  //get the target likelihood circumventing the missing value issue
  int P = Y.n_cols;
  int n = Y.n_rows;
  double L = 0;
  double B = 0;
  double G = 0;
  for (int i=0; i < n; ++i){
    arma::uvec naind = find_finite(Y.row(i).t());
    if(naind.size()>0){
      arma::uvec rowind = arma::zeros<arma::uvec>(1);
      rowind(0) = i;
      arma::rowvec Ytemp = Y(rowind, naind.t());
      L = L + dmvnrm_arma(Ytemp,
                          X(i)*beta(naind).t(),
                          Sigma(naind,naind.t()),
                          true);
    }
  }
  arma::uvec ind = find(gam==1);
  int s = ind.size();
  if(s>0){
    arma::vec ds = Sigma.diag();
    for (int j=0; j<s; ++j){
      int newind = ind(j);
      B = B + R::dnorm(beta(newind), 0, sqrt(sigmabeta), true);
    }
  }
  G = log(std::tgamma(s+1)*std::tgamma(P-s+1)/std::tgamma(P+2));
  arma::vec out = arma::zeros<arma::vec>(3);
  out(0) = L;
  out(1) = B;
  out(2) = G;
  return out;
}











// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat update_Sigma_c(const int n,
                         const int nu,
                         const arma::vec X,
                         const arma::vec beta,
                         const arma::mat Phi,
                         const arma::mat Y){
  int P = Y.n_cols;
  arma::mat X2 = arma::zeros<arma::mat>(n, 1);
  X2.col(0) = X;
  arma::mat beta2 = arma::zeros<arma::mat>(P,1);
  beta2.col(0) = beta;
  arma::mat r = Y - X2 * beta2.t();
  arma::mat emp = em_with_zero_mean_c(r,100);
  arma::cube res = rinvwish_c(1, n+nu, emp*n + Phi*nu);
  return res.slice(0);
}


// [[Rcpp::export]]
Rcpp::List update_gamma_random_c(const arma::vec X,
                             const arma::mat Y,
                             const arma::vec gam){
  arma::vec prob = arma::ones<arma::vec>(gam.size());
  NumericVector prob2 = wrap(prob);
  int changeind = sample_index(gam.size(), prob2) - 1;
  arma::vec newgam = gam;
  newgam(changeind) = abs(gam(changeind) - 1);
  // int change = newgam(changeind);
  return(
    Rcpp::List::create(
      Rcpp::Named("gam") = newgam,
      Rcpp::Named("changeind") = changeind)
  );
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec betagam_accept_random_c(const arma::vec X,
                              const arma::mat Y,
                              const double sigmabeta1,
                              const arma::mat inputSigma,
                              const double Vbeta,
                              const arma::vec gam1,
                              const arma::vec beta1,
                              const arma::vec gam2,
                              const arma::vec beta2,
                              const int changeind,
                              const int change){
  double newtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam2,beta2));
  double oldtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam1,beta1));
  double proposal_iter = R::dnorm(beta1(changeind)-beta2(changeind),0, sqrt(Vbeta),true);
  double proposal_ratio = proposal_iter;
  // arma::rowvec marcor2 = min(marcor)+max(marcor)-marcor;
  // double proposal_ratio = 0;
  // if(change==1){
  //   arma::uvec ind1 = find(gam1==0);
  //   arma::uvec ind2 = find(gam2==1);
  //   double tempadd = marcor(changeind)/sum(marcor(ind1));
  //   double tempremove = marcor2(changeind)/sum(marcor2(ind2));
  //   proposal_ratio = -log(tempadd)+log(tempremove)-proposal_iter;
  // }else{
  //   arma::uvec ind1 = find(gam1==1);
  //   arma::uvec ind2 = find(gam2==0);
  //   double tempadd = marcor(changeind)/sum(marcor(ind2));
  //   double tempremove = marcor2(changeind) / sum(marcor2(ind1));
  //   proposal_ratio = log(tempadd)-log(tempremove)+proposal_iter;
  // }
  double final_ratio = newtarget-oldtarget+proposal_ratio;
  arma::vec out = arma::zeros<arma::vec>(4);
  out(0) = final_ratio;
  out(1) = newtarget;
  out(2) = oldtarget;
  out(3) = proposal_ratio;
  return(out);
}



// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
Rcpp::List update_betagam_random_c(const arma::vec X,
                               const arma::mat Y,
                               arma::vec gam1,
                               arma::vec beta1,
                               const arma::mat Sigma,
                               const double sigmabeta,
                               const double Vbeta,
                               const int bgiter,
                               const double smallchange){
  arma::vec gam2;
  arma::vec beta2;
  for (int i=1; i<bgiter; ++i){
    Rcpp::List temp = update_gamma_random_c(X,Y,gam1);
    arma::vec gam2 = as<arma::vec>(temp["gam"]);
    int changeind = temp["changeind"];
    int change = gam2(changeind);
    arma::vec beta2 = beta1 % gam2;
    if(change==1){
      arma::uvec ind = find(gam1==1);
      beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(smallchange)));
      beta2(changeind) = beta2(changeind) + (rnorm(1, 0, sqrt(Vbeta)))[0];
    }else{
      arma::uvec ind = find(gam2==1);
      beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(smallchange)));
      beta2(changeind) = 0;
    }
    arma::vec A = betagam_accept_random_c(X,Y,sigmabeta,
                                      Sigma,Vbeta,
                                      gam1,beta1,
                                      gam2,beta2,
                                      changeind,change);
    NumericVector check2 = runif(1);
    double check = check2(0);
    if(exp(A(0)) > check){
      gam1 = gam2; beta1 = beta2;
    }
  }
  return Rcpp::List::create(
    Rcpp::Named("gam")= gam1,
    Rcpp::Named("beta") = beta1
  );
}



// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
double get_sigmabeta_from_h_c(const double h,
                              const arma::vec gam,
                              const arma::mat Sigma,
                              const arma::vec X,
                              const int P){
  //convert h to sigmabeta conditioning on gamma and Sigma
  arma::uvec ind = find(gam == 1);
  arma::vec ds = Sigma.diag();
  int n = X.size();
  double num = h * sum(ds);
  double denom = (1-h)* sum(X%X)/n * ind.size() + 1;
  return num/denom;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
double get_h_from_sigmabeta_c(const arma::vec X,
                              const double sigmabeta,
                              const arma::mat Sigma,
                              const arma::vec gam,
                              const int n,
                              const int P){
  //converts sigmabeta to h conditioning on gamma and Sigma
  arma::uvec ind = find(gam==1);
  arma::vec ds = Sigma.diag();
  double num = sum(X%X)/n * ind.size() * sigmabeta;
  double denom = num + sum(ds);
  return num/denom;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
Rcpp::List update_h_c(const double initialh,
                      const int hiter,
                      const arma::vec gam,
                      const arma::vec beta,
                      const arma::mat Sig,
                      const arma::vec X,
                      int P){
  double h1 = initialh;
  double sigbeta1 = get_sigmabeta_from_h_c(initialh, gam, Sig, X, P);
  arma::vec lik = arma::zeros<arma::vec>(hiter);
  arma::vec ds = Sig.diag();
  for (int i=1; i<hiter; ++i){
    double h2 = h1;
    NumericVector(rr) = runif(1, -0.05, 0.05);
    double r = rr(0);
    h2 = h2 + r;
    if(h2<1e-3){h2 = abs(2*1e-6 - h2);}
    if(h2>0.9){h2 = 1.8-h2;}
    arma::uvec ind = find(gam==1);
    double sigmabeta1 = get_sigmabeta_from_h_c(h1, gam, Sig, X, P);
    double sigmabeta2 = get_sigmabeta_from_h_c(h2, gam, Sig, X, P);
    double lik1 = 0; double lik2 = 0;
    for (int j=0; j < ind.size(); ++j){
      int newind = ind(j);
      lik1 = lik1 + R::dnorm(beta(newind), 0, sqrt(sigmabeta1*ds(newind)), true);
      lik2 = lik2 + R::dnorm(beta(newind), 0, sqrt(sigmabeta2*ds(newind)), true);
    }
    double acceptanceprob = exp(lik2-lik1);
    arma::vec ee = runif(1);
    double e = ee(0);
    if(e<acceptanceprob){
      h1 = h2; sigbeta1 = sigmabeta2;
    }
  }
  return Rcpp::List::create(
    Rcpp::Named("h") = h1,
    Rcpp::Named("sigbeta") = sigbeta1
  );
}


//' Main function for variable selection
//' @param X covariate with length N, sample size
//' @param Y multivariate normal response variable N by P
//' @param initial_chain list of starting points for beta, gamma, sigma, and sigmabeta. beta is length P for the coefficients, gamma is length P inclusion vector where each element is 0 or 1. sigma should be P x P covariance matrix, and sigmabeta should be the expected variance of the betas.
//' @param Phi prior for the covariance matrix. We suggest identity matrix if there is no appropriate prior information
//' @param marcor length P vector of correlation between X and each variable of Y
//' @param niter total number of iteration for MCMC
//' @param bgiter number of MH iterations within one iteration of MCMC to fit Beta and Gamma
//' @param burnin number of MH iterations for h, proportion of variance explained
//' @param hiter number of first iterations to ignore
//' @param Vbeta variance of beta
//' @param smallchange perturbation size for MH algorithm
//' @param verbose if set TRUE, print gamma for each iteration
//' @return list of posterior beta, gamma, and covariance matrix sigma
//' @examples
//' beta = c(rep(0.5, 3), rep(0,3))
//' n = 200; T = length(beta); nu = T+5
//' Sigma = matrix(0.8, T, T); diag(Sigma) = 1
//' X = as.numeric(scale(rnorm(n)))
//' error = MASS::mvrnorm(n, rep(0,T), Sigma)
//' gamma = c(rep(1,3), rep(0,3))
//' Y = X %*% t(beta) + error; Y = scale(Y)
//' Phi = matrix(0.5, T, T); diag(Phi) = 1
//' initial_chain = list(beta = rep(0,T),
//'                      gamma = rep(0,T),
//'                      Sigma = Phi,
//'                      sigmabeta = 1)
//' result = mmvbvs(X = X,
//'                 Y = Y,
//'                 initial_chain = initial_chain,
//'                 Phi = Phi,
//'                 marcor = colMeans(X*Y, na.rm=TRUE),
//'                 niter=10,
//'                 verbose = FALSE)
//' @export
// [[Rcpp::export]]
Rcpp::List mmvbvs(const arma::vec X,
                        const arma::mat Y,
                        const Rcpp::List initial_chain,//const Rcpp::List initial_chain2,
                        const arma::mat Phi,
                        const arma::rowvec marcor,
                        const int niter   = 1000,
                        const int bgiter  = 500,
                        const int hiter   = 50,
                        const int burnin  = 100000,
                        const int Vbeta   = 1,
                        const double smallchange = 1e-4,
                        const bool verbose = true){
  //initialize if not user-defined
  int P = Y.n_cols;
  int n = Y.n_rows;
  int nu = n;

  arma::mat outbeta1 = arma::zeros<arma::mat>(P, niter);
  arma::mat outgam1 = arma::zeros<arma::mat>(P,niter);
  arma::cube outSigma1 = arma::zeros<arma::cube>(P,P,niter);
  arma::vec outsb1 = arma::zeros<arma::vec>(niter);
  arma::vec outh1 = arma::zeros<arma::vec>(niter);
  arma::mat tar1 = arma::zeros<arma::mat>(3, niter);

  outbeta1.col(0)    = as<arma::vec>(initial_chain["beta"]);
  outgam1.col(0)     = as<arma::vec>(initial_chain["gamma"]);
  outSigma1.slice(0) = as<arma::mat>(initial_chain["Sigma"]);
  outsb1(0)          = initial_chain["sigmabeta"];

  for (int i=1; i<niter; ++i){
    Rcpp::List bg = update_betagam_random_c(X,
                                        Y,
                                        outgam1.col(i-1),
                                        outbeta1.col(i-1),
                                        outSigma1.slice(i-1),
                                        outsb1(i-1),
                                        Vbeta,
                                        bgiter,
                                        smallchange);
    outgam1.col(i)  = as<arma::vec>(bg["gam"]);
    outbeta1.col(i) = as<arma::vec>(bg["beta"]);
    outSigma1.slice(i) = update_Sigma_c(n,nu,X,outbeta1.col(i),Phi,Y);
    Rcpp::List hsig = update_h_c(outh1[i-1],
                                 hiter,
                                 outgam1.col(i),
                                 outbeta1.col(i),
                                 outSigma1.slice(i),
                                 X,
                                 P);
    outh1(i) = hsig["h"];
    outsb1(i) = hsig["sigbeta"];
    if(!arma::is_finite(outsb1(i))){
      outsb1(i) = 1;
    }
    tar1.col(i) = get_target_c(X,
                               Y,
                               outsb1(i),
                               outSigma1.slice(i),
                               outgam1.col(i),
                               outbeta1.col(i));
    // cout << tar1.col(i).t() << "\n";
    if((i%10==0) & (verbose)){
      // cout << outgam1.col(i).t() << "\n";
      // cout << i << "\n";
    }
    if((i > 5)){
      if(abs(sum(tar1.col(i)) - sum(tar1.col(i-1))) < 1e-5){
        // cout << "Target likelihood converged!" << "\n";
        break;
      }
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("gamma") = outgam1.t(),
    Rcpp::Named("beta") = outbeta1.t(),
    Rcpp::Named("Sigma") = outSigma1,
    Rcpp::Named("sigmabeta") = outsb1,
    Rcpp::Named("h") = outh1,
    Rcpp::Named("target") = tar1.t()
  );

}



// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// Rcpp::List doMCMC_c(arma::vec X,
//                     arma::mat Y,
//                     int n,
//                     int T,
//                     arma::mat Phi,
//                     int nu,
//                     arma::vec initialbeta,
//                     arma::vec initialgamma,
//                     arma::mat initialSigma,
//                     double initialsigmabeta,
//                     arma::rowvec marcor,
//                     double Vbeta,
//                     int niter,
//                     int bgiter,
//                     int hiter,
//                     int switer,
//                     double smallchange){
//   //empty arrays to save values
//   arma::mat outbeta = arma::zeros<arma::mat>(T, niter);
//   arma::mat outgam = arma::zeros<arma::mat>(T,niter);
//   arma::cube outSigma = arma::zeros<arma::cube>(T,T,niter);
//   arma::vec outsb = arma::zeros<arma::vec>(niter);
//   arma::vec outh = arma::zeros<arma::vec>(niter);
//   arma::mat tar = arma::zeros<arma::mat>(3, niter);
//   //initialize
//   outbeta.col(0) = initialbeta;
//   outgam.col(0) = initialgamma;
//   outSigma.slice(0) = initialSigma;
//   outsb(0) = initialsigmabeta;
//   tar.col(0) = arma::zeros<arma::vec>(3);
//   outh(0) = get_h_from_sigmabeta_c(X,outsb(0),outSigma.slice(0),
//        outgam.col(0), n, T);
//   for (int i=1; i<niter; ++i){
//     arma::vec gam1    = outgam.col(i-1);
//     arma::vec beta1   = outbeta.col(i-1);
//     arma::mat Sigma1  = outSigma.slice(i-1);
//     double sigmabeta1 = outsb(i-1);
//     double h1         = outh(i-1);
//     Rcpp::List bg = update_betagam_sw_c(X,Y,gam1,beta1,Sigma1,
//                                         abs(marcor),sigmabeta1,Vbeta,
//                                         bgiter,switer,smallchange);
//     arma::vec gam2  = as<arma::vec>(bg["gam"]);
//     arma::vec beta2 = as<arma::vec>(bg["beta"]);
//     arma::mat Sigma2 = update_Sigma_c(n,nu,X,beta2,Phi,Y);
//     Rcpp::List hsig = update_h_c(h1, hiter, gam2, beta2, Sigma2, X, T);
//     outh(i) = hsig["h"];
//     outsb(i) = hsig["sigbeta"];
//     outgam.col(i) = gam2;
//     outbeta.col(i) = beta2;
//     outSigma.slice(i) = Sigma2;
//     if(!arma::is_finite(outsb(i))){
//       outsb(i) = 1000;
//     }
//     tar.col(i) = get_target_c(X,Y,outsb(i), Sigma2,gam2, beta2);
//     cout << i << "\n";
//   }
//   return Rcpp::List::create(
//     Rcpp::Named("gam") = wrap(outgam.t()),
//     Rcpp::Named("beta") = wrap(outbeta.t()),
//     Rcpp::Named("sigbeta") = wrap(outsb),
//     Rcpp::Named("Sigma") = wrap(outSigma),
//     Rcpp::Named("tar") = wrap(tar.t())
//   );
//
// }
//
//


// Rcpp::List update_betagam_sw_c(const arma::vec X,
//                                const arma::mat Y,
//                                const arma::vec gam1,
//                                const arma::vec beta1,
//                                const arma::mat Sigma,
//                                const arma::rowvec marcor,
//                                const double sigmabeta,
//                                const double Vbeta,
//                                const int bgiter,
//                                const int smallworlditer){
//   int T = gam1.size();
//   arma::mat outgamma = arma::zeros<arma::mat>(T,bgiter);
//   arma::mat outbeta = arma::zeros<arma::mat>(T,bgiter);
//   outgamma.col(0) = gam1;
//   outbeta.col(0) = beta1;
//   arma::vec tar = arma::zeros<arma::vec>(bgiter);
//   for (int i=1; i<bgiter; ++i){
//     Rcpp::List temp = update_gamma_sw_c(X,Y,outgamma.col(i-1), marcor);
//     arma::vec gam1 = outgamma.col(i-1);
//     arma::vec beta1 = outbeta.col(i-1);
//     //small world proposal
//     if(i%30==0){
//       double proposal_ratio = 0;
//       arma::vec betatemp1 = beta1;
//       arma::vec gamtemp1 = gam1;
//       arma::vec betatemp2 = betatemp1;
//       arma::vec gamtemp2 = gamtemp1;
//       for (int j=0; j < smallworlditer; ++j){
//         Rcpp::List temp = update_gamma_sw_c(X,Y,gamtemp1, marcor);
//         arma::vec gamtemp2 = as<arma::vec>(temp["gam"]);
//         arma::vec betatemp2 = betatemp1 % gamtemp2;
//         arma::uvec ind = find(gamtemp2==1);
//         betatemp2(ind) = betatemp1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(Vbeta)));
//         int changeind = temp["changeind"];
//         int change = gamtemp2(changeind);
//         double proposaliter = R::dnorm(betatemp1(changeind)-betatemp2(changeind),
//                                        0,sqrt(Vbeta), true);
//         arma::rowvec marcor2 = min(marcor)+max(marcor)-marcor;
//         if(change==1){
//           arma::uvec ind1 = find(gamtemp1==0);
//           arma::uvec ind2 = find(gamtemp2==1);
//           double tempadd = marcor(changeind)/sum(marcor(ind1));
//           double tempremove = marcor2(changeind)/sum(marcor2(ind2));
//           proposaliter = -log(tempadd)-log(tempremove)-proposaliter;
//         }else{
//           arma::uvec ind1 = find(gamtemp1==1);
//           arma::uvec ind2 = find(gamtemp2==0);
//           double tempadd = marcor(changeind)/sum(marcor(ind2));
//           double tempremove = marcor2(changeind) / sum(marcor2(ind1));
//           proposaliter = log(tempadd)+log(tempremove)+proposaliter;
//         }
//         proposal_ratio = proposal_ratio + proposaliter;
//         gamtemp1 = gamtemp2; betatemp1 = betatemp2;
//       }
//       arma::vec gam2 = gamtemp2;
//       arma::vec beta2 = betatemp2;
//       double newtarget = sum(get_target_c(X,Y,sigmabeta,Sigma,gam2,beta2));
//       double oldtarget = sum(get_target_c(X,Y,sigmabeta,Sigma,gam1,beta1));
//       double A = newtarget-oldtarget + proposal_ratio;
//       arma::vec check2 = runif(1,0,1); double check = check2(0);
//       if(exp(A) > check){
//         tar(i) = newtarget;
//         outgamma.col(i)= gam2;
//         outbeta.col(i) = beta2;
//       }else{
//         tar(i) = oldtarget;
//         outgamma.col(i) = gam1;
//         outbeta.col(i) = beta1;
//       }
//     }else{
//       Rcpp::List temp = update_gamma_sw_c(X,Y,gam1, marcor);
//       arma::vec gam2 = as<arma::vec>(temp["gam"]);
//       arma::vec beta2 = beta1 % gam2;
//       arma::uvec ind = find(gam2==1);
//       beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(Vbeta)));
//       int changeind = temp["changeind"];
//       int change = gam2(changeind);
//       arma::vec A = betagam_accept_c(X,Y,sigmabeta,
//                                      Sigma,Vbeta,
//                                      gam1,beta1,
//                                      gam2,beta2,
//                                      changeind,change);
//       NumericVector check2 = runif(1);
//       double check = check2(0);
//       if(exp(A(0))>check){
//         tar(i) = A(1);
//         outgamma.col(i) = gam2; outbeta.col(i) = beta2;
//       }else{
//         tar(i) = A(2);
//         outgamma.col(i) = gam1; outbeta.col(i) = beta1;
//       }
//     }
//   }
//
//   arma::vec outgamma2 = outgamma.col(bgiter-1);
//   arma::vec outbeta2 = outbeta.col(bgiter-1);
//   return Rcpp::List::create(
//     Rcpp::Named("gam")= outgamma2,
//     Rcpp::Named("beta") = outbeta2,
//     Rcpp::Named("tar") = tar
//   );
// }




// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// Rcpp::List update_betagam_c(const arma::vec X,
//                             const arma::mat Y,
//                             arma::vec gam1,
//                             arma::vec beta1,
//                             const arma::mat Sigma,
//                             const double sigmabeta,
//                             const double Vbeta,
//                             const arma::rowvec marcor,
//                             const int bgiter){
//   //update and beta and gamma 'bgiter' times
//   for (int i=1; i<bgiter; ++i){
//     Rcpp::List temp = update_gamma_c(X,Y,gam1, marcor);
//     arma::vec gam2 = as<arma::vec>(temp["gam"]);
//     arma::vec beta2 = beta1 % gam2;
//     arma::uvec ind = find(gam2==1);
//     beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(Vbeta)));
//     int changeind = temp["changeind"];
//
//     int change = gam2(changeind);
//     arma::vec A = betagam_accept_c(X,Y,sigmabeta,
//                                    Sigma,Vbeta,marcor,
//                                    gam1,beta1,
//                                    gam2,beta2,
//                                    changeind,change);
//     NumericVector check2 = runif(1);
//     double check = check2(0);
//     if(exp(A(0))>check){
//       gam1 = gam2; beta1 = beta2;
//     }
//   }
//   return Rcpp::List::create(
//     Rcpp::Named("gam") = gam1,
//     Rcpp::Named("beta") = beta1
//   );
// }

// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// Rcpp::List update_gamma_c(const arma::vec X,
//                           const arma::mat Y,
//                           const arma::vec gam,
//                           const arma::rowvec marcor){
//   //update gamma once
//   int changeind;
//   arma::vec newgam = gam;
//   int T = gam.size();
//   arma::vec prob = arma::zeros<arma::vec>(2);
//   prob(0) = 0.5; prob(1) = 0.5;
//   arma::uvec ind0 = find(gam==0);
//   arma::uvec ind1 = find(gam==1);
//   int s = ind1.size();
//   NumericVector prob2 = wrap(prob);
//   int cas;
//   if(s==0){
//     cas = 1;
//   }else if(s==T){
//     cas = 2;
//   }else{
//     cas = sample_index(2, prob2);
//   }
//   if (cas==1){
//     int add = 1;
//     if(s<(T-1)){
//       arma::vec mc = marcor(ind0);
//       NumericVector mc2 = wrap(mc);
//       add = sample_index(ind0.size(), mc2);
//     }
//     newgam(ind0(add-1)) = 1;
//     changeind = ind0(add-1);
//   }else{
//     int remove = sample_index(s);
//     newgam(ind1(remove-1)) = 0;
//     changeind = ind1(remove-1);
//   }
//   return(
//     Rcpp::List::create(
//       Rcpp::Named("gam") = newgam,
//       Rcpp::Named("changeind") = changeind)
//   );
// }


//this is for update_betagam_sw_c
// if(i%20000==0){
//   //small world proposal
//   double proposal_ratio = 0;
//   arma::vec betatemp1 = beta1;
//   arma::vec gamtemp1 = gam1;
//   arma::vec betatemp2 = betatemp1;
//   arma::vec gamtemp2 = gamtemp1;
//   for (int j=0; j < smallworlditer; ++j){
//     Rcpp::List temp = update_gamma_sw_c(X,Y,gamtemp1, marcor);
//     arma::vec gamtemp2 = as<arma::vec>(temp["gam"]);
//     int changeind = temp["changeind"];
//     arma::vec betatemp2 = betatemp1 % gamtemp2;
//     arma::uvec ind = find(gamtemp2==1);
//     //betatemp2(ind) = betatemp1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(Vbeta)));
//     int change = gamtemp2(changeind);
//     //double proposaliter = R::dnorm(betatemp1(changeind)-betatemp2(changeind),
//     //                               0,sqrt(Vbeta), true);
//     double proposaliter = 0; //
//     arma::rowvec marcor2 = min(marcor) + max(marcor) - marcor;
//     if(change==1){
//       betatemp2(changeind) = betatemp1(changeind) + (rnorm(1, 0, sqrt(Vbeta)))[0]; //
//       arma::uvec ind1 = find(gamtemp1==0);
//       arma::uvec ind2 = find(gamtemp2==1);
//       double tempadd = marcor(changeind)/sum(marcor(ind1));
//       double tempremove = marcor2(changeind)/sum(marcor2(ind2));
//       proposaliter = -log(tempadd)+log(tempremove)-proposaliter;
//     }else{
//       betatemp2(changeind) = 0; //
//       arma::uvec ind1 = find(gamtemp1==1);
//       arma::uvec ind2 = find(gamtemp2==0);
//       double tempadd = marcor(changeind)/sum(marcor(ind2));
//       double tempremove = marcor2(changeind) / sum(marcor2(ind1));
//       proposaliter = log(tempadd)-log(tempremove)+proposaliter;
//     }
//     proposal_ratio = proposal_ratio + proposaliter;
//     gamtemp1 = gamtemp2; betatemp1 = betatemp2;
//   }
//   gam2 = gamtemp2;
//   beta2 = betatemp2;
//   double newtarget = sum(get_target_c(X,Y,sigmabeta,Sigma,gam2,beta2));
//   double oldtarget = sum(get_target_c(X,Y,sigmabeta,Sigma,gam1,beta1));
//   double A = newtarget - oldtarget + proposal_ratio;
//   arma::vec check2 = runif(1,0,1); double check = check2(0);
//   if(exp(A) > check){
//     gam1 = gam2;
//     beta1 = beta2;
//   }
// }else{
// }

// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// arma::mat em_cov_c(arma::mat y,
//                    const int maxit){
//   int orig_p = y.n_cols;
//   arma::vec vars = arma::zeros<arma::vec>(orig_p);
//   arma::rowvec means = arma::zeros<arma::rowvec>(orig_p);
//   for (int i=0; i < orig_p; ++i){
//     arma::vec ycol = y.col(i);
//     arma::uvec finiteind = find_finite(ycol);
//     arma::vec yy = ycol(finiteind);
//     means(i) = mean(yy);
//     vars(i) = sum((yy-means(i))%(yy-means(i)));
//   }
//   arma::uvec valid_ind = find(vars>1e-6);
//   y = y.cols(valid_ind);
//   int p = y.n_cols;
//   int n = y.n_rows;
//   arma::rowvec mu = arma::zeros<arma::rowvec>(p);
//   arma::mat y_imputed = y;
//   for (int j = 0; j < p; ++j){
//     arma::uvec colind = arma::zeros<arma::uvec>(1);
//     colind(0) = j;
//     arma::uvec nawhere = find_nonfinite(y_imputed.col(j));
//     arma::uvec nonnawhere = find_finite(y_imputed.col(j));
//     arma::vec tempcolmean = mean(y_imputed(nonnawhere, colind), 0);
//     y_imputed(nawhere, colind).fill(tempcolmean(0));
//   }
//   arma::mat oldSigma = y_imputed.t() * y_imputed / n;
//   arma::mat Sigma = oldSigma;
//   double diff = 1;
//   int it = 1;
//   while (diff>0.001 && it < maxit){
//     arma::mat bias = arma::zeros<arma::mat>(p,p);
//     for (int i=0; i<n; ++i){
//       arma::rowvec tempdat = y.row(i);
//       arma::uvec ind = find_finite(tempdat);
//       arma::uvec nind = find_nonfinite(tempdat);
//       if (0 < ind.size() && ind.size() < p){
//         bias(nind, nind) += Sigma(nind, nind) - Sigma(nind, ind) * (Sigma(ind, ind).i()) * Sigma(ind, nind);
//         arma::uvec rowind = arma::zeros<arma::uvec>(1);
//         rowind(0) = i;
//         arma::mat yvec = y(rowind, ind);
//         y_imputed(rowind, nind) = means(nind).t() + (Sigma(nind, ind)*(Sigma(ind, ind).i())*y(rowind, ind).t()).t();
//       }
//     }
//     //update mean
//     for (int j=0; j < orig_p; ++j){
//       arma::vec ycol = y.col(j);
//       arma::uvec finiteind = find_finite(ycol);
//       arma::vec yy = ycol(finiteind);
//       means(j) = mean(yy);
//     }
//     Sigma = cov(y_imputed) + bias/n;
//     arma::mat diffmat = (Sigma-oldSigma);
//     arma::mat diffsq = diffmat%diffmat;
//     diff = accu(diffsq);
//     oldSigma = Sigma;
//     it = it + 1;
//   }
//   arma::mat finalSigma = arma::zeros<arma::mat>(orig_p, orig_p);
//   finalSigma.submat(valid_ind, valid_ind.t()) = Sigma;
//   return finalSigma;
// }
//
// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// arma::vec betagam_accept_c(const arma::vec X,
//                            const arma::mat Y,
//                            const double sigmabeta1,
//                            const arma::mat inputSigma,
//                            const double Vbeta,
//                            const arma::rowvec marcor,
//                            const arma::vec gam1,
//                            const arma::vec beta1,
//                            const arma::vec gam2,
//                            const arma::vec beta2,
//                            const int changeind,
//                            const int change){
//   //compute the target likelihood and the proposal ratio
//   //to decide if you should accept the proposed beta and gamma
//   double newtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam2,beta2));
//   double oldtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam1,beta1));
//   double proposal_ratio = R::dnorm(beta1(changeind)-beta2(changeind),0,sqrt(Vbeta),true);
//   int s1 = sum(gam1==1);
//   int s2 = sum(gam2==1);
//   if(change==1){
//     arma::uvec ind1 = find(gam1==0);
//     double temp1 = marcor(changeind)/sum(marcor(ind1));
//     proposal_ratio = -log(temp1)-log(s2)-proposal_ratio;
//   }else{
//     arma::uvec ind2 = find(gam2==0);
//     double temp2 = marcor(changeind)/sum(marcor(ind2));
//     proposal_ratio = log(temp2)+log(s1)+proposal_ratio;
//   }
//   double final_ratio = newtarget-oldtarget+proposal_ratio;
//   arma::vec out = arma::zeros<arma::vec>(4);
//   out(0) = final_ratio;
//   out(1) = newtarget;
//   out(2) = oldtarget;
//   out(3) = proposal_ratio;
//   return(out);
// }

//
// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// Rcpp::List update_gamma_sw_c(const arma::vec X,
//                              const arma::mat Y,
//                              const arma::vec gam,
//                              const arma::rowvec marcor){
//   int changeind = 0;
//   //flip marcor
//   arma::rowvec marcor2 = max(marcor) + min(marcor) - marcor;
//   arma::vec newgam = gam;
//   int T = gam.size();
//   arma::vec prob = arma::zeros<arma::vec>(2);
//   prob(0) = 0.5; prob(1) = 0.5;
//   arma::uvec ind0 = find(gam==0);
//   arma::uvec ind1 = find(gam==1);
//   int s = ind1.size();
//   NumericVector prob2 = wrap(prob);
//   int cas = sample_index(2, prob2);
//   if(s==0){
//     cas = 1;
//   }else if(s==T){
//     cas = 2;
//   }
//   if (cas==1){
//     int add = 1;
//     if(s<(T-1)){
//       arma::vec mc = marcor(ind0);
//       NumericVector mc2 = wrap(mc);
//       add = sample_index(ind0.size(), mc2);
//     }
//     newgam(ind0(add-1)) = 1;
//     changeind = ind0(add-1);
//   }
//   if(cas==2){
//     int remove=1;
//     if(s > 1){
//       arma::vec mc = marcor2(ind1);
//       NumericVector mc2 = wrap(mc);
//       remove = sample_index(ind1.size(), mc2);
//     }
//     newgam(ind1(remove-1)) = 0;
//     changeind = ind1(remove-1);
//   }
//   return(
//     Rcpp::List::create(
//       Rcpp::Named("gam") = newgam,
//       Rcpp::Named("changeind") = changeind)
//   );
// }
//
// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// Rcpp::List update_betagam_sw_c(const arma::vec X,
//                                const arma::mat Y,
//                                arma::vec gam1,
//                                arma::vec beta1,
//                                const arma::mat Sigma,
//                                const arma::rowvec marcor,
//                                const double sigmabeta,
//                                const double Vbeta,
//                                const int bgiter,
//                                const double smallchange){
//   arma::vec gam2;
//   arma::vec beta2;
//   for (int i=1; i<bgiter; ++i){
//       Rcpp::List temp = update_gamma_sw_c(X,Y,gam1,marcor);
//       arma::vec gam2 = as<arma::vec>(temp["gam"]);
//       int changeind = temp["changeind"];
//       int change = gam2(changeind);
//       arma::vec beta2 = beta1 % gam2;
//       if(change==1){
//         arma::uvec ind = find(gam1==1);
//         beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(smallchange)));
//         beta2(changeind) = beta2(changeind) + (rnorm(1, 0, sqrt(Vbeta)))[0];
//       }else{
//         arma::uvec ind = find(gam2==1);
//         beta2(ind) = beta1(ind) + as<arma::vec>(rnorm(ind.size(), 0, sqrt(smallchange)));
//         beta2(changeind) = 0;
//       }
//       // cout << gam1.t() <<"\n"<< gam2.t() << "\n"<< changeind << "\n" << change << "\n" <<
//       //   beta1.t() << "\n" << beta2.t() << "\n";
//       arma::vec A = betagam_accept_sw_c(X,Y,sigmabeta,
//                                         Sigma,Vbeta,marcor,
//                                         gam1,beta1,
//                                         gam2,beta2,
//                                         changeind,change);
//       NumericVector check2 = runif(1);
//       double check = check2(0);
//       if(exp(A(0)) > check){
//         gam1 = gam2; beta1 = beta2;
//       }
//     }
//   return Rcpp::List::create(
//     Rcpp::Named("gam")= gam1,
//     Rcpp::Named("beta") = beta1
//   );
// }
//
// // [[Rcpp::depends("RcppArmadillo")]]
// // [[Rcpp::export]]
// arma::vec betagam_accept_sw_c(const arma::vec X,
//                               const arma::mat Y,
//                               const double sigmabeta1,
//                               const arma::mat inputSigma,
//                               const double Vbeta,
//                               const arma::rowvec marcor,
//                               const arma::vec gam1,
//                               const arma::vec beta1,
//                               const arma::vec gam2,
//                               const arma::vec beta2,
//                               const int changeind,
//                               const int change){
//   double newtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam2,beta2));
//   double oldtarget = sum(get_target_c(X,Y,sigmabeta1,inputSigma,gam1,beta1));
//   double proposal_iter = R::dnorm(beta1(changeind)-beta2(changeind),0,sqrt(Vbeta),true);
//   arma::rowvec marcor2 = min(marcor)+max(marcor)-marcor;
//   double proposal_ratio = 0;
//   if(change==1){
//     arma::uvec ind1 = find(gam1==0);
//     arma::uvec ind2 = find(gam2==1);
//     double tempadd = marcor(changeind)/sum(marcor(ind1));
//     double tempremove = marcor2(changeind)/sum(marcor2(ind2));
//     proposal_ratio = -log(tempadd)+log(tempremove)-proposal_iter;
//   }else{
//     arma::uvec ind1 = find(gam1==1);
//     arma::uvec ind2 = find(gam2==0);
//     double tempadd = marcor(changeind)/sum(marcor(ind2));
//     double tempremove = marcor2(changeind) / sum(marcor2(ind1));
//     proposal_ratio = log(tempadd)-log(tempremove)+proposal_iter;
//   }
//   double final_ratio = newtarget-oldtarget+proposal_ratio;
//   arma::vec out = arma::zeros<arma::vec>(4);
//   out(0) = final_ratio;
//   out(1) = newtarget;
//   out(2) = oldtarget;
//   out(3) = proposal_ratio;
//   return(out);
// }
