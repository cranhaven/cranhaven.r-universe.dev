#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double ll_cpp(arma::vec theta, arma::vec gamma, arma::mat delta, arma::mat groups,
                   arma::mat beta, arma::vec m_cat, arma::mat X,
                   bool gamma_penalized, bool delta_penalized, bool theta_penalized,
                   double lambda_in, double lambda_out, double lambda_delta, double lambda_theta, double eps, int mode
                   ) {

  arma::mat exp_gamma = exp(gamma);

  std::size_t I = gamma.n_elem;
  std::size_t V = theta.n_elem;
  std::size_t max_J = max(m_cat);
  std::size_t J;

  // std::cout << "I = " << I << std::endl;
  // std::cout << "V = " << V << std::endl;
  // std::cout << "max_J = " << max_J << std::endl;

  // std::cout << m_cat << std::endl;

  arma::mat delta_group = groups * delta.t();

  arma::cube delta_cube(V,(max_J) * I,1);

  if(mode == 2){
    arma::cube delta_cube_full(V,(max_J) * I,1);
    delta_cube_full.slice(0) = delta_group;
    delta_cube = reshape(delta_cube_full,V,(max_J),I);
  }

  arma::cube elem(V, I, max_J + 1);

  // std::cout << size(elem) << std::endl;

  for (size_t i = 0; i < I; ++i) {
    J = m_cat[i];
    for (size_t v = 0; v < V; ++v) {
      for (size_t j = 0; j < J; ++j) {
        if(mode == 1){
          elem(v, i, j+1) = elem(v, i, j) + (theta(v) - beta(i, j) - delta_group(v, i)) * exp_gamma(i);
        } else {
          elem(v, i, j+1) = elem(v, i, j) + (theta(v) - beta(i, j) - delta_cube(v,j,i)) * exp_gamma(i);
        }
      }
    }
  }

  double ll = 0.0; // log-likelihood

  for (size_t i = 0; i < I; ++i) {
    for (size_t v = 0; v < V; ++v) {
      if(!NumericVector::is_na(X(v, i))){
        ll += elem(v, i, X(v, i));
      }
    }
  }

  // std::cout << "log-likelihood after phase one: " << ll << std::endl;

  // exponentiate element matrix
  elem = exp(elem);

  for (size_t i = 0; i < I; ++i) {
    J = m_cat[i];
    for (size_t v = 0; v < V; ++v) {
      double temp = 0.0;
      // note that elem(v, i, 0) == 0 (1)
      if(!NumericVector::is_na(X(v, i))){
        for (size_t j = 0; j < J + 1; ++j) {
          temp += elem(v, i, j);
        }
        ll -= log(temp);
      }
    }
  }


  if (theta_penalized) {
    ll -= lambda_theta * arma::sum(theta % theta);
  }

  if (gamma_penalized && theta_penalized) {
    ll -= lambda_in * arma::sum(gamma % gamma);
  } else if (gamma_penalized) {
    ll -= lambda_out * arma::sum(gamma % gamma);
  }

  if (delta_penalized) {
    // std::cout << arma::sum(arma::pow(arma::abs(delta), 1 + eps)) << std::endl;
    // std::cout << arma::as_scalar(arma::sum(arma::pow(arma::abs(delta), 1 + eps))) << std::endl;
    ll -= lambda_delta * arma::as_scalar(arma::accu(arma::pow(arma::abs(delta), 1 + eps)));
  }

  //std::cout << -ll << std::endl;



  return -ll;
}

// [[Rcpp::export]]
Rcpp::List grad_cpp(arma::vec theta, arma::vec gamma, arma::mat delta, arma::mat groups,
                   arma::mat beta, arma::vec m_cat, arma::mat X,
                   bool gamma_penalized, bool delta_penalized, bool theta_penalized,
                   double lambda_in, double lambda_out, double lambda_delta, double lambda_theta, double eps, int mode
) {

  arma::mat exp_gamma = exp(gamma);

  std::size_t I = gamma.n_elem;
  std::size_t V = theta.n_elem;
  std::size_t max_J = max(m_cat);
  std::size_t J;
  std::size_t G = groups.n_cols;

  // std::cout << groups.n_cols << std::endl;
  // std::cout << groups.n_rows << std::endl;

  arma::mat X_wo_NA = X;


  for (size_t i = 0; i < I; ++i) {
    for (size_t v = 0; v < V; ++v) {
      if(NumericVector::is_na(X_wo_NA(v,i))){
        X_wo_NA(v,i) = 0;
      }
    }
  }

  // std::cout << "I = " << I << std::endl;
  // std::cout << "V = " << V << std::endl;

  arma::mat delta_group = groups * delta.t();

  arma::cube delta_cube(V,(max_J) * I,1);

  if(mode == 2){
    arma::cube delta_cube_full(V,(max_J) * I,1);
    delta_cube_full.slice(0) = delta_group;
    delta_cube = reshape(delta_cube_full,V,(max_J),I);
  }

  arma::cube Psi(V, I, max_J + 1);

  // std::cout << "in 1" << std::endl;

  for (size_t i = 0; i < I; ++i) {
    J = m_cat[i];
    // std::cout << "J = " << J << std::endl;
    for (size_t v = 0; v < V; ++v) {

        for (size_t j = 0; j < J; ++j) {
          if(!NumericVector::is_na(X(v, i))){
            if(mode == 1){
              Psi(v, i, j+1) = Psi(v, i, j) + (theta(v) - beta(i, j) - delta_group(v, i)) * exp_gamma(i);
            } else {
              Psi(v, i, j+1) = Psi(v, i, j) + (theta(v) - beta(i, j) - delta_cube(v, j, i)) * exp_gamma(i);
            }
          } else {
            Psi(v, i, j+1) = 0;
          }
        }

    }
  }

  // std::cout << "in 2" << std::endl;

  arma::cube exp_Psi = exp(Psi);
  arma::mat cumul_Psi(V, I);
  arma::mat theta_Psi(V, I);
  arma::mat gamma_Psi(V, I);

  for (size_t i = 0; i < I; ++i) {
    J = m_cat[i] + 1;
    for (size_t v = 0; v < V; ++v) {
      if(!NumericVector::is_na(X(v, i))){
        for (size_t j = 1; j < J; ++j) {
          cumul_Psi(v, i) += exp_Psi(v, i, j);
          theta_Psi(v, i) += j * exp_gamma(i) * exp_Psi(v, i, j);
          gamma_Psi(v, i) += Psi(v, i, j) * exp_Psi(v, i, j);
        }
      } else {
        cumul_Psi(v, i) = 0;
        theta_Psi(v, i) = 0;
        gamma_Psi(v, i) = 0;
      }
    }
  }

  // std::cout << "cumul theta gamma" << std::endl;

  arma::cube beta_Psi(V, I, max_J+1);
  arma::cube delta_Psi(V, I, G);

  // NOTE: cumul_Psi needs to be computed by this point
  for (size_t i = 0; i < I; ++i) {
    J = m_cat[i] + 1;
    for (size_t v = 0; v < V; ++v) {
      if(!NumericVector::is_na(X(v, i))){
        for (size_t g = 0; g < G; ++g) {
          for (size_t j = 1; j < J; ++j) {
            delta_Psi(v, i, g) += j * exp_gamma(i) * exp_Psi(v, i, j) * groups(v, g) / (1 + cumul_Psi(v, i));
          }
        }
        // beta
        for (size_t j = J-1; j > 0; --j) {
          beta_Psi(v, i, j-1) = beta_Psi(v, i, j) + exp_gamma(i) * exp_Psi(v, i, j) / (1 + cumul_Psi(v, i));
        }
      }
    }
  }

  // std::cout << "in 3" << std::endl;

  // std::cout << "beta delta" << std::endl;

  //arma::vec t1_theta = X * exp_gamma;
  //arma::vec t2_theta = arma::sum(theta_Psi / (1 + cumul_Psi), 1);
  arma::vec grad_theta = X_wo_NA * exp_gamma - arma::sum(theta_Psi / (1 + cumul_Psi), 1);

  // std::cout << "grad theta" << std::endl;

  arma::mat grad_beta = arma::sum(beta_Psi, 0);

  grad_beta.shed_col(max_J);

  // std::cout << grad_beta << std::endl;

  for (size_t v = 0; v < V; ++v) {
    for (size_t i = 0; i < I; ++i) {
      if(!NumericVector::is_na(X(v, i))){
        J = X(v, i);
        for (size_t j = 0; j < J; ++j) {
          grad_beta(i, j) -= exp_gamma(i);
        }
      }
    }
  }

  // std::cout << "grad beta" << std::endl;
  arma::vec t2_gamma = - arma::sum(gamma_Psi / (1 + cumul_Psi), 0).t();
  arma::vec grad_gamma = - arma::sum(gamma_Psi / (1 + cumul_Psi), 0).t();

  arma::vec t1_gamma(I);

  for (size_t i = 0; i < I; ++i) {
    for (size_t v = 0; v < V; ++v) {
      if(!NumericVector::is_na(X(v, i))){
        t1_gamma(i) += Psi(v, i, X(v, i));
        grad_gamma(i) += Psi(v, i, X(v, i));
      }
    }
  }

  // std::cout << "grad gamma" << std::endl;
  // std::cout << "t1 gamma" << std::endl;
  // std::cout << t1_gamma << std::endl;
  //
  // std::cout << "t2 gamma" << std::endl;
  // std::cout << t2_gamma << std::endl;

  // std::cout << "in 4" << std::endl;

  arma::mat grad_delta_temp = - arma::diagmat(exp_gamma) * X_wo_NA.t();
  arma::vec grad_delta_temp2 = arma::vectorise(arma::sum(delta_Psi, 0));
  arma::mat grad_delta_temp3 = arma::sum(delta_Psi, 0);

  arma::mat grad_delta = arma::vectorise(- arma::diagmat(exp_gamma) * X_wo_NA.t() * groups) + arma::vectorise(arma::sum(delta_Psi, 0));

  // std::cout << "in 5" << std::endl;


  // arma::mat grad_delta = - arma::diagmat(exp_gamma) * X.t() * groups + arma::vectorise(arma::sum(delta_Psi, 0));

  // arma::cube temp_delta = arma::sum(delta_Psi, 0);

  // std::cout << "t1 delta" << std::endl;
  // std::cout << arma::diagmat(exp_gamma) * X.t() * groups << std::endl;
  //
  // std::cout << "t2 delta" << std::endl;
  // std::cout << arma::sum(delta_Psi, 0) << std::endl;

  // std::cout << "grad delta" << std::endl;
  // std::cout << temp_delta.subcube(arma::span(0), arma::span(), arma::span(0)) << std::endl;

  if (theta_penalized) {
    grad_theta -= 2 * lambda_theta * theta;
  }

  // std::cout << "penalty theta" << std::endl;
  // std::cout << grad_theta << std::endl;


  // std::cout << "grad gamma" << std::endl;

  if (gamma_penalized && theta_penalized) {
    grad_gamma -= 2 * lambda_in * gamma;
  } else if (gamma_penalized) {
    grad_gamma -= 2 * lambda_out * gamma;
  }

  // std::cout << "penalty term" << std::endl;
  // std::cout << 2 * lambda_in * gamma;

  // std::cout << "penalty gamma" << std::endl;
  // std::cout << grad_gamma << std::endl;



  if (delta_penalized) {
    // grad_delta -= lambda_delta * (1 + eps) * (arma::sign(delta) % arma::pow(arma::abs(delta), eps));
    grad_delta -= lambda_delta * (1 + eps) * arma::vectorise((arma::sign(delta)) % (arma::pow(arma::abs(delta), eps)));
  }

  // std::cout << "in 6" << std::endl;


  return Rcpp::List::create(
    Rcpp::Named("grad_theta") = grad_theta,
    Rcpp::Named("grad_beta") = grad_beta.t().as_col(),
    Rcpp::Named("grad_gamma") = grad_gamma,
    Rcpp::Named("grad_delta") = grad_delta.t().as_col()
  );

  // return -arma::join_cols(grad_theta, grad_beta.t().as_col(), grad_gamma, grad_delta.t().as_col());
  // return -grad_delta;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//



/*** R

  source('../R/gpcmdif.R')
  source('../R/pjmle.R')
  source('../R/loglik_function.R')
  # loglik_fast <- function() {

  # generate example to work with
  setting <- autoRasch::autoRaschOptions()
  setting$isHessian <- FALSE

  dset <- autoRasch::polydif_inh_dset[,c(14:17,19)]
  V <- nrow(autoRasch::polydif_inh_dset)
  X <- as.matrix(dset)

  groups_map <- c(rep(1, V/2),rep(0, V/2))

  dataPrep <- data_prep(dset = dset, fixed_par = setting$fixed_par, groups_map = groups_map)

  I <- ncol(dataPrep$dset)
  J <- dataPrep$n_th
  G <- ncol(dataPrep$groups_map)

  set.seed(2021) # for reproducibility
  theta <- runif(V,-1,1) * setting$random.init.th
  beta <- runif(I*J,-1,1) * setting$random.init.th
  beta_mat <- matrix(beta, I, J, byrow = TRUE) # maybe we should agree on by column instead
  gamma <- runif(I,-1,1) * setting$random.init.th
  delta <- runif(I * G, -1, 1) * setting$random.init.th
  delta_mat <- matrix(delta, I, G, byrow = TRUE)

  # NOTE: this whole part dealing with the fixed parameters has to be written separately
  nlmPar <- c(theta,beta,gamma,delta)
  length_theta <- length(theta)
  length_beta <- length(beta)
  length_gamma <- length(gamma)
  length_delta <- length(delta)
  length_array <- c(length_theta,length_beta,length_gamma,length_delta)
  fullPar_arr <- c("theta","beta","gamma","delta")

  if(is.null(setting$fixed_par)){
    estPar_arr <- fullPar_arr
  } else {
    estPar_arr <- fullPar_arr[-c(which(fullPar_arr %in% setting$fixed_par))]
  }
  fixLength_arr <- length_array[c(which(fullPar_arr %in% setting$fixed_par))]

  fixValue <- c()
  estLength_array <- length_array

  if(!identical(grep("delta",setting$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((sum(length_array[1:3])+1):(sum(length_array[1:4])))]
    estLength_array <- estLength_array[-c(4)]
    if(!is.null(setting$fixed_delta)){
      fixValue <- c(setting$fixed_delta,fixValue)
    } else {
      fixValue <- c(rep(0,(ncol(dset)*ncol(dataPrep$groups_map))),fixValue)
    }
  }
  if(!identical(grep("^gamma",setting$fixed_par), integer(0))){
    if(length(nlmPar) == length(length_array)){
      nlmPar <- nlmPar[-c((sum(length_array[1:2])+1):(sum(length_array[1:3])),(sum(length_array[1:4])+1):(sum(length_array[1:5])))]
      estLength_array <- estLength_array[-c(3,5)]
      if(!is.null(setting$fixed_gamma)){
        fixValue <- c(setting$fixed_gamma, rep(0,(ncol(dset)*ncol(dataPrep$groups_map))), fixValue)
      } else {
        fixValue <- c(rep(0,ncol(dset)), rep(0,(ncol(dset)*ncol(dataPrep$groups_map))), fixValue)
      }
    } else {
      nlmPar <- nlmPar[-c((sum(length_array[1:2])+1):(sum(length_array[1:3])))]
      estLength_array <- estLength_array[-c(3)]
      if(!is.null(setting$fixed_gamma)){
        fixValue <- c(setting$fixed_gamma, fixValue)
        # print(setting$fixed_gamma)
      } else {
        fixValue <- c(rep(0,ncol(dset)), fixValue)
      }
    }
  }
  if(!identical(grep("^beta",setting$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((sum(length_array[1])+1):(sum(length_array[1:2])))]
    estLength_array <- estLength_array[-c(2)]
    fixValue <- c(setting$fixed_beta, fixValue)
  }
  if(!identical(grep("theta",setting$fixed_par), integer(0))){
    nlmPar <- nlmPar[-c((1):(sum(length_array[1])))]
    estLength_array <- estLength_array[-c(1)]
    fixValue <- c(setting$fixed_theta, fixValue)
  }

  # original implementation
  loglik_benchmark <- function() {
    loglik_fun(nlmPar, dataPrep$dset, setting$lambda_theta, setting$lambda_in, setting$lambda_out, setting$lambda_delta,
               estPar_arr, setting$fixed_par, fixValue, fixLength_arr, estLength_array,
               dataPrep$groups_map, dataPrep$mt_vek, dataPrep$mt_idx, dataPrep$dimResp,
               dataPrep$allcat, dataPrep$n_th, dataPrep$XN, dataPrep$XNA, setting$eps,
               setting$isPenalized_gamma, setting$isPenalized_theta, setting$isPenalized_delta)
  }
  # rewritten implementation
  loglik_novel <- function() {
    loglik_fun_novel(nlmPar, dataPrep$dset, setting$lambda_theta, setting$lambda_in, setting$lambda_out, setting$lambda_delta,
                   estPar_arr, setting$fixed_par, fixValue, fixLength_arr, estLength_array,
                   dataPrep$groups_map, dataPrep$mt_vek, dataPrep$mt_idx, dataPrep$dimResp,
                   dataPrep$allcat, dataPrep$n_th, dataPrep$XN, dataPrep$XNA, setting$eps,
                   setting$isPenalized_gamma, setting$isPenalized_theta, setting$isPenalized_delta)
  }
  # fast Rcpp implementation
  loglik_fast <- function() {
    loglik_fun_fast(nlmPar, dataPrep$dset, setting$lambda_theta, setting$lambda_in, setting$lambda_out, setting$lambda_delta,
               estPar_arr, setting$fixed_par, fixValue, fixLength_arr, estLength_array,
               dataPrep$groups_map, dataPrep$mt_vek, dataPrep$mt_idx, dataPrep$dimResp,
               dataPrep$allcat, dataPrep$n_th, dataPrep$XN, dataPrep$XNA, setting$eps,
               setting$isPenalized_gamma, setting$isPenalized_theta, setting$isPenalized_delta)
    # ll_cpp(theta, gamma, delta_mat, dataPrep$groups_map, beta_mat, dataPrep$mt_vek, X,
    #             setting$isPenalized_gamma, setting$isPenalized_delta, setting$isPenalized_theta,
    #             setting$lambda_in, setting$lambda_out, setting$lambda_delta, setting$lambda_theta, setting$eps)
  }

  stopifnot(all.equal(loglik_benchmark(), loglik_novel()))
  stopifnot(all.equal(loglik_benchmark(), loglik_fast()))

  # microbenchmark::microbenchmark(loglik_benchmark(), loglik_novel(), loglik_fast())
  # rbenchmark::benchmark(loglik_benchmark(), loglik_novel(), loglik_fast())

  # original implementation
  grad_benchmark <- function() {
    grad_fun(nlmPar = nlmPar,
             dset = dataPrep$dset,
             lambda_theta = setting$lambda_theta, lambda_in = setting$lambda_in,lambda_out = setting$lambda_out, eps = setting$eps,
             lambda_delta = setting$lambda_delta, estPar_arr = estPar_arr, estLength_array = estLength_array,
             fixLength_arr = fixLength_arr, allcat = dataPrep$allcat, dimResp = dataPrep$dimResp, n_th = dataPrep$n_th, XN = dataPrep$XN, XNA = dataPrep$XNA, #XREAL = dataPrep$XREAL,
             groups_map = dataPrep$groups_map, mt_vek = dataPrep$mt_vek, mt_idx = dataPrep$mt_idx, fixed_par = setting$fixed_par, fixValue = fixValue,
             isPenalized_gamma = setting$isPenalized_gamma, isPenalized_theta = setting$isPenalized_theta,
             isPenalized_delta = setting$isPenalized_delta)
  }
  # rewritten implementation
  grad_novel <- function() {
    grad_fun_novel(nlmPar = nlmPar,
                 dset = dataPrep$dset,
                 lambda_theta = setting$lambda_theta, lambda_in = setting$lambda_in,lambda_out = setting$lambda_out, eps = setting$eps,
                 lambda_delta = setting$lambda_delta, estPar_arr = estPar_arr, estLength_array = estLength_array,
                 fixLength_arr = fixLength_arr, allcat = dataPrep$allcat, dimResp = dataPrep$dimResp, n_th = dataPrep$n_th, XN = dataPrep$XN, XNA = dataPrep$XNA, #XREAL = dataPrep$XREAL,
                 groups_map = dataPrep$groups_map, mt_vek = dataPrep$mt_vek, mt_idx = dataPrep$mt_idx, fixed_par = setting$fixed_par, fixValue = fixValue,
                 isPenalized_gamma = setting$isPenalized_gamma, isPenalized_theta = setting$isPenalized_theta,
                 isPenalized_delta = setting$isPenalized_delta)
  }

  grad_fast <- function() {
    grad_fun_fast(nlmPar = nlmPar,
                   dset = dataPrep$dset,
                   lambda_theta = setting$lambda_theta, lambda_in = setting$lambda_in,lambda_out = setting$lambda_out, eps = setting$eps,
                   lambda_delta = setting$lambda_delta, estPar_arr = estPar_arr, estLength_array = estLength_array,
                   fixLength_arr = fixLength_arr, allcat = dataPrep$allcat, dimResp = dataPrep$dimResp, n_th = dataPrep$n_th, XN = dataPrep$XN, XNA = dataPrep$XNA, #XREAL = dataPrep$XREAL,
                   groups_map = dataPrep$groups_map, mt_vek = dataPrep$mt_vek, mt_idx = dataPrep$mt_idx, fixed_par = setting$fixed_par, fixValue = fixValue,
                   isPenalized_gamma = setting$isPenalized_gamma, isPenalized_theta = setting$isPenalized_theta,
                   isPenalized_delta = setting$isPenalized_delta)
  }

  stopifnot(all.equal(grad_novel(), grad_benchmark(), check.attributes = FALSE))
  stopifnot(all.equal(grad_fast(), grad_benchmark(), check.attributes = FALSE))

  # microbenchmark::microbenchmark(grad_benchmark(), grad_novel(), grad_fast())
  # rbenchmark::benchmark(grad_benchmark(), grad_novel(), grad_fast())

*/


