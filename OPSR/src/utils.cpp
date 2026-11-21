#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
List opsr_prepare_coefs(NumericVector& theta, int nReg) {
  CharacterVector theta_names = theta.names();
  NumericVector gamma, kappa, sigma, rho;
  std::vector<NumericVector> beta(nReg);  // a vector of beta vectors for each regime

  kappa.push_back(R_NegInf);  // first element of kappa is -Inf

  for (int i = 0; i < theta.size(); i++) {
    std::string name = as<std::string>(theta_names[i]);  // for substr method

    if (name.substr(0, 2) == "s_") {
      gamma.push_back(theta[i]);
    } else if (name.substr(0, 5) == "kappa") {
      kappa.push_back(theta[i]);
    } else if (name.substr(0, 5) == "sigma") {
      sigma.push_back(theta[i]);
    } else if (name.substr(0, 3) == "rho") {
      rho.push_back(theta[i]);
    } else if (name[0] == 'o' && std::isdigit(name[1]) && name[2] == '_') {
      // extract beta elements, based on the regime (e.g., o1_, o2_, etc.)
      int regime = name[1] - '0' - 1;  // convert '1', '2', etc. to 0-based index
      beta[regime].push_back(theta[i]);
    }
  }

  kappa.push_back(R_PosInf);  // last element of kappa is Inf

  // construct the list of lists for each ordinal category
  List theta_(nReg);
  for (int i = 0; i < nReg; i++) {
    theta_[i] = List::create(
      Named("gamma") = gamma,
      Named("kappa_j_1") = kappa[i],
      Named("kappa_j") = kappa[i + 1],
      Named("beta_j") = beta[i],
      Named("sigma_j") = sigma[i],
      Named("rho_j") = rho[i]
    );
  }
  return theta_;
}

Theta make_theta(arma::colvec gamma, double kappa_j_1, double kappa_j,
                 arma::colvec beta_j, double sigma_j, double rho_j) {
  Theta theta = {
    gamma, kappa_j_1, kappa_j, beta_j, sigma_j, rho_j
  };

  return theta;
}

Theta* make_theta_array(List theta) {
  int nReg = theta.size();
  List theta_j;
  Theta* theta_array = new Theta[nReg];
  for (int j = 0; j < nReg; j++) {
    theta_j = theta[j];
    theta_array[j] = make_theta(theta_j["gamma"], theta_j["kappa_j_1"], theta_j["kappa_j"],
                                theta_j["beta_j"], theta_j["sigma_j"], theta_j["rho_j"]);
  }
  return theta_array;
}

void free_theta_array(Theta* theta_array) {
  delete[] theta_array;
}

// [[Rcpp::export]]
bool opsr_check_omp() {
#ifdef _OPENMP
  return true;
#else
  return false;
#endif
}

// [[Rcpp::export]]
int opsr_max_threads() {
#ifdef _OPENMP
  return omp_get_max_threads();
#else
  return 1;
#endif
}

