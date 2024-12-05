#define ARMA_64BIT_WORD

// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"
#include "armadillo"
#include "chrono"
#include "numeric"
#include "vector"
#include "iostream"


using namespace Rcpp;
using namespace sugar;
using namespace arma;



// Function to find the index of an element in a character vector
// // [[Rcpp::export]]

Rcpp::NumericVector findIndices_char(Rcpp::CharacterVector vec, const std::string& target) {

  int n = vec.size();
  std::vector<int> indices;  // Use a std::vector to collect indices

  for (int i = 0; i < n; ++i) {
    if (vec[i] == target) {
      indices.push_back(i);
    }
  }

  // Convert std::vector<int> to Rcpp::NumericVector
  Rcpp::NumericVector result(indices.begin(), indices.end());
  return result;
}

// Function to find the index of an element in a numeric vector
// // [[Rcpp::export]]
int findIndex_numeric(const arma::vec& vec, double target) {

  int n = vec.n_elem;  // Use .n_elem to get the number of elements

  for (int i = 0; i < n; ++i) {
    if (vec(i) == target) {
        return i;
    }
  }
  return -1;  // Return -1 if the target is not found
}



// Softmax function implemented using Rcpp
arma::vec softmax_log(arma::vec log_values) {
  // Scale log values to prevent numerical instability
  double max_log_value = arma::max(log_values);
  arma::vec adjusted_values = log_values - max_log_value;

  // Exponentiate the adjusted values
  arma::vec exp_values = arma::exp(adjusted_values);

  // Calculate softmax probabilities
  double sum_exp_values = arma::sum(exp_values);
  arma::vec softmax_probabilities = exp_values / sum_exp_values;

  return softmax_probabilities;
}


#include <unordered_map>

// // [[Rcpp::export]]
double ddirichlet(const arma::vec& x, const arma::vec& alpha) {
  double sum_alpha = arma::accu(alpha);
  double sum_x = arma::accu(x);

  double log_density = lgamma(sum_alpha) - arma::accu(lgamma(alpha)) +
    arma::accu((alpha - 1) % arma::log(x));

  return std::exp(log_density - lgamma(sum_x));
}

// // [[Rcpp::export]]
double log_ddirichlet(const arma::vec& x, const arma::vec& alpha) {
  double sum_alpha = arma::accu(alpha);
  double sum_x = arma::accu(x);

  double log_density = lgamma(sum_alpha) - arma::accu(lgamma(alpha)) +
    arma::accu((alpha - 1) % arma::log(x));

  double log_dist = log_density - lgamma(sum_x);
  return log_dist;
}

//------------------------------------------------------------//
//------------------------------------------------------------//
// Function to run CAVI algorithm (with no feature selection)
//------------------------------------------------------------//
//------------------------------------------------------------//

// [[Rcpp::export]]
Rcpp::List CAVI_algorithm_standard(Rcpp:: List mydata,               // List of R datasets (R is the number of outcome, R>=2)
                          Rcpp:: List data_summary,         // summary of data information
                          Rcpp:: List hyper_parameters,     // hyper-parameters for model
                          Rcpp:: List initial_values,       // initial model parameters
                          const Rcpp:: CharacterVector dist,   // dist
                          const int K,                            // maximum number of clusters
                          const int max_iter,			                // maximum iteration of the VB algorithm
                          const int early_stop,                   // whether to stop the algorithm when converges
                          const int per,                          // print information every per iteration
                          const double epsilon,
                          const double convergence_threshold       // define a convergence threshold for the change in ELBO
) {

  Rcpp::Environment base("package:base");
  Function digamma = base["digamma"];
  Function lfactorial = base["lfactorial"];
  Function lgamma = base["lgamma"];
  Rcpp::Environment stats("package:stats");
  Function dnorm = stats["dnorm"];
  Function dgamma = stats["dgamma"];

  // Data summary values
  const int N = data_summary["N"];
  const int R = data_summary["R"];
  const Rcpp:: List p = data_summary["p"];
  const Rcpp:: List M = data_summary["M"];

  // Hyper-parameters
  const Rcpp:: List mu_0 = hyper_parameters["mu_0"];
  const arma::vec s2_0 = hyper_parameters["s2_0"];
  const arma::vec a_0 = hyper_parameters["a_0"];
  const arma::vec b_0 = hyper_parameters["b_0"];
  const arma::vec u_0 = hyper_parameters["u_0"];
  const arma::vec v_0 = hyper_parameters["v_0"];

  const Rcpp:: List  kappa_0 = hyper_parameters["kappa_0"];
  const arma:: vec   alpha_0 = hyper_parameters["alpha_0"];

  // Load initial values
  arma:: mat  phi = initial_values["phi"];
  arma:: mat  log_phi = log(phi + epsilon);
  arma:: vec  ppi = initial_values["ppi"];
  arma:: vec  alpha = initial_values["alpha"];
  arma:: uvec zz = initial_values["zz"];

  Rcpp:: List omega = initial_values["omega"];
  Rcpp:: List ss = initial_values["ss"];
  Rcpp:: List rho = initial_values["rho"];
  Rcpp:: List omega_tmp = initial_values["omega_tmp"];
  Rcpp:: List e = initial_values["e"];
  Rcpp:: List gamma = initial_values["gamma"];
  Rcpp:: List xi = initial_values["xi"];

  Rcpp:: List mu = initial_values["mu"];
  Rcpp:: List sigma2 = initial_values["sigma2"];
  Rcpp:: List mu_tilde = initial_values["mu_tilde"];
  Rcpp:: List s2_tilde = initial_values["s2_tilde"];
  Rcpp:: List a_tilde = initial_values["a_tilde"];
  Rcpp:: List b_tilde = initial_values["b_tilde"];

  Rcpp:: List theta = initial_values["theta"];
  Rcpp:: List theta_e = initial_values["theta_e"];
  Rcpp:: List kappa = initial_values["kappa"];

  Rcpp:: List lambda = initial_values["lambda"];
  Rcpp:: List lambda_e = initial_values["lambda_e"];
  Rcpp:: List u_tilde = initial_values["u_tilde"];
  Rcpp:: List v_tilde = initial_values["v_tilde"];

  // CAVI algorithm parameters
  // Record the start time
  auto start_time = std::chrono::high_resolution_clock::now();

  int iter = 0;

  arma::vec elbo(max_iter);
  arma::vec EP(max_iter);
  arma::vec EQ(max_iter);
  //arma::umat ZZ(max_iter, N, arma::fill::zeros);

  //Rcpp:: List myvalue(10);

  while (true) {

    iter ++ ;

    // Update cluster assignment
    arma::uvec indices = arma::index_max(phi, 1);
    // Convert to 1-based indexing
    indices += 1;
    zz = indices;
    //ZZ.row(iter - 1) = zz.t();

    //Update cluster proportions (alpha): alpha is a vector of length K
    arma::vec phi_sum = arma::sum(phi, 0).t();
    alpha = alpha_0 + phi_sum;

    //Update the mixing proportion
    ppi = alpha  / arma::sum(alpha);

    for (int i = 0; i < N; ++i) {
      phi.row(i) =   softmax_log(vectorise(log_phi.row(i))).t() ;
    }


    //--------------------------------------------------------------------------//
    // Update Variational Parameters and Calculate ELBO
    //--------------------------------------------------------------------------//
    double E_log_pss = 0;
    double E_log_pX_sum = 0;
    double E_log_pzz = 0;
    double E_log_prior_zz = 0;
    double E_log_prior_mu = 0;
    double E_log_prior_sigma2 = 0;
    double E_log_prior_theta = 0;
    double E_log_prior_lambda = 0;

    double E_qss = 0 ;
    double E_qmu = 0;
    double E_qsigma2 = 0 ;
    double E_qtheta = 0 ;
    double E_qlambda = 0;

  for (int k = 0; k < K; ++k) {

        // cluster assignment parameters
        double E_logpi_k = R:: digamma(alpha(k)) - R:: digamma(sum(alpha));

        arma::vec E_logpx_sum(N);


        for (int r = 0; r < R; ++r) {
          arma:: mat mydata_r = mydata(r);
          int p_r = p(r);

          arma::mat sump(N, p_r, arma::fill::zeros) ;

          //---------------------------------------//
            if (dist(r) == "gaussian") {
               Rcpp::NumericVector indx_tmp = findIndices_char(dist, "gaussian") ;
              int indx =  findIndex_numeric(indx_tmp, r);

              arma::mat mu_mat = mu(indx);
              arma::rowvec mu_0_mat = mu_0(indx);
              arma::mat sigma2_mat = sigma2(indx);
              arma::mat mu_tilde_mat = mu_tilde(indx);
              arma::mat s2_tilde_mat = s2_tilde(indx);

              arma::mat a_tilde_mat = a_tilde(indx);
              arma::mat b_tilde_mat = b_tilde(indx);
              arma::mat a_b_tilde_mat = a_tilde_mat/b_tilde_mat;

              // Update cluster-specific means
             arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

             //s2_tilde_mat.row(k) = 1 / (1 / s2_0(indx) + a_b_tilde_mat.row(k) % (arma::sum(phi_rep, 0)));
             //mu_tilde_mat.row(k) = s2_tilde_mat.row(k) % a_b_tilde_mat.row(k) % (trans(phi.col(k)) * mydata_r)  ;

              s2_tilde_mat.row(k) = 1 / (1 / s2_0(indx) +  (arma::sum(phi_rep, 0)));
              //mu_tilde_mat.row(k) = s2_tilde_mat.row(k) %  (trans(phi.col(k)) * mydata_r)  ;
              mu_tilde_mat.row(k) = s2_tilde_mat.row(k) %  arma::sum(phi_rep % mydata_r, 0)  ;

              mu_tilde(indx) = mu_tilde_mat;
              s2_tilde(indx) = s2_tilde_mat;

              mu_mat.row(k) =  mu_tilde_mat.row(k);
              mu(indx) =  mu_mat;

              // Update cluster-specific variances
              a_tilde_mat.row(k) = a_0(indx) + 0.5 * (arma::sum(phi_rep, 0)) ;
              arma::mat mu_mat_rep = repmat(mu_mat.row(k), N, 1);

              arma::mat s2_tilde_mat_rep = repmat(s2_tilde_mat.row(k), N, 1);
              arma::mat tmp3_mat =  pow(mydata_r - mu_mat_rep,2) + s2_tilde_mat_rep;

              //b_tilde_mat.row(k) = b_0(indx) + 0.5 * trans(phi.col(k)) * ( tmp3_mat);
	             b_tilde_mat.row(k) = b_0(indx) + 0.5 * arma::sum(phi_rep % tmp3_mat, 0);
              sigma2_mat.row(k) =  b_tilde_mat.row(k) / a_tilde_mat.row(k);

              a_tilde(indx) = a_tilde_mat;
              b_tilde(indx) = b_tilde_mat;
              sigma2(indx) =  sigma2_mat;

              // update phi
              arma::mat tmp1 = as<arma::vec>(digamma(a_tilde_mat.row(k) + epsilon)).t()  ;
                arma::mat tmp1_rep = repmat(tmp1, N, 1);
              arma::mat tmp2 = log(b_tilde_mat.row(k)  + epsilon);
                arma::mat tmp2_rep = repmat(tmp2, N, 1);
              arma::mat tmp3 = a_tilde_mat.row(k) / b_tilde_mat.row(k);
                arma::mat tmp3_rep = repmat(tmp3, N, 1);

              sump = 0.5*(tmp1_rep - tmp2_rep - tmp3_rep % tmp3_mat);


              // Calculating ELBO parameters
              E_log_pX_sum += arma::accu(phi_rep % sump);

              arma::mat dnorm_mat =  log(as<arma::vec>(dnorm( _("x") = mu_mat.row(k),
                                                          _("mean") = mu_tilde_mat.row(k),
                                                          _("sd") = sqrt(s2_tilde_mat.row(k)))) + epsilon) ;
              E_qmu += arma::accu(dnorm_mat) ;
              arma::mat dgamma_mat =  log(as<arma::vec>(dgamma(_("x") = 1/sigma2_mat.row(k).t(),
                                                               _("shape") = a_tilde_mat.row(k).t(),
                                                               _("rate") =  b_tilde_mat.row(k).t() )) + epsilon)  ;
              E_qsigma2 +=  arma::accu(dgamma_mat);
              arma::mat dnorm_prior_mat = log(as<arma::vec>(dnorm(_("x") = mu_mat.row(k),
                                                                  _("mean") = mu_0_mat,
                                                                  _("sd") =  sqrt(s2_0(indx)))) + epsilon) ;

              E_log_prior_mu +=  arma::accu(dnorm_prior_mat);
              arma::mat dgamma_prior_mat =  log(as<arma::vec>(dgamma( _("x") = 1/sigma2_mat.row(k),
                                                                      _("shape") = a_0(indx),
                                                                      _("rate") = b_0(indx))) + epsilon) ;
              E_log_prior_sigma2 += arma::accu(dgamma_prior_mat);
            }
            //---------------------------------------//

            if(dist(r) == "multinomial"){
               Rcpp::NumericVector indx_tmp = findIndices_char(dist, "multinomial") ;
              int indx =  findIndex_numeric(indx_tmp, r);

              int M_r = M(indx);
              arma::cube kappa_cube = kappa(indx);
              arma::cube kappa_0_cube = kappa_0(indx);
              arma::cube theta_cube = theta(indx);

              // Update cluster-specific parameters

              arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

              for (int m = 0; m < M_r; ++m) {
                arma::mat kappa_cube_m = kappa_cube.slice(m);
                arma::mat kappa_0_cube_m = kappa_0_cube.slice(m);
                arma::mat theta_cube_m = theta_cube.slice(m);

                arma::mat tmpp0 =  arma::conv_to<arma::mat>::from(mydata_r==(m + 1));
                kappa_cube_m.row(k) = kappa_0_cube_m.row(k) +  arma::sum(tmpp0 % phi_rep, 0);
                  kappa_cube.slice(m) = kappa_cube_m;
                  kappa(indx) = kappa_cube;

                arma::mat tmp_sum = arma::sum(kappa_cube, 2);
                theta_cube_m.row(k) = kappa_cube_m.row(k)/tmp_sum.row(k);
                  theta_cube.slice(m) = theta_cube_m;
                  theta(indx) = theta_cube;

                arma::mat tmpp1 = as<arma::vec> (digamma(kappa_cube_m.row(k)  + epsilon ));
                arma::mat tmpp1_rep = repmat(tmpp1.t(), N, 1);

                arma::mat tmpp2 = as<arma::vec> (digamma(tmp_sum.row(k) + epsilon));
                arma::mat tmpp2_rep = repmat(tmpp2.t(), N, 1);

                sump += tmpp0 % (tmpp1_rep - tmpp2_rep);  // for updating phi
              }

              // Calculating ELBO parameters
              for (int m = 0; m < M_r; ++m) {
                arma::mat kappa_cube_m = kappa_cube.slice(m);
                arma::mat kappa_0_cube_m = kappa_0_cube.slice(m);
                arma::mat theta_cube_m = theta_cube.slice(m);

                arma::mat tmpp7 =  arma::conv_to<arma::mat>::from((mydata_r==(m + 1)));
                arma::mat tmpp8 = as<arma::vec> (digamma(kappa_cube_m.row(k)  + epsilon ));
                  arma::mat tmp_sum = arma::sum(kappa_cube, 2);
                arma::mat tmpp9 = as<arma::vec> (digamma(tmp_sum.row(k) + epsilon));
                arma::mat tmpp8_rep = repmat(tmpp8.t(), N, 1);
                arma::mat tmpp9_rep = repmat(tmpp9.t(), N, 1);

                E_log_pX_sum += arma::accu(phi_rep % (tmpp7 % (tmpp8_rep - tmpp9_rep)))  ;
                E_qtheta += log_ddirichlet(theta_cube_m.row(k).t(), kappa_cube_m.row(k).t());
                E_log_prior_theta +=  log_ddirichlet(theta_cube_m.row(k).t(), kappa_0_cube_m.row(k).t());
              }
            }

            if(dist(r) == "poisson"){
               Rcpp::NumericVector indx_tmp = findIndices_char(dist, "poisson") ;
              int indx =  findIndex_numeric(indx_tmp, r);
              arma:: mat u_tilde_mat = u_tilde(indx);
              arma:: mat v_tilde_mat = v_tilde(indx);
              arma:: mat lambda_mat = lambda(indx);
              arma:: mat lambda_e_mat = lambda_e(indx);

              // update cluster-specific parameters
              arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

              u_tilde_mat.row(k) = u_0(indx) + arma::sum(phi_rep % mydata_r, 0) ; // sum over rows, leading to a vector of length p_r
              v_tilde_mat.row(k) = v_0(indx) + arma::sum(phi_rep, 0) ;

              lambda_mat.row(k) = u_tilde_mat.row(k)/v_tilde_mat.row(k);

              u_tilde(indx) = u_tilde_mat;
              v_tilde(indx) = v_tilde_mat;
              lambda(indx) = lambda_mat;

              // Update variable selection indicators
              arma::mat tmp1 = as<arma::vec>(digamma(u_tilde_mat.row(k) + epsilon)).t()  ;
              arma::mat tmp1_rep = repmat(tmp1, N, 1);
              arma::mat tmp2 = log(v_tilde_mat.row(k) + epsilon) ;
              arma::mat tmp2_rep = repmat(tmp2, N, 1);
              arma::mat tmp3 = u_tilde_mat.row(k)/ v_tilde_mat.row(k)  ;
              arma::mat tmp3_rep = repmat(tmp3, N, 1);
              arma::mat tmp4 = as<arma::mat>(lfactorial(mydata_r)) ;

              sump  =  mydata_r  % (tmp1_rep - tmp2_rep) - tmp3_rep - tmp4; // for updating phi;

              // Calculating ELBO parameters
              E_log_pX_sum +=  arma::accu(phi_rep % sump);

              arma::mat dgamma_mat =  log(as<arma::vec>(dgamma(_("x") = lambda_mat.row(k),
                                                               _("shape") = u_tilde_mat.row(k),
                                                               _("rate") =  v_tilde_mat.row(k) )) + epsilon)  ;
              E_qlambda +=  arma::accu(dgamma_mat);

              arma::mat dgamma_prior_mat =  log(as<arma::vec>(dgamma(_("x") = lambda_mat.row(k),
                                                                     _("shape") = u_0(indx),
                                                                     _("rate") =  v_0(indx) )) + epsilon)  ;
              E_log_prior_lambda += arma::accu(dgamma_prior_mat);
            }

          E_logpx_sum  += arma::sum(sump, 1); // Updating phi
        }

        log_phi.col(k) = E_logpi_k + E_logpx_sum;// Updating phi

        E_log_pzz +=  sum(phi.col(k) * (R::digamma(alpha(k)) - R::digamma(sum(alpha))));  // ELBO for zz

      }

  arma::mat tmp = log_phi - log(phi  + epsilon);
  double E_log_phi  = arma::accu(phi % tmp);
  double E_log_prior_ppi = log_ddirichlet(ppi, alpha_0)  ;

  double E_qppi =  log_ddirichlet(ppi, alpha)  ;
  double E_qzz = arma::accu(phi % log(phi  + epsilon));

  double E_log_p =  E_log_pX_sum +
      E_log_phi + E_log_pzz  + E_log_pss  +
      (E_log_prior_ppi + E_log_prior_zz  + E_log_prior_mu + E_log_prior_sigma2 +
      E_log_prior_theta + E_log_prior_lambda) ;

  double E_q =  E_qzz + E_qppi + E_qss +
			(E_qmu + E_qsigma2  + E_qtheta + E_qlambda);

    EP(iter-1) = E_log_p;
    EQ(iter-1) = E_q;
    elbo(iter-1) = E_log_p -  E_q;

// Print iteration number
   if (iter % per == 0){
      Rprintf("iteration = %i ", iter);
        Rprintf("elbo = %f  \n",elbo(iter-1));
    }
    // Check a condition to break out of the loop
    if (early_stop == 1 && iter > 1){
      if (fabs(elbo(iter-1) - elbo(iter-2)) < convergence_threshold) {
        Rprintf("Converged. Stopping at iteration %i \n", iter);
        break;
      }
      if (early_stop == 0 && iter > max_iter) {
        Rprintf("maximum iteration is reached");
        break;
      }
    }
    if (iter == max_iter){   break;  }

  }

  // Record the end time
  auto end_time = std::chrono::high_resolution_clock::now();

  // Calculate the elapsed time in seconds
  std::chrono::duration<double> elapsed_seconds = end_time - start_time;
  double elapsed_time = elapsed_seconds.count();

  Rcpp::List model_parameters = Rcpp::List::create(
    Rcpp::Named("ppi") = ppi.t(),
    Rcpp::Named("phi") = phi,
    Rcpp::Named("alpha") = alpha,
    Rcpp::Named("zz") = zz,

    //Rcpp::Named("omega") = omega,
    //Rcpp::Named("ss") = ss,
    //Rcpp::Named("rho") = rho,
    //Rcpp::Named("omega_tmp") = omega_tmp,
    //Rcpp::Named("xi") = xi,
    //Rcpp::Named("e") = e,
    //Rcpp::Named("gamma") = gamma,

    Rcpp::Named("mu") = mu,
    Rcpp::Named("sigma2") = sigma2,
    Rcpp::Named("mu_tilde") = mu_tilde,
    Rcpp::Named("s2_tilde") = s2_tilde,
    Rcpp::Named("a_tilde") = a_tilde,
    Rcpp::Named("b_tilde") = b_tilde,

    Rcpp::Named("theta") = theta,
    Rcpp::Named("theta_e") = theta_e,
    Rcpp::Named("kappa") = kappa,
    Rcpp::Named("lambda") = lambda,
    Rcpp::Named("lambda_e") = lambda_e,
    Rcpp::Named("u_tilde") = u_tilde,
    Rcpp::Named("v_tilde") = v_tilde);

  return Rcpp::List::create( Rcpp::Named("elbo") =  elbo,
			     Rcpp::Named("EP") =  EP,
			     Rcpp::Named("EQ") =  EQ,
                             Rcpp::Named("iter") =  iter,
                             Rcpp::Named("max_iter") =  max_iter,
                             Rcpp::Named("cluster") =  zz,
                             Rcpp::Named("dist") =  dist,
                             Rcpp::Named("mydata") =  mydata,
                             Rcpp::Named("K") =  K,
                             Rcpp::Named("initial_values") =  initial_values,
                             Rcpp::Named("data_summary") =  data_summary,
                             Rcpp::Named("hyper_parameters") =  hyper_parameters,
                             Rcpp::Named("model_parameters") =  model_parameters,
                             Rcpp::Named("elapsed_time") =  elapsed_time
  );

}



//------------------------------------------------------------//
//------------------------------------------------------------//
// Function to run CAVI algorithm (with global selection)
//------------------------------------------------------------//
//------------------------------------------------------------//

// [[Rcpp::export]]
Rcpp::List CAVI_algorithm_global(Rcpp:: List mydata,               // List of R datasets (R is the number of outcome, R>=2)
                          Rcpp:: List data_summary,         // summary of data information
                          Rcpp:: List hyper_parameters,     // hyper-parameters for model
                          Rcpp:: List initial_values,       // initial model parameters
                          const Rcpp:: CharacterVector dist,   // dist
                          const int K,                            // maximum number of clusters
                          const int max_iter,			                // maximum iteration of the VB algorithm
                          const int early_stop,                   // whether to stop the algorithm when converges
                          const int per,                          // print information every per iteration
                          const double epsilon,
                          const double convergence_threshold       // define a convergence threshold for the change in ELBO
) {

  Rcpp::Environment base("package:base");
  Function digamma = base["digamma"];
  Function lfactorial = base["lfactorial"];
  Function lgamma = base["lgamma"];
  Rcpp::Environment stats("package:stats");
  Function dnorm = stats["dnorm"];
  Function dgamma = stats["dgamma"];
  Function dmultinom = stats["dmultinom"];
  //Rcpp::Environment MCMCpack("package:MCMCpack");
      //Function ddirichlet = MCMCpack["ddirichlet"];

  // Data summary values
  const int N = data_summary["N"];
  const int R = data_summary["R"];
  const Rcpp:: List p = data_summary["p"];
  const Rcpp:: List M = data_summary["M"];

  // Hyper-parameters
  const Rcpp::List mu_0 = hyper_parameters["mu_0"];
  const arma::vec s2_0 = hyper_parameters["s2_0"];
  const arma::vec a_0 = hyper_parameters["a_0"];
  const arma::vec b_0 = hyper_parameters["b_0"];
  const arma::vec u_0 = hyper_parameters["u_0"];
  const arma::vec v_0 = hyper_parameters["v_0"];

  const Rcpp:: List  kappa_0 = hyper_parameters["kappa_0"];
  const arma:: vec   alpha_0 = hyper_parameters["alpha_0"];

  // Load initial values
  arma:: mat  phi = initial_values["phi"];
  arma:: mat  log_phi = log(phi + epsilon);
  arma:: vec  ppi = initial_values["ppi"];
  arma:: vec  alpha = initial_values["alpha"];
  arma:: uvec zz = initial_values["zz"];

  Rcpp:: List omega = initial_values["omega"];
  Rcpp:: List ss = initial_values["ss"];
  Rcpp:: List rho = initial_values["rho"];

  Rcpp:: List omega_tmp = initial_values["omega_tmp"];
  Rcpp:: List e = initial_values["e"];
  Rcpp:: List gamma = initial_values["gamma"];
  Rcpp:: List xi = initial_values["xi"];

  Rcpp:: List mu = initial_values["mu"];
  Rcpp:: List sigma2 = initial_values["sigma2"];
  Rcpp:: List mu_tilde = initial_values["mu_tilde"];
  Rcpp:: List s2_tilde = initial_values["s2_tilde"];
  Rcpp:: List a_tilde = initial_values["a_tilde"];
  Rcpp:: List b_tilde = initial_values["b_tilde"];

  Rcpp:: List theta = initial_values["theta"];
  Rcpp:: List theta_e = initial_values["theta_e"];
  Rcpp:: List kappa = initial_values["kappa"];

  Rcpp:: List lambda = initial_values["lambda"];
  Rcpp:: List lambda_e = initial_values["lambda_e"];
  Rcpp:: List u_tilde = initial_values["u_tilde"];
  Rcpp:: List v_tilde = initial_values["v_tilde"];

  // CAVI algorithm parameters
  // Record the start time
  auto start_time = std::chrono::high_resolution_clock::now();

  int iter = 0;

  arma::vec elbo(max_iter);
  arma::vec EP(max_iter);
  arma::vec EQ(max_iter);

  //arma::umat ZZ(max_iter, N, arma::fill::zeros);

  Rcpp:: List myvalue(10);


  while (true) {

    iter ++ ;

    // Update cluster assignment
    arma::uvec indices = arma::index_max(phi, 1);
    // Convert to 1-based indexing
    indices += 1;
    zz = indices;
    //ZZ.row(iter - 1) = zz.t();

    //Update cluster proportions (alpha): alpha is a vector of length K
    arma::vec phi_sum = arma::sum(phi, 0).t();
    alpha = alpha_0 + phi_sum;

    //Update the mixing proportion
    ppi = alpha  / arma::sum(alpha);

    for (int i = 0; i < N; ++i) {
      phi.row(i) =   softmax_log(vectorise(log_phi.row(i))).t();
    }

    //--------------------------------------------------------------------------//
    // Update Variational Parameters and Calculate ELBO
    //--------------------------------------------------------------------------//
    double E_log_pss = 0;
    double E_log_pX_sum = 0;
    double E_log_pzz = 0;
    double E_log_prior_zz = 0;
    double E_log_prior_mu = 0;
    double E_log_prior_sigma2 = 0;
    double E_log_prior_theta = 0;
    double E_log_prior_lambda = 0;

    double E_qss = 0 ;
    double E_qmu = 0;
    double E_qsigma2 = 0 ;
    double E_qtheta = 0 ;
    double E_qlambda = 0;

  for (int k = 0; k < K; ++k) {

        // cluster assignment parameters
        double E_logpi_k = R:: digamma(alpha(k)) - R:: digamma(sum(alpha));

        arma::vec E_logpx_sum(N);

        E_log_pzz +=  sum(phi.col(k) * (R::digamma(alpha(k)) - R::digamma(sum(alpha))));  // ELBO for zz

        for (int r = 0; r < R; ++r) {
          arma::mat mydata_r = mydata(r);
          arma::mat omega_r = omega(r);
          arma::mat omega_tmp_r = omega_tmp(r);
          arma::mat xi_r = xi(r);
          arma::mat rho_r = rho(r);
          arma::mat ss_r = ss(r);
          int p_r = p(r);

          arma::mat sump(N, p_r, arma::fill::zeros) ;
          arma::mat sump1 (N, p_r,arma::fill::zeros);
          arma::mat sump2 (N, p_r,arma::fill::zeros);

          //---------------------------------------//
            if (dist(r) == "gaussian") {

               Rcpp::NumericVector indx_tmp = findIndices_char(dist, "gaussian") ;
              int indx =  findIndex_numeric(indx_tmp, r);

              arma::mat mu_mat = mu(indx);
                arma::mat mu_mat_k = mu_mat.row(k);

              arma::rowvec mu_0_mat = mu_0(indx);

              arma::mat sigma2_mat = sigma2(indx);
              arma::mat sigma2_mat_k = sigma2_mat.row(k);

              arma::mat mu_tilde_mat = mu_tilde(indx);// mean of mu
                arma::mat mu_tilde_mat_k = mu_tilde_mat.row(k);

              arma::mat s2_tilde_mat = s2_tilde(indx);// var of mu
                arma::mat s2_tilde_mat_k = s2_tilde_mat.row(k);

              arma::mat a_tilde_mat = a_tilde(indx);
                arma::mat a_tilde_mat_k = a_tilde_mat.row(k);

              arma::mat b_tilde_mat = b_tilde(indx);
                arma::mat b_tilde_mat_k = b_tilde_mat.row(k);

              arma::mat a_b_tilde_mat = a_tilde_mat/b_tilde_mat;
                arma::mat a_b_tilde_mat_k = a_tilde_mat_k/b_tilde_mat_k;

              arma::mat e_mat = e(indx);
              arma::mat gamma_mat = gamma(indx);

              arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

              // Update cluster-specific means
              //s2_tilde_mat_k = 1 / (1 / s2_0(indx) +     arma::sum(phi_rep % omega_r, 0));
             //mu_tilde_mat_k = s2_tilde_mat_k % arma::sum(phi_rep % omega_r % mydata_r, 0);

              s2_tilde_mat_k = 1 / (1 / s2_0(indx) +   a_b_tilde_mat_k % arma::sum(phi_rep % omega_r, 0));
              mu_tilde_mat_k = s2_tilde_mat_k %  a_b_tilde_mat_k %  arma::sum(phi_rep % omega_r % mydata_r, 0)  ;

              mu_tilde_mat.row(k) = mu_tilde_mat_k;
              s2_tilde_mat.row(k) = s2_tilde_mat_k;
              mu_tilde(indx) = mu_tilde_mat;
              s2_tilde(indx) = s2_tilde_mat;

              mu_mat_k =  mu_tilde_mat_k;
              mu_mat.row(k) = mu_mat_k;
              mu(indx) =  mu_mat;

              // Update cluster-specific variances
              a_tilde_mat_k = a_0(indx) + 0.5 * arma::sum(phi_rep % omega_r, 0);
              arma::mat mu_mat_rep = repmat(mu_mat_k, N, 1);

              arma::mat s2_tilde_mat_rep = repmat(s2_tilde_mat_k, N, 1);
              arma::mat tmp3_mat =  pow(mydata_r - mu_mat_rep,2) + s2_tilde_mat_rep;
              b_tilde_mat_k = b_0(indx) + 0.5 * arma::sum(phi_rep % omega_r % tmp3_mat, 0);

              sigma2_mat_k =  b_tilde_mat_k / a_tilde_mat_k;
              a_tilde_mat.row(k) = a_tilde_mat_k;
              b_tilde_mat.row(k) = b_tilde_mat_k;
              sigma2_mat.row(k) = sigma2_mat_k;

              a_tilde(indx) = a_tilde_mat;
              b_tilde(indx) = b_tilde_mat;
              sigma2(indx) =  sigma2_mat;

              // Update variable selection indicators
              arma::mat tmp1 = as<arma::vec>(digamma(a_tilde_mat_k + epsilon)).t()  ;
                arma::mat tmp1_rep = repmat(tmp1, N, 1);
              arma::mat tmp2 = log(b_tilde_mat_k  + epsilon);
                arma::mat tmp2_rep = repmat(tmp2, N, 1);
              arma::mat tmp3 = a_tilde_mat_k / b_tilde_mat_k;
                arma::mat tmp3_rep = repmat(tmp3, N, 1);

              sump = 0.5*(tmp1_rep - tmp2_rep - tmp3_rep % tmp3_mat);

              sump1 = 0.5* phi_rep % (tmp1_rep - tmp2_rep - tmp3_rep % tmp3_mat);
                arma::mat tmp4 = arma::sum(1 - omega_r  + epsilon, 0);
                arma::mat tmp5 = arma::sum((1 - omega_r + epsilon) % mydata_r, 0);
                e_mat =  tmp5/tmp4;
                e(indx) = e_mat;

                arma::mat e_mat_rep = repmat(e_mat, N, 1);
                arma::mat tmp6 = pow(mydata_r - e_mat_rep,2);
                arma::mat tmp7 = arma::sum( (1 - omega_r  + epsilon) % tmp6, 0) ;
                gamma_mat =  tmp4 / tmp7;  // precision = 1/sigma2
                gamma(indx) = gamma_mat;

                arma::mat gamma_mat_rep = repmat(gamma_mat, N, 1);
                arma::mat tmpp1 = log(gamma_mat_rep + epsilon);
              sump2 = 0.5*  phi_rep % (tmpp1 - gamma_mat_rep % tmp6);

              // Update variable selection indicators
              omega_tmp_r = exp(sump1);
              xi_r = exp(sump2);
              arma::mat rho_r_rep = repmat(rho_r, N, 1);
              omega_r  =  (rho_r_rep % exp(sump1)) / (rho_r_rep % exp(sump1) + (1 - rho_r_rep) % exp(sump2));

              // Calculating ELBO parameters
              E_log_pX_sum += arma::accu(phi_rep % omega_r  % sump);

              arma::mat dnorm_mat =  log(as<arma::vec>(dnorm( _("x") = mu_mat.row(k),
                                                          _("mean") = mu_tilde_mat.row(k),
                                                          _("sd") = sqrt(s2_tilde_mat.row(k)))) + epsilon) ;
              E_qmu += arma::accu(dnorm_mat) ;

              arma::mat dgamma_mat =  log(as<arma::vec>(dgamma(_("x") = 1/sigma2_mat.row(k).t(),
                                                               _("shape") = a_tilde_mat.row(k).t(),
                                                               _("rate") =  b_tilde_mat.row(k).t() )) + epsilon)  ;
              E_qsigma2 +=  arma::accu(dgamma_mat);

              arma::mat dnorm_prior_mat = log(as<arma::vec>(dnorm(_("x") = mu_mat.row(k),
                                                                  _("mean") = mu_0_mat,
                                                                  _("sd") =  sqrt(s2_0(indx)))) + epsilon) ;

              E_log_prior_mu +=  arma::accu(dnorm_prior_mat);
              arma::mat dgamma_prior_mat =  log(as<arma::vec>(dgamma( _("x") = 1/sigma2_mat.row(k),
                                                                      _("shape") = a_0(indx),
                                                                      _("rate") = b_0(indx))) + epsilon) ;
              E_log_prior_sigma2 += arma::accu(dgamma_prior_mat);

            }
            //---------------------------------------//

            if(dist(r) == "multinomial"){
               Rcpp::NumericVector indx_tmp = findIndices_char(dist, "multinomial") ;
              int indx =  findIndex_numeric(indx_tmp, r);

              int M_r = M(indx);
              arma::cube kappa_cube = kappa(indx);
              arma::cube kappa_0_cube = kappa_0(indx);
              arma::cube theta_cube = theta(indx);
              arma::cube theta_e_cube = theta_e(indx);

              // Update cluster-specific parameters

              arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

              for (int m = 0; m < M_r; ++m) {

                arma::mat kappa_cube_m = kappa_cube.slice(m);
                arma::mat kappa_0_cube_m = kappa_0_cube.slice(m);
                arma::mat theta_cube_m = theta_cube.slice(m);
                arma::mat theta_e_cube_m = theta_e_cube.slice(m);

                //update kappa
                arma::mat tmpp0 =  arma::conv_to<arma::mat>::from((mydata_r==(m + 1)));
                kappa_cube_m.row(k) = kappa_0_cube_m.row(k) +  arma::sum(tmpp0 % phi_rep % omega_r, 0);
                kappa_cube.slice(m) = kappa_cube_m;
                kappa(indx) = kappa_cube;

                //update theta
                arma::mat tmp_sum = arma::sum(kappa_cube, 2);
                arma::mat tmpp1 = as<arma::vec> (digamma(kappa_cube_m.row(k)  + epsilon ));
                arma::mat tmpp2 = as<arma::vec> (digamma(tmp_sum.row(k) + epsilon));
                theta_cube_m.row(k) = kappa_cube_m.row(k)/tmp_sum.row(k);
                theta_cube.slice(m) = theta_cube_m;
                theta(indx) = theta_cube;

                arma::mat tmpp1_rep = repmat(tmpp1.t(), N, 1);
                arma::mat tmpp2_rep = repmat(tmpp2.t(), N, 1);

                sump += tmpp0 % (tmpp1_rep - tmpp2_rep);  // for updating phi

                sump1 += phi_rep % (tmpp0 % (tmpp1_rep - tmpp2_rep));
                arma::mat tmpp4 = arma::sum((1 - omega_r  + epsilon) % tmpp0, 0);
                arma::mat tmpp5 = arma::sum(1 - omega_r  + epsilon, 0);
                theta_e_cube_m.row(k) =  tmpp4/tmpp5;

                arma::mat theta_e_cube_rep = repmat(theta_e_cube_m.row(k), N, 1);
                sump2 += phi_rep % (tmpp0 % log(theta_e_cube_rep  + epsilon));
                }

              // Update variable selection indicators
              omega_tmp_r = exp(sump1);
              xi_r = exp(sump2);
              arma::mat rho_r_rep = repmat(rho_r, N, 1);
              omega_r  =  (rho_r_rep % exp(sump1)) / (rho_r_rep % exp(sump1) + (1 - rho_r_rep) % exp(sump2));

              // Calculating ELBO parameters and marginal likelihood
              for (int m = 0; m < M_r; ++m) {
                arma::mat tmpp7 =  arma::conv_to<arma::mat>::from((mydata_r==(m + 1)));
                arma::mat tmpp8 = as<arma::vec> (digamma(kappa_cube.slice(m).row(k)  + epsilon ));
                  arma::mat tmp_sum = arma::sum(kappa_cube, 2);
                arma::mat tmpp9 = as<arma::vec> (digamma(tmp_sum.row(k) + epsilon));
                  arma::mat tmpp8_rep = repmat(tmpp8.t(), N, 1);
                  arma::mat tmpp9_rep = repmat(tmpp9.t(), N, 1);

              //E_log_pX_sum += arma::accu(phi_rep % omega_r % (tmpp7 % (tmpp8_rep - tmpp9_rep)))  ;
              E_qtheta += log_ddirichlet(theta_cube.slice(m).row(k).t(), kappa_cube.slice(m).row(k).t());
              E_log_prior_theta +=  log_ddirichlet(theta_cube.slice(m).row(k).t(), kappa_0_cube.slice(m).row(k).t());
              }
              E_log_pX_sum += arma::accu(phi_rep % omega_r  % sump);

            }

             if(dist(r) == "poisson"){
                Rcpp::NumericVector indx_tmp = findIndices_char(dist, "poisson") ;
               int indx =  findIndex_numeric(indx_tmp, r);
               arma:: mat u_tilde_mat = u_tilde(indx);
               arma:: mat v_tilde_mat = v_tilde(indx);
               arma:: mat lambda_mat = lambda(indx);
               arma:: mat lambda_e_mat = lambda_e(indx);

               // update cluster-specific parameters
              arma::mat phi_rep = repmat(phi.col(k), 1, p_r);

              u_tilde_mat.row(k) = u_0(indx) + arma::sum(phi_rep % omega_r % mydata_r, 0) ; // sum over rows, leading to a vector of length p_r
              v_tilde_mat.row(k) = v_0(indx) + arma::sum(phi_rep % omega_r, 0) ;

              lambda_mat.row(k) = u_tilde_mat.row(k)/v_tilde_mat.row(k);

              u_tilde(indx) = u_tilde_mat;
              v_tilde(indx) = v_tilde_mat;
              lambda(indx) = lambda_mat;

               // Update variable selection indicators
               arma::mat tmp1 = as<arma::vec>(digamma(u_tilde_mat.row(k) + epsilon)).t()  ;
                arma::mat tmp1_rep = repmat(tmp1, N, 1);
               arma::mat tmp2 = log(v_tilde_mat.row(k) + epsilon) ;
                arma::mat tmp2_rep = repmat(tmp2, N, 1);
               arma::mat tmp3 = u_tilde_mat.row(k)/ v_tilde_mat.row(k)  ;
               arma::mat tmp3_rep = repmat(tmp3, N, 1);
               arma::mat tmp4 = as<arma::mat>(lfactorial(mydata_r)) ;

               sump  =  mydata_r  % (tmp1_rep - tmp2_rep) - tmp3_rep - tmp4; // for updating phi;

               sump1 = phi_rep % (mydata_r  % (tmp1_rep - tmp2_rep) - tmp3_rep - tmp4) ;

               arma::mat tmp5 = arma::sum((1 - omega_r + epsilon) % mydata_r, 0);
               arma::mat tmp6 = arma::sum(1 - omega_r  + epsilon, 0);
               lambda_e_mat.row(k) =  tmp5/tmp6;

               arma::mat lambda_e_mat_rep = repmat(lambda_e_mat.row(k), N, 1);
               sump2 = phi_rep % (mydata_r  % log(lambda_e_mat_rep + epsilon) - lambda_e_mat_rep -  tmp4);

               // Update variable selection indicators
               omega_tmp_r = exp(sump1);
               xi_r = exp(sump2);

               arma::mat rho_r_rep = repmat(rho_r, N, 1);
               omega_r  =  (rho_r_rep % exp(sump1)) / (rho_r_rep % exp(sump1) + (1 - rho_r_rep) % exp(sump2));

               // Calculating ELBO parameters
               E_log_pX_sum +=  arma::accu(phi_rep % omega_r  % sump);

               arma::mat dgamma_mat =  log(as<arma::vec>(dgamma(_("x") = lambda_mat.row(k),
                                                                _("shape") = u_tilde_mat.row(k),
                                                                _("rate") =  v_tilde_mat.row(k) )) + epsilon)  ;
               E_qlambda +=  arma::accu(dgamma_mat);

               arma::mat dgamma_prior_mat =  log(as<arma::vec>(dgamma(_("x") = lambda_mat.row(k),
                                                                _("shape") = u_0(indx),
                                                                _("rate") =  v_0(indx) )) + epsilon)  ;
               E_log_prior_lambda += arma::accu(dgamma_prior_mat);
             }

          omega_tmp(r) = omega_tmp_r;
          xi(r) = xi_r;
          omega(r) = omega_r;

          E_logpx_sum  += arma::sum(omega_r * sump.t(), 0).t(); // Updating phi

          // Update the variable selection indicators (ss)
          rho(r) = (arma:: sum(omega_r, 0)/N) ;
          arma::uvec ind1 = find(omega_r >= 0.5);
          ss_r.elem(ind1).fill(1);
          arma::uvec ind0 = find(omega_r < 0.5);
          ss_r.elem(ind0).fill(0);
          ss(r) = ss_r;

          // Calculate ELBO for variable selection indicators
          arma::mat log_rho_r = log(rho_r + epsilon);
          arma::mat log_1_rho_r = log( 1 - rho_r + epsilon);

          arma::mat tmp1_mat = omega_r * trans(log_rho_r);
          arma::mat tmp2_mat = (1 - omega_r) * trans(log_1_rho_r);
          E_log_pss +=  arma::accu(tmp1_mat + tmp2_mat);

          arma::mat tmp3_mat = log(omega_tmp_r * trans(rho_r)  + epsilon );
          arma::mat tmp4_mat = log(xi_r * trans(1  - rho_r)  + epsilon );
          arma::mat tmp5_mat = trans(ss_r) * tmp3_mat + trans(1 - ss_r) * tmp4_mat;
          E_qss	+= arma::accu(tmp5_mat);
        }
        log_phi.col(k) = E_logpi_k + E_logpx_sum;// Updating phi
      }

    arma::mat tmp = log_phi - log(phi  + epsilon);
    double E_log_phi  = arma::accu(phi % tmp);
    double E_log_prior_ppi = log_ddirichlet(ppi, alpha_0)  ;

    double E_qppi =  log_ddirichlet(ppi, alpha)  ;
    double E_qzz = arma::accu(phi % log(phi  + epsilon));

    double E_log_p =    E_log_pX_sum +
        E_log_phi +    E_log_pzz  +   E_log_pss  +
        (E_log_prior_ppi + E_log_prior_zz  + E_log_prior_mu + E_log_prior_sigma2 +
        E_log_prior_theta + E_log_prior_lambda) ;

    double E_q =  E_qppi + E_qzz +  E_qss +
		  (E_qmu + E_qsigma2  + E_qtheta + E_qlambda);

    EP(iter-1) = E_log_p;
    EQ(iter-1) = E_q;
    elbo(iter-1) = E_log_p - E_q;

// Print iteration number
   if (iter % per == 0){
        Rprintf("iteration = %i ", iter);
        //Rprintf("elbo = %f ",elbo(iter-1));
        //Rprintf("elbo = %f ", EP(iter-1));
        Rprintf("elbo = %f  \n", elbo(iter-1));
    }
    // Check a condition to break out of the loop
    if (early_stop == 1 && iter > 1){
      if (fabs(elbo(iter-1) - elbo(iter-2)) < convergence_threshold) {
        Rprintf("Converged. Stopping at iteration %i \n", iter);
        break;
      }
      if (early_stop == 0 && iter > max_iter) {
        Rprintf("maximum iteration is reached");
        break;
      }
    }
    if (iter == max_iter){   break;  }

  }

  // Record the end time
  auto end_time = std::chrono::high_resolution_clock::now();

  // Calculate the elapsed time in seconds
  std::chrono::duration<double> elapsed_seconds = end_time - start_time;
  double elapsed_time = elapsed_seconds.count();

Rcpp::List model_parameters = Rcpp::List::create(
    Rcpp::Named("ppi") = ppi.t(),
    Rcpp::Named("phi") = phi,
    Rcpp::Named("alpha") = alpha,
    Rcpp::Named("zz") = zz,

    //Rcpp::Named("omega_tmp") = omega_tmp,
    //Rcpp::Named("xi") = xi,
    //Rcpp::Named("e") = e,
    //Rcpp::Named("gamma") = gamma,

    Rcpp::Named("omega") = omega,
    Rcpp::Named("ss") = ss,
    Rcpp::Named("rho") = rho,

    Rcpp::Named("mu") = mu,
    Rcpp::Named("sigma2") = sigma2,
    Rcpp::Named("mu_tilde") = mu_tilde,
    Rcpp::Named("s2_tilde") = s2_tilde,
    Rcpp::Named("a_tilde") = a_tilde,
    Rcpp::Named("b_tilde") = b_tilde,

    Rcpp::Named("theta") = theta,
    Rcpp::Named("theta_e") = theta_e,
    Rcpp::Named("kappa") = kappa,
    Rcpp::Named("lambda") = lambda,
    Rcpp::Named("lambda_e") = lambda_e,
    Rcpp::Named("u_tilde") = u_tilde,
    Rcpp::Named("v_tilde") = v_tilde);

  return Rcpp::List::create(
			     Rcpp::Named("elbo") =  elbo,
			     Rcpp::Named("EP") =  EP,
			     Rcpp::Named("EQ") =  EQ,
                             Rcpp::Named("iter") =  iter,
                             Rcpp::Named("max_iter") =  max_iter,
                             Rcpp::Named("cluster") =  zz,
                             Rcpp::Named("dist") =  dist,
                             Rcpp::Named("mydata") =  mydata,
                             Rcpp::Named("K") =  K,
                             Rcpp::Named("initial_values") =  initial_values,
                             Rcpp::Named("data_summary") =  data_summary,
                             Rcpp::Named("hyper_parameters") =  hyper_parameters,
                             Rcpp::Named("model_parameters") =  model_parameters,
                             Rcpp::Named("elapsed_time") =  elapsed_time

  );

}

















