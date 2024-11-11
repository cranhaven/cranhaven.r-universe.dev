#include "WpProj_types.h"
#include <vector>
#include "SufficientStatistics.h"
#include "utils.h"
#include "oem_xtx.h"
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.h"
// #include "updateLambda.h"
#include "transport.h"
// #include <thread>
// #include <chrono>
// 
// void sleep(){
//   std::this_thread::sleep_for (std::chrono::seconds(1));
// }


using namespace Rcpp;

//[[Rcpp::export]]
SEXP W2penalized(SEXP X_,
                 SEXP Y_,
                 SEXP theta_,
                 SEXP family_,
                 SEXP penalty_,
                 SEXP groups_,
                 SEXP unique_groups_,
                 SEXP group_weights_,
                 SEXP lambda_,
                 SEXP nlambda_,
                 SEXP lmin_ratio_,
                 SEXP alpha_,
                 SEXP gamma_,
                 SEXP tau_,
                 SEXP scale_factor_,
                 SEXP penalty_factor_,
                 SEXP opts_)
{
  
  
  const matMap X(as<matMap >(X_));
  // const matMap Y_copy(as<matMap >(Y_));
  // const matMap theta_copy(as<matMap >(theta_));
  
  matrix Y = Rcpp::as<matMap >(Y_);//Y_copy;
  matrix theta = Rcpp::as<matMap >(theta_); //theta_copy;
  
  const int S = Y.cols();
  const int p = X.rows();
  const int N = X.cols();
  
  matrix mu =  theta.transpose() * X; //start with ''true'' mu
  
  matrix xtx = matrix::Zero(p, p);
  matrix xty = matrix::Zero(p, 1); //may be resized in suff stat function
  
  vector scale_factor(as<vector>(scale_factor_));
  const vectorI groups(as<vectorI>(groups_));
  const vectorI unique_groups(as<vectorI>(unique_groups_));
  
  
  // In glmnet, we minimize
  //   1/(2n) * ||y - X * beta||^2 + lambda * ||beta||_1
  // which is equivalent to minimizing
  //   1/2 * ||y - X * beta||^2 + n * lambda * ||beta||_1
  vector group_weights(as<vector>(group_weights_));
  
  
  std::vector<vector> lambda(as< std::vector<vector> >(lambda_));
  
  vector lambda_tmp;
  lambda_tmp = lambda[0];
  
  int nl = as<int>(nlambda_);
  vector lambda_base(nl);
  
  int nlambda = lambda_tmp.size();
  
  List opts(opts_);
  const int maxit        = as<int>(opts["maxit"]);
  const double tol       = as<double>(opts["tol"]);
  const double alpha     = as<double>(alpha_);
  const double gamma     = as<double>(gamma_);
  const double tau       = as<double>(tau_);
  const int infm_maxit   = as<int>(opts["infm_maxit"]);
  const bool display_progress = as<bool>(opts["display_progress"]);
  CharacterVector method(as<CharacterVector>(opts["method"]));
  std::string transport_method = as<std::string>(opts["transport_method"]);
  const int model_size   = as<int>(opts["model_size"]);
  const bool not_same    = as<bool>(opts["not_same"]);
  double epsilon    = as<double>(opts["epsilon"]);
  double OTmaxit    = as<int>(opts["OTmaxit"]);
  const bool same = !(not_same);
  bool selection;
  if(method(0) == "selection.variable") {
    selection = true;
  } else {
    selection = false;
  }
  // const double pseudo_obs = as<double>(opts["pseudo_observations"]);
  
  CharacterVector family(as<CharacterVector>(family_));
  std::string penalty(as< std::string >(penalty_));
  vector penalty_factor(as<vector>(penalty_factor_));
  
  // matrix xty_temp = matrix::Zero(p,N);
  // const double pseudo_obs = as<double>(pseudo_obs_);
  // const double wt = double(pseudo_obs)/(double(N) + double(pseudo_obs));
  // matrix theta_norm = matrix::Zero(p,1);
  
  //fill xtx and xty and theta_norm. Also resize theta if location scale method
  // sufficient_stat(X, Y,
  //                 theta,
  //                 true, //true=not the same
  //                 S, p, N,
  //                 wt,
  //                 xtx, xty, theta_norm,
  //                 method); //Y will be sorted by column if using scale or loc.scale
  sufficient_stat(X, Y,
                  theta,
                  not_same, //true=not the same
                  S, p, N,
                  xtx, xty,
                  method,
                  transport_method,
                  epsilon,
                  OTmaxit); //Y will be sorted by column if using scale or loc.scale
  // Rcout << xty << std::endl;
  // Rcout << xtx << std::endl;
  // Rcout << theta_norm;
  matrix xty_old = xty;
  // matrix xtx_old = xtx;
  
  //order indices of x * theta
  matrixI idx_mu(S,N);
  
  // Rcpp::Rcout << "xtx: " << xtx.rows() << ", " << xtx.cols() <<"\n";
  // Rcpp::Rcout << "xty: " << xty.rows() << ", " << xty.cols() <<"\n";
  // Rcpp::Rcout << "X: " << X.rows() << ", " << X.cols() <<"\n";
  // Rcpp::Rcout << "Y: " << Y.rows() << ", " << Y.cols() <<"\n";
  // Rcpp::Rcout << "S: " << S <<"\n";
  
  //change scale factor to make estimation easier, let's say
  if ( scale_factor.size() == 0 ) {
    // if ( (scale_factor.size() == 0) && (penalty != "selection.lasso")  ) {
    scale_factor.resize(xtx.cols());
    scale_factor = xtx.diagonal();
    scale_factor.noalias() = scale_factor.cwiseSqrt();
  } //else if ( (penalty == "selection.lasso")  && (scale_factor.size() != 0)) {
  //   scale_factor.fill(1.0);
  //   scale_factor.resize(0);
  // }
  
  // if(method(0) != "projection"){
  //   sort_matrix(Y);
  // }
  
  // initialize pointers
  oemBase_gen<matrix> *solver = NULL; // solver doesn't point to anything yet
  
  // initialize classes
  if (family(0) == "gaussian")
  {
    // Rcpp::Rcout << groups <<"\n";
    // sleep();
    solver = new oemXTX_gen(xtx, xty, groups, unique_groups,
                            group_weights, penalty_factor,
                            scale_factor, selection, tol);
  } else {
    if (family(0) == "binomial")
    {
      throw std::invalid_argument("binomial not available for oem_fit_dense, use oem_fit_logistic_dense");
      //solver = new oem(X, Y, penalty_factor, irls_tol, irls_maxit, eps_abs, eps_rel);
    }
  }
  
  // initialize oem
  solver->init_oem();
  
  // generate lambda vector
  double lmax = 0.0;
  lmax = solver->compute_lambda_zero(penalty); //
  
  bool provided_lambda = false;
  std::string elasticnettxt(".net");
  bool is_net_pen = penalty.find(elasticnettxt) != std::string::npos;
  
  if (nlambda < 1) {
    double lmin = as<double>(lmin_ratio_) * lmax;
    
    // lambda_base.setLinSpaced(nl, std::log(lmin), std::log(lmax));
    lambda_base.setLinSpaced(nl, std::log(lmax), std::log(lmin));
    lambda_base = lambda_base.array().exp();
    // lambda_base(0) = 0.0;
    // lambda_base(nl-1) = 0.0;
    nlambda = lambda_base.size();
    
    lambda_tmp.resize(nlambda);
    
    if (is_net_pen)
    {
      lambda_tmp = (lambda_base.array() / alpha).matrix(); // * n; //
    } else
    {
      lambda_tmp = lambda_base; // * n; //
    }
    
  } else {
    provided_lambda = true;
    lambda_tmp = lambda[0];
  }
  
  // results matrix
  matrix beta = matrix::Zero(xty.size(), nlambda);
  
  // matrix to store values to see if permutations converge
  matrix old( xty.rows(), xty.cols() );
  
  //set initial value of the coefficients to a large number so won't converge on first iteration
  old.fill(1e6);
  
  // diagnostic checks
  IntegerMatrix niter( infm_maxit, nlambda );
  IntegerVector innerIter( nlambda );
  double ilambda = 0.0;
  // double num_tol = Eigen::NumTraits<double>::dummy_precision();
  
  vector loss(nlambda);
  loss.fill(1e99);
  innerIter.fill(0);
  
  // progress bar
  if(display_progress) {
    Rcpp::Rcout << "\n";
  }
  ETAProgressBar pb;
  Progress prog( nlambda, display_progress, pb );
  
  
  // if (method(0) == "projection") {
  //   lambda_tmp.reverseInPlace();
  // }
  
  // const matrix xty_original = xty;
  // vector best_lambda(p);
  
  
  for (int i = 0; i < nlambda; i++)
  {
    // vectors to save current and last value of coefficients
    matrix res(xty.rows(), xty.cols());
    
    // if (i % 10 == 0) {
      Rcpp::checkUserInterrupt();
    // }
    
    //set current lambda
    ilambda = lambda_tmp(i);
    // Rcpp::Rcout << ilambda <<"\n";
    if(i == 0) {
      //intitalize with lambda and other parameters
      solver->init(ilambda, penalty,
                   alpha, gamma, tau);
      // if(method(0) != "projection") solver->beta_ones();
    } else {
      solver->init_warm(ilambda);
    }
    
    for(int j = 0; j < infm_maxit; j++) {
      
      //save number of sorting iterations
      innerIter[i] += 1;
      
      if (j % 10 == 0) {
        Rcpp::checkUserInterrupt();
      }
      
      //save number of optimizing iterations
      niter(j,i)     = solver->solve(maxit);
      
      //get new solution based on current xty
      res = solver->get_beta();

      //check if iterations have converged
      if (stopRuleMat(res, old, tol)) {
        break;
      } else {
        old = res;
      } //fi stopRule
      if (nonZero(res) && ilambda > 0 && method(0) != "projection") {
        //update mu and xty with some sorting for W2
        // void xty_update(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted for method = scale/loc.scale
        //                 const refMatConst & theta,
        //                 const refMatConst & result,
        //                 matrix & mu,
        //                 const int S, const int N, const int P,
        //                 matrix & xty, matrixI & idx_mu,
        //                 const Rcpp::CharacterVector & method,
        //                 const std::string & transport_method)
        xty_update(X, Y, theta, res, mu, S, N, p, xty, idx_mu, 
                   method, transport_method,
                   epsilon, OTmaxit);
        
        //update solver with new xty
        solver->init_warm_xty(); // still maps to original xty
      } // fi sorting
    } // end loop alternating ordering and optimization
    
    
    // save coefficient otherwise
      beta.col(i)  = res;
      
    // break if larger than max coef
    if( countNonZero(res) > model_size) {
      beta.conservativeResize(Eigen::NoChange, i+1);
      lambda_tmp.conservativeResize(i+1);
      break;
    }
    
    //update progress bar
    // if(display_progress){
      prog.increment();
    // }
    // if (innerIter[i] > 1 && same && method(0) != "projection") {
    //   xty = xty_old;
    //   solver->init_warm_xty();
    // } //really slows things down!
  } //end loop over lambda values
  
  double d = solver->get_d();
  
  delete solver;
  solver = NULL;
  
  // if (method(0) != "projection") {
  //   lambda_tmp.reverseInPlace();
  // }
  
  return Rcpp::List::create(Named("beta")       = Rcpp::wrap(beta),
                            Named("lambda")     = lambda_tmp,
                            Named("niter")      = niter,
                            Named("innerIter")  = innerIter,
                            Named("loss")       = loss,
                            Named("d")          = d,
                            Named("xtx")        = Rcpp::wrap(xtx),
                            Named("xty")        = Rcpp::wrap(xty_old),
                            Named("xtyFinal")   = Rcpp::wrap(xty));
  
}

