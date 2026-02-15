#ifndef MAPFIT_EMFIT_H
#define MAPFIT_EMFIT_H

#include <Rcpp.h>
#include <string>

enum Status {
  Initial,
  Initialized,
  Estimating,
  Convergence,
  MaxIteration
};

struct EMOptions {
  int maxiter;
  double atol;
  double rtol;
  int steps;
  bool verbose;
  enum Status status;
  int iter;
  double llf;
  double aerror;
  double rerror;
  double poisson_eps;
  double ufactor;
  bool stationary;
  double inte_eps;
  int inte_divide;

  EMOptions() :
    maxiter(0),
    atol(0),
    rtol(0),
    steps(0),
    verbose(false),
    status(Initial),
    iter(0),
    llf(0),
    aerror(0.0),
    rerror(0.0),
    poisson_eps(0.0),
    ufactor(0.0),
    stationary(false),
    inte_eps(0.0),
    inte_divide(0) {}
};

template <typename ModelT, typename DataT, typename OptionT, typename ET, typename WorkSpace>
void emfit(ModelT& model, const DataT& data, OptionT& options, ET& eres, WorkSpace& work) {
  options.status = Estimating;
  int iter = 0;
  double prev_llf, llf;
  double aerror = 0.0, rerror = 0.0;
  prev_llf = estep(model, data, eres, options, work);
  if (std::isnan(prev_llf)) {
    throw std::range_error("Fail to estimate parameters.");
  }
  mstep(eres, model, options);
  iter += 1;
  
  if (options.verbose) {
    Rcpp::Rcout << "iter=" << iter << " "
                << "llf=" << prev_llf << std::endl;
  }
  Rcpp::checkUserInterrupt();

  // loop
  while (true) {
    for (int k=0; k < options.steps; k++) {
      llf = estep(model, data, eres, options, work);
      mstep(eres, model, options);
      iter += 1;
    }
    if (std::isnan(llf)) {
      throw std::range_error("Fail to estimate parameters.");
    }
    aerror = llf - prev_llf;
    rerror = std::abs(aerror / prev_llf);
    
    if (aerror < 0.0) {
      Rcpp::warning("Warning: LLF does not increases (iter=%d, llf=%g, diff=%d)", iter, llf, aerror);
    }

    if (options.verbose) {
      Rcpp::Rcout << "iter=" << iter << " "
                  << "llf=" << llf << " "
                  << "(diff=" << aerror << " "
                  << "rerror=" << rerror << ")"<< std::endl;
    }
    Rcpp::checkUserInterrupt();
    
    if (std::abs(aerror) < options.atol && std::abs(rerror) < options.rtol) {
      options.status = Convergence; // convergence
      break;
    }
    
    if (iter >= options.maxiter) {
      options.status = MaxIteration; // max iteration
      break;
    }

    prev_llf = llf;
  }
  options.iter = iter;
  options.llf = llf;
  options.aerror = std::abs(aerror);
  options.rerror = std::abs(rerror);
  return;
}

#endif
