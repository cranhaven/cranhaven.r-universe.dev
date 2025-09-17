#ifndef _fourierh
#define _fourierh


#include <RcppArmadillo.h>
#include "function_class.h"
#include "fourier_basis.h"

using namespace Rcpp;
using namespace arma;


//[[Rcpp::export]]
SEXP init_fourier_basis(const arma::vec& range, int order, bool trig_basis = false) {
  
  if (order < 1) stop("Fourier order must be strictly positive!"); // Ændret 24-10-2017
  if (range.n_elem > 2) Rf_warning("Only the first and second elements of range will be used");
  
  if (trig_basis) {
    fourier_basis_trig *ff = new fourier_basis_trig(range(0), range(1), order);
    XPtr<fourier_basis_trig> ff_ptr(ff, true);
    return ff_ptr;
  }
  else {
    fourierBasis *ff = new fourierBasis(range(0), range(1), order);
    XPtr<fourierBasis> ff_ptr(ff, true);
    return ff_ptr;
  }
  
}

#endif
