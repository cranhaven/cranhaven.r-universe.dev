#include "utils.h"

arma::vec diff(const arma::vec &x){
  arma::vec out(x.n_elem - 1L);
  const double *x_i = x.begin();
  for(auto it = out.begin(); it != out.end(); ++it, ++x_i)
    *it = *(x_i + 1) - *x_i;

  return out;
}
