#include "bs.h"

// [[Rcpp::export]]
arma::vec get_underlying_cpp(
    const arma::vec &S, const arma::vec &D, const arma::vec &T,
    const arma::vec &r, const arma::vec &vol, const double tol){
  // assume that all input vecs are equal length
  arma::uword n = S.n_elem;
  arma::vec out(n);

  // starting values
  double V_min = S[0],
         V_max = 100 * S[0] + D[0],
         V_mid =  10 * S[0] + D[0];

  // compute output
  const double *s_i   = S.begin(),
               *d_i   = D.begin(),
               *t_i   = T.begin(),
               *r_i   = r.begin(),
               *vol_i = vol.begin();
        double *o_i   = out.begin();
  for(arma::uword i = 0;
      i < n;
      ++i, ++s_i, ++d_i, ++t_i, ++r_i, ++vol_i, ++o_i){
    *o_i = BS_call_cpp_inv(
      *s_i, *d_i, *t_i, *r_i, *vol_i, tol, V_min, V_max, V_mid);

    // assume that the next answer is the close
    V_mid = *o_i;
    V_max = V_mid * 3;
    V_min = V_mid / 3;
  }

  return out;
}
