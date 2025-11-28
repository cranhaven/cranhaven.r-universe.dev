#include "matrix_minmax.h"


// col_min_idx: calculate colvec min value index within limited range
arma::uword col_min_idx(const arma::colvec& u, const arma::ucolvec& wlmt) {

  arma::uword min_val_idx;

  // wlmt.size() == 0 ? u.min(min_val_idx) : ( u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.min(min_val_idx);

  } else {

    u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx);

  }

  return min_val_idx;
}

// col_max_idx: calculate colvec max value index within limited range
arma::uword col_max_idx(const arma::colvec& u, const arma::ucolvec& wlmt) {

  arma::uword max_val_idx;

  // wlmt.size() == 0 ? u.max(max_val_idx);: ( u(wlmt).max(max_val_idx); max_val_idx = wlmt(max_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.max(max_val_idx);

  } else {

    u(wlmt).max(max_val_idx); max_val_idx = wlmt(max_val_idx);

  }

  return max_val_idx;
}

// col_min_val: calculate colvec min value within limited range
double col_min_val(const arma::colvec& u, const arma::ucolvec& wlmt) {

  return wlmt.size() > 0 ? u(wlmt).min() : u.min() ;

}

// col_max_val: calculate colvec max value within limited range
double col_max_val(const arma::colvec& u, const arma::ucolvec& wlmt) {

  return wlmt.size() > 0 ? u(wlmt).max() : u.max() ;
}

// col_rgn_val: calculate colvec range = max - min value within limited range3
double col_rgn_val(const arma::colvec& u, const arma::ucolvec& wlmt) {

  return wlmt.size() > 0 ? u(wlmt).max() - u(wlmt).min() : u.max() - u.min() ;
}

