#include "header.hpp"

sexp default_spline(double &t, sexp &time_vec, sexp &par_vec) {

  if(etr::length(time_vec) == 0) {
    Rcpp::stop("time vector is empty");
  }

  int idx0, idx1, idx2, idx3;
  double t0, t1, t2, t3;
  double y0, y1, y2, y3;

  idx0 = 0;
  idx1 = 0;
  idx2 = 0;
  idx3 = 0;
  t0 = t1 = t2 = t3 = 0.;
  y0 = y1 = y2 = y3 = 0.;

  if (t < time_vec[0]) {
    return par_vec[0];
  } else if (t > time_vec[time_vec.size() - 1]) {
    return par_vec[par_vec.size() - 1];
  }

  for (size_t i = 0; i <= time_vec.size(); i++) {

    if (i == (time_vec.size() - 1)) {

      idx0 = time_vec.size() - 2;
      t0 = time_vec[idx0];
      y0 = par_vec[idx0];

      idx1 = time_vec.size() - 1;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];

      idx2 = time_vec.size() - time_vec.size();
      t2 = time_vec[idx2];
      y2 = par_vec[idx2];

      idx3 = time_vec.size() + 1 - time_vec.size();
      t3 = time_vec[idx3];
      y3 = par_vec[idx3];
      break;

    } else if (t >= time_vec[i] && t < time_vec[i + 1]) {

      if (i == 0) {
        idx0 = time_vec.size() - 1;
        t0 = time_vec[idx0];
      } else {
        idx0 = i - 1;
        t0 = time_vec[idx0];
      }

      y0 = par_vec[idx0];
      idx1 = i;
      t1 = time_vec[idx1];
      y1 = par_vec[idx1];

      if (i == time_vec.size() - 1) {
        idx2 = 0;
        t2 = time_vec[idx2] + time_vec[etr::length(time_vec) - 1];
      } else {
        idx2 = i + 1;
        t2 = time_vec[idx2];
      }
      y2 = par_vec[idx2];

      if (i == time_vec.size() - 2) {
        idx3 = 0;
        t3 = time_vec[idx3] + time_vec[etr::length(time_vec) - 1];
      } else if (i == time_vec.size() - 1) {
        idx3 = 1;
        t3 = time_vec[idx3] + time_vec[etr::length(time_vec) - 1];
      } else {
        idx3 = i + 2;
        t3 = time_vec[idx3];
      }
      y3 = par_vec[idx3];
      break;
    }

  } // search for the beginning of the interpolation intervall

  double x = (t - t1) / (t2 - t1);
  double m1 = (y2 - y0) / (t2 - t0);
  double m2 = (y3 - y1) / (t3 - t1);

  double res = ((2. * pow(x, 3) - 3. * pow(x, 2) + 1.) * y1 +
                (pow(x, 3) - 2. * pow(x, 2) + x) * (t2 - t1) * m1 +
                (-2. * pow(x, 3) + 3. * pow(x, 2)) * y2 +
                (pow(x, 3) - pow(x, 2)) * (t2 - t1) * m2);
  return res;
}

sexp default_error_fct(double num_points, double a, double b) {
  return std::abs((a - b) / b) / num_points;
}

sexp mock_jac(double &t, sexp &, sexp &, sexp &, sexp &) {
  Rcpp::stop("something went wrong. Mock jacobian is called!");
}

// [[Rcpp::export]]
Rcpp::XPtr<error_calc_fct> get_default_error_fct() {
  return Rcpp::XPtr<error_calc_fct>(new error_calc_fct(&default_error_fct));
}

// [[Rcpp::export]]
Rcpp::XPtr<spline_fct> get_default_spline_fct() {
  return Rcpp::XPtr<spline_fct>(new spline_fct(&default_spline));
}

// [[Rcpp::export]]
Rcpp::XPtr<JAC> get_mock_jac_fct() {
  return Rcpp::XPtr<JAC>(new JAC(&mock_jac));
}
