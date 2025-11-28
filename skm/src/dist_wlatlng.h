#ifndef __DIST_WLATLNG__
#define __DIST_WLATLNG__


#include <algorithm>

#include <RcppArmadillo.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
// using namespace RcppParallel;


//' dist_wlatlng_cpp
//'
//' @description
//'
//'  calculate distance between coordinate1<lat1, lng1> and coordinate2<lat2, lng2>
//'
//' @details
//'
//'  calculate the great circle distance between 2 points with Haversine formula,
//'  which deliberately ignores elevation differences.
//'
//'  Haversine formula (from R.W. Sinnott, "Virtues of the Haversine",
//'  Sky and Telescope, vol. 68, no. 2, 1984, p. 159):
//'
//'  dlon = lon2 - lon1
//'
//'  dlat = lat2 - lat1
//'
//'  a = sin^2(dlat/2) + cos(lat1) * cos(lat2) * sin^2(dlon/2)
//'
//'  c = 2 * arcsin(min(1,sqrt(a)))
//'
//'  d = R * c
//'
//' @param lat1 latitude of coordinate1
//'
//' @param lng1 longitude of coordinate1
//'
//' @param lat2 latitude of coordinate2
//'
//' @param lng2 longitude of coordinate2
//'
//' @param measure "mi" (mile) or "km" (kilometer)
//'
//' @name dist_wlatlng_cpp


// dist_wlatlng_mi_cpp
//' @details
//'  dist_wlatlng_mi_cpp:
//'
//'   calculate distance between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in mile
//' @rdname dist_wlatlng_cpp
// [[Rcpp::export]]
double dist_wlatlng_mi_cpp(double lat1, double lng1, double lat2, double lng2);


// dist_wlatlng_km_cpp
//' @details
//'  dist_wlatlng_km_cpp:
//'
//'   calculate distance between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in kilometer
//' @rdname dist_wlatlng_cpp
// [[Rcpp::export]]
double dist_wlatlng_km_cpp(double lat1, double lng1, double lat2, double lng2);


// distSgl_wlatlng_cpp
//' @details
//'  distSgl_wlatlng_cpp:
//'
//'   calculate distance between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in
//'   mile (measure = "mi") or kilometer (measure = "km"), default is mile.
//'
//'   implement as serial computing over vector of lat1, lng1, lat2, lng2
//' @rdname dist_wlatlng_cpp
// [[Rcpp::export]]
arma::vec distSgl_wlatlng_cpp(arma::vec lat1, arma::vec lng1, arma::vec lat2, arma::vec lng2, std::string measure = "mi");


// distRpl extend RcppParallel::Worker for paralleFor calls
// struct distRpl : public RcppParallel::Worker;


// distRpl_wlatlng_cpp
//' @details
//'  distRpl_wlatlng_cpp:
//'
//'   calculate distance between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in
//'   mile (measure = "mi") or kilometer (measure = "km"), default is mile.
//'
//'   implement as parallel computing over vector of lat1, lng1, lat2, lng2 via RcppParallel
//' @param distRpl_GS The grain size of a parallel algorithm sets a minimum chunk size for parallelization.
//'  In other words, at what point to stop processing input on separate threads (as sometimes creating more
//'  threads can degrade the performance of an algorithm by introducing excessive synchronization overhead).
//'  Default is 100.
//' @rdname dist_wlatlng_cpp
// [[Rcpp::export]]
NumericVector distRpl_wlatlng_cpp(
  NumericVector lat1, NumericVector lng1, NumericVector lat2, NumericVector lng2, std::string measure = "mi", int distRpl_GS = 100
);


#endif // __DIST_WLATLNG__
