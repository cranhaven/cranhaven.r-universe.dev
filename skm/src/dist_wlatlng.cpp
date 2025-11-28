#include "dist_wlatlng.h"


// dist_wlatlng_mi_cpp: dist between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in mile
double dist_wlatlng_mi_cpp(double lat1, double lng1, double lat2, double lng2) {

  // earth radium in 3956 mi or 6367 km
  return 3956.00 * 2 *
    std::asin(std::min(1.00, std::sqrt((std::pow(std::sin((lat2 - lat1) / 2 * arma::datum::pi/180), 2) +
    std::cos(lat1 * arma::datum::pi/180) * std::cos(lat2 * arma::datum::pi/180) * std::pow(std::sin((lng2 - lng1)/2 * arma::datum::pi/180), 2)))));

}

// dist_wlatlng_km_cpp: dist between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in kilometer
double dist_wlatlng_km_cpp(double lat1, double lng1, double lat2, double lng2) {

  // earth radium in 3956 mi or 6367 km
  return 6367.00 * 2 *
    std::asin(std::min(1.00, std::sqrt((std::pow(std::sin((lat2 - lat1) / 2 * arma::datum::pi/180), 2) +
    std::cos(lat1 * arma::datum::pi/180) * std::cos(lat2 * arma::datum::pi/180) * std::pow(std::sin((lng2 - lng1)/2 * arma::datum::pi/180), 2)))));

}

// distSgl_wlatlng_cpp: dist between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in mile mi or kilometer km, mile is default
//  implement as serial computing over vector of lat1, lng1, lat2, lng2
arma::vec distSgl_wlatlng_cpp(arma::vec lat1, arma::vec lng1, arma::vec lat2, arma::vec lng2, std::string measure) {

  arma::vec d(lat1.size());

  if ( measure.compare("km") == 0 ) {

    for (arma::uword i = 0; i < lat1.size(); i++) {

      d(i) = dist_wlatlng_km_cpp(lat1(i), lng1(i), lat2(i), lng2(i));

    }

  } else {

    for (arma::uword i = 0; i < lat1.size(); i++) {

      d(i) = dist_wlatlng_mi_cpp(lat1(i), lng1(i), lat2(i), lng2(i));

    }


  }


  return d;
}


// distRpl extend RcppParallel::Worker for paralleFor calls
struct distRpl : public RcppParallel::Worker {

  // lat1 lng1 lat2 lng2 - input vectors
  const RcppParallel::RVector<double> lat1;
  const RcppParallel::RVector<double> lng1;
  const RcppParallel::RVector<double> lat2;
  const RcppParallel::RVector<double> lng2;

  // d - output vector distance
  RcppParallel::RVector<double> d;

  // m - measure: 0 - mi or 1 - km
  const RcppParallel::RVector<int> m;

  // .constructor convert input/output into RMatrix/RVector type for RcppParallel
  distRpl(const NumericVector& lat1, const NumericVector& lng1,
          const NumericVector& lat2, const NumericVector& lng2,
          NumericVector& d, const IntegerVector& m)
    : lat1(lat1), lng1(lng1), lat2(lat2), lng2(lng2), d(d), m(m) {}

  // parallel calls to dist_wlatlng_cpp
  void operator()(std::size_t begin, std::size_t end) {

    // -> going to be a ParallelFor!
    if ( m[0] == 1 ) {

      for (std::size_t i = begin; i < end; i++) {

        d[i] = dist_wlatlng_km_cpp(lat1[i], lng1[i], lat2[i], lng2[i]);;

      }

    } else {

      for (std::size_t i = begin; i < end; i++) {

        d[i] = dist_wlatlng_mi_cpp(lat1[i], lng1[i], lat2[i], lng2[i]);;

      }

    }

  }

};

// distRpl_wlatlng_cpp: dist between coordinate1<lat1, lng1> and coordinate2<lat2, lng2> in mile mi or kilometer km, mile is default
//  implement as parallel computing over vector of lat1, lng1, lat2, lng2 via RcppParallel
NumericVector distRpl_wlatlng_cpp(
  NumericVector lat1, NumericVector lng1, NumericVector lat2, NumericVector lng2, std::string measure, int distRpl_GS
) {


  // allocate memory space for output vector d
  NumericVector d(lat1.size());

  IntegerVector m(1, 0); m[0] = (measure.compare("km") == 0) ? 1 : 0;

  distRpl a_distRpl(lat1, lng1, lat2, lng2, d, m);

  parallelFor(0, lat1.size(), a_distRpl, distRpl_GS);

  return d;

}

