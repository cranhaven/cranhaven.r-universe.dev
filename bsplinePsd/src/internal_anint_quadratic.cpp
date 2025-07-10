#include <Rcpp.h>
using namespace Rcpp;

// C++ functions for analytical integration of QUADRATIC B-splines
// Needed for normalising to QUADRATIC B-spline densities

// AnInQ1 - remember that numbering starts from 0
//' @keywords internal
// [[Rcpp::export]]
double AnInQ1(NumericVector knot) {
            
            double upper, lower, constant, anint;

            upper = knot[1];
            lower = knot[0];

            constant = 1 / ((knot[2] - knot[0]) * (knot[1] - knot[0]));

            anint = constant * ((pow(upper, 3) - pow(lower, 3)) / 3 - knot[0] * (pow(upper, 2) - pow(lower, 2)) +
            pow(knot[0], 2) * (upper - lower));
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnInQ2 
//' @keywords internal
// [[Rcpp::export]]
double AnInQ2(NumericVector knot) {
            
            double upper, lower, constant, anint;

            upper = knot[2];
            lower = knot[1];

            constant = 1 / ((knot[2] - knot[0]) * (knot[2] - knot[1]));

            anint = constant * (-(pow(upper, 3) - pow(lower, 3)) / 3 + (knot[0] + knot[2]) * (pow(upper, 2) - pow(lower, 2)) / 2 -
            (knot[0] * knot[2]) * (upper - lower));
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnInQ3 
//' @keywords internal
// [[Rcpp::export]]
double AnInQ3(NumericVector knot) {
            
            double upper, lower, constant, anint;

            upper = knot[2];
            lower = knot[1];

            constant = 1 / ((knot[3] - knot[1]) * (knot[2] - knot[1]));

            anint = constant * (-(pow(upper, 3) - pow(lower, 3)) / 3 + (knot[1] + knot[3]) * (pow(upper, 2) - pow(lower, 2)) / 2 -
            (knot[1] * knot[3]) * (upper - lower));
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnInQ4
//' @keywords internal
// [[Rcpp::export]]
double AnInQ4(NumericVector knot) {
            
            double upper, lower, constant, anint;

            upper = knot[3];
            lower = knot[2];

            constant = 1 / ((knot[3] - knot[1]) * (knot[3] - knot[2]));

            anint = constant * ((pow(upper, 3) - pow(lower, 3)) / 3 - knot[3] * (pow(upper, 2) - pow(lower, 2)) +
            pow(knot[3], 2) * (upper - lower));
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }
