#include <Rcpp.h>
using namespace Rcpp;

// C++ functions for analytical integration of CUBIC B-splines
// Needed for normalising to CUBIC B-spline densities

// AnIn1 - remember that numbering starts from 0
//' @keywords internal
// [[Rcpp::export]]
double AnIn1(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = 1 / (knot[1] - knot[0]);  
            m2 = 1 / (knot[2] - knot[0]);  
            m3 = 1 / (knot[3] - knot[0]);  
            
            c1 = -knot[0] * m1;  
            c2 = -knot[0] * m2;
            c3 = -knot[0] * m3;
            
            anint = (c1 * c2 * c3) * (knot[1] - knot[0]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[1], 2) - pow(knot[0], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[1], 3) - pow(knot[0], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[1], 4) - pow(knot[0], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn2
//' @keywords internal
// [[Rcpp::export]]
double AnIn2(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = -1 / (knot[2] - knot[1]);  
            m2 = 1 / (knot[2] - knot[0]);
            m3 = 1 / (knot[3] - knot[0]); 
            
            c1 = -knot[2] * m1;  
            c2 = -knot[0] * m2;  
            c3 = -knot[0] * m3;
            
            anint = (c1 * c2 * c3) * (knot[2] - knot[1]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[2], 2) - pow(knot[1], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[2], 3) - pow(knot[1], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[2], 4) - pow(knot[1], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }            
            
            return anint;
            
            }

// AnIn3
//' @keywords internal
// [[Rcpp::export]]
double AnIn3(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = 1 / (knot[2] - knot[1]);  
            m2 = -1 / (knot[3] - knot[1]);
            m3 = 1 / (knot[3] - knot[0]); 
            
            c1 = -knot[1] * m1;  
            c2 = -knot[3] * m2;  
            c3 = -knot[0] * m3;
            
            anint = (c1 * c2 * c3) * (knot[2] - knot[1]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[2], 2) - pow(knot[1], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[2], 3) - pow(knot[1], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[2], 4) - pow(knot[1], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn4
//' @keywords internal
// [[Rcpp::export]]
double AnIn4(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = 1 / (knot[2] - knot[1]);  
            m2 = 1 / (knot[3] - knot[1]);
            m3 = -1 / (knot[4] - knot[1]); 
            
            c1 = -knot[1] * m1;
            c2 = -knot[1] * m2;  
            c3 = -knot[4] * m3;
            
            anint = (c1 * c2 * c3) * (knot[2] - knot[1]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[2], 2) - pow(knot[1], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[2], 3) - pow(knot[1], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[2], 4) - pow(knot[1], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn5
//' @keywords internal
// [[Rcpp::export]]
double AnIn5(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = -1 / (knot[3] - knot[2]);  
            m2 = -1 / (knot[3] - knot[1]);
            m3 = 1 / (knot[3] - knot[0]);
            
            c1 = -knot[3] * m1;  
            c2 = -knot[3] * m2;  
            c3 = -knot[0] * m3;
            
            anint = (c1 * c2 * c3) * (knot[3] - knot[2]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[3], 2) - pow(knot[2], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[3], 3) - pow(knot[2], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[3], 4) - pow(knot[2], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn6
//' @keywords internal
// [[Rcpp::export]]
double AnIn6(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = -1 / (knot[3] - knot[2]);
            m2 = 1 / (knot[3] - knot[1]);
            m3 = -1 / (knot[4] - knot[1]); 
            
            c1 = -knot[3] * m1;
            c2 = -knot[1] * m2; 
            c3 = -knot[4] * m3;
            
            anint = (c1 * c2 * c3) * (knot[3] - knot[2]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[3], 2) - pow(knot[2], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[3], 3) - pow(knot[2], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[3], 4) - pow(knot[2], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn7
//' @keywords internal
// [[Rcpp::export]]
double AnIn7(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = 1 / (knot[3] - knot[2]);  
            m2 = -1 / (knot[4] - knot[2]);
            m3 = -1 / (knot[4] - knot[1]); 
            
            c1 = -knot[2] * m1;  
            c2 = -knot[4] * m2;  
            c3 = -knot[4] * m3;
            
            anint = (c1 * c2 * c3) * (knot[3] - knot[2]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[3], 2) - pow(knot[2], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[3], 3) - pow(knot[2], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[3], 4) - pow(knot[2], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }

// AnIn8
//' @keywords internal
// [[Rcpp::export]]
double AnIn8(NumericVector knot) {
            
            double c1, c2, c3, m1, m2, m3, anint;
            
            m1 = -1 / (knot[4] - knot[3]);  
            m2 = -1 / (knot[4] - knot[2]);
            m3 = -1 / (knot[4] - knot[1]); 
            
            c1 = -knot[4] * m1;  
            c2 = -knot[4] * m2;  
            c3 = -knot[4] * m3;
            
            anint = (c1 * c2 * c3) * (knot[4] - knot[3]) + 
            ((c2 * c3 * m1 ) + (c1 * c3 * m2) + (c1 * c2 * m3)) * (pow(knot[4], 2) - pow(knot[3], 2)) / 2 +
            ((c3 * m1 * m2) + (c2 * m1 * m3) + (c1 * m2 * m3)) * (pow(knot[4], 3) - pow(knot[3], 3)) / 3 +
            (m1 * m2 * m3) * (pow(knot[4], 4) - pow(knot[3], 4)) / 4;
            
            if (std::isnan(anint)) {
            anint = 0;
            }
            
            return anint;
            
            }
