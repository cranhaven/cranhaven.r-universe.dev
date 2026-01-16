#include "RRandom.h"

/**
 Random number generators.
 */
double RRandom::runif(){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::runif(1)[0];
}
double RRandom::runif(double a, double b){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::runif(1, a, b)[0];
}

double RRandom::rexp(){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rexp(1)[0];
}
double RRandom::rexp(double l){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rexp(1, l)[0];
}
double RRandom::rgamma(double a, double b){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rgamma(1, a, b)[0];
}
double RRandom::rnorm(){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rnorm(1)[0];
}
double RRandom::rnorm(double m, double s){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rnorm(1, m, s)[0];
}
double RRandom::rpoisson(double l){
    Rcpp::RNGScope rcpp_rngScope_gen;
    return Rcpp::rpois(1, l)[0];
}
/*
double RRandom::mcmillerone(int n, double a, double sigma, double p, double q, double r, double s);
void RRandom::rmiller(double *ab, double p, double q, double r, double s);
double RRandom::rbeta(double a, double b);
void RRandom::rdirichlet(int n, double *p, double *x);
double RRandom::rchisq(double n);
double RRandom::rscaledinvchisq(double nu, double tau2);
void RRandom::rnorminvchisq(double *m, double *s2, double mu, double kappa, double nu, double tau2);
double RRandom::logdint(int x, int n, double *pi);
int RRandom::rint(int n, double *pi);
*/
