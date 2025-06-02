//
// Created by madleina on 5/2/25.
//

#ifndef ARMA_INCLUDE_H
#define ARMA_INCLUDE_H

#ifdef USE_RCPP
#include <RcppArmadillo.h> // needs to be included before <Rcpp.h> to avoid compilation error
#include <Rcpp.h>
#else
#include <armadillo>
#endif

#endif // ARMA_INCLUDE_H
