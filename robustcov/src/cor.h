#ifndef COR_H
#define COR_H

#define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
#include "utils.h"
#include "fastCorKendall.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::depends(RcppArmadillo)]]

// functions to be used within C++
double covGK(const vec & x, const vec & y);
double scaleQn(const vec & x);
double scaleMAD(vec x);

//double corSpearman(const vec& x, const vec& y, const bool& consistent);
//double corKendall(const vec& x, const vec& y, const bool& consistent);
//double corQuadrant(const vec& x, const vec& y, const bool& consistent);
double corSpearman(const vec& x, const vec& y);
double corKendall(const vec& x, const vec& y);
double corQuadrant(const vec& x, const vec& y);
double covQn(const vec & x, const vec & y);

#endif
