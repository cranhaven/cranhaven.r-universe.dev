#ifndef RRandom_h
#define RRandom_h

#include <Rcpp.h>
using namespace Rcpp;

#include "util/util.h"

class RRandom: public util::Random {
public:
  RRandom() : util::Random() {}

    /**
     Random number generators.
     */
    double runif() override;
    double runif(double a, double b) override;
    double rexp() override;
    double rexp(double l) override;
    double rgamma(double a, double b) override;
    double rnorm() override;
    double rnorm(double m, double s) override;
    double rpoisson(double l) override;
/*
    double mcmillerone(int n, double a, double sigma, double p, double q, double r, double s);
    void rmiller(double *ab, double p, double q, double r, double s);
    double rbeta(double a, double b);
    void rdirichlet(int n, double *p, double *x);
    double rchisq(double n);
    double rscaledinvchisq(double nu, double tau2);
    void rnorminvchisq(double *m, double *s2, double mu, double kappa, double nu, double tau2);
    double logdint(int x, int n, double *pi);
    int rint(int n, double *pi);
*/
 };

#endif // RRandom_h
