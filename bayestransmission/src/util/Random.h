// util/Random.h
#ifndef ALUN_UTIL_RANDOM_H
#define ALUN_UTIL_RANDOM_H

#include "Object.h"
#include <stdlib.h>

namespace util{
class Random : public Object
{
private:
	double ahrensDieter(double a);
	double chengFeast(double a);

public:
	// Protected constructor - derived classes must implement their own
	// This class has been made virtual for the R Package implementation.
	Random() {}
	Random(int seed) {}
	
	// Virtual destructor is important for abstract base classes
	virtual ~Random() {}

	static double log2pi;

	// void setSeed(int seed);

/**
	Probability density functions.
*/

	double logdgamma(double x, double a, double b);
	double logdbeta(double x, double a, double b);
	double logddirichlet(int n, double *p, double *x);
	double logdnorm(double x, double m, double s);
	double logdmillerone(double x, double p, double q, double r, double s);
	double logdexp(double x, double lambda);
	double logdint(int x, int n, double *pi);
/**
	Random number generators.
*/
	virtual double runif()=0;
	virtual double runif(double a, double b);
	virtual double rexp();
	virtual double rexp(double l);
	virtual double rgamma(double a, double b);
	virtual double mcmillerone(int n, double a, double sigma, double p, double q, double r, double s);
	virtual void rmiller(double *ab, double p, double q, double r, double s);
	virtual double rbeta(double a, double b);
	virtual void rdirichlet(int n, double *p, double *x);
	virtual double rchisq(double n);
	virtual double rnorm();
	virtual double rnorm(double m, double s);
	virtual double rscaledinvchisq(double nu, double tau2);
	virtual void rnorminvchisq(double *m, double *s2, double mu, double kappa, double nu, double tau2);
	virtual double rpoisson(double l);
	virtual int rint(int n, double *pi);

};
} // namespace util
#endif // ALUN_UTIL_RANDOM_H
