#include <Rcpp.h>
using namespace Rcpp;

#include "util/util.h"

namespace util{

double Random::log2pi = log(2*M_PI);

double Random::ahrensDieter(double a)
{
    while (true)
    {
        double u = runif();

        if (u <= M_E / (a+M_E))
        {
            double x = pow( (a+M_E)*u/M_E , 1/a );
            if (runif() <= exp(-x))
                return x;
        }
        else
        {
            double x = -log((a+M_E)*(1-u)/a/M_E);
            if (runif() <= pow(x,a-1))
                return x;
        }
    }
}

double Random::chengFeast(double a)
{
    double b1 = 2.5;
    double b2 = 1.86;

    double c1 = a-1;
    double c2 = (a-1.0/6.0/a)/c1;
    double c3 = 2/c1;
    double c4 = c3+2;
    double c5 = 1/sqrt(a);

    double u = 0;
    double v = 0;
    double w = 0;

    do
    {
        do
        {
            u = runif();
            v = runif();
            if (a > b1)
                u = v + c5 * (1 - b2*u);
        }
        while(u>1 || u < 0);

        w = c2 * v/u;
    }
    while (c3*u+w+1/w > c4 && c3*log(u)-log(w)+w > 1);

    return c1*w;
}


double Random::logdgamma(double x, double a, double b)
{
    return a * log(b) + (a-1) * log(x) - b * x - lgamma(a);
}

double Random::logdbeta(double x, double a, double b)
{
    return (a-1) * log(x) + (b-1) * log(1-x) - lgamma(a) - lgamma(b) + lgamma(a+b);

}

double Random::logddirichlet(int n, double *p, double *x)
{
    double tot = 0;
    double res = 0;

    for (int i=0; i<n; i++)
    {
        tot += p[i];
        res += (p[i]-1) * log(x[i]);
        res -= lgamma(p[i]);
    }

    return res + lgamma(tot);
}

double Random::logdnorm(double x, double m, double s)
{
    return -0.5*log2pi - log(s) - 0.5 * (x-m)*(x-m)/s/s;
}

double Random::logdmillerone(double x, double p, double q, double r, double s)
{
    return lgamma(x*s) - r * lgamma(x) + x*p - x*s* log(q);
}

double Random::logdexp(double x, double lambda)
{
    //return log ( lambda * exp(-lambda * x) );
    return log(lambda) - lambda * x;
}

/**
 Random number generators.
 */
// double Random::runif()
// {
//     return std::rand() / (double) RAND_MAX;
// }

double Random::runif(double a, double b)
{
    return a + runif()*(b-a);
}

double Random::rexp()
{
    return -log(runif());
}

double Random::rexp(double l)
{
    return rexp()/l;
}

double Random::rgamma(double a, double b)
{
    return ( a <= 1 ? ahrensDieter(a) : chengFeast(a) ) /b;
}

double Random::mcmillerone(int n, double a, double sigma, double p, double q, double r, double s)
{
    double x = a;
    double y = 0;

    for (int i=0; i<n; i++)
    {
        y = exp(log(x) + rnorm(0,sigma));
        if (log(runif()) < logdmillerone(y,p,q,r,s) + log(y) -logdmillerone(x,p,q,r,s) - log(x))
            x = y;
    }

    return x;
}

void Random::rmiller(double *ab, double p, double q, double r, double s)
{
    ab[0] = mcmillerone(100,ab[0],0.1,p,q,r,s);
    ab[1] = rgamma(ab[0]*s,q);
}

double Random::rbeta(double a, double b)
{
    double x = rgamma(a,1);
    double y = rgamma(b,1);
    return x/(x+y);
}

void Random::rdirichlet(int n, double *p, double *x)
{
    double tot = 0;
    for (int i=0; i<n; i++)
    {
        x[i] = rgamma(p[i],1);
        tot += x[i];
    }
    for (int i=0; i<n; i++)
        x[i] /= tot;
}

double Random::rchisq(double n)
{
    return rgamma(n/2,0.5);
}

double Random::rnorm()
{
    return sqrt(-2*log(runif())) * sin(2*M_PI*runif());
}

double Random::rnorm(double m, double s)
{
    return m + s * rnorm();
}

double Random::rscaledinvchisq(double nu, double tau2)
{
    return nu * tau2 / rchisq(nu);
}

void Random::rnorminvchisq(double *m, double *s2, double mu, double kappa, double nu, double tau2)
{
    *s2 = rscaledinvchisq(nu,tau2);
    *m = mu + rnorm() * sqrt(*s2/kappa);
}

double Random::rpoisson(double l)
{
    double L = exp(-l);
    double p = 1;
    int k = 0;

    do
    {
        k++;
        p *= runif();
    }
    while (p > L);

    return k - 1;
}

double Random::logdint(int x, int n, double *pi)
{
    double tot = 0;
    for (int i=0; i<n; i++){
        if (pi[i] > 0){
            tot += pi[i];
        }
    }
    double p = pi[x] > 0 ? pi[x] : 0;

    return log(p/tot);
}

int Random::rint(int n, double *pi)
{
    double tot = 0;
    for (int i=0; i<n; i++)
    {
        if (pi[i] > 0)
            tot += pi[i];
    }
    double U = runif() * tot;
    tot = 0;
    for (int i=0; i<n; i++)
    {
        if (pi[i] > 0)
            tot += pi[i];
        if (U <= tot)
            return i;
    }

    return -1;
}


} // namespace util
