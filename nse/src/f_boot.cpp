#include <Rcpp.h>

using namespace Rcpp;

int f_geodev (double p)
{
  double b, y, z;

  b = (-1.0)/log(1.0-(1/p));
  y = Rcpp::rexp(1)(0);
  z = b*y;
  return std::max(floor(z),1.0);
}

int f_WRAP (int i, int n)
{
  if (i < 1)
    return i%n+n;
  else if (i > n)
    return (i-1)%n+1;
  else
    return i;
}

int f_disuni (int n)
{
  double temp;

  temp = Rcpp::runif(1)(0)*n+1.0;
  return floor(temp);
}


Rcpp::NumericVector f_StatBoot(Rcpp::NumericVector x, double p)
{
  int i, j, I, L;
  int n = x.size()-1;
  Rcpp::NumericVector xBoot(n);
  i = 0;
  while (i < n)
  {
    I = f_disuni(n);
    L = f_geodev(p);
    j = 0;
    while ((j < L) && (i < n))
    {
      xBoot[i] = x[f_WRAP(I+j,n)];

      i++;
      j++;
    }
  }
  return(xBoot);
}

Rcpp::NumericVector f_BlockBoot(Rcpp::NumericVector x, double p)
{
  int i, j, I;
  int n = x.size()-1;
  Rcpp::NumericVector xBoot(n);
  i = 0;
  while (i < n)
  {
    I = f_disuni(n);
    j = 0;
    while ((j < p) && (i < n))
    {
      xBoot[i] = x[f_WRAP(I+j,n)];

      i++;
      j++;
    }
  }
  return(xBoot);
}

// [[Rcpp::export]]
Rcpp::NumericVector f_bootstrap (Rcpp::NumericVector x, double p, int type)
{
  if (type == 0) return(f_StatBoot(x, p));
  else if (type == 1) return(f_BlockBoot(x, p));
  else return(0);
}
