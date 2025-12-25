#include <Rcpp.h>
using namespace Rcpp;
#include "sampleW.h"
#include "checkconstraints.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

void sampleG_imp(int* data, int* HHdata, int* ni, int* cum_ni, int n, double* phi, int maxdd,int p,
                 double* omega, int FF, int SS, std::vector<NumericMatrix>& Lambdas, double* pi,
                 double* rand, int* group, int* indi, int begin, int end) {
  int count = cum_ni[begin];
  //use one-d indexing here to be consistant with Matalb code
  //might need to abandon this if we are going to abondon the Matlab version
  double *Gupdateprob1 = new double[FF];
  int maxDDtP = maxdd*p;
  for (int h = begin; h < end; h++) {
    for (int k=0; k < FF; k++) {
      try {
        double Gupdateprod = 1.0;
        for (int memberindex=0; memberindex < ni[h]; memberindex++){
          int base = (cum_ni[h]+memberindex)*p; //base for data
          double add = 0.0;
          for (int l=0; l < SS; l++) {
            double phiprod = 1.0;
            int phi_base = maxDDtP*(k*SS+l);
            for (int j=0; j < p; j++) {
              int u = data[base+j]-1;
              phiprod *= phi[phi_base+j*maxdd+u];
            }
            add += omega[FF*l+k]*phiprod;
          } // closing l++
          Gupdateprod *= add;
        } // closing member++

        for (int hv = 0; hv < Lambdas.size(); hv++) {
          Gupdateprod *= Lambdas[hv][(HHdata[h+hv*n]-1)*FF+k];
        }
        Gupdateprob1[k] = pi[k]*Gupdateprod;
      } catch (...) {
        Gupdateprob1[k] = 0;
      }
    } // closing k++

    group[h] = samplew(Gupdateprob1, FF, rand[h]);
    for (int m=0; m < ni[h];m++) {
      indi[count++] = group[h];
    }
  }
  delete [] Gupdateprob1;
}

struct GSamlpler : public Worker {
  //input
  RMatrix<double> phi;
  RMatrix<int> data;
  RMatrix<double> omega;
  RVector<double> pi;
  RVector<int> ni;
  RMatrix<int> HHdata;
  List lambda;
  RVector<double> rand;

  //output
  RVector<int> group;
  RVector<int> indi;

  //working variables
  int p;
  int FF;
  int SS;
  int maxdd;
  std::vector<NumericMatrix> Lambdas;
  int nIndividuals;
  int n; //number of households
  int* cum_ni = NULL;

  GSamlpler(NumericMatrix phi, IntegerMatrix data,
            NumericMatrix omega, NumericVector pi, IntegerVector ni,
            IntegerMatrix HHdata, List lambda, NumericVector rand, IntegerVector group, IntegerVector indi)
    :phi(phi), data(data), omega(omega),pi(pi), ni(ni), HHdata(HHdata), lambda(lambda), rand(rand), group(group), indi(indi) {
    p = data.nrow();
    FF = omega.nrow();
    SS = omega.ncol();
    maxdd = phi.nrow() / p;
    for (int i = 0; i < lambda.length(); i++) {
      Lambdas.push_back(lambda[i]);
    }

    nIndividuals = data.ncol();

    n = ni.length(); //number of households
    cum_ni = new int[n];
    cum_ni[0] = 0;
    for (int i = 1; i < n; i++) {
      cum_ni[i] = cum_ni[i-1] + ni[i-1];
    }
  }
  ~GSamlpler() {
    if (cum_ni != NULL) {
      delete [] cum_ni;
    }
  }
  void operator()(std::size_t begin, std::size_t end) {
    ::sampleG_imp(data.begin(), HHdata.begin(), ni.begin(), cum_ni, n, phi.begin(), maxdd, p,
                  omega.begin(), FF, SS, Lambdas, pi.begin(), rand.begin(), group.begin(), indi.begin(), begin, end);
  }
};

// [[Rcpp::export]]
List sampleG(NumericMatrix phi, IntegerMatrix data,
             NumericMatrix omega, NumericVector pi, IntegerVector ni,
             IntegerMatrix HHdata, List lambda, int Parallel) {

  int n = ni.length(); //number of household
  IntegerVector group(n);
  NumericVector rand = runif(n);

  int nIndividuals = data.ncol();
  IntegerVector indi(nIndividuals);

  if (Parallel) {
    GSamlpler gs(phi, data, omega, pi, ni, HHdata, lambda, rand, group, indi);
    parallelFor(0, n, gs,GRAINSIZE);
  } else {
    int p = data.nrow();
    int FF = omega.nrow();
    int SS = omega.ncol();
    int maxdd = phi.nrow() / p;
    std::vector<NumericMatrix> Lambdas;
    for (int i = 0; i < lambda.length(); i++) {
      Lambdas.push_back(lambda[i]);
    }

    int *cum_ni = new int[n];
    cum_ni[0] = 0;
    for (int i = 1; i < n; i++) {
      cum_ni[i] = cum_ni[i-1] + ni[i-1];
    }
    sampleG_imp(data.begin(), HHdata.begin(), ni.begin(), cum_ni, n, phi.begin(), maxdd, p,
                omega.begin(), FF, SS, Lambdas, pi.begin(), rand.begin(), group.begin(), indi.begin(), 0, n);
    delete [] cum_ni;
  }

  return List::create(Named("G", group), Named("G_Individuals", indi));
}
