#include <Rcpp.h>
using namespace Rcpp;
#include "sampleW.h"
#include "checkconstraints.h"
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

void sampleM_imp(int* data,  int* indi, double* phi, double* omega, int FF, int SS,
                 int* G, int* serial, int p, int maxdd, double* rand, int begin, int end) {
  int maxDDtp = maxdd *p;
  double *Gupdateprob2= new double[SS];
  for (int m = begin; m < end; m++) {
    int G_asg = G[int(serial[m])-1];
    int base = m*p;
    int base2 = maxDDtp*((G_asg-1)*SS);
    for (int l = 0; l < SS; l++) {
      try {
        double phiprod = 1.0;
        for (int j = 0; j < p; j++) {
          int u = data[base+j]-1;
          phiprod *= phi[base2+j*maxdd+u];
        }
        Gupdateprob2[l] = omega[FF*l+G_asg-1]*phiprod; //can use logrithm to speed up here and also for accuracy
      } catch(...) {
        Gupdateprob2[l] = 0;
      }
    }
    indi[m] = samplew(Gupdateprob2, SS, rand[m]);
  }
  delete [] Gupdateprob2;
}

struct MSamlpler : public Worker
{
  //input
  RMatrix<double> phi;
  RMatrix<int> data;
  RMatrix<double> omega;
  RVector<int> G;
  RVector<int> serial;
  RVector<double> rand;

  //output
  RVector<int> indi;

  //working varaibles
  int p;
  int nIndividuals;
  int FF;
  int SS;
  int maxDDtp;
  int maxdd;

  // initialize with source and destination
  MSamlpler(const NumericMatrix phi, const IntegerMatrix data, const NumericMatrix omega, const IntegerVector G,
            const IntegerVector serial, NumericVector rand,  IntegerVector indi)
    : phi(phi), data(data), omega(omega), G(G), serial(serial), rand(rand),indi(indi) {
    p = data.nrow();
    nIndividuals = data.ncol();
    FF = omega.nrow();
    SS = omega.ncol();
    maxDDtp = phi.nrow();
    maxdd = maxDDtp / p;
  }
  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    ::sampleM_imp(data.begin(), indi.begin(), phi.begin(), omega.begin(), FF, SS,
                  G.begin(), serial.begin(), p, maxdd, rand.begin(),begin, end);
  }
};

// [[Rcpp::export]]
IntegerVector sampleM(NumericMatrix phi, IntegerMatrix data,
                      NumericMatrix omega, IntegerVector G, IntegerVector serial, int Parallel) {
  int nIndividuals = data.ncol();
  NumericVector rand = runif(nIndividuals);
  IntegerVector indi(nIndividuals);

  if (Parallel) {
    MSamlpler ms(phi, data, omega, G,serial, rand, indi);
    parallelFor(0,nIndividuals, ms,GRAINSIZE);
  } else {
    int p = data.nrow();
    int FF = omega.nrow();
    int SS = omega.ncol();
    int maxDDtp = phi.nrow();
    int maxdd = maxDDtp / p;
    sampleM_imp(data.begin(), indi.begin(),  phi.begin(), omega.begin(), FF, SS,
                G.begin(), serial.begin(), p, maxdd, rand.begin(), 0, nIndividuals);

  }
  return indi;
}

