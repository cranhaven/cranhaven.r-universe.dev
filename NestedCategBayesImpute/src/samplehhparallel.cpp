#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

#include "samplehouseholds.h"
#include "utils.h"
#include "sampleW.h"
#include "checkconstraints.h"

struct IndivDataSampler : public Worker {
  //Input
  int* data;
  int* hhindexh;
  double* nextrand;
  int nHouseholds;
  double** ps;
  int* d;
  int p;
  int SS;
  int householdsize;
  int DIM;
  int currrentbatchbase;

  IndivDataSampler(int* data, int* hhindexh, double* nextrand, int nHouseholds,
                   double** ps, int* d, int p, int SS,int householdsize, int DIM,int currrentbatchbase)
    :data(data), hhindexh(hhindexh), nextrand(nextrand), nHouseholds(nHouseholds),
     ps(ps), d(d), p(p), SS(SS), householdsize(householdsize), DIM(DIM), currrentbatchbase(currrentbatchbase) {
  }
  void operator()(std::size_t begin, std::size_t end) { //almost idential to the serical version, consider rewrite the version
    ::sampleIndivData(data, hhindexh, nextrand, nHouseholds, ps, d, p, SS, householdsize, DIM, currrentbatchbase, begin, end);
  }
  ~IndivDataSampler() {
  }
};

void sampleIndivDataParallel(int* data, int* hhindexh, double* nextrand, int nHouseholds,
                             double** ps, int* d, int p, int SS,int householdsize, int DIM,int currrentbatchbase) {
  IndivDataSampler worker(data, hhindexh, nextrand, nHouseholds, ps, d, p, SS, householdsize, DIM, currrentbatchbase);
  parallelFor(0, nHouseholds, worker,GRAINSIZE);
}

struct HHDataSampler : public Worker {
  //Input
  int* data;
  int* hhindexh;
  double* nextrand;
  int nHouseholds;
  int DIM;
  double* lambda;
  int n_lambda;
  int FF;
  int householdsize;
  int p;
  int g;

  //Working
  int** columns = NULL;
  double* lambda_t  = NULL;

  HHDataSampler(int* data, int* hhindexh, double* nextrand, int nHouseholds, int DIM,  double* lambda, int n_lambda,
                int FF, int householdsize,  int p, int g)
    :data(data), hhindexh(hhindexh), nextrand(nextrand), nHouseholds(nHouseholds), DIM(DIM), lambda(lambda), n_lambda(n_lambda),
     FF(FF), householdsize(householdsize), p(p), g(g) {
    columns = new int*[householdsize];
    for (int j = 0; j < householdsize; j++) {
      columns[j] = data + (j * DIM + 2 + p + g) * nHouseholds; //zero-based column
    }
    //prepare lambdas for for group sampling, first need to transpose lambda
    //the code  here duplicate the lines above for omega
    lambda_t = new double[FF * n_lambda];
    ::transposeAndNormalize(lambda, FF, n_lambda,  lambda_t);
  }
  ~HHDataSampler() {
    if (lambda_t != NULL) {
      delete [] lambda_t;
      delete [] columns;
    }
  }
  void operator()(std::size_t begin, std::size_t end) {
    for (int i = begin; i < end; i++) {
      int group = hhindexh[i]-1;
      double* currentp = lambda_t + group * n_lambda;
      columns[0][i] = std::distance(currentp, std::lower_bound(currentp, currentp+n_lambda,nextrand[i])) + 1;
      if (columns[0][i] > n_lambda) {columns[0][i] = n_lambda;}
    }
    for (int j = 1; j < householdsize; j++) {
      std::copy(columns[0]+begin, columns[0] + end, columns[j]+begin);
    }
  }
};

void sampleHHDataParallel(int* data, int* hhindexh, double* nextrand, int nHouseholds, int DIM,  double* lambda, int n_lambda,
                          int FF, int householdsize,  int p, int g) {
  HHDataSampler worker(data, hhindexh, nextrand,nHouseholds, DIM,  lambda, n_lambda, FF, householdsize, p, g);
  parallelFor(0, nHouseholds, worker,GRAINSIZE);

}


struct HHIndexSampler : public Worker {
  double** lambda;
  int n_lambdas;
  int householdsize;
  double* pi;
  int FF;
  double* nextrand;
  int* hhindexh;
  int nHouseholds;
  int HeadAtGroupLevel;

  double* pi_lambda_last = NULL;
  HHIndexSampler(double** lambda, int n_lambdas, int householdsize, double* pi, int FF,
                 double* nextrand, int* hhindexh, int nHouseholds,int HeadAtGroupLevel)
    :lambda(lambda),n_lambdas(n_lambdas),householdsize(householdsize),pi(pi),FF(FF),
     nextrand(nextrand),hhindexh(hhindexh),nHouseholds(nHouseholds), HeadAtGroupLevel(HeadAtGroupLevel) {
    int addjusted = HeadAtGroupLevel==0 ? (householdsize - 1 -1) : (householdsize - 1);
    double* currentlambdacolumn = lambda[n_lambdas-1] + addjusted * FF; //column hh_size-1, addjusted to zero based
    pi_lambda_last = new double[FF];
    //note that now household size start from 1, instead of 2
    for (int i = 0; i < FF; i++) {
      pi_lambda_last[i] = pi[i] * currentlambdacolumn[i];
    }
    //cumsum and normalize
    cumnorm_inplace(pi_lambda_last, FF);
  }
  void operator()(std::size_t begin, std::size_t end) {
    for (int i = begin; i < end; i++) {
      hhindexh[i] = std::distance(pi_lambda_last, std::lower_bound(pi_lambda_last, pi_lambda_last+FF,nextrand[i])) + 1;
      if(hhindexh[i]> FF) {hhindexh[i] = FF;}
    }
  }
  ~HHIndexSampler() {
    if (pi_lambda_last != NULL) {
      delete [] pi_lambda_last;
    }
  }
};

void sampleHHindexParallel(double** lambda, int n_lambdas, int householdsize, double* pi,
                           int FF, double* nextrand, int* hhindexh, int nHouseholds,int HeadAtGroupLevel) {
  HHIndexSampler worker(lambda, n_lambdas, householdsize, pi, FF, nextrand, hhindexh, nHouseholds, HeadAtGroupLevel);
  parallelFor(0, nHouseholds, worker,GRAINSIZE);

}

struct IndivIndexSampler : public Worker {
  int* data;
  int* hhindexh;
  int nHouseholds;
  int base;
  int householdsize;
  double* omegat;
  int SS;
  double* nextrand;
  IndivIndexSampler(int* data,int* hhindexh, int nHouseholds, int base, int householdsize,
                          double* omegat, int SS, double* nextrand)
    :data(data), hhindexh(hhindexh),nHouseholds(nHouseholds),base(base),householdsize(householdsize),omegat(omegat),SS(SS),nextrand(nextrand) {
  }
  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    ::sampleIndivIndex(data, hhindexh, nHouseholds, base, householdsize, omegat, SS, nextrand + begin * householdsize, begin, end);
  }
};

void sampleIndivIndexParallel(int* data,int* hhindexh, int nHouseholds, int base, int householdsize,
                            double* omegat, int SS, double* nextrand) {
  //Rcout << "parallel mode" << std::endl;
  IndivIndexSampler worker(data, hhindexh, nHouseholds, base, householdsize,omegat, SS, nextrand);
  parallelFor(0, nHouseholds, worker,GRAINSIZE);

}

struct HeadAtGroupLevelHHSampler : public Worker
{
  // source matrix
  RMatrix<double> phi;
  RMatrix<double> omega;
  RVector<double> pi;
  RVector<int> d;
  List lambda;
  int currrentbatchbase;
  int householdsize;

  int FF;
  int SS;
  int p;
  int n_lambdas;
  int *lambda_columns = NULL;
  double **lambdas = NULL;
  int maxDDtp;
  int maxdd;

  int DIM ; //not output the household size
  int ncol;

  NumericMatrix omegaT;

  // destination matrix
  RMatrix<int> data;
  NumericVector r; //at most this many
  int HeadAtGroupLevel;

  // initialize with source and destination
  HeadAtGroupLevelHHSampler(NumericMatrix phi, NumericMatrix omega,
                            NumericVector pi,IntegerVector d,
                            List lambda,
                            int currrentbatchbase, int householdsize,
                            IntegerMatrix data, int HeadAtGroupLevel)
    : phi(phi), omega(omega), pi(pi), d(d), lambda(lambda), currrentbatchbase(currrentbatchbase),
      householdsize(householdsize),data(data),HeadAtGroupLevel(HeadAtGroupLevel) {
    FF = omega.nrow();
    SS = omega.ncol();
    p = d.length();
    n_lambdas = lambda.length();
    lambda_columns = new int[n_lambdas];
    lambdas = new double*[n_lambdas];
    maxDDtp = phi.nrow();
    maxdd = maxDDtp / p;

    DIM = 2 + p + n_lambdas - 1; //not output the household size
    ncol = DIM * householdsize + householdsize  + 1;

    //copy data from list of matrices to C++
    for (int i = 0; i < n_lambdas; i++) {
      NumericMatrix l = lambda[i];
      lambda_columns[i] = l.ncol();
      lambdas[i] = new double[l.length()];
      std::copy(l.begin(), l.end(), lambdas[i]);
    }
    //prepare omega for group sampling, first need to transpose omega
    omegaT = transposeAndNormalize(omega);

    r = runif(data.nrow()  * ncol);
  }

  void cleanup() {
    delete [] lambda_columns;
    for (int i = 0; i < n_lambdas; i++) {
      delete [] lambdas[i];
    }
    delete [] lambdas;
  }

  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    int nHouseholds = end - begin;
    ::sampleHouseholds_imp(data.begin() + begin * ncol, r.begin() +  begin * ncol,
                                                 lambdas, lambda_columns, omegaT.begin(),
                                                 phi.begin(), pi.begin(),d.begin(),
                                                 nHouseholds, householdsize, FF, SS,maxdd,p,
                                                 currrentbatchbase + begin,n_lambdas, HeadAtGroupLevel, 1);
  }
};

IntegerMatrix sampleHH(NumericMatrix phi, NumericMatrix omega, NumericVector pi,
                                             IntegerVector d, List lambda,
                                             int currrentbatch, int nHouseholds,  int householdsize, int HeadAtGroupLevel) {
  int DIM = 2 + d.length() + lambda.length() - 1;
  IntegerMatrix data(nHouseholds, DIM * householdsize + householdsize  + 1);
  HeadAtGroupLevelHHSampler worker(phi, omega, pi, d, lambda, currrentbatch*nHouseholds, householdsize, data, HeadAtGroupLevel);
  parallelFor(0, data.nrow(), worker,1000);
  //worker.cleanup();
  return data;
}
