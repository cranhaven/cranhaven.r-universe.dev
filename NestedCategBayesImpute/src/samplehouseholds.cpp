#include <Rcpp.h>
using namespace Rcpp;

#include "samplehouseholds.h"
#include "utils.h"

// [[Rcpp::export]]
IntegerMatrix households2individuals(IntegerMatrix data, int hh_size){

  int nHouseholds = data.ncol();

  //use the raw data instead, which has hh_size * DIM + 1 + hh_size columns (in C)
  int columns = data.nrow();
  //int hh_size = (columns - 1) / (DIM+1);
  int DIM = (columns -1) / hh_size -1;
  IntegerMatrix Individuals(DIM + 2, nHouseholds*hh_size);

  int c9 = hh_size * DIM;
  int count = 0;
  for (int i = 0; i < nHouseholds; i++) {
    int base = i * columns;
    for (int j = 0; j < hh_size; j++) {
      for (int k = 0; k < DIM;k++) {
        Individuals[count++] = data[base + j*DIM+k];
      }
      Individuals[count++] = data[base + c9];
      Individuals[count++] = data[base + c9 + 1 + j];
    }
  }
  return(Individuals);
}

// [[Rcpp::export]]
IntegerMatrix samplehouseholds(NumericMatrix phi, NumericMatrix omega, NumericVector pi,
                               IntegerVector d, List lambda,
                               int currrentbatch, int nHouseholds,  int householdsize, int HeadAtGroupLevel, int Parallel) {

  int FF = omega.nrow();
  int SS = omega.ncol();
  int p = d.length();
  int n_lambdas = lambda.length();
  int *lambda_columns = new int[n_lambdas];
  double **lambdas = new double*[n_lambdas];
  int maxDDtp = phi.nrow();
  int maxdd = maxDDtp / p;

  //int ncol = householdsize * DIM + 1 + householdsize;
  //output data: zero-based
  //column 0: household unique index
  //column 1: member index within the household (pernum:person number?)
  //column 2 to 2+p-1: individual data
  //column 2+p: 2+p+n_lambdas-2: household level data
  //column 2+p+n_lambdas-1: household group indicator
  //column last hh_size: individual group indicator
  int DIM = 2 + p + n_lambdas - 1; //not output the household size
  int ncol = DIM * householdsize + householdsize  + 1;
  IntegerMatrix data(nHouseholds, ncol);

  //copy data from list of matrices to C++
  for (int i = 0; i < n_lambdas; i++) {
    NumericMatrix l = lambda[i];
    lambda_columns[i] = l.ncol();
    lambdas[i] = new double[l.length()];
    std::copy(l.begin(), l.end(), lambdas[i]);
  }
  //printf("in samplehouseholds\n");
  NumericVector rand = runif(nHouseholds * ncol); //at most this many
  sampleHouseholds_imp(data.begin(), rand.begin(), lambdas, lambda_columns, omega.begin(),
                       phi.begin(), pi.begin(),d.begin(),
                       nHouseholds, householdsize, FF, SS,maxdd,p, currrentbatch,n_lambdas, HeadAtGroupLevel, Parallel);


  //clean up
  delete [] lambda_columns;
  for (int i = 0; i < n_lambdas; i++) {
    delete [] lambdas[i];
  }
  delete [] lambdas;
  //printf("done samplehouseholds\n");
  return data;
}
