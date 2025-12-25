//  sampleHouseholds.h
#include <Rcpp.h>
using namespace Rcpp;
void sampleHHindex(double** lambda, int n_lambdas, int householdsize, double* pi, int FF, double* nextrand, int* hhindexh, int nHouseholds);
void sampleHouseholds_imp(int* data, double* rand,  double** lambda, int* lambda_columns, double* omega, double* phi,
                      double *pi, int* d,int nHouseholds, int householdsize, int FF,int SS,
                      int maxdd, int p, int currrentbatch, int n_lambdas, int HeadAtGroupLevel, int Parallel);
IntegerMatrix samplehouseholds(NumericMatrix phi, NumericMatrix omega, NumericVector pi,
                                                     IntegerVector d, List lambda,
                                                     int currrentbatch, int nHouseholds,  int householdsize,int HeadAtGroupLevel, int Parallel);
IntegerMatrix samplehouseholds(NumericMatrix phi, NumericMatrix omega, NumericVector pi,
                               IntegerVector d, List lambda,
                               int currrentbatch, int nHouseholds,  int householdsize);
IntegerMatrix households2individuals(IntegerMatrix data, int hh_size);
void sampleIndivIndex(int* data,int* hhindexh, int nHouseholds, int base, int householdsize,
                            double* omegat, int SS, double* nextrand, std::size_t begin, std::size_t end);
void sampleIndivIndexParallel(int* data,int* hhindexh, int nHouseholds, int base, int householdsize,
                            double* omegat, int SS, double* nextrand);
void sampleHHindexParallel(double** lambda, int n_lambdas, int householdsize, double* pi,
                           int FF, double* nextrand, int* hhindexh, int nHouseholds,int HeadAtGroupLevel) ;
void sampleHHDataParallel(int* data, int* hhindexh, double* nextrand, int nHouseholds, int DIM,
                          double* lambda, int n_lambda, int FF, int householdsize,  int p, int g);
void sampleIndivDataParallel(int* data, int* hhindexh, double* nextrand, int nHouseholds,
                             double** ps, int* d, int p, int SS,int householdsize, int DIM,int currrentbatchbase);
void sampleIndivData(int* data, int* hhindexh, double* nextrand, int nHouseholds,
                     double** ps, int* d, int p, int SS,int householdsize, int DIM,
                     int currrentbatchbase, int begin, int end);
void sampleIndivData(int* data, int* hhindexh, double* nextrand, int nHouseholds,
                     double** ps, int* d, int p, int SS,int householdsize, int DIM,
                     int currrentbatchbase);

