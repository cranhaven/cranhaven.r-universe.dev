#include <Rcpp.h>
using namespace Rcpp;
//  sampleW.h
void cumnorm_inplace(double* p, int n);
int samplew(double *p, int n, double d);
void samplew_multi(double *p, int n, double *d,int howmany);
//this version put results into a different place
void samplew_multi2(double *p, int n, double *d, int* result,int howmany);

IntegerVector checkSZ2(IntegerMatrix Data_to_check, int h);
IntegerVector sampleW_multi(NumericVector p, NumericVector d);
IntegerVector CheckSZ_batch(IntegerMatrix X_house, IntegerMatrix X_indiv);
