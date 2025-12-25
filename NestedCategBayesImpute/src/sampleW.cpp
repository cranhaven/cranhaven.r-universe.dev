#include <Rcpp.h>
using namespace Rcpp;
#include "sampleW.h"
#include <algorithm>

void cumnorm_inplace(double* p, int n) {
  double dsum  =0;
  int i;
  double *myw;
  myw = new double[n];
  for (i = 0; i < n;i++) {
    dsum+=p[i];
  }
  if (dsum <= 0 ) {dsum = 1;}
  p[0] = p[0] / dsum;
  for (i = 1; i < n;i++) {
    p[i] = p[i] / dsum + p[i-1];
  }
}

//  sampleW.cpp
//  Created by Quanli Wang on 2/20/16.
int samplew(double *p, int n, double d) {
    double dsum;
    int i;
    dsum = 0;
    double *myw;
    myw = new double[n];
    for (i = 0; i < n;i++) {
        dsum+=p[i];
    }
    if (dsum <=0 ) {dsum =1;}
    myw[0] = p[0] / dsum;
    for (i = 1; i < n;i++) {
        myw[i] = p[i] / dsum + myw[i-1];
    }
    int k = std::distance(myw, std::lower_bound(myw, myw+n,d)) + 1;
    if (k > n) {k=n;}
    delete [] myw;
    return k;
}

void samplew_multi(double *p, int n, double *d,int howmany) {
    double dsum;
    int i,k;
    dsum = 0;
    double *myw;
    myw = new double[n];
    for (i = 0; i < n;i++) {
        dsum+=p[i];
    }
    if (dsum <=0 ) {dsum =1;}
    myw[0] = p[0] / dsum;
    for (i = 1; i < n;i++) {
        myw[i] = p[i] / dsum + myw[i-1];
    }
    for (int h=0; h < howmany; h++) {
        k = std::distance(myw, std::lower_bound(myw, myw+n,d[h])) + 1;
        if (k > n) { k = n;}
        d[h] = k;
    }
    delete [] myw;
}

//this version put results into a different place
void samplew_multi2(double *p, int n, double *d, int* result,int howmany) {
    double dsum;
    int i,k;
    dsum = 0;
    double *myw;
    myw = new double[n];
    for (i = 0; i < n;i++) {
        dsum+=p[i];
    }
    if (dsum <=0 ) {dsum =1;}
    myw[0] = p[0] / dsum;
    for (i = 1; i < n;i++) {
        myw[i] = p[i] / dsum + myw[i-1];
    }
    for (int h=0; h < howmany; h++) {
      k = std::distance(myw, std::lower_bound(myw, myw+n,d[h])) + 1;
      if (k > n) { k = n;}
        result[h] = k;
    }
    delete [] myw;
}


