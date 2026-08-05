/*
 * Mathematical functions needed
 */

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"


double logit_inv(double x){
  double expx;
  
  if(x < 0){
    expx = exp(x);
    return(expx/(1.0+expx));
  }else{
    expx = exp(-x);
    return(1.0/(1.0+expx));
  }
}

/*
double logit_inv(double x){
  
  if(x < 0){
    return 1.0 / (1.0 + exp(-x));
  }else{
    return 1.0 - 1.0 / (1.0 + exp(x));
  }
}
 */

double safe_log1pexp(double x) {
  if (x > 700) {
    // e^700 overflows double; log(1+e^x) is approx x
    return x; 
  } else if (x > 0) {
    // log(1+e^x) = log(e^x * (e^-x + 1)) = x + log(1+e^-x)
    return x + log1p(exp(-x));
  } else if (x < -37) {
    // e^-37 is very small; log(1+e^x) is approx e^x
    return exp(x);
  } else {
    // Small to medium negative values: use log1p for precision
    return log1p(exp(x));
  }
}

double ord_qk(int k, double eta, double* c, int* Kord) {
  double etak, etak_1;
  double pk, pk_1, qk;
  
  if(k == 0){
    pk_1 = 0.0;                      // complementary probability
    etak = eta - c[k];
    pk = 1.0 / (1.0 + exp(etak));    // complementary probability
    qk = pk - pk_1;
  }else{
    if(k == *Kord){
      pk = 0.0;
      etak_1 = eta - c[k-1];
      pk_1 = 1.0 / (1.0 + exp(-etak_1));
      qk = pk_1 - pk;
    }else{
      etak = eta - c[k];
      etak_1 = eta - c[k-1];
      
      if(etak > 0){
        pk = 1.0 / (1.0 + exp(etak));     // complementary probability
        pk_1 = 1.0 / (1.0 + exp(etak_1)); // complementary probability
        qk = pk - pk_1;
      }else{
        pk = 1.0 / (1.0 + exp(-etak));
        pk_1 = 1.0 / (1.0 + exp(-etak_1));
        qk = pk_1 - pk;
      }
    }
  }
  return qk;
}
