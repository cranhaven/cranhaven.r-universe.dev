#include <math.h>
#include <string.h>
#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>
#include <stdio.h>


SEXP SSL_gaussian(SEXP X_, SEXP y_, SEXP penalty_, SEXP variance_, SEXP lambda1_, SEXP lambda0s_, SEXP theta_, SEXP sigma_, SEXP a_,  SEXP b_, SEXP eps_, SEXP max_iter_, SEXP counter_);
SEXP standardize(SEXP X_);


// Cross product of y with jth column of X
double crossprod(double *X, double *y, int n, int j) {

  int nn = n*j;

  double val=0;

  for (int i=0;i<n;i++) val += X[nn+i]*y[i];

  return(val);
}


double sum(double *x, int n) {

  double val=0;

  for (int i=0;i<n;i++) val += x[i];

  return(val);
}

int checkConvergence(double *beta, double *beta_old, double eps, int l, int p) {

  int converged = 1;

  for (int j=0; j<p; j++) {

    if (fabs((beta[l*p+j]-beta_old[j])/beta_old[j]) > eps) {

      converged = 0;

      break;

    }

  }

  return(converged);
}


double pstar(double x, double theta, double lambda1, double lambda0){

  double value;

  if (lambda1==lambda0){return 1;} else{

  value=(1-theta)/theta*lambda0/lambda1*exp(-fabs(x)*(lambda0-lambda1));

  value+=1;

  value=1/value;

  return value;}


}

double expectation_approx(double *beta, double a, double b, int p, int l){

  int sum=0;

  int i;
  
  for (i=0;i<p;i++){
    
    if(beta[l*p+i]!=0){sum++;}
  
  } 

  return (sum+a)/(a+b+p);

}


double lambdastar(double x, double theta, double lambda1, double lambda0){

  double aux;

  if (lambda1==lambda0){return lambda1;} else{

    aux=pstar(x,theta,lambda1,lambda0);

    return aux*lambda1+(1-aux)*lambda0;}

}

double update_sigma2(double *r, int n){
  
  double sum_r2 = 0;
  
  for(int i = 0; i < n; i++) sum_r2 += pow(r[i], 2);
  
  double sig;
  
  sig = (sum_r2)/(n + 2);
  
  return(sig);
}


double SSL(double z, double beta, double lambda0, double lambda1, double theta, double v, int n, double delta, double sigma2) {

  double s=0;

  double lambda;

  if (z > 0) s = 1;

  else if (z < 0) s = -1;

  if (fabs(z) <= delta) return(0);

  else {
    
    lambda=lambdastar(beta, theta, lambda1, lambda0);
    
    double temp;
    
    temp = fabs(z) - sigma2*lambda;
    
    if (temp > 0) return(temp*s/n);
    
    else return(0);
   
  }
}

double g(double x, double theta, double sigma2, double lambda1, double lambda0, int n){
  
  double value=lambdastar(x,theta,lambda1,lambda0);
  
  return pow((value-lambda1),2)+2*n/sigma2*log(pstar(x,theta,lambda1,lambda0));
}


double threshold(double theta, double sigma2, double lambda1, double lambda0, int n){
  
  if (lambda0==lambda1){return sigma2*lambda1;} else{
    
    if( g(0,theta,sigma2, lambda1,lambda0, n)>0){
      
      return sqrt(2* n *sigma2*log(1/pstar(0,theta,lambda1,lambda0)))+sigma2*lambda1;
      
    }
    
    else{
      
      return sigma2*lambdastar(0,theta,lambda1,lambda0);
      
    }
  }
}
