#include <Rcpp.h>
#include <array>
#include "shared.h"
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]


// [[Rcpp::export]]
NumericVector cpp_gmRNA_basic(double n, double r_on, double r_degr) {
  if(!isInteger(n)) {
    return NumericVector(0);
  }
  NumericVector res((int)n);

  double t0 = 0, x0 = 0, tmax = 20/r_degr;
  double x, tx;
  double lambda1, lambda2, lambdax;
  double tau, tau_stern, u;
  int k;

  for(int i = 0; i < n; i++) {
    x = x0;
    tx = t0;
    // step 1
    lambda1 = r_on;
    lambda2 = r_degr * x;
    lambdax = lambda1 + lambda2;

    // step 2
    NumericVector tau_vec = rexp(1, lambdax);
    tau = tau_vec[0];
    tau_stern = min(NumericVector::create(tau, tmax - tx));
    tx += tau_stern;

    while(tx < tmax) {
      // step 3
      NumericVector u_vec = runif(1);
      u = u_vec[0];
      k = (u <= (lambda1/lambdax)) ? 1 : 2;

      // step 4
      x = (k == 1) ? x+1 : x-1;

      // step 5 includes step 1
      lambda1 = r_on;
      lambda2 = r_degr * x;
      lambdax = lambda1 + lambda2;

      // step 6
      NumericVector tau_vec = rexp(1, lambdax);
      tau = tau_vec[0];
      tau_stern = min(NumericVector::create(tau, tmax - tx));
      tx += tau_stern;
    }
    res[i] = x;
  }
  return res;
}


// [[Rcpp::export]]
NumericVector cpp_gmRNA_switch(double n, double r_act, double r_deact, double r_on, double r_degr) {
  if(!isInteger(n)) {
    return NumericVector(0);
  }

  NumericVector res((int)n);
  double t0 = 0, tmax = 20/r_degr, tx;
  int k;
  std::array<double, 3> x0{ {1, 0, 0} };
  std::array<double, 3>x;
  double r_act1, r_act2, r_act3, r_act4, r_actx;
  double tau, tau_stern, u;

  for(int i = 0; i < n; i++) {
    tx = t0;
    x = x0;
    // step 1
    r_act1 = r_act * x[0];
    r_act2 = r_deact * x[1];
    r_act3 = r_on * x[1];
    r_act4 = r_degr * x[2];
    r_actx = r_act1 + r_act2 + r_act3 + r_act4;

    // step 2
    NumericVector tau_vec = rexp(1, r_actx);
    tau = tau_vec[0];
    tau_stern = min(NumericVector::create(tau, tmax - tx));
    tx += tau_stern;

    while(tx < tmax) {
      // step 3
      NumericVector u_vec = runif(1);
      u = u_vec[0];
      if(u <= r_act1/r_actx)
        k = 1;
      else if(u <= (r_act1+r_act2)/r_actx)
        k = 2;
      else if(u <= (r_act1+r_act2+r_act3)/r_actx)
        k = 3;
      else
        k = 4;

      // step 4
      switch(k) {
      case 1:
        x[0]--;
        x[1]++;
        break;
      case 2:
        x[0]++;
        x[1]--;
        break;
      case 3:
        x[2]++;
        break;
      case 4:
        x[2]--;
        break;
      }

      // step 5
      r_act1 = r_act * x[0];
      r_act2 = r_deact * x[1];
      r_act3 = r_on * x[1];
      r_act4 = r_degr * x[2];
      r_actx = r_act1 + r_act2 + r_act3 + r_act4;

      // step 6
      NumericVector tau_vec = rexp(1, r_actx);
      tau = tau_vec[0];
      tau_stern = min(NumericVector::create(tau, tmax - tx));
      tx += tau_stern;
    }
    res[i] = x[2];
  }
  return res;
}


// [[Rcpp::export]]
NumericVector cpp_gmRNA_burst(double n, double r_burst, double s_burst, double r_degr) {
  if(!isInteger(n)) {
    return NumericVector(0);
  }
  NumericVector res((int)n);

  double t0 = 0, x0 = 0, tmax = 20/r_degr;
  double x, tx;
  double lambda1, lambda2, lambdax;
  double tau, tau_stern, u;
  int k;

  for(int i = 0; i < n; i++) {
    x = x0;
    tx = t0;
    // step 1
    lambda1 = r_burst;
    lambda2 = r_degr * x;
    lambdax = lambda1 + lambda2;

    // step 2
    NumericVector tau_vec = rexp(1, lambdax);
    tau = tau_vec[0];
    tau_stern = min(NumericVector::create(tau, tmax - tx));
    tx += tau_stern;

    while(tx < tmax) {
      // step 3
      NumericVector u_vec = runif(1);
      u = u_vec[0];
      k = (u <= (lambda1/lambdax)) ? 1 : 2;

      // step 4
      if(tau <= tau_stern) {
        if(k == 1) {
          // burst
          NumericVector r_vec = rgeom(1, 1/(1 + s_burst));
          x = x + r_vec[0];
        } else {
          // degradation
          x--;
        }
      }

      // step 5
      lambda1 = r_burst;
      lambda2 = r_degr * x;
      lambdax = lambda1 + lambda2;

      // step 6
      NumericVector tau_vec = rexp(1, lambdax);
      tau = tau_vec[0];
      tau_stern = min(NumericVector::create(tau, tmax - tx));
      tx += tau_stern;
    }
    res[i] = x;
  }
  return res;
}

// [[Rcpp::export]]
NumericVector cpp_gmRNA_basic_burst(double n, double r_on, double r_burst, double s_burst, double r_degr) {
  if(!isInteger(n)) {
    return NumericVector(0);
  }
  NumericVector res((int)n);

  double t0 = 0, x0 = 0, tmax = 20/r_degr;
  double x, tx;
  double lambda1, lambda2, lambda3, lambdax;
  double tau, tau_stern, u;
  int k;

  for(int i = 0; i < n; i++) {
    x = x0;
    tx = t0;
    // step 1
    lambda1 = r_on;
    lambda2 = r_burst;
    lambda3 = r_degr * x;
    lambdax = lambda1 + lambda2 + lambda3;

    // step 2
    NumericVector tau_vec = rexp(1, lambdax);
    tau = tau_vec[0];
    tau_stern = min(NumericVector::create(tau, tmax - tx));
    tx += tau_stern;

    while(tx < tmax) {
      // step 3
      NumericVector u_vec = runif(1);
      u = u_vec[0];
      if(u <= lambda1/lambdax)
        k = 1;
      else if(u <= (lambda1 + lambda2)/lambdax)
        k = 2;
      else k = 3;

      // step 4
      if(tau <= tau_stern) {
        if(k == 1){
          x = x+1;
        } else if(k == 2) {
          // burst
          NumericVector r_vec = rgeom(1, 1/(1 + s_burst));
          x = x + r_vec[0];
        } else {
          // degradation
          x= x - 1;
        }

      }

      // step 5 includes step 1
      lambda1 = r_on;
      lambda2 = r_burst;
      lambda3 = r_degr * x;
      lambdax = lambda1 + lambda2 + lambda3;

      // step 6
      NumericVector tau_vec = rexp(1, lambdax);
      tau = tau_vec[0];
      tau_stern = min(NumericVector::create(tau, tmax - tx));
      tx += tau_stern;
    }
    res[i] = x;
  }
  return res;
}



// [[Rcpp::export]]
NumericVector cpp_rInvGaus(double n, double mu, double lambda){

  NumericVector res((int)n);
  double y, x;
  NumericVector z, u;

  for(int i=0; i<n; ++i){
    z = rnorm(1,0,1);
    y = z[0] * z[0];
    x = mu + 0.5 * mu * mu * y/lambda - 0.5 * (mu/lambda) * sqrt(4 * mu * lambda * y + mu * mu * y * y);
    u = runif(1,0,1);
    if(u[0] <= mu/(mu + x)){
      res[i] = x;
    }else{
      res[i] = mu * mu/x;
    };
  }
  return res;
}

// // [[Rcpp::export]]
// NumericVector cpp_gmRNA_IGbasic_burst(double n, double mu_IG, double r_burst, double r_degr) {
//   if(!isInteger(n)) {
//     return NumericVector(0);
//   }
//   NumericVector res((int)n);
//
//   double t0 = 0, x0 = 0, tmax = 20/r_degr;
//   double x, tx;
//   double lambda1, lambda2, lambda3, lambdax;
//   double tau, tau_stern, u;
//   int k;
//   NumericVector lambda_draw;
//
//   for(int i = 0; i < n; i++) {
//     x = x0;
//     tx = t0;
//     // step 1
//     lambda_draw = cpp_rInvGaus(1, mu_IG, mu_IG * r_burst);
//     lambda1 = lambda_draw[0];
//     lambda2 = r_burst;
//     lambda3 = r_degr * x;
//     lambdax = lambda1 + lambda2 + lambda3;
//
//     // step 2
//     NumericVector tau_vec = rexp(1, lambdax);
//     tau = tau_vec[0];
//     tau_stern = min(NumericVector::create(tau, tmax - tx));
//     tx += tau_stern;
//     while(tx < tmax) {
//       // step 3
//       NumericVector u_vec = runif(1);
//       u = u_vec[0];
//       if(u <= lambda1/lambdax)
//         k = 1;
//       else if(u <= (lambda1 + lambda2)/lambdax)
//         k = 2;
//       else k = 3;
//
//       // step 4
//       if(tau <= tau_stern) {
//         if(k == 1){
//           x = x + 1;
//         } else if(k == 2) {
//           // burst
//           NumericVector r_vec = rnbinom(1, 1/2, r_burst/(r_burst + 2* mu_IG));
//           x = x + r_vec[0];
//         } else {
//           // degradation
//           x= x - 1;
//         }
//
//       }
//
//       // step 5 includes step 1
//       lambda_draw = cpp_rInvGaus(1, mu_IG, mu_IG * r_burst);
//       lambda1 = lambda_draw[0];
//       lambda2 = r_burst;
//       lambda3 = r_degr * x;
//       lambdax = lambda1 + lambda2 + lambda3;
//
//       // step 6
//       NumericVector tau_vec = rexp(1, lambdax);
//       tau = tau_vec[0];
//       tau_stern = min(NumericVector::create(tau, tmax - tx));
//       tx += tau_stern;
//     }
//     res[i] = x;
//   }
//   return res;
// }

