// [[Rcpp::depends(BH)]]
#include <RcppArmadillo.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <float.h>
#include <boost/any.hpp>
// #include <omp.h>
#include <time.h>
#include <sstream>
#include <string>
#include <random>

using namespace Rcpp;
using namespace std;
using namespace arma;

// // [[Rcpp::export]]
// double herdobj(NumericVector& xx, NumericMatrix& des, double sigma) {
//   //Objective function for (sequential) kernel herding
//   
//   double sigma2 = pow(sigma,2.0);
//   int pp = des.ncol();
//   int nn = des.nrow();
//   double runsum = 0.0;
//   double tmp = 0.0;
//   
//   double coef1 = pow( sigma2/(sigma2+1), ((double)(pp))/2.0 );
//   double coef2 = - 1.0 / (2.0*(sigma2+1));
//   for (int l=0; l<pp; l++){
//     tmp += pow(xx(l),2.0);
//   }
//   runsum += coef1 * exp(coef2*tmp);
//   
//   for (int i=0; i<nn; i++){
//     tmp = 0.0;
//     for (int l=0; l<pp; l++){
//       tmp += pow( xx(l) - des(i,l), 2.0 );
//     }
//     runsum += - (1.0/((double)nn+1.0)) * exp(-1.0/(2.0*sigma2) * tmp);
//   }
//   
//   return (runsum);
// }
// 
// 
// // [[Rcpp::export]]
// NumericVector herdgrad(NumericVector& xx, NumericMatrix& des, double sigma) {
//   //Gradient for (sequential) kernel herding
//   
//   double sigma2 = pow(sigma,2.0);
//   int pp = des.ncol();
//   int nn = des.nrow();
//   NumericVector runvec(pp,0.0);
//   
//   double tmp = 0.0;
//   
//   double coef1 = pow( sigma2/(sigma2+1), ((double)(pp))/2.0 );
//   double coef2 = - 1.0 / (2.0*(sigma2+1));
//   for (int l=0; l<pp; l++){
//     tmp += pow(xx(l),2.0);
//   }
//   for (int l=0; l<pp; l++){
//     runvec(l) += - coef1 * exp(coef2*tmp) * (1.0/(sigma2+1)) * xx(l);
//   }
// 
//   for (int i=0; i<nn; i++){
//     tmp = 0.0;
//     for (int l=0; l<pp; l++){
//       tmp += pow( xx(l) - des(i,l), 2.0 );
//     }
//     for (int l=0; l<pp; l++){
//       runvec(l) += (1.0/((double)nn+1.0)) * (1.0/sigma2) * exp(-1.0/(2.0*sigma2) * tmp) * (xx(l)-des(i,l));
//     }
//   }
//   
//   return (runvec);
// }

// [[Rcpp::export]]
double herdobj_seq(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp, double sigma) {
  //Objective function for (sequential) kernel herding
  
  double sigma2 = pow(sigma,2.0);
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - distsamp(m,l), 2.0 );
    }
    runsum += (1.0/(double)NN) * exp(-1.0/(2.0*sigma2) * tmp);
  }
  
  for (int i=0; i<nn; i++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - des(i,l), 2.0 );
    }
    runsum += - (1.0/((double)nn+1.0)) * exp(-1.0/(2.0*sigma2) * tmp);
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector herdgrad_seq(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp, double sigma) {
  //Gradient for (sequential) kernel herding
  
  double sigma2 = pow(sigma,2.0);
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);
  
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - distsamp(m,l), 2.0 );
    }
    for (int l=0; l<pp; l++){
      runvec(l) += - (1.0/(double)NN) * (1.0/sigma2) * exp(-1.0/(2.0*sigma2) * tmp) * (xx(l)-distsamp(m,l));
    }
  }
  
  for (int i=0; i<nn; i++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - des(i,l), 2.0 );
    }
    for (int l=0; l<pp; l++){
      runvec(l) += (1.0/((double)nn+1.0)) * (1.0/sigma2) * exp(-1.0/(2.0*sigma2) * tmp) * (xx(l)-des(i,l));
    }
  }
  
  return (runvec);
}



// [[Rcpp::export]]
double herdobj_full(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp, double sigma) {
  //Objective function for (sequential) kernel herding
  
  double sigma2 = pow(sigma,2.0);
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - distsamp(m,l), 2.0 );
    }
    runsum += (1.0/(double)NN) * exp(-1.0/(2.0*sigma2) * tmp);
  }
  
  for (int i=0; i<nn; i++){
    if (i != idx){
      tmp = 0.0;
      for (int l=0; l<pp; l++){
        tmp += pow( xx(l) - des(i,l), 2.0 );
      }
      runsum += - (1.0/((double)nn)) * exp(-1.0/(2.0*sigma2) * tmp);
    }
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector herdgrad_full(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp, double sigma) {
  //Gradient for (sequential) kernel herding
  
  double sigma2 = pow(sigma,2.0);
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);
  
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 0.0;
    for (int l=0; l<pp; l++){
      tmp += pow( xx(l) - distsamp(m,l), 2.0 );
    }
    for (int l=0; l<pp; l++){
      runvec(l) += - (1.0/(double)NN) * (1.0/sigma2) * exp(-1.0/(2.0*sigma2) * tmp) * (xx(l)-distsamp(m,l));
    }
  }
  
  for (int i=0; i<nn; i++){
    if (i != idx){
      tmp = 0.0;
      for (int l=0; l<pp; l++){
        tmp += pow( xx(l) - des(i,l), 2.0 );
      }
      for (int l=0; l<pp; l++){
        runvec(l) += 1.0/((double)nn) * (1.0/sigma2) * exp(-1.0/(2.0*sigma2) * tmp) * (xx(l)-des(i,l));
      }
    }
  }
  
  return (runvec);
}






// [[Rcpp::export]]
double pspobj_seq(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp,
                   double lambda, double nu) {
  //Objective function for sequential PSPs
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += (1.0/(double)NN) * tmp;
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0) + lambda ) ); // power 2
    }
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += - (1.0/((double)nn+1.0)) * tmp;
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector pspgrad_seq(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp,
                           double lambda, double nu) {
  //Gradient for (sequential) kernel herding

  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);

  double tmp = 1.0;
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    tmp = (1.0/(double)NN) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      // runvec(l) += tmp / ( abs( xx(l) - distsamp(m,l) ) + lambda ) * (xx(l) - distsamp(m,l)) / abs(xx(l) - distsamp(m,l)) * (- nu); // power 1
      runvec(l) += tmp / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) * (xx(l) - distsamp(m,l)) * (- 2.0* nu); // power 1
    }
  }

  for (int i=0; i<nn; i++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0 ) + lambda ) ); // power 2
    }
    tmp = - (1.0/((double)nn+1.0)) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      // runvec(l) += tmp / ( abs( xx(l) - des(i,l) ) + lambda ) * (xx(l) - des(i,l)) / abs(xx(l) - des(i,l)) * (- nu); // power 1
      runvec(l) += tmp / ( pow( xx(l) - des(i,l), 2.0) + lambda ) * (xx(l) - des(i,l)) * (- 2.0*nu); // power 2
    }
  }

  return (runvec);
}




// [[Rcpp::export]]
double pspobj_full(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp,
                  double lambda, double nu) {
  //Objective function for sequential PSPs
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += (1.0/(double)NN) * tmp;
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    if (i != idx){
      for (int l=0; l<pp; l++){
        // tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
        tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0) + lambda ) ); // power 2
      }
      tmp = pow(tmp,nu)*pow(lambda,nu*pp);
      runsum += - (1.0/(double)nn) * tmp;
    }
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector pspgrad_full(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp,
                          double lambda, double nu) {
  //Gradient for (sequential) kernel herding
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);
  
  double tmp = 1.0;
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      // tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    tmp = (1.0/(double)NN) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      // runvec(l) += tmp / ( abs( xx(l) - distsamp(m,l) ) + lambda ) * (xx(l) - distsamp(m,l)) / abs(xx(l) - distsamp(m,l)) * (- nu); // power 1
      runvec(l) += tmp / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) * (xx(l) - distsamp(m,l)) * (- 2.0* nu); // power 1
    }
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    if (i != idx){
      for (int l=0; l<pp; l++){
        // tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
        tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0 ) + lambda ) ); // power 2
      }
      tmp = - (1.0/(double)nn) * pow(tmp,nu)*pow(lambda,nu*pp);
      for (int l=0; l<pp; l++){
        // runvec(l) += tmp / ( abs( xx(l) - des(i,l) ) + lambda ) * (xx(l) - des(i,l)) / abs(xx(l) - des(i,l)) * (- nu); // power 1
        runvec(l) += tmp / ( pow( xx(l) - des(i,l), 2.0) + lambda ) * (xx(l) - des(i,l)) * (- 2.0*nu); // power 2
      }
    }
  }
  
  return (runvec);
}






















// [[Rcpp::export]]
double pspobj_seq2(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp,
                  double lambda, double nu) {
  //Objective function for sequential PSPs
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += (1.0/(double)NN) * tmp;
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0) + lambda ) ); // power 2
    }
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += - (1.0/((double)nn+1.0)) * tmp;
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector pspgrad_seq2(NumericVector& xx, NumericMatrix& des, NumericMatrix& distsamp,
                          double lambda, double nu) {
  //Gradient for (sequential) kernel herding
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);
  
  double tmp = 1.0;
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    tmp = (1.0/(double)NN) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      runvec(l) += tmp / ( abs( xx(l) - distsamp(m,l) ) + lambda ) * (xx(l) - distsamp(m,l)) / abs(xx(l) - distsamp(m,l)) * (- nu); // power 1
      // runvec(l) += tmp / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) * (xx(l) - distsamp(m,l)) * (- 2.0* nu); // power 1
    }
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0 ) + lambda ) ); // power 2
    }
    tmp = - (1.0/((double)nn+1.0)) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      runvec(l) += tmp / ( abs( xx(l) - des(i,l) ) + lambda ) * (xx(l) - des(i,l)) / abs(xx(l) - des(i,l)) * (- nu); // power 1
      // runvec(l) += tmp / ( pow( xx(l) - des(i,l), 2.0) + lambda ) * (xx(l) - des(i,l)) * (- 2.0*nu); // power 2
    }
  }
  
  return (runvec);
}




// [[Rcpp::export]]
double pspobj_full2(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp,
                   double lambda, double nu) {
  //Objective function for sequential PSPs
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  double runsum = 0.0;
  double tmp = 0.0;
  
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    
    tmp = pow(tmp,nu)*pow(lambda,nu*pp);
    runsum += (1.0/(double)NN) * tmp;
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    if (i != idx){
      for (int l=0; l<pp; l++){
        tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
        // tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0) + lambda ) ); // power 2
      }
      tmp = pow(tmp,nu)*pow(lambda,nu*pp);
      runsum += - (1.0/(double)nn) * tmp;
    }
  }
  
  return (runsum);
}

// [[Rcpp::export]]
NumericVector pspgrad_full2(NumericVector& xx, NumericMatrix& des, int idx, NumericMatrix& distsamp,
                           double lambda, double nu) {
  //Gradient for (sequential) kernel herding
  
  int pp = des.ncol();
  int nn = des.nrow();
  int NN = distsamp.nrow();
  NumericVector runvec(pp,0.0);
  
  double tmp = 1.0;
  for (int m=0; m<NN; m++){
    tmp = 1.0;
    for (int l=0; l<pp; l++){
      tmp = tmp * ( 1.0 / ( abs( xx(l) - distsamp(m,l) ) + lambda ) ); // power 1
      // tmp = tmp * ( 1.0 / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) ); // power 2
    }
    tmp = (1.0/(double)NN) * pow(tmp,nu)*pow(lambda,nu*pp);
    for (int l=0; l<pp; l++){
      runvec(l) += tmp / ( abs( xx(l) - distsamp(m,l) ) + lambda ) * (xx(l) - distsamp(m,l)) / abs(xx(l) - distsamp(m,l)) * (- nu); // power 1
      // runvec(l) += tmp / ( pow( xx(l) - distsamp(m,l), 2.0) + lambda ) * (xx(l) - distsamp(m,l)) * (- 2.0* nu); // power 1
    }
  }
  
  for (int i=0; i<nn; i++){
    tmp = 1.0;
    if (i != idx){
      for (int l=0; l<pp; l++){
        tmp = tmp * ( 1.0 / ( abs( xx(l) - des(i,l) ) + lambda ) ); // power 1
        // tmp = tmp * ( 1.0 / ( pow( xx(l) - des(i,l), 2.0 ) + lambda ) ); // power 2
      }
      tmp = - (1.0/(double)nn) * pow(tmp,nu)*pow(lambda,nu*pp);
      for (int l=0; l<pp; l++){
        runvec(l) += tmp / ( abs( xx(l) - des(i,l) ) + lambda ) * (xx(l) - des(i,l)) / abs(xx(l) - des(i,l)) * (- nu); // power 1
        // runvec(l) += tmp / ( pow( xx(l) - des(i,l), 2.0) + lambda ) * (xx(l) - des(i,l)) * (- 2.0*nu); // power 2
      }
    }
  }
  
  return (runvec);
}


