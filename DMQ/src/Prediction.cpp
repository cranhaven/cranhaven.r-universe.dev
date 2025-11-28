#include <RcppArmadillo.h>
#include "Targeting.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
arma::mat XiPrediction(arma::vec vXi_bar, arma::vec vTau, int iH, arma::vec vEta_tp1,
                           double dPhi, double dGamma, int iTau_star, arma::vec vScaling){
  int j;
  int l;
  int h;
  int s;
  int iJ = vTau.size();
  arma::mat mXi_H = zeros(iH, iJ);
  
  double dBaz = 0.0;
  double dFoo1 = 0.0;
  double dFoo2 = 1.0;
  double dA = 0.0;
  
  for (h = 1; h <= iH; h++) {
    for (j = 1; j <= iJ; j++) {
      
      if (j != iTau_star + 1) {
        if (h == 1) {
          
          mXi_H(0, j - 1) = exp(vEta_tp1(j - 1));
          
        } else {
          
          dFoo2 = 1.0;
          dFoo1 = 0.0;
          
          if (j > iTau_star + 1) {
            for (s = 0; s <= h - 2; s++) {
              dFoo1 += pow(dPhi, s * 1.0);
              dA = dGamma * pow(dPhi, s * 1.0) / vScaling(j - 1);
              dBaz = 0.0;
              for (l = j - 1; l <= iJ; l++) {
                dBaz += (g_fun(l,j,iJ, vTau) * exp(-dA * (iJ - l)));
              }
              dFoo2 *= (exp(dA * accu(vTau.subvec(j - 1, iJ - 1)) ) * dBaz);
            }
          }
          if (j < iTau_star + 1) {
            for (s = 0; s <= h - 2; s++) {
              dFoo1 += pow(dPhi, s * 1.0);
              dA = dGamma * pow(dPhi, s * 1.0) / vScaling(j - 1);
              dBaz = 0.0;
              for (l = 0; l <= j; l++) {
                dBaz += (h_fun(l,j,iJ, vTau) * exp(dA * (j - l)));
              }
              dFoo2 *= (exp(-dA *  accu(vTau.subvec(0, j - 1))) * dBaz);
            }
          }
          mXi_H(h - 1, j - 1) =  exp(vXi_bar(j - 1) * (1.0 - dPhi) * dFoo1  + pow(dPhi, h * 1.0 - 1.0) * vEta_tp1(j - 1)) * dFoo2;
        }
      }
    }
    
  }
  
  return mXi_H;
  
}


//[[Rcpp::export]]
arma::vec ReferencePrediction(int iH, double dQ_bar, double dBeta, double dAlpha, double dQ_tp1) {
  
  arma::vec vReferencePred(iH);
  int h;
  int s;
  
  double dFoo = 0.0;
  
  vReferencePred(0) = dQ_tp1;
  
  if (iH > 1) {
    
    for (h = 2; h <= iH; h++) {
      dFoo = 0.0;
      for (s = 0; s <= h - 2; s++) {
        dFoo += pow(dBeta, s * 1.0);
      }
      
      vReferencePred(h - 1) = dQ_bar * (1.0 - dBeta) * dFoo + pow(dBeta, h * 1.0 - 1.0) * dQ_tp1;
      
    }
    
  }
  
  
  return vReferencePred;
}
