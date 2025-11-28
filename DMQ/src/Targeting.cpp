#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
arma::vec Find_Xi0(arma::vec vQ_0, arma::vec vTau, double dPhi, double dGamma, int iTau_star, arma::vec vScaling) {
  
  int iJ = vTau.size();
  int j;
  int s;
  int l;
  
  arma::vec vXi0 = zeros(iJ);
  
  double dA = 0.0;
  double dBaz = 0.0;
  
  arma::vec vLogQuantileDifference = zeros(iJ);
  
  if (iTau_star > 0) {
    for (j = iTau_star - 1; j >= 0 ; j--) {
      vLogQuantileDifference(j) =  log(vQ_0(j + 1) - vQ_0(j));
    }
  }
  
  
  if (iTau_star < iJ - 1) {
    for (j = iTau_star + 1; j < iJ ; j++) {
      vLogQuantileDifference(j) =  log(vQ_0(j) - vQ_0(j - 1));
    }
  }
  
  double dFoo = 0.0;
  
  for (j = 1; j <= iJ; j++) {
    
    dFoo = 0.0;
    
    if (j != iTau_star + 1) {
      if (j > iTau_star + 1) {
        
        for (s = 0; s <= 2000; s++) {
          dA = dGamma * pow(dPhi, s * 1.0) / vScaling(j - 1);
          dBaz = 0.0;
          if (j < iJ) {
            for (l = j; l <= iJ - 1; l++) {
              dBaz += ((vTau(l) - vTau(l - 1)) * exp(-dA * (iJ - l)));
            }
          }
          
          dFoo += (log(exp(-dA * (iJ - j + 1)) * vTau(j - 1) + (1.0 - vTau(iJ - 1)) + dBaz));
          
        }
        
        vXi0(j - 1) =  vLogQuantileDifference(j - 1) - (accu(vTau.subvec(j - 1, iJ - 1))/vScaling(j - 1)) * dGamma /(1.0 - dPhi) - dFoo;
        
      }
      if (j < iTau_star + 1) {
        
        for (s = 0; s <= 2000; s++) {
          dA = dGamma * pow(dPhi, s * 1.0) / vScaling(j - 1);
          dBaz = 0.0;
          if (j > 1) {
            for (l = 1; l <= j - 1; l++) {
              dBaz += ((vTau(l) - vTau(l - 1)) * exp(-dA * (l * 1.0)));
            }
          }
          
          dFoo += (log(exp(-dA * j) * vTau(0) + (1.0 - vTau(j - 1)) +  dBaz));
          
        }
        
        vXi0(j - 1) =  vLogQuantileDifference(j - 1) - (accu(vTau.subvec(0, j - 1))/vScaling(j - 1)) * dGamma /(1.0 - dPhi) - dFoo;
        
      }
      
    } else {
      vXi0(iTau_star) = 0.0;
    }
    
  }
  
  return vXi0;
  
}

//[[Rcpp::export]]
double h_fun(int l, int j, int iJ, arma::vec vTau) {
  
  double dh = 0.0;
  
  if (l == 0) {
    dh = vTau(0);
  } else if (l == j) {
    dh = 1.0 - vTau(j - 1);
  } else {
    dh = vTau(l) - vTau(l-1);
  }
  
  return dh;
}

//[[Rcpp::export]]
double g_fun(int l, int j, int iJ, arma::vec vTau) {
  
  double dh = 0.0;
  
  if (l == j-1) {
    dh = vTau(j-1);
  } else if (l == iJ) {
    dh = 1.0 - vTau(iJ - 1);
  } else {
    dh = vTau(l) - vTau(l-1);
  }
  
  return dh;
}

