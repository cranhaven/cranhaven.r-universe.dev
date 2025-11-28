#include <RcppArmadillo.h>
#include "Targeting.h"

using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
double Loss(double dY, double dQ, double dTau, bool smooth = false) {

  double dLoss = 0.0;

  if (smooth) {

    dLoss = (dTau - 1.0/(1.0 + exp((dY-dQ)/0.001))) * (dY - dQ);

  } else {
    if (dY < dQ) {
      dLoss = (dTau - 1.0) * (dY - dQ);
    }

    if (dY > dQ) {
      dLoss = dTau * (dY - dQ);;
    }
  }



  return dLoss;
}

//[[Rcpp::export]]
double dHit(double dY, double dQ, double dTau, bool smooth = false) {
  
  double dZ = 0.0;
  
  if (smooth) {
    
    dZ = 1.0/(1.0 + exp((dY-dQ)/0.001)) - dTau;
    
  } else {
    if (dY < dQ) {
      dZ = 1.0 - dTau;
    }
    if (dY > dQ) {
      dZ = -dTau;
    }
  }
  
  return dZ;
}

//[[Rcpp::export]]
List FilterDMQ(arma::vec vY, arma::vec vTau, arma::vec vQ_0, int iTau_star, double dBeta, double dAlpha,
                double dGamma, double dPhi, std::string ScalingType, arma::vec vVar, bool smooth) {
  
  int iT = vY.size();
  int iJ = vTau.size();
  int j;
  int t;
  
  arma::mat mQ(iJ, iT + 1);
  arma::mat mEta = zeros(iJ, iT + 1);
  arma::mat mLoss(iJ, iT);
  arma::mat mZ(iJ, iT);
  arma::mat mU(iJ, iT);
  double dLoss = 0.0;

  arma::vec vScaling = ones(iJ);
  
  if (ScalingType == "InvSqrt") {
    for (j = 0; j < iJ; j++) {
      vScaling(j) = pow(vVar(j), 0.5);
    } 
  } 
  if (ScalingType == "Inv") {
    for (j = 0; j < iJ; j++) {
      vScaling(j) = vVar(j);
    }
  }
  if (ScalingType == "Identity") {
    for (j = 0; j < iJ; j++) {
      vScaling(j) = 1.0;
    }
  }
  //include vScaling
  arma::vec vEta_Bar = Find_Xi0(vQ_0, vTau, dPhi, dGamma, iTau_star, vScaling);

  mQ.col(0) = vQ_0;
  mEta.col(0) = vEta_Bar;

  for (j = 0; j < iJ; j++) {
    mLoss(j, 0) = Loss(vY(0), mQ(j, 0), vTau(j), smooth);
    dLoss += mLoss(j, 0);
    mZ(j, 0) = dHit(vY(0), mQ(j, 0), vTau(j), smooth);
  }
  
  for (t = 1; t < iT + 1; t++) {
    
    mU(iTau_star, t - 1) = -accu(mZ.col(t - 1))/vScaling(iTau_star);
    
    mQ(iTau_star, t) = vQ_0(iTau_star) * (1.0 - dBeta) + dBeta * mQ(iTau_star, t - 1) + dAlpha * mU(iTau_star, t - 1);
    
    for (j = 0; j < iJ; j++) {
      if (j < iTau_star) {
        mU(j, t - 1) = accu(mZ.col(t - 1).subvec(0, j))/vScaling(j);
      }
      if (j > iTau_star) {
        mU(j, t - 1) = -accu(mZ.col(t - 1).subvec(j, iJ - 1))/vScaling(j);
      }
      
      if (j != iTau_star) {
        mEta(j, t)   =  vEta_Bar(j) * (1.0 - dPhi) + dPhi * mEta(j, t - 1) + dGamma * mU(j, t - 1);
      }
    }
    
    if (iTau_star > 0) {
      for (j = iTau_star - 1; j >= 0 ; j--) {
        mQ(j, t) =  mQ(j + 1, t) - exp(mEta(j, t));
      }
    }
    
    if (iTau_star < iJ) {
      for (j = iTau_star + 1; j < iJ ; j++) {
        mQ(j, t) =  mQ(j - 1, t) + exp(mEta(j, t));
      }
    }
    
    if (t < iT) {
      for (j = 0; j < iJ; j++) {
        mLoss(j, t) = Loss(vY(t), mQ(j, t), vTau(j), smooth);
        dLoss += mLoss(j, t);
        mZ(j, t) = dHit(vY(t), mQ(j, t), vTau(j), smooth);
      }
    }
    
  }
  
  List lOut;
  
  lOut["mLoss"] = mLoss;
  lOut["dLoss"] = dLoss;
  lOut["mQ"] = mQ;
  lOut["mEta"] = mEta;
  lOut["mU"] = mU;
  lOut["vY"] = vY;
  lOut["vScaling"] = vScaling;
  lOut["smooth"] = smooth;
  
  return lOut;
}





