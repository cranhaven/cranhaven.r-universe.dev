#include <RcppArmadillo.h>
#include "Targeting.h"
#include "Filtering.h"

using namespace arma;
using namespace Rcpp;

double InverseCDF(double dU, double dX0, double dX1, double dF0, double dF1) {

  double dX = dX0 + (dX1 - dX0) * (dU - dF0)/(dF1 - dF0);

  return dX;
}

//[[Rcpp::export]]
double Sim_C(arma::vec vQ, arma::vec vCDF) {

  int iJ = vCDF.size();

  double dU = Rf_runif(1e-10, 1.0 - 1e-10);
  double dX = 0.0;

  if ((dU < vCDF(0)) || (dU > vCDF(iJ-1))){
  
  // use Gaussian tails
  double dM1 = vCDF(0)*vQ(0);
  double dM2 = vCDF(0)*pow(vQ(0), 2.0);
  
  for(int j = 1; j < iJ; j++) {
    
    dM1 += ((vCDF(j) - vCDF(j-1))*vQ(j));
    dM2 += ((vCDF(j) - vCDF(j-1))*pow(vQ(j), 2.0));
    
  }
  
  double dSigma2 = dM2 - pow(dM1, 2.0);
  
  if (dSigma2 < 0.0) {
    dSigma2 = dM2;
  }
  
  dX = Rf_qnorm5(dU, dM1, pow(dSigma2, 0.5), 1, 0);
    
  } else {

    int s = 0;
    bool bRun = true;

    while(bRun) {
      if (vCDF(s) < dU) {
        s += 1;
      } else {
        bRun = false;
      }
    }
    double dF0 = vCDF(s - 1);
    double dF1 = vCDF(s);
    double dX0 = vQ(s - 1);
    double dX1 = vQ(s);
    dX = InverseCDF(dU, dX0, dX1, dF0, dF1);
  }
  return dX;
}

//[[Rcpp::export]]
List Simulate_DMQ_C(int iT, arma::vec vTau, arma::vec vQ_0, Function fSim, 
                    int iTau_star, double dBeta, double dAlpha, double dGamma, 
                    double dPhi, std::string ScalingType, arma::vec vVar) {

  int iJ = vTau.size();
  int j;
  int t;

  arma::vec vY(iT);
  arma::mat mQ(iJ, iT);
  arma::mat mEta = zeros(iJ, iT);
  arma::mat mZ(iJ, iT);
  arma::mat mU(iJ, iT);
  arma::mat mInnov(iJ, iT);

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

  NumericVector FOO;

  FOO = fSim(mQ.col(0), vTau);

  vY(0) = FOO[0];

  for (j = 0; j < iJ; j++) {
    mZ(j, 0) = dHit(vY(0), mQ(j, 0), vTau(j));
  }

  for (t = 1; t < iT; t++) {

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

    FOO = fSim(mQ.col(t), vTau);
    vY(t) = FOO[0];

    for (j = 0; j < iJ; j++) {
      mZ(j, t) = dHit(vY(t), mQ(j, t), vTau(j));
    }

  }

  List lOut;

  lOut["mQ"] = mQ;
  lOut["mEta"] = mEta;
  lOut["mU"] = mU;
  lOut["vY"] = vY;

  return lOut;

}

