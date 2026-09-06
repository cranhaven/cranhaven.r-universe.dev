#include "mm_modelExt.h"

using namespace Rcpp ;
using namespace arma;


mm_modelExt::mm_modelExt(List model) : mm_model::mm_model(model)
{
    fixedObs = Rcpp::clone(as<NumericVector>(model[12]));
    P = Rcpp::clone(as<NumericVector>(model[13]));
    beta =  Rcpp::clone(as<NumericVector>(model[14]));
    NumericVector stayersTemp(T);
    stayerID = 0;
    int i, check;
    check = 1;
    for(i = 0; i < T; i++) {
        stayersTemp[i] = checkIndStayer(i);
        if(check && stayers[i]) {
            stayerID = i;
            check = 0;
        }
    }
    stayers = Rcpp::clone(as<NumericVector>(stayersTemp));
    numStayers = (int) std::accumulate(stayers.begin(), stayers.end(), 0.0);

}


int mm_modelExt::getFixedObs(int i, int j, int r, int n)
{
    return(fixedObs[i + j + J*r + J*maxR*n]);
}

double mm_modelExt::getP()
{
    return P[0];
}

double mm_modelExt::getBeta()
{
    return beta[0];
}

NumericVector mm_modelExt::getStayers()
{
    return stayers;
}

int mm_modelExt::getNumStayers()
{
    return numStayers;
}

// checks if an individual is a stayer
int mm_modelExt::checkIndStayer(int i)
{
    int j, r, n;
    int ret = 1;
    for(j = 0; j < J; j++) {
        for(r = 0; r < getR(j); r++) {
            for(n = 0; n < getN(i, j, r); n++ ) {
                if(getObs(i, j, r, n) !=  getFixedObs(0, j, r, n)) {
                    ret = 0;
                }
            }
        }
    }
    return ret;
}

int mm_modelExt::getStayerID()
{
    return stayerID;
}

int mm_modelExt::getStayers(int i)
{
    return (int) stayers[i];
}

void mm_modelExt::setP(double target)
{
    P[0] = target;
}

void mm_modelExt::setBeta(double target)
{
    beta[0] = target;
}

Rcpp::List mm_modelExt::returnModel()
{
    return Rcpp::List::create(Rcpp::Named("alpha", alpha),
                              Rcpp::Named("theta", theta),
                              Rcpp::Named("phi", phi),
                              Rcpp::Named("delta", delta),
                              Rcpp::Named("P", P),
                              Rcpp::Named("beta", beta));
}
