#ifndef MM_MODELEXT_H
#define MM_MODELEXT_H


#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>
#include "settings.h"
#include "mm_model.h"

using namespace Rcpp ;
using namespace arma;

class mm_modelExt: public mm_model
{
public:
    //constructor
    mm_modelExt(List model);
    int getFixedObs(int i, int j, int r, int n);
    double getP();
    double getBeta();
    NumericVector getStayers();
    int getStayers(int i);
    int getNumStayers();
    double getStayerProb();
    int getStayerID();

    //set individual element
    void setP(double target);
    void setBeta(double target);
    Rcpp::List returnModel();

protected:
    NumericVector fixedObs;
    NumericVector stayers;
    NumericVector P;
    NumericVector beta;
    int numStayers;
    int stayerID;


    int checkIndStayer(int i);

private:
};
#endif // MM_MODEL_H
