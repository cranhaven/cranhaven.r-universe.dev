#ifndef MM_MODEL_H
#define MM_MODEL_H


#include <stdlib.h>
#include <math.h>
#include <RcppArmadillo.h>

using namespace Rcpp ;
using namespace arma;

class mm_model
{
public:
    //constructor
    mm_model(List model);
    int indN(int i, int j, int r);
    int indTheta(int j, int k, int v);
    int indPhi(int i, int k);
    int indDelta(int i, int j, int r, int n, int k);
    int indObs(int i, int j, int r, int n);

    //get individual element
    NumericVector getAlpha();
    double getAlpha(int k);
    int getT();
    int getJ();
    int getK();
    int getR(int j);
    int getV(int j);
    int getN(int i, int j, int r);
    double getTheta(int j, int k, int v);
    double getPhi(int i, int k);
    double getDelta(int i, int j, int r, int n, int k);
    int getObs(int i, int j, int r, int n);
    std::string getDist(int j);



    //set individual element
    void setAlpha(int k, double target);
    void setTheta(int j, int k, int v, double target);
    void setPhi(int i, int k, double target);
    void setDelta(int i, int j, int r, int n, int k, double target);

    //update helpers
    void normalizeDelta(int i, int j, int r, int n, double delta_sum);
    void normalizeTheta(int j, int k, double theta_sum);
    void incPhi(int i, int k, double inc);
    void incAlpha(int k, double inc);
    void incTheta(int j, int k, int v, double inc);
    Rcpp::List returnModel();

protected:
    int T;
    int J;
    IntegerVector Rj ;
    int maxR ;
    IntegerVector Nijr ;
    int maxN ;
    int K ;
    IntegerVector Vj ;
    int maxV ;
    NumericVector alpha;
    NumericVector theta;
    NumericVector phi ;
    NumericVector delta ;
    NumericVector obs ;
    CharacterVector dist;
private:
};
#endif // MM_MODEL_H
