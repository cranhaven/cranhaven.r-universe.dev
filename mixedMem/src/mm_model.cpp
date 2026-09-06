#include "settings.h"
#include "mm_model.h"

using namespace Rcpp ;
using namespace arma;


mm_model::mm_model(List model)
{
    T = (int) as<NumericVector>(model[0])[0];
    J = (int) as<NumericVector>(model[1])[0];
    Rj = as<NumericVector>(model[2]);
    maxR = max(Rj);
    Nijr = as<NumericVector>(model[3]);
    maxN = max(Nijr);
    K = (int) as<NumericVector>(model[4])[0];
    Vj = as<NumericVector>(model[5]);
    maxV = max(Vj);

    alpha = Rcpp::clone(as<NumericVector>(model[6]));
    theta = Rcpp::clone(as<NumericVector>(model[7]));
    phi = Rcpp::clone(as<NumericVector>(model[8]));
    delta = Rcpp::clone(as<NumericVector>(model[9]));
    dist = as<CharacterVector>(model[10]);
    obs = Rcpp::clone(as<NumericVector>(model[11]));
}

int mm_model::indN(int i, int j, int r)
{
    return(i + T*j + (T*J)*r);
}

int mm_model::indTheta(int j, int k, int v)
{
    return(j + J*k + (J*K)*v);
}

int mm_model::indPhi(int i, int k)
{
    return(i + T*k);
}

int mm_model::indDelta(int i, int j, int r, int n, int k)
{
    return(i + T*j + (T*J)*r + (T*J*maxR)*n+ (T*J*maxR*maxN)*k);
}

int mm_model::indObs(int i, int j, int r, int n)
{
    return(i + T*j + (T*J)*r + (T*J*maxR)*n);
}

//get functions
std::string mm_model::getDist(int j)
{
    return as<std::string>(dist[j]);
}

double mm_model::getAlpha(int k)
{
    return(alpha[k]);
}

NumericVector mm_model::getAlpha()
{
    return(alpha);
}
int mm_model::getK()
{
    return(K);
}

int mm_model::getJ()
{
    return(J);
}

int mm_model::getT()
{
    return(T);
}
int mm_model::getR(int j)
{
    return(Rj[j]);
}

int mm_model::getV(int j)
{
    return(Vj[j]);
}
int mm_model::getN(int i, int j, int r)
{
    return(Nijr[i + T*j + (T*J)*r]);
}

double mm_model::getTheta(int j, int k, int v)
{
    return(theta[j + J*k + (J*K)*v]);
}

double mm_model::getPhi(int i, int k)
{
    return(phi[i + T*k]);
}

double mm_model::getDelta(int i, int j, int r, int n, int k)
{
    return(delta[i + T*j + (T*J)*r + (T*J*maxR)*n+ (T*J*maxR*maxN)*k]);
}

int mm_model::getObs(int i, int j, int r, int n)
{
    return(obs[i + T*j + (T*J)*r + (T*J*maxR)*n]);
}


//Set Functions
void mm_model::setAlpha(int k, double target)
{
    alpha[k] = target;
}

void mm_model::setTheta(int j, int k, int v, double target)
{
    theta[j + J*k + (J*K)*v] = target;
}

void mm_model::setPhi(int i, int k, double target)
{
    phi[i + T*k] = target;
}

void mm_model::setDelta(int i, int j, int r, int n, int k, double target)
{
    delta[i + T*j + (T*J)*r + (T*J*maxR)*n+ (T*J*maxR*maxN)*k] = target;
}


//Update Helpers
void mm_model::normalizeDelta(int i, int j, int r, int n, double delta_sum)
{
    int check = 0;
    int k;

    if(!(delta_sum>0))
    {
        for(k = 0; k < K; k++)
        {
            delta[indDelta(i,j,r,n,k)] = 1.0/K;
        }
    }
    else
    {
        for(k = 0; k < K; k++)
        {

            delta[indDelta(i,j,r,n,k)] /= delta_sum;

            if(delta[indDelta(i,j,r,n,k)] == 0)
            {
                check++;
                delta[indDelta(i,j,r,n,k)] = BUMP;
            }
            else if(delta[indDelta(i,j,r,n,k)] ==1)
            {
                check--;
                delta[indDelta(i,j,r,n,k)] = 1.0 - BUMP;
            }
        }

        if(check != 0)
        {
            for(k = 0; k <  K; k++)
            {
                delta[indDelta(i,j,r,n,k)] /= (1.0+check*BUMP);
            }
        }
    }
}

void mm_model::normalizeTheta(int j, int k, double theta_sum)
{
    int v;
    int check = 0;
    for(v = 0; v < Vj[j]; v++)
    {
        theta[indTheta(j,k,v)] /= theta_sum;
        //Check for 0 or 1
        if(theta[indTheta(j,k,v)]==1)
        {
            theta[indTheta(j,k,v)] = 1.0-BUMP;
            check--;
        }
        else if(theta[indTheta(j,k,v)] == 0)
        {
            theta[indTheta(j,k,v)] = BUMP;
            check++;
        }
    }

    if(check != 0 )
    {
        for(v = 0; v < Vj[j]; v++)
        {
            theta[indTheta(j,k,v)] /= (1.0 + check*BUMP);
        }
    }


}

void mm_model::incTheta(int j, int k, int v, double inc)
{
    theta[indTheta(j,k,v)] += inc;
}

void mm_model::incPhi(int i, int k, double inc)
{
    phi[indPhi(i,k)] += inc;
}

void mm_model::incAlpha(int k, double inc)
{
    alpha[k] += inc;
}

Rcpp::List mm_model::returnModel()
{
    return Rcpp::List::create(Rcpp::Named("alpha", alpha),
                              Rcpp::Named("theta", theta),
                              Rcpp::Named("phi", phi),
                              Rcpp::Named("delta", delta));
}

