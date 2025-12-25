//
//  ProbitML.hpp
//  
//
//  Created by kuojung on 2020/2/26.
//

#ifndef ProbitML_hpp
#define ProbitML_hpp

//#include <math.h>

#include <stdio.h>
#include <unistd.h>   // for isatty()
//#include <algorithm>
//#include <assert.h>
#include <cmath>
#include <ctime>    // For time()
//#include <cstdlib>  // For srand() and rand()
//#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <iomanip>
//#include <list>
//#include <limits>
#include <vector>
#include <string>
//#include <sstream>
#include <algorithm>


#include<iostream>
//#include<chrono> //for sleeping
#include<thread> // --do--
#include<cstdlib>//for random increments


//#include <RcppArmadillo.h>
#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]

using namespace std;

using namespace Rcpp;
using namespace arma;


class ProbitMLModelSelection{
private:
    int Num_of_iterations, Num_of_Timepoints;
    int Num_of_obs, Num_of_covariates;
    int Num_of_RanEffs, Num_of_deltas;
    mat Y, Y_star_sample; //TxN
    cube X, Z, U; //TxPxN, TxQxN, TxTx(a*N)
    field<cube> UU; //TxTxNxa
    vec TimePointsAvailable;//group_indices,
    List Data, InitialValues, HyperPara, UpdatePara, TuningPara;
    cube Sigma_samples, b_samples;
    mat beta_samples, nu_samples, delta_samples;
    
    mat Sigma_mean, b_mean;
    vec beta_mean, nu_mean, delta_mean;
    double sigma2_beta, sigma2_delta, v_gamma, Vb;
    double tuning_delta;
    mat Lambda, Ib_diag, Idelta_diag;
    
    double acc_rate_delta; 
    
    bool updateystar, updateb, updatenu, updatebeta, updateSigma, updatedelta; //, updateomega;
    bool Unconstraint, Robustness, Interactive;
    
    double AIC, BIC, CIC, DIC, MPL, logL, RJ_R, ACC;// MSPE;
    //cube pred_y;
    //vec lower, upper;
    
public:
    ProbitMLModelSelection(int iNum_of_iterations, List list_Data, bool bRobustness, List list_InitialValues, List list_HyperPara, List list_UpdatePara, List list_TuningPara, bool bInteractive);
    

    void Update_nu(int iter);

    void Update_Sigma(int iter);
    void Update_delta(int iter);
    void Update_ystar_b_beta_Sigma(int iter);
    
    void ParameterEstimation();
    
    mat Ri_Version2(int i, int tp, vec delta);
    
    SEXP MCMC_Procedure();
};

#endif /* ProbitML_hpp */
