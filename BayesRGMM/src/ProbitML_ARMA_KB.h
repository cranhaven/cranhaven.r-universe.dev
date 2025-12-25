//
//  ProbitML.hpp
//  
//
//  Created by kuojung on 2020/2/26.
//

#ifndef ProbitML_ARMA_KB_hpp
#define ProbitML_ARMA_KB_hpp

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



class ProbitMLModelSelectionARMAKB{
private:
    int Num_of_iterations, Num_of_Timepoints;
    int Num_of_obs, Num_of_covariates;
    int Num_of_RanEffs;
    mat Y, Y_star_sample; //TxN
    cube X, Z; //TxPxN, TxQxN
    vec TimePointsAvailable, ARMA_Order;//group_indices,
    List Data, InitialValues, HyperPara, UpdatePara, TuningPara;
    cube Sigma_samples, b_samples, phi_samples, psi_samples;
    mat beta_samples, nu_samples;
    double phi_tune, psi_tune;
    
    mat Sigma_mean, b_mean, phi_mean, psi_mean;
    vec beta_mean, nu_mean;
    double sigma2_beta, v_gamma, Vb;
    mat Lambda, Ib_diag;
    
    double acc_phi_rate, acc_psi_rate;
    
    bool updateystar, updateb, updatenu, updatebeta, updateSigma, updatephi, updatepsi; //, updateomega;
    bool SinglePhiPsi, Robustness, Interactive;
    //bool Unconstraint;
    
    double AIC, BIC, CIC, DIC, MPL, logL, RJ_R, ACC;// MSPE;
    //cube pred_y;
    //vec lower, upper;
    
public:
    ProbitMLModelSelectionARMAKB(int iNum_of_iterations, List list_Data, bool bRobustness, List list_InitialValues, List list_HyperPara, List list_UpdatePara, List list_TuningPara, vec vARMA_Order, bool bInteractive);
    
    void Update_ystar_b_nu_beta_Sigma(int iter);

    void Update_phi(int iter);
    void Update_psi(int iter);
    void Update_single_psi(int iter);
    void Update_single_phi(int iter);
    
    void ParameterEstimation();

    mat CovARMA(int tp, vec phi, vec psi);
    mat Phi(int tp, vec psi);
    mat Psi(int tp, vec psi);
    
    SEXP MCMC_Procedure();
};

#endif /* ProbitML_hpp */
