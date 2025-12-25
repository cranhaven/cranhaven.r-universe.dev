//
//  ProbitML.cpp
//  
//
//  Created by kuojung on 2020/2/26.
//

// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "ProbitML_ARMA_KB.h"
//#include "tmvrnormGibbs.h"
#include "tmvrnormGibbs_KJLEE.h"
//RNGScope scope;
//#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


ProbitMLModelSelectionARMAKB::ProbitMLModelSelectionARMAKB(int iNum_of_iterations, List list_Data, bool b_Robustness, List list_InitialValues, List list_HyperPara, List list_UpdatePara, List list_TuningPara, vec vARMA_Order, bool b_Interactive)
{
    Num_of_iterations = iNum_of_iterations;
    Data = list_Data;
    InitialValues = list_InitialValues;
    HyperPara = list_HyperPara;
    UpdatePara = list_UpdatePara;
    TuningPara = list_TuningPara;
    Robustness = b_Robustness;
    Interactive = b_Interactive;
    
    phi_tune = as<double>(TuningPara["TuningPhi"]);
    psi_tune = as<double>(TuningPara["TuningPsi"]);
    
    ARMA_Order = vARMA_Order;

    Y = as<mat>(Data["Y"]);
    X = as<cube>(Data["X"]);
    Z = as<cube>(Data["Z"]);

    TimePointsAvailable = as<vec>(Data["TimePointsAvailable"]);
    
    
    Num_of_obs = Y.n_cols;
    Num_of_Timepoints = Y.n_rows;
    Num_of_RanEffs = Z.n_cols;
    Num_of_covariates = X.n_cols;
    
    
    updateystar = as<bool>(UpdatePara["UpdateYstar"]);
    updateb = as<bool>(UpdatePara["UpdateRandomEffect"]);
    updatenu = as<bool>(UpdatePara["UpdateNu"]);
    updatebeta = as<bool>(UpdatePara["UpdateBeta"]);
    updateSigma = as<bool>(UpdatePara["UpdateSigma"]);
    updatephi = as<bool>(UpdatePara["UpdatePhi"]);
    updatepsi = as<bool>(UpdatePara["UpdatePsi"]);
    
    SinglePhiPsi = as<bool>(UpdatePara["SinglePhiPsi"]);

    
    b_samples.set_size(Num_of_RanEffs, Num_of_obs, Num_of_iterations);
    b_samples.zeros();
    nu_samples.set_size(Num_of_obs, Num_of_iterations);
    nu_samples.zeros();
    beta_samples.set_size(Num_of_covariates,Num_of_iterations);
    beta_samples.zeros();
    Sigma_samples.set_size(Num_of_RanEffs, Num_of_RanEffs, Num_of_iterations);
    Sigma_samples.zeros();
    
    phi_samples.set_size(1, Num_of_obs, Num_of_iterations);
    phi_samples.zeros();
    psi_samples.set_size(1, Num_of_obs, Num_of_iterations);
    psi_samples.zeros();
    
    phi_mean.set_size(1, Num_of_obs);
    phi_mean.zeros();
    psi_mean.set_size(1, Num_of_obs);
    psi_mean.zeros();
    
    if(ARMA_Order(0)>0){
        phi_samples.set_size(ARMA_Order(0), Num_of_obs, Num_of_iterations);
        phi_samples.zeros();
        phi_samples.slice(0) = as<mat>(InitialValues["phi"]);
        phi_mean.set_size(ARMA_Order(0), Num_of_obs);
        phi_mean.zeros();

    }
    if(ARMA_Order(1)>0){
        psi_samples.set_size(ARMA_Order(1), Num_of_obs, Num_of_iterations);
        psi_samples.zeros();
        psi_samples.slice(0) = as<mat>(InitialValues["psi"]);
        psi_mean.set_size(ARMA_Order(1), Num_of_obs);
        psi_mean.zeros();
    }


    b_mean.set_size(Num_of_RanEffs, Num_of_obs);
    b_mean.zeros();
    nu_mean.set_size(Num_of_obs);
    nu_mean.zeros();
    beta_mean.set_size(Num_of_covariates);
    beta_mean.zeros();
    Sigma_mean.set_size(Num_of_RanEffs, Num_of_RanEffs);
    Sigma_mean.zeros();
    


    Y_star_sample = as<mat>(InitialValues["y.star"]);


    b_samples.slice(0) = as<mat>(InitialValues["b"]);
    nu_samples.col(0) = as<vec>(InitialValues["nu"]);
    beta_samples.col(0) = as<vec>(InitialValues["beta"]);
    Sigma_samples.slice(0) = as<mat>(InitialValues["Sigma"]);


    

    //Rcout<< "Read Hyperparameters." << endl;
    // Hyperparameters
    v_gamma = as<double>(HyperPara["v.gamma"]);
    sigma2_beta = as<double>(HyperPara["sigma2.beta"]);

    Vb = as<double>(HyperPara["InvWishart.df"]);
    Lambda = as<mat>(HyperPara["InvWishart.Lambda"]);
    
    Ib_diag.eye(Num_of_RanEffs, Num_of_RanEffs);

    acc_phi_rate = 0.;
    acc_psi_rate = 0.;
    
    AIC = 0.;
    BIC = 0.;
    CIC = 0.;
    DIC = 0.;
    MPL = 0.;
    logL =0.;
    RJ_R = 0.;
    ACC = 0.; 
}



mat ProbitMLModelSelectionARMAKB::CovARMA(int tp, vec phi, vec psi)
{
    mat Phi = eye(tp, tp);
    mat Psi = eye(tp, tp);
    mat CovARMA_tmp;
    for(int t=1; t<tp; t++){
        for(int j=(t-1); (t-j)<=ARMA_Order(0) && j>=0; j--)
            Phi(t, j) = -phi(t-j-1);
        for(int j=(t-1); (t-j)<=ARMA_Order(1) && j>=0; j--)
            Psi(t, j) = psi(t-j-1);
    }
    
    CovARMA_tmp = Phi.i()* Psi* Psi.t()*(Phi.t().i());
    return CovARMA_tmp;
}



mat ProbitMLModelSelectionARMAKB::Phi(int tp, vec phi)
{
    
    mat Phi = eye(tp, tp);
    for(int t=1; t<tp; t++){
        for(int j=(t-1); (t-j)<=ARMA_Order(0) && j>=0; j--)
            Phi(t, j) = -phi(t-j-1);
    }
    return Phi;
}

mat ProbitMLModelSelectionARMAKB::Psi(int tp, vec psi)
{
    
    mat Psi = eye(tp, tp);
    for(int t=1; t<tp; t++){
        for(int j=(t-1); (t-j)<=ARMA_Order(1) && j>=0; j--)
            Psi(t, j) = psi(t-j-1);
    }
    return Psi;
}


void ProbitMLModelSelectionARMAKB::Update_ystar_b_nu_beta_Sigma(int iter)
{
    //if(iter % 100 == 0)
    //    Rcout << "Update ystar, b, nu, beta, Sigmab simultaneously" << endl;
    vec mu_tmp_b, res_b, mu_tmp_ystar;

    vec res_beta, mu_tmp_beta = zeros<vec>(Num_of_covariates);

    mat Ri_tmp, Ri_tmp_inv;

    mat Sigma_tmp_beta = zeros<mat>(Num_of_covariates, Num_of_covariates);
    mat Sigma_tmp_b = zeros<mat>(Num_of_RanEffs, Num_of_RanEffs);
    mat Sigma_tmp = zeros<mat>(Num_of_RanEffs, Num_of_RanEffs);

    int tp;
    vec lower, upper;
    mat X_tmp, Z_tmp, Phi_tmp, Phi_tmp_inv, Psi_tmp, Psi_tmp_inv;
    vec b_vec;
    double beta_tmp;
    double alpha_tmp = 0.5*(v_gamma + Num_of_RanEffs);

    for(int i=0; i<Num_of_obs; i++){
        tp = TimePointsAvailable(i);
        X_tmp = X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = (Z.slice(i).rows(0, tp-1));
        if(SinglePhiPsi){
            Ri_tmp = CovARMA(tp, phi_samples.slice(iter).col(0), psi_samples.slice(iter).col(0));
            if(updatephi){
                Phi_tmp = Phi(tp,phi_samples.slice(iter).col(0));
                Phi_tmp_inv = Phi_tmp.i();
            }
            else{
                Phi_tmp = eye(tp, tp);
                Phi_tmp_inv = eye(tp, tp);
            }
            if(updatepsi){
                Psi_tmp = Psi(tp,psi_samples.slice(iter).col(0));
                Psi_tmp_inv = Psi_tmp.i();
            }
            else{
                Psi_tmp = eye(tp, tp);
                Psi_tmp_inv = eye(tp, tp);
            }
        }
            //CovARMA(tp, phi_samples.slice(iter).col(0), psi_samples.slice(iter).col(0));
        else{
            if(updatephi){
                Phi_tmp = Phi(tp,phi_samples.slice(iter).col(i));
                Phi_tmp_inv = Phi_tmp.i();
            }
            else{
                Phi_tmp = eye(tp, tp);
                Phi_tmp_inv = eye(tp, tp);
            }

            if(updatepsi){
                Psi_tmp = Psi(tp,psi_samples.slice(iter).col(i));
                Psi_tmp_inv = Psi_tmp.i();
            }
            else{
                Psi_tmp = eye(tp, tp);
                Psi_tmp_inv = eye(tp, tp);
            }
            Ri_tmp = CovARMA(tp, phi_samples.slice(iter).col(i), psi_samples.slice(iter).col(i));
        }
        if(updateystar){
            mu_tmp_ystar = X_tmp*beta_samples.col(iter) +  Phi_tmp_inv*Z_tmp*b_samples.slice(iter).col(i);

            lower.set_size(tp);
            upper.set_size(tp);
            lower.elem(find(Y(span(0, tp-1), i)>0)).zeros();
            lower.elem(find(Y(span(0, tp-1), i)==0)).ones();
            lower.elem(find(Y(span(0, tp-1), i)==0)) *= -datum::inf;
            
            upper.elem(find(Y(span(0, tp-1), i)==0)).zeros();
            upper.elem(find(Y(span(0, tp-1), i)>0)).ones();
            upper.elem(find(Y(span(0, tp-1), i)>0)) *= datum::inf;
            
            mu_tmp_ystar.elem( find( ( (Y(span(0, tp-1), i)-1) % mu_tmp_ystar) < 0 )).zeros();
            mu_tmp_ystar.elem( find( ( (Y(span(0, tp-1), i) ) % mu_tmp_ystar) < 0 )).zeros();
            
        
            if(tp == 1)
                Y_star_sample(0, i) = rtruncnorm(1, as_scalar(mu_tmp_ystar), as_scalar(Ri_tmp), as_scalar(lower),  as_scalar(upper))(0);
            else
                Y_star_sample(span(0, tp-1), i) = rtmvnorm_gibbs_KJLEE(1, mu_tmp_ystar, Ri_tmp, lower, upper, 100, zeros<vec>(tp), 5).t();

            if(Y_star_sample.col(i).has_nan())
                Y_star_sample.col(i).zeros();
            Y_star_sample.col(i) = clamp(Y_star_sample.col(i), -10, 10);
            //Y_star_sample.col(i).replace(find(Y_star_sample.col(i))>5, 5);
            //Y_star_sample.col(i).replace(find(Y_star_sample.col(i))<-5, -5);
            
            if(Y_star_sample(span(0, tp-1), i).has_nan()){
                Rcout << "iter = " << iter << "\t i = " << i << endl;
                Rcout << "mu_tmp_ystar = " << mu_tmp_ystar << endl;
                Rcout << "Omegai_tmp = \n" << Ri_tmp << endl;
                Rcout << "Y(span(0, tp-1), i) = " << Y(span(0, tp-1), i) << endl;
                Rcout << "Y_star_sample(span(0, tp-1), i)=" << Y_star_sample(span(0, tp-1), i) << endl;
            }
                
        }
 
        if(updateb){
            Sigma_tmp_b = (Z_tmp.t()*Psi_tmp_inv.t()*Psi_tmp_inv*Phi_tmp*Z_tmp + nu_samples(i, iter)*Sigma_samples.slice(iter).i());
            //Rcout << "============ Check 5.1 ============" << endl;
            if(!Sigma_tmp_b.is_sympd())
                Sigma_tmp_b.eye();
            else
                Sigma_tmp_b = inv_sympd(Sigma_tmp_b);
            //Rcout << "============ Check 6 ============" << endl;
            res_b = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter);
            //Rcout << "============ Check 7 ============" << endl;
            mu_tmp_b = Sigma_tmp_b*Z_tmp.t()*Psi_tmp_inv.t()*Psi_tmp_inv*Phi_tmp*res_b;
            
            
            if(Sigma_tmp_b.has_nan()){
                Sigma_tmp_b.eye();
                mu_tmp_b.zeros();
            }

            b_samples.slice(iter+1).col(i) = mvnrnd(mu_tmp_b, Sigma_tmp_b);
            b_vec = b_samples.slice(iter+1).col(i);
        }
        else{
            b_samples.slice(iter+1).col(i) = b_samples.slice(iter).col(i);
            b_vec = b_samples.slice(iter+1).col(i);
        }
            
        //Rcout << "b_vec = " << b_vec << endl;
        beta_tmp = 0.5*(as_scalar(b_vec.t()*Sigma_samples.slice(iter).i()*b_vec) + v_gamma);
        //Rcout << "============ Check 8 ============" << endl;
        //Rcout << "alpha_tmp = " << alpha_tmp << "\t" << "beta_tmp=" << beta_tmp << endl;
        if(updatenu){
            nu_samples(i, iter+1) = randg( 1, distr_param(alpha_tmp, 1./beta_tmp))(0);
        }
        else
            nu_samples(i, iter+1) = nu_samples(i, iter);

        Ri_tmp_inv = inv_sympd(Ri_tmp);

        Sigma_tmp_beta += (X_tmp.t()*Ri_tmp_inv*X_tmp);
        res_beta = Y_star_sample(span(0, tp-1), i)- Phi_tmp_inv*Z_tmp*b_vec;
        mu_tmp_beta += X_tmp.t()*Ri_tmp_inv*res_beta;
        Sigma_tmp += nu_samples(i, iter+1)*(b_vec*b_vec.t());
        
      }

    if(updatebeta){
        Sigma_tmp_beta.diag() += 1./sigma2_beta;
        Sigma_tmp_beta = inv_sympd(Sigma_tmp_beta);
        mu_tmp_beta = Sigma_tmp_beta * mu_tmp_beta;
        
        beta_samples.col(iter+1) = mvnrnd(mu_tmp_beta, Sigma_tmp_beta);
    }
    else
        beta_samples.col(iter+1) = beta_samples.col(iter);
    
    Sigma_tmp = (Sigma_tmp + Lambda);

    if(updateSigma)
        Sigma_samples.slice(iter+1) =iwishrnd( Sigma_tmp, (Num_of_obs + Vb));
    else
        Sigma_samples.slice(iter+1) = Sigma_samples.slice(iter);
}



void ProbitMLModelSelectionARMAKB::Update_phi(int iter)
{
    //if(iter % 100 == 0)
    //Rcout << "iter = " << iter << " Update phi" << endl;
    double phi_den = 0., phi_num = 0.;
    vec phi_cand;
    
    vec res;
    mat X_tmp, Z_tmp;
    int tp;
    mat Cov_tmp_inv;
    mat Phi_tmp, Phi_tmp_inv;
    
    for(int i=0; i<Num_of_obs; i++){
        //phi_cand = mvnrnd(phi_samples.slice(iter).col(i), 0.1*eye(ARMA_Order(0),ARMA_Order(0)));
        
        if(ARMA_Order(0)==1){
            do{
                phi_cand = mvnrnd(phi_samples.slice(iter).col(i), phi_tune*eye(ARMA_Order(0),ARMA_Order(0)));
                //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
            }
            while(abs(as_scalar(phi_cand))>1);
        }
        else{
            do{
                //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
                phi_cand = mvnrnd(phi_samples.slice(iter).col(i), phi_tune*eye(ARMA_Order(0),ARMA_Order(0)));
                //bool_phi = (sum(phi_cand)<1) && (as_scalar(diff(phi_cand)<1)) && (abs(phi_cand(1))<1);
            }
            while((sum(phi_cand)>1) || (as_scalar(diff(phi_cand)>1)) || (abs(phi_cand(1))>1));
        }
        
        tp = TimePointsAvailable(i);
        X_tmp = X.slice(i).rows(0, tp-1);  //X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = Z.slice(i).rows(0, tp-1);
        
        Phi_tmp = Phi(tp,phi_samples.slice(iter).col(i));
        Phi_tmp_inv = Phi_tmp.i();
        res = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter+1)-Z_tmp*b_samples.slice(iter+1).col(i);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter).col(i), psi_samples.slice(iter).col(i)));
        phi_den = 0.5*log(det(Cov_tmp_inv)) - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_cand, psi_samples.slice(iter).col(i)));
        phi_num = 0.5*log(det(Cov_tmp_inv)) - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        //Rcout << "phi_num = " << phi_num << "\t" << "phi_den = " << phi_den << "\tphi_num - phi_den = " << phi_num - phi_den << endl;
        
        if(log(randu()) < phi_num - phi_den ){
            phi_samples.slice(iter+1).col(i) = phi_cand;
        }
        else
            phi_samples.slice(iter+1).col(i) = phi_samples.slice(iter).col(i);
        
    }
    
    
}

void ProbitMLModelSelectionARMAKB::Update_psi(int iter)
{
    //if(iter % 100 == 0)
        //Rcout << "iter = " << iter << " Update psi" << endl;
    double psi_den = 0., psi_num = 0.;
    vec psi_cand;
    
    vec res;
    mat X_tmp, Z_tmp;
    int tp;
    mat Cov_tmp_inv;
    mat Phi_tmp, Phi_tmp_inv;
    
    for(int i=0; i<Num_of_obs; i++){
        
        if(ARMA_Order(1)==1){
            do{
                psi_cand = mvnrnd(psi_samples.slice(iter).col(i), psi_tune*eye(ARMA_Order(1),ARMA_Order(1)));
                //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
            }
            while(abs(as_scalar(psi_cand))>1);
        }
        else{
            do{
                //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
                psi_cand = mvnrnd(psi_samples.slice(iter).col(i), psi_tune*eye(ARMA_Order(1),ARMA_Order(1)));
                //bool_phi = (sum(phi_cand)<1) && (as_scalar(diff(phi_cand)<1)) && (abs(phi_cand(1))<1);
            }
            while((sum(psi_cand)>1) || (as_scalar(diff(psi_cand)>1)) || (abs(psi_cand(1))>1));
        }

        
        tp = TimePointsAvailable(i);
        X_tmp = X.slice(i).rows(0, tp-1);  //X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = Z.slice(i).rows(0, tp-1);
        Phi_tmp = Phi(tp,phi_samples.slice(iter+1).col(i));
        Phi_tmp_inv = Phi_tmp.i();

        res = Y_star_sample(span(0, tp-1), i) - Phi_tmp_inv*X_tmp*beta_samples.col(iter+1)-Z_tmp*b_samples.slice(iter+1).col(i);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter+1).col(i), psi_samples.slice(iter).col(i)));
        psi_den = 0.5*log(det(Cov_tmp_inv)) - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter+1).col(i), psi_cand));
        psi_num = 0.5*log(det(Cov_tmp_inv)) - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        //Rcout << "phi_num = " << phi_num << "\t" << "phi_den = " << phi_den << "\tphi_num - phi_den = " << phi_num - phi_den << endl;
        
        if(log(randu()) < psi_num - psi_den ){
            psi_samples.slice(iter+1).col(i) = psi_cand;
        }
        else
            psi_samples.slice(iter+1).col(i) = psi_samples.slice(iter).col(i);
        
    }
}

void ProbitMLModelSelectionARMAKB::Update_single_phi(int iter)
{
    //if(iter % 100 == 0)
        //Rcout << "iter = " << iter << " Update single phi" << endl;
    double phi_den = 0., phi_num = 0.;
    vec phi_cand;
    
    vec res;
    mat X_tmp, Z_tmp;
    mat Phi_tmp, Phi_tmp_inv;
    int tp;
    mat Cov_tmp_inv;
    
    phi_cand = mvnrnd(phi_samples.slice(iter).col(0), phi_tune*eye(ARMA_Order(0),ARMA_Order(0)));
    
    for(int i=0; i<Num_of_obs; i++){
        //phi_cand = mvnrnd(phi_samples.slice(iter).col(i), 0.1*eye(ARMA_Order(0),ARMA_Order(0)));
        
        
        tp = TimePointsAvailable(i);
        X_tmp = X.slice(i).rows(0, tp-1);  //X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = Z.slice(i).rows(0, tp-1);
        
        Phi_tmp = Phi(tp,phi_samples.slice(iter).col(0));
        Phi_tmp_inv = Phi_tmp.i();
        
        res = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter+1)-Phi_tmp_inv*Z_tmp*b_samples.slice(iter+1).col(i);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter).col(0), psi_samples.slice(iter).col(0)));
        phi_den += - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        Phi_tmp = Phi(tp,phi_cand);
        Phi_tmp_inv = Phi_tmp.i();
        
        res = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter+1)-Phi_tmp_inv*Z_tmp*b_samples.slice(iter+1).col(i);

        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_cand, psi_samples.slice(iter).col(0)));
        phi_num += - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        //Rcout << "phi_num = " << phi_num << "\t" << "phi_den = " << phi_den << "\tphi_num - phi_den = " << phi_num - phi_den << endl;
    }
    //Rcout << "phi_num = " << phi_num << "\t" << "phi_den = " << phi_den << "\tphi_num - phi_den = " << phi_num - phi_den << endl;
    if(log(randu()) < phi_num - phi_den ){
        phi_samples.slice(iter+1).col(0) = phi_cand;
        acc_phi_rate++;
    }
    else
        phi_samples.slice(iter+1).col(0) = phi_samples.slice(iter).col(0);
    
    //Rcout << "phi_cand = " << phi_cand << " phi = " << phi_samples.slice(iter+1).col(0) << endl;
    
    if((iter+1)%500 == 0){
        //Rcout << "tuning_delta = " << tuning_delta << endl;
        //Rcout << "acc_rate_delta/iter = " << acc_rate_delta/iter << endl;
        if( acc_phi_rate/iter<0.25 )
            phi_tune = phi_tune/2.;
        if( (1.*acc_phi_rate)/iter>0.50 )
            phi_tune = 2*phi_tune;
        
        //Rcout << "tuning_delta = " << tuning_delta << endl;
        
    }
    //Rcout <<"Done for phi" << endl;
}

void ProbitMLModelSelectionARMAKB::Update_single_psi(int iter)
{
    //if(iter % 100 == 0)
        //Rcout << "iter = " << iter << " Update single psi" << endl;
    double psi_den = 0., psi_num = 0.;
    vec psi_cand;
    
    vec res;
    mat X_tmp, Z_tmp;
    mat Phi_tmp, Phi_tmp_inv;
    int tp;
    mat Cov_tmp_inv;
    
    /*
    if(ARMA_Order(1)==1){
        do{
            psi_cand = mvnrnd(psi_samples.slice(iter).col(0), psi_tune*eye(ARMA_Order(1),ARMA_Order(1)));
            //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
        }
        while(abs(as_scalar(psi_cand))>1);
    }
    else{
        do{
            //phi_cand =  phi_samples(i, iter)+0.01*(2*randu()-1);
            psi_cand = mvnrnd(psi_samples.slice(iter).col(0), psi_tune*eye(ARMA_Order(1),ARMA_Order(1)));
            //bool_phi = (sum(phi_cand)<1) && (as_scalar(diff(phi_cand)<1)) && (abs(phi_cand(1))<1);
        }
        while((sum(psi_cand)>1) || (as_scalar(diff(psi_cand)>1)) || (abs(psi_cand(1))>1));
    }
     */
    psi_cand = mvnrnd(psi_samples.slice(iter).col(0), psi_tune*eye(ARMA_Order(1),ARMA_Order(1)));
    for(int i=0; i<Num_of_obs; i++){
        
        tp = TimePointsAvailable(i);
        X_tmp = X.slice(i).rows(0, tp-1);  //X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = Z.slice(i).rows(0, tp-1);
        
        if(updatephi){
            Phi_tmp = Phi(tp,phi_samples.slice(iter+1).col(0));
            Phi_tmp_inv = Phi_tmp.i();
        }
        else{
            Phi_tmp = eye(tp, tp);
            Phi_tmp_inv = eye(tp, tp);
        }
        
        //Phi_tmp = Phi(tp,phi_samples.slice(iter+1).col(0));
        //Phi_tmp_inv = Phi_tmp.i();
        
        res = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter+1)-Phi_tmp_inv*Z_tmp*b_samples.slice(iter+1).col(i);
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter+1).col(0), psi_samples.slice(iter).col(0)));
        psi_den += - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
        
        
        Cov_tmp_inv = inv_sympd( CovARMA(tp, phi_samples.slice(iter+1).col(0), psi_cand));
        psi_num += - 0.5*as_scalar(res.t()* Cov_tmp_inv*res);
   
        //Rcout << "phi_num = " << phi_num << "\t" << "phi_den = " << phi_den << "\tphi_num - phi_den = " << phi_num - phi_den << endl;
    }
    if(log(randu()) < psi_num - psi_den ){
        psi_samples.slice(iter+1).col(0) = psi_cand;
        acc_psi_rate++;
    }
    else
        psi_samples.slice(iter+1).col(0) = psi_samples.slice(iter).col(0);
    
    //Rcout << "Done for psi" << endl;
    if( acc_psi_rate/iter<0.25 )
        psi_tune = psi_tune/2.;
    if( (1.*acc_psi_rate)/iter>0.50 )
        psi_tune = 2*psi_tune;
}


void ProbitMLModelSelectionARMAKB::ParameterEstimation()
{

    b_mean = mean(b_samples, 2);
    Sigma_mean = mean(Sigma_samples, 2);
    beta_mean = mean(beta_samples, 1);
    nu_mean = mean(nu_samples, 1);
    phi_mean = mean(phi_samples, 2);
    psi_mean = mean(psi_samples, 2);
    

    rowvec X_tmp, Z_tmp;
    //vec mu_tmp;
    double pit, CPO_tmp, ESS=0, GP=0, ESS_GP_tmp, RJ1, RJ2;
    logL = 0.;
    
    mat Djt(Num_of_Timepoints, Num_of_covariates, fill::zeros);
    mat Omega_I(Num_of_covariates, Num_of_covariates, fill::zeros), M_LZ(Num_of_covariates, Num_of_covariates, fill::zeros);
    vec mu_it(Num_of_Timepoints, fill::zeros), p_it(Num_of_Timepoints, fill::zeros);
    mat A_sqrt, Cov_Y, V, V_inv, Omega_I_inv, V_LZ, Gamma_RJ, Djt_sub;

    int tp;
    
    CIC = 0.;
    RJ_R = 0.;
    ACC = 0.;
    
    mat CPO = zeros<mat>(Num_of_obs, TimePointsAvailable.max());


    for(int i=0; i<Num_of_obs; i++){
        //Rcout << "i = " << i << endl;
        tp = TimePointsAvailable(i);
        for(int t=0; t<tp; t++){
            
            X_tmp = X.slice(i).row(t);
            Z_tmp = Z.slice(i).row(t);
            //Rcout << "X_tmp = " << X_tmp << endl;
            //Rcout << "beta_mean = " << beta_mean << endl;
            mu_it(t) = as_scalar( X_tmp*beta_mean + Z_tmp*b_mean.col(i));
            p_it(t) = normcdf(mu_it(t));
            
            ACC += 1.*(1.*(mu_it(t)>0) == Y(t, i));
            
            for(int j=0; j<Num_of_covariates; j++)
                Djt(t, j) = X(t,j,i)/normpdf(Rf_pnorm5(mu_it(t), 0., 1., 1, 0));
        }

        Djt_sub = Djt.head_rows(tp);

        Cov_Y = (Y(span(0, tp-1), i)-p_it.head(tp))*(Y(span(0, tp-1), i)-p_it.head(tp)).t();

        A_sqrt = diagmat(sqrt(p_it.head(tp)));
        V = A_sqrt*A_sqrt;

        V_inv = V.i();
        Omega_I += Djt_sub.t()*V_inv*Djt_sub;

        M_LZ += Djt_sub.t()*V_inv*Cov_Y*V_inv*Djt_sub;

        ESS_GP_tmp = as_scalar((Y(span(0, tp-1), i)-mu_it.head(tp)).t()*V_inv*(Y(span(0, tp-1), i)-mu_it.head(tp)));

        ESS += ESS_GP_tmp;
        GP += -0.5*ESS_GP_tmp + log(det(V));
    }
    Omega_I_inv = Omega_I.i();
    Gamma_RJ = Omega_I_inv*M_LZ;
    V_LZ = Gamma_RJ*Omega_I_inv;
    
    RJ1 = trace(Gamma_RJ)/Num_of_covariates;
    RJ2 = accu((diagvec(Gamma_RJ*Gamma_RJ)))/Num_of_covariates;

    RJ_R = sqrt((1-RJ1)*(1-RJ1)+(1-RJ2)*(1-RJ2));

    CIC = trace(Omega_I*V_LZ);
 
    
    for(int i=0; i<Num_of_obs; i++){
        for(int t=0; t<TimePointsAvailable(i); t++){
            X_tmp = X.slice(i).row(t);
            Z_tmp = Z.slice(i).row(t);
            for(int iter = Num_of_iterations/2; iter<Num_of_iterations; iter++){
                pit = normcdf( as_scalar( X_tmp*beta_samples.col(iter) + Z_tmp*b_samples.slice(iter).col(i) ), 0., 1.);
                if(pit == 1 && Y(t, i) == 1){
                    DIC += 0.;
                    CPO_tmp = 0.;
                }
                else if(pit == 0 && Y(t, i) == 0){
                    DIC += 0.;
                    CPO_tmp = 0.;
                }
                else{
                    CPO_tmp = Y(t, i)*log(pit) + (1-Y(t, i))*log(1-pit);
                    DIC += CPO_tmp;
                    
                }
                
                CPO(i, t) += exp(-CPO_tmp);
            }
    
            
            
            pit = normcdf(as_scalar( X_tmp*beta_mean + Z_tmp*b_mean.col(i) ), 0., 1.);
            
            if(pit == 1 && Y(t, i) == 1)
                logL += 0.;
            else if(pit == 0 && Y(t, i) == 0)
                logL += 0.;
            else
                logL += Y(t, i)*log(pit) + (1-Y(t, i))*log(1-pit);
        }
    }


    CPO = 1./CPO;

    //Rcout << "CPO = " << endl << CPO.submat(0, 0, 9, 3) << endl;

    CPO.elem( find_nonfinite(CPO) ).zeros();

    MPL = accu(CPO);
    DIC = -4*DIC/(Num_of_iterations/2) + 2*logL;
    AIC = -2*logL + 2 * (Num_of_covariates+Num_of_obs*Num_of_RanEffs + accu(ARMA_Order) );
    BIC = -2*logL + log(Num_of_obs) * (Num_of_covariates+Num_of_obs*Num_of_RanEffs+ accu(ARMA_Order) );

}


SEXP ProbitMLModelSelectionARMAKB::MCMC_Procedure()
{
    Rcout << "Start running MCMC procedure:"<< endl;
    
    List PosteriorSamples;
    List PosteriorEstimates;
    List MH_AcceptanceRates;
    List Posterior;
    
    //time_t start = time(NULL);
    
    int iter = 0, percent;
    
    
    while(iter < Num_of_iterations-1){
            Update_ystar_b_nu_beta_Sigma(iter);

        if(SinglePhiPsi){
            if(updatephi)
                Update_single_phi(iter);
            else
                phi_samples.slice(iter+1) = phi_samples.slice(iter);
            
            if(updatepsi)
                Update_single_psi(iter);
            else
                psi_samples.slice(iter+1) = psi_samples.slice(iter);
        }
        else{
            if(updatephi)
                Update_phi(iter);
            else
                phi_samples.slice(iter+1) = phi_samples.slice(iter);
            
            if(updatepsi)
                Update_psi(iter);
            else
                psi_samples.slice(iter+1) = psi_samples.slice(iter);
        }
        
        
        percent = (100 *iter) / (Num_of_iterations-2) ;
        iter++;

        
        if(percent%2==0 && Interactive){
            Rcout << "\r" <<  "[" << std::string(percent / 2, (char)61) << std::string(100 / 2 - percent / 2, ' ') << "]" << "\t" << percent << "%";
            //Rcout << percent << "%" << " [Iteration " << iter + 1 << " of " << Num_of_iterations << "]";
            Rcout.flush();
        }
        
        
        //if(iter %100 == 0)
            //Rcout << "iter = " << iter << endl;

    }
    Rcout << endl << "Finish MCMC Procedure." << endl;
    
    ParameterEstimation();
    
    //Rcout << "============= FMR: MCMC: Done =============="<< endl;
    //time_t end = time(NULL);
    //Rcout<<"Execution Time: "<< (double)(end-start)<<" Seconds"<<std::endl;
    
    PosteriorSamples["ystar.samples"] = Y_star_sample;
    PosteriorSamples["b.samples"] = b_samples;
    PosteriorSamples["nu.samples"] = nu_samples;
    PosteriorSamples["beta.samples"] = beta_samples;
    PosteriorSamples["Sigma.samples"] = Sigma_samples;
    
    
 
    PosteriorEstimates["beta.mean"] = beta_mean;
    PosteriorEstimates["nu.mean"] = nu_mean;
    PosteriorEstimates["b.mean"] = b_mean;
    PosteriorEstimates["Sigma.mean"] = Sigma_mean;
    if(ARMA_Order(0)>0){
        PosteriorSamples["phi.samples"] = phi_samples;
        PosteriorEstimates["phi.mean"] = phi_mean;
        MH_AcceptanceRates["Acceptance.rate.for.phi"] = acc_phi_rate/Num_of_iterations;
    }
    
    if(ARMA_Order(1)>0){
        PosteriorSamples["psi.samples"] = psi_samples;
        PosteriorEstimates["psi.mean"] = psi_mean;
        MH_AcceptanceRates["Acceptance.rate.for.psi"] = acc_psi_rate/Num_of_iterations;
    }


    PosteriorEstimates["AIC"] = AIC;
    PosteriorEstimates["BIC"] = BIC;
    PosteriorEstimates["CIC"] = CIC;
    PosteriorEstimates["logL"] = logL;

    PosteriorEstimates["DIC"] = DIC;
    PosteriorEstimates["RJR"] = RJ_R;

    PosteriorEstimates["MPL"] = MPL;
    PosteriorEstimates["ACC"] = ACC/accu(TimePointsAvailable);


    
    Posterior["PosteriorEstimates"] = PosteriorEstimates;
    Posterior["PosteriorSamples"] = PosteriorSamples;
    if(accu(ARMA_Order)>0)
        Posterior["MH_AcceptanceRates"] = MH_AcceptanceRates;
 
    //Rcout << endl << "=======================================" << endl;
    //Rcout << "acceptance rate for phi= " << acc_phi_rate/Num_of_iterations << endl;
    //Rcout << "acceptance rate for psi= " << acc_psi_rate/Num_of_iterations << endl;
    //Rcout << "=======================================" << endl;
    return (Posterior);
}

