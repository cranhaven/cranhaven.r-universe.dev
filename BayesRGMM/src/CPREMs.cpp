//
//  CPREMs.cpp
//
//
//  Created by kuojung on 2021/6/07.
//

// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "CPREMs.h"
#include "tmvrnormGibbs_KJLEE.h"
//RNGScope scope;
//#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

CumulativeProbitModel::CumulativeProbitModel(int iNum_of_iterations, List list_Data, bool b_Robustness,
                                             List list_InitialValues, List list_HyperPara, List list_UpdatePara, List list_TuningPara, bool b_Interactive)
{
    Rcout<< "Start reading Data" << endl;
    Num_of_iterations = iNum_of_iterations;
    Data = list_Data;
    InitialValues = list_InitialValues;
    HyperPara = list_HyperPara;
    UpdatePara = list_UpdatePara;
    TuningPara = list_TuningPara;
    Robustness = b_Robustness;
    Interactive = b_Interactive;
    
    updateystar = as<bool>(UpdatePara["UpdateYstar"]);
    updatealpha = as<bool>(UpdatePara["UpdateAlpha"]);
    updateb = as<bool>(UpdatePara["UpdateRandomEffect"]);
    updatenu = as<bool>(UpdatePara["UpdateNu"]);
    updatebeta = as<bool>(UpdatePara["UpdateBeta"]);
    updateSigma = as<bool>(UpdatePara["UpdateSigma"]);
    updatedelta = as<bool>(UpdatePara["UpdateDelta"]);
    
    Y = as<umat>(Data["Y"]);
    X = as<cube>(Data["X"]);
    Z = as<cube>(Data["Z"]);
    
    Y_pred.set_size( size(Y) );
    Y_pred.fill(datum::nan);
    //Rcout << size(Y_pred) << endl; //.fill(1000);
    
    //Rcout << "Y.org = \n" << Y << endl;
    
    //Rcout << "Y = \n" << Y.elem(find_finite(Y)) << endl;
    uvec cats = find(unique(Y)<100);
    //find(unique(Y)<100);
    
    //Rcout << "Y = \n" << Y << endl;
    //Rcout << "cats = " << cats << endl;
    Num_of_Cats = cats.n_elem;

    Y = Y - (Y.min()-0);
    
    
    
    //Rcout << "Y = \n" << Y << endl;
   
    Rcout<< "End reading Data" << endl;
    
    Num_of_obs = Y.n_cols;
    Num_of_Timepoints = Y.n_rows;
    Num_of_RanEffs = Z.n_cols;
    Num_of_covariates = X.n_cols;
 
    
    //Rcout << "Num_of_Cats = " << Num_of_Cats << endl;
    Num_of_deltas = 0;
    U.reset();
    UU.reset();
    delta_samples.reset();
    delta_mean.reset();
    sigma2_delta = 1.;
    tuning_delta = 1.;
    Idelta_diag.reset();
    acc_rate_delta = 0;
    if(updatedelta){
        //Rcout << "updatedelta" << endl;
        U = as<cube>(Data["U"]);
        Num_of_deltas = U.n_slices/Num_of_obs;
        UU.set_size(Num_of_deltas);
        
        for(int delta_index = 0; delta_index<Num_of_deltas; delta_index++){
            //Rcout << "delta_index = " << delta_index << endl;
            UU(delta_index) = U.slices( (Num_of_obs*delta_index), (Num_of_obs*(delta_index+1)-1));
        }
        delta_samples.set_size(Num_of_deltas, Num_of_iterations);
        delta_samples.zeros();
        delta_mean.set_size(Num_of_deltas);
        delta_mean.zeros();
        delta_samples.col(0) = as<vec>(InitialValues["delta"]);

        sigma2_delta = as<double>(HyperPara["sigma2.delta"]);


        tuning_delta = as<double>(TuningPara["TuningDelta"]);
        Idelta_diag.eye(Num_of_deltas, Num_of_deltas);
        acc_rate_delta = 0.;
 
    }
    
    tuning_alpha = 1.; //as<double>(TuningPara["TuningAlpha"]);
    acc_rate_alpha = 0.;
    
    TimePointsAvailable = as<vec>(Data["TimePointsAvailable"]);
    
    b_samples.set_size(Num_of_RanEffs, Num_of_obs, Num_of_iterations);
    b_samples.zeros();
    nu_samples.set_size(Num_of_obs, Num_of_iterations);
    nu_samples.ones();
    beta_samples.set_size(Num_of_covariates,Num_of_iterations);
    beta_samples.zeros();
    Sigma_samples.set_size(Num_of_RanEffs, Num_of_RanEffs, Num_of_iterations);
    Sigma_samples.zeros();
    alpha_samples.set_size(Num_of_Cats+1,Num_of_iterations);
    alpha_samples.zeros();
        
    b_mean.set_size(Num_of_RanEffs, Num_of_obs);
    b_mean.zeros();
    nu_mean.set_size(Num_of_obs);
    nu_mean.ones();
    beta_mean.set_size(Num_of_covariates);
    beta_mean.zeros();
    Sigma_mean.set_size(Num_of_RanEffs, Num_of_RanEffs);
    Sigma_mean.zeros();
    alpha_mean.set_size(Num_of_Cats+1);
    alpha_mean.zeros();
    
    //Rcout<< "Initial Values alpha" << endl;
    
    Y_star_sample = as<mat>(InitialValues["y.star"]);
    b_samples.slice(0) = as<mat>(InitialValues["b"]);
    //Rcout<< "Initial Values nu" << endl;
    if(Robustness)
        nu_samples.col(0) = as<vec>(InitialValues["nu"]);
    //Rcout<< "Initial Values beta" << endl;
    beta_samples.col(0) = as<vec>(InitialValues["beta"]);
    //Rcout<< "Initial Values Sigma" << endl;
    Sigma_samples.slice(0) = as<mat>(InitialValues["Sigma"]);
    //Rcout<< "Initial Values delta" << endl;
    alpha_samples.col(0) = as<vec>(InitialValues["alpha"]);
    
    
    //Rcout << "Sigma_samples = " << size(Sigma_samples) << "\t" << Sigma_samples.slice(0) << endl;
    
    
    Rcout<< "Read Hyperparameters." << endl;
    // Hyperparameters
    v_gamma = as<double>(HyperPara["v.gamma"]);
    sigma2_beta = as<double>(HyperPara["sigma2.beta"]);

    Vb = as<double>(HyperPara["InvWishart.df"]);
    Lambda = as<mat>(HyperPara["InvWishart.Lambda"]);

    //Rcout<< "Read Tuning parameters." << endl;
    
    sigma2_alpha = as<double>(HyperPara["sigma2.alpha"]);;
    Ib_diag.eye(Num_of_RanEffs, Num_of_RanEffs);
    
    AIC = 0.;
    BIC = 0.;
    CIC = 0.;
    DIC = 0.;
    MPL = 0.;
    logL =0.;
    RJ_R = 0.;
    ACC = 0.; 
    
}

mat CumulativeProbitModel::Ri_Version2(int i, int tp, vec delta)
{
    //Rcout << "delta = " << delta << endl;
    mat F_tmp(tp, tp), F(tp, tp);
    F.zeros();
    F_tmp.zeros();

    for(int delta_ind_U = 0; delta_ind_U<Num_of_deltas; delta_ind_U++)
        F_tmp += delta(delta_ind_U)*UU(delta_ind_U).slice(i)(0, 0, size(tp, tp) );
    
    F_tmp = datum::pi*exp(F_tmp)/(1.+exp(F_tmp));
    F(0, 0) = 1;
    
    for(int t=1; t<tp; t++)
        F(t, 0) = cos(F_tmp(t, 0));
    for(int j = 1; j<tp-1; j++)
        for(int t = j+1; t<tp; t++)
            F(t, j) = cos(F_tmp(t, j))*prod(sin(F_tmp(t, span(0, j-1) )));
    for(int t=1; t<tp; t++)
        F(t, t) = prod(sin(F_tmp(t, span(0, t-1) )));
    mat Ri = F * F.t();
    return (Ri);
}


void CumulativeProbitModel::Update_ystar_b_beta_Sigma(int iter)
{
    //if(iter == 1)
        //Rcout << "Update ystar, b, beta, Sigmab simultaneously" << endl;
    vec mu_tmp_b, res_b, mu_tmp_ystar;
    vec res_beta, mu_tmp_beta = zeros<vec>(Num_of_covariates);

    mat Ri_tmp, Ri_inv;

    mat Sigma_tmp_beta = zeros<mat>(Num_of_covariates, Num_of_covariates);
    mat Sigma_tmp_b = zeros<mat>(Num_of_RanEffs, Num_of_RanEffs);
    mat Sigma_tmp = zeros<mat>(Num_of_RanEffs, Num_of_RanEffs);

    int tp;

    vec lower, upper, alpha_sample_tmp;
    mat X_tmp, Z_tmp;
    vec b_vec;
    
    double alpha_num =0., alpha_den =0.;
    
    
    
    alpha_sample_tmp = alpha_samples.col(iter);
    alpha_num =0.;
    alpha_den =0.;
    
    rowvec x_tmp, z_tmp;
    double mu_tmp_alpha;
    
    if(updatealpha){
        //Rcout << "Update alpha start" <<endl;
        alpha_samples(0, iter+1) = -datum::inf;
        alpha_samples(Num_of_Cats, iter+1) = datum::inf;
        //Rcout << "tuning_alpha = " << tuning_alpha << endl;
        for(int i=1; i< Num_of_Cats; i++)
        {
            alpha_samples(i, iter+1) = r_truncnorm(alpha_samples(i, iter), tuning_alpha, alpha_samples(i-1, iter+1),  alpha_samples(i+1, iter) );
        }
        
        //Rcout << "alpha_samples(i, iter+1) = " << alpha_samples.col(iter+1) << endl;
        //double p_truncnorm(const double x, const double mu, const double sigma,
        // const double a, const double b, const int lower_tail = 1,
        // const int log_p = 0)
        for(unsigned int cat_ind = 0; cat_ind<Num_of_Cats; cat_ind++)
            for(int subj_ind =0; subj_ind<Num_of_obs; subj_ind++)
                for(int tp_ind =0; tp_ind<TimePointsAvailable(subj_ind); tp_ind++){
                    if(Y(tp_ind, subj_ind)==cat_ind){
                    //Rcout << X.slice(subj_ind).row(tp_ind) << endl;
                        x_tmp = X(span(tp_ind), span(0, Num_of_covariates-1), span(subj_ind));
                        //Rcout << "x_tmp = " << x_tmp << endl;
                        z_tmp = (Z.slice(subj_ind).row(tp_ind));
                        
                        mu_tmp_alpha = as_scalar(x_tmp*beta_samples.col(iter) + z_tmp*b_samples.slice(iter).col(subj_ind));
                        
                        alpha_num += log(normcdf(-mu_tmp_alpha+alpha_samples(cat_ind+1, iter+1)) - normcdf(-mu_tmp_alpha+alpha_samples(cat_ind, iter+1)));
                        alpha_den += log(normcdf(-mu_tmp_alpha+alpha_sample_tmp(cat_ind+1)) - normcdf(-mu_tmp_alpha+alpha_sample_tmp(cat_ind)));
                    }
                }
                    
        alpha_num += - 0.5*accu(square(alpha_samples(span(1, Num_of_Cats-1), iter+1)))/sigma2_alpha;
        alpha_den += - 0.5*accu(square(alpha_sample_tmp(span(1, Num_of_Cats-1))))/sigma2_alpha;
        acc_rate_alpha++;
        if( log(Rf_runif(0., 1.)) > (alpha_num-alpha_den) ){
            alpha_samples.col(iter+1) = alpha_sample_tmp;
            acc_rate_alpha--;
        }
        
        if((iter+1)%500 == 0){
            if( acc_rate_alpha/iter<0.25 )
                tuning_alpha = tuning_alpha/2.;
            if( (1.*acc_rate_alpha)/iter>0.50 )
                tuning_alpha = 2.*tuning_alpha;
            
        }
        
        
    }
    else
        alpha_samples.col(iter+1) = alpha_samples.col(iter);
    
    //Rcout << "Update alpha end" << endl;

    //Rcout << "Update ystar start" << endl;

    for(int i=0; i<Num_of_obs; i++){
        tp = TimePointsAvailable(i);

        //Rcout << "i = " << i << endl;

        if(updatedelta){
            //Rcout << "delta_samples.col(iter))  aaa = " << delta_samples.col(iter) << endl;
            Ri_tmp = Ri_Version2(i, tp, delta_samples.col(iter));
        }
        else
            Ri_tmp = eye(tp, tp);
        
        //Rcout << "Ri_tmp aaa =" << Ri_tmp << endl;

        if(!Ri_tmp.is_sympd())
            Ri_tmp.eye();
        Ri_inv = inv_sympd(Ri_tmp);
        
        //Rcout << "Check 1" << endl;
        //lower.elem(find(Y(span(0, tp-1), i)>0)).zeros();
        //lower.elem(find(Y(span(0, tp-1), i)==0)).ones();
        //lower.elem(find(Y(span(0, tp-1), i)==0)) *= -datum::inf;
        
        //upper.elem(find(Y(span(0, tp-1), i)==0)).zeros();
        //upper.elem(find(Y(span(0, tp-1), i)>0)).ones();
        //upper.elem(find(Y(span(0, tp-1), i)>0)) *= datum::inf;

        
        X_tmp = X(span(0, tp-1), span(0, Num_of_covariates-1), span(i));
        Z_tmp = (Z.slice(i).rows(0, tp-1));
        mu_tmp_ystar = X_tmp*beta_samples.col(iter) + Z_tmp*b_samples.slice(iter).col(i);

        //Rcout << "End alpha sampling" << endl;
        //Rcout << "Alpha_sample = " << Alpha_sample.t() << endl;
        //Rcout << "Obs = " << i << endl;
        lower.set_size(tp);
        upper.set_size(tp);
        vec  alpha_tmp = alpha_samples.col(iter+1);
        //Rcout << "Y(span(0, tp-1), i) = " << Y(span(0, tp-1), i).t() << endl;
        upper = alpha_tmp( Y(span(0, tp-1), i)+1);
        //
        //Rcout << "Y(span(0, tp-1), i)-1 = " << Y(span(0, tp-1), i).t()+1 << endl;
        lower = alpha_tmp( Y(span(0, tp-1), i));
        
        //Rcout << "upper = " << upper << endl;
        //Rcout << "lower = " << lower << endl;
        //Rcout << "Check 2" << endl;
        if(updateystar){
            
            //Rcout << "Update ystar" <<endl;
            //Rcout << "Ri_tmp = " << Ri_tmp << endl;
            if(tp == 1)
                Y_star_sample(0, i) = rtruncnorm(1, as_scalar(mu_tmp_ystar), as_scalar(Ri_tmp), as_scalar(lower),  as_scalar(upper))(0);
            else
                Y_star_sample(span(0, tp-1), i) = rtmvnorm_gibbs_KJLEE(1, mu_tmp_ystar, Ri_tmp, lower, upper, 100, zeros<vec>(tp), 5).t();
            
            if(Y_star_sample.col(i).has_nan())
                Y_star_sample.col(i).zeros();
            Y_star_sample.col(i) = clamp(Y_star_sample.col(i), -10, 10);

        }
        //Rcout << "Check 3" << endl;
        //Rcout << "Y_star_sample.col(i) = " << Y_star_sample.col(i) << endl;
        
        //Rcout << "Sigma_samples.slice(iter) a = " << Sigma_samples.slice(iter) << endl;
        
        Sigma_tmp_b = Z_tmp.t()*Ri_inv*Z_tmp+nu_samples(i, iter)*Sigma_samples.slice(iter).i();
        //Rcout << "Check 4" << endl;
        //Rcout << "check 1 = " << endl;
        
        if(!Sigma_tmp_b.is_sympd())
            Sigma_tmp_b.eye();
        else
            Sigma_tmp_b = inv_sympd(Sigma_tmp_b);
        
        //Rcout << "check 5  " << endl;
        
        res_b = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter);
        mu_tmp_b = Sigma_tmp_b*Z_tmp.t()*Ri_inv*res_b;
        
        if(Sigma_tmp_b.has_nan()){
            Sigma_tmp_b.eye();
            mu_tmp_b.zeros();
        }
        //Rcout << "updateb start" << endl;
        if(updateb)
            b_samples.slice(iter+1).col(i) = mvnrnd(mu_tmp_b, Sigma_tmp_b);
        else
            b_samples.slice(iter+1).col(i) = b_samples.slice(iter).col(i);
        //Rcout << "updateb end" << endl;
        //Rcout << "b_samples.slice(iter+1).col(i)  = " << b_samples.slice(iter+1).col(i)  << endl;
        Sigma_tmp_beta += (X_tmp.t()*Ri_inv*X_tmp);
                
        res_beta = Y_star_sample(span(0, tp-1), i)- Z_tmp*b_samples.slice(iter+1).col(i);
        
        mu_tmp_beta += X_tmp.t()*Ri_inv*res_beta;
        
        b_vec = b_samples.slice(iter+1).col(i);
        
        Sigma_tmp += nu_samples(i, iter)*(b_vec*b_vec.t());
        
    }
            
    //Rcout << "Update ystar end" << endl;

    Sigma_tmp_beta.diag() += 1./sigma2_beta;
    Sigma_tmp_beta = inv_sympd(Sigma_tmp_beta);
    mu_tmp_beta = Sigma_tmp_beta * mu_tmp_beta;
        

    if(updatebeta)
        beta_samples.col(iter+1) = mvnrnd(mu_tmp_beta, Sigma_tmp_beta);
    else
        beta_samples.col(iter+1) = beta_samples.col(iter);
    
    Sigma_tmp = (Sigma_tmp + Lambda);

    if(!Sigma_tmp.is_symmetric() || Sigma_tmp.has_nan()){
        Sigma_tmp.eye();
    }

    //Rcout << "Sigma_samples.slice(iter) = " << Sigma_samples.slice(iter) << endl;
    
    //Rcout << "Sigma_tmp = " << Sigma_tmp << "\t" << "Num_of_obs + Vb " << Num_of_obs + Vb << endl;
    
    
    if(updateSigma)
        Sigma_samples.slice(iter+1) = iwishrnd( Sigma_tmp, (Num_of_obs + Vb));
    else
        Sigma_samples.slice(iter+1) = Sigma_samples.slice(iter);
    
    //Rcout << "Sigma_samples.slice(iter+1)" << Sigma_samples.slice(iter+1) << endl;
}

void CumulativeProbitModel::Update_nu(int iter)
{
    //if(iter == 1)
        //Rcout << "Update nu" << endl;
    double alpha_tmp, beta_tmp;
    vec b_vec;
    alpha_tmp = 0.5*(v_gamma + Num_of_RanEffs);

    for(int i=0; i<Num_of_obs; i++){
        b_vec = b_samples.slice(iter+1).col(i); //( span::all, span(i), span(iter+1));
        beta_tmp = 0.5*(as_scalar(b_vec.t()*Sigma_samples.slice(iter+1).i()*b_vec) + v_gamma);
        nu_samples(i, iter+1) = randg( 1, distr_param(alpha_tmp, 1./beta_tmp))(0);  //Rf_rgamma(alpha_tmp, 1./beta_tmp); //
    }
    //Rcout << "Update nu done" << endl;
}



void CumulativeProbitModel::Update_delta(int iter)
{
    //if(iter == 0)
        //Rcout << "Update delta" << endl;
    double delta_den = 0., delta_num = 0.;
    vec delta_cand =  mvnrnd(delta_samples.col(iter), tuning_delta*Idelta_diag);
    
    vec res;
    mat Ri_inv, X_tmp, Z_tmp;
    int tp;
    for(int i=0; i<Num_of_obs; i++){
        tp = TimePointsAvailable(i);
        X_tmp = X.slice(i).rows(0, tp-1);
        Z_tmp = Z.slice(i).rows(0, tp-1);

        res = Y_star_sample(span(0, tp-1), i) - X_tmp*beta_samples.col(iter+1)-Z_tmp*b_samples.slice(iter+1).col(i);

        Ri_inv = Ri_Version2(i, tp, delta_samples.col(iter));
        
        if(!Ri_inv.is_sympd())
            Ri_inv.eye();
        else
            Ri_inv = inv_sympd(Ri_inv);
         
        delta_den += 0.5*log(det(Ri_inv)) - 0.5*as_scalar(res.t()* Ri_inv*res);
        Ri_inv = Ri_Version2(i, tp, delta_cand);
        
        if(!Ri_inv.is_sympd())
            Ri_inv.eye();
        else
            Ri_inv = inv_sympd(Ri_inv);
        
        delta_num += 0.5*log(det(Ri_inv)) - 0.5*as_scalar(res.t()* Ri_inv*res);
    }
        
    delta_den = delta_den - 0.5*accu(square(delta_samples.col(iter)))/sigma2_delta;
    delta_num = delta_num - 0.5*accu(square(delta_cand))/sigma2_delta;
    
    if(log(Rf_runif(0., 1.)) < delta_num - delta_den ){
        delta_samples.col(iter+1) = delta_cand;
        acc_rate_delta++;
    }
    else
        delta_samples.col(iter+1) = delta_samples.col(iter);
    
    if((iter+1)%500 == 0){
        if( acc_rate_delta/iter<0.25 )
            tuning_delta = tuning_delta/2.;
        if( (1.*acc_rate_delta)/iter>0.50 )
            tuning_delta = 2*tuning_delta;
        
    }
    //Rcout << "Update delta done" << endl;
}

void CumulativeProbitModel::ParameterEstimation()
{
    //Rcout << "Time = " << TimePointsAvailable << endl;
     
    b_mean = mean(b_samples.tail_slices(Num_of_iterations/2), 2);
    Sigma_mean = mean(Sigma_samples.tail_slices(Num_of_iterations/2), 2);
    beta_mean = mean(beta_samples.tail_cols( Num_of_iterations/2 ), 1);
    if(Robustness)
        nu_mean = mean(nu_samples.tail_cols( Num_of_iterations/2 ), 1);
    
    if(updatedelta)
        delta_mean = mean(delta_samples.tail_cols( Num_of_iterations/2 ), 1);

    mat SigmaEstInverse = Sigma_mean.i();
    //vec aa;
    //Rcout << "alpha_samples = \n" << alpha_samples << endl;
    //alpha_samples( find_nonfinite(alpha_samples) ) = 10000;
    //alpha_mean.ones();  //mean(alpha_samples, 1);
    //Rcout << "alpha_mean = \n" << mean(alpha_samples, 1) << endl;
   
    //vec aa;
    //aa_mean = mean(alpha_samples, 1);
    
    //vec aa;
    alpha_mean = mean(alpha_samples.tail_cols( Num_of_iterations/2 ), 1);
    
    //alpha_mean = alpha_samples.tail_cols(1)
    //alpha_mean = aa;
    //Rcout << "aa = \n" << aa << endl;
    alpha_mean.head(1) = -datum::inf;
    alpha_mean.tail(1) = datum::inf;
    
    

    CIC = 0.;
    RJ_R = 0.;
    ACC = 0.;
    if(1){
    rowvec X_tmp, Z_tmp, Ri_tmp;

    vec pit_vec(Num_of_Cats, fill::zeros), pit_vec_tmp(Num_of_Cats, fill::zeros);
        
    double CPO_tmp, ESS_GP_tmp, RJ1, RJ2, ESS=0., GP=0.;
    logL = 0.;
    mat Djt(Num_of_Timepoints, Num_of_covariates, fill::zeros);
    mat Omega_I(Num_of_covariates, Num_of_covariates, fill::zeros), M_LZ(Num_of_covariates, Num_of_covariates, fill::zeros);
    vec mu_it(Num_of_Timepoints, fill::zeros), p_it(Num_of_Timepoints, fill::zeros);
    mat A_sqrt, Cov_Y, V, V_inv, Omega_I_inv, V_LZ, Gamma_RJ, Djt_sub;
    
    mat CPO = zeros<mat>(Num_of_obs, TimePointsAvailable.max());
    int tp;
    



    //Rcout << "============== 0 ============"<<endl;
    for(int i=0; i<Num_of_obs; i++){
        //Rcout << "i = " << i << endl;
        tp = TimePointsAvailable(i);
        for(int t=0; t<tp; t++){
            
            X_tmp = X.slice(i).row(t);
            Z_tmp = Z.slice(i).row(t);
            //Rcout << "X_tmp = " << X_tmp << endl;
            //Rcout << "Z_tmp = " << Z_tmp << endl;
            mu_it(t) = as_scalar( X_tmp*beta_mean+Z_tmp*b_mean.col(i));
            //p_it(t) = normcdf(mu_it(t));
            
            for(unsigned int cat_ind = 0; cat_ind<Num_of_Cats; cat_ind++){
                //Rcout << "=========================================" << endl;
                //Rcout << "mu_it = " << mu_it(t) << endl;
                //Rcout << "alpha = " << alpha_mean.t() << endl;
                //Rcout << "i = " << i << " t = " << t << endl;
                if((mu_it(t) < alpha_mean[cat_ind+1]) && (mu_it(t) > alpha_mean[cat_ind]) ){
                    //Rcout << "cat = " << cat_ind << endl;
                    ACC +=  1.*(Y(t, i)== cat_ind);
                    Y_pred(t, i) = cat_ind;
                }
                
                
                //Rcout << "=========================================" << endl;
            }
            
            p_it(t) = normcdf(alpha_mean(Y_pred(t, i)+1), 0., 1.) - normcdf(alpha_mean(Y_pred(t, i)), 0., 1.);
                                 
            //ACC += 1.*(1.*(mu_it(t)>0) == Y(t, i));
            
            for(int j=0; j<Num_of_covariates; j++)
                Djt(t, j) = X(t,j,i)/(normpdf(Rf_pnorm5(alpha_mean(Y_pred(t, i)+1), 0., 1., 1, 0)) - normpdf(Rf_pnorm5(alpha_mean(Y_pred(t, i)), 0., 1., 1, 0)));
        }
        //Rcout << "============== 1 ============"<<endl;
        Djt_sub = Djt.head_rows(tp);
        //Rcout << "============== 2 ============"<<endl;
        Cov_Y = (Y(span(0, tp-1), i)-p_it.head(tp))*(Y(span(0, tp-1), i)-p_it.head(tp)).t();
        //Rcout << "============== 3 ============" << endl;
        A_sqrt = diagmat(sqrt(p_it.head(tp)));
        if(updatedelta)
            V = A_sqrt*Ri_Version2(i, tp, delta_mean)*A_sqrt;
        else
            V = A_sqrt*A_sqrt;
        //Rcout << "============== 4 ============"<<endl;
        V_inv = V.i();
        Omega_I += Djt_sub.t()*V_inv*Djt_sub;
        //Rcout << "============== 5 ============"<<endl;
        M_LZ += Djt_sub.t()*V_inv*Cov_Y*V_inv*Djt_sub;
        //Rcout << "============== 6 ============"<<endl;
        ESS_GP_tmp = as_scalar((Y(span(0, tp-1), i)-mu_it.head(tp)).t()*V_inv*(Y(span(0, tp-1), i)-mu_it.head(tp)));
        //Rcout << "============== 7 ============"<<endl;
        ESS += ESS_GP_tmp;
        GP += -0.5*ESS_GP_tmp + log(det(V));
    }
    //Rcout << "============== 2 ============"<<endl;
    //Rcout << " Omega_I = " << endl << Omega_I << endl;
    Omega_I_inv = Omega_I.i();
    //Rcout << "============== 3 ============"<<endl;
    Gamma_RJ = Omega_I_inv*M_LZ;
    //Rcout << "============== 4 ============"<<endl;
    V_LZ = Gamma_RJ*Omega_I_inv;
    //Rcout << "============== 5 ============"<<endl;
    RJ1 = trace(Gamma_RJ)/Num_of_covariates;
    RJ2 = accu((diagvec(Gamma_RJ*Gamma_RJ)))/Num_of_covariates;

    RJ_R = sqrt((1-RJ1)*(1-RJ1)+(1-RJ2)*(1-RJ2));
    //SC = ESS/(N-P-a
    //GP = -0.5*GP

    CIC = trace(Omega_I*V_LZ);
    //Rcout << "RJ_R = " << RJ_R << "\tCIC = " << CIC << endl;
    //cat("RJ.R = ", RJ.R, "\t", "SC = ", SC, "\n")
    for(int cat_ind = 0; cat_ind<Num_of_Cats; cat_ind++)
        pit_vec(cat_ind) = normcdf(alpha_mean(cat_ind+1), 0., 1.) - normcdf(alpha_mean(cat_ind), 0., 1.);
    
    //Rcout << "pit_vec = " << pit_vec << endl;
    //Rcout << "============== 6 ============"<<endl;
    for(int i=0; i<Num_of_obs; i++){
        


        for(int t=0; t<TimePointsAvailable(i); t++){
            
            X_tmp = X.slice(i).row(t);
            Z_tmp = Z.slice(i).row(t);
                    
            
            for(int iter = Num_of_iterations/2; iter<Num_of_iterations; iter++){
                
                for(int cat_ind = 0; cat_ind<Num_of_Cats; cat_ind++)
                    pit_vec_tmp(cat_ind) = normcdf(alpha_samples(cat_ind+1, iter), 0., 1.) - normcdf(alpha_samples(cat_ind, iter), 0., 1.);
                
                //pit = normcdf(as_scalar( X_tmp*beta_samples.col(iter) + Z_tmp*b_samples.slice(iter).col(i) ) , 0., 1.);
                CPO_tmp = log(pit_vec_tmp(Y(t, i))); //Y(t, i)*log(pit) + (1-Y(t, i))*log(1-pit);
                DIC += CPO_tmp;
                
                /*
                if(pit == 1 && Y(t, i) == 1){
                    DIC += 0.;
                    CPO_tmp = 0.;
                }
                else if(pit == 0 && Y(t, i) == 0){
                    DIC += 0.;
                //else if(pit == 0 && Y(t, i) == 1)
                //    Likelihood += 0.;
                //else if(pit == 1 && Y(t, i) == 0)
                //    Likelihood += 0.;
                    CPO_tmp = 0.;
                }
                else{
                    //Likelihood *= pow(pit, Y(t, i))*pow( (1-pit), (1-Y(t, i)) );
                    CPO_tmp = Y(t, i)*log(pit) + (1-Y(t, i))*log(1-pit);
                    DIC += CPO_tmp;
                    
                }
                */
                CPO(i, t) += exp(-CPO_tmp);
            }
    
            
            
            //pit = normcdf(as_scalar( X_tmp*beta_mean + Z_tmp*b_mean.col(i) ), 0., 1.);
            
            logL += log(pit_vec(Y(t, i)));
            
            //Rcout << 0.5*b_mean.col(i).t()*SigmaEstInverse*b_mean.col(i) << endl;
            //- accu(square(alpha_mean(span(1, Num_of_Cats-1))))/sigma2_alpha - 0.5*accu(square(beta_mean))/sigma2_beta - 0.5*accu(square(delta_mean))/sigma2_delta-0.5*(Vb+Num_of_RanEffs+1)*log(det(Sigma_mean))-0.5*trace(Lambda*Sigma_mean.i());
            
            //Rcout << "i=" << i << ", t=" << t << "\tpit=" << pit << endl;
            
            //if(pit == 1 && Y(t, i) == 1)
            //    logL += 0.;
            //else if(pit == 0 && Y(t, i) == 0)
            //    logL += 0.;
            //else if(pit == 0 && Y(t, i) == 1)
            //    Likelihood += 0.;
            //else if(pit == 1 && Y(t, i) == 0)
            //    Likelihood += 0.;
            //else
                //Likelihood *= pow(pit, Y(t, i))*pow( (1-pit), (1-Y(t, i)) );
            //    logL += Y(t, i)*log(pit) + (1-Y(t, i))*log(1-pit);
        }
        logL += -0.5 *log(det(Sigma_mean)) - 0.5*as_scalar(b_mean.col(i).t()*SigmaEstInverse*b_mean.col(i));

        
    }
    if(updatedelta)
        logL += -0.5*accu(square(alpha_mean(span(1, Num_of_Cats-1))))/sigma2_alpha-0.5*accu(square(beta_mean))/sigma2_beta-0.5*accu(square(delta_mean))/sigma2_delta -0.5*(Vb+Num_of_RanEffs+1)*log(det(Sigma_mean))-0.5*trace(Lambda*SigmaEstInverse);
    else
        logL += -0.5*accu(square(alpha_mean(span(1, Num_of_Cats-1))))/sigma2_alpha-0.5*accu(square(beta_mean))/sigma2_beta -0.5*(Vb+Num_of_RanEffs+1)*log(det(Sigma_mean))-0.5*trace(Lambda*SigmaEstInverse);
    //Rcout << "============== 7 ============"<<endl;
    CPO = 1./CPO;
    
    //Rcout << "CPO = " << endl << CPO.submat(0, 0, 9, 3) << endl;
    
    CPO.elem( find_nonfinite(CPO) ).zeros();
    
    MPL = accu(CPO);
    DIC = -4*DIC/(Num_of_iterations/2) + 2*logL;
    AIC = -2*logL + 2 * (Num_of_covariates+Num_of_obs*Num_of_RanEffs + Num_of_deltas);
    BIC = -2*logL + log(Num_of_obs) * (Num_of_covariates+Num_of_obs*Num_of_RanEffs+ Num_of_deltas);
    
    //Rcout << "============== 8 ============"<<endl;
    }
}


SEXP CumulativeProbitModel::MCMC_Procedure()
{
    Rcout << "Start running MCMC procedure:"<< endl;
    
    int percent = 0;
    List PosteriorSamples;
    List PosteriorEstimates;
    List MH_AcceptanceRates;
    List Posterior;
    
    int iter = 0;

    while(iter < Num_of_iterations-1){
        
        //Rcout << "iter = " << iter << endl;
        
        Update_ystar_b_beta_Sigma(iter);
        
        //Rcout << "Update nu start" << endl;
        if(Robustness)
            Update_nu(iter);
        else
            nu_samples.col(iter+1) = nu_samples.col(iter);
        //Rcout << "Update nu end" << endl;
        
        
        if(updatedelta)
            Update_delta(iter);
        //Rcout << "Update delta end" << endl;
        
        //percent = (100 *iter) / (Num_of_iterations-2) ;
        //iter++;
        //if(iter%10000==0)
        //    Rcout << "iter = " << iter << endl;
        
        
        percent = (100 *iter) / (Num_of_iterations-2) ;
        iter++;
        
        

        
        if(percent%2==0 && Interactive){
            Rcout << "\r" <<  "[" << std::string(percent / 2, (char)61) << std::string(100 / 2 - percent / 2, ' ') << "]" << "\t" << percent << "%";
            //Rcout << percent << "%" << " [Iteration " << iter + 1 << " of " << Num_of_iterations << "]";
            Rcout.flush();
        }
         

    }
    Rcout << endl << "Finish MCMC Procedure." << endl;
  
    ParameterEstimation();
    
    Rcout << endl << "Finish Parameter Estimation." << endl;
    
    PosteriorSamples["alpha.samples"] = alpha_samples;
    PosteriorSamples["ystar.samples"] = Y_star_sample;
    PosteriorSamples["b.samples"] = b_samples;
    if(Robustness)
        PosteriorSamples["nu.samples"] = nu_samples;
    PosteriorSamples["beta.samples"] = beta_samples;
    PosteriorSamples["Sigma.samples"] = Sigma_samples;
    PosteriorSamples["delta.samples"] = delta_samples;
    if(updatedelta)
        PosteriorEstimates["delta.mean"] = delta_mean;

    PosteriorEstimates["beta.mean"] = beta_mean;
    if(Robustness)
        PosteriorEstimates["nu.mean"] = nu_mean;
    PosteriorEstimates["b.mean"] = b_mean;
    PosteriorEstimates["Sigma.mean"] = Sigma_mean;
    PosteriorEstimates["alpha.mean"] = alpha_mean;
    
    Rcout << "AIC = " << AIC  << "\tBIC = " << BIC << "\tCIC = " << CIC << "\tlogL = " << logL << endl;
    Rcout << "DIC = " << DIC << "\tRJR = " << RJ_R  << "\tMPL = " << MPL << "\tACC = " << ACC/accu(TimePointsAvailable) << endl;
    

    PosteriorEstimates["AIC"] = AIC;
    PosteriorEstimates["BIC"] = BIC;
    PosteriorEstimates["CIC"] = CIC;
    PosteriorEstimates["logL"] = logL;
    PosteriorEstimates["DIC"] = DIC;
    PosteriorEstimates["RJR"] = RJ_R;
    PosteriorEstimates["MPL"] = MPL;
    PosteriorEstimates["ACC"] = ACC/accu(TimePointsAvailable);
    
    //Rcout << "Y_pred = " << Y_pred << endl;
    PosteriorEstimates["Y.Pred"] = Y_pred;
    
    //Rcout << "============== 10 ===========" << endl;
    
    MH_AcceptanceRates["Acceptance.rate.for.delta"] = acc_rate_delta/Num_of_iterations;
    MH_AcceptanceRates["Acceptance.rate.for.alpha"] = acc_rate_alpha/Num_of_iterations;
 
    Posterior["PosteriorEstimates"] = PosteriorEstimates;
    Posterior["PosteriorSamples"] = PosteriorSamples;
    Posterior["MH_AcceptanceRates"] = MH_AcceptanceRates;
    
    //Rcout << "============== 11 ===========" << endl;

    return (Posterior);
}

