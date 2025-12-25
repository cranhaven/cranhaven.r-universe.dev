#include "tmvrnormGibbs_KJLEE.h"

/*
 * This file contains code relevant to a C++ implementation of a Gibbs sampler (a specific case of Metropolis-Hastings MCMC) to
 * sample from a truncated multivariate Gaussian distribution.
 */

mat rtmvnorm_gibbs_KJLEE(int n, vec mean_vec, mat sigma, vec lower, vec upper, int burn_in_samples, vec start_value, int thinning)
{
    //Rcout << "rtmvnorm_gibbs_KJLEE 1" << endl;
    int d = mean_vec.n_elem;
    
    int S = burn_in_samples;
    mat X = zeros<mat>(n, d);
        
    mat Sigma, sigma_ii, Sigma_i;
    vec U = randu((S + n*thinning) * d);
    //Rcout << "U = " << U << endl;
    int l = 0;
    
    mat P(d, d-1);
    vec sd(d);
    
    //Rcout << "rtmvnorm_gibbs_KJLEE 2" << endl;
    for(int i=0; i<d; i++){
        Sigma = sigma;
        Sigma.shed_col(i);
        Sigma_i = Sigma.row(i);
        Sigma.shed_row(i);
        sigma_ii = sigma(i,i);
        P.row(i) = Sigma_i *inv_sympd(Sigma); //# (1 x (d-1)) * ((d-1) x (d-1)) =  (1 x (d-1))
        sd(i) = sqrt(  as_scalar(sigma_ii - P.row(i)*Sigma_i.t()) );
    }
    //Rcout << "rtmvnorm_gibbs_KJLEE 3" << endl;
    //Rcout << "sd = "  << sd << endl;
    
    //Rcout << "P = " << endl << P << endl;
    //Rcout << "normcdf(1.23, 4.56, 7.89) = " << normcdf(1.23, 4.56, 7.89) << endl;
    
    
    vec x = start_value;
    vec x_tmp, mean_tmp;
    double Fa, Fb;
    double mu_i;
    for(int j = -S; j< (n*thinning); j++){
        //Rcout << "j = " << j << endl;
        for(int i=0; i<d; i++){
            //Rcout << "i = " << i << endl;
            x_tmp = x;
            mean_tmp = mean_vec;
            x_tmp.shed_row(i);
            mean_tmp.shed_row(i);
            //Rcout << "x[-i] = " << x_tmp.t() << endl;
            mu_i = as_scalar(mean_vec(i) + P.row(i)*(x_tmp-mean_tmp));
            //Rcout << "mean(i) = " << mean_vec(i) << "\tmu_i = " << mu_i << endl;
            Fa = normcdf(lower(i), mu_i, sd(i));
            Fb = normcdf(upper(i), mu_i, sd(i));
            
            x(i) = mu_i + sd(i)*Rf_qnorm5( (U(l) * (Fb - Fa) + Fa), 0., 1., 1, 0);
            
            //Rcout << "Fa = " << Fa << "\t Fb = " << Fb << "\t (U(l) * (Fb - Fa) + Fa) = " << (U(l) * (Fb - Fa) + Fa) << endl;
            //qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
            l++;
        }
        //Rcout << "out x = " << endl << x << endl;
        if(j>=0){
            if(thinning == 1){
                X.row(j) = x.t();
                //Rcout << "no x = " << endl << x << endl;
            }
            else if(j % thinning == 0){
                X.row(j/thinning) = x.t();
                //Rcout << "thin x = " << endl << x << endl;
            }
                
        }
    }

    return X;
}


