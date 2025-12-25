// Main functions for dgumbel
// Copyright (C) 2020 Berent Lunde
// License: GPL-3

//#include "../inst/include/dgumbel.hpp"
#include "dgumbel.hpp"

// DENSITY FUNCTION

template<class T>
T dgumbel(double x, T location, T scale, bool log_dens){
    
    T z = (x-location)/scale;
    T log_fx = -(z+exp(-z)) - log(scale);
    T res;
    if(log_dens){
        res = log_fx;
    }else{
        res = exp(log_fx);
    }
    return res;
    
}

// [[Rcpp::export(.dgumbel)]]
Rcpp::NumericVector dgumbel(Rcpp::NumericVector x, double location, double scale, bool log_dens){
    
    int n = x.size();
    Rcpp::NumericVector res(n);
    
    for(int i=0; i<n; i++){
        res[i] = dgumbel<double>(x[i], location, scale, log_dens);
    }
    
    return res;
    
}

// [[Rcpp::export(.ddgumbel)]]
Rcpp::NumericVector ddgumbel(Rcpp::NumericVector x, double location, double scale, bool log_dens){
    
    int n = x.size();
    Rcpp::NumericMatrix grad(2,n);
    
    for(int i=0; i<n; i++){
        adept::Stack stack;
        
        adtype location_ad = location;
        adtype scale_ad = scale;
        
        stack.new_recording();
        
        adtype res0 = dgumbel<adtype>(x[i], location_ad, scale_ad, log_dens);
        adtype res = res0/1.0;
        
        res.set_gradient(1.0);
        stack.compute_adjoint();
        
        grad(0,i) = location_ad.get_gradient();
        grad(1,i) = scale_ad.get_gradient();
    }
    
    return grad;
    
}


// DISTRIBUTION FUNCTION

template<class T>
T pgumbel(double q, T location, T scale, bool lower_tail, bool log_p){
    
    T z = (q-location)/scale;
    T log_px = -exp(-z); // log p(X <= x)
    T res;
    
    if(lower_tail && log_p){
        res = log_px;
    }else if(lower_tail && !log_p){
        res = exp(log_px);
    }else if(!lower_tail && log_p){
        res = log(1.0 - exp(log_px));
    }else{
        res = 1.0 - exp(log_px);
    }
    
    return res;
    
}

// [[Rcpp::export(.pgumbel)]]
Rcpp::NumericVector pgumbel(Rcpp::NumericVector q, double location, double scale, bool lower_tail, bool log_p){
    
    int n = q.size();
    Rcpp::NumericVector res(n);
    
    for(int i=0; i<n; i++){
        res[i] = pgumbel<double>(q[i], location, scale, lower_tail, log_p);
    }
    
    return res;
    
}

// [[Rcpp::export(.dpgumbel)]]
Rcpp::NumericMatrix dpgumbel(Rcpp::NumericVector q, double location, double scale, bool lower_tail, bool log_p){
    
    int n = q.size();
    Rcpp::NumericMatrix grad(2,n);
    
    for(int i=0; i<n; i++){
        adept::Stack stack;
        
        adtype location_ad = location;
        adtype scale_ad = scale;
        
        stack.new_recording();
        
        adtype res0 = pgumbel<adtype>(q[i], location_ad, scale_ad, lower_tail, log_p);
        adtype res = res0/1.0;
        
        res.set_gradient(1.0);
        stack.compute_adjoint();
        
        grad(0,i) = location_ad.get_gradient();
        grad(1,i) = scale_ad.get_gradient();
    }
    
    return grad;
    
}


// QUANTILE FUNCTION

template<class T>
T qgumbel(double p, T location, T scale, bool lower_tail){
    
    if(!lower_tail){
        p = 1.0-p;
    }
    T res = location - scale*log(-log(p));
    return res;
}

// [[Rcpp::export(.qgumbel)]]
Rcpp::NumericVector qgumbel(Rcpp::NumericVector p, double location, double scale, bool lower_tail){
    
    int n = p.size();
    Rcpp::NumericVector res(n);
    
    for(int i=0; i<n; i++){
        res[i] = qgumbel<double>(p[i], location, scale, lower_tail);
    }
    
    return res;
    
}

// [[Rcpp::export(.dqgumbel)]]
Rcpp::NumericMatrix dqgumbel(Rcpp::NumericVector p, double location, double scale, bool lower_tail){
    
    int n = p.size();
    Rcpp::NumericMatrix grad(2,n);
    
    for(int i=0; i<n; i++){
        adept::Stack stack;
        
        adtype location_ad = location;
        adtype scale_ad = scale;
        
        stack.new_recording();
        
        adtype res0 = qgumbel<adtype>(p[i], location_ad, scale_ad, lower_tail);
        adtype res = res0/1.0;
        
        res.set_gradient(1.0);
        stack.compute_adjoint();
        
        grad(0,i) = location_ad.get_gradient();
        grad(1,i) = scale_ad.get_gradient();
    }
    
    return grad;
    
}


// RANDOM NUMBER GENERATION

// [[Rcpp::export(.rgumbel)]]
Rcpp::NumericVector rgumbel(int n, double location, double scale){
    
    // Simulate from standard uniform
    Rcpp::NumericVector u = Rcpp::runif(n, 0, 1);
    
    // Transform to gumbel
    Rcpp::NumericVector x = qgumbel(u, location, scale, true);
    
    return x;
    
}
