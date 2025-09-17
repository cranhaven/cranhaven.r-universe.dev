
#define ARMA_NO_DEBUG
#include <RcppArmadillo.h>
#include "cor.h"
#include "NPD_proj.h"
#include "fastCorKendall.h"
#include "utils.h"

using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]

// this is the megic number 1/(\Phi^{-1}(0.75))
//const double invphiinv_75 =  1.482602; 

/*
 Core of calculating all these correlation coefficients, will basically all returns 
 a correlation or covariance matrix
*/



// --------------------
// Gnanadesikan-Kettenring estimator
// --------------------

//' @name covGK
//' @title Gnanadesikan-Kettenring estimator for *covariance*
//' @description This routine calculates the Gnanadesikan-Kettenring estimator, diagonal will be MAD
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p, GK estimator, note that it's not necessarily positive
//' @examples covGK(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat covGK(const arma::mat & data){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = covGK(data.col(i),data.col(j));
        }
    }
    res = arma::symmatu(res);
    vec MAD = MAD_cpp(data);
    res.diag() += MAD % MAD;
    return(res);
}


// --------------------
// Spearman
// --------------------

//' @name corSpearman
//' @title Spearman correlation
//' @description This routine calculates the Spearman correlation
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p of spearman correlations
//' @examples corSpearman(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat corSpearman(const arma::mat & data){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = corSpearman(data.col(i),data.col(j));
        }
    }
    res = arma::symmatu(res);
    res.diag() += 1;
    return(res);
}


// --------------------
// Kendall's tau
// --------------------

//' @name corKendall
//' @title Kendall's tau
//' @description This routine calculates the Kendall's tau
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p, Kendall's tau
//' @examples corKendall(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat corKendall(const arma::mat & data){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = corKendall(data.col(i),data.col(j));
        }
    }
    res = arma::symmatu(res);
    res.diag() += 1;
    return(res);
}

// --------------------
// Quadrant correlation coefficients
// --------------------

//' @name corQuadrant
//' @title Quadrant correlation coefficients
//' @description This routine calculates Quadrant correlation coefficients
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p, Quadrant correlation coefficients
//' @examples corQuadrant(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat corQuadrant(const arma::mat & data){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = corQuadrant(data.col(i),data.col(j));
        }
    }
    res = arma::symmatu(res);
    res.diag() += 1;
    return(res);
}


// --------------------
// SpearmanU estimator for *covariance*
// --------------------

//' @name covSpearmanU
//' @title SpearmanU estimator for *covariance*
//' @description This routine calculates the SpearmanU, the pairwise covariance matrix estimator proposed in Oellererand Croux
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p of spearmanU correlation 
//' @examples covSpearmanU(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat covSpearmanU(const arma::mat & data){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    vec MADscale = MAD_cpp(data);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = corSpearman(data.col(i),data.col(j)) * MADscale(i) * MADscale(j);
        }
    }
    res = arma::symmatu(res);
    res.diag() += MADscale % MADscale;
    return(res);
}



// --------------------
// orthogonalized Gnanadesikan-Kettenring
// --------------------

//' @name covOGK
//' @title Orthogonalized Gnanadesikan-Kettenring (OGK) estimator for *covariance*
//' @description This routine calculates the Orthogonalized Gnanadesikan-Kettenring (OGK) estimator for *covariance*, using scale estimation of Gn, as in Maronna and Zamar
//' @param data the n by p raw data matrix
//' @return a matrix with dimension p by p, OGK estimator
//' @examples covOGK(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat covOGK(const arma::mat & data){ 
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    vec Dmat(p);
    mat Z = data; 
    for(int i = 0 ; i < p ; i++){
        Dmat(i) = scaleQn(data.col(i));// get the D in Maronna and Zamar
    }
    Z.each_row() /= Dmat.t(); // rescale by Qn;
    mat U = covGK(Z);
    U.diag() = ones(p); // enforce it being "correlation" 

    vec Lambda;
    mat E;
    // solve the eigen system
    eig_sym(Lambda, E, U);

    mat A = E;
    A.each_col() %= Dmat; 
    Z = Z * E;
    
    vec Gamma(p); 
    for(int i = 0; i < p; i++){
        Gamma(i) = scaleQn(Z.col(i)); 
        Gamma(i) *= Gamma(i);
    }
    //Gamma %= Gamma; 

    res = A * diagmat(Gamma) * A.t();
    return(res);
}

// --------------------
// NPD estimator
// --------------------

//' @name covNPD
//' @title NPD estimator for *covariance* based on Qn 
//' @description This routine calculates the NPD estimator for *covariance* based on Qn
//' @param data the n by p raw data matrix
//' @param eigenTol tolerance in eigen system, used in finding nearest positive matrix
//' @param convTol tolerance in cov, used in finding nearest positive matrix
//' @param psdTol tolerance in psd, used in finding nearest positive matrix
//' @param maxit max iterations in finding nearest positive matrix
//' @return a matrix with dimension p by p, NPD estimator
//' @examples covNPD(matrix(rnorm(500),100,5)) 
//' @export
// [[Rcpp::export]]
arma::mat covNPD(const arma::mat & data, const float eigenTol = 1e-06, const float convTol = 1e-07, 
             const float psdTol = 1e-08, const int maxit = 1000){
    int p = data.n_cols;
    mat res(p,p, arma::fill::zeros);
    for(int i = 0 ; i < p-1 ; i++){
        for(int j = i + 1 ; j < p ; j++){ // upper triangular
            res(i,j) = covQn(data.col(i),data.col(j));
        }
    }
    res = arma::symmatu(res);
    vec Qn(p); 
    for(int i = 0; i < p; i++){
        Qn(i) = scaleQn(data.col(i)); 
    }
    res.diag() += Qn % Qn;
    res = nearPPSD(res, eigenTol, convTol , psdTol ,maxit);
    return(res);
}

