// load Rcpp
#include <RcppArmadillo.h>
#ifdef _OPENMP
# include <omp.h>
#endif
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat GetLinearKernel(arma::mat X){
    double p = X.n_rows;
    return X.t()*X/p;
}

////////////////////////////////////////////////////////////////////////////

//Below are functions for MAPITR looking for interaction effects for pathways

////////////////////////////////////////////////////////////////////////////

//' Main MAPITR cpp code
//' 
//' @param X a p x n genotype matrix
//' @param Y a n x r phenotype matrix
//' @param regions a list of q pathways
//' @param cores an integer for the number of cores
//'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List MAPITRBase(arma::mat X,arma::mat Y,List regions,int cores = 1){
    int i;
    const int n = X.n_cols;
    const int nsnp = X.n_rows;
    const int p = regions.size();
    //const int q = Z.n_rows;
    
    //Set up the vectors to save the outputs
    NumericVector sigma_est(p);
    NumericVector pve(p);
    arma::mat Lambda(n,p);
    
    //Pre-compute the Linear GSM
    arma::mat GSM = GetLinearKernel(X);

    #ifdef _OPENMP
        omp_set_num_threads(cores);
    #endif
#pragma omp parallel for schedule(dynamic)
    for(i=0; i<p; i++){
	//Extract phenotype
	arma::vec y = Y.col(i);

        //Pre-compute the Linear GSM
        arma::uvec j = regions[i];
       
        //Compute K covariance matrices
        arma::mat K = (GSM*nsnp-GetLinearKernel(X.rows(j-1))*j.n_elem)/(nsnp-j.n_elem-1);
        arma::mat G = GetLinearKernel(X.rows(j-1))%K;
        
        //Transform K and G using projection M
        arma::mat b = zeros(n);
	b.col(0) = ones<vec>(n); 
        arma::mat btb_inv = inv(b.t()*b);
        arma::mat Kc = K-b*btb_inv*(b.t()*K)-(K*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(K*b))*btb_inv*b.t();
        arma::mat Gc = G-b*btb_inv*(b.t()*G)-(G*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(G*b))*btb_inv*b.t();
        arma::vec yc = (eye<mat>(n,n)-(b*btb_inv)*b.t())*y;
        
        //Compute the quantities q and S
        arma::vec q = zeros(3); //Create k-vector q to save
        arma::mat S = zeros(3,3); //Create kxk-matrix S to save
        
        q(0) = as_scalar(yc.t()*Kc*yc);
        q(1) = as_scalar(yc.t()*Gc*yc);
        q(2) = as_scalar(yc.t()*(eye<mat>(n,n)-(b*btb_inv)*b.t())*yc);
        
        S(0,0) = as_scalar(accu(Kc%Kc));
        S(0,1) = as_scalar(accu(Kc%Gc));
        S(0,2) = as_scalar(accu(Kc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(1,0) = S(0,1);
        S(1,1) = as_scalar(accu(Gc%Gc));
        S(1,2) = as_scalar(accu(Gc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(2,0) = S(0,2);
        S(2,1) = S(1,2);
        S(2,2) = as_scalar(accu((eye<mat>(n,n)-(b*btb_inv)*b.t())%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        //Compute delta and Sinv
        arma::mat Sinv = inv(S);
        arma::vec delta = Sinv*q;
        
        //Record nu^2, and tau^2 under the null hypothesis
        arma::vec q_sub = zeros(2);
        arma::mat S_sub = zeros(2,2);
        
        q_sub(0)=q(0);
        q_sub(1)=q(2);
        
        S_sub(0,0)=S(0,0);
        S_sub(0,1)=S(0,2);
        
        S_sub(1,0)=S(2,0);
        S_sub(1,1)=S(2,2);
        
        //Compute P and P^{1/2} matrix
        arma::vec delta_null = inv(S_sub)*q_sub;
        
        arma::vec eigval;
        arma::mat eigvec;
        
        eig_sym(eigval,eigvec,delta_null(0)*Kc+delta_null(1)*(eye<mat>(n,n)-(b*btb_inv)*b.t()));
        
        //Find the eigenvalues of the projection matrix
        arma::vec evals;
        
        eig_sym(evals, (eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0))))*(Sinv(1,0)*Kc+Sinv(1,1)*Gc+Sinv(1,2)*(eye<mat>(n,n)-(b*btb_inv)*b.t()))*(eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0)))));
        Lambda.col(i) = evals;
        
        //Save point estimates and SE of the epistasis component
        sigma_est(i) = delta(1);
        
        //Compute the PVE
        pve(i) = delta(1)/accu(delta);
    }
    
    //Return a list of the arguments
    return Rcpp::List::create(Rcpp::Named("Est") = sigma_est, Rcpp::Named("Eigenvalues") = Lambda,Rcpp::Named("PVE") = pve);
}

//' Main MAPITR + Covariates cpp code
//' 
//' @param X a p x n genotype matrix
//' @param Y a n x r phenotype matrix
//' @param regions a list of q pathways
//' @param Z a q x n matrix of covariates
//' @param cores an integer for the number of cores
//'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List MAPITRBaseCovs(arma::mat X,arma::mat Y,arma::mat Z,List regions,int cores = 1){
    int i;
    const int n = X.n_cols;
    const int nsnp = X.n_rows;
    const int p = regions.size();
    const int q = Z.n_rows;
   
    //Set up the vectors to save the outputs
    NumericVector sigma_est(p);
    NumericVector pve(p);
    arma::mat Lambda(n,p);
    
    //Pre-compute the Linear GSM
    arma::mat GSM = GetLinearKernel(X);

    #ifdef _OPENMP
        omp_set_num_threads(cores);
    #endif
#pragma omp parallel for schedule(dynamic)
    for(i=0; i<p; i++){
	//Extract phenotype
	arma::vec y = Y.col(i);

        //Pre-compute the Linear GSM
        arma::uvec j = regions[i];
       
        //Compute K covariance matrices
        arma::mat K = (GSM*nsnp-GetLinearKernel(X.rows(j-1))*j.n_elem)/(nsnp-j.n_elem-1);
        arma::mat G = GetLinearKernel(X.rows(j-1))%K;
        
        //Transform K and G using projection M
	arma::mat b = zeros(n,q+1);
	b.col(0) = ones<vec>(n); b.cols(1,q) = Z.t();

	arma::mat btb_inv = inv(b.t()*b);
        arma::mat Kc = K-b*btb_inv*(b.t()*K)-(K*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(K*b))*btb_inv*b.t();
        arma::mat Gc = G-b*btb_inv*(b.t()*G)-(G*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(G*b))*btb_inv*b.t();
        arma::vec yc = (eye<mat>(n,n)-(b*btb_inv)*b.t())*y;
        
        //Compute the quantities q and S
        arma::vec q = zeros(3); //Create k-vector q to save
        arma::mat S = zeros(3,3); //Create kxk-matrix S to save
        
        q(0) = as_scalar(yc.t()*Kc*yc);
        q(1) = as_scalar(yc.t()*Gc*yc);
        q(2) = as_scalar(yc.t()*(eye<mat>(n,n)-(b*btb_inv)*b.t())*yc);
        
        S(0,0) = as_scalar(accu(Kc%Kc));
        S(0,1) = as_scalar(accu(Kc%Gc));
        S(0,2) = as_scalar(accu(Kc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(1,0) = S(0,1);
        S(1,1) = as_scalar(accu(Gc%Gc));
        S(1,2) = as_scalar(accu(Gc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(2,0) = S(0,2);
        S(2,1) = S(1,2);
        S(2,2) = as_scalar(accu((eye<mat>(n,n)-(b*btb_inv)*b.t())%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        //Compute delta and Sinv
        arma::mat Sinv = inv(S);
        arma::vec delta = Sinv*q;
        
        //Record nu^2, and tau^2 under the null hypothesis
        arma::vec q_sub = zeros(2);
        arma::mat S_sub = zeros(2,2);
        
        q_sub(0)=q(0);
        q_sub(1)=q(2);
        
        S_sub(0,0)=S(0,0);
        S_sub(0,1)=S(0,2);
        
        S_sub(1,0)=S(2,0);
        S_sub(1,1)=S(2,2);
        
        //Compute P and P^{1/2} matrix
        arma::vec delta_null = inv(S_sub)*q_sub;
        
        arma::vec eigval;
        arma::mat eigvec;
        
        eig_sym(eigval,eigvec,delta_null(0)*Kc+delta_null(1)*(eye<mat>(n,n)-(b*btb_inv)*b.t()));
        
        //Find the eigenvalues of the projection matrix
        arma::vec evals;
        
        eig_sym(evals, (eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0))))*(Sinv(1,0)*Kc+Sinv(1,1)*Gc+Sinv(1,2)*(eye<mat>(n,n)-(b*btb_inv)*b.t()))*(eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0)))));
        Lambda.col(i) = evals;
        
        //Save point estimates and SE of the epistasis component
        sigma_est(i) = delta(1);
        
        //Compute the PVE
        pve(i) = delta(1)/accu(delta);
    }
    
    //Return a list of the arguments
    return Rcpp::List::create(Rcpp::Named("Est") = sigma_est, Rcpp::Named("Eigenvalues") = Lambda,Rcpp::Named("PVE") = pve);
}

////////////////////////////////////////////////////////////////////////////

//Below are functions for MAPITR looking for interaction effects for pathways
//No OpenMP versions

////////////////////////////////////////////////////////////////////////////

//' Main MAPITR cpp code, no OpenMP version
//' 
//' @param X a p x n genotype matrix
//' @param Y a n x r phenotype matrix
//' @param regions a list of q pathways
//'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List MAPITRBase_noOpenMP(arma::mat X,arma::mat Y,List regions){
    int i;
    const int n = X.n_cols;
    const int nsnp = X.n_rows;
    const int p = regions.size();
    //const int q = Z.n_rows;
    
    //Set up the vectors to save the outputs
    NumericVector sigma_est(p);
    NumericVector pve(p);
    arma::mat Lambda(n,p);
    
    //Pre-compute the Linear GSM
    arma::mat GSM = GetLinearKernel(X);

    for(i=0; i<p; i++){
	//Extract phenotype
	arma::vec y = Y.col(i);

        //Pre-compute the Linear GSM
        arma::uvec j = regions[i];
       
        //Compute K covariance matrices
        arma::mat K = (GSM*nsnp-GetLinearKernel(X.rows(j-1))*j.n_elem)/(nsnp-j.n_elem-1);
        arma::mat G = GetLinearKernel(X.rows(j-1))%K;
        
        //Transform K and G using projection M
        arma::mat b = zeros(n);
	b.col(0) = ones<vec>(n); 
        arma::mat btb_inv = inv(b.t()*b);
        arma::mat Kc = K-b*btb_inv*(b.t()*K)-(K*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(K*b))*btb_inv*b.t();
        arma::mat Gc = G-b*btb_inv*(b.t()*G)-(G*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(G*b))*btb_inv*b.t();
        arma::vec yc = (eye<mat>(n,n)-(b*btb_inv)*b.t())*y;
        
        //Compute the quantities q and S
        arma::vec q = zeros(3); //Create k-vector q to save
        arma::mat S = zeros(3,3); //Create kxk-matrix S to save
        
        q(0) = as_scalar(yc.t()*Kc*yc);
        q(1) = as_scalar(yc.t()*Gc*yc);
        q(2) = as_scalar(yc.t()*(eye<mat>(n,n)-(b*btb_inv)*b.t())*yc);
        
        S(0,0) = as_scalar(accu(Kc%Kc));
        S(0,1) = as_scalar(accu(Kc%Gc));
        S(0,2) = as_scalar(accu(Kc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(1,0) = S(0,1);
        S(1,1) = as_scalar(accu(Gc%Gc));
        S(1,2) = as_scalar(accu(Gc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(2,0) = S(0,2);
        S(2,1) = S(1,2);
        S(2,2) = as_scalar(accu((eye<mat>(n,n)-(b*btb_inv)*b.t())%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        //Compute delta and Sinv
        arma::mat Sinv = inv(S);
        arma::vec delta = Sinv*q;
        
        //Record nu^2, and tau^2 under the null hypothesis
        arma::vec q_sub = zeros(2);
        arma::mat S_sub = zeros(2,2);
        
        q_sub(0)=q(0);
        q_sub(1)=q(2);
        
        S_sub(0,0)=S(0,0);
        S_sub(0,1)=S(0,2);
        
        S_sub(1,0)=S(2,0);
        S_sub(1,1)=S(2,2);
        
        //Compute P and P^{1/2} matrix
        arma::vec delta_null = inv(S_sub)*q_sub;
        
        arma::vec eigval;
        arma::mat eigvec;
        
        eig_sym(eigval,eigvec,delta_null(0)*Kc+delta_null(1)*(eye<mat>(n,n)-(b*btb_inv)*b.t()));
        
        //Find the eigenvalues of the projection matrix
        arma::vec evals;
        
        eig_sym(evals, (eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0))))*(Sinv(1,0)*Kc+Sinv(1,1)*Gc+Sinv(1,2)*(eye<mat>(n,n)-(b*btb_inv)*b.t()))*(eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0)))));
        Lambda.col(i) = evals;
        
        //Save point estimates and SE of the epistasis component
        sigma_est(i) = delta(1);
        
        //Compute the PVE
        pve(i) = delta(1)/accu(delta);
    }
    
    //Return a list of the arguments
    return Rcpp::List::create(Rcpp::Named("Est") = sigma_est, Rcpp::Named("Eigenvalues") = Lambda,Rcpp::Named("PVE") = pve);
}

//' Main MAPITR + Covariates cpp code, no OpenMP version
//' 
//' @param X a p x n genotype matrix
//' @param Y a n x r phenotype matrix
//' @param regions a list of q pathways
//' @param Z a q x n matrix of covariates
//'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
List MAPITRBaseCovs_noOpenMP(arma::mat X,arma::mat Y,arma::mat Z,List regions){
    int i;
    const int n = X.n_cols;
    const int nsnp = X.n_rows;
    const int p = regions.size();
    const int q = Z.n_rows;
   
    //Set up the vectors to save the outputs
    NumericVector sigma_est(p);
    NumericVector pve(p);
    arma::mat Lambda(n,p);
    
    //Pre-compute the Linear GSM
    arma::mat GSM = GetLinearKernel(X);

    for(i=0; i<p; i++){
	//Extract phenotype
	arma::vec y = Y.col(i);

        //Pre-compute the Linear GSM
        arma::uvec j = regions[i];
       
        //Compute K covariance matrices
        arma::mat K = (GSM*nsnp-GetLinearKernel(X.rows(j-1))*j.n_elem)/(nsnp-j.n_elem-1);
        arma::mat G = GetLinearKernel(X.rows(j-1))%K;
        
        //Transform K and G using projection M
	arma::mat b = zeros(n,q+1);
	b.col(0) = ones<vec>(n); b.cols(1,q) = Z.t();

	arma::mat btb_inv = inv(b.t()*b);
        arma::mat Kc = K-b*btb_inv*(b.t()*K)-(K*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(K*b))*btb_inv*b.t();
        arma::mat Gc = G-b*btb_inv*(b.t()*G)-(G*b)*btb_inv*b.t()+b*btb_inv*(b.t()*(G*b))*btb_inv*b.t();
        arma::vec yc = (eye<mat>(n,n)-(b*btb_inv)*b.t())*y;
        
        //Compute the quantities q and S
        arma::vec q = zeros(3); //Create k-vector q to save
        arma::mat S = zeros(3,3); //Create kxk-matrix S to save
        
        q(0) = as_scalar(yc.t()*Kc*yc);
        q(1) = as_scalar(yc.t()*Gc*yc);
        q(2) = as_scalar(yc.t()*(eye<mat>(n,n)-(b*btb_inv)*b.t())*yc);
        
        S(0,0) = as_scalar(accu(Kc%Kc));
        S(0,1) = as_scalar(accu(Kc%Gc));
        S(0,2) = as_scalar(accu(Kc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(1,0) = S(0,1);
        S(1,1) = as_scalar(accu(Gc%Gc));
        S(1,2) = as_scalar(accu(Gc%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        S(2,0) = S(0,2);
        S(2,1) = S(1,2);
        S(2,2) = as_scalar(accu((eye<mat>(n,n)-(b*btb_inv)*b.t())%(eye<mat>(n,n)-(b*btb_inv)*b.t())));
        
        //Compute delta and Sinv
        arma::mat Sinv = inv(S);
        arma::vec delta = Sinv*q;
        
        //Record nu^2, and tau^2 under the null hypothesis
        arma::vec q_sub = zeros(2);
        arma::mat S_sub = zeros(2,2);
        
        q_sub(0)=q(0);
        q_sub(1)=q(2);
        
        S_sub(0,0)=S(0,0);
        S_sub(0,1)=S(0,2);
        
        S_sub(1,0)=S(2,0);
        S_sub(1,1)=S(2,2);
        
        //Compute P and P^{1/2} matrix
        arma::vec delta_null = inv(S_sub)*q_sub;
        
        arma::vec eigval;
        arma::mat eigvec;
        
        eig_sym(eigval,eigvec,delta_null(0)*Kc+delta_null(1)*(eye<mat>(n,n)-(b*btb_inv)*b.t()));
        
        //Find the eigenvalues of the projection matrix
        arma::vec evals;
        
        eig_sym(evals, (eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0))))*(Sinv(1,0)*Kc+Sinv(1,1)*Gc+Sinv(1,2)*(eye<mat>(n,n)-(b*btb_inv)*b.t()))*(eigvec.cols(find(eigval>0))*diagmat(sqrt(eigval(find(eigval>0))))*trans(eigvec.cols(find(eigval>0)))));
        Lambda.col(i) = evals;
        
        //Save point estimates and SE of the epistasis component
        sigma_est(i) = delta(1);
        
        //Compute the PVE
        pve(i) = delta(1)/accu(delta);
    }
    
    //Return a list of the arguments
    return Rcpp::List::create(Rcpp::Named("Est") = sigma_est, Rcpp::Named("Eigenvalues") = Lambda,Rcpp::Named("PVE") = pve);
}
