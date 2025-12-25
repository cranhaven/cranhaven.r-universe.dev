//#include <numeric>      // std::iota
#include <RcppArmadillo.h>
//#include <vector>
//#include <limits>
//#include <algorithm>
//#define NDEBUG 1

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
void showValue(int x) {
    Rcout << "The value is " << x << std::endl;
}

// [[Rcpp::export]]
double norm2(NumericVector x){
	arma::vec xx = x;
	double g=arma::norm(xx,2);
	return (as<double>(wrap(g)));
}

// [[Rcpp::export]]
double ST1a(double z,double gam){
	if(z>0 && gam<fabs(z)) return(z-gam);
	if(z<0 && gam<fabs(z)) return(z+gam);
	if(gam>=fabs(z)) return(0);
	else return(0);

}

// [[Rcpp::export]]
colvec ST3a(colvec z,colvec gam){
	int n=z.size();
	colvec z1(n);
	for( int i=0; i<n;++i){
		double z11=z(i);
	  double g11=gam(i);
		z1(i)=ST1a(z11,g11);
	}
	return(z1);
}

// [[Rcpp::export]]
uvec ind(int n2,int m){
	std::vector<int> subs;
	for(int i =0 ; i<n2;++i){
    subs.push_back(i);
	}
	subs.erase(subs.begin()+m);
	return(conv_to<uvec>::from(subs));
}

// [[Rcpp::export]]
mat FISTA(
    const mat& Y, 
    const mat& Z, 
    mat& B, 
    mat& W, 
    const rowvec lambda1,
    const double eps, 
    double step){
  
  B=trans(B);
  W=trans(W);
  colvec B1=B.col(0);

  double j = 1;
  mat I(Z.n_cols,Z.n_cols);
  I.eye();
  int np = B.n_cols;
  
  for( int i =0; i<np; ++i){
    B1=B.col(i);
    colvec BOLD=B.col(i);
    colvec BOLDOLD=BOLD;
	  double thresh=10*eps;
	  double templam = 0.0;
	  templam=lambda1(0);
	  double maxiters=1000;
	  j=1;
	  colvec Wj = W.col(i);
	  Wj = Wj*templam*step;
	  //Wj = Wj*templam;
	  while((thresh>eps) & (j<maxiters)){

 			  colvec v=BOLD+((j-2)/(j+1))*(BOLD-BOLDOLD);
 			  B1=ST3a(vectorise(v)+step*vectorise((trans(Y.col(i))-trans(v)*Z)*trans(Z)),Wj);
 			  thresh=max(abs(B1-v));
 			  BOLDOLD=BOLD;
 			  BOLD=B1;
 			  j+=1;
 			  
	  }

    B.col(i)=B1;

	}

  B=trans(B);

  return(B);

} 



// [[Rcpp::export]]
cube lamloopFISTA(
    NumericVector beta_, 
    const mat& Y,
    const mat& Z,
    NumericVector W_, 
    const mat& lambda1,
    const double eps,
    const colvec& YMean2, 
    const colvec& ZMean2,
    mat& B1, 
    double step){

  mat b2 = B1;
  mat B1F2 = B1;
  mat W2 = B1;
  
  
  // Here we read in the R array as a NumericVector as it retains 
  // its dims attribute, We can then use these dims to set our 
  // arma::cube dimensions and recreate the array.
  
  // Beta is an array with dimensions 
  IntegerVector dimsB=beta_.attr("dim");
  cube bcube(beta_.begin(),dimsB[0],dimsB[1],dimsB[2],false);
  
  cube bcube2(dimsB[0],dimsB[1]+1,dimsB[2]);
  bcube2.fill(0);
  
  colvec nu=zeros<colvec>(dimsB[0]);
  
  IntegerVector dimsW=W_.attr("dim");
  cube wcube(W_.begin(),dimsW[0],dimsW[1],dimsW[2],false);

	int nlambda1 = lambda1.n_rows;
  int n_r = dimsW[2];
  for(int i=0;i<nlambda1;++i){ 
    
    //rowvec lam1_temp(1);
		//lam1_temp(0) = lambda1(i);
    
	  for(int j=0;j<n_r;++j){ 
	    
	    rowvec lam1_temp(1);
	  	lam1_temp(0) = lambda1(i,j);
	    //showValue((i)*n_r+j);
		  //double lam2_temp = lambda2(j);
		  mat B1F2 = bcube.slice((i)*n_r+j);
		  mat W1F2 = wcube.slice(j);
		  B1 = FISTA(Y,Z,B1F2,W1F2,lam1_temp,eps,step); 
		  nu = YMean2 - B1 * ZMean2;
		  bcube2.slice((i)*n_r+j) = mat(join_horiz(nu, B1));
		  
	  }
	  
	}
  
  return(bcube2);
}

