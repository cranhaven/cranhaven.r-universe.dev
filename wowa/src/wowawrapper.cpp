
/*Three different implementations of the Weighted OWA function.

	This library implements Torra's algorithm based on RIM quantifier interpolation,
	Beliakov's prunned n-ary tree algorithm PnTA and the Implicit WOWA.

	The user needs to supply the:
	OWA weighting vector w,
	the inputs weightings p
	the vector of inputs x
	all of dimension n.

SEXP OWA(SEXP n, SEXP x, SEXP w)
SEXP weightedf(SEXP x, SEXP p, SEXP w, SEXP n, SEXP Fn, SEXP L)
SEXP weightedOWAQuantifierBuild(SEXP p, SEXP w, SEXP n, SEXP temp, SEXP Tnum)
SEXP weightedOWAQuantifier(SEXP x, SEXP p, SEXP w, SEXP n, SEXP temp, SEXP Tnum)
SEXP ImplicitWOWA(SEXP x, SEXP p, SEXP w, SEXP n)
SEXP WAn(SEXP x, SEXP w, SEXP n, SEXP L)

Where:
x = inputs
p = array of weights of inputs x[],
w = array of weights for OWA, n = the dimension of x, p, w.
    the weights need not add to one but should be  non-negative.
n = the dimension of x,w,p
temp[] = working memory, keeps the spline knots and coefficients computed in weightedOWAQuantifierBuild in weightedowa.cpp
Tnum  = the number of knots in the monotone spline, as computed in  weightedOWAQuantifierBuild in weightedowa.cpp
L = number of binary tree levels. Run time = O[(n-1)L]

Fn = Function F is the symmetric base aggregator.



The details of these methods can be found in:

	G. Beliakov, H. Bustince, and T. Calvo. A Practical Guide to Averaging Functions.
	Springer, Berlin, Heidelberg, 2016.

	G. Beliakov. A method of introducing weights into OWA operators and other
	symmetric functions. In V. Kreinovich, editor, Uncertainty Modeling. Dedicated
	to B. Kovalerchuk, pages 37–52. Springer, Cham, 2017.

	G. Beliakov. Comparing apples and oranges: The weighted OWA function, submitted, 2017.

	V. Torra. The weighted F operator. Int. J. Intelligent Systems, 12:153–166, 1997.


	This program is distributed under LGPL-3 Licence without any charge.


	Compile this file with g++ weightedowa.cpp or another C++ compiled.

	The library depends on the monotone spline library by Gleb Beliakov, included here as a .h file

	Please cite our work:


	G. Beliakov. A method of introducing weights into OWA operators and other
	symmetric functions. In V. Kreinovich, editor, Uncertainty Modeling. Dedicated
	to B. Kovalerchuk, pages 37–52. Springer, Cham, 2017.

	J.J. Dujmovic and G. Beliakov. Idempotent weighted aggregation based on binary
	aggregation trees. Int. J. Intelligent Systems, in press, DOI:10.1002/int.21828, 2017.


	Copyright Gleb Beliakov, 2017
	gleb@deakin.edu.au
*/

#include <cstdlib>
#include <vector>

#include <Rcpp.h>
#include <Rdefines.h>
#include <stdint.h>

using namespace Rcpp;

// forward declarations (instead of .h file)
 double OWA(int n, double x[],double w[]);
 double OWASorted(int n, double x[],double w[]);
 double weightedf(double x[], double p[], double w[], int n,
 double(*F)(int, double[],double[]), int L);
 void weightedOWAQuantifierBuild( double p[], double w[], int n, double temp[], int& T);
 double weightedOWAQuantifier(double x[], double p[], double w[], int n, double temp[], int T);		 
 double WAn(double * x, double * w, int n, int L, double(*F)(double,double)); 
 double ImplicitWOWA(double x[], double p[], double w[], int n );

 //' WOWA code in C++ is being called from R environment with multiple arguments
 //' It then periodically calls a function in R
//' "//[[Rcpp::export]]" is needed to export the function to R environment
//' @param str input character vector
//' @exportPattern("^[[:alpha:]]+")
//' @useDynLib WOWA
//' @importFrom Rcpp evalCpp
//[[Rcpp::export]]

 SEXP OWA_R(SEXP n, SEXP x, SEXP w){

     int N = as<int>(n);
     double *X, *W;

     X = NUMERIC_POINTER(x);
     W = NUMERIC_POINTER(w);

	double out = OWASorted(N, X, W);

	return wrap(out);
 }
 SEXP WAM_R(SEXP n, SEXP x, SEXP w){

     int N = as<int>(n);
     double *X, *W;

     X = NUMERIC_POINTER(x);
     W = NUMERIC_POINTER(w);

	double out = OWA(N, X, W);

	return wrap(out);
 }


void* fn; 
void* fn2;
//global
 
double myfun( int n, double* x, double* w)
{
	double f1;
	SEXP fval;
	Rcpp::Function func((SEXP) fn);

 //   PROTECT(fval = Rf_eval(func(n, x, w), Env)); //evaluates the function in R, fval is the output
    NumericVector X=as<NumericVector>(wrap(std::vector<double>(x,x+n)));
    NumericVector W=as<NumericVector>(wrap(std::vector<double>(w,w+n)));	
    fval=func(wrap(n), X,W);

    f1=as<double>(fval);	
 
//   Rprintf("\n%s  %f %d %f\n", "Back to myf-C: ", f1,n , x[0]);

   return f1;	
}

double myfun2( double x, double y)
{
	double f1;
	SEXP fval;
	Rcpp::Function func((SEXP) fn2);
	
    fval=func(wrap(x), wrap(y));

    f1=as<double>(fval);	
 
//   Rprintf("\n%s  %f %d %f\n", "Back to myf-C: ", f1,n , x[0]);
    return f1;	
}
// add weightedOWA (explicitly make fn=OWA)
 
 SEXP weightedf_R(SEXP x, SEXP p, SEXP w, SEXP n, SEXP Fn, SEXP L){
    double *X, *P, *W;
    int N = as<int>(n);
    /*check Fn = double(*F)(int, double[],double[])*/
    int Lev = as<int>(L);

	fn=(void*) Fn;

     X = NUMERIC_POINTER(x);
     P = NUMERIC_POINTER(p);
     W = NUMERIC_POINTER(w);
	 
//   Rprintf("\n%s  %f %d %f %x", "before myf-C: ", X[0],Lev, W[0],fn);
	double out = weightedf(X, P, W, N, &myfun,   Lev);
	return wrap(out);
 }


 SEXP weightedOWAQuantifierBuild_R(SEXP p, SEXP w, SEXP n){
     double *P, *W;
     double *TEMP;

    int N = as<int>(n);
    int TNUM = 12*(N+1);
	NumericVector spl(TNUM);

     P = NUMERIC_POINTER(p);
     W = NUMERIC_POINTER(w);
     TEMP = NUMERIC_POINTER(spl);
	  
	 weightedOWAQuantifierBuild(P, W, N, TEMP, TNUM);
// to return two parameters in a list
	return List::create(Named("spl") = spl, 
                        Named("Tnum") = TNUM);
 }


 SEXP weightedOWAQuantifier_R(SEXP x, SEXP p, SEXP w, SEXP n, SEXP temp, SEXP Tnum){
    double *X, *P, *W;
    double *TEMP;

    int N = as<int>(n);
    int TNUM = as<int>(Tnum);

     X = NUMERIC_POINTER(x);
     P = NUMERIC_POINTER(p);
     W = NUMERIC_POINTER(w);
     TEMP = NUMERIC_POINTER(temp);
//	 	 Rprintf("\n%s  %f %d %f\n", "build: ", TEMP[1], TNUM, TEMP[2]);
		 
		 
	double out = weightedOWAQuantifier(X, P, W, N, TEMP, TNUM);
	return wrap(out);
 }


SEXP ImplicitWOWA_R(SEXP x, SEXP p, SEXP w, SEXP n){
    double *X, *P, *W;
    int N = as<int>(n);

     X = NUMERIC_POINTER(x);
     P = NUMERIC_POINTER(p);
     W = NUMERIC_POINTER(w);

	double out = ImplicitWOWA(X, P, W, N);
	return wrap(out);
 }

SEXP WAn_R(SEXP x, SEXP w, SEXP n, SEXP L, SEXP Fn){
    double *X, *W;
    int N = as<int>(n);
    int Lev = as<int>(L);
	fn2=(void*) Fn;

     X = NUMERIC_POINTER(x);
     W = NUMERIC_POINTER(w);
	 
	double out = WAn(X, W, N, Lev, &myfun2);
	return wrap(out);
 }

