
#include <stdio.h>
#include <math.h>
#include <locale.h>
#include <stdlib.h>
#include <string>
#include <algorithm>
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <time.h>
#include <numeric>
using namespace std;


#include "monspline.h"
int counter;

template <class ForwardIterator, class T>
  void iota (ForwardIterator first, ForwardIterator last, T val)
{
  while (first!=last) {
    *first = val;
    ++first;
    ++val;
  }
}

template <typename Container>
struct compare_indirect_index
  {
  const Container& container;
  compare_indirect_index( const Container& container ): container( container ) { }
  bool operator () ( size_t lindex, size_t rindex ) const
    {
    return container[ lindex ] > container[ rindex ];
    }
  };


  double OWA(int n, double x[],double w[])
 { /* no sorting is needed when used in the tree */
    double z=0;
    for(int i=0;i<n;i++) z+=x[i]*w[i];
    return z;
 }
 
double OWA(int n, double x[],double w[], int index[])
 { /*  sorting is needed in general */
    double z=0;
    for(int i=0;i<n;i++) z+=x[index[i]]*w[i];
    return z;
 }
 
 
   double OWASorted(int n, double x[],double w[])
 { /*  sorting is needed in general */
	 int *indices= new int [n];
  //  int indices[n];
	::iota( indices, indices+n, 0 );  // found in <numeric>
	sort( indices, indices+n, compare_indirect_index <typeof(x)> ( x ) );
	double t=  OWA(n,x,w, indices);
	delete[] indices;
	return t;	
 }


 double node(int n, double x[], long int N[], long int C, int & k,
             double w[], double(*F)(int, double [],double[]), int indices[], double* z)
 {
   /* recursive function in the n-ary tree processing
   Parameters: x - input vector, N vector of multiplicities of  x
   m current level of recursion counted from L (root node)  to 0
   k - input-output parameter, the index of x[k] being processed   */	
   if(N[k]==0) k++;
   if(N[k]>= C) {  /* we use idempotency here to prune the tree */
       N[k] -= C;
       if(N[k]<=0) return x[indices[k++]]; else return x[indices[k]];
   }
    C /= n;
   /* tree not pruned, process the children nodes */
   //if (C == 0) k++;
   for(int i=0;i<n;i++) z[i]=node(n,x,N,C,k,w,F,indices,z+n);
   return F(n,z,w);
}


double weightedf(double x[], double p[], double w[], int n,
         double(*F)(int, double[],double[]), int L)
/*
 Function F is the symmetric base aggregator.
 p[] = array of weights of inputs x[],
 w[] = array of weights for OWA, n = the dimension of x, p, w.
 the weights must add to one and be non-negative.
 L = number of binary tree levels. Run time = O[(n-1)L]  */
{
   double r=1.0/n;
   long int t=0, C=1;
   int k=0;
   for(int i=0;i<L;i++) C*=n;  /* C=n^L */

  int *indices= new int [n];
 // int indices[n];
  ::iota( indices, indices+n, 0 );  // found in <numeric>
  sort( indices, indices+n, compare_indirect_index <typeof(x)> ( x ) );

//   sortpairs(x, x+n, p);
//   long int N[n]; /* multiplicities of x based on the weights */
   long int *N= new long int [n];

   for(int i=0;i<n-1;i++)  {
  	   N[i]=p[indices[i]]*C+r;   t+=N[i];
   }
   N[n-1]=C-t;

   double *z=new double[(L+1)*n];
   double y= node(n,x,N,C,k,w,F,indices,z);
   delete[] z;
   delete[] indices;
   delete[] N;		
   return y;
}


void weightedOWAQuantifierBuild( double p[], double w[], int n, double temp[], int& T)
/*
 Builds the RIM quantifier of the Weighted OWA, should be called before weightedOWAQuantifier.
input:
 p[] = array of weights of the inputs,
 w[] = array of weights for OWA, n = the dimension of  p, w.
 the weights must add to one and be non-negative.
output:
 temp[] = working memory, keeps the spline knots and coefficients for later use in weightedOWAQuantifier
 should be at least 12(n+1) in length and the memory should be allocated by the calling program
 T  = the number of knots in the monotone spline
 */
{
	double *h,*alpha, *beta,*gamma,*ttemp; // temp variables
	h=&temp[0];
	alpha=&temp[2*n+2];
	beta=&temp[4*(n+1)];
	gamma=&temp[6*(n+1)];
	ttemp=&temp[8*(n+1)];

	double *x=new double[n+1];
	double *y=new double[n+1];
	x[0]=0; y[0]=0;
	for(int i=1;i<=n;i++) {
			x[i]=double(i)/n;
			y[i]=y[i-1]+w[i-1];
		}

	T= BuildMonotonSpline(x,y,n+1,h,alpha,beta,gamma,ttemp);

	delete[] x;
	delete[] y;
}


double weightedOWAQuantifier(double x[], double p[], double w[], int n, double temp[], int T)
/*
 Calculates the value of the WOWA, with quantifier function obtained in weightedOWAQuantifierBuild

input:
 x[] = inputs
 p[] = array of weights of inputs x[],
 w[] = array of weights for OWA, n = the dimension of x, p, w.
 the weights must add to one and be non-negative.
 temp[] = working memory, keeps the spline knots and coefficients computed in weightedOWAQuantifierBuild
 T  = the number of knots in the monotone spline, as computed in  weightedOWAQuantifierBuild
 */
{
 int *indices= new int [n];
 double *weights= new double [n];
//  int indices[n];
//  double weights[n];
  	double *h,*alpha, *beta,*gamma; // temp variables
	h=&temp[0];
	alpha=&temp[2*n+2];
	beta=&temp[4*(n+1)];
	gamma=&temp[6*(n+1)];

  ::iota( indices, indices+n, 0 );  // found in <numeric>
  sort( indices, indices+n, compare_indirect_index <typeof(x)> ( x ) );

// build the required values for the coefficients
	double t2=0;


	for(int i=0;i<n;i++) {
			t2 += p[indices[i]];
			weights[i]=MonotoneSplineValue(t2,h,alpha,beta,gamma,T);
	}

    for(int i=n-1;i>0;i--) {
		weights[i]=weights[i]-weights[i-1];
	}


   t2=0;
	double t=OWA(n,x,weights, indices);
        delete[] indices;
	delete[] weights; 

	return t;
}

#define MEPS 10e-12
double ImplicitWOWA(double x[], double p[], double w[], int n )
/*
	Calculates implicit Weighted OWA function
	input:
 x[] = inputs
 p[] = array of weights of inputs x[],
 w[] = array of weights for OWA, n = the dimension of x, p, w.
 the weights need not add to one but should be  non-negative.
 n = the dimension of x,w,p

*/
{
  int *indices= new int [n];
 // int indices[n];
  ::iota( indices, indices+n, 0 );  // found in <numeric>
  sort( indices, indices+n, compare_indirect_index <typeof(p)> ( p ) );

 	double den=OWA(n,p,w,indices);
	if(den<=MEPS) den=MEPS;

	double *px=new double[n];
	for(int i=0;i<n;i++) px[i]=p[i]*x[i];

  ::iota( indices, indices+n, 0 );  // found in <numeric>
  sort( indices, indices+n, compare_indirect_index <typeof(px)> ( px) );

	double numer=OWA(n,px,w,indices);
	delete[] px;
	delete[] indices;
	return numer/den;
}


double testf(double x, double y) {return ((x+y)/2);}

double WA2(double x1,double x2,double W, int L, double(*F)( double, double))
{
  long int B = (long int)(W*pow(2.,L)+0.5) - 1;
  double y = x1;
  while(L--)
  {
//printf("%f %f %f \n",y,x1,x2);
      y = F(y, B%2 ?  x1 : x2);
      B/=2;
  }                    // Function F is the symmetric base aggregator.
  return y;            // y is the aggregated value in the case where 
}             
double WAnABL(double * x, double * w, int n, int L, double(*F)( double, double))
{
	--n;
	double Wsum=w[n], y=x[n];
	while(n) {
		--n;
//printf("%d %f %f\n",n,y,x[n]);
	    y=WA2(x[n],y,w[n]/(w[n]+Wsum),L, F);
		Wsum+=w[n];
	}

	return y;
}




template<typename INT>
double node_RWAnT(double x[], INT N[], INT m, volatile int & k, double(*F)(double,double))
{
   /* recursive function in the binary tree processing 
      Parameters: x - input vector, N vector of multiplicities of the components of x
      m current level of recursion counted from L (root node) downwards to 0 (leaves)
      k - input-output parameter, points to the current index of x[k] being processed

 */	
	INT C= (INT)1<<m; /* C=2^m*/
	if(N[k]>= C) {  /* we use idempotency here to prune the tree */
		N[k] -= C;
		double y=x[k];
		if(N[k]<=0) k++; /* once all the repeated components x[k] are exhausted, move to next k */
					/* hence k is passed by reference */
		return y;
	}

	/* tree not pruned, process the children nodes */
	return F( node_RWAnT(x,N,m-1,k,F), node_RWAnT(x,N,m-1,k,F) );
}

template<typename INT>
double RWAnT(double  x[], double w[], int n,    double(*F)(double,double), INT L)
/*
 Function F is the symmetric base aggregator.
 w[ ] = array of weights of inputs x[ ], n is the dimension of x and w
 the weights must add to one and be non-negative
 L = number of binary tree levels
 Run time = O[(n-1)L]
*/
{
	INT t=0;
	volatile int k=0;
	INT C=(INT)1<<L;   /* C=2^m*/
/* prepare the multiplicites based on the weights w */

	INT* N= new INT[n];  /* multiplicities of x based on the weights */
	for(int i=0;i<n-1;i++)
	{
	   	N[i]=w[i]*C+0.5; /**/
		t+=N[i];
	}	
	N[n-1]=C-t;

    double r=node_RWAnT(x,N,L,k,F);
    delete[] N;
    return r;
}

double WAn(double * x, double * w, int n, int L, double(*F)( double, double))
{
	return RWAnT(x,w,n,F,(long int)L);
}

/*
int main()
{

	This program illustrates the use of the three WOWA methods.
	The main function can be commented out in order to use this library in other programs.



// preparing some test inputs

int n=4, L=10; // dimension and the number of levels of the n-ary tree
double x[4]={0.3,0.4,0.8,0.2};   // inputs
double w[4]={0.4,0.35,0.2,0.05}; // OWA weights
double p[4]={0.3,0.25,0.3,0.15}; // inputs weights


// calling the PnTA algorithm
  double y=weightedf(x,p,w,n,&OWA,L);
  printf("WOWA %f \n",y);




}
*/
