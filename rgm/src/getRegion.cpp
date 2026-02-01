#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector getRegionCPP(NumericVector x, NumericVector y) {
  int n = x.length();
  IntegerVector result(n);
  
  for(int i=0; i<n; i++)
  {
    if(x[i] > 0)
    { // x > 0
      if(y[i] > 0)
      { // x > 0, y > 0
        if(y[i] > 1-x[i])
          result[i]=1;
        else
          result[i]=5;
      } 
      else
      { // x > 0, y < 0
        if(y[i] > x[i]-1)
          result[i]=8;
        else
          result[i]=4;
      }
    } // x < 0
    else
    {
      if(y[i] > 0)
      { // x < 0, y > 0
        if(y[i] > x[i]+1)
          result[i]=2;
        else
          result[i]=6;
      }
      else
      { // x < 0, y < 0
        if(y[i] > -x[i]-1)
          result[i]=7;
        else
          result[i]=3;
      }  
    } // end of if/else
  } // end of for loop
  return result;
}

// [[Rcpp::export]]
int checkRegionCPP(int x, int y){
  if(x > 0)
  { // x > 0
    if(y > 0)
    { // x > 0, y > 0
      if(y > 1-x)
        return 1;
      else
        return 5;
    } 
    else
    { // x > 0, y < 0
      if(y > x-1)
        return 8;
      else
        return 4;
    }
  } // x < 0
  else
  {
    if(y > 0)
    { // x < 0, y > 0
      if(y > x+1)
        return 2;
      else
        return 6;
    }
    else
    { // x < 0, y < 0
      if(y > -x-1)
        return 7;
      else
       return 3;
    }  
  } // end of if/else
}



// This is an example of how to include R chunks in the C++ file for testing.

/*** R
set.seed(10)
table(getRegionCPP(x=rnorm(100),y=rnorm(100,.1)))
*/
