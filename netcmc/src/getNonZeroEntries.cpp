#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector getNonZeroEntries(NumericVector vector){
  
  int counter = 0;
  for(int j = 0; j < vector.size(); j++) {
    if(vector[j] != 0){
      counter++;
    }
  }
  
  NumericVector indecies(counter);
  
  int indeciesCounter = 0;
  for(int j = 0; j < vector.size(); j++) {
    if(vector[j] != 0){
      indecies[indeciesCounter] = j;
      indeciesCounter++;
    }
  }
  
  return indecies;
} 

