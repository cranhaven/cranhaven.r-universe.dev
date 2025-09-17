#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector getSubvectorIndecies(NumericVector vector,
                                   NumericVector indecies)
{
  
  NumericVector answer(indecies.size());
  int counter = 0;
  
  for(int i = 0; i < vector.size(); i++){
    if(i == indecies[counter]){
      answer[counter] = vector[i];
      counter++;
    }
  }
  
  return answer;
  
}
