#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double getSumLogExpIndecies(NumericVector trials,
                            NumericVector XBetaStar,
                            NumericVector resultantVByVRandomEffects,
                            NumericVector resultantWByURandomEffects,
                            NumericVector indecies)
{
  
  double answer = 0.0;
  int counter = 0;
  
  for(int i = 0; i < XBetaStar.size(); i++){
    if(i == indecies[counter]){
      answer = answer + trials[i] * log(1 + exp(XBetaStar[i] + resultantVByVRandomEffects[i] + resultantWByURandomEffects[i]));
      counter++;
    }
  }
  
  return answer;
  
}
