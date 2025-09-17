#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

double getSumExpNetworkIndecies(NumericVector XBetaStar,
                                NumericVector resultantWByURandomEffects,
                                NumericVector uRandomEffectRepeatedVector,
                                NumericVector indecies)
{
  
  double answer = 0.0;
  int counter = 0;
  
  for(int i = 0; i < XBetaStar.size(); i++){
    if(i == indecies[counter]){
      answer = answer + exp(XBetaStar[i] + uRandomEffectRepeatedVector[i] + resultantWByURandomEffects[i]);
      counter++;
    }
  }
  
  return answer;
  
}
