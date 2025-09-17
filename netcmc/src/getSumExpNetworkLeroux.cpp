#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

double getSumExpNetworkLeroux(NumericVector XBetaStar, NumericVector resultantVByVRandomEffects, NumericVector resultantWByURandomEffects)
{
  
  double answer = 0.0;
  
  for(int i = 0; i < XBetaStar.size(); i++){
    answer = answer + exp(XBetaStar[i] + resultantVByVRandomEffects[i] + resultantWByURandomEffects[i]);
  }
  
  return answer;
  
}

