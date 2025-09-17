#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

double getSumExpNetwork(NumericVector XBetaStar, NumericVector resultantWByURandomEffects)
{
  
  double answer = 0.0;
  
  for(int i = 0; i < XBetaStar.size(); i++){
    answer = answer + exp(XBetaStar[i] + resultantWByURandomEffects[i]);
  }
  
  return answer;
  
}
