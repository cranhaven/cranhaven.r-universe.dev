#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double sumMatrix(NumericMatrix matrix)
{
  
  int numberOfRows = matrix.rows();
  int numberOfColumns = matrix.cols();
  
  double answer = 0.0;
  
  for(int i = 0; i < numberOfRows; i++){
    for(int j = 0; j < numberOfColumns; j++){
      answer = answer + matrix(i, j);
    }
  }
  
  return answer;
  
}
