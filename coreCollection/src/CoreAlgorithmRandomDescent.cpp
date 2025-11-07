#include "CoreAlgorithmRandomDescent.h"
using namespace Rcpp;

const std::string ALGORITHM_RANDOM_DESCENT = "randomDescent";

Rcpp::IntegerVector CoreAlgorithmRandomDescent::getCore (CoreMethod &m) {
  Rcpp::IntegerVector c1 = m.getInitial();
  Rcpp::IntegerVector c2;
  double m1 = m.measure(c1), m2;
  int steps=0;
  int n = m.accessionNumber * m.randomCoreNumber;
  int k = std::max(10000, 2 * n);
  int steps1=0, steps2=0;
  while(steps1<k && steps2<k) {
    c2 = m.getRandomNeighbour(c1);
    m2 = m.measure(c2);
    if(m.improvement(m1,m2)) {
      c1 = clone(c2);
      m1 = m2;
      steps2 = 0;
    } else {
      steps2+=steps1;
    }
    steps1++;
  }
  return c1;
}







