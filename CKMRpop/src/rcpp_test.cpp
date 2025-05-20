#include <Rcpp.h>
using namespace Rcpp;



// Reference testing
//
// Boing
// @export
// [[Rcpp::export]]
void recursive_push_back(CharacterVector& Boing, int i) {
  Boing.push_back(" Boing ");
  if(i>0) recursive_push_back(Boing, i - 1);
}


// A function used for quickly testing things
//
// Boing
// @param v1 the input vector for testing
// @export
// [[Rcpp::export]]
CharacterVector rcpp_test(CharacterVector v1) {
  CharacterVector v3 = clone(v1);
  recursive_push_back(v3, 5);

  return(unique(v3));
}

