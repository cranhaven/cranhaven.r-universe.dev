// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>

// vector
// [[Rcpp::export]]
void vector_flip_b(Rcpp::XPtr<std::vector<bool> > x) {
  x->flip();
}
