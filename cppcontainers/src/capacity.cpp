// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <string>
#include <cstddef>

// vector
// [[Rcpp::export]]
std::size_t vector_capacity_i(Rcpp::XPtr<std::vector<int> > x) {
  return x->capacity();
}
// [[Rcpp::export]]
std::size_t vector_capacity_d(Rcpp::XPtr<std::vector<double> > x) {
  return x->capacity();
}
// [[Rcpp::export]]
std::size_t vector_capacity_s(Rcpp::XPtr<std::vector<std::string> > x) {
  return x->capacity();
}
// [[Rcpp::export]]
std::size_t vector_capacity_b(Rcpp::XPtr<std::vector<bool> > x) {
  return x->capacity();
}
