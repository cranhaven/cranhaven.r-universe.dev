// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
std::size_t forward_list_unique_i(Rcpp::XPtr<std::forward_list<int> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t forward_list_unique_d(Rcpp::XPtr<std::forward_list<double> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t forward_list_unique_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t forward_list_unique_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  return x->unique();
}

// list
// [[Rcpp::export]]
std::size_t list_unique_i(Rcpp::XPtr<std::list<int> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t list_unique_d(Rcpp::XPtr<std::list<double> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t list_unique_s(Rcpp::XPtr<std::list<std::string> > x) {
  return x->unique();
}
// [[Rcpp::export]]
std::size_t list_unique_b(Rcpp::XPtr<std::list<bool> > x) {
  return x->unique();
}
