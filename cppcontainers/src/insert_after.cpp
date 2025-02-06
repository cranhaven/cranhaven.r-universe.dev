// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <string>
#include <iterator>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_insert_after_i(Rcpp::XPtr<std::forward_list<int> > x, Rcpp::IntegerVector& v, const std::size_t position) {
  x->insert_after(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_insert_after_d(Rcpp::XPtr<std::forward_list<double> > x, Rcpp::NumericVector& v, const std::size_t position) {
  x->insert_after(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_insert_after_s(Rcpp::XPtr<std::forward_list<std::string> > x, Rcpp::CharacterVector& v, const std::size_t position) {
  x->insert_after(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_insert_after_b(Rcpp::XPtr<std::forward_list<bool> > x, Rcpp::LogicalVector& v, const std::size_t position) {
  x->insert_after(std::next(x->begin(), position), v.begin(), v.end());
}
