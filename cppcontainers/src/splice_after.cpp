// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <string>
#include <iterator>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_splice_after_i(Rcpp::XPtr<std::forward_list<int> > x, Rcpp::XPtr<std::forward_list<int> > y, const std::size_t x_position,
  const std::size_t y_from, const std::size_t y_to) {
  x->splice_after(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void forward_list_splice_after_d(Rcpp::XPtr<std::forward_list<double> > x, Rcpp::XPtr<std::forward_list<double> > y, const std::size_t x_position,
  const std::size_t y_from, const std::size_t y_to) {
  x->splice_after(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void forward_list_splice_after_s(Rcpp::XPtr<std::forward_list<std::string> > x, Rcpp::XPtr<std::forward_list<std::string> > y, const std::size_t x_position,
  const std::size_t y_from, const std::size_t y_to) {
  x->splice_after(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void forward_list_splice_after_b(Rcpp::XPtr<std::forward_list<bool> > x, Rcpp::XPtr<std::forward_list<bool> > y, const std::size_t x_position,
  const std::size_t y_from, const std::size_t y_to) {
  x->splice_after(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
