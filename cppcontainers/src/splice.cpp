// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <list>
#include <string>
#include <iterator>
#include <cstddef>

// list
// [[Rcpp::export]]
void list_splice_i(Rcpp::XPtr<std::list<int> > x, Rcpp::XPtr<std::list<int> > y, const std::size_t x_position, const std::size_t y_from,
  const std::size_t y_to) {
  x->splice(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void list_splice_d(Rcpp::XPtr<std::list<double> > x, Rcpp::XPtr<std::list<double> > y, const std::size_t x_position, const std::size_t y_from,
  const std::size_t y_to) {
  x->splice(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void list_splice_s(Rcpp::XPtr<std::list<std::string> > x, Rcpp::XPtr<std::list<std::string> > y, const std::size_t x_position, const std::size_t y_from,
  const std::size_t y_to) {
  x->splice(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
// [[Rcpp::export]]
void list_splice_b(Rcpp::XPtr<std::list<bool> > x, Rcpp::XPtr<std::list<bool> > y, const std::size_t x_position, const std::size_t y_from,
  const std::size_t y_to) {
  x->splice(std::next(x->begin(), x_position), *y, std::next(y->begin(), y_from), std::next(y->begin(), y_to));
}
