// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <string>
#include <iterator>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_erase_after_i(Rcpp::XPtr<std::forward_list<int> > x, const std::size_t from, const std::size_t to) {
  x->erase_after(std::next(x->begin(), from), std::next(x->begin(), to));
}
// [[Rcpp::export]]
void forward_list_erase_after_d(Rcpp::XPtr<std::forward_list<double> > x, const std::size_t from, const std::size_t to) {
  x->erase_after(std::next(x->begin(), from), std::next(x->begin(), to));
}
// [[Rcpp::export]]
void forward_list_erase_after_s(Rcpp::XPtr<std::forward_list<std::string> > x, const std::size_t from, const std::size_t to) {
  x->erase_after(std::next(x->begin(), from), std::next(x->begin(), to));
}
// [[Rcpp::export]]
void forward_list_erase_after_b(Rcpp::XPtr<std::forward_list<bool> > x, const std::size_t from, const std::size_t to) {
  x->erase_after(std::next(x->begin(), from), std::next(x->begin(), to));
}
