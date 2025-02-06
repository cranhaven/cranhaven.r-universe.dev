// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <string>
#include <iterator>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_emplace_after_i(Rcpp::XPtr<std::forward_list<int> > x, const int v, const std::size_t position) {
  x->emplace_after(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void forward_list_emplace_after_d(Rcpp::XPtr<std::forward_list<double> > x, const double v, const std::size_t position) {
  x->emplace_after(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void forward_list_emplace_after_s(Rcpp::XPtr<std::forward_list<std::string> > x, const std::string v, const std::size_t position) {
  x->emplace_after(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void forward_list_emplace_after_b(Rcpp::XPtr<std::forward_list<bool> > x, const bool v, const std::size_t position) {
  x->emplace_after(std::next(x->begin(), position), v);
}
