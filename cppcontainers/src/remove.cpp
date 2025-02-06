// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_remove_i(Rcpp::XPtr<std::forward_list<int> > x, const int v) {
  x->remove(v);
}
// [[Rcpp::export]]
void forward_list_remove_d(Rcpp::XPtr<std::forward_list<double> > x, const double v) {
  x->remove(v);
}
// [[Rcpp::export]]
void forward_list_remove_s(Rcpp::XPtr<std::forward_list<std::string> > x, const std::string v) {
  x->remove(v);
}
// [[Rcpp::export]]
void forward_list_remove_b(Rcpp::XPtr<std::forward_list<bool> > x, const bool v) {
  x->remove(v);
}

// list
// [[Rcpp::export]]
void list_remove_i(Rcpp::XPtr<std::list<int> > x, const int v) {
  x->remove(v);
}
// [[Rcpp::export]]
void list_remove_d(Rcpp::XPtr<std::list<double> > x, const double v) {
  x->remove(v);
}
// [[Rcpp::export]]
void list_remove_s(Rcpp::XPtr<std::list<std::string> > x, const std::string v) {
  x->remove(v);
}
// [[Rcpp::export]]
void list_remove_b(Rcpp::XPtr<std::list<bool> > x, const bool v) {
  x->remove(v);
}
