// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_reverse_i(Rcpp::XPtr<std::forward_list<int> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void forward_list_reverse_d(Rcpp::XPtr<std::forward_list<double> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void forward_list_reverse_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void forward_list_reverse_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  x->reverse();
}

// list
// [[Rcpp::export]]
void list_reverse_i(Rcpp::XPtr<std::list<int> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void list_reverse_d(Rcpp::XPtr<std::list<double> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void list_reverse_s(Rcpp::XPtr<std::list<std::string> > x) {
  x->reverse();
}
// [[Rcpp::export]]
void list_reverse_b(Rcpp::XPtr<std::list<bool> > x) {
  x->reverse();
}
