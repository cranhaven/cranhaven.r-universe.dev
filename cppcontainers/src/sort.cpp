// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// forward_list
// [[Rcpp::export]]
void forward_list_sort_i(Rcpp::XPtr<std::forward_list<int> > x) {
  x->sort();
}
// [[Rcpp::export]]
void forward_list_sort_d(Rcpp::XPtr<std::forward_list<double> > x) {
  x->sort();
}
// [[Rcpp::export]]
void forward_list_sort_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  x->sort();
}
// [[Rcpp::export]]
void forward_list_sort_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  x->sort();
}

// list
// [[Rcpp::export]]
void list_sort_i(Rcpp::XPtr<std::list<int> > x) {
  x->sort();
}
// [[Rcpp::export]]
void list_sort_d(Rcpp::XPtr<std::list<double> > x) {
  x->sort();
}
// [[Rcpp::export]]
void list_sort_s(Rcpp::XPtr<std::list<std::string> > x) {
  x->sort();
}
// [[Rcpp::export]]
void list_sort_b(Rcpp::XPtr<std::list<bool> > x) {
  x->sort();
}
