// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <forward_list>
#include <string>

// [[Rcpp::export]]
Rcpp::XPtr<std::forward_list<int> > forward_list_i(Rcpp::IntegerVector& v) {
  std::forward_list<int>* s = new std::forward_list<int>(v.begin(), v.end());
  Rcpp::XPtr<std::forward_list<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::forward_list<double> > forward_list_d(Rcpp::NumericVector& v) {
  std::forward_list<double>* s = new std::forward_list<double>(v.begin(), v.end());
  Rcpp::XPtr<std::forward_list<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::forward_list<std::string> > forward_list_s(Rcpp::CharacterVector& v) {
  std::forward_list<std::string>* s = new std::forward_list<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::forward_list<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::forward_list<bool> > forward_list_b(Rcpp::LogicalVector& v) {
  std::forward_list<bool>* s = new std::forward_list<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::forward_list<bool> > p(s);
  return p;
}
