// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <list>
#include <string>

// [[Rcpp::export]]
Rcpp::XPtr<std::list<int> > list_i(Rcpp::IntegerVector& v) {
  std::list<int>* s = new std::list<int>(v.begin(), v.end());
  Rcpp::XPtr<std::list<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::list<double> > list_d(Rcpp::NumericVector& v) {
  std::list<double>* s = new std::list<double>(v.begin(), v.end());
  Rcpp::XPtr<std::list<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::list<std::string> > list_s(Rcpp::CharacterVector& v) {
  std::list<std::string>* s = new std::list<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::list<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::list<bool> > list_b(Rcpp::LogicalVector& v) {
  std::list<bool>* s = new std::list<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::list<bool> > p(s);
  return p;
}
