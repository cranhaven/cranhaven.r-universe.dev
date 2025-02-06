// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <string>

// [[Rcpp::export]]
Rcpp::XPtr<std::multiset<int> > multiset_i(Rcpp::IntegerVector& v) {
  std::multiset<int>* s = new std::multiset<int>(v.begin(), v.end());
  Rcpp::XPtr<std::multiset<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multiset<double> > multiset_d(Rcpp::NumericVector& v) {
  std::multiset<double>* s = new std::multiset<double>(v.begin(), v.end());
  Rcpp::XPtr<std::multiset<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multiset<std::string> > multiset_s(Rcpp::CharacterVector& v) {
  std::multiset<std::string>* s = new std::multiset<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::multiset<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multiset<bool> > multiset_b(Rcpp::LogicalVector& v) {
  std::multiset<bool>* s = new std::multiset<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::multiset<bool> > p(s);
  return p;
}
