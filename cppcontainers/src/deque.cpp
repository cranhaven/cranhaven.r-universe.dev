// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <deque>
#include <string>

// [[Rcpp::export]]
Rcpp::XPtr<std::deque<int> > deque_i(Rcpp::IntegerVector& v) {
  std::deque<int>* s = new std::deque<int>(v.begin(), v.end());
  Rcpp::XPtr<std::deque<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::deque<double> > deque_d(Rcpp::NumericVector& v) {
  std::deque<double>* s = new std::deque<double>(v.begin(), v.end());
  Rcpp::XPtr<std::deque<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::deque<std::string> > deque_s(Rcpp::CharacterVector& v) {
  std::deque<std::string>* s = new std::deque<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::deque<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::deque<bool> > deque_b(Rcpp::LogicalVector& v) {
  std::deque<bool>* s = new std::deque<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::deque<bool> > p(s);
  return p;
}
