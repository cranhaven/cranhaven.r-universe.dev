// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <string>

// [[Rcpp::export]]
Rcpp::XPtr<std::vector<int> > vector_i(Rcpp::IntegerVector& v) {
  std::vector<int>* s = new std::vector<int>(v.begin(), v.end());
  Rcpp::XPtr<std::vector<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::vector<double> > vector_d(Rcpp::NumericVector& v) {
  std::vector<double>* s = new std::vector<double>(v.begin(), v.end());
  Rcpp::XPtr<std::vector<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::vector<std::string> > vector_s(Rcpp::CharacterVector& v) {
  std::vector<std::string>* s = new std::vector<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::vector<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::vector<bool> > vector_b(Rcpp::LogicalVector& v) {
  std::vector<bool>* s = new std::vector<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::vector<bool> > p(s);
  return p;
}
