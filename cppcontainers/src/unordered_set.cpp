// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_set>
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_set<int> > unordered_set_i(Rcpp::IntegerVector& v) {
  std::unordered_set<int>* s = new std::unordered_set<int>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_set<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_set<double> > unordered_set_d(Rcpp::NumericVector& v) {
  std::unordered_set<double>* s = new std::unordered_set<double>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_set<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_set<std::string> > unordered_set_s(Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  std::unordered_set<std::string>* s = new std::unordered_set<std::string>(c.begin(), c.end());
  Rcpp::XPtr<std::unordered_set<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_set<bool> > unordered_set_b(Rcpp::LogicalVector& v) {
  std::unordered_set<bool>* s = new std::unordered_set<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_set<bool> > p(s);
  return p;
}
