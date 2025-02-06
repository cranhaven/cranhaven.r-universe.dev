// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_set>
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multiset<int> > unordered_multiset_i(Rcpp::IntegerVector& v) {
  std::unordered_multiset<int>* s = new std::unordered_multiset<int>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_multiset<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multiset<double> > unordered_multiset_d(Rcpp::NumericVector& v) {
  std::unordered_multiset<double>* s = new std::unordered_multiset<double>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_multiset<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multiset<std::string> > unordered_multiset_s(Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  std::unordered_multiset<std::string>* s = new std::unordered_multiset<std::string>(c.begin(), c.end());
  Rcpp::XPtr<std::unordered_multiset<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multiset<bool> > unordered_multiset_b(Rcpp::LogicalVector& v) {
  std::unordered_multiset<bool>* s = new std::unordered_multiset<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::unordered_multiset<bool> > p(s);
  return p;
}
