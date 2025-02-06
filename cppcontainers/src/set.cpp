// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <string>

// template <typename T>
// Rcpp::XPtr<std::set<T> > set(SEXP& v) {
//   std::set<T>* s = new std::set<T>(v.begin(), v.end());
//   Rcpp::XPtr<std::set<T> > p(s);
//   return p;
// }

// [[Rcpp::export]]
Rcpp::XPtr<std::set<int> > set_i(Rcpp::IntegerVector& v) {
  std::set<int>* s = new std::set<int>(v.begin(), v.end());
  Rcpp::XPtr<std::set<int> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::set<double> > set_d(Rcpp::NumericVector& v) {
  std::set<double>* s = new std::set<double>(v.begin(), v.end());
  Rcpp::XPtr<std::set<double> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::set<std::string> > set_s(Rcpp::CharacterVector& v) {
  std::set<std::string>* s = new std::set<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::set<std::string> > p(s);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::set<bool> > set_b(Rcpp::LogicalVector& v) {
  std::set<bool>* s = new std::set<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::set<bool> > p(s);
  return p;
}
