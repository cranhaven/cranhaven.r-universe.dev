// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <queue>
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::XPtr<std::queue<int> > queue_i(Rcpp::IntegerVector& v) {
  std::queue<int>* s = new std::queue<int>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::queue<int> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::queue<double> > queue_d(Rcpp::NumericVector& v) {
  std::queue<double>* s = new std::queue<double>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::queue<double> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::queue<std::string> > queue_s(Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  std::queue<std::string>* s = new std::queue<std::string>;
  for(auto& i : c) {
    s->push(i);
  }
  Rcpp::XPtr<std::queue<std::string> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::queue<bool> > queue_b(Rcpp::LogicalVector& v) {
  std::queue<bool>* s = new std::queue<bool>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::queue<bool> > p(s);
  return p;
}
