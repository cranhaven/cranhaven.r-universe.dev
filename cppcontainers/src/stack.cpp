// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <stack>
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::XPtr<std::stack<int> > stack_i(Rcpp::IntegerVector& v) {
  std::stack<int>* s = new std::stack<int>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::stack<int> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::stack<double> > stack_d(Rcpp::NumericVector& v) {
  std::stack<double>* s = new std::stack<double>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::stack<double> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::stack<std::string> > stack_s(Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  std::stack<std::string>* s = new std::stack<std::string>;
  for(auto& i : c) {
    s->push(i);
  }
  Rcpp::XPtr<std::stack<std::string> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::stack<bool> > stack_b(Rcpp::LogicalVector& v) {
  std::stack<bool>* s = new std::stack<bool>;
  for(auto& i : v) {
    s->push(i);
  }
  Rcpp::XPtr<std::stack<bool> > p(s);
  return p;
}
