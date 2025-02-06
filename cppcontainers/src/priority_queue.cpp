// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <queue>
#include <string>
#include <vector>
#include <functional>

// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<int> > priority_queue_i_d(Rcpp::IntegerVector& v) {
  std::priority_queue<int>* s = new std::priority_queue<int>(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<int> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<double> > priority_queue_d_d(Rcpp::NumericVector& v) {
  std::priority_queue<double>* s = new std::priority_queue<double>(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<double> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<std::string> > priority_queue_s_d(Rcpp::CharacterVector& v) {
  std::priority_queue<std::string>* s = new std::priority_queue<std::string>(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<std::string> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<bool> > priority_queue_b_d(Rcpp::LogicalVector& v) {
  std::priority_queue<bool>* s = new std::priority_queue<bool>(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<bool> > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > priority_queue_i_a(Rcpp::IntegerVector& v) {
  std::priority_queue<int, std::vector<int>, std::greater<int> >* s = new std::priority_queue<int, std::vector<int>, std::greater<int> >(v.begin(),
    v.end());
  Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > priority_queue_d_a(Rcpp::NumericVector& v) {
  std::priority_queue<double, std::vector<double>, std::greater<double> >* s = new std::priority_queue<double, std::vector<double>,
    std::greater<double> >(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > priority_queue_s_a(Rcpp::CharacterVector& v) {
  std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> >* s = new std::priority_queue<std::string,
    std::vector<std::string>, std::greater<std::string> >(v.begin(), v.end());
  Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > p(s);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > priority_queue_b_a(Rcpp::LogicalVector& v) {
  std::priority_queue<bool, std::vector<bool>, std::greater<bool> >* s = new std::priority_queue<bool, std::vector<bool>, std::greater<bool> >(v.begin(),
    v.end());
  Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > p(s);
  return p;
}
