// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <string>

// vector
// [[Rcpp::export]]
void vector_shrink_to_fit_i(Rcpp::XPtr<std::vector<int> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void vector_shrink_to_fit_d(Rcpp::XPtr<std::vector<double> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void vector_shrink_to_fit_s(Rcpp::XPtr<std::vector<std::string> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void vector_shrink_to_fit_b(Rcpp::XPtr<std::vector<bool> > x) {
  x->shrink_to_fit();
}

// deque
// [[Rcpp::export]]
void deque_shrink_to_fit_i(Rcpp::XPtr<std::deque<int> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void deque_shrink_to_fit_d(Rcpp::XPtr<std::deque<double> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void deque_shrink_to_fit_s(Rcpp::XPtr<std::deque<std::string> > x) {
  x->shrink_to_fit();
}
// [[Rcpp::export]]
void deque_shrink_to_fit_b(Rcpp::XPtr<std::deque<bool> > x) {
  x->shrink_to_fit();
}
