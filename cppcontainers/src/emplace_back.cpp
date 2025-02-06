// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <list>
#include <string>

// vector
// [[Rcpp::export]]
void vector_emplace_back_i(Rcpp::XPtr<std::vector<int> > x, const int v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void vector_emplace_back_d(Rcpp::XPtr<std::vector<double> > x, const double v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void vector_emplace_back_s(Rcpp::XPtr<std::vector<std::string> > x, const std::string v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void vector_emplace_back_b(Rcpp::XPtr<std::vector<bool> > x, const bool v) {
  x->emplace_back(v);
}

// deque
// [[Rcpp::export]]
void deque_emplace_back_i(Rcpp::XPtr<std::deque<int> > x, const int v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void deque_emplace_back_d(Rcpp::XPtr<std::deque<double> > x, const double v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void deque_emplace_back_s(Rcpp::XPtr<std::deque<std::string> > x, const std::string v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void deque_emplace_back_b(Rcpp::XPtr<std::deque<bool> > x, const bool v) {
  x->emplace_back(v);
}

// list
// [[Rcpp::export]]
void list_emplace_back_i(Rcpp::XPtr<std::list<int> > x, const int v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void list_emplace_back_d(Rcpp::XPtr<std::list<double> > x, const double v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void list_emplace_back_s(Rcpp::XPtr<std::list<std::string> > x, const std::string v) {
  x->emplace_back(v);
}
// [[Rcpp::export]]
void list_emplace_back_b(Rcpp::XPtr<std::list<bool> > x, const bool v) {
  x->emplace_back(v);
}
