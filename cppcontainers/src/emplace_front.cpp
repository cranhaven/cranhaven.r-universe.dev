// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <deque>
#include <forward_list>
#include <list>
#include <string>

// deque
// [[Rcpp::export]]
void deque_emplace_front_i(Rcpp::XPtr<std::deque<int> > x, const int v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void deque_emplace_front_d(Rcpp::XPtr<std::deque<double> > x, const double v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void deque_emplace_front_s(Rcpp::XPtr<std::deque<std::string> > x, const std::string v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void deque_emplace_front_b(Rcpp::XPtr<std::deque<bool> > x, const bool v) {
  x->emplace_front(v);
}

// forward_list
// [[Rcpp::export]]
void forward_list_emplace_front_i(Rcpp::XPtr<std::forward_list<int> > x, const int v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void forward_list_emplace_front_d(Rcpp::XPtr<std::forward_list<double> > x, const double v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void forward_list_emplace_front_s(Rcpp::XPtr<std::forward_list<std::string> > x, const std::string v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void forward_list_emplace_front_b(Rcpp::XPtr<std::forward_list<bool> > x, const bool v) {
  x->emplace_front(v);
}

// list
// [[Rcpp::export]]
void list_emplace_front_i(Rcpp::XPtr<std::list<int> > x, const int v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void list_emplace_front_d(Rcpp::XPtr<std::list<double> > x, const double v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void list_emplace_front_s(Rcpp::XPtr<std::list<std::string> > x, const std::string v) {
  x->emplace_front(v);
}
// [[Rcpp::export]]
void list_emplace_front_b(Rcpp::XPtr<std::list<bool> > x, const bool v) {
  x->emplace_front(v);
}
