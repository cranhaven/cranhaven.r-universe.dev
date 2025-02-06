// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <deque>
#include <forward_list>
#include <list>
#include <string>

// deque
// [[Rcpp::export]]
void deque_pop_front_i(Rcpp::XPtr<std::deque<int> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void deque_pop_front_d(Rcpp::XPtr<std::deque<double> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void deque_pop_front_s(Rcpp::XPtr<std::deque<std::string> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void deque_pop_front_b(Rcpp::XPtr<std::deque<bool> > x) {
  x->pop_front();
}

// forward_list
// [[Rcpp::export]]
void forward_list_pop_front_i(Rcpp::XPtr<std::forward_list<int> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void forward_list_pop_front_d(Rcpp::XPtr<std::forward_list<double> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void forward_list_pop_front_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void forward_list_pop_front_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  x->pop_front();
}

// list
// [[Rcpp::export]]
void list_pop_front_i(Rcpp::XPtr<std::list<int> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void list_pop_front_d(Rcpp::XPtr<std::list<double> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void list_pop_front_s(Rcpp::XPtr<std::list<std::string> > x) {
  x->pop_front();
}
// [[Rcpp::export]]
void list_pop_front_b(Rcpp::XPtr<std::list<bool> > x) {
  x->pop_front();
}
