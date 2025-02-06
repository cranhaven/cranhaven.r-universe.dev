// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <list>
#include <string>

// vector
// [[Rcpp::export]]
void vector_pop_back_i(Rcpp::XPtr<std::vector<int> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void vector_pop_back_d(Rcpp::XPtr<std::vector<double> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void vector_pop_back_s(Rcpp::XPtr<std::vector<std::string> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void vector_pop_back_b(Rcpp::XPtr<std::vector<bool> > x) {
  x->pop_back();
}

// deque
// [[Rcpp::export]]
void deque_pop_back_i(Rcpp::XPtr<std::deque<int> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void deque_pop_back_d(Rcpp::XPtr<std::deque<double> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void deque_pop_back_s(Rcpp::XPtr<std::deque<std::string> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void deque_pop_back_b(Rcpp::XPtr<std::deque<bool> > x) {
  x->pop_back();
}

// list
// [[Rcpp::export]]
void list_pop_back_i(Rcpp::XPtr<std::list<int> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void list_pop_back_d(Rcpp::XPtr<std::list<double> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void list_pop_back_s(Rcpp::XPtr<std::list<std::string> > x) {
  x->pop_back();
}
// [[Rcpp::export]]
void list_pop_back_b(Rcpp::XPtr<std::list<bool> > x) {
  x->pop_back();
}
