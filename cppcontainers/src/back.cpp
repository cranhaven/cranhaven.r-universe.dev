// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <queue>
#include <vector>
#include <deque>
#include <list>
#include <string>

// queue
// [[Rcpp::export]]
int queue_back_i(Rcpp::XPtr<std::queue<int> > x) {
  return x->back();
}
// [[Rcpp::export]]
double queue_back_d(Rcpp::XPtr<std::queue<double> > x) {
  return x->back();
}
// [[Rcpp::export]]
std::string queue_back_s(Rcpp::XPtr<std::queue<std::string> > x) {
  return x->back();
}
// [[Rcpp::export]]
bool queue_back_b(Rcpp::XPtr<std::queue<bool> > x) {
  return x->back();
}

// vector
// [[Rcpp::export]]
int vector_back_i(Rcpp::XPtr<std::vector<int> > x) {
  return x->back();
}
// [[Rcpp::export]]
double vector_back_d(Rcpp::XPtr<std::vector<double> > x) {
  return x->back();
}
// [[Rcpp::export]]
std::string vector_back_s(Rcpp::XPtr<std::vector<std::string> > x) {
  return x->back();
}
// [[Rcpp::export]]
bool vector_back_b(Rcpp::XPtr<std::vector<bool> > x) {
  return x->back();
}

// deque
// [[Rcpp::export]]
int deque_back_i(Rcpp::XPtr<std::deque<int> > x) {
  return x->back();
}
// [[Rcpp::export]]
double deque_back_d(Rcpp::XPtr<std::deque<double> > x) {
  return x->back();
}
// [[Rcpp::export]]
std::string deque_back_s(Rcpp::XPtr<std::deque<std::string> > x) {
  return x->back();
}
// [[Rcpp::export]]
bool deque_back_b(Rcpp::XPtr<std::deque<bool> > x) {
  return x->back();
}

// list
// [[Rcpp::export]]
int list_back_i(Rcpp::XPtr<std::list<int> > x) {
  return x->back();
}
// [[Rcpp::export]]
double list_back_d(Rcpp::XPtr<std::list<double> > x) {
  return x->back();
}
// [[Rcpp::export]]
std::string list_back_s(Rcpp::XPtr<std::list<std::string> > x) {
  return x->back();
}
// [[Rcpp::export]]
bool list_back_b(Rcpp::XPtr<std::list<bool> > x) {
  return x->back();
}
