// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <queue>
#include <vector>
#include <deque>
#include <forward_list>
#include <list>
#include <string>

// queue
// [[Rcpp::export]]
int queue_front_i(Rcpp::XPtr<std::queue<int> > x) {
  return x->front();
}
// [[Rcpp::export]]
double queue_front_d(Rcpp::XPtr<std::queue<double> > x) {
  return x->front();
}
// [[Rcpp::export]]
std::string queue_front_s(Rcpp::XPtr<std::queue<std::string> > x) {
  return x->front();
}
// [[Rcpp::export]]
bool queue_front_b(Rcpp::XPtr<std::queue<bool> > x) {
  return x->front();
}

// vector
// [[Rcpp::export]]
int vector_front_i(Rcpp::XPtr<std::vector<int> > x) {
  return x->front();
}
// [[Rcpp::export]]
double vector_front_d(Rcpp::XPtr<std::vector<double> > x) {
  return x->front();
}
// [[Rcpp::export]]
std::string vector_front_s(Rcpp::XPtr<std::vector<std::string> > x) {
  return x->front();
}
// [[Rcpp::export]]
bool vector_front_b(Rcpp::XPtr<std::vector<bool> > x) {
  return x->front();
}

// deque
// [[Rcpp::export]]
int deque_front_i(Rcpp::XPtr<std::deque<int> > x) {
  return x->front();
}
// [[Rcpp::export]]
double deque_front_d(Rcpp::XPtr<std::deque<double> > x) {
  return x->front();
}
// [[Rcpp::export]]
std::string deque_front_s(Rcpp::XPtr<std::deque<std::string> > x) {
  return x->front();
}
// [[Rcpp::export]]
bool deque_front_b(Rcpp::XPtr<std::deque<bool> > x) {
  return x->front();
}

// forward_list
// [[Rcpp::export]]
int forward_list_front_i(Rcpp::XPtr<std::forward_list<int> > x) {
  return x->front();
}
// [[Rcpp::export]]
double forward_list_front_d(Rcpp::XPtr<std::forward_list<double> > x) {
  return x->front();
}
// [[Rcpp::export]]
std::string forward_list_front_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  return x->front();
}
// [[Rcpp::export]]
bool forward_list_front_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  return x->front();
}

// list
// [[Rcpp::export]]
int list_front_i(Rcpp::XPtr<std::list<int> > x) {
  return x->front();
}
// [[Rcpp::export]]
double list_front_d(Rcpp::XPtr<std::list<double> > x) {
  return x->front();
}
// [[Rcpp::export]]
std::string list_front_s(Rcpp::XPtr<std::list<std::string> > x) {
  return x->front();
}
// [[Rcpp::export]]
bool list_front_b(Rcpp::XPtr<std::list<bool> > x) {
  return x->front();
}
