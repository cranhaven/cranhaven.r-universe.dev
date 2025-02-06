// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <stack>
#include <queue>
#include <string>

// stack
// [[Rcpp::export]]
int stack_top_i(Rcpp::XPtr<std::stack<int> > x) {
  return x->top();
}
// [[Rcpp::export]]
double stack_top_d(Rcpp::XPtr<std::stack<double> > x) {
  return x->top();
}
// [[Rcpp::export]]
std::string stack_top_s(Rcpp::XPtr<std::stack<std::string> > x) {
  return x->top();
}
// [[Rcpp::export]]
bool stack_top_b(Rcpp::XPtr<std::stack<bool> > x) {
  return x->top();
}

// priority_queue
// [[Rcpp::export]]
int priority_queue_top_i_d(Rcpp::XPtr<std::priority_queue<int> > x) {
  return x->top();
}
// [[Rcpp::export]]
double priority_queue_top_d_d(Rcpp::XPtr<std::priority_queue<double> > x) {
  return x->top();
}
// [[Rcpp::export]]
std::string priority_queue_top_s_d(Rcpp::XPtr<std::priority_queue<std::string> > x) {
  return x->top();
}
// [[Rcpp::export]]
bool priority_queue_top_b_d(Rcpp::XPtr<std::priority_queue<bool> > x) {
  return x->top();
}
// [[Rcpp::export]]
int priority_queue_top_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > x) {
  return x->top();
}
// [[Rcpp::export]]
double priority_queue_top_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > x) {
  return x->top();
}
// [[Rcpp::export]]
std::string priority_queue_top_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > x) {
  return x->top();
}
// [[Rcpp::export]]
bool priority_queue_top_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > x) {
  return x->top();
}
