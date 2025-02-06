// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <stack>
#include <queue>
#include <string>

// stack
// [[Rcpp::export]]
void stack_pop_i(Rcpp::XPtr<std::stack<int> > x) {
  x->pop();
}
// [[Rcpp::export]]
void stack_pop_d(Rcpp::XPtr<std::stack<double> > x) {
  x->pop();
}
// [[Rcpp::export]]
void stack_pop_s(Rcpp::XPtr<std::stack<std::string> > x) {
  x->pop();
}
// [[Rcpp::export]]
void stack_pop_b(Rcpp::XPtr<std::stack<bool> > x) {
  x->pop();
}

// queue
// [[Rcpp::export]]
void queue_pop_i(Rcpp::XPtr<std::queue<int> > x) {
  x->pop();
}
// [[Rcpp::export]]
void queue_pop_d(Rcpp::XPtr<std::queue<double> > x) {
  x->pop();
}
// [[Rcpp::export]]
void queue_pop_s(Rcpp::XPtr<std::queue<std::string> > x) {
  x->pop();
}
// [[Rcpp::export]]
void queue_pop_b(Rcpp::XPtr<std::queue<bool> > x) {
  x->pop();
}

// priority_queue
// [[Rcpp::export]]
void priority_queue_pop_i_d(Rcpp::XPtr<std::priority_queue<int> > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_d_d(Rcpp::XPtr<std::priority_queue<double> > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_s_d(Rcpp::XPtr<std::priority_queue<std::string> > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_b_d(Rcpp::XPtr<std::priority_queue<bool> > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > x) {
  x->pop();
}
// [[Rcpp::export]]
void priority_queue_pop_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > x) {
  x->pop();
}
