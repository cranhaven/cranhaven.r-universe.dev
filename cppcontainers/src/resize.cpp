// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// vector
// [[Rcpp::export]]
void vector_resize_i(Rcpp::XPtr<std::vector<int> > x, const std::size_t n, const int v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void vector_resize_d(Rcpp::XPtr<std::vector<double> > x, const std::size_t n, const double v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void vector_resize_s(Rcpp::XPtr<std::vector<std::string> > x, const std::size_t n, const std::string v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void vector_resize_b(Rcpp::XPtr<std::vector<bool> > x, const std::size_t n, const bool v) {
  x->resize(n, v);
}

// deque
// [[Rcpp::export]]
void deque_resize_i(Rcpp::XPtr<std::deque<int> > x, const std::size_t n, const int v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void deque_resize_d(Rcpp::XPtr<std::deque<double> > x, const std::size_t n, const double v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void deque_resize_s(Rcpp::XPtr<std::deque<std::string> > x, const std::size_t n, const std::string v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void deque_resize_b(Rcpp::XPtr<std::deque<bool> > x, const std::size_t n, const bool v) {
  x->resize(n, v);
}

// forward_list
// [[Rcpp::export]]
void forward_list_resize_i(Rcpp::XPtr<std::forward_list<int> > x, const std::size_t n, const int v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void forward_list_resize_d(Rcpp::XPtr<std::forward_list<double> > x, const std::size_t n, const double v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void forward_list_resize_s(Rcpp::XPtr<std::forward_list<std::string> > x, const std::size_t n, const std::string v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void forward_list_resize_b(Rcpp::XPtr<std::forward_list<bool> > x, const std::size_t n, const bool v) {
  x->resize(n, v);
}

// list
// [[Rcpp::export]]
void list_resize_i(Rcpp::XPtr<std::list<int> > x, const std::size_t n, const int v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void list_resize_d(Rcpp::XPtr<std::list<double> > x, const std::size_t n, const double v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void list_resize_s(Rcpp::XPtr<std::list<std::string> > x, const std::size_t n, const std::string v) {
  x->resize(n, v);
}
// [[Rcpp::export]]
void list_resize_b(Rcpp::XPtr<std::list<bool> > x, const std::size_t n, const bool v) {
  x->resize(n, v);
}
