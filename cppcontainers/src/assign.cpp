// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <vector>
#include <deque>
#include <forward_list>
#include <list>
#include <string>

// vector
// [[Rcpp::export]]
void vector_assign_i(Rcpp::XPtr<std::vector<int> > x, Rcpp::IntegerVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_assign_d(Rcpp::XPtr<std::vector<double> > x, Rcpp::NumericVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_assign_s(Rcpp::XPtr<std::vector<std::string> > x, Rcpp::CharacterVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_assign_b(Rcpp::XPtr<std::vector<bool> > x, Rcpp::LogicalVector& v) {
  x->assign(v.begin(), v.end());
}

// deque
// [[Rcpp::export]]
void deque_assign_i(Rcpp::XPtr<std::deque<int> > x, Rcpp::IntegerVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_assign_d(Rcpp::XPtr<std::deque<double> > x, Rcpp::NumericVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_assign_s(Rcpp::XPtr<std::deque<std::string> > x, Rcpp::CharacterVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_assign_b(Rcpp::XPtr<std::deque<bool> > x, Rcpp::LogicalVector& v) {
  x->assign(v.begin(), v.end());
}

// forward_list
// [[Rcpp::export]]
void forward_list_assign_i(Rcpp::XPtr<std::forward_list<int> > x, Rcpp::IntegerVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_assign_d(Rcpp::XPtr<std::forward_list<double> > x, Rcpp::NumericVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_assign_s(Rcpp::XPtr<std::forward_list<std::string> > x, Rcpp::CharacterVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void forward_list_assign_b(Rcpp::XPtr<std::forward_list<bool> > x, Rcpp::LogicalVector& v) {
  x->assign(v.begin(), v.end());
}

// list
// [[Rcpp::export]]
void list_assign_i(Rcpp::XPtr<std::list<int> > x, Rcpp::IntegerVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void list_assign_d(Rcpp::XPtr<std::list<double> > x, Rcpp::NumericVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void list_assign_s(Rcpp::XPtr<std::list<std::string> > x, Rcpp::CharacterVector& v) {
  x->assign(v.begin(), v.end());
}
// [[Rcpp::export]]
void list_assign_b(Rcpp::XPtr<std::list<bool> > x, Rcpp::LogicalVector& v) {
  x->assign(v.begin(), v.end());
}
