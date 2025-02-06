// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <map>
#include <unordered_map>
#include <vector>
#include <deque>
#include <string>
#include <cstddef>

// map
// [[Rcpp::export]]
int map_at_i_i(Rcpp::XPtr<std::map<int, int> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
double map_at_i_d(Rcpp::XPtr<std::map<int, double> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string map_at_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool map_at_i_b(Rcpp::XPtr<std::map<int, bool> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
int map_at_d_i(Rcpp::XPtr<std::map<double, int> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
double map_at_d_d(Rcpp::XPtr<std::map<double, double> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string map_at_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool map_at_d_b(Rcpp::XPtr<std::map<double, bool> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
int map_at_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
double map_at_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string map_at_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool map_at_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
int map_at_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
double map_at_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string map_at_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool map_at_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool key) {
  return x->at(key);
}

// unordered_map
// [[Rcpp::export]]
int unordered_map_at_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
double unordered_map_at_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string unordered_map_at_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool unordered_map_at_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, const int key) {
  return x->at(key);
}
// [[Rcpp::export]]
int unordered_map_at_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
double unordered_map_at_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string unordered_map_at_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool unordered_map_at_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, const double key) {
  return x->at(key);
}
// [[Rcpp::export]]
int unordered_map_at_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
double unordered_map_at_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string unordered_map_at_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool unordered_map_at_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const std::string key) {
  return x->at(key);
}
// [[Rcpp::export]]
int unordered_map_at_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
double unordered_map_at_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
std::string unordered_map_at_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const bool key) {
  return x->at(key);
}
// [[Rcpp::export]]
bool unordered_map_at_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const bool key) {
  return x->at(key);
}

// vector
// [[Rcpp::export]]
int vector_at_i(Rcpp::XPtr<std::vector<int> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
double vector_at_d(Rcpp::XPtr<std::vector<double> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
std::string vector_at_s(Rcpp::XPtr<std::vector<std::string> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
bool vector_at_b(Rcpp::XPtr<std::vector<bool> > x, const std::size_t index) {
  return x->at(index);
}

// deque
// [[Rcpp::export]]
int deque_at_i(Rcpp::XPtr<std::deque<int> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
double deque_at_d(Rcpp::XPtr<std::deque<double> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
std::string deque_at_s(Rcpp::XPtr<std::deque<std::string> > x, const std::size_t index) {
  return x->at(index);
}
// [[Rcpp::export]]
bool deque_at_b(Rcpp::XPtr<std::deque<bool> > x, const std::size_t index) {
  return x->at(index);
}
