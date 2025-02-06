// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <map>
#include <unordered_map>
#include <string>

// template <typename K, typename V>
// void try_emplace(Rcpp::XPtr<T> x, const K k, const V v) {
//   x->try_emplace(k, v);
// }

// map
// [[Rcpp::export]]
void map_try_emplace_i_i(Rcpp::XPtr<std::map<int, int> > x, const int k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_i_d(Rcpp::XPtr<std::map<int, double> > x, const int k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const int k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_i_b(Rcpp::XPtr<std::map<int, bool> > x, const int k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_d_i(Rcpp::XPtr<std::map<double, int> > x, const double k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_d_d(Rcpp::XPtr<std::map<double, double> > x, const double k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const double k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_d_b(Rcpp::XPtr<std::map<double, bool> > x, const double k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const std::string k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const std::string k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const std::string k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const std::string k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void map_try_emplace_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool k, const bool v) {
  x->try_emplace(k, v);
}

// unordered_map
// [[Rcpp::export]]
void unordered_map_try_emplace_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, const int k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, const int k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const int k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, const int k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, const double k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, const double k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const double k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, const double k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const std::string k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const std::string k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const std::string k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const std::string k, const bool v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, const bool k, const int v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, const bool k, const double v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const bool k, const std::string v) {
  x->try_emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_try_emplace_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const bool k, const bool v) {
  x->try_emplace(k, v);
}
