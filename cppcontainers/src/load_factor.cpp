// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <cstddef>

// unordered_set
// [[Rcpp::export]]
double unordered_set_load_factor_i(Rcpp::XPtr<std::unordered_set<int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_set_load_factor_d(Rcpp::XPtr<std::unordered_set<double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_set_load_factor_s(Rcpp::XPtr<std::unordered_set<std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_set_load_factor_b(Rcpp::XPtr<std::unordered_set<bool> > x) {
  return x->load_factor();
}

// unordered_multiset
// [[Rcpp::export]]
double unordered_multiset_load_factor_i(Rcpp::XPtr<std::unordered_multiset<int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_load_factor_d(Rcpp::XPtr<std::unordered_multiset<double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_load_factor_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_load_factor_b(Rcpp::XPtr<std::unordered_multiset<bool> > x) {
  return x->load_factor();
}

// unordered_map
// [[Rcpp::export]]
double unordered_map_load_factor_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_map_load_factor_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x) {
  return x->load_factor();
}

// unordered_multimap
// [[Rcpp::export]]
double unordered_multimap_load_factor_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x) {
  return x->load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_load_factor_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x) {
  return x->load_factor();
}
