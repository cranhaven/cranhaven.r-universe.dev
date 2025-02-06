// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <cstddef>

// unordered_set
// [[Rcpp::export]]
double unordered_set_max_load_factor_i_get(Rcpp::XPtr<std::unordered_set<int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_set_max_load_factor_d_get(Rcpp::XPtr<std::unordered_set<double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_set_max_load_factor_s_get(Rcpp::XPtr<std::unordered_set<std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_set_max_load_factor_b_get(Rcpp::XPtr<std::unordered_set<bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
void unordered_set_max_load_factor_i_set(Rcpp::XPtr<std::unordered_set<int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_set_max_load_factor_d_set(Rcpp::XPtr<std::unordered_set<double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_set_max_load_factor_s_set(Rcpp::XPtr<std::unordered_set<std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_set_max_load_factor_b_set(Rcpp::XPtr<std::unordered_set<bool> > x, const double l) {
  x->max_load_factor(l);
}

// unordered_multiset
// [[Rcpp::export]]
double unordered_multiset_max_load_factor_i_get(Rcpp::XPtr<std::unordered_multiset<int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_max_load_factor_d_get(Rcpp::XPtr<std::unordered_multiset<double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_max_load_factor_s_get(Rcpp::XPtr<std::unordered_multiset<std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multiset_max_load_factor_b_get(Rcpp::XPtr<std::unordered_multiset<bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
void unordered_multiset_max_load_factor_i_set(Rcpp::XPtr<std::unordered_multiset<int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multiset_max_load_factor_d_set(Rcpp::XPtr<std::unordered_multiset<double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multiset_max_load_factor_s_set(Rcpp::XPtr<std::unordered_multiset<std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multiset_max_load_factor_b_set(Rcpp::XPtr<std::unordered_multiset<bool> > x, const double l) {
  x->max_load_factor(l);
}

// unordered_map
// [[Rcpp::export]]
double unordered_map_max_load_factor_i_i_get(Rcpp::XPtr<std::unordered_map<int, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_i_d_get(Rcpp::XPtr<std::unordered_map<int, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_i_s_get(Rcpp::XPtr<std::unordered_map<int, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_i_b_get(Rcpp::XPtr<std::unordered_map<int, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_d_i_get(Rcpp::XPtr<std::unordered_map<double, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_d_d_get(Rcpp::XPtr<std::unordered_map<double, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_d_s_get(Rcpp::XPtr<std::unordered_map<double, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_d_b_get(Rcpp::XPtr<std::unordered_map<double, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_s_i_get(Rcpp::XPtr<std::unordered_map<std::string, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_s_d_get(Rcpp::XPtr<std::unordered_map<std::string, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_s_s_get(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_s_b_get(Rcpp::XPtr<std::unordered_map<std::string, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_b_i_get(Rcpp::XPtr<std::unordered_map<bool, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_b_d_get(Rcpp::XPtr<std::unordered_map<bool, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_b_s_get(Rcpp::XPtr<std::unordered_map<bool, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_map_max_load_factor_b_b_get(Rcpp::XPtr<std::unordered_map<bool, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_i_i_set(Rcpp::XPtr<std::unordered_map<int, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_i_d_set(Rcpp::XPtr<std::unordered_map<int, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_i_s_set(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_i_b_set(Rcpp::XPtr<std::unordered_map<int, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_d_i_set(Rcpp::XPtr<std::unordered_map<double, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_d_d_set(Rcpp::XPtr<std::unordered_map<double, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_d_s_set(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_d_b_set(Rcpp::XPtr<std::unordered_map<double, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_s_i_set(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_s_d_set(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_s_s_set(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_s_b_set(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_b_i_set(Rcpp::XPtr<std::unordered_map<bool, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_b_d_set(Rcpp::XPtr<std::unordered_map<bool, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_b_s_set(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_map_max_load_factor_b_b_set(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const double l) {
  x->max_load_factor(l);
}

// unordered_multimap
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_i_i_get(Rcpp::XPtr<std::unordered_multimap<int, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_i_d_get(Rcpp::XPtr<std::unordered_multimap<int, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_i_s_get(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_i_b_get(Rcpp::XPtr<std::unordered_multimap<int, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_d_i_get(Rcpp::XPtr<std::unordered_multimap<double, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_d_d_get(Rcpp::XPtr<std::unordered_multimap<double, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_d_s_get(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_d_b_get(Rcpp::XPtr<std::unordered_multimap<double, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_s_i_get(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_s_d_get(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_s_s_get(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_s_b_get(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_b_i_get(Rcpp::XPtr<std::unordered_multimap<bool, int> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_b_d_get(Rcpp::XPtr<std::unordered_multimap<bool, double> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_b_s_get(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
double unordered_multimap_max_load_factor_b_b_get(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x) {
  return x->max_load_factor();
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_i_i_set(Rcpp::XPtr<std::unordered_multimap<int, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_i_d_set(Rcpp::XPtr<std::unordered_multimap<int, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_i_s_set(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_i_b_set(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_d_i_set(Rcpp::XPtr<std::unordered_multimap<double, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_d_d_set(Rcpp::XPtr<std::unordered_multimap<double, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_d_s_set(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_d_b_set(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_s_i_set(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_s_d_set(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_s_s_set(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_s_b_set(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_b_i_set(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_b_d_set(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_b_s_set(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, const double l) {
  x->max_load_factor(l);
}
// [[Rcpp::export]]
void unordered_multimap_max_load_factor_b_b_set(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, const double l) {
  x->max_load_factor(l);
}
