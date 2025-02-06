// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <cstddef>

// unordered_set
// [[Rcpp::export]]
void unordered_set_rehash_i(Rcpp::XPtr<std::unordered_set<int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_set_rehash_d(Rcpp::XPtr<std::unordered_set<double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_set_rehash_s(Rcpp::XPtr<std::unordered_set<std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_set_rehash_b(Rcpp::XPtr<std::unordered_set<bool> > x, const std::size_t n) {
  x->rehash(n);
}

// unordered_multiset
// [[Rcpp::export]]
void unordered_multiset_rehash_i(Rcpp::XPtr<std::unordered_multiset<int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multiset_rehash_d(Rcpp::XPtr<std::unordered_multiset<double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multiset_rehash_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multiset_rehash_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, const std::size_t n) {
  x->rehash(n);
}

// unordered_map
// [[Rcpp::export]]
void unordered_map_rehash_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_map_rehash_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const std::size_t n) {
  x->rehash(n);
}

// unordered_multimap
// [[Rcpp::export]]
void unordered_multimap_rehash_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, const std::size_t n) {
  x->rehash(n);
}
// [[Rcpp::export]]
void unordered_multimap_rehash_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, const std::size_t n) {
  x->rehash(n);
}
