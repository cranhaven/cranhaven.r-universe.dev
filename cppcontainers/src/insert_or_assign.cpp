// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <string>
#include <vector>

// map
template <typename KM, typename VM, typename KV, typename VV> // K: key data type, V: value data type, R: R vector
void map_insert_or_assign(Rcpp::XPtr<std::map<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert_or_assign(k[i], v[i]);
  }
}
// [[Rcpp::export]]
void map_insert_or_assign_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void map_insert_or_assign_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void map_insert_or_assign_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  map_insert_or_assign(x, s, c);
}
// [[Rcpp::export]]
void map_insert_or_assign_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void map_insert_or_assign_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void map_insert_or_assign_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  map_insert_or_assign(x, k, v);
}

// unordered_map
template <typename KM, typename VM, typename KV, typename VV> // K: key data type, V: value data type, R: R vector
void unordered_map_insert_or_assign(Rcpp::XPtr<std::unordered_map<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert_or_assign(k[i], v[i]);
  }
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert_or_assign(x, s, c);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert_or_assign(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert_or_assign(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_or_assign_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert_or_assign(x, k, v);
}
