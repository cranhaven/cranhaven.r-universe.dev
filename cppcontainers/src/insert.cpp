// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <vector>
#include <deque>
#include <list>
#include <string>
#include <cstddef>
#include <iterator>

// set
// [[Rcpp::export]]
void set_insert_i(Rcpp::XPtr<std::set<int> > x, Rcpp::IntegerVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void set_insert_d(Rcpp::XPtr<std::set<double> > x, Rcpp::NumericVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void set_insert_s(Rcpp::XPtr<std::set<std::string> > x, Rcpp::CharacterVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void set_insert_b(Rcpp::XPtr<std::set<bool> > x, Rcpp::LogicalVector& v) {
  x->insert(v.begin(), v.end());
}

// unordered_set
// [[Rcpp::export]]
void unordered_set_insert_i(Rcpp::XPtr<std::unordered_set<int> > x, Rcpp::IntegerVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void unordered_set_insert_d(Rcpp::XPtr<std::unordered_set<double> > x, Rcpp::NumericVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void unordered_set_insert_s(Rcpp::XPtr<std::unordered_set<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c (v.begin(), v.end());
  x->insert(c.begin(), c.end());
}
// [[Rcpp::export]]
void unordered_set_insert_b(Rcpp::XPtr<std::unordered_set<bool> > x, Rcpp::LogicalVector& v) {
  x->insert(v.begin(), v.end());
}

// multiset
// [[Rcpp::export]]
void multiset_insert_i(Rcpp::XPtr<std::multiset<int> > x, Rcpp::IntegerVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void multiset_insert_d(Rcpp::XPtr<std::multiset<double> > x, Rcpp::NumericVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void multiset_insert_s(Rcpp::XPtr<std::multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c (v.begin(), v.end());
  x->insert(c.begin(), c.end());
}
// [[Rcpp::export]]
void multiset_insert_b(Rcpp::XPtr<std::multiset<bool> > x, Rcpp::LogicalVector& v) {
  x->insert(v.begin(), v.end());
}

// unordered_multiset
// [[Rcpp::export]]
void unordered_multiset_insert_i(Rcpp::XPtr<std::unordered_multiset<int> > x, Rcpp::IntegerVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void unordered_multiset_insert_d(Rcpp::XPtr<std::unordered_multiset<double> > x, Rcpp::NumericVector& v) {
  x->insert(v.begin(), v.end());
}
// [[Rcpp::export]]
void unordered_multiset_insert_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c (v.begin(), v.end());
  x->insert(c.begin(), c.end());
}
// [[Rcpp::export]]
void unordered_multiset_insert_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, Rcpp::LogicalVector& v) {
  x->insert(v.begin(), v.end());
}

// map
template <typename KM, typename VM, typename KV, typename VV> // KM: keys (map), VM: values (map), KV: keys (vector), VV: values (vector)
void map_insert(Rcpp::XPtr<std::map<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert({k[i], v[i]});
  }
}
// [[Rcpp::export]]
void map_insert_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert(x, k, s);
}
// [[Rcpp::export]]
void map_insert_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert(x, k, s);
}
// [[Rcpp::export]]
void map_insert_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert(x, s, v);
}
// [[Rcpp::export]]
void map_insert_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert(x, s, v);
}
// [[Rcpp::export]]
void map_insert_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  map_insert(x, s, c);
}
// [[Rcpp::export]]
void map_insert_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  map_insert(x, s, v);
}
// [[Rcpp::export]]
void map_insert_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  map_insert(x, k, v);
}
// [[Rcpp::export]]
void map_insert_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_insert(x, k, s);
}
// [[Rcpp::export]]
void map_insert_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  map_insert(x, k, v);
}

// unordered_map
template <typename KM, typename VM, typename KV, typename VV> // KM: keys (map), VM: values (map), KV: keys (vector), VV: values (vector)
void unordered_map_insert(Rcpp::XPtr<std::unordered_map<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert({k[i], v[i]});
  }
}
// [[Rcpp::export]]
void unordered_map_insert_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert(x, s, c);
}
// [[Rcpp::export]]
void unordered_map_insert_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_map_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_map_insert_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  unordered_map_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_map_insert_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_map_insert_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  unordered_map_insert(x, k, v);
}

// multimap
template <typename KM, typename VM, typename KV, typename VV> // KM: keys (map), VM: values (map), KV: keys (vector), VV: values (vector)
void multimap_insert(Rcpp::XPtr<std::multimap<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert({k[i], v[i]});
  }
}
// [[Rcpp::export]]
void multimap_insert_i_i(Rcpp::XPtr<std::multimap<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_i_d(Rcpp::XPtr<std::multimap<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void multimap_insert_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_d_i(Rcpp::XPtr<std::multimap<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_d_d(Rcpp::XPtr<std::multimap<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void multimap_insert_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void multimap_insert_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void multimap_insert_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  multimap_insert(x, s, c);
}
// [[Rcpp::export]]
void multimap_insert_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void multimap_insert_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void multimap_insert_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void multimap_insert_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  multimap_insert(x, k, v);
}

// unordered_multimap
template <typename KM, typename VM, typename KV, typename VV> // KM: keys (map), VM: values (map), KV: keys (vector), VV: values (vector)
void unordered_multimap_insert(Rcpp::XPtr<std::unordered_multimap<KM, VM> > x, KV& k, VV& v) {
  const std::size_t i_size = k.size();
  for(std::size_t i = 0; i != i_size; ++i) {
    x->insert({k[i], v[i]});
  }
}
// [[Rcpp::export]]
void unordered_multimap_insert_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, Rcpp::IntegerVector& k, Rcpp::IntegerVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, Rcpp::IntegerVector& k, Rcpp::NumericVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, Rcpp::IntegerVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_multimap_insert_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, Rcpp::IntegerVector& k, Rcpp::LogicalVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, Rcpp::NumericVector& k, Rcpp::IntegerVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, Rcpp::NumericVector& k, Rcpp::NumericVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, Rcpp::NumericVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_multimap_insert_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, Rcpp::NumericVector& k, Rcpp::LogicalVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, Rcpp::CharacterVector& k, Rcpp::IntegerVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, Rcpp::CharacterVector& k, Rcpp::NumericVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, Rcpp::CharacterVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_insert(x, s, c);
}
// [[Rcpp::export]]
void unordered_multimap_insert_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, Rcpp::CharacterVector& k, Rcpp::LogicalVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(k);
  unordered_multimap_insert(x, s, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, Rcpp::LogicalVector& k, Rcpp::IntegerVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, Rcpp::LogicalVector& k, Rcpp::NumericVector& v) {
  unordered_multimap_insert(x, k, v);
}
// [[Rcpp::export]]
void unordered_multimap_insert_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, Rcpp::LogicalVector& k, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_insert(x, k, s);
}
// [[Rcpp::export]]
void unordered_multimap_insert_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, Rcpp::LogicalVector& k, Rcpp::LogicalVector& v) {
  unordered_multimap_insert(x, k, v);
}

// vector
// [[Rcpp::export]]
void vector_insert_i(Rcpp::XPtr<std::vector<int> > x, Rcpp::IntegerVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_insert_d(Rcpp::XPtr<std::vector<double> > x, Rcpp::NumericVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_insert_s(Rcpp::XPtr<std::vector<std::string> > x, Rcpp::CharacterVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void vector_insert_b(Rcpp::XPtr<std::vector<bool> > x, Rcpp::LogicalVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}

// deque
// [[Rcpp::export]]
void deque_insert_i(Rcpp::XPtr<std::deque<int> > x, Rcpp::IntegerVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_insert_d(Rcpp::XPtr<std::deque<double> > x, Rcpp::NumericVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_insert_s(Rcpp::XPtr<std::deque<std::string> > x, Rcpp::CharacterVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}
// [[Rcpp::export]]
void deque_insert_b(Rcpp::XPtr<std::deque<bool> > x, Rcpp::LogicalVector& v, const std::size_t position) {
  x->insert(x->begin() + position, v.begin(), v.end());
}

// list
// [[Rcpp::export]]
void list_insert_i(Rcpp::XPtr<std::list<int> > x, Rcpp::IntegerVector& v, const std::size_t position) {
  x->insert(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void list_insert_d(Rcpp::XPtr<std::list<double> > x, Rcpp::NumericVector& v, const std::size_t position) {
  x->insert(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void list_insert_s(Rcpp::XPtr<std::list<std::string> > x, Rcpp::CharacterVector& v, const std::size_t position) {
  x->insert(std::next(x->begin(), position), v.begin(), v.end());
}
// [[Rcpp::export]]
void list_insert_b(Rcpp::XPtr<std::list<bool> > x, Rcpp::LogicalVector& v, const std::size_t position) {
  x->insert(std::next(x->begin(), position), v.begin(), v.end());
}
