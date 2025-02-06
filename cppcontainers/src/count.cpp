// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <string>
#include <vector>
#include <cstddef>

// set
template <typename C, typename R> // C: set data type, R: R vector
Rcpp::IntegerVector set_count(Rcpp::XPtr<std::set<C> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector set_count_i(Rcpp::XPtr<std::set<int> > x, Rcpp::IntegerVector& v) {
  return set_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector set_count_d(Rcpp::XPtr<std::set<double> > x, Rcpp::NumericVector& v) {
  return set_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector set_count_s(Rcpp::XPtr<std::set<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return set_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector set_count_b(Rcpp::XPtr<std::set<bool> > x, Rcpp::LogicalVector& v) {
  return set_count(x, v);
}

// unordered_set
template <typename C, typename R> // C: unordered set data type, R: R vector
Rcpp::IntegerVector unordered_set_count(Rcpp::XPtr<std::unordered_set<C> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_set_count_i(Rcpp::XPtr<std::unordered_set<int> > x, Rcpp::IntegerVector& v) {
  return unordered_set_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_set_count_d(Rcpp::XPtr<std::unordered_set<double> > x, Rcpp::NumericVector& v) {
  return unordered_set_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_set_count_s(Rcpp::XPtr<std::unordered_set<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_set_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_set_count_b(Rcpp::XPtr<std::unordered_set<bool> > x, Rcpp::LogicalVector& v) {
  return unordered_set_count(x, v);
}

// multiset
template <typename C, typename R> // C: multiset data type, R: R vector
Rcpp::IntegerVector multiset_count(Rcpp::XPtr<std::multiset<C> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector multiset_count_i(Rcpp::XPtr<std::multiset<int> > x, Rcpp::IntegerVector& v) {
  return multiset_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multiset_count_d(Rcpp::XPtr<std::multiset<double> > x, Rcpp::NumericVector& v) {
  return multiset_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multiset_count_s(Rcpp::XPtr<std::multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return multiset_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multiset_count_b(Rcpp::XPtr<std::multiset<bool> > x, Rcpp::LogicalVector& v) {
  return multiset_count(x, v);
}

// unordered_multiset
template <typename C, typename R> // C: unordered multiset data type, R: R vector
Rcpp::IntegerVector unordered_multiset_count(Rcpp::XPtr<std::unordered_multiset<C> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multiset_count_i(Rcpp::XPtr<std::unordered_multiset<int> > x, Rcpp::IntegerVector& v) {
  return unordered_multiset_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multiset_count_d(Rcpp::XPtr<std::unordered_multiset<double> > x, Rcpp::NumericVector& v) {
  return unordered_multiset_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multiset_count_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_multiset_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multiset_count_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, Rcpp::LogicalVector& v) {
  return unordered_multiset_count(x, v);
}

// map
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
Rcpp::IntegerVector map_count(Rcpp::XPtr<std::map<K, V> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::IntegerVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::IntegerVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::IntegerVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::IntegerVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::NumericVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::NumericVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::NumericVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::NumericVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::LogicalVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::LogicalVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::LogicalVector& v) {
  return map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector map_count_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::LogicalVector& v) {
  return map_count(x, v);
}

// unordered_map
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
Rcpp::IntegerVector unordered_map_count(Rcpp::XPtr<std::unordered_map<K, V> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::IntegerVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::IntegerVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::IntegerVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::IntegerVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::NumericVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::NumericVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::NumericVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::NumericVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_map_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::LogicalVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::LogicalVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::LogicalVector& v) {
  return unordered_map_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_map_count_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::LogicalVector& v) {
  return unordered_map_count(x, v);
}

// multimap
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
Rcpp::IntegerVector multimap_count(Rcpp::XPtr<std::multimap<K, V> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_i_i(Rcpp::XPtr<std::multimap<int, int> > x, Rcpp::IntegerVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_i_d(Rcpp::XPtr<std::multimap<int, double> > x, Rcpp::IntegerVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, Rcpp::IntegerVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, Rcpp::IntegerVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_d_i(Rcpp::XPtr<std::multimap<double, int> > x, Rcpp::NumericVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_d_d(Rcpp::XPtr<std::multimap<double, double> > x, Rcpp::NumericVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, Rcpp::NumericVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, Rcpp::NumericVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, Rcpp::LogicalVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, Rcpp::LogicalVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, Rcpp::LogicalVector& v) {
  return multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector multimap_count_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, Rcpp::LogicalVector& v) {
  return multimap_count(x, v);
}

// unordered_multimap
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
Rcpp::IntegerVector unordered_multimap_count(Rcpp::XPtr<std::unordered_multimap<K, V> > x, R& v) {
  const std::size_t v_size = v.size();
  Rcpp::IntegerVector c (v_size);
  for(std::size_t i = 0; i != v_size; ++i) {
    c[i] = x->count(v[i]);
  }
  return c;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, Rcpp::IntegerVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, Rcpp::IntegerVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, Rcpp::IntegerVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, Rcpp::IntegerVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, Rcpp::NumericVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, Rcpp::NumericVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, Rcpp::NumericVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, Rcpp::NumericVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  return unordered_multimap_count(x, s);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, Rcpp::LogicalVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, Rcpp::LogicalVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, Rcpp::LogicalVector& v) {
  return unordered_multimap_count(x, v);
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multimap_count_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, Rcpp::LogicalVector& v) {
  return unordered_multimap_count(x, v);
}
