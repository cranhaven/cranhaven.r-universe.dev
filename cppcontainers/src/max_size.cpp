// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <vector>
#include <deque>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// template <typename T>
// std::size_t max_size(Rcpp::XPtr<T> x) {
//   return x->max_size();
// }

// set
// [[Rcpp::export]]
std::size_t set_max_size_i(Rcpp::XPtr<std::set<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t set_max_size_d(Rcpp::XPtr<std::set<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t set_max_size_s(Rcpp::XPtr<std::set<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t set_max_size_b(Rcpp::XPtr<std::set<bool> > x) {
  return x->max_size();
}

// unordered_set
// [[Rcpp::export]]
std::size_t unordered_set_max_size_i(Rcpp::XPtr<std::unordered_set<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_set_max_size_d(Rcpp::XPtr<std::unordered_set<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_set_max_size_s(Rcpp::XPtr<std::unordered_set<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_set_max_size_b(Rcpp::XPtr<std::unordered_set<bool> > x) {
  return x->max_size();
}

// multiset
// [[Rcpp::export]]
std::size_t multiset_max_size_i(Rcpp::XPtr<std::multiset<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multiset_max_size_d(Rcpp::XPtr<std::multiset<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multiset_max_size_s(Rcpp::XPtr<std::multiset<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multiset_max_size_b(Rcpp::XPtr<std::multiset<bool> > x) {
  return x->max_size();
}

// unordered_multiset
// [[Rcpp::export]]
std::size_t unordered_multiset_max_size_i(Rcpp::XPtr<std::unordered_multiset<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multiset_max_size_d(Rcpp::XPtr<std::unordered_multiset<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multiset_max_size_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multiset_max_size_b(Rcpp::XPtr<std::unordered_multiset<bool> > x) {
  return x->max_size();
}

// map
// [[Rcpp::export]]
std::size_t map_max_size_i_i(Rcpp::XPtr<std::map<int, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_i_d(Rcpp::XPtr<std::map<int, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_i_s(Rcpp::XPtr<std::map<int, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_i_b(Rcpp::XPtr<std::map<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_d_i(Rcpp::XPtr<std::map<double, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_d_d(Rcpp::XPtr<std::map<double, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_d_s(Rcpp::XPtr<std::map<double, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_d_b(Rcpp::XPtr<std::map<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_s_i(Rcpp::XPtr<std::map<std::string, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_s_d(Rcpp::XPtr<std::map<std::string, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_s_b(Rcpp::XPtr<std::map<std::string, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_b_i(Rcpp::XPtr<std::map<bool, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_b_d(Rcpp::XPtr<std::map<bool, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_b_s(Rcpp::XPtr<std::map<bool, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t map_max_size_b_b(Rcpp::XPtr<std::map<bool, bool> > x) {
  return x->max_size();
}

// unordered_map
// [[Rcpp::export]]
std::size_t unordered_map_max_size_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_i_b(Rcpp::XPtr<std::unordered_map<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_map_max_size_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x) {
  return x->max_size();
}

// multimap
// [[Rcpp::export]]
std::size_t multimap_max_size_i_i(Rcpp::XPtr<std::multimap<int, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_i_d(Rcpp::XPtr<std::multimap<int, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_i_b(Rcpp::XPtr<std::multimap<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_d_i(Rcpp::XPtr<std::multimap<double, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_d_d(Rcpp::XPtr<std::multimap<double, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_d_b(Rcpp::XPtr<std::multimap<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_b_i(Rcpp::XPtr<std::multimap<bool, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_b_d(Rcpp::XPtr<std::multimap<bool, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t multimap_max_size_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x) {
  return x->max_size();
}

// unordered_multimap
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_i_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t unordered_multimap_max_size_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x) {
  return x->max_size();
}

// vector
// [[Rcpp::export]]
std::size_t vector_max_size_i(Rcpp::XPtr<std::vector<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t vector_max_size_d(Rcpp::XPtr<std::vector<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t vector_max_size_s(Rcpp::XPtr<std::vector<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t vector_max_size_b(Rcpp::XPtr<std::vector<bool> > x) {
  return x->max_size();
}

// deque
// [[Rcpp::export]]
std::size_t deque_max_size_i(Rcpp::XPtr<std::deque<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t deque_max_size_d(Rcpp::XPtr<std::deque<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t deque_max_size_s(Rcpp::XPtr<std::deque<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t deque_max_size_b(Rcpp::XPtr<std::deque<bool> > x) {
  return x->max_size();
}

// forward_list
// [[Rcpp::export]]
std::size_t forward_list_max_size_i(Rcpp::XPtr<std::forward_list<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t forward_list_max_size_d(Rcpp::XPtr<std::forward_list<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t forward_list_max_size_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t forward_list_max_size_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  return x->max_size();
}

// list
// [[Rcpp::export]]
std::size_t list_max_size_i(Rcpp::XPtr<std::list<int> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t list_max_size_d(Rcpp::XPtr<std::list<double> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t list_max_size_s(Rcpp::XPtr<std::list<std::string> > x) {
  return x->max_size();
}
// [[Rcpp::export]]
std::size_t list_max_size_b(Rcpp::XPtr<std::list<bool> > x) {
  return x->max_size();
}
