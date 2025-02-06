// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <forward_list>
#include <list>
#include <string>

// template <typename T>
// void merge(Rcpp::XPtr<T> x, Rcpp::XPtr<T> y) {
//   x->merge(*y);
// }

// set
// [[Rcpp::export]]
void set_merge_i(Rcpp::XPtr<std::set<int> > x, Rcpp::XPtr<std::set<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void set_merge_d(Rcpp::XPtr<std::set<double> > x, Rcpp::XPtr<std::set<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void set_merge_s(Rcpp::XPtr<std::set<std::string> > x, Rcpp::XPtr<std::set<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void set_merge_b(Rcpp::XPtr<std::set<bool> > x, Rcpp::XPtr<std::set<bool> > y) {
  x->merge(*y);
}

// unordered_set
// [[Rcpp::export]]
void unordered_set_merge_i(Rcpp::XPtr<std::unordered_set<int> > x, Rcpp::XPtr<std::unordered_set<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_set_merge_d(Rcpp::XPtr<std::unordered_set<double> > x, Rcpp::XPtr<std::unordered_set<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_set_merge_s(Rcpp::XPtr<std::unordered_set<std::string> > x, Rcpp::XPtr<std::unordered_set<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_set_merge_b(Rcpp::XPtr<std::unordered_set<bool> > x, Rcpp::XPtr<std::unordered_set<bool> > y) {
  x->merge(*y);
}

// multiset
// [[Rcpp::export]]
void multiset_merge_i(Rcpp::XPtr<std::multiset<int> > x, Rcpp::XPtr<std::multiset<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multiset_merge_d(Rcpp::XPtr<std::multiset<double> > x, Rcpp::XPtr<std::multiset<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multiset_merge_s(Rcpp::XPtr<std::multiset<std::string> > x, Rcpp::XPtr<std::multiset<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multiset_merge_b(Rcpp::XPtr<std::multiset<bool> > x, Rcpp::XPtr<std::multiset<bool> > y) {
  x->merge(*y);
}

// unordered_multiset
// [[Rcpp::export]]
void unordered_multiset_merge_i(Rcpp::XPtr<std::unordered_multiset<int> > x, Rcpp::XPtr<std::unordered_multiset<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multiset_merge_d(Rcpp::XPtr<std::unordered_multiset<double> > x, Rcpp::XPtr<std::unordered_multiset<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multiset_merge_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, Rcpp::XPtr<std::unordered_multiset<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multiset_merge_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, Rcpp::XPtr<std::unordered_multiset<bool> > y) {
  x->merge(*y);
}

// map
// [[Rcpp::export]]
void map_merge_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::XPtr<std::map<int, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::XPtr<std::map<int, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::XPtr<std::map<int, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::XPtr<std::map<int, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::XPtr<std::map<double, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::XPtr<std::map<double, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::XPtr<std::map<double, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::XPtr<std::map<double, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::XPtr<std::map<std::string, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::XPtr<std::map<std::string, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::XPtr<std::map<std::string, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::XPtr<std::map<std::string, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::XPtr<std::map<bool, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::XPtr<std::map<bool, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::XPtr<std::map<bool, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void map_merge_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::XPtr<std::map<bool, bool> > y) {
  x->merge(*y);
}

// unordered_map
// [[Rcpp::export]]
void unordered_map_merge_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::XPtr<std::unordered_map<int, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::XPtr<std::unordered_map<int, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::XPtr<std::unordered_map<int, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::XPtr<std::unordered_map<int, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::XPtr<std::unordered_map<double, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::XPtr<std::unordered_map<double, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::XPtr<std::unordered_map<double, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::XPtr<std::unordered_map<double, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::XPtr<std::unordered_map<std::string, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::XPtr<std::unordered_map<std::string, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::XPtr<std::unordered_map<std::string, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::XPtr<std::unordered_map<std::string, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::XPtr<std::unordered_map<bool, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::XPtr<std::unordered_map<bool, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::XPtr<std::unordered_map<bool, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_map_merge_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::XPtr<std::unordered_map<bool, bool> > y) {
  x->merge(*y);
}

// multimap
// [[Rcpp::export]]
void multimap_merge_i_i(Rcpp::XPtr<std::multimap<int, int> > x, Rcpp::XPtr<std::multimap<int, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_i_d(Rcpp::XPtr<std::multimap<int, double> > x, Rcpp::XPtr<std::multimap<int, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, Rcpp::XPtr<std::multimap<int, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, Rcpp::XPtr<std::multimap<int, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_d_i(Rcpp::XPtr<std::multimap<double, int> > x, Rcpp::XPtr<std::multimap<double, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_d_d(Rcpp::XPtr<std::multimap<double, double> > x, Rcpp::XPtr<std::multimap<double, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, Rcpp::XPtr<std::multimap<double, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, Rcpp::XPtr<std::multimap<double, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, Rcpp::XPtr<std::multimap<std::string, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, Rcpp::XPtr<std::multimap<std::string, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, Rcpp::XPtr<std::multimap<std::string, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, Rcpp::XPtr<std::multimap<std::string, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, Rcpp::XPtr<std::multimap<bool, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, Rcpp::XPtr<std::multimap<bool, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, Rcpp::XPtr<std::multimap<bool, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void multimap_merge_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, Rcpp::XPtr<std::multimap<bool, bool> > y) {
  x->merge(*y);
}

// unordered_multimap
// [[Rcpp::export]]
void unordered_multimap_merge_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, Rcpp::XPtr<std::unordered_multimap<int, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, Rcpp::XPtr<std::unordered_multimap<int, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, Rcpp::XPtr<std::unordered_multimap<int, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, Rcpp::XPtr<std::unordered_multimap<int, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, Rcpp::XPtr<std::unordered_multimap<double, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, Rcpp::XPtr<std::unordered_multimap<double, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x,
  Rcpp::XPtr<std::unordered_multimap<double, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, Rcpp::XPtr<std::unordered_multimap<double, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, Rcpp::XPtr<std::unordered_multimap<std::string, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x,
  Rcpp::XPtr<std::unordered_multimap<std::string, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x,
  Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, Rcpp::XPtr<std::unordered_multimap<std::string, bool> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, Rcpp::XPtr<std::unordered_multimap<bool, int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, Rcpp::XPtr<std::unordered_multimap<bool, double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, Rcpp::XPtr<std::unordered_multimap<bool, std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void unordered_multimap_merge_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, Rcpp::XPtr<std::unordered_multimap<bool, bool> > y) {
  x->merge(*y);
}

// forward_list
// [[Rcpp::export]]
void forward_list_merge_i(Rcpp::XPtr<std::forward_list<int> > x, Rcpp::XPtr<std::forward_list<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void forward_list_merge_d(Rcpp::XPtr<std::forward_list<double> > x, Rcpp::XPtr<std::forward_list<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void forward_list_merge_s(Rcpp::XPtr<std::forward_list<std::string> > x, Rcpp::XPtr<std::forward_list<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void forward_list_merge_b(Rcpp::XPtr<std::forward_list<bool> > x, Rcpp::XPtr<std::forward_list<bool> > y) {
  x->merge(*y);
}

// list
// [[Rcpp::export]]
void list_merge_i(Rcpp::XPtr<std::list<int> > x, Rcpp::XPtr<std::list<int> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void list_merge_d(Rcpp::XPtr<std::list<double> > x, Rcpp::XPtr<std::list<double> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void list_merge_s(Rcpp::XPtr<std::list<std::string> > x, Rcpp::XPtr<std::list<std::string> > y) {
  x->merge(*y);
}
// [[Rcpp::export]]
void list_merge_b(Rcpp::XPtr<std::list<bool> > x, Rcpp::XPtr<std::list<bool> > y) {
  x->merge(*y);
}
