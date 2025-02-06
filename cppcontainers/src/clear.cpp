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
// std::size_t clear(Rcpp::XPtr<T> x) {
//   x->clear();
// }

// set
// [[Rcpp::export]]
void set_clear_i(Rcpp::XPtr<std::set<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void set_clear_d(Rcpp::XPtr<std::set<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void set_clear_s(Rcpp::XPtr<std::set<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void set_clear_b(Rcpp::XPtr<std::set<bool> > x) {
  x->clear();
}

// unordered_set
// [[Rcpp::export]]
void unordered_set_clear_i(Rcpp::XPtr<std::unordered_set<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_set_clear_d(Rcpp::XPtr<std::unordered_set<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_set_clear_s(Rcpp::XPtr<std::unordered_set<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_set_clear_b(Rcpp::XPtr<std::unordered_set<bool> > x) {
  x->clear();
}

// multiset
// [[Rcpp::export]]
void multiset_clear_i(Rcpp::XPtr<std::multiset<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multiset_clear_d(Rcpp::XPtr<std::multiset<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multiset_clear_s(Rcpp::XPtr<std::multiset<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multiset_clear_b(Rcpp::XPtr<std::multiset<bool> > x) {
  x->clear();
}

// unordered_multiset
// [[Rcpp::export]]
void unordered_multiset_clear_i(Rcpp::XPtr<std::unordered_multiset<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multiset_clear_d(Rcpp::XPtr<std::unordered_multiset<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multiset_clear_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multiset_clear_b(Rcpp::XPtr<std::unordered_multiset<bool> > x) {
  x->clear();
}

// map
// [[Rcpp::export]]
void map_clear_i_i(Rcpp::XPtr<std::map<int, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_i_d(Rcpp::XPtr<std::map<int, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_i_s(Rcpp::XPtr<std::map<int, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_i_b(Rcpp::XPtr<std::map<int, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_d_i(Rcpp::XPtr<std::map<double, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_d_d(Rcpp::XPtr<std::map<double, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_d_s(Rcpp::XPtr<std::map<double, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_d_b(Rcpp::XPtr<std::map<double, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_s_i(Rcpp::XPtr<std::map<std::string, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_s_d(Rcpp::XPtr<std::map<std::string, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_s_b(Rcpp::XPtr<std::map<std::string, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_b_i(Rcpp::XPtr<std::map<bool, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_b_d(Rcpp::XPtr<std::map<bool, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_b_s(Rcpp::XPtr<std::map<bool, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void map_clear_b_b(Rcpp::XPtr<std::map<bool, bool> > x) {
  x->clear();
}

// unordered_map
// [[Rcpp::export]]
void unordered_map_clear_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_map_clear_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x) {
  x->clear();
}

// multimap
// [[Rcpp::export]]
void multimap_clear_i_i(Rcpp::XPtr<std::multimap<int, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_i_d(Rcpp::XPtr<std::multimap<int, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_i_b(Rcpp::XPtr<std::multimap<int, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_d_i(Rcpp::XPtr<std::multimap<double, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_d_d(Rcpp::XPtr<std::multimap<double, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_d_b(Rcpp::XPtr<std::multimap<double, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_b_i(Rcpp::XPtr<std::multimap<bool, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_b_d(Rcpp::XPtr<std::multimap<bool, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void multimap_clear_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x) {
  x->clear();
}

// unordered_multimap
// [[Rcpp::export]]
void unordered_multimap_clear_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void unordered_multimap_clear_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x) {
  x->clear();
}

// vector
// [[Rcpp::export]]
void vector_clear_i(Rcpp::XPtr<std::vector<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void vector_clear_d(Rcpp::XPtr<std::vector<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void vector_clear_s(Rcpp::XPtr<std::vector<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void vector_clear_b(Rcpp::XPtr<std::vector<bool> > x) {
  x->clear();
}

// deque
// [[Rcpp::export]]
void deque_clear_i(Rcpp::XPtr<std::deque<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void deque_clear_d(Rcpp::XPtr<std::deque<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void deque_clear_s(Rcpp::XPtr<std::deque<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void deque_clear_b(Rcpp::XPtr<std::deque<bool> > x) {
  x->clear();
}

// forward_list
// [[Rcpp::export]]
void forward_list_clear_i(Rcpp::XPtr<std::forward_list<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void forward_list_clear_d(Rcpp::XPtr<std::forward_list<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void forward_list_clear_s(Rcpp::XPtr<std::forward_list<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void forward_list_clear_b(Rcpp::XPtr<std::forward_list<bool> > x) {
  x->clear();
}

// list
// [[Rcpp::export]]
void list_clear_i(Rcpp::XPtr<std::list<int> > x) {
  x->clear();
}
// [[Rcpp::export]]
void list_clear_d(Rcpp::XPtr<std::list<double> > x) {
  x->clear();
}
// [[Rcpp::export]]
void list_clear_s(Rcpp::XPtr<std::list<std::string> > x) {
  x->clear();
}
// [[Rcpp::export]]
void list_clear_b(Rcpp::XPtr<std::list<bool> > x) {
  x->clear();
}
