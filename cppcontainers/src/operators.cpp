// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <stack>
#include <queue>
#include <vector>
#include <deque>
#include <forward_list>
#include <list>
#include <string>
#include <cstddef>

// set
// [[Rcpp::export]]
bool set_equal_i(Rcpp::XPtr<std::set<int> > x, Rcpp::XPtr<std::set<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool set_equal_d(Rcpp::XPtr<std::set<double> > x, Rcpp::XPtr<std::set<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool set_equal_s(Rcpp::XPtr<std::set<std::string> > x, Rcpp::XPtr<std::set<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool set_equal_b(Rcpp::XPtr<std::set<bool> > x, Rcpp::XPtr<std::set<bool> > y) {
  return(*x == *y);
}

// unordered_set
// [[Rcpp::export]]
bool unordered_set_equal_i(Rcpp::XPtr<std::unordered_set<int> > x, Rcpp::XPtr<std::unordered_set<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_set_equal_d(Rcpp::XPtr<std::unordered_set<double> > x, Rcpp::XPtr<std::unordered_set<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_set_equal_s(Rcpp::XPtr<std::unordered_set<std::string> > x, Rcpp::XPtr<std::unordered_set<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_set_equal_b(Rcpp::XPtr<std::unordered_set<bool> > x, Rcpp::XPtr<std::unordered_set<bool> > y) {
  return(*x == *y);
}

// multiset
// [[Rcpp::export]]
bool multiset_equal_i(Rcpp::XPtr<std::multiset<int> > x, Rcpp::XPtr<std::multiset<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multiset_equal_d(Rcpp::XPtr<std::multiset<double> > x, Rcpp::XPtr<std::multiset<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multiset_equal_s(Rcpp::XPtr<std::multiset<std::string> > x, Rcpp::XPtr<std::multiset<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multiset_equal_b(Rcpp::XPtr<std::multiset<bool> > x, Rcpp::XPtr<std::multiset<bool> > y) {
  return(*x == *y);
}

// unordered_multiset
// [[Rcpp::export]]
bool unordered_multiset_equal_i(Rcpp::XPtr<std::unordered_multiset<int> > x, Rcpp::XPtr<std::unordered_multiset<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multiset_equal_d(Rcpp::XPtr<std::unordered_multiset<double> > x, Rcpp::XPtr<std::unordered_multiset<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multiset_equal_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, Rcpp::XPtr<std::unordered_multiset<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multiset_equal_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, Rcpp::XPtr<std::unordered_multiset<bool> > y) {
  return(*x == *y);
}

// map
// [[Rcpp::export]]
bool map_equal_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::XPtr<std::map<int, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::XPtr<std::map<int, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::XPtr<std::map<int, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::XPtr<std::map<int, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::XPtr<std::map<double, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::XPtr<std::map<double, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::XPtr<std::map<double, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::XPtr<std::map<double, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::XPtr<std::map<std::string, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::XPtr<std::map<std::string, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::XPtr<std::map<std::string, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::XPtr<std::map<std::string, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::XPtr<std::map<bool, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::XPtr<std::map<bool, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::XPtr<std::map<bool, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool map_equal_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::XPtr<std::map<bool, bool> > y) {
  return(*x == *y);
}

// unordered_map
// [[Rcpp::export]]
bool unordered_map_equal_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::XPtr<std::unordered_map<int, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::XPtr<std::unordered_map<int, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::XPtr<std::unordered_map<int, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::XPtr<std::unordered_map<int, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::XPtr<std::unordered_map<double, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::XPtr<std::unordered_map<double, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::XPtr<std::unordered_map<double, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::XPtr<std::unordered_map<double, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::XPtr<std::unordered_map<std::string, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::XPtr<std::unordered_map<std::string, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::XPtr<std::unordered_map<std::string, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::XPtr<std::unordered_map<std::string, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::XPtr<std::unordered_map<bool, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::XPtr<std::unordered_map<bool, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::XPtr<std::unordered_map<bool, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_map_equal_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::XPtr<std::unordered_map<bool, bool> > y) {
  return(*x == *y);
}

// multimap
// [[Rcpp::export]]
bool multimap_equal_i_i(Rcpp::XPtr<std::multimap<int, int> > x, Rcpp::XPtr<std::multimap<int, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_i_d(Rcpp::XPtr<std::multimap<int, double> > x, Rcpp::XPtr<std::multimap<int, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, Rcpp::XPtr<std::multimap<int, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, Rcpp::XPtr<std::multimap<int, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_d_i(Rcpp::XPtr<std::multimap<double, int> > x, Rcpp::XPtr<std::multimap<double, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_d_d(Rcpp::XPtr<std::multimap<double, double> > x, Rcpp::XPtr<std::multimap<double, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, Rcpp::XPtr<std::multimap<double, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, Rcpp::XPtr<std::multimap<double, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, Rcpp::XPtr<std::multimap<std::string, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, Rcpp::XPtr<std::multimap<std::string, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, Rcpp::XPtr<std::multimap<std::string, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, Rcpp::XPtr<std::multimap<std::string, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, Rcpp::XPtr<std::multimap<bool, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, Rcpp::XPtr<std::multimap<bool, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, Rcpp::XPtr<std::multimap<bool, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool multimap_equal_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, Rcpp::XPtr<std::multimap<bool, bool> > y) {
  return(*x == *y);
}

// unordered_multimap
// [[Rcpp::export]]
bool unordered_multimap_equal_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, Rcpp::XPtr<std::unordered_multimap<int, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, Rcpp::XPtr<std::unordered_multimap<int, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, Rcpp::XPtr<std::unordered_multimap<int, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, Rcpp::XPtr<std::unordered_multimap<int, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, Rcpp::XPtr<std::unordered_multimap<double, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, Rcpp::XPtr<std::unordered_multimap<double, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x,
  Rcpp::XPtr<std::unordered_multimap<double, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, Rcpp::XPtr<std::unordered_multimap<double, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, Rcpp::XPtr<std::unordered_multimap<std::string, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x,
  Rcpp::XPtr<std::unordered_multimap<std::string, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x,
  Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, Rcpp::XPtr<std::unordered_multimap<std::string, bool> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, Rcpp::XPtr<std::unordered_multimap<bool, int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, Rcpp::XPtr<std::unordered_multimap<bool, double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, Rcpp::XPtr<std::unordered_multimap<bool, std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool unordered_multimap_equal_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, Rcpp::XPtr<std::unordered_multimap<bool, bool> > y) {
  return(*x == *y);
}

// stack
// [[Rcpp::export]]
bool stack_equal_i(Rcpp::XPtr<std::stack<int> > x, Rcpp::XPtr<std::stack<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool stack_equal_d(Rcpp::XPtr<std::stack<double> > x, Rcpp::XPtr<std::stack<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool stack_equal_s(Rcpp::XPtr<std::stack<std::string> > x, Rcpp::XPtr<std::stack<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool stack_equal_b(Rcpp::XPtr<std::stack<bool> > x, Rcpp::XPtr<std::stack<bool> > y) {
  return(*x == *y);
}

// queue
// [[Rcpp::export]]
bool queue_equal_i(Rcpp::XPtr<std::queue<int> > x, Rcpp::XPtr<std::queue<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool queue_equal_d(Rcpp::XPtr<std::queue<double> > x, Rcpp::XPtr<std::queue<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool queue_equal_s(Rcpp::XPtr<std::queue<std::string> > x, Rcpp::XPtr<std::queue<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool queue_equal_b(Rcpp::XPtr<std::queue<bool> > x, Rcpp::XPtr<std::queue<bool> > y) {
  return(*x == *y);
}

// vector
// [[Rcpp::export]]
bool vector_equal_i(Rcpp::XPtr<std::vector<int> > x, Rcpp::XPtr<std::vector<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool vector_equal_d(Rcpp::XPtr<std::vector<double> > x, Rcpp::XPtr<std::vector<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool vector_equal_s(Rcpp::XPtr<std::vector<std::string> > x, Rcpp::XPtr<std::vector<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool vector_equal_b(Rcpp::XPtr<std::vector<bool> > x, Rcpp::XPtr<std::vector<bool> > y) {
  return(*x == *y);
}

// deque
// [[Rcpp::export]]
bool deque_equal_i(Rcpp::XPtr<std::deque<int> > x, Rcpp::XPtr<std::deque<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool deque_equal_d(Rcpp::XPtr<std::deque<double> > x, Rcpp::XPtr<std::deque<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool deque_equal_s(Rcpp::XPtr<std::deque<std::string> > x, Rcpp::XPtr<std::deque<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool deque_equal_b(Rcpp::XPtr<std::deque<bool> > x, Rcpp::XPtr<std::deque<bool> > y) {
  return(*x == *y);
}

// forward_list
// [[Rcpp::export]]
bool forward_list_equal_i(Rcpp::XPtr<std::forward_list<int> > x, Rcpp::XPtr<std::forward_list<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool forward_list_equal_d(Rcpp::XPtr<std::forward_list<double> > x, Rcpp::XPtr<std::forward_list<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool forward_list_equal_s(Rcpp::XPtr<std::forward_list<std::string> > x, Rcpp::XPtr<std::forward_list<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool forward_list_equal_b(Rcpp::XPtr<std::forward_list<bool> > x, Rcpp::XPtr<std::forward_list<bool> > y) {
  return(*x == *y);
}

// list
// [[Rcpp::export]]
bool list_equal_i(Rcpp::XPtr<std::list<int> > x, Rcpp::XPtr<std::list<int> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool list_equal_d(Rcpp::XPtr<std::list<double> > x, Rcpp::XPtr<std::list<double> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool list_equal_s(Rcpp::XPtr<std::list<std::string> > x, Rcpp::XPtr<std::list<std::string> > y) {
  return(*x == *y);
}
// [[Rcpp::export]]
bool list_equal_b(Rcpp::XPtr<std::list<bool> > x, Rcpp::XPtr<std::list<bool> > y) {
  return(*x == *y);
}

// map
// [[Rcpp::export]]
int map_bracket_i_i(Rcpp::XPtr<std::map<int, int> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double map_bracket_i_d(Rcpp::XPtr<std::map<int, double> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string map_bracket_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool map_bracket_i_b(Rcpp::XPtr<std::map<int, bool> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int map_bracket_d_i(Rcpp::XPtr<std::map<double, int> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double map_bracket_d_d(Rcpp::XPtr<std::map<double, double> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string map_bracket_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool map_bracket_d_b(Rcpp::XPtr<std::map<double, bool> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int map_bracket_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double map_bracket_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string map_bracket_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool map_bracket_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int map_bracket_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double map_bracket_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string map_bracket_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool map_bracket_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool key) {
  return (*x)[key];
}

// unordered_map
// [[Rcpp::export]]
int unordered_map_bracket_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double unordered_map_bracket_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string unordered_map_bracket_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool unordered_map_bracket_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, const int key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int unordered_map_bracket_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double unordered_map_bracket_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string unordered_map_bracket_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool unordered_map_bracket_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, const double key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int unordered_map_bracket_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double unordered_map_bracket_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string unordered_map_bracket_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool unordered_map_bracket_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const std::string key) {
  return (*x)[key];
}
// [[Rcpp::export]]
int unordered_map_bracket_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
double unordered_map_bracket_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
std::string unordered_map_bracket_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const bool key) {
  return (*x)[key];
}
// [[Rcpp::export]]
bool unordered_map_bracket_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const bool key) {
  return (*x)[key];
}

// vector
// [[Rcpp::export]]
int vector_bracket_i(Rcpp::XPtr<std::vector<int> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
double vector_bracket_d(Rcpp::XPtr<std::vector<double> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
std::string vector_bracket_s(Rcpp::XPtr<std::vector<std::string> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
bool vector_bracket_b(Rcpp::XPtr<std::vector<bool> > x, const std::size_t index) {
  return (*x)[index];
}

// deque
// [[Rcpp::export]]
int deque_bracket_i(Rcpp::XPtr<std::deque<int> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
double deque_bracket_d(Rcpp::XPtr<std::deque<double> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
std::string deque_bracket_s(Rcpp::XPtr<std::deque<std::string> > x, const std::size_t index) {
  return (*x)[index];
}
// [[Rcpp::export]]
bool deque_bracket_b(Rcpp::XPtr<std::deque<bool> > x, const std::size_t index) {
  return (*x)[index];
}
