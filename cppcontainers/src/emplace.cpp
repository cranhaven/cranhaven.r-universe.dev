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
#include <list>
#include <string>
#include <iterator>

// set
// [[Rcpp::export]]
void set_emplace_i(Rcpp::XPtr<std::set<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void set_emplace_d(Rcpp::XPtr<std::set<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void set_emplace_s(Rcpp::XPtr<std::set<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void set_emplace_b(Rcpp::XPtr<std::set<bool> > x, const bool v) {
  x->emplace(v);
}

// unordered_set
// [[Rcpp::export]]
void unordered_set_emplace_i(Rcpp::XPtr<std::unordered_set<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_set_emplace_d(Rcpp::XPtr<std::unordered_set<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_set_emplace_s(Rcpp::XPtr<std::unordered_set<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_set_emplace_b(Rcpp::XPtr<std::unordered_set<bool> > x, const bool v) {
  x->emplace(v);
}

// multiset
// [[Rcpp::export]]
void multiset_emplace_i(Rcpp::XPtr<std::multiset<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void multiset_emplace_d(Rcpp::XPtr<std::multiset<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void multiset_emplace_s(Rcpp::XPtr<std::multiset<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void multiset_emplace_b(Rcpp::XPtr<std::multiset<bool> > x, const bool v) {
  x->emplace(v);
}

// unordered_multiset
// [[Rcpp::export]]
void unordered_multiset_emplace_i(Rcpp::XPtr<std::unordered_multiset<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_multiset_emplace_d(Rcpp::XPtr<std::unordered_multiset<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_multiset_emplace_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void unordered_multiset_emplace_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, const bool v) {
  x->emplace(v);
}

// map
// [[Rcpp::export]]
void map_emplace_i_i(Rcpp::XPtr<std::map<int, int> > x, const int k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_i_d(Rcpp::XPtr<std::map<int, double> > x, const int k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const int k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_i_b(Rcpp::XPtr<std::map<int, bool> > x, const int k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_d_i(Rcpp::XPtr<std::map<double, int> > x, const double k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_d_d(Rcpp::XPtr<std::map<double, double> > x, const double k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const double k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_d_b(Rcpp::XPtr<std::map<double, bool> > x, const double k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const std::string k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const std::string k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const std::string k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const std::string k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void map_emplace_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool k, const bool v) {
  x->emplace(k, v);
}

// unordered_map
// [[Rcpp::export]]
void unordered_map_emplace_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, const int k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, const int k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, const int k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, const int k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, const double k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, const double k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, const double k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, const double k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, const std::string k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, const std::string k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, const std::string k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, const std::string k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, const bool k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, const bool k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, const bool k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_map_emplace_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, const bool k, const bool v) {
  x->emplace(k, v);
}

// multimap
// [[Rcpp::export]]
void multimap_emplace_i_i(Rcpp::XPtr<std::multimap<int, int> > x, const int k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_i_d(Rcpp::XPtr<std::multimap<int, double> > x, const int k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, const int k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, const int k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_d_i(Rcpp::XPtr<std::multimap<double, int> > x, const double k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_d_d(Rcpp::XPtr<std::multimap<double, double> > x, const double k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, const double k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, const double k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, const std::string k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, const std::string k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, const std::string k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, const std::string k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, const bool k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, const bool k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, const bool k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void multimap_emplace_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, const bool k, const bool v) {
  x->emplace(k, v);
}

// unordered_multimap
// [[Rcpp::export]]
void unordered_multimap_emplace_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, const int k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, const int k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, const int k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, const int k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, const double k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, const double k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, const double k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, const double k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, const std::string k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, const std::string k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, const std::string k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, const std::string k, const bool v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, const bool k, const int v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, const bool k, const double v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, const bool k, const std::string v) {
  x->emplace(k, v);
}
// [[Rcpp::export]]
void unordered_multimap_emplace_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, const bool k, const bool v) {
  x->emplace(k, v);
}

// stack
// [[Rcpp::export]]
void stack_emplace_i(Rcpp::XPtr<std::stack<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void stack_emplace_d(Rcpp::XPtr<std::stack<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void stack_emplace_s(Rcpp::XPtr<std::stack<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void stack_emplace_b(Rcpp::XPtr<std::stack<bool> > x, const bool v) {
  x->emplace(v);
}

// queue
// [[Rcpp::export]]
void queue_emplace_i(Rcpp::XPtr<std::queue<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void queue_emplace_d(Rcpp::XPtr<std::queue<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void queue_emplace_s(Rcpp::XPtr<std::queue<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void queue_emplace_b(Rcpp::XPtr<std::queue<bool> > x, const bool v) {
  x->emplace(v);
}

// priority_queue
// [[Rcpp::export]]
void priority_queue_emplace_i_d(Rcpp::XPtr<std::priority_queue<int> > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_d_d(Rcpp::XPtr<std::priority_queue<double> > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_s_d(Rcpp::XPtr<std::priority_queue<std::string> > x, const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_b_d(Rcpp::XPtr<std::priority_queue<bool> > x, const bool v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > x, const int v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > x, const double v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > x,
  const std::string v) {
  x->emplace(v);
}
// [[Rcpp::export]]
void priority_queue_emplace_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > x, const bool v) {
  x->emplace(v);
}

// vector
// [[Rcpp::export]]
void vector_emplace_i(Rcpp::XPtr<std::vector<int> > x, const int v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void vector_emplace_d(Rcpp::XPtr<std::vector<double> > x, const double v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void vector_emplace_s(Rcpp::XPtr<std::vector<std::string> > x, const std::string v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void vector_emplace_b(Rcpp::XPtr<std::vector<bool> > x, const bool v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}

// deque
// [[Rcpp::export]]
void deque_emplace_i(Rcpp::XPtr<std::deque<int> > x, const int v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void deque_emplace_d(Rcpp::XPtr<std::deque<double> > x, const double v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void deque_emplace_s(Rcpp::XPtr<std::deque<std::string> > x, const std::string v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}
// [[Rcpp::export]]
void deque_emplace_b(Rcpp::XPtr<std::deque<bool> > x, const bool v, const std::size_t position) {
  x->emplace(x->begin() + position, v);
}

// list
// [[Rcpp::export]]
void list_emplace_i(Rcpp::XPtr<std::list<int> > x, const int v, const std::size_t position) {
  x->emplace(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void list_emplace_d(Rcpp::XPtr<std::list<double> > x, const double v, const std::size_t position) {
  x->emplace(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void list_emplace_s(Rcpp::XPtr<std::list<std::string> > x, const std::string v, const std::size_t position) {
  x->emplace(std::next(x->begin(), position), v);
}
// [[Rcpp::export]]
void list_emplace_b(Rcpp::XPtr<std::list<bool> > x, const bool v, const std::size_t position) {
  x->emplace(std::next(x->begin(), position), v);
}
