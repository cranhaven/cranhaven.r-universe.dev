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
template <typename C, typename R> // C: set data type, R: R vector
void set_erase(Rcpp::XPtr<std::set<C> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void set_erase_i(Rcpp::XPtr<std::set<int> > x, Rcpp::IntegerVector& v) {
  set_erase(x, v);
}
// [[Rcpp::export]]
void set_erase_d(Rcpp::XPtr<std::set<double> > x, Rcpp::NumericVector& v) {
  set_erase(x, v);
}
// [[Rcpp::export]]
void set_erase_s(Rcpp::XPtr<std::set<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  set_erase(x, s);
}
// [[Rcpp::export]]
void set_erase_b(Rcpp::XPtr<std::set<bool> > x, Rcpp::LogicalVector& v) {
  set_erase(x, v);
}

// unordered_set
template <typename C, typename R> // C: unordered set data type, R: R vector
void unordered_set_erase(Rcpp::XPtr<std::unordered_set<C> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void unordered_set_erase_i(Rcpp::XPtr<std::unordered_set<int> > x, Rcpp::IntegerVector& v) {
  unordered_set_erase(x, v);
}
// [[Rcpp::export]]
void unordered_set_erase_d(Rcpp::XPtr<std::unordered_set<double> > x, Rcpp::NumericVector& v) {
  unordered_set_erase(x, v);
}
// [[Rcpp::export]]
void unordered_set_erase_s(Rcpp::XPtr<std::unordered_set<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_set_erase(x, s);
}
// [[Rcpp::export]]
void unordered_set_erase_b(Rcpp::XPtr<std::unordered_set<bool> > x, Rcpp::LogicalVector& v) {
  unordered_set_erase(x, v);
}

// multiset
template <typename C, typename R> // C: multiset data type, R: R vector
void multiset_erase(Rcpp::XPtr<std::multiset<C> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void multiset_erase_i(Rcpp::XPtr<std::multiset<int> > x, Rcpp::IntegerVector& v) {
  multiset_erase(x, v);
}
// [[Rcpp::export]]
void multiset_erase_d(Rcpp::XPtr<std::multiset<double> > x, Rcpp::NumericVector& v) {
  multiset_erase(x, v);
}
// [[Rcpp::export]]
void multiset_erase_s(Rcpp::XPtr<std::multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multiset_erase(x, s);
}
// [[Rcpp::export]]
void multiset_erase_b(Rcpp::XPtr<std::multiset<bool> > x, Rcpp::LogicalVector& v) {
  multiset_erase(x, v);
}

// unordered_multiset
template <typename C, typename R> // C: unordered multiset data type, R: R vector
void unordered_multiset_erase(Rcpp::XPtr<std::unordered_multiset<C> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void unordered_multiset_erase_i(Rcpp::XPtr<std::unordered_multiset<int> > x, Rcpp::IntegerVector& v) {
  unordered_multiset_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multiset_erase_d(Rcpp::XPtr<std::unordered_multiset<double> > x, Rcpp::NumericVector& v) {
  unordered_multiset_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multiset_erase_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multiset_erase(x, s);
}
// [[Rcpp::export]]
void unordered_multiset_erase_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, Rcpp::LogicalVector& v) {
  unordered_multiset_erase(x, v);
}

// map
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
void map_erase(Rcpp::XPtr<std::map<K, V> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void map_erase_i_i(Rcpp::XPtr<std::map<int, int> > x, Rcpp::IntegerVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_i_d(Rcpp::XPtr<std::map<int, double> > x, Rcpp::IntegerVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_i_s(Rcpp::XPtr<std::map<int, std::string> > x, Rcpp::IntegerVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_i_b(Rcpp::XPtr<std::map<int, bool> > x, Rcpp::IntegerVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_d_i(Rcpp::XPtr<std::map<double, int> > x, Rcpp::NumericVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_d_d(Rcpp::XPtr<std::map<double, double> > x, Rcpp::NumericVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_d_s(Rcpp::XPtr<std::map<double, std::string> > x, Rcpp::NumericVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_d_b(Rcpp::XPtr<std::map<double, bool> > x, Rcpp::NumericVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_s_i(Rcpp::XPtr<std::map<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_erase(x, s);
}
// [[Rcpp::export]]
void map_erase_s_d(Rcpp::XPtr<std::map<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_erase(x, s);
}
// [[Rcpp::export]]
void map_erase_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_erase(x, s);
}
// [[Rcpp::export]]
void map_erase_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  map_erase(x, s);
}
// [[Rcpp::export]]
void map_erase_b_i(Rcpp::XPtr<std::map<bool, int> > x, Rcpp::LogicalVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_b_d(Rcpp::XPtr<std::map<bool, double> > x, Rcpp::LogicalVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, Rcpp::LogicalVector& v) {
  map_erase(x, v);
}
// [[Rcpp::export]]
void map_erase_b_b(Rcpp::XPtr<std::map<bool, bool> > x, Rcpp::LogicalVector& v) {
  map_erase(x, v);
}

// unordered_map
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
void unordered_map_erase(Rcpp::XPtr<std::unordered_map<K, V> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void unordered_map_erase_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, Rcpp::IntegerVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, Rcpp::IntegerVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, Rcpp::IntegerVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, Rcpp::IntegerVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, Rcpp::NumericVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, Rcpp::NumericVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, Rcpp::NumericVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, Rcpp::NumericVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_erase(x, s);
}
// [[Rcpp::export]]
void unordered_map_erase_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_erase(x, s);
}
// [[Rcpp::export]]
void unordered_map_erase_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_erase(x, s);
}
// [[Rcpp::export]]
void unordered_map_erase_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_map_erase(x, s);
}
// [[Rcpp::export]]
void unordered_map_erase_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, Rcpp::LogicalVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, Rcpp::LogicalVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, Rcpp::LogicalVector& v) {
  unordered_map_erase(x, v);
}
// [[Rcpp::export]]
void unordered_map_erase_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, Rcpp::LogicalVector& v) {
  unordered_map_erase(x, v);
}

// multimap
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
void multimap_erase(Rcpp::XPtr<std::multimap<K, V> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void multimap_erase_i_i(Rcpp::XPtr<std::multimap<int, int> > x, Rcpp::IntegerVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_i_d(Rcpp::XPtr<std::multimap<int, double> > x, Rcpp::IntegerVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, Rcpp::IntegerVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, Rcpp::IntegerVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_d_i(Rcpp::XPtr<std::multimap<double, int> > x, Rcpp::NumericVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_d_d(Rcpp::XPtr<std::multimap<double, double> > x, Rcpp::NumericVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, Rcpp::NumericVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, Rcpp::NumericVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_erase(x, s);
}
// [[Rcpp::export]]
void multimap_erase_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_erase(x, s);
}
// [[Rcpp::export]]
void multimap_erase_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_erase(x, s);
}
// [[Rcpp::export]]
void multimap_erase_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  multimap_erase(x, s);
}
// [[Rcpp::export]]
void multimap_erase_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, Rcpp::LogicalVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, Rcpp::LogicalVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, Rcpp::LogicalVector& v) {
  multimap_erase(x, v);
}
// [[Rcpp::export]]
void multimap_erase_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, Rcpp::LogicalVector& v) {
  multimap_erase(x, v);
}

// unordered_multimap
template <typename K, typename V, typename R> // K: key data type, V: value data type, R: R vector
void unordered_multimap_erase(Rcpp::XPtr<std::unordered_multimap<K, V> > x, R& v) {
  for(auto i : v) {
    x->erase(i);
  }
}
// [[Rcpp::export]]
void unordered_multimap_erase_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, Rcpp::IntegerVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, Rcpp::IntegerVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, Rcpp::IntegerVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, Rcpp::IntegerVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, Rcpp::NumericVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, Rcpp::NumericVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, Rcpp::NumericVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, Rcpp::NumericVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_erase(x, s);
}
// [[Rcpp::export]]
void unordered_multimap_erase_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_erase(x, s);
}
// [[Rcpp::export]]
void unordered_multimap_erase_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_erase(x, s);
}
// [[Rcpp::export]]
void unordered_multimap_erase_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> s = Rcpp::as<std::vector<std::string> >(v);
  unordered_multimap_erase(x, s);
}
// [[Rcpp::export]]
void unordered_multimap_erase_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, Rcpp::LogicalVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, Rcpp::LogicalVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, Rcpp::LogicalVector& v) {
  unordered_multimap_erase(x, v);
}
// [[Rcpp::export]]
void unordered_multimap_erase_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, Rcpp::LogicalVector& v) {
  unordered_multimap_erase(x, v);
}

// vector
template <typename T> // T: vector data type
void vector_erase(Rcpp::XPtr<std::vector<T> > x, std::size_t from, std::size_t to) {
  if(to < from) {
    Rcpp::stop("from must be smaller than or equal to to.");
  }
  const std::size_t x_size = x->size();
  --from;
  if(from >= x_size) {
    from = x_size;
  }
  if(to >= x_size) {
    to = x_size;
  }
  x->erase(x->begin() + from, x->begin() + to);
}
// [[Rcpp::export]]
void vector_erase_i(Rcpp::XPtr<std::vector<int> > x, const std::size_t from, const std::size_t to) {
  vector_erase(x, from, to);
}
// [[Rcpp::export]]
void vector_erase_d(Rcpp::XPtr<std::vector<double> > x, const std::size_t from, const std::size_t to) {
  vector_erase(x, from, to);
}
// [[Rcpp::export]]
void vector_erase_s(Rcpp::XPtr<std::vector<std::string> > x, const std::size_t from, const std::size_t to) {
  vector_erase(x, from, to);
}
// [[Rcpp::export]]
void vector_erase_b(Rcpp::XPtr<std::vector<bool> > x, const std::size_t from, const std::size_t to) {
  vector_erase(x, from, to);
}

// deque
template <typename T> // T: deque data type
void deque_erase(Rcpp::XPtr<std::deque<T> > x, std::size_t from, std::size_t to) {
  if(to < from) {
    Rcpp::stop("from must be smaller than or equal to to.");
  }
  const std::size_t x_size = x->size();
  --from;
  if(from >= x_size) {
    from = x_size;
  }
  if(to >= x_size) {
    to = x_size;
  }
  x->erase(x->begin() + from, x->begin() + to);
}
// [[Rcpp::export]]
void deque_erase_i(Rcpp::XPtr<std::deque<int> > x, const std::size_t from, const std::size_t to) {
  deque_erase(x, from, to);
}
// [[Rcpp::export]]
void deque_erase_d(Rcpp::XPtr<std::deque<double> > x, const std::size_t from, const std::size_t to) {
  deque_erase(x, from, to);
}
// [[Rcpp::export]]
void deque_erase_s(Rcpp::XPtr<std::deque<std::string> > x, const std::size_t from, const std::size_t to) {
  deque_erase(x, from, to);
}
// [[Rcpp::export]]
void deque_erase_b(Rcpp::XPtr<std::deque<bool> > x, const std::size_t from, const std::size_t to) {
  deque_erase(x, from, to);
}

// list
template <typename T> // T: list data type
void list_erase(Rcpp::XPtr<std::list<T> > x, std::size_t from, std::size_t to) {
  if(to < from) {
    Rcpp::stop("from must be smaller than or equal to to.");
  }
  const std::size_t x_size = x->size();
  --from;
  if(from >= x_size) {
    from = x_size;
  }
  if(to >= x_size) {
    to = x_size;
  }
  x->erase(std::next(x->begin(), from), std::next(x->begin(), to));
}
// [[Rcpp::export]]
void list_erase_i(Rcpp::XPtr<std::list<int> > x, const std::size_t from, const std::size_t to) {
  list_erase(x, from, to);
}
// [[Rcpp::export]]
void list_erase_d(Rcpp::XPtr<std::list<double> > x, const std::size_t from, const std::size_t to) {
  list_erase(x, from, to);
}
// [[Rcpp::export]]
void list_erase_s(Rcpp::XPtr<std::list<std::string> > x, const std::size_t from, const std::size_t to) {
  list_erase(x, from, to);
}
// [[Rcpp::export]]
void list_erase_b(Rcpp::XPtr<std::list<bool> > x, const std::size_t from, const std::size_t to) {
  list_erase(x, from, to);
}
