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
#include <iostream>
#include <type_traits>
#include <string_view>

// set
template <typename T>
void set_show(Rcpp::XPtr<std::set<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void set_show_i(Rcpp::XPtr<std::set<int> > p) {
  set_show(p);
}
// [[Rcpp::export]]
void set_show_d(Rcpp::XPtr<std::set<double> > p) {
  set_show(p);
}
// [[Rcpp::export]]
void set_show_s(Rcpp::XPtr<std::set<std::string> > p) {
  set_show(p);
}
// [[Rcpp::export]]
void set_show_b(Rcpp::XPtr<std::set<bool> > p) {
  set_show(p);
}

// unordered_set
template <typename T>
void unordered_set_show(Rcpp::XPtr<std::unordered_set<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_set_show_i(Rcpp::XPtr<std::unordered_set<int> > p) {
  unordered_set_show(p);
}
// [[Rcpp::export]]
void unordered_set_show_d(Rcpp::XPtr<std::unordered_set<double> > p) {
  unordered_set_show(p);
}
// [[Rcpp::export]]
void unordered_set_show_s(Rcpp::XPtr<std::unordered_set<std::string> > p) {
  unordered_set_show(p);
}
// [[Rcpp::export]]
void unordered_set_show_b(Rcpp::XPtr<std::unordered_set<bool> > p) {
  unordered_set_show(p);
}

// multiset
template <typename T>
void multiset_show(Rcpp::XPtr<std::multiset<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  typename std::multiset<T>::iterator j = p->begin();
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void multiset_show_i(Rcpp::XPtr<std::multiset<int> > p) {
  multiset_show(p);
}
// [[Rcpp::export]]
void multiset_show_d(Rcpp::XPtr<std::multiset<double> > p) {
  multiset_show(p);
}
// [[Rcpp::export]]
void multiset_show_s(Rcpp::XPtr<std::multiset<std::string> > p) {
  multiset_show(p);
}
// [[Rcpp::export]]
void multiset_show_b(Rcpp::XPtr<std::multiset<bool> > p) {
  multiset_show(p);
}

// unordered_multiset
template <typename T>
void unordered_multiset_show(Rcpp::XPtr<std::unordered_multiset<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_multiset_show_i(Rcpp::XPtr<std::unordered_multiset<int> > p) {
  unordered_multiset_show(p);
}
// [[Rcpp::export]]
void unordered_multiset_show_d(Rcpp::XPtr<std::unordered_multiset<double> > p) {
  unordered_multiset_show(p);
}
// [[Rcpp::export]]
void unordered_multiset_show_s(Rcpp::XPtr<std::unordered_multiset<std::string> > p) {
  unordered_multiset_show(p);
}
// [[Rcpp::export]]
void unordered_multiset_show_b(Rcpp::XPtr<std::unordered_multiset<bool> > p) {
  unordered_multiset_show(p);
}

// map
template <typename K, typename V>
void map_show(Rcpp::XPtr<std::map<K, V> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  for(std::size_t i = 0; i != n_print; ++i) {
    Rcpp::Rcout << lhs;
    if constexpr (kbool) {
      Rcpp::Rcout << (j->first ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->first;
    }
    Rcpp::Rcout << ctr;
    if constexpr (vbool) {
      Rcpp::Rcout << (j->second ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->second;
    }
    Rcpp::Rcout << rhs;
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void map_show_i_i(Rcpp::XPtr<std::map<int, int> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_i_d(Rcpp::XPtr<std::map<int, double> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_i_s(Rcpp::XPtr<std::map<int, std::string> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_i_b(Rcpp::XPtr<std::map<int, bool> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_d_i(Rcpp::XPtr<std::map<double, int> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_d_d(Rcpp::XPtr<std::map<double, double> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_d_s(Rcpp::XPtr<std::map<double, std::string> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_d_b(Rcpp::XPtr<std::map<double, bool> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_s_i(Rcpp::XPtr<std::map<std::string, int> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_s_d(Rcpp::XPtr<std::map<std::string, double> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_s_s(Rcpp::XPtr<std::map<std::string, std::string> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_s_b(Rcpp::XPtr<std::map<std::string, bool> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_b_i(Rcpp::XPtr<std::map<bool, int> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_b_d(Rcpp::XPtr<std::map<bool, double> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_b_s(Rcpp::XPtr<std::map<bool, std::string> > p) {
  map_show(p);
}
// [[Rcpp::export]]
void map_show_b_b(Rcpp::XPtr<std::map<bool, bool> > p) {
  map_show(p);
}

// unordered_map
template <typename K, typename V>
void unordered_map_show(Rcpp::XPtr<std::unordered_map<K, V> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  for(std::size_t i = 0; i != n_print; ++i) {
    Rcpp::Rcout << lhs;
    if constexpr (kbool) {
      Rcpp::Rcout << (j->first ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->first;
    }
    Rcpp::Rcout << ctr;
    if constexpr (vbool) {
      Rcpp::Rcout << (j->second ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->second;
    }
    Rcpp::Rcout << rhs;
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_map_show_i_i(Rcpp::XPtr<std::unordered_map<int, int> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_i_d(Rcpp::XPtr<std::unordered_map<int, double> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_d_i(Rcpp::XPtr<std::unordered_map<double, int> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_d_d(Rcpp::XPtr<std::unordered_map<double, double> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > p) {
  unordered_map_show(p);
}
// [[Rcpp::export]]
void unordered_map_show_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > p) {
  unordered_map_show(p);
}

// multimap
template <typename K, typename V>
void multimap_show(Rcpp::XPtr<std::multimap<K, V> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  for(std::size_t i = 0; i != n_print; ++i) {
    Rcpp::Rcout << lhs;
    if constexpr (kbool) {
      Rcpp::Rcout << (j->first ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->first;
    }
    Rcpp::Rcout << ctr;
    if constexpr (vbool) {
      Rcpp::Rcout << (j->second ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->second;
    }
    Rcpp::Rcout << rhs;
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void multimap_show_i_i(Rcpp::XPtr<std::multimap<int, int> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_i_d(Rcpp::XPtr<std::multimap<int, double> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_i_s(Rcpp::XPtr<std::multimap<int, std::string> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_i_b(Rcpp::XPtr<std::multimap<int, bool> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_d_i(Rcpp::XPtr<std::multimap<double, int> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_d_d(Rcpp::XPtr<std::multimap<double, double> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_d_s(Rcpp::XPtr<std::multimap<double, std::string> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_d_b(Rcpp::XPtr<std::multimap<double, bool> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_s_i(Rcpp::XPtr<std::multimap<std::string, int> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_s_d(Rcpp::XPtr<std::multimap<std::string, double> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_b_i(Rcpp::XPtr<std::multimap<bool, int> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_b_d(Rcpp::XPtr<std::multimap<bool, double> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > p) {
  multimap_show(p);
}
// [[Rcpp::export]]
void multimap_show_b_b(Rcpp::XPtr<std::multimap<bool, bool> > p) {
  multimap_show(p);
}

// unordered_multimap
template <typename K, typename V>
void unordered_multimap_show(Rcpp::XPtr<std::unordered_multimap<K, V> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  for(std::size_t i = 0; i != n_print; ++i) {
    Rcpp::Rcout << lhs;
    if constexpr (kbool) {
      Rcpp::Rcout << (j->first ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->first;
    }
    Rcpp::Rcout << ctr;
    if constexpr (vbool) {
      Rcpp::Rcout << (j->second ? "TRUE" : "FALSE");
    } else {
      Rcpp::Rcout << j->second;
    }
    Rcpp::Rcout << rhs;
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_multimap_show_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > p) {
  unordered_multimap_show(p);
}
// [[Rcpp::export]]
void unordered_multimap_show_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > p) {
  unordered_multimap_show(p);
}

// stack
template <typename T>
void stack_show(Rcpp::XPtr<std::stack<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty stack";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "Top element: " << q << p->top() << q;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void stack_show_i(Rcpp::XPtr<std::stack<int> > p) {
  stack_show(p);
}
// [[Rcpp::export]]
void stack_show_d(Rcpp::XPtr<std::stack<double> > p) {
  stack_show(p);
}
// [[Rcpp::export]]
void stack_show_s(Rcpp::XPtr<std::stack<std::string> > p) {
  stack_show(p);
}
// [[Rcpp::export]]
void stack_show_b(Rcpp::XPtr<std::stack<bool> > p) {
  stack_show(p);
}

// queue
template <typename T>
void queue_show(Rcpp::XPtr<std::queue<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "First element: " << q << p->front() << q;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void queue_show_i(Rcpp::XPtr<std::queue<int> > p) {
  queue_show(p);
}
// [[Rcpp::export]]
void queue_show_d(Rcpp::XPtr<std::queue<double> > p) {
  queue_show(p);
}
// [[Rcpp::export]]
void queue_show_s(Rcpp::XPtr<std::queue<std::string> > p) {
  queue_show(p);
}
// [[Rcpp::export]]
void queue_show_b(Rcpp::XPtr<std::queue<bool> > p) {
  queue_show(p);
}

// priority_queue
template <typename T>
void priority_queue_show_d(Rcpp::XPtr<std::priority_queue<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty priority queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "First element: " << q << p->top() << q;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void priority_queue_show_i_d(Rcpp::XPtr<std::priority_queue<int> > p) {
  priority_queue_show_d(p);
}
// [[Rcpp::export]]
void priority_queue_show_d_d(Rcpp::XPtr<std::priority_queue<double> > p) {
  priority_queue_show_d(p);
}
// [[Rcpp::export]]
void priority_queue_show_s_d(Rcpp::XPtr<std::priority_queue<std::string> > p) {
  priority_queue_show_d(p);
}
// [[Rcpp::export]]
void priority_queue_show_b_d(Rcpp::XPtr<std::priority_queue<bool> > p) {
  priority_queue_show_d(p);
}
template <typename T>
void priority_queue_show_a(Rcpp::XPtr<std::priority_queue<T, std::vector<T>, std::greater<T> > > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty priority queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "First element: " << q << p->top() << q;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void priority_queue_show_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > p) {
  priority_queue_show_a(p);
}
// [[Rcpp::export]]
void priority_queue_show_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > p) {
  priority_queue_show_a(p);
}
// [[Rcpp::export]]
void priority_queue_show_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > p) {
  priority_queue_show_a(p);
}
// [[Rcpp::export]]
void priority_queue_show_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > p) {
  priority_queue_show_a(p);
}

// vector
template <typename T>
void vector_show(Rcpp::XPtr<std::vector<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << (*p)[i] << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void vector_show_i(Rcpp::XPtr<std::vector<int> > p) {
  vector_show(p);
}
// [[Rcpp::export]]
void vector_show_d(Rcpp::XPtr<std::vector<double> > p) {
  vector_show(p);
}
// [[Rcpp::export]]
void vector_show_s(Rcpp::XPtr<std::vector<std::string> > p) {
  vector_show(p);
}
// [[Rcpp::export]]
void vector_show_b(Rcpp::XPtr<std::vector<bool> > p) {
  vector_show(p);
}

// deque
template <typename T>
void deque_show(Rcpp::XPtr<std::deque<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << (*p)[i] << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void deque_show_i(Rcpp::XPtr<std::deque<int> > p) {
  deque_show(p);
}
// [[Rcpp::export]]
void deque_show_d(Rcpp::XPtr<std::deque<double> > p) {
  deque_show(p);
}
// [[Rcpp::export]]
void deque_show_s(Rcpp::XPtr<std::deque<std::string> > p) {
  deque_show(p);
}
// [[Rcpp::export]]
void deque_show_b(Rcpp::XPtr<std::deque<bool> > p) {
  deque_show(p);
}

// forward_list
template <typename T>
void forward_list_show(Rcpp::XPtr<std::forward_list<T> > p) {
  auto j = p->begin();
  auto e = p->end();
  std::size_t i = 0;
  while(i < 100 && j != e) {
    ++j;
    ++i;
  }
  for(auto k = p->begin(); k != j; ++k) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*k) ? "TRUE " : "FALSE ");
    } else if constexpr (std::is_same_v<T, std::string>) {
      Rcpp::Rcout << "\"" << *k << "\" ";
    } else {
      Rcpp::Rcout << *k << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void forward_list_show_i(Rcpp::XPtr<std::forward_list<int> > p) {
  forward_list_show(p);
}
// [[Rcpp::export]]
void forward_list_show_d(Rcpp::XPtr<std::forward_list<double> > p) {
  forward_list_show(p);
}
// [[Rcpp::export]]
void forward_list_show_s(Rcpp::XPtr<std::forward_list<std::string> > p) {
  forward_list_show(p);
}
// [[Rcpp::export]]
void forward_list_show_b(Rcpp::XPtr<std::forward_list<bool> > p) {
  forward_list_show(p);
}

// list
template <typename T>
void list_show(Rcpp::XPtr<std::list<T> > p) {
  std::size_t n_print = p->size();
  if(n_print > 100) {
    n_print = 100;
    Rcpp::Rcout << "First 100 elements: ";
  }
  auto j = p->begin();
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  for(std::size_t i = 0; i != n_print; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void list_show_i(Rcpp::XPtr<std::list<int> > p) {
  list_show(p);
}
// [[Rcpp::export]]
void list_show_d(Rcpp::XPtr<std::list<double> > p) {
  list_show(p);
}
// [[Rcpp::export]]
void list_show_s(Rcpp::XPtr<std::list<std::string> > p) {
  list_show(p);
}
// [[Rcpp::export]]
void list_show_b(Rcpp::XPtr<std::list<bool> > p) {
  list_show(p);
}
