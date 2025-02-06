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
template <typename T>  // T: set data type
void set_print(Rcpp::XPtr<std::set<T> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const T from, const bool use_to,
  const T to) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      // print from the back
      auto j = x->rbegin();
      for(std::size_t i = 0; i != n; ++i) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *j << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    } else {
      // print from the front
      auto j = x->begin();
      for(std::size_t i = 0; i != n; ++i) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *j << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (j and k)
      typename std::set<T>::iterator j;
      if(use_from) {
        j = x->lower_bound(from);
        if(j == x->end()) {
          std::string emsg;
          if constexpr (std::is_same_v<T, std::string>) {
            emsg += from;
          } else if constexpr (std::is_same_v<T, bool>) {
            emsg += ((from) ? "TRUE" : "FALSE");
          } else {
            emsg += std::to_string(from);
          }
          emsg += " is larger than the maximum value in x.";
          Rcpp::stop(emsg);
        }
      } else {
        j = x->begin();
      }
      typename std::set<T>::iterator k;
      if(use_to) {
        k = x->upper_bound(to);
      } else {
        k = x->end();
      }
      // print elements
      std::size_t i = 0;
      for(auto l = j; l != k; ++l) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*l) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *l << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++i;
      }
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void set_print_i(Rcpp::XPtr<std::set<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from, const bool use_to,
  const int to) {
  set_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void set_print_d(Rcpp::XPtr<std::set<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  set_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void set_print_s(Rcpp::XPtr<std::set<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const std::string from,
  const bool use_to, const std::string to) {
  set_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void set_print_b(Rcpp::XPtr<std::set<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  set_print(x, use_n, n, reverse, use_from, from, use_to, to);
}

// unordered_set
template <typename T> // T: unordered set data type
void unordered_set_print(Rcpp::XPtr<std::unordered_set<T> > x, std::size_t n) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  for(std::size_t i = 0; i != n; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    if(i % 4999 == 0) {
      Rcpp::Rcout << std::flush;
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_set_print_i(Rcpp::XPtr<std::unordered_set<int> > x, std::size_t n) {
  unordered_set_print(x, n);
}
// [[Rcpp::export]]
void unordered_set_print_d(Rcpp::XPtr<std::unordered_set<double> > x, std::size_t n) {
  unordered_set_print(x, n);
}
// [[Rcpp::export]]
void unordered_set_print_s(Rcpp::XPtr<std::unordered_set<std::string> > x, std::size_t n) {
  unordered_set_print(x, n);
}
// [[Rcpp::export]]
void unordered_set_print_b(Rcpp::XPtr<std::unordered_set<bool> > x, std::size_t n) {
  unordered_set_print(x, n);
}

// multiset
template <typename T>  // T: multiset data type
void multiset_print(Rcpp::XPtr<std::multiset<T> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const T from,
  const bool use_to, const T to) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      // print from the back
      auto j = x->rbegin();
      for(std::size_t i = 0; i != n; ++i) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *j << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    } else {
      // print from the front
      auto j = x->begin();
      for(std::size_t i = 0; i != n; ++i) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *j << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (j and k)
      typename std::multiset<T>::iterator j;
      if(use_from) {
        j = x->lower_bound(from);
        if(j == x->end()) {
          std::string emsg;
          if constexpr (std::is_same_v<T, std::string>) {
            emsg += from;
          } else if constexpr (std::is_same_v<T, bool>) {
            emsg += ((from) ? "TRUE" : "FALSE");
          } else {
            emsg += std::to_string(from);
          }
          emsg += " is larger than the maximum value in x.";
          Rcpp::stop(emsg);
        }
      } else {
        j = x->begin();
      }
      typename std::multiset<T>::iterator k;
      if(use_to) {
        k = x->upper_bound(to);
      } else {
        k = x->end();
      }
      // print elements
      std::size_t i = 0;
      for(auto l = j; l != k; ++l) {
        if constexpr (std::is_same_v<T, bool>) {
          Rcpp::Rcout << ((*l) ? "TRUE" : "FALSE") << ' ';
        } else {
          Rcpp::Rcout << q << *l << q << ' ';
        }
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++i;
      }
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void multiset_print_i(Rcpp::XPtr<std::multiset<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  multiset_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multiset_print_d(Rcpp::XPtr<std::multiset<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  multiset_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multiset_print_s(Rcpp::XPtr<std::multiset<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  multiset_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multiset_print_b(Rcpp::XPtr<std::multiset<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  multiset_print(x, use_n, n, reverse, use_from, from, use_to, to);
}

// unordered_multiset
template <typename T> // T: unordered multiset data type
void unordered_multiset_print(Rcpp::XPtr<std::unordered_multiset<T> > x, std::size_t n) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  for(std::size_t i = 0; i != n; ++i) {
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << *j << q << ' ';
    }
    if(i % 4999 == 0) {
      Rcpp::Rcout << std::flush;
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_multiset_print_i(Rcpp::XPtr<std::unordered_multiset<int> > x, std::size_t n) {
  unordered_multiset_print(x, n);
}
// [[Rcpp::export]]
void unordered_multiset_print_d(Rcpp::XPtr<std::unordered_multiset<double> > x, std::size_t n) {
  unordered_multiset_print(x, n);
}
// [[Rcpp::export]]
void unordered_multiset_print_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, std::size_t n) {
  unordered_multiset_print(x, n);
}
// [[Rcpp::export]]
void unordered_multiset_print_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, std::size_t n) {
  unordered_multiset_print(x, n);
}

// map
template <typename K, typename V> // K: key data type, V: value data type
void map_print(Rcpp::XPtr<std::map<K, V> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const K from, const bool use_to,
  const K to) {
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      // print from the back
      auto j = x->rbegin();
      for(std::size_t i = 0; i != n; ++i) {
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
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    } else {
      // print from the front
      auto j = x->begin();
      for(std::size_t i = 0; i != n; ++i) {
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
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (j and k)
      typename std::map<K, V>::iterator j;
      if(use_from) {
        j = x->lower_bound(from);
        if(j == x->end()) {
          std::string emsg;
          if constexpr (std::is_same_v<K, std::string>) {
            emsg += from;
          } else {
            emsg += std::to_string(from);
          }
          emsg += " is larger than the maximum value in x.";
          Rcpp::stop(emsg);
        }
      } else {
        j = x->begin();
      }
      typename std::map<K, V>::iterator k;
      if(use_to) {
        k = x->upper_bound(to);
      } else {
        k = x->end();
      }
      // print elements
      std::size_t i = 0;
      for(auto l = j; l != k; ++l) {
        Rcpp::Rcout << lhs;
        if constexpr (kbool) {
          Rcpp::Rcout << (l->first ? "TRUE" : "FALSE");
        } else {
          Rcpp::Rcout << l->first;
        }
        Rcpp::Rcout << ctr;
        if constexpr (vbool) {
          Rcpp::Rcout << (l->second ? "TRUE" : "FALSE");
        } else {
          Rcpp::Rcout << l->second;
        }
        Rcpp::Rcout << rhs;
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++i;
      }
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void map_print_i_i(Rcpp::XPtr<std::map<int, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_i_d(Rcpp::XPtr<std::map<int, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_i_b(Rcpp::XPtr<std::map<int, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_d_i(Rcpp::XPtr<std::map<double, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_d_d(Rcpp::XPtr<std::map<double, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_d_b(Rcpp::XPtr<std::map<double, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void map_print_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  map_print(x, use_n, n, reverse, use_from, from, use_to, to);
}

// unordered_map
template <typename K, typename V> // K: key data type, V: value data type
void unordered_map_print(Rcpp::XPtr<std::unordered_map<K, V> > x, std::size_t n) {
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  for(std::size_t i = 0; i != n; ++i) {
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
    if(i % 4999 == 0) {
      Rcpp::Rcout << std::flush;
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_map_print_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, std::size_t n) {
  unordered_map_print(x, n);
}
// [[Rcpp::export]]
void unordered_map_print_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, std::size_t n) {
  unordered_map_print(x, n);
}

// multimap
template <typename K, typename V> // K: key data type, V: value data type
void multimap_print(Rcpp::XPtr<std::multimap<K, V> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const K from,
  const bool use_to, const K to) {
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      // print from the back
      auto j = x->rbegin();
      for(std::size_t i = 0; i != n; ++i) {
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
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    } else {
      // print from the front
      auto j = x->begin();
      for(std::size_t i = 0; i != n; ++i) {
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
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++j;
      }
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (j and k)
      typename std::multimap<K, V>::iterator j;
      if(use_from) {
        j = x->lower_bound(from);
        if(j == x->end()) {
          std::string emsg;
          if constexpr (std::is_same_v<K, std::string>) {
            emsg += from;
          } else {
            emsg += std::to_string(from);
          }
          emsg += " is larger than the maximum value in x.";
          Rcpp::stop(emsg);
        }
      } else {
        j = x->begin();
      }
      typename std::multimap<K, V>::iterator k;
      if(use_to) {
        k = x->upper_bound(to);
      } else {
        k = x->end();
      }
      // print elements
      std::size_t i = 0;
      for(auto l = j; l != k; ++l) {
        Rcpp::Rcout << lhs;
        if constexpr (kbool) {
          Rcpp::Rcout << (l->first ? "TRUE" : "FALSE");
        } else {
          Rcpp::Rcout << l->first;
        }
        Rcpp::Rcout << ctr;
        if constexpr (vbool) {
          Rcpp::Rcout << (l->second ? "TRUE" : "FALSE");
        } else {
          Rcpp::Rcout << l->second;
        }
        Rcpp::Rcout << rhs;
        if(i % 4999 == 0) {
          Rcpp::Rcout << std::flush;
        }
        ++i;
      }
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void multimap_print_i_i(Rcpp::XPtr<std::multimap<int, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_i_d(Rcpp::XPtr<std::multimap<int, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_d_i(Rcpp::XPtr<std::multimap<double, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_d_d(Rcpp::XPtr<std::multimap<double, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void multimap_print_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  multimap_print(x, use_n, n, reverse, use_from, from, use_to, to);
}

// unordered_multimap
template <typename K, typename V> // K: key data type, V: value data type
void unordered_multimap_print(Rcpp::XPtr<std::unordered_multimap<K, V> > x, std::size_t n) {
  constexpr bool kstring = std::is_same_v<K, std::string>;
  constexpr bool vstring = std::is_same_v<V, std::string>;
  constexpr bool kbool = !kstring && std::is_same_v<K, bool>;
  constexpr bool vbool = !vstring && std::is_same_v<V, bool>;
  constexpr std::string_view lhs = kstring ? "[\"" : "[";
  constexpr std::string_view ctr = kstring ? (vstring ? "\",\"" : "\",") : (vstring ? ",\"" : ",");
  constexpr std::string_view rhs = vstring ? "\"] " : "] ";
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  for(std::size_t i = 0; i != n; ++i) {
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
    if(i % 4999 == 0) {
      Rcpp::Rcout << std::flush;
    }
    ++j;
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void unordered_multimap_print_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}
// [[Rcpp::export]]
void unordered_multimap_print_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, std::size_t n) {
  unordered_multimap_print(x, n);
}

// stack
template <typename T>
void stack_print(Rcpp::XPtr<std::stack<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty stack";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "Top element: ";
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((p->top()) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << p->top() << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void stack_print_i(Rcpp::XPtr<std::stack<int> > p) {
  stack_print(p);
}
// [[Rcpp::export]]
void stack_print_d(Rcpp::XPtr<std::stack<double> > p) {
  stack_print(p);
}
// [[Rcpp::export]]
void stack_print_s(Rcpp::XPtr<std::stack<std::string> > p) {
  stack_print(p);
}
// [[Rcpp::export]]
void stack_print_b(Rcpp::XPtr<std::stack<bool> > p) {
  stack_print(p);
}

// queue
template <typename T>
void queue_print(Rcpp::XPtr<std::queue<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "First element: ";
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((p->front()) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << p->front() << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void queue_print_i(Rcpp::XPtr<std::queue<int> > p) {
  queue_print(p);
}
// [[Rcpp::export]]
void queue_print_d(Rcpp::XPtr<std::queue<double> > p) {
  queue_print(p);
}
// [[Rcpp::export]]
void queue_print_s(Rcpp::XPtr<std::queue<std::string> > p) {
  queue_print(p);
}
// [[Rcpp::export]]
void queue_print_b(Rcpp::XPtr<std::queue<bool> > p) {
  queue_print(p);
}

// priority_queue
template <typename T>
void priority_queue_print_d(Rcpp::XPtr<std::priority_queue<T> > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty priority queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "Top element: ";
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((p->top()) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << p->top() << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void priority_queue_print_i_d(Rcpp::XPtr<std::priority_queue<int> > p) {
  priority_queue_print_d(p);
}
// [[Rcpp::export]]
void priority_queue_print_d_d(Rcpp::XPtr<std::priority_queue<double> > p) {
  priority_queue_print_d(p);
}
// [[Rcpp::export]]
void priority_queue_print_s_d(Rcpp::XPtr<std::priority_queue<std::string> > p) {
  priority_queue_print_d(p);
}
// [[Rcpp::export]]
void priority_queue_print_b_d(Rcpp::XPtr<std::priority_queue<bool> > p) {
  priority_queue_print_d(p);
}
template <typename T>
void priority_queue_print_a(Rcpp::XPtr<std::priority_queue<T, std::vector<T>, std::greater<T> > > p) {
  if(p->empty()) {
    Rcpp::Rcout << "Empty priority queue";
  } else {
    constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
    Rcpp::Rcout << "Top element: ";
    if constexpr (std::is_same_v<T, bool>) {
      Rcpp::Rcout << ((p->top()) ? "TRUE" : "FALSE") << ' ';
    } else {
      Rcpp::Rcout << q << p->top() << q << ' ';
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void priority_queue_print_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > p) {
  priority_queue_print_a(p);
}
// [[Rcpp::export]]
void priority_queue_print_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > p) {
  priority_queue_print_a(p);
}
// [[Rcpp::export]]
void priority_queue_print_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > p) {
  priority_queue_print_a(p);
}
// [[Rcpp::export]]
void priority_queue_print_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > p) {
  priority_queue_print_a(p);
}

// vector
template <typename T>
void vector_print(Rcpp::XPtr<std::vector<T> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  const std::size_t x_size = p->size();
  if(use_n) {
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      from = x_size - 1;
      to = from - n;
    } else {
      from = 0;
      to = n;
    }
  } else {
    if(use_from) {
      --from;
      if(from >= x_size) {
        Rcpp::stop("from points to an index outside x.");
      }
    } else {
      from = 0;
    }
    if(use_to) {
      if(to > x_size) {
        Rcpp::stop("to points to an index outside x.");
      }
    } else {
      to = x_size;
    }
    if(use_from && use_to && from >= to) {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  if(reverse) {
    // print from the back
    for(std::size_t i = from; i != to; --i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << (*p)[i] << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
    }
  } else {
    // print from the front
    for(std::size_t i = from; i != to; ++i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << (*p)[i] << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void vector_print_i(Rcpp::XPtr<std::vector<int> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  vector_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void vector_print_d(Rcpp::XPtr<std::vector<double> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  vector_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void vector_print_s(Rcpp::XPtr<std::vector<std::string> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  vector_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void vector_print_b(Rcpp::XPtr<std::vector<bool> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  vector_print(p, use_n, n, reverse, use_from, from, use_to, to);
}

// deque
template <typename T>
void deque_print(Rcpp::XPtr<std::deque<T> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  const std::size_t x_size = p->size();
  if(use_n) {
    if(n > x_size || n == 0) {
      n = x_size;
    }
    if(reverse) {
      from = x_size - 1;
      to = from - n;
    } else {
      from = 0;
      to = n;
    }
  } else {
    if(use_from) {
      --from;
      if(from >= x_size) {
        Rcpp::stop("from points to an index outside x.");
      }
    } else {
      from = 0;
    }
    if(use_to) {
      if(to > x_size) {
        Rcpp::stop("to points to an index outside x.");
      }
    } else {
      to = x_size;
    }
    if(use_from && use_to && from >= to) {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
  if(reverse) {
    // print from the back
    for(std::size_t i = from; i != to; --i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << (*p)[i] << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
    }
  } else {
    // print from the front
    for(std::size_t i = from; i != to; ++i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << (((*p)[i]) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << (*p)[i] << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void deque_print_i(Rcpp::XPtr<std::deque<int> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  deque_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void deque_print_d(Rcpp::XPtr<std::deque<double> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  deque_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void deque_print_s(Rcpp::XPtr<std::deque<std::string> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  deque_print(p, use_n, n, reverse, use_from, from, use_to, to);
}
// [[Rcpp::export]]
void deque_print_b(Rcpp::XPtr<std::deque<bool> > p, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from,
  const bool use_to, std::size_t to) {
  deque_print(p, use_n, n, reverse, use_from, from, use_to, to);
}

// forward_list
template <typename T>
void forward_list_print(Rcpp::XPtr<std::forward_list<T> > p, const std::size_t n) {
  auto j = p->begin();
  auto e = p->end();
  std::size_t i = 0;
  while(i < n && j != e) {
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
void forward_list_print_i(Rcpp::XPtr<std::forward_list<int> > p, const std::size_t n) {
  forward_list_print(p, n);
}
// [[Rcpp::export]]
void forward_list_print_d(Rcpp::XPtr<std::forward_list<double> > p, const std::size_t n) {
  forward_list_print(p, n);
}
// [[Rcpp::export]]
void forward_list_print_s(Rcpp::XPtr<std::forward_list<std::string> > p, const std::size_t n) {
  forward_list_print(p, n);
}
// [[Rcpp::export]]
void forward_list_print_b(Rcpp::XPtr<std::forward_list<bool> > p, const std::size_t n) {
  forward_list_print(p, n);
}

// list
template <typename T> // T: list data type
void list_print(Rcpp::XPtr<std::list<T> > x, std::size_t n, const bool reverse) {
  constexpr std::string_view q = (std::is_same_v<T, std::string>) ? "\"" : "";
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  if(reverse) {
    auto j = x->rbegin();
    for(std::size_t i = 0; i != n; ++i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << *j << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
      ++j;
    }
  } else {
    auto j = x->begin();
    for(std::size_t i = 0; i != n; ++i) {
      if constexpr (std::is_same_v<T, bool>) {
        Rcpp::Rcout << ((*j) ? "TRUE" : "FALSE") << ' ';
      } else {
        Rcpp::Rcout << q << *j << q << ' ';
      }
      if(i % 4999 == 0) {
        Rcpp::Rcout << std::flush;
      }
      ++j;
    }
  }
  Rcpp::Rcout << std::endl;
}
// [[Rcpp::export]]
void list_print_i(Rcpp::XPtr<std::list<int> > x, std::size_t n, const bool reverse) {
  list_print(x, n, reverse);
}
// [[Rcpp::export]]
void list_print_d(Rcpp::XPtr<std::list<double> > x, std::size_t n, const bool reverse) {
  list_print(x, n, reverse);
}
// [[Rcpp::export]]
void list_print_s(Rcpp::XPtr<std::list<std::string> > x, std::size_t n, const bool reverse) {
  list_print(x, n, reverse);
}
// [[Rcpp::export]]
void list_print_b(Rcpp::XPtr<std::list<bool> > x, std::size_t n, const bool reverse) {
  list_print(x, n, reverse);
}
