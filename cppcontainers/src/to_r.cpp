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
#include <type_traits>
#include <iterator>

template <typename T>
void from_gt_maximum(const T from) {
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

// set
template <typename C, typename R> // C: set data type, R: R vector
R set_to_r(Rcpp::XPtr<std::set<C> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const C from, const bool use_to,
  const C to, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  if(!use_n && !use_from && !use_to) {
    return Rcpp::wrap(*x);
  }
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size) {
      n = x_size;
    }
    if(reverse) {
      const auto b = x->rbegin();
      auto e = b;
      std::advance(e, n);
      R rvector (b, e);
      return rvector;
    } else {
      const auto b = x->begin();
      auto e = b;
      std::advance(e, n);
      R rvector (b, e);
      return rvector;
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (b and e)
      typename std::set<C>::iterator b;
      if(use_from) {
        b = x->lower_bound(from);
        if(b == x->end()) {
          from_gt_maximum(from);
        }
      } else {
        b = x->begin();
      }
      typename std::set<C>::iterator e;
      if(use_to) {
        e = x->upper_bound(to);
      } else {
        e = x->end();
      }
      R rvector (b, e);
      return rvector;
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
}
// [[Rcpp::export]]
Rcpp::IntegerVector set_to_r_i(Rcpp::XPtr<std::set<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  Rcpp::IntegerVector t;
  return set_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector set_to_r_d(Rcpp::XPtr<std::set<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const double from,
  const bool use_to, const double to) {
  Rcpp::NumericVector t;
  return set_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector set_to_r_s(Rcpp::XPtr<std::set<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector t;
  return set_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector set_to_r_b(Rcpp::XPtr<std::set<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  Rcpp::LogicalVector t;
  return set_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}

// unordered_set
template <typename C, typename R> // C: unordered set data type, R: R vector
R unordered_set_to_r(Rcpp::XPtr<std::unordered_set<C> > x, std::size_t n, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  if(n == 0) {
    return Rcpp::wrap(*x);
  }
  const std::size_t x_size = x->size();
  if(n > x_size) {
    n = x_size;
  }
  const auto b = x->begin();
  auto e = b;
  std::advance(e, n);
  R rvector (b, e);
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_set_to_r_i(Rcpp::XPtr<std::unordered_set<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return unordered_set_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector unordered_set_to_r_d(Rcpp::XPtr<std::unordered_set<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return unordered_set_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector unordered_set_to_r_s(Rcpp::XPtr<std::unordered_set<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return unordered_set_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector unordered_set_to_r_b(Rcpp::XPtr<std::unordered_set<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return unordered_set_to_r(x, n, t);
}

// multiset
template <typename C, typename R> // C: multiset data type, R: R vector
R multiset_to_r(Rcpp::XPtr<std::multiset<C> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const C from, const bool use_to,
  const C to, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  if(!use_n && !use_from && !use_to) {
    return Rcpp::wrap(*x);
  }
  if(use_n) {
    const std::size_t x_size = x->size();
    if(n > x_size) {
      n = x_size;
    }
    if(reverse) {
      const auto b = x->rbegin();
      auto e = b;
      std::advance(e, n);
      R rvector (b, e);
      return rvector;
    } else {
      const auto b = x->begin();
      auto e = b;
      std::advance(e, n);
      R rvector (b, e);
      return rvector;
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (b and e)
      typename std::multiset<C>::iterator b;
      if(use_from) {
        b = x->lower_bound(from);
        if(b == x->end()) {
          from_gt_maximum(from);
        }
      } else {
        b = x->begin();
      }
      typename std::multiset<C>::iterator e;
      if(use_to) {
        e = x->upper_bound(to);
      } else {
        e = x->end();
      }
      R rvector (b, e);
      return rvector;
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
}
// [[Rcpp::export]]
Rcpp::IntegerVector multiset_to_r_i(Rcpp::XPtr<std::multiset<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector t;
  return multiset_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector multiset_to_r_d(Rcpp::XPtr<std::multiset<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector t;
  return multiset_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector multiset_to_r_s(Rcpp::XPtr<std::multiset<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector t;
  return multiset_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector multiset_to_r_b(Rcpp::XPtr<std::multiset<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector t;
  return multiset_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}

// unordered_multiset
template <typename C, typename R> // C: unordered multiset data type, R: R vector
R unordered_multiset_to_r(Rcpp::XPtr<std::unordered_multiset<C> > x, std::size_t n, R& t) {
  // t tells the compiler the vector type, but is otherwise not utilized
  if(n == 0) {
    return Rcpp::wrap(*x);
  }
  const std::size_t x_size = x->size();
  if(n > x_size) {
    n = x_size;
  }
  const auto b = x->begin();
  auto e = b;
  std::advance(e, n);
  R rvector (b, e);
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector unordered_multiset_to_r_i(Rcpp::XPtr<std::unordered_multiset<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return unordered_multiset_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector unordered_multiset_to_r_d(Rcpp::XPtr<std::unordered_multiset<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return unordered_multiset_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector unordered_multiset_to_r_s(Rcpp::XPtr<std::unordered_multiset<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return unordered_multiset_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector unordered_multiset_to_r_b(Rcpp::XPtr<std::unordered_multiset<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return unordered_multiset_to_r(x, n, t);
}

// map
template <typename KM, typename VM, typename KD, typename VD> // KM: keys (map), VM: values (map), KD: keys (data frame), VD: values (data frame)
Rcpp::DataFrame map_to_r(Rcpp::XPtr<std::map<KM, VM> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const KM from,
  const bool use_to, const KM to, KD& kt, VD& vt) { // kt and vt tell the compiler the vector types, but are otherwise not utilized
  if(!use_from && !use_to) {
    const std::size_t x_size = x->size();
    if(!use_n || n > x_size) {
      n = x_size;
    }
    if(reverse) {
      auto j = x->rbegin();
      KD k (n);
      VD v (n);
      for(std::size_t i = 0; i != n; ++i) {
        k[i] = j->first;
        v[i] = j->second;
        ++j;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    } else {
      auto j = x->begin();
      KD k (n);
      VD v (n);
      for(std::size_t i = 0; i != n; ++i) {
        k[i] = j->first;
        v[i] = j->second;
        ++j;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (b and e)
      typename std::map<KM, VM>::iterator b;
      if(use_from) {
        b = x->lower_bound(from);
        if(b == x->end()) {
          from_gt_maximum(from);
        }
      } else {
        b = x->begin();
      }
      typename std::map<KM, VM>::iterator e;
      if(use_to) {
        e = x->upper_bound(to);
      } else {
        e = x->end();
      }
      n = std::distance(b, e);
      KD k (n);
      VD v (n);
      std::size_t i = 0;
      for(auto j = b; j != e; ++j) {
        k[i] = j->first;
        v[i] = j->second;
        ++i;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_i_i(Rcpp::XPtr<std::map<int, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_i_d(Rcpp::XPtr<std::map<int, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::NumericVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_i_s(Rcpp::XPtr<std::map<int, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::CharacterVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_i_b(Rcpp::XPtr<std::map<int, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const int from,
  const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::LogicalVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_d_i(Rcpp::XPtr<std::map<double, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::IntegerVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_d_d(Rcpp::XPtr<std::map<double, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_d_s(Rcpp::XPtr<std::map<double, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::CharacterVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_d_b(Rcpp::XPtr<std::map<double, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::LogicalVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_s_i(Rcpp::XPtr<std::map<std::string, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::IntegerVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_s_d(Rcpp::XPtr<std::map<std::string, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::NumericVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_s_s(Rcpp::XPtr<std::map<std::string, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_s_b(Rcpp::XPtr<std::map<std::string, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::LogicalVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_b_i(Rcpp::XPtr<std::map<bool, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::IntegerVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_b_d(Rcpp::XPtr<std::map<bool, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::NumericVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_b_s(Rcpp::XPtr<std::map<bool, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::CharacterVector vt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame map_to_r_b_b(Rcpp::XPtr<std::map<bool, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const bool from,
  const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  return map_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}

// unordered_map
template <typename KM, typename VM, typename KD, typename VD> // KM: keys (map), VM: values (map), KD: keys (data frame), VD: values (data frame)
Rcpp::DataFrame unordered_map_to_r(Rcpp::XPtr<std::unordered_map<KM, VM> > x, std::size_t n, KD& kt, VD& vt) {
  // kt and vt tell the compiler the vector types, but are otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  KD k (n);
  VD v (n);
  for(std::size_t i = 0; i != n; ++i) {
    k[i] = j->first;
    v[i] = j->second;
    ++j;
  }
  return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_i_i(Rcpp::XPtr<std::unordered_map<int, int> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  return unordered_map_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_i_d(Rcpp::XPtr<std::unordered_map<int, double> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::NumericVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_i_s(Rcpp::XPtr<std::unordered_map<int, std::string> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::CharacterVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_i_b(Rcpp::XPtr<std::unordered_map<int, bool> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::LogicalVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_d_i(Rcpp::XPtr<std::unordered_map<double, int> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::IntegerVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_d_d(Rcpp::XPtr<std::unordered_map<double, double> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  return unordered_map_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_d_s(Rcpp::XPtr<std::unordered_map<double, std::string> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::CharacterVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_d_b(Rcpp::XPtr<std::unordered_map<double, bool> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::LogicalVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_s_i(Rcpp::XPtr<std::unordered_map<std::string, int> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::IntegerVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_s_d(Rcpp::XPtr<std::unordered_map<std::string, double> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::NumericVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_s_s(Rcpp::XPtr<std::unordered_map<std::string, std::string> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  return unordered_map_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_s_b(Rcpp::XPtr<std::unordered_map<std::string, bool> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::LogicalVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_b_i(Rcpp::XPtr<std::unordered_map<bool, int> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::IntegerVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_b_d(Rcpp::XPtr<std::unordered_map<bool, double> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::NumericVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_b_s(Rcpp::XPtr<std::unordered_map<bool, std::string> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::CharacterVector vt;
  return unordered_map_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_map_to_r_b_b(Rcpp::XPtr<std::unordered_map<bool, bool> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  return unordered_map_to_r(x, n, kt, kt);
}

// multimap
template <typename KM, typename VM, typename KD, typename VD> // KM: keys (map), VM: values (map), KD: keys (data frame), VD: values (data frame)
Rcpp::DataFrame multimap_to_r(Rcpp::XPtr<std::multimap<KM, VM> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, const KM from,
  const bool use_to, const KM to, KD& kt, VD& vt) { // kt and vt tell the compiler the vector types, but are otherwise not utilized
  if(!use_from && !use_to) {
    const std::size_t x_size = x->size();
    if(!use_n || n > x_size) {
      n = x_size;
    }
    if(reverse) {
      auto j = x->rbegin();
      KD k (n);
      VD v (n);
      for(std::size_t i = 0; i != n; ++i) {
        k[i] = j->first;
        v[i] = j->second;
        ++j;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    } else {
      auto j = x->begin();
      KD k (n);
      VD v (n);
      for(std::size_t i = 0; i != n; ++i) {
        k[i] = j->first;
        v[i] = j->second;
        ++j;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    }
  } else {
    if(use_from != use_to || from <= to) {
      // identify from and to iterators (b and e)
      typename std::multimap<KM, VM>::iterator b;
      if(use_from) {
        b = x->lower_bound(from);
        if(b == x->end()) {
          from_gt_maximum(from);
        }
      } else {
        b = x->begin();
      }
      typename std::multimap<KM, VM>::iterator e;
      if(use_to) {
        e = x->upper_bound(to);
      } else {
        e = x->end();
      }
      n = std::distance(b, e);
      KD k (n);
      VD v (n);
      std::size_t i = 0;
      for(auto j = b; j != e; ++j) {
        k[i] = j->first;
        v[i] = j->second;
        ++i;
      }
      return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
    } else {
      Rcpp::stop("from must be smaller than or equal to to.");
    }
  }
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_i_i(Rcpp::XPtr<std::multimap<int, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_i_d(Rcpp::XPtr<std::multimap<int, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::NumericVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_i_s(Rcpp::XPtr<std::multimap<int, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::CharacterVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_i_b(Rcpp::XPtr<std::multimap<int, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const int from, const bool use_to, const int to) {
  Rcpp::IntegerVector kt;
  Rcpp::LogicalVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_d_i(Rcpp::XPtr<std::multimap<double, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::IntegerVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_d_d(Rcpp::XPtr<std::multimap<double, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_d_s(Rcpp::XPtr<std::multimap<double, std::string> > x, const bool use_n, std::size_t n, const bool reverse,
  const bool use_from, const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::CharacterVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_d_b(Rcpp::XPtr<std::multimap<double, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const double from, const bool use_to, const double to) {
  Rcpp::NumericVector kt;
  Rcpp::LogicalVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_s_i(Rcpp::XPtr<std::multimap<std::string, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::IntegerVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_s_d(Rcpp::XPtr<std::multimap<std::string, double> > x, const bool use_n, std::size_t n, const bool reverse,
  const bool use_from, const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::NumericVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_s_s(Rcpp::XPtr<std::multimap<std::string, std::string> > x, const bool use_n, std::size_t n, const bool reverse,
  const bool use_from, const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_s_b(Rcpp::XPtr<std::multimap<std::string, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const std::string from, const bool use_to, const std::string to) {
  Rcpp::CharacterVector kt;
  Rcpp::LogicalVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_b_i(Rcpp::XPtr<std::multimap<bool, int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::IntegerVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_b_d(Rcpp::XPtr<std::multimap<bool, double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::NumericVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_b_s(Rcpp::XPtr<std::multimap<bool, std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  Rcpp::CharacterVector vt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame multimap_to_r_b_b(Rcpp::XPtr<std::multimap<bool, bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  const bool from, const bool use_to, const bool to) {
  Rcpp::LogicalVector kt;
  return multimap_to_r(x, use_n, n, reverse, use_from, from, use_to, to, kt, kt);
}

// unordered_multimap
template <typename KM, typename VM, typename KD, typename VD> // KM: keys (map), VM: values (map), KD: keys (data frame), VD: values (data frame)
Rcpp::DataFrame unordered_multimap_to_r(Rcpp::XPtr<std::unordered_multimap<KM, VM> > x, std::size_t n, KD& kt, VD& vt) {
  // kt and vt tell the compiler the vector types, but are otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  auto j = x->begin();
  KD k (n);
  VD v (n);
  for(std::size_t i = 0; i != n; ++i) {
    k[i] = j->first;
    v[i] = j->second;
    ++j;
  }
  return Rcpp::DataFrame::create(Rcpp::Named("key") = k, Rcpp::Named("value") = v);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_i_i(Rcpp::XPtr<std::unordered_multimap<int, int> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  return unordered_multimap_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_i_d(Rcpp::XPtr<std::unordered_multimap<int, double> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::NumericVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_i_s(Rcpp::XPtr<std::unordered_multimap<int, std::string> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::CharacterVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_i_b(Rcpp::XPtr<std::unordered_multimap<int, bool> > x, std::size_t n) {
  Rcpp::IntegerVector kt;
  Rcpp::LogicalVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_d_i(Rcpp::XPtr<std::unordered_multimap<double, int> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::IntegerVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_d_d(Rcpp::XPtr<std::unordered_multimap<double, double> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  return unordered_multimap_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_d_s(Rcpp::XPtr<std::unordered_multimap<double, std::string> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::CharacterVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_d_b(Rcpp::XPtr<std::unordered_multimap<double, bool> > x, std::size_t n) {
  Rcpp::NumericVector kt;
  Rcpp::LogicalVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_s_i(Rcpp::XPtr<std::unordered_multimap<std::string, int> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::IntegerVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_s_d(Rcpp::XPtr<std::unordered_multimap<std::string, double> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::NumericVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_s_s(Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  return unordered_multimap_to_r(x, n, kt, kt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_s_b(Rcpp::XPtr<std::unordered_multimap<std::string, bool> > x, std::size_t n) {
  Rcpp::CharacterVector kt;
  Rcpp::LogicalVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_b_i(Rcpp::XPtr<std::unordered_multimap<bool, int> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::IntegerVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_b_d(Rcpp::XPtr<std::unordered_multimap<bool, double> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::NumericVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_b_s(Rcpp::XPtr<std::unordered_multimap<bool, std::string> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  Rcpp::CharacterVector vt;
  return unordered_multimap_to_r(x, n, kt, vt);
}
// [[Rcpp::export]]
Rcpp::DataFrame unordered_multimap_to_r_b_b(Rcpp::XPtr<std::unordered_multimap<bool, bool> > x, std::size_t n) {
  Rcpp::LogicalVector kt;
  return unordered_multimap_to_r(x, n, kt, kt);
}

// stack
template <typename C, typename R> // C: stack data type, R: R vector
R stack_to_r(Rcpp::XPtr<std::stack<C> > x, std::size_t n, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  R rvector (n);
  for(std::size_t i = 0; i != n; ++i) {
    rvector[i] = x->top();
    x->pop();
  }
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector stack_to_r_i(Rcpp::XPtr<std::stack<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return stack_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector stack_to_r_d(Rcpp::XPtr<std::stack<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return stack_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector stack_to_r_s(Rcpp::XPtr<std::stack<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return stack_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector stack_to_r_b(Rcpp::XPtr<std::stack<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return stack_to_r(x, n, t);
}

// queue
template <typename C, typename R> // C: queue data type, R: R vector
R queue_to_r(Rcpp::XPtr<std::queue<C> > x, std::size_t n, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  R rvector (n);
  for(std::size_t i = 0; i != n; ++i) {
    rvector[i] = x->front();
    x->pop();
  }
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector queue_to_r_i(Rcpp::XPtr<std::queue<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return queue_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector queue_to_r_d(Rcpp::XPtr<std::queue<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return queue_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector queue_to_r_s(Rcpp::XPtr<std::queue<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return queue_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector queue_to_r_b(Rcpp::XPtr<std::queue<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return queue_to_r(x, n, t);
}

// priority_queue
template <typename C, typename R> // C: priority queue data type, R: R vector
R priority_queue_to_r_d(Rcpp::XPtr<std::priority_queue<C> > x, std::size_t n, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  R rvector (n);
  for(std::size_t i = 0; i != n; ++i) {
    rvector[i] = x->top();
    x->pop();
  }
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector priority_queue_to_r_i_d(Rcpp::XPtr<std::priority_queue<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return priority_queue_to_r_d(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector priority_queue_to_r_d_d(Rcpp::XPtr<std::priority_queue<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return priority_queue_to_r_d(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector priority_queue_to_r_s_d(Rcpp::XPtr<std::priority_queue<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return priority_queue_to_r_d(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector priority_queue_to_r_b_d(Rcpp::XPtr<std::priority_queue<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return priority_queue_to_r_d(x, n, t);
}
template <typename C, typename R> // C: priority queue data type, R: R vector
R priority_queue_to_r_a(Rcpp::XPtr<std::priority_queue<C, std::vector<C>, std::greater<C> > > x, std::size_t n, R& t) {
  // t tells the compiler the vector type, but is otherwise not utilized
  const std::size_t x_size = x->size();
  if(n > x_size || n == 0) {
    n = x_size;
  }
  R rvector (n);
  for(std::size_t i = 0; i != n; ++i) {
    rvector[i] = x->top();
    x->pop();
  }
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector priority_queue_to_r_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return priority_queue_to_r_a(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector priority_queue_to_r_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > x, std::size_t n) {
  Rcpp::NumericVector t;
  return priority_queue_to_r_a(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector priority_queue_to_r_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > x,
  std::size_t n) {
  Rcpp::CharacterVector t;
  return priority_queue_to_r_a(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector priority_queue_to_r_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return priority_queue_to_r_a(x, n, t);
}

// vector
template <typename C, typename R> // C: vector data type, R: R vector
R vector_to_r(Rcpp::XPtr<std::vector<C> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from, const bool use_to,
  std::size_t to, R& t) {
  if(!use_n && !use_from && !use_to) {
    return Rcpp::wrap(*x);
  }
  const std::size_t x_size = x->size();
  if(use_n) {
    if(n > x_size) {
      to = x_size;
    } else {
      to = n;
    }
    from = 0;
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
    // initialize from the back
    R rvector (x->rbegin() + from, x->rbegin() + to);
    return rvector;
  } else {
    // initialize from the front
    R rvector (x->begin() + from, x->begin() + to);
    return rvector;
  }
}
// [[Rcpp::export]]
Rcpp::IntegerVector vector_to_r_i(Rcpp::XPtr<std::vector<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::IntegerVector t;
  return vector_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector vector_to_r_d(Rcpp::XPtr<std::vector<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::NumericVector t;
  return vector_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector vector_to_r_s(Rcpp::XPtr<std::vector<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::CharacterVector t;
  return vector_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector vector_to_r_b(Rcpp::XPtr<std::vector<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::LogicalVector t;
  return vector_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}

// deque
template <typename C, typename R> // C: deque data type, R: R vector
R deque_to_r(Rcpp::XPtr<std::deque<C> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from, std::size_t from, const bool use_to,
  std::size_t to, R& t) {
  if(!use_n && !use_from && !use_to) {
    return Rcpp::wrap(*x);
  }
  const std::size_t x_size = x->size();
  if(use_n) {
    if(n > x_size) {
      to = x_size;
    } else {
      to = n;
    }
    from = 0;
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
    // initialize from the back
    R rvector (x->rbegin() + from, x->rbegin() + to);
    return rvector;
  } else {
    // initialize from the front
    R rvector (x->begin() + from, x->begin() + to);
    return rvector;
  }
}
// [[Rcpp::export]]
Rcpp::IntegerVector deque_to_r_i(Rcpp::XPtr<std::deque<int> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::IntegerVector t;
  return deque_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector deque_to_r_d(Rcpp::XPtr<std::deque<double> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::NumericVector t;
  return deque_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector deque_to_r_s(Rcpp::XPtr<std::deque<std::string> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::CharacterVector t;
  return deque_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector deque_to_r_b(Rcpp::XPtr<std::deque<bool> > x, const bool use_n, std::size_t n, const bool reverse, const bool use_from,
  std::size_t from, const bool use_to, std::size_t to) {
  Rcpp::LogicalVector t;
  return deque_to_r(x, use_n, n, reverse, use_from, from, use_to, to, t);
}

// forward_list
template <typename C, typename R> // C: forward list data type, R: R vector
R forward_list_to_r(Rcpp::XPtr<std::forward_list<C> > x, std::size_t n, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  if(n == 0) {
    return Rcpp::wrap(*x);
  }
  const auto b = x->begin();
  auto j = b;
  const auto e = x->end();
  std::size_t i = 0;
  while(i < n && j != e) {
    ++j;
    ++i;
  }
  R rvector (b, j);
  return rvector;
}
// [[Rcpp::export]]
Rcpp::IntegerVector forward_list_to_r_i(Rcpp::XPtr<std::forward_list<int> > x, std::size_t n) {
  Rcpp::IntegerVector t;
  return forward_list_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector forward_list_to_r_d(Rcpp::XPtr<std::forward_list<double> > x, std::size_t n) {
  Rcpp::NumericVector t;
  return forward_list_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector forward_list_to_r_s(Rcpp::XPtr<std::forward_list<std::string> > x, std::size_t n) {
  Rcpp::CharacterVector t;
  return forward_list_to_r(x, n, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector forward_list_to_r_b(Rcpp::XPtr<std::forward_list<bool> > x, std::size_t n) {
  Rcpp::LogicalVector t;
  return forward_list_to_r(x, n, t);
}

// list
template <typename C, typename R> // C: list data type, R: R vector
R list_to_r(Rcpp::XPtr<std::list<C> > x, std::size_t n, const bool reverse, R& t) { // t tells the compiler the vector type, but is otherwise not utilized
  if(n == 0) {
    return Rcpp::wrap(*x);
  }
  const std::size_t x_size = x->size();
  if(n > x_size) {
    n = x_size;
  }
  if(reverse) {
    // initialize from the back
    R rvector (x->rbegin(), std::next(x->rbegin(), n));
    return rvector;
  } else {
    // initialize from the front
    R rvector (x->begin(), std::next(x->begin(), n));
    return rvector;
  }
}
// [[Rcpp::export]]
Rcpp::IntegerVector list_to_r_i(Rcpp::XPtr<std::list<int> > x, std::size_t n, const bool reverse) {
  Rcpp::IntegerVector t;
  return list_to_r(x, n, reverse, t);
}
// [[Rcpp::export]]
Rcpp::NumericVector list_to_r_d(Rcpp::XPtr<std::list<double> > x, std::size_t n, const bool reverse) {
  Rcpp::NumericVector t;
  return list_to_r(x, n, reverse, t);
}
// [[Rcpp::export]]
Rcpp::CharacterVector list_to_r_s(Rcpp::XPtr<std::list<std::string> > x, std::size_t n, const bool reverse) {
  Rcpp::CharacterVector t;
  return list_to_r(x, n, reverse, t);
}
// [[Rcpp::export]]
Rcpp::LogicalVector list_to_r_b(Rcpp::XPtr<std::list<bool> > x, std::size_t n, const bool reverse) {
  Rcpp::LogicalVector t;
  return list_to_r(x, n, reverse, t);
}
