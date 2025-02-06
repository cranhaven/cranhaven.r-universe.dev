// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <cstddef>

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<int, int> > unordered_multimap_i_i(Rcpp::IntegerVector& keys, Rcpp::IntegerVector& values) {
  std::unordered_multimap<int, int>* m = new std::unordered_multimap<int, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<int, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<int, double> > unordered_multimap_i_d(Rcpp::IntegerVector& keys, Rcpp::NumericVector& values) {
  std::unordered_multimap<int, double>* m = new std::unordered_multimap<int, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<int, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<int, std::string> > unordered_multimap_i_s(Rcpp::IntegerVector& keys, Rcpp::CharacterVector& values) {
  std::unordered_multimap<int, std::string>* m = new std::unordered_multimap<int, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<int, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<int, bool> > unordered_multimap_i_b(Rcpp::IntegerVector& keys, Rcpp::LogicalVector& values) {
  std::unordered_multimap<int, bool>* m = new std::unordered_multimap<int, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<int, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<double, int> > unordered_multimap_d_i(Rcpp::NumericVector& keys, Rcpp::IntegerVector& values) {
  std::unordered_multimap<double, int>* m = new std::unordered_multimap<double, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<double, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<double, double> > unordered_multimap_d_d(Rcpp::NumericVector& keys, Rcpp::NumericVector& values) {
  std::unordered_multimap<double, double>* m = new std::unordered_multimap<double, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<double, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<double, std::string> > unordered_multimap_d_s(Rcpp::NumericVector& keys, Rcpp::CharacterVector& values) {
  std::unordered_multimap<double, std::string>* m = new std::unordered_multimap<double, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<double, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<double, bool> > unordered_multimap_d_b(Rcpp::NumericVector& keys, Rcpp::LogicalVector& values) {
  std::unordered_multimap<double, bool>* m = new std::unordered_multimap<double, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<double, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<std::string, int> > unordered_multimap_s_i(Rcpp::CharacterVector& keys, Rcpp::IntegerVector& values) {
  std::unordered_multimap<std::string, int>* m = new std::unordered_multimap<std::string, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<std::string, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<std::string, double> > unordered_multimap_s_d(Rcpp::CharacterVector& keys, Rcpp::NumericVector& values) {
  std::unordered_multimap<std::string, double>* m = new std::unordered_multimap<std::string, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<std::string, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > unordered_multimap_s_s(Rcpp::CharacterVector& keys, Rcpp::CharacterVector& values) {
  std::unordered_multimap<std::string, std::string>* m = new std::unordered_multimap<std::string, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<std::string, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<std::string, bool> > unordered_multimap_s_b(Rcpp::CharacterVector& keys, Rcpp::LogicalVector& values) {
  std::unordered_multimap<std::string, bool>* m = new std::unordered_multimap<std::string, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<std::string, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<bool, int> > unordered_multimap_b_i(Rcpp::LogicalVector& keys, Rcpp::IntegerVector& values) {
  std::unordered_multimap<bool, int>* m = new std::unordered_multimap<bool, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<bool, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<bool, double> > unordered_multimap_b_d(Rcpp::LogicalVector& keys, Rcpp::NumericVector& values) {
  std::unordered_multimap<bool, double>* m = new std::unordered_multimap<bool, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<bool, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<bool, std::string> > unordered_multimap_b_s(Rcpp::LogicalVector& keys, Rcpp::CharacterVector& values) {
  std::unordered_multimap<bool, std::string>* m = new std::unordered_multimap<bool, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<bool, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::unordered_multimap<bool, bool> > unordered_multimap_b_b(Rcpp::LogicalVector& keys, Rcpp::LogicalVector& values) {
  std::unordered_multimap<bool, bool>* m = new std::unordered_multimap<bool, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::unordered_multimap<bool, bool> > p(m);
  return p;
}
