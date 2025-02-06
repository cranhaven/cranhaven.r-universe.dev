// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <map>
#include <string>
#include <cstddef>

// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<int, int> > multimap_i_i(Rcpp::IntegerVector& keys, Rcpp::IntegerVector& values) {
  std::multimap<int, int>* m = new std::multimap<int, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<int, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<int, double> > multimap_i_d(Rcpp::IntegerVector& keys, Rcpp::NumericVector& values) {
  std::multimap<int, double>* m = new std::multimap<int, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<int, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<int, std::string> > multimap_i_s(Rcpp::IntegerVector& keys, Rcpp::CharacterVector& values) {
  std::multimap<int, std::string>* m = new std::multimap<int, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<int, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<int, bool> > multimap_i_b(Rcpp::IntegerVector& keys, Rcpp::LogicalVector& values) {
  std::multimap<int, bool>* m = new std::multimap<int, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<int, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<double, int> > multimap_d_i(Rcpp::NumericVector& keys, Rcpp::IntegerVector& values) {
  std::multimap<double, int>* m = new std::multimap<double, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<double, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<double, double> > multimap_d_d(Rcpp::NumericVector& keys, Rcpp::NumericVector& values) {
  std::multimap<double, double>* m = new std::multimap<double, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<double, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<double, std::string> > multimap_d_s(Rcpp::NumericVector& keys, Rcpp::CharacterVector& values) {
  std::multimap<double, std::string>* m = new std::multimap<double, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<double, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<double, bool> > multimap_d_b(Rcpp::NumericVector& keys, Rcpp::LogicalVector& values) {
  std::multimap<double, bool>* m = new std::multimap<double, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<double, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<std::string, int> > multimap_s_i(Rcpp::CharacterVector& keys, Rcpp::IntegerVector& values) {
  std::multimap<std::string, int>* m = new std::multimap<std::string, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<std::string, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<std::string, double> > multimap_s_d(Rcpp::CharacterVector& keys, Rcpp::NumericVector& values) {
  std::multimap<std::string, double>* m = new std::multimap<std::string, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<std::string, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<std::string, std::string> > multimap_s_s(Rcpp::CharacterVector& keys, Rcpp::CharacterVector& values) {
  std::multimap<std::string, std::string>* m = new std::multimap<std::string, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<std::string, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<std::string, bool> > multimap_s_b(Rcpp::CharacterVector& keys, Rcpp::LogicalVector& values) {
  std::multimap<std::string, bool>* m = new std::multimap<std::string, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<std::string, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<bool, int> > multimap_b_i(Rcpp::LogicalVector& keys, Rcpp::IntegerVector& values) {
  std::multimap<bool, int>* m = new std::multimap<bool, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<bool, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<bool, double> > multimap_b_d(Rcpp::LogicalVector& keys, Rcpp::NumericVector& values) {
  std::multimap<bool, double>* m = new std::multimap<bool, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<bool, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<bool, std::string> > multimap_b_s(Rcpp::LogicalVector& keys, Rcpp::CharacterVector& values) {
  std::multimap<bool, std::string>* m = new std::multimap<bool, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<bool, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::multimap<bool, bool> > multimap_b_b(Rcpp::LogicalVector& keys, Rcpp::LogicalVector& values) {
  std::multimap<bool, bool>* m = new std::multimap<bool, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::multimap<bool, bool> > p(m);
  return p;
}
