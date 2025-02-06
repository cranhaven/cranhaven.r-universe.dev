// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <map>
#include <string>
#include <cstddef>

// [[Rcpp::export]]
Rcpp::XPtr<std::map<int, int> > map_i_i(Rcpp::IntegerVector& keys, Rcpp::IntegerVector& values) {
  std::map<int, int>* m = new std::map<int, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<int, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<int, double> > map_i_d(Rcpp::IntegerVector& keys, Rcpp::NumericVector& values) {
  std::map<int, double>* m = new std::map<int, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<int, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<int, std::string> > map_i_s(Rcpp::IntegerVector& keys, Rcpp::CharacterVector& values) {
  std::map<int, std::string>* m = new std::map<int, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<int, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<int, bool> > map_i_b(Rcpp::IntegerVector& keys, Rcpp::LogicalVector& values) {
  std::map<int, bool>* m = new std::map<int, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<int, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::map<double, int> > map_d_i(Rcpp::NumericVector& keys, Rcpp::IntegerVector& values) {
  std::map<double, int>* m = new std::map<double, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<double, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<double, double> > map_d_d(Rcpp::NumericVector& keys, Rcpp::NumericVector& values) {
  std::map<double, double>* m = new std::map<double, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<double, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<double, std::string> > map_d_s(Rcpp::NumericVector& keys, Rcpp::CharacterVector& values) {
  std::map<double, std::string>* m = new std::map<double, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<double, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<double, bool> > map_d_b(Rcpp::NumericVector& keys, Rcpp::LogicalVector& values) {
  std::map<double, bool>* m = new std::map<double, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<double, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::map<std::string, int> > map_s_i(Rcpp::CharacterVector& keys, Rcpp::IntegerVector& values) {
  std::map<std::string, int>* m = new std::map<std::string, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::map<std::string, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<std::string, double> > map_s_d(Rcpp::CharacterVector& keys, Rcpp::NumericVector& values) {
  std::map<std::string, double>* m = new std::map<std::string, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::map<std::string, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<std::string, std::string> > map_s_s(Rcpp::CharacterVector& keys, Rcpp::CharacterVector& values) {
  std::map<std::string, std::string>* m = new std::map<std::string, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::map<std::string, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<std::string, bool> > map_s_b(Rcpp::CharacterVector& keys, Rcpp::LogicalVector& values) {
  std::map<std::string, bool>* m = new std::map<std::string, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    m->emplace(keys[i], values[i]);
  }
  Rcpp::XPtr<std::map<std::string, bool> > p(m);
  return p;
}

// [[Rcpp::export]]
Rcpp::XPtr<std::map<bool, int> > map_b_i(Rcpp::LogicalVector& keys, Rcpp::IntegerVector& values) {
  std::map<bool, int>* m = new std::map<bool, int>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<bool, int> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<bool, double> > map_b_d(Rcpp::LogicalVector& keys, Rcpp::NumericVector& values) {
  std::map<bool, double>* m = new std::map<bool, double>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<bool, double> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<bool, std::string> > map_b_s(Rcpp::LogicalVector& keys, Rcpp::CharacterVector& values) {
  std::map<bool, std::string>* m = new std::map<bool, std::string>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<bool, std::string> > p(m);
  return p;
}
// [[Rcpp::export]]
Rcpp::XPtr<std::map<bool, bool> > map_b_b(Rcpp::LogicalVector& keys, Rcpp::LogicalVector& values) {
  std::map<bool, bool>* m = new std::map<bool, bool>;
  const std::size_t v_size = keys.size();
  for(std::size_t i = 0; i != v_size; ++i) {
    (*m)[keys[i]] = values[i];
  }
  Rcpp::XPtr<std::map<bool, bool> > p(m);
  return p;
}
