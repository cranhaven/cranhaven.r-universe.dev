// [[Rcpp::plugins(cpp20)]]
#include <Rcpp.h>
#include <stack>
#include <queue>
#include <string>
#include <vector>

// stack
// [[Rcpp::export]]
void stack_push_i(Rcpp::XPtr<std::stack<int> > x, Rcpp::IntegerVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void stack_push_d(Rcpp::XPtr<std::stack<double> > x, Rcpp::NumericVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void stack_push_s(Rcpp::XPtr<std::stack<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  for(auto& i : c) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void stack_push_b(Rcpp::XPtr<std::stack<bool> > x, Rcpp::LogicalVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}

// queue
// [[Rcpp::export]]
void queue_push_i(Rcpp::XPtr<std::queue<int> > x, Rcpp::IntegerVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void queue_push_d(Rcpp::XPtr<std::queue<double> > x, Rcpp::NumericVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void queue_push_s(Rcpp::XPtr<std::queue<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  for(auto& i : c) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void queue_push_b(Rcpp::XPtr<std::queue<bool> > x, Rcpp::LogicalVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}

// priority_queue
// [[Rcpp::export]]
void priority_queue_push_i_d(Rcpp::XPtr<std::priority_queue<int> > x, Rcpp::IntegerVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_d_d(Rcpp::XPtr<std::priority_queue<double> > x, Rcpp::NumericVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_s_d(Rcpp::XPtr<std::priority_queue<std::string> > x, Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  for(auto& i : c) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_b_d(Rcpp::XPtr<std::priority_queue<bool> > x, Rcpp::LogicalVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_i_a(Rcpp::XPtr<std::priority_queue<int, std::vector<int>, std::greater<int> > > x, Rcpp::IntegerVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_d_a(Rcpp::XPtr<std::priority_queue<double, std::vector<double>, std::greater<double> > > x, Rcpp::NumericVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_s_a(Rcpp::XPtr<std::priority_queue<std::string, std::vector<std::string>, std::greater<std::string> > > x,
  Rcpp::CharacterVector& v) {
  const std::vector<std::string> c = Rcpp::as<std::vector<std::string> >(v);
  for(auto& i : c) {
    x->push(i);
  }
}
// [[Rcpp::export]]
void priority_queue_push_b_a(Rcpp::XPtr<std::priority_queue<bool, std::vector<bool>, std::greater<bool> > > x, Rcpp::LogicalVector& v) {
  for(auto& i : v) {
    x->push(i);
  }
}
