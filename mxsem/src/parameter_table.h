#ifndef PARAMETER_TABLE_H
#define PARAMETER_TABLE_H
#include <Rcpp.h>

struct algebra{
  std::vector<std::string> new_parameters;
  std::vector<std::string> new_parameters_free;
  std::vector<std::string> lhs, op, rhs;
};

struct variables{
  std::vector<std::string> manifests;
  std::vector<std::string> latents;
};

class parameter_table{
public:
  std::vector<std::string> lhs, op, rhs, modifier, lbound, ubound, free;
  std::vector<std::string> user_defined;
  algebra alg;
  variables vars;

  void add_line(){
    lhs.push_back("");
    op.push_back("");
    rhs.push_back("");
    modifier.push_back("");
    lbound.push_back("");
    ubound.push_back("");
    free.push_back("TRUE");
  }
};

#endif
