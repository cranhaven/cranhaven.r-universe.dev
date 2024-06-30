#ifndef STR_OPERATIONS_H
#define STR_OPERATIONS_H
#include <Rcpp.h>

struct equation_elements{
  std::string lhs{""};
  std::string separator{""};
  std::string rhs{""};
};

struct str_rhs_elem{
  std::string rhs = "";
  std::string modifier = "";
};

std::vector<str_rhs_elem> split_eqation_rhs(std::string rhs);

equation_elements split_string_once(const std::string& str, const std::string& at);

std::vector<std::string> split_string_all(const std::string& str, const char at);

void check_lhs(const std::string& lhs, const std::string not_allowed = "!+*=~:");

bool char_in_string(const char c, const std::string& str);

bool is_number(const std::string& str);

#endif
