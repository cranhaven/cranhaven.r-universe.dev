#ifndef CREATE_ALGEBRAS_H
#define CREATE_ALGEBRAS_H
#include "parameter_table.h"
#include <Rcpp.h>

void make_algebras(const std::vector<std::string>& equations,
                   parameter_table& pt);

#endif
