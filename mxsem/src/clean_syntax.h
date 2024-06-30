#ifndef CLEAN_SYNTAX_H
#define CLEAN_SYNTAX_H
#include <Rcpp.h>

std::vector<std::string> clean_syntax(const std::string& syntax);

void check_cleaned(const std::vector<std::string> cleaned_syntax);

#endif
