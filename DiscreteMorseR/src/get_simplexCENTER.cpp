#include <Rcpp.h>
#include <vector>
#include <sstream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get_simplexCENTER_cpp(CharacterVector simplex, NumericMatrix vertices) {
  std::string simplex_str = as<std::string>(simplex[0]);
  std::vector<int> vert_ids;
  std::istringstream ss(simplex_str);
  std::string token;
  
  while (ss >> token) {
    vert_ids.push_back(std::stoi(token));
  }
  
  double sum_x = 0, sum_y = 0, sum_z = 0;
  int count = 0;
  
  for (int id : vert_ids) {
    int idx = id - 1;
    if (idx >= 0 && idx < vertices.nrow()) {
      sum_x += vertices(idx, 0);
      sum_y += vertices(idx, 1);
      sum_z += vertices(idx, 2);
      count++;
    }
  }
  
  if (count > 0) {
    return NumericVector::create(sum_x/count, sum_y/count, sum_z/count);
  } else {
    return NumericVector::create(NA_REAL, NA_REAL, NA_REAL);
  }
}
