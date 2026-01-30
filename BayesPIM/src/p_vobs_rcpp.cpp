#include <Rcpp.h>
using namespace Rcpp;

// Utility function to mimic geom function
double geom(int j, double kappa) {
  return kappa * std::pow(1 - kappa, j - 1);
}

// Utility function to mimic geom.inf function
double geom_inf(int j, double kappa) {
  return std::pow(1 - kappa, j - 1);
}

// [[Rcpp::export]]
List P_vobs_Rcpp(List Vobs, double kappa) {
  int n = Vobs.size();
  IntegerVector m(n);
  LogicalVector cens(n);
  int m_max = 0;
  
  for (int i = 0; i < n; ++i) {
    NumericVector currentVec = Vobs[i];
    m[i] = currentVec.size() - 1;
    cens[i] = std::isinf(currentVec[currentVec.size() - 1]);
    if (m[i] > m_max) {
      m_max = m[i];
    }
  }
  
  NumericVector p_vec = NumericVector(m_max);
  NumericVector p_vec_inf = NumericVector(m_max);
  
  for (int j = 1; j <= m_max; ++j) {
    p_vec[j - 1] = geom(j, kappa);
    p_vec_inf[j - 1] = geom_inf(j, kappa);
  }
  
  List P(n);
  
  for (int i = 0; i < n; ++i) {
    NumericVector temp(m[i]);
    
    if (!cens[i]) {
      for (int j = 0; j < m[i]; ++j) {
        temp[j] = p_vec[m[i] - j - 1];
      }
    } else {
      for (int j = 0; j < m[i]; ++j) {
        temp[j] = p_vec_inf[m[i] - j - 1];
      }
    }
    
    P[i] = temp;
  }
  
  return P;
}
