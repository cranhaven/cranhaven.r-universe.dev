/* Based on original code by Robin Hankin */

#include "qspray.h"

using namespace QSPRAY;

// -------------------------------------------------------------------------- //
Qspray<gmpq> prepare(const Rcpp::List&         Powers, 
                     const Rcpp::StringVector& coeffs) {
  Polynomial<gmpq> S;
  for(signed int i = 0; i < Powers.size(); i++) {
    Rcpp::IntegerVector Exponents = Powers(i);
    gmpq coeff(Rcpp::as<std::string>(coeffs(i)));
    if(coeff != 0) {
      powers pows(Exponents.begin(), Exponents.end());
      QSPRAY::utils::simplifyPowers(pows);
      S[pows] += coeff;
    }
  }
  // Now remove zero entries:
  Polynomial<gmpq>::iterator it = S.begin();
  while(it != S.end()) {
    if(it->second == 0) {
      it = S.erase(it);  //  in C++11, erase() returns *next* iterator
    } else {
      ++it;  // else just increment the iterator
    }
  }
  
  return Qspray<gmpq>(S);
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_maker(const Rcpp::List&         Powers,
                        const Rcpp::StringVector& coeffs) {
  return returnQspray(prepare(Powers, coeffs));
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_deriv(const Rcpp::List&          Powers, 
                        const Rcpp::StringVector&  coeffs,   
                        const Rcpp::IntegerVector& n     ){
  Qspray<gmpq> Q = makeQspray(Powers, coeffs);
  std::vector<unsigned int> orders(n.begin(), n.end());
  Qspray<gmpq> Qprime = Q.deriv(orders);
  return returnQspray(Qprime);
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_add(const Rcpp::List&         Powers1,
                      const Rcpp::StringVector& coeffs1,
                      const Rcpp::List&         Powers2,
                      const Rcpp::StringVector& coeffs2) {
  Qspray<gmpq> Q1 = makeQspray(Powers1, coeffs1);
  Qspray<gmpq> Q2 = makeQspray(Powers2, coeffs2);  
  return returnQspray(Q1 + Q2);
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_subtract(const Rcpp::List&         Powers1,
                           const Rcpp::StringVector& coeffs1,
                           const Rcpp::List&         Powers2,
                           const Rcpp::StringVector& coeffs2) {
  Qspray<gmpq> Q1 = makeQspray(Powers1, coeffs1);
  Qspray<gmpq> Q2 = makeQspray(Powers2, coeffs2);  
  return returnQspray(Q1 - Q2);
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_mult(const Rcpp::List&         Powers1,
                       const Rcpp::StringVector& coeffs1,
                       const Rcpp::List&         Powers2,
                       const Rcpp::StringVector& coeffs2) {
  Qspray<gmpq> Q1 = makeQspray(Powers1, coeffs1);
  Qspray<gmpq> Q2 = makeQspray(Powers2, coeffs2);  
  return returnQspray(Q1 * Q2);
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
bool qspray_equality(const Rcpp::List&         Powers1,
                     const Rcpp::StringVector& coeffs1,
                     const Rcpp::List&         Powers2,
                     const Rcpp::StringVector& coeffs2) {
  Qspray<gmpq> Q1 = makeQspray(Powers1, coeffs1);
  Qspray<gmpq> Q2 = makeQspray(Powers2, coeffs2);  
  return Q1 == Q2;
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List qspray_power(const Rcpp::List&         Powers,
                        const Rcpp::StringVector& coeffs,
                        unsigned int n) {
   Qspray<gmpq> Q = makeQspray(Powers, coeffs);
   return returnQspray(Q.power(n));
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
int lexLeadingIndexCPP(const Rcpp::List& Powers) {
  int n = Powers.size();
  if(n == 1) {
    return 1;
  }
  Rcpp::IntegerVector Exponents0 = Powers(0);
  powers pows0(Exponents0.begin(), Exponents0.end());
  int out = 0;
  for(int i = 1; i < n; i++) {
    Rcpp::IntegerVector Exponents = Powers(i);
    powers pows(Exponents.begin(), Exponents.end());
    bool ismax = std::lexicographical_compare(
      std::begin(pows0), std::end(pows0), std::begin(pows), std::end(pows)
    );
    if(ismax) {
      out = i;
      pows0 = pows;
    }      
  }
  return out + 1;
}
