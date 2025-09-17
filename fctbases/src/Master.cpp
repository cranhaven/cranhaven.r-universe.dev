
// Masterfil. Kontroller det hele, også dig.

/* Functional object
 * (c) Niels Olsen, 2019
 *
 * Base class for functional objects, to be used in FDA or related settings.
 * Re-implementation.
 *
 * 
 */

#include <RcppArmadillo.h>
#include <set>

using namespace std;
using namespace Rcpp;
using namespace arma;

static std::set<size_t> medlemmer; // Indeholder alle fct_class

#include "defs/fctbases_types.hpp"
#include "function_class.h" // functionObject, the main thing. 


//Checks if SEXP is a pointer to a valid function_class object
inline bool check_if_valid(SEXP address) {
  
  if (TYPEOF(address) == EXTPTRSXP) {
    
    size_t st = (size_t) R_ExternalPtrAddr(address);
    std::set<size_t>::iterator it = medlemmer.find(st);
    if (it == medlemmer.end()) return false;
    else return true;
  }
  else return false;
};

//Checks if pointer is to a valid function_class object
inline bool check_if_valid_st(size_t st) {
  cout << st;
  std::set<size_t>::iterator it = medlemmer.find(st);
  if (it == medlemmer.end()) return false;
  else return true;
}


//[[Rcpp::export]]
bool removeMember(SEXP address) {
  if (check_if_valid(address)) {
    functionObject* ft = (functionObject*) R_ExternalPtrAddr(address);
    
    delete ft; // Objektet tager sig selv af stakken
    
    return true;
  }
  else return false;
};

// Bedre implementation som liste.
// Bemærk at lazy evaluation kan gøre ting mærkelige.
//[[Rcpp::export]]
Rcpp::List getObjectsOnList() {

  Rcpp::List ret;

  std::set<size_t>::iterator it;
  for (it=medlemmer.begin(); it!=medlemmer.end(); ++it)
  {
    size_t st = *it;
    SEXP s = PROTECT(R_MakeExternalPtr((void*) st, R_NilValue, R_NilValue));
    ret.push_back(s);
    UNPROTECT(1);
  }
  return ret;
};

// Include files
#include "evaluation.h"    // Evaluates the objects
#include "fourier_header.h"
#include "polynomium.h"
#include "bspline_header.h"




