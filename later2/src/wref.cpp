#include <cpp4r.hpp>

[[cpp4r::register]] SEXP new_weakref(SEXP x) {
  return R_MakeWeakRef(x, R_NilValue, R_NilValue, FALSE);
}

[[cpp4r::register]] SEXP wref_key(SEXP x) {
  return x != R_NilValue ? R_WeakRefKey(x) : R_NilValue;
}
