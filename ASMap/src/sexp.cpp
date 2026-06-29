#include <R.h>
#include <Rdefines.h>
SEXP elem(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names;
  int i;

  names = Rf_getAttrib(list, R_NamesSymbol);
  for(i = 0; i < Rf_length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names,i)), str) == 0) {
      elmt = VECTOR_ELT(list,i);
      break;
    }
  return elmt;
}
SEXP ielem(SEXP list, int i)
{
  SEXP elmt = R_NilValue;
  elmt = VECTOR_ELT(list,i);
  return elmt;
}
