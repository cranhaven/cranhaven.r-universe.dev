#include <R.h>
#include <Rdefines.h>

#include <vector>

/*

In this file we archive code snippets, such as functions, that we found helpful
when learning the C interface for R.

*/

SEXP archived_inputoutput(SEXP m, SEXP n) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = 2 * INTEGER(m)[0] + 3 * INTEGER(n)[0];
  Rprintf("processing %d and %d\n", INTEGER(m)[0], INTEGER(n)[0]);
  Rprintf("output will be %d\n", INTEGER(result)[0]);
  UNPROTECT(1);
  return result;
}

SEXP archived_hwnumeric(SEXP a, SEXP b)
{
  SEXP result = PROTECT(NEW_NUMERIC(1));
  REAL(result)[0] = REAL(a)[0] + REAL(b)[0];
  Rprintf("hello world! the sum of %f and %f is %f\n", REAL(a)[0], REAL(b)[0], REAL(result)[0]);
  Rprintf("the value of REALSXP is %d\n", REALSXP);
  UNPROTECT(1);
  return result;
}

SEXP archived_hwinteger(SEXP a, SEXP b) 
{
  SEXP result = PROTECT(NEW_INTEGER(1));
  INTEGER(result)[0] = INTEGER(a)[0] + INTEGER(b)[0];
  Rprintf("hello world! the sum of %d and %d is %d\n", INTEGER(a)[0], INTEGER(b)[0], INTEGER(result)[0]);
  Rprintf("the value of INTSXP is %d\n", INTSXP);
  UNPROTECT(1);
  return result;
}

// checks data type of each column in a dataframe 
// commented-out lines caused warnings in windows build under R-CMD-check workflow
SEXP archived_dftypes(SEXP x) 
{
  int len = length(x);
  SEXP result = PROTECT(NEW_CHARACTER(len));

  for(int i = 0; i < len; ++i) {
    switch(TYPEOF(VECTOR_ELT(x, i))) {
      case(REALSXP):
        CHARACTER_POINTER(result)[i] = mkChar("numeric");
        //Rprintf("%f\n", VECTOR_ELT(x, i));
        break;
      case(INTSXP):
        CHARACTER_POINTER(result)[i] = mkChar("integer");
        //Rprintf("%d\n", VECTOR_ELT(x, i));
        break;
      case(LGLSXP):
        CHARACTER_POINTER(result)[i] = mkChar("logical");
        //Rprintf("%d\n", VECTOR_ELT(x, i));
        break;
      case(STRSXP):
        CHARACTER_POINTER(result)[i] = mkChar("character");
        //Rprintf("%c\n", VECTOR_ELT(x, i));
        break;
      case(VECSXP):
        CHARACTER_POINTER(result)[i] = mkChar("list");
        break;
      default:
        CHARACTER_POINTER(result)[i] = mkChar("unknown");
    }
  }
  UNPROTECT(1);
  return result;
}

// read in a numeric vector, output a modified vector
SEXP archived_vectormanip(SEXP x) 
{
  int len = length(x);
  SEXP result = PROTECT(NEW_NUMERIC(len)); // allocVector(REALSXP, len)
  if(TYPEOF(x) != REALSXP) Rprintf("should be a double\n");
  for(int i = 0; i < len; ++i) {
    REAL(result)[i] = 10 * REAL(x)[i] + 1;
  }
  UNPROTECT(1);
  return result;
}

// read in a data frame and output a different data frame
SEXP archived_dfmanip(SEXP x) 
{
  // extend a dataframe containing 3 rows to 4 rows
  SEXP oldcol1 = PROTECT(allocVector(REALSXP, 3));  // assume three double types
  SEXP oldcol2 = PROTECT(allocVector(REALSXP, 3));
  SEXP newcol1 = PROTECT(allocVector(REALSXP, 4));
  SEXP newcol2 = PROTECT(allocVector(REALSXP, 4));

  oldcol1 = VECTOR_ELT(x, 0);                       // extract first column from x
  oldcol2 = VECTOR_ELT(x, 1);                       // extract second column from x

  // set elements of data frame
  REAL(newcol1)[0] = REAL(oldcol1)[0];
  REAL(newcol1)[1] = REAL(oldcol1)[1];
  REAL(newcol1)[2] = REAL(oldcol1)[2];
  REAL(newcol1)[3] = 4.0;
  REAL(newcol2)[0] = REAL(oldcol2)[0];
  REAL(newcol2)[1] = REAL(oldcol2)[1];
  REAL(newcol2)[2] = REAL(oldcol2)[2];
  REAL(newcol2)[3] = 8.0;

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, newcol1);
  SET_VECTOR_ELT(result, 1, newcol2);

  SEXP names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(names, 0, mkChar("x"));            // name first column x
  SET_STRING_ELT(names, 1, mkChar("y"));            // name second column y

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -4;                        // number of rows

  setAttrib(result, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(result, R_RowNamesSymbol, rownames);
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(7);
  return result;
}

static void print_df_r(SEXP x) 
{
  int m = length(x);
  for(int i = 0; i < m; ++i) {
    printf("%d : ", i);
    SEXP dummy = PROTECT(VECTOR_ELT(x, i));
    int n = length(dummy);
    for(int j = 0; j < n; ++j) {
      printf("%f ", REAL_ELT(dummy, j));
    }
    UNPROTECT(1);
    printf("\n");
  }
}

static void convert_df_vecvec(SEXP x) 
{
  int m = length(x);
  std::vector<std::vector<int>> s(m);
  for(int i = 0; i < m; ++i) {
    printf("%d : ", i);
    SEXP dummy = PROTECT(VECTOR_ELT(x, i));
    int n = length(dummy);
    for(int j = 0; j < n; ++j) {
      printf("%f ", REAL_ELT(dummy, j));
    }
    UNPROTECT(1);
    printf("\n");
  }
}
