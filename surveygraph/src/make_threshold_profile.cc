#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

static void df_to_cppvector(const SEXP &df, std::vector<vector<double>> &stmp)
{
  vector<vector<double>> surveytmp;

  SEXP check = PROTECT(allocVector(VECSXP, length(df)));
  for(int i = 0; i < length(df); ++i){
    check = VECTOR_ELT(df, i);
    if(TYPEOF(check) == STRSXP){
      // string column, do nothing
    }else if(TYPEOF(check) == REALSXP){
      vector<double> coltmp;
      for(int j = 0; j < length(check); ++j){
        coltmp.push_back(REAL(check)[j]);
      }
      surveytmp.push_back(coltmp);
    }else if(TYPEOF(check) == INTSXP){
      vector<double> coltmp;
      for(int j = 0; j < length(check); ++j){
        coltmp.push_back(double(INTEGER(check)[j]));
      }
      surveytmp.push_back(coltmp);
    }else{
      // neither string, real or integer, do nothing
    }
  }

  int ncol = surveytmp.size();
  int nrow = surveytmp[0].size();

  // take the transpose
  stmp = std::vector<std::vector<double>>(nrow, std::vector<double>(ncol));
  for(int i = 0; i < surveytmp.size(); ++i){
    for(int j = 0; j < surveytmp[i].size(); ++j){
      stmp[j][i] = surveytmp[i][j];
    }
  }

  UNPROTECT(1);
}

static void normalise_columns(std::vector<vector<double>> &s)
{
  // compute the max and min of each column
  vector<double> colmax(s[0].size(), -1e6);
  vector<double> colmin(s[0].size(),  1e6);
  for(int j = 0; j < s[0].size(); ++j){
    for(int i = 0; i < s.size(); ++i){
      if(s[i][j] > colmax[j]) colmax[j] = s[i][j];
      if(s[i][j] < colmin[j]) colmin[j] = s[i][j];
    }
  }

  // map column entries to the interval to [-1, 1]
  for(int j = 0; j < s[0].size(); ++j){
    double m = 2 / (colmax[j] - colmin[j]);
    double b = -(colmax[j] + colmin[j]) / (colmax[j] - colmin[j]);
    for(int i = 0; i < s.size(); ++i){
      s[i][j] = m * s[i][j] + b;
    }
  }
}

// read in a data frame and output list containing two integer vectors
// lists, containing edge lists for respondent and item graphs
SEXP rmake_threshold_profile_agent(SEXP df) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp};
  S.make_threshold_profile_agent();

  // put data from threshold_agent (threshold, z, lcc) into a list
  SEXP t_agent = PROTECT(allocVector(REALSXP, S.profile_agent.size())); // threshold
  SEXP z_agent = PROTECT(allocVector(REALSXP, S.profile_agent.size())); // average degree
  SEXP l_agent = PROTECT(allocVector(REALSXP, S.profile_agent.size())); // LCC
  SEXP i_agent = PROTECT(allocVector(INTSXP, S.profile_agent.size()));  // isolated node count
  SEXP c_agent = PROTECT(allocVector(INTSXP, S.profile_agent.size()));  // component count

  for(int i = 0; i < S.profile_agent.size(); ++i) {
    REAL(t_agent)[i] = S.profile_agent[i][0];
    REAL(z_agent)[i] = S.profile_agent[i][1];
    REAL(l_agent)[i] = S.profile_agent[i][2];
    INTEGER(i_agent)[i] = int(S.profile_agent[i][3] + 0.5);
    INTEGER(c_agent)[i] = int(S.profile_agent[i][4] + 0.5);
  }

  SEXP list_agent = PROTECT(allocVector(VECSXP, 5));
  SET_VECTOR_ELT(list_agent, 0, t_agent);
  SET_VECTOR_ELT(list_agent, 1, z_agent);
  SET_VECTOR_ELT(list_agent, 2, l_agent);
  SET_VECTOR_ELT(list_agent, 3, i_agent);
  SET_VECTOR_ELT(list_agent, 4, c_agent);

  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, mkChar("threshold"));   // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));          // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));         // name third column lcc, for largest connected component
  SET_STRING_ELT(names, 3, mkChar("isolated"));    // name fourth column isolated, for number of isolated nodes
  SET_STRING_ELT(names, 4, mkChar("components"));  // name fifth column components, for number of components

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(t_agent);    // number of rows in agent edgelist

  setAttrib(list_agent, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_agent, R_RowNamesSymbol, rownames);
  setAttrib(list_agent, R_NamesSymbol, names);

  UNPROTECT(8);
  return list_agent;
}

SEXP rmake_threshold_profile_symbolic(SEXP df) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp};
  S.make_threshold_profile_symbolic();

  // put data from threshold_symbolic (threshold, z, lcc) into a list
  SEXP t_symbolic = PROTECT(allocVector(REALSXP, S.profile_symbolic.size())); // threshold
  SEXP z_symbolic = PROTECT(allocVector(REALSXP, S.profile_symbolic.size())); // average degree
  SEXP l_symbolic = PROTECT(allocVector(REALSXP, S.profile_symbolic.size())); // LCC
  SEXP i_symbolic = PROTECT(allocVector(INTSXP, S.profile_symbolic.size()));  // isolated node count
  SEXP c_symbolic = PROTECT(allocVector(INTSXP, S.profile_symbolic.size()));  // number of components

  for(int i = 0; i < S.profile_symbolic.size(); ++i) {
    REAL(t_symbolic)[i] = S.profile_symbolic[i][0];
    REAL(z_symbolic)[i] = S.profile_symbolic[i][1];
    REAL(l_symbolic)[i] = S.profile_symbolic[i][2];
    INTEGER(i_symbolic)[i] = int(S.profile_symbolic[i][3] + 0.5);
    INTEGER(c_symbolic)[i] = int(S.profile_symbolic[i][4] + 0.5);
  }

  SEXP list_symbolic = PROTECT(allocVector(VECSXP, 5));
  SET_VECTOR_ELT(list_symbolic, 0, t_symbolic);
  SET_VECTOR_ELT(list_symbolic, 1, z_symbolic);
  SET_VECTOR_ELT(list_symbolic, 2, l_symbolic);
  SET_VECTOR_ELT(list_symbolic, 3, i_symbolic);
  SET_VECTOR_ELT(list_symbolic, 4, c_symbolic);

  SEXP names = PROTECT(allocVector(STRSXP, 5));
  SET_STRING_ELT(names, 0, mkChar("threshold"));   // name first column threshold
  SET_STRING_ELT(names, 1, mkChar("ad"));          // name second column ad, for average degree
  SET_STRING_ELT(names, 2, mkChar("lcc"));         // name third column lcc, for largest connected component
  SET_STRING_ELT(names, 3, mkChar("isolated"));    // name fourth column isolated, for number of isolated nodes
  SET_STRING_ELT(names, 4, mkChar("components"));  // name fifth column components, for number of components

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(t_symbolic);    // number of rows in symbolic edgelist

  setAttrib(list_symbolic, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(list_symbolic, R_RowNamesSymbol, rownames);
  setAttrib(list_symbolic, R_NamesSymbol, names);

  UNPROTECT(8);
  return list_symbolic;
}
