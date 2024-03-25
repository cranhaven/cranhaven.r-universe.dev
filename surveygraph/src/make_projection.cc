#include <R.h>
#include <Rdefines.h>

#include "surveygraph.h"

#include <vector>

/* 

This is where we should handle data cleaning, checking for a 'group' column, type 
checking etc.

TODO: currently assuming df is a data.frame... do not do this

Use numeric coercion to read deta. If not coercable (returns NULL?) skip column.

*/
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

static void vectors_to_df(const graph &g, SEXP &c, SEXP &df)
{
  SEXP u_vector = PROTECT(allocVector(INTSXP, g.e));  // u column
  SEXP v_vector = PROTECT(allocVector(INTSXP, g.e));  // v column
  SEXP w_vector = PROTECT(allocVector(REALSXP, g.e)); // weight column

  int i = 0;
  for(auto &it : g.network){
    for(auto &jt : it.second){
      if(it.first < jt.u){
        INTEGER(u_vector)[i] = it.first + 1;
        INTEGER(v_vector)[i] = jt.u + 1;
        REAL(w_vector)[i] = int(100.0 * jt.w) / 100.0;
        if(INTEGER(c)[0] == 0) REAL(w_vector)[i] += 1.0;
        i += 1;
      }
    }
  }

  SET_VECTOR_ELT(df, 0, u_vector);
  SET_VECTOR_ELT(df, 1, v_vector);
  SET_VECTOR_ELT(df, 2, w_vector);

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("u"));            // name first column u
  SET_STRING_ELT(names, 1, mkChar("v"));            // name second column v
  SET_STRING_ELT(names, 2, mkChar("weight"));       // name third column weight, required by igraph

  SEXP rownames = PROTECT(allocVector(INTSXP, 2));
  INTEGER(rownames)[0] = NA_INTEGER;                // default entry if size below too small
  INTEGER(rownames)[1] = -length(u_vector);         // number of rows in agent edge list

  setAttrib(df, R_ClassSymbol, ScalarString(mkChar("data.frame")));
  setAttrib(df, R_RowNamesSymbol, rownames);
  setAttrib(df, R_NamesSymbol, names);

  UNPROTECT(5);
}

SEXP rmake_proj_agent_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_lcc();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_agent_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_ad();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_agent_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_agent_similar();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_agent, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_lcc(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 0, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_lcc();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_ad(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 1, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_ad();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}

SEXP rmake_proj_symbolic_similar(SEXP df, SEXP mvalue, SEXP c, SEXP sim_metric) 
{
  std::vector<std::vector<double>> surveytmp;
  df_to_cppvector(df, surveytmp);
  normalise_columns(surveytmp);

  surveygraph S{surveytmp, 2, REAL(mvalue)[0], INTEGER(sim_metric)[0]};
  S.make_proj_symbolic_similar();

  SEXP e = PROTECT(allocVector(VECSXP, 3));
  vectors_to_df(S.g_symbolic, c, e);

  UNPROTECT(1);

  return e;
}
