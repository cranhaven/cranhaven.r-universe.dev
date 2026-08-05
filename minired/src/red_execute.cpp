//
//  red_execute.cpp -- Redatam query/run API
//
//  Copyright (C) 2024 Jaime Salvador
//
//  This file is part of Redatam package
//
//  Redatam package is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 2 of the License, or
//  (at your option) any later version.
//
//  Redatam package is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Redatam package.  If not, see <http://www.gnu.org/licenses/>.

#include "cpp11.hpp"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

#include <vector>

#include "redengine_c.h"

extern std::shared_ptr<RedatamAPI> API;

bool __sp_callback (void *userData, int timeExpected, int timeElapsed, int percent, void *params) {

  //std::string msg = fmt::format("\r Redatam process: {}%", percent);

  //char buff[500];
  //snprintf(buff, sizeof(buff), "\r Redatam process: %d%", percent);

  R_CheckUserInterrupt();

//  try
//  {
//    //Rcpp::checkUserInterrupt();
//  }
//  catch(Rcpp::internal::InterruptedException e)
//  {
//    return true;
//  }

  return false;

};

void compiler_handler_callback(int type, int code, char* code_str, char* msg, int line_no, int col_no) {

  char buff[100];

  if(type==0) { //exception
    REprintf("%s\n", msg);
  }
  else if(type==1) { //error
    //std::string error_msg = fmt::format("{}: line {}:{} {}", code_str, line_no, col_no, msg);
    snprintf(buff, sizeof(buff), "%s: line %d:%d %s", code_str, line_no, col_no, msg);

    REprintf("%s\n", buff);
  }
  else { //warning
    //std::string error_msg = fmt::format("{}: line {}:{} {}", code_str, line_no, col_no, msg);
    snprintf(buff, sizeof(buff), "%s: line %d:%d %s", code_str, line_no, col_no, msg);

    Rf_warning("%s\n", buff);
  }

  R_FlushConsole();
}

void dataset_new_row_handler(int row, int size, int* types, void** data, void* user_data) {
  std::vector<SEXP>* sexpColumns = (std::vector<SEXP> *)user_data;

  int cols = sexpColumns->size();

  for(int j=0; j<cols; j++ ) {
    SEXP col = sexpColumns->at(j);

    if( data[j]==nullptr ) {
      if( types[j]==1 ) {
        INTEGER(col)[static_cast<int>(row)] = NA_INTEGER;
      }
      else if( types[j]==2 ) {
        REAL(col)[static_cast<int>(row)] = NA_REAL;
      }
      else if( types[j]==3 ) {
        SET_STRING_ELT(col,row, NA_STRING );
      }
    }
    else {
      if( types[j]==1 ) {
        int64_t* value = (int64_t *)data[j];
        INTEGER(col)[static_cast<int>(row)] = *value;
      }
      else if( types[j]==2 ) {
        double* value = (double *)data[j];
        REAL(col)[static_cast<int>(row)] = *value;
      }
      else if( types[j]==3 ) {
        char* value = (char *)data[j];
        SET_STRING_ELT(col,row, Rf_mkChar(value));
      }
    }
  }
}

SEXP createOutput( void* session_ptr, int index ) {
  int out_cols=2;
  int out_rows=2;
  int out_type=1;
  int dimension;

  std::vector<char*> out_name(10);

  API->redc_session_output_data(session_ptr, index, &out_type, &dimension, &out_cols, &out_rows, (char **)out_name.data());

  std::vector<int> fields_type(out_cols);
  std::vector<char*> fields_name(out_cols);

  API->redc_session_output_fields_type(session_ptr, index, fields_type.data(), (char **)fields_name.data());

  std::vector<SEXP> sexpColumns;
  for(int i=0;i<out_cols;i++) {
    SEXP col;

    if(fields_type[i]==1) { // integer
      Rf_protect(col=Rf_allocVector(INTSXP, out_rows));
    }
    else if(fields_type[i]==2) { // real
      Rf_protect(col=Rf_allocVector(REALSXP, out_rows));
    }
    else if(fields_type[i]==3) { // string
      Rf_protect(col=Rf_allocVector(STRSXP, out_rows));
    }

    sexpColumns.push_back(col);
  }

  //data
  API->redc_session_output_iterate(session_ptr, index, dataset_new_row_handler, (void *)&sexpColumns);

  //dataframe
  SEXP ans = PROTECT(Rf_allocVector(VECSXP, out_cols));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, out_cols));
  SEXP rnms = PROTECT(Rf_allocVector(INTSXP, 2));


  for(int i=0; i<out_cols; i++ ) {
    SET_STRING_ELT(nms, i, Rf_mkCharCE( fields_name[i], CE_UTF8 ));

    SET_VECTOR_ELT(ans, i, sexpColumns[i] );
  }

  INTEGER(rnms)[0] = NA_INTEGER;
  INTEGER(rnms)[1] = -out_rows;

  Rf_setAttrib(ans, R_ClassSymbol, Rf_ScalarString(Rf_mkChar("data.frame")));
  Rf_setAttrib(ans, R_RowNamesSymbol, rnms);
  Rf_setAttrib(ans, R_NamesSymbol, nms);

  std::string tableType = "";
  std::string tableName = std::string(out_name[0]);

  if( out_type==0 ){
    tableType = "table";
  }
  else if( out_type==1 ){
    tableType = "arealist";
  }
  else if( out_type==2 ){
    tableType = "tablist";
  }
  else if( out_type==3 ){
    tableType = "tabop";
  }

  Rf_setAttrib(ans, Rf_mkString("redatam.table.type"), Rf_mkString(tableType.c_str()));
  Rf_setAttrib(ans, Rf_mkString("redatam.table.name"), Rf_mkString(tableName.c_str()));

  SEXP vars = Rf_allocVector( STRSXP, 1 );

  Rf_setAttrib(ans, Rf_mkString("redatam.table.vars"), vars);

  UNPROTECT(out_cols+3);

  return(ans);
}

[[cpp11::register]]
cpp11::writable::list redatam_query( SEXP dic, const std::string& spc )
{
  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  if( spc.empty() ) {
    cpp11::stop("SPC can't be empty" );
  }

  void* session_ptr = API->redc_run_program(ptr, spc.c_str(), compiler_handler_callback,__sp_callback);

  if(session_ptr==nullptr) {
    // errores de compilación
    return cpp11::writable::list();
  }

  int count = API->redc_session_output_count(session_ptr);

  if(count==0) {
    return cpp11::writable::list();
  }

  std::vector<SEXP> ret;

  for(int i=0;i<count;i++) {
    SEXP df = createOutput(session_ptr, i);

    ret.push_back(df);
  }

  return cpp11::writable::list(ret.begin(), ret.end());

}

[[cpp11::register]]
cpp11::writable::list redatam_run( SEXP dic, const std::string& file_name ) {

  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  if( file_name.empty() ) {
    cpp11::stop("SPC file_name can't be empty" );
  }

  void* session_ptr = API->redc_run_program_file(ptr, file_name.c_str(), compiler_handler_callback, __sp_callback);

  if(session_ptr==nullptr) {
    // errores de compilación
    return {};
  }

  int count = API->redc_session_output_count(session_ptr);

  if(count==0) {
    return {};
  }

  std::vector<SEXP> ret;

  for(int i=0;i<count;i++) {
    SEXP df = createOutput(session_ptr, i);

    ret.push_back(df);
  }

  return cpp11::writable::list(ret.begin(), ret.end());
}



