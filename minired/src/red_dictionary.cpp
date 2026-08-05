//
//  red_dictionary.cpp -- Redatam dictionary API
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

#include <vector>

#include "redengine_c.h"

extern std::shared_ptr<RedatamAPI> API;

void DICTIONARY_R_CFinalizer_t(SEXP obj) {
  if(!R_ExternalPtrAddr(obj)) return;

  void* ptr = R_ExternalPtrAddr(obj);
  API->redc_dictionary_close(ptr);
  R_ClearExternalPtr(obj); /* not really needed */
}

void dictionary_entity_handle_callback(void* ptr, const char* name, const char* label, int is_selectable, int is_virtual, const char* code_var, const char* label_var, void* user_data) {

  //printf("dictionary_entity_handle_callback==>%s\n", name);

  int64_t** ptrs = (int64_t **)user_data;

  cpp11::writable::strings* names       = (cpp11::writable::strings* )ptrs[0];
  cpp11::writable::integers* selectable = (cpp11::writable::integers* )ptrs[1];
  cpp11::writable::integers* virtual_   = (cpp11::writable::integers* )ptrs[2];
  cpp11::writable::strings* codes       = (cpp11::writable::strings* )ptrs[3];
  cpp11::writable::strings* labels      = (cpp11::writable::strings* )ptrs[4];

  names->push_back(name);
  selectable->push_back(is_selectable);
  virtual_->push_back(is_virtual);

  if( code_var!=nullptr )
    codes->push_back(code_var);
  else
    codes->push_back("");

  if( label_var!=nullptr )
    labels->push_back(label_var);
  else
    labels->push_back("");
}

void dictionary_variable_handle_callback(void* ptr, const char* name, const char* label, const char* alias, int type, const char* typeName, void* user_data) {
  int64_t** ptrs = (int64_t **)user_data;

  cpp11::writable::strings* names     = (cpp11::writable::strings* )ptrs[0];
  cpp11::writable::strings* labels    = (cpp11::writable::strings* )ptrs[1];
  cpp11::writable::strings* typeNames = (cpp11::writable::strings* )ptrs[2];
  cpp11::writable::strings* aliases   = (cpp11::writable::strings* )ptrs[3];

  names->push_back(name);
  labels->push_back(label);
  typeNames->push_back(typeName);

  if( alias!=nullptr )
    aliases->push_back(alias);
  else
    aliases->push_back("");
}


[[cpp11::register]]
SEXP redatam_open( const std::string& dictionary_name )
{
  //void* ptr = redc_dictionary_open(dictionary_name.c_str(), nullptr, nullptr);
  void* ptr = API->redc_dictionary_open(dictionary_name.c_str(), nullptr, nullptr);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object");
  }

  SEXP ptr_dic;
  PROTECT( ptr_dic = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue) );
  R_RegisterCFinalizerEx( ptr_dic, DICTIONARY_R_CFinalizer_t, TRUE );
  UNPROTECT( 1 );

  return ptr_dic;
}

[[cpp11::register]]
void redatam_save( SEXP dic, const std::string& name="" )
{
  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  //API->redc_dictionary_save(ptr);

  R_ClearExternalPtr(dic);
}

[[cpp11::register]]
void redatam_close( SEXP dic )
{
  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  API->redc_dictionary_close(ptr);

  R_ClearExternalPtr(dic);
}


[[cpp11::register]]
cpp11::writable::list redatam_entities( SEXP dic )
{
  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  cpp11::writable::list entities = cpp11::writable::list(5);

  cpp11::writable::strings names;
  cpp11::writable::integers selectable;
  cpp11::writable::integers virtual_;
  cpp11::writable::strings codes;
  cpp11::writable::strings labels;

  void* ptrs[5] = {&names, &selectable, &virtual_, &codes, &labels};

  API->redc_dictionary_list_entitites(ptr, dictionary_entity_handle_callback, (void *)&ptrs);

  entities[0] = names;
  entities[1] = selectable;
  entities[2] = virtual_;
  entities[3] = codes;
  entities[4] = labels;

  int rows = names.size();

  entities.names() = {"name", "selectable", "virtual", "codes", "labels"};
  entities.attr("class") = "data.frame";
  entities.attr("row.names") = {NA_INTEGER, -rows};

  return entities;
}

[[cpp11::register]]
cpp11::writable::list redatam_variables( SEXP dic, const std::string& entity_name )
{
  void* ptr = R_ExternalPtrAddr(dic);

  if(ptr==nullptr) {
    cpp11::stop("Dictionary must be a valid object" );
  }

  cpp11::writable::list variables = cpp11::writable::list(4);

  cpp11::writable::strings names;
  cpp11::writable::strings labels;
  cpp11::writable::strings typeName;
  cpp11::writable::strings alias;

  void* ptrs[4] = {&names, &labels, &typeName, &alias};

  API->redc_dictionary_list_variables(ptr, entity_name.c_str(), dictionary_variable_handle_callback, &ptrs);

  variables[0] = names;
  variables[1] = labels;
  variables[2] = typeName;
  variables[3] = alias;

  int rows = names.size();

  variables.names() = {"name", "label", "typeName", "alias"};
  variables.attr("class") = "data.frame";
  variables.attr("row.names") = {NA_INTEGER, -rows};

  return variables;
}

