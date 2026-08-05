///////////////////////////////////////////////////////////////////////////
// Copyright (C) 2024  Jaime Salvador                                    //
//                                                                       //
// This program is free software: you can redistribute it and/or modify  //
// it under the terms of the GNU General Public License as published by  //
// the Free Software Foundation, either version 3 of the License, or     //
// (at your option) any later version.                                   //
//                                                                       //
// This program is distributed in the hope that it will be useful,       //
// but WITHOUT ANY WARRANTY; without even the implied warranty of        //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         //
// GNU General Public License for more details.                          //
//                                                                       //
// You should have received a copy of the GNU General Public License     //
// along with this program.  If not, see <http://www.gnu.org/licenses/>. //
///////////////////////////////////////////////////////////////////////////

#ifndef REDENGINE_REDENGINE_C_H
#define REDENGINE_REDENGINE_C_H

#include <functional>

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

typedef void* dictionary_ptr;
typedef void* VoidPtr;

typedef void* session_ptr;

typedef void (*compiler_callback)(int type, int code, char* code_str, char* msg, int line_no, int col_no);
typedef bool (*execution_callback)(void* userData, int timeExpected, int timeElapsed, int percent, void* params);

typedef void (*dataset_new_row_handle)(int row, int size, int* types, VoidPtr* data, void* user_data);

//--dictionary
typedef void (*dictionary_entity_handle)(VoidPtr ptr, const char* name, const char* label, int selectable, int is_virtual, const char* code_var, const char* label_var, void* user_data);
typedef void (*dictionary_variable_handle)(VoidPtr ptr, const char* name, const char* label, const char* alias, int type, const char* typeName, void* user_data);


#ifdef __cplusplus
}
#endif

struct RedatamAPI {
  bool is_runtime_loaded;

  std::function<void()> redc_init;
  std::function<void()> redc_destroy;
  std::function<char*()> redc_version;
  std::function<char*()> redc_banner;

  std::function<dictionary_ptr(const char* path, dictionary_entity_handle handle, void* user_data)> redc_dictionary_open;
  std::function<void(dictionary_ptr ptr)> redc_dictionary_close;
  std::function<void(dictionary_ptr ptr, dictionary_entity_handle handle, void* user_data)> redc_dictionary_list_entitites;
  std::function<void(dictionary_ptr ptr, const char* entName, dictionary_variable_handle handle, void* user_data)> redc_dictionary_list_variables;

  std::function<void(session_ptr ptr, int index, int* type, int* dimension, int* field_count, int* row_count, char** out_name)> redc_session_output_data;
  std::function<void(session_ptr ptr, int index, int* fields_type, char** fields_name)> redc_session_output_fields_type;
  std::function<void(session_ptr ptr, int index,dataset_new_row_handle handle, void* user_data)> redc_session_output_iterate;
  std::function<int( session_ptr ptr )> redc_session_output_count;

  std::function<session_ptr( dictionary_ptr ptr, const char* buffer, compiler_callback callback, execution_callback ex_callback)> redc_run_program;
  std::function<session_ptr( dictionary_ptr ptr, const char* file_name, compiler_callback callback, execution_callback ex_callback )> redc_run_program_file;
};

#endif //REDENGINE_REDENGINE_C_H
