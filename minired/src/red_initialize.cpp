//
//  red_initialize.cpp -- Redatam initialize API
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


#include <iostream>

#include <string>
#include <functional>
#include <iostream>

#include "cpp11.hpp"

#include "redengine_c.h"

#include "dylib.hpp"

std::shared_ptr<dylib> _RedatamEngineLibPtr;

std::shared_ptr<RedatamAPI> API;

typedef std::function<char*()> redc_versionFn;

void redatamEngine(std::string libRuntimeName) {

  try {
    if(_RedatamEngineLibPtr==nullptr) {
      _RedatamEngineLibPtr = std::make_shared<dylib>(libRuntimeName, "redengine-1.0.0-rc2" );
      API = std::make_shared<RedatamAPI>();
    }

    API->redc_init    = _RedatamEngineLibPtr->get_function<void()>("redc_init");
    API->redc_destroy = _RedatamEngineLibPtr->get_function<void()>("redc_destroy");

    API->redc_version = _RedatamEngineLibPtr->get_function<char*()>("redc_version");
    API->redc_banner  = _RedatamEngineLibPtr->get_function<char*()>("redc_banner");
    API->redc_dictionary_open = _RedatamEngineLibPtr->get_function<dictionary_ptr(const char* path, dictionary_entity_handle handle, void* user_data)>("redc_dictionary_open");
    API->redc_dictionary_close = _RedatamEngineLibPtr->get_function<void(dictionary_ptr ptr)>("redc_dictionary_close");
    API->redc_dictionary_list_entitites = _RedatamEngineLibPtr->get_function<void(dictionary_ptr ptr, dictionary_entity_handle handle, void* user_data)>("redc_dictionary_list_entitites");
    API->redc_dictionary_list_variables = _RedatamEngineLibPtr->get_function<void(dictionary_ptr ptr, const char* entName, dictionary_variable_handle handle, void* user_data)>("redc_dictionary_list_variables");

    API->redc_session_output_data =  _RedatamEngineLibPtr->get_function<void(session_ptr ptr, int index, int* type, int* dimension, int* field_count, int* row_count, char** out_name)>("redc_session_output_data");
    API->redc_session_output_fields_type = _RedatamEngineLibPtr->get_function<void(session_ptr ptr, int index, int* fields_type, char** fields_name)>("redc_session_output_fields_type");
    API->redc_session_output_iterate = _RedatamEngineLibPtr->get_function<void(session_ptr ptr, int index,dataset_new_row_handle handle, void* user_data)>("redc_session_output_iterate");
    API->redc_session_output_count = _RedatamEngineLibPtr->get_function<int( session_ptr ptr )>("redc_session_output_count");

    API->redc_run_program = _RedatamEngineLibPtr->get_function<session_ptr( dictionary_ptr ptr, const char* buffer, compiler_callback callback, execution_callback ex_callback)>("redc_run_program");
    API->redc_run_program_file = _RedatamEngineLibPtr->get_function<session_ptr( dictionary_ptr ptr, const char* file_name, compiler_callback callback, execution_callback ex_callback )>("redc_run_program_file");

    API->is_runtime_loaded = true;
  }
  catch (const dylib::load_error &) {
    //"failed to load 'foo' library"
    API->is_runtime_loaded = false;
  } catch (const dylib::symbol_error &) {
    //"failed to get 'pi_value' symbol"
    API->is_runtime_loaded = false;
  }
}

[[cpp11::register]]
std::string redatam_version( ) {
  if(API->is_runtime_loaded) {
    return API->redc_version();
  }
  else {
    return "API no loaded!";
  }
}

[[cpp11::register]]
void redatam_init_( std::string pachageDir ) {

  redatamEngine(pachageDir);

  if(API->is_runtime_loaded) {
    API->redc_init();
  }
}

[[cpp11::register]]
void redatam_destroy_( ) {

  if(API->is_runtime_loaded) {
    API->redc_destroy();
  }

  API.reset();
  _RedatamEngineLibPtr.reset();

  auto tmp_directory = std::filesystem::temp_directory_path() / "redatam";
  /*bool ok = */std::filesystem::remove_all(tmp_directory);
}




