cmake_minimum_required(VERSION 3.10)
project(swipl-json)

include("../cmake/PrologPackage.cmake")
has_package(http HAVE_HTTP_PACKAGE)
has_package(pcre HAVE_PCRE_PACKAGE)
has_package(clib HAVE_CLIB_PACKAGE)

set(JSON_PL_FILES json.pl json_convert.pl json_grammar.pl)
set(JSON_COMPAT_FILES json.pl json_convert.pl js_grammar.pl)
set(JSON_TESTS json)
prepend(JSON_COMPAT_FILES compat ${JSON_COMPAT_FILES})
if(HAVE_PCRE_PACKAGE)
  list(APPEND JSON_PL_FILES json_schema.pl
       json_rpc_server.pl json_rpc_common.pl)
  if(HAVE_HTTP_PACKAGE)
    list(APPEND JSON_TESTS json_schema)
  endif()
  if(HAVE_CLIB_PACKAGE AND MULTI_THREADED)
    list(APPEND JSON_TESTS json_rpc)
  endif()
endif()
if(MULTI_THREADED)
  list(APPEND JSON_PL_FILES json_rpc_client.pl)
endif()

swipl_plugin(
    json
    C_SOURCES json.c
    PL_LIBS ${JSON_PL_FILES})

test_libs(${JSON_TESTS})

if(HAVE_HTTP_PACKAGE)
set(JSON_HTTP_FILES)
if(NOT EMSCRIPTEN)
  set(JSON_HTTP_FILES http_json.pl)
endif()

  swipl_plugin(
      json_http_plugin
      PL_LIB_SUBDIR http
      PL_LIBS ${JSON_HTTP_FILES} ${JSON_COMPAT_FILES})
  add_dependencies(json json_http_plugin)
endif()

pkg_doc(json
	SOURCES
	    jsonmd.md)
