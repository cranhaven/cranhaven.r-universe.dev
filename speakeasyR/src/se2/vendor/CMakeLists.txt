# Collects a minimal set of object files from igraph needed to compile
# speakeasy 2 (SE2). This may be fragile as igraph could move where certain
# functions are but the advantage is reducing the dependencies required to
# build SE2, making it easier to build elsewhere. It also makes a cleaner way
# to merge the SE2 code with the required igraph code for distribution to
# higher level interfaces removing the need to worry about external shared
# libs. Secondarily, it reduces compile times and final lib size.

# Set default symbol visibility to hidden
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

set(INTERNAL_ARPACK 1)
set(INTERNAL_BLAS 1)
set(INTERNAL_LAPACK 1)

include(CheckSymbolExists)
include(CheckIncludeFiles)

check_symbol_exists(strcasecmp strings.h HAVE_STRCASECMP)
check_symbol_exists(strncasecmp strings.h HAVE_STRNCASECMP)
check_symbol_exists(_stricmp string.h HAVE__STRICMP)
check_symbol_exists(_strnicmp string.h HAVE__STRNICMP)
check_symbol_exists(strdup string.h HAVE_STRDUP)
check_symbol_exists(strndup string.h HAVE_STRNDUP)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/igraph/etc/cmake)
include(CheckTLSSupport)
check_tls_support(TLS_KEYWORD)

if(TLS_KEYWORD AND SE2_PARALLEL)
  message(STATUS "Compiling thread safe")
  set(IGRAPH_THREAD_SAFE YES PARENT_SCOPE)
else()
  message(STATUS "Not compiling thread safe")
  set(IGRAPH_THREAD_SAFE NO PARENT_SCOPE)
  set(TLS_KEYWORD "")
endif()

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(IGRAPH_INTEGER_SIZE 64)
else()
  set(IGRAPH_INTEGER_SIZE 32)
endif()

set(igraph_src ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src)
set(igraph_core ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src/core)
set(igraph_graph ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src/graph)
set(igraph_internal ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src/internal)
set(igraph_random ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src/random)
set(f2c ${CMAKE_CURRENT_SOURCE_DIR}/igraph/vendor/f2c)

# Assume these won't change.
file(GLOB cs_files ${CMAKE_CURRENT_SOURCE_DIR}/igraph/vendor/cs/*.c)
file(GLOB pcg_files ${CMAKE_CURRENT_SOURCE_DIR}/igraph/vendor/pcg/*.c)
file(GLOB lapack_files ${CMAKE_CURRENT_SOURCE_DIR}/igraph/vendor/lapack/*.c)

add_library(igraph
  OBJECT
  ${igraph_core}/interruption.c
  ${igraph_core}/vector.c
  ${igraph_core}/memory.c
  ${igraph_core}/indheap.c
  ${igraph_core}/matrix.c
  ${igraph_core}/vector_ptr.c
  ${igraph_core}/vector_list.c
  ${igraph_core}/printing.c
  ${igraph_core}/error.c
  ${igraph_core}/sparsemat.c
  ${igraph_random}/random.c
  ${igraph_random}/rng_glibc2.c
  ${igraph_random}/rng_mt19937.c
  ${igraph_random}/rng_pcg32.c
  ${igraph_random}/rng_pcg64.c
  ${igraph_graph}/type_indexededgelist.c
  ${igraph_graph}/iterators.c
  ${igraph_graph}/type_common.c
  ${igraph_graph}/caching.c
  ${igraph_graph}/attributes.c
  ${igraph_internal}/hacks.c
  ${igraph_internal}/qsort.c
  ${igraph_internal}/qsort_r.c
  ${igraph_src}/math/complex.c
  ${igraph_src}/math/utils.c
  ${igraph_src}/properties/degrees.c
  ${igraph_src}/constructors/basic_constructors.c
  ${igraph_src}/community/community_misc.c
  ${igraph_src}/linalg/arpack.c
  ${cs_files}
  ${pcg_files}
  ${lapack_files}
  ${f2c}/wref.c
  ${f2c}/endfile.c
  ${f2c}/err.c
  ${f2c}/wrtfmt.c
  ${f2c}/sig_die.c
  ${f2c}/f77_aloc.c
  ${f2c}/fmt.c
  ${f2c}/open.c
  ${f2c}/sfe.c
  ${f2c}/util.c
  ${f2c}/wsfe.c
  ${f2c}/close.c
  ${f2c}/ctype.c
  ${f2c}/exit_.c
  ${f2c}/i_len.c
  ${f2c}/s_cat.c
  ${f2c}/s_cmp.c
  ${f2c}/d_lg10.c
  ${f2c}/d_sign.c
  ${f2c}/etime_.c
  ${f2c}/fmtlib.c
  ${f2c}/i_dnnt.c
  ${f2c}/i_nint.c
  ${f2c}/pow_dd.c
  ${f2c}/pow_di.c
  ${f2c}/s_copy.c
  ${f2c}/s_stop.c

  # Needed only for examples
  ${igraph_src}/games/preference.c
  ${igraph_src}/games/sbm.c
  ${igraph_src}/constructors/famous.c
  ${igraph_src}/math/safe_intop.c
  ${igraph_src}/core/matrix_list.c
)

if (NOT BUILD_SHARED_LIBS)
  target_compile_definitions(igraph PUBLIC IGRAPH_STATIC)
else()
  target_compile_definitions(igraph PRIVATE igraph_EXPORTS)
endif()

target_link_libraries(igraph PRIVATE m)

include(GenerateExportHeader)
generate_export_header(igraph
  STATIC_DEFINE IGRAPH_STATIC
  EXPORT_FILE_NAME ${PROJECT_BINARY_DIR}/include/igraph_export.h
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src/config.h.in
  ${PROJECT_BINARY_DIR}/vendor/igraph/src/config.h
)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/igraph/include/igraph_config.h.in
  ${PROJECT_BINARY_DIR}/include/igraph_config.h
)

target_include_directories(igraph
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/igraph/include
  ${PROJECT_BINARY_DIR}/include
  PRIVATE
  ${PROJECT_BINARY_DIR}/vendor/igraph/src
  ${CMAKE_CURRENT_SOURCE_DIR}/igraph/src
  ${CMAKE_CURRENT_SOURCE_DIR}/igraph/vendor
)
