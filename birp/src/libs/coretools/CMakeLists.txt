cmake_minimum_required(VERSION 3.14)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(CMAKE_BUILD_TYPE "" Release)
option(BUILD_TESTING "Build the tests" OFF)

# Policies (for properly setting the timestamps of extracted contents)
if (POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif (POLICY CMP0135)

# Special build types
if("${CMAKE_BUILD_TYPE}" STREQUAL "Asan")
  set(SAN "-O1;-g;-fsanitize=address;-fno-omit-frame-pointer")
  set(SAN_LINK "-fsanitize=address")
  message("Compiling and linking with adress sanitizer")
elseif("${CMAKE_BUILD_TYPE}" STREQUAL "Msan")
  set(SAN "-O1;-g;-fsanitize=memory;-fno-omit-frame-pointer;-fsanitize-memory-track-origins")
  set(SAN_LINK "-fsanitize=memory")
  message("Compiling and linking with memory sanitizer")
elseif("${CMAKE_BUILD_TYPE}" STREQUAL "UBsan")
  set(SAN "-O1;-g;-fsanitize=undefined;-fno-omit-frame-pointer")
  set(SAN_LINK "-fsanitize=undefined")
  message("Compiling and linking with undefined behavior sanitizer")
else()
  set(SAN "")
  set(SAN_LINK "")
endif()

# Project
project(coretools LANGUAGES CXX)
add_library(${PROJECT_NAME} STATIC)

# Packages
include(FetchContent)
find_package(ZLIB REQUIRED)

option(CONDA "Compiling with/for conda" OFF)
option(RCPP "Using Rcpp" OFF)
option(DEVTOOLS "Using devtools" ON)

if(CONDA AND RCPP)
  message(WARNING "Conda and RCPP on at the same time!")
endif()

# Rcpp
if(RCPP)
  set(DEVTOOLS OFF)
  message(STATUS "Compiling in Rcpp mode")
  # Find R
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")
  find_package(R)
  if(NOT ${R_FOUND})
	message(FATAL_ERROR "No R found!")
  endif()
  # Check if Rcpp and RcppArmadillo include directories are defined (via configure)
  if(RCPP_INCLUDE_DIRS AND RCPP_ARMADILLO_INCLUDE_DIRS)
      message(STATUS "Using Rcpp and RcppArmadillo include directories from configure: ${RCPP_INCLUDE_DIRS} and ${RCPP_ARMADILLO_INCLUDE_DIRS}")
  else()
      message(FATAL_ERROR "RCPP_INCLUDE_DIRS and/or RCPP_ARMADILLO_INCLUDE_DIRS not provided. Please run configure script to set it.")
  endif()
  set(USE_RCPP USE_RCPP)
  # Use custom fast_float that is compatible with CRAN rules
  FetchContent_Declare(fast_float
	URL        https://bitbucket.org/wegmannlab/fast_float/get/b75c74acabf98d9be9d3852466e1078ba93b5111.tar.gz
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/fast_float"
  )
else()
  message(STATUS "Compiling in command-line (non-Rcpp) mode")

  FetchContent_Declare(fast_float
	URL        https://github.com/fastfloat/fast_float/archive/refs/tags/v8.0.2.tar.gz
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/fast_float"
  )
endif()

if(CONDA)
  # conda install cmake ninja gcc gxx armadillo fmt nlohmann_json openmp zlib sysroot_linux-64=2.17
  # CC=$(which gcc) CXX=$(which g++) cmake .. -GNinja -DCONDA=ON -DCMAKE_LIBRARY_PATH=$CONDA_PREFIX/lib
  find_package(Armadillo REQUIRED)
  if (NOT RCPP)
    find_package(fmt REQUIRED)
    target_link_libraries(${PROJECT_NAME} PUBLIC fmt::fmt)
  endif()
elseif (NOT RCPP) # Rcpp does not need armadillo and fmt
  option(FETCHARMA "Fetching Armadillo" ON)
  if(FETCHARMA)
    set(STATIC_LIB ON CACHE BOOL "" FORCE)
    message("Fetching Armadillo")
    FetchContent_Declare(armadillo
      URL        https://sourceforge.net/projects/arma/files/armadillo-14.4.1.tar.xz
      SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/arma"
    )
    FetchContent_MakeAvailable(armadillo)
  else()
    message("Using locally installed Armadillo")
    find_package(Armadillo REQUIRED)
    set(armadillo ${ARMADILLO_LIBRARIES})
  endif ()

  FetchContent_Declare(fmt URL
          https://github.com/fmtlib/fmt/archive/refs/tags/11.1.4.tar.gz
          SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/fmt"
  )
  FetchContent_MakeAvailable(fmt)
  target_link_libraries(${PROJECT_NAME} PUBLIC fmt::fmt)
endif()

option(PARALLEL "Parallelize with OpenMP" ON)
if(PARALLEL AND NOT RCPP) # only allow parallel if Rcpp is not used
  find_package(OpenMP)
  if(OpenMP_CXX_FOUND)
	target_link_libraries(${PROJECT_NAME} PUBLIC OpenMP::OpenMP_CXX)
  endif()
endif()

# This is header-only and therefore no problem with conda
FetchContent_MakeAvailable(fast_float)

file(GLOB_RECURSE CORETOOLS_SOURCES CONFIGURE_DEPENDS core/*.h core/*.cpp)
execute_process(COMMAND git rev-parse HEAD OUTPUT_VARIABLE GIT_HEAD OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
option(CHECK_INTERVALS "Check for valid weakType intervals" OFF)

target_sources(${PROJECT_NAME} PRIVATE ${CORETOOLS_SOURCES})
target_include_directories(${PROJECT_NAME}
  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/core" ${R_INCLUDE_DIRS} ${RCPP_INCLUDE_DIRS} ${RCPP_ARMADILLO_INCLUDE_DIRS}
  SYSTEM INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/core"
        ${R_INCLUDE_DIRS} ${RCPP_INCLUDE_DIRS} ${RCPP_ARMADILLO_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} PUBLIC ${SAN_LINK} armadillo fast_float ${ZLIB_LIBRARIES} ${R_LIBRARY})
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_EXTENSIONS OFF)
target_compile_options(${PROJECT_NAME} PUBLIC ${SAN} PRIVATE -Wall -Wextra)
target_compile_definitions(${PROJECT_NAME} PUBLIC GITVERSION=\"${GIT_HEAD}\" ${USE_RCPP}
  PRIVATE "$<$<BOOL:${CHECK_INTERVALS}>:CHECK_INTERVALS>" "$<$<BOOL:${DEVTOOLS}>:DEVTOOLS>")

# Test
if(BUILD_TESTING)
  FetchContent_Declare(googletest
	URL        https://github.com/google/googletest/archive/refs/tags/v1.16.0.tar.gz
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/googletest"
  )
  FetchContent_MakeAvailable(googletest)

  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  option(gtest_disable_pthreads "Disable uses of pthreads in gtest." ON)

  if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	add_executable("${PROJECT_NAME}_unitTests")
	file(GLOB_RECURSE TEST_SOURCES tests/*.h tests/*.cpp)


	target_sources("${PROJECT_NAME}_unitTests" PRIVATE ${TEST_SOURCES})
	target_include_directories("${PROJECT_NAME}_unitTests" PRIVATE tests)
	target_link_libraries("${PROJECT_NAME}_unitTests" PRIVATE ${PROJECT_NAME} gtest_main gmock_main ${SAN_LINK})
	target_compile_definitions("${PROJECT_NAME}_unitTests" PUBLIC ${USE_RCPP} PRIVATE CHECK_INTERVALS DEVTOOLS)
	target_compile_options(${PROJECT_NAME}_unitTests PRIVATE -Wall -Wextra ${SAN})
  endif()
endif()
