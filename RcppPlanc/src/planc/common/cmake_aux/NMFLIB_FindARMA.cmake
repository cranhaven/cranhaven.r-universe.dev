# Copyright 2016 Ramakrishnan Kannan
# Find Armadillo, BLAS and LAPACK.

#include(${PROJECT_SOURCE_DIR}/common/cmake_aux/FindSuperLU.cmake)

find_path(ARMADILLO_INCLUDE_DIR
  NAMES armadillo
  PATHS "$ENV{ProgramFiles}/Armadillo/include"
  )

message(STATUS "     ARMA_FOUND = ${ARMADILLO_INCLUDE_DIR}")

set(NMFLIB_USE_LAPACK           false)
set(NMFLIB_USE_BLAS             false)
set(NMFLIB_USE_ATLAS            false)

OPTION(CMAKE_IGNORE_MKL "Build Ignoring MKL" ON)

#to build sparse comment or uncomment this line.
#OPTION(CMAKE_BUILD_SPARSE "Build Sparse" OFF)
#if(CMAKE_BUILD_SPARSE)
#  add_definitions(-DBUILD_SPARSE=1)
#endif()

OPTION(CMAKE_WITH_BARRIER_TIMING "Barrier placed to collect time" ON)
if(CMAKE_WITH_BARRIER_TIMING)
  add_definitions(-D__WITH__BARRIER__TIMING__=1)
endif()

#C++11 standard
set (CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
#set (CMAKE_CXX_EXTENSIONS OFF) # use -std=c++11 instead of -std=gnu++11

#while exception get the stack trace
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -rdynamic")

set(CMAKE_MODULE_PATH ${ARMADILLO_INCLUDE_DIR}/../cmake_aux/Modules/)
message(STATUS "CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}" )

include(CheckIncludeFileCXX)
include(CheckLibraryExists)

##
## Find LAPACK and BLAS libraries, or their optimised versions
##

set(NMFLIB_OS unix)
set(ARMADILLO_INCLUDE_DIRS ${ARMADILLO_INCLUDE_DIR}/../)
set(ARMADILLO_LIBRARY_DIRS ${ARMADILLO_INCLUDE_DIR}/../)

# for cray wrapper required is failing

find_package(BLAS)
find_package(LAPACK)
find_package(HDF5)

message(STATUS "    BLAS_FOUND = ${BLAS_FOUND}"    )
message(STATUS "  LAPACK_FOUND = ${LAPACK_FOUND}"  )
message(STATUS "    HDF5_FOUND = ${HDF5_FOUND}")

set(NMFLIB_USE_BLAS true)
set(NMFLIB_USE_LAPACK true)
 if(HDF5_FOUND)
  if(DEFINED HDF5_LIBRARIES)
  set(NMFLIB_LIBS ${NMFLIB_LIBS} ${HDF5_LIBRARIES})
  elseif(DEFINED ${HDF5_hdf5_LIBRARY_RELEASE})
  set(NMFLIB_LIBS ${NMFLIB_LIBS} ${HDF5_hdf5_LIBRARY_RELEASE})
  endif()
endif()
if(BLAS_FOUND)
  set(NMFLIB_LIBS ${NMFLIB_LIBS} ${BLAS_LIBRARIES})
endif()
if(LAPACK_FOUND)
  set(NMFLIB_LIBS ${NMFLIB_LIBS} ${LAPACK_LIBRARIES})
endif()
if(SUPERLU_FOUND)
  set(NMFLIB_LIBS ${NMFLIB_LIBS} ${SUPERLU_LIBRARIES})
endif()
find_package(OpenMP)
if(OpenMP_CXX_FOUND)
add_compile_options(${OpenMP_CXX_FLAGS})
include_directories(${OpenMP_CXX_INCLUDE_DIRS})
endif()
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DNDEBUG -g3 -O0" CACHE STRING "CXX_FLAGS_DEBUG" FORCE )
#set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3" CACHE STRING "CXX_FLAGS_RELEASE" FORCE )
#set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-as-needed" CACHE STRING "SHARED_LINKER_FLAGS" FORCE)

#if(DEFINED CMAKE_CXX_COMPILER_ID AND DEFINED CMAKE_CXX_COMPILER_VERSION)
#  if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 4.8.3)
#    set(NMFLIB_USE_EXTERN_CXX11_RNG true)
#    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#    message(STATUS "Detected gcc 4.8.3 or later. Added '-std=c++11' to compiler flags")
#  endif()
#endif()

#OPTION(CMAKE_BUILD_CUDA "Build with CUDA/NVBLAS" OFF)
if(CMAKE_BUILD_CUDA)
  find_package(CUDA REQUIRED)
else()
  find_package(CUDA)
endif()
message(STATUS " CUDA_FOUND = ${CUDA_FOUND}" )
if (CUDA_FOUND)
  add_definitions(-DBUILD_CUDA=1)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
  set(NMFLIB_LIBS ${CUDA_LIBRARIES} ${NMFLIB_LIBS})
  set(NMFLIB_LIBS ${CUDA_CUBLAS_LIBRARIES} ${NMFLIB_LIBS})
  find_library(CUDA_NVBLAS_LIBRARY
                NAMES nvblas
                PATHS ${CUDA_TOOLKIT_ROOT_DIR}
                PATH_SUFFIXES lib64
                NO_DEFAULT_PATH)
  set(NMFLIB_LIBS ${CUDA_NVBLAS_LIBRARY} ${NMFLIB_LIBS})
  include_directories(${CUDA_INCLUDE_DIRS})
endif()
#endif()
