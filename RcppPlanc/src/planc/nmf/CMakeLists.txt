cmake_minimum_required(VERSION 3.21...3.29 FATAL_ERROR)
#Copyright 2016 Ramakrishnan Kannan
include(GenerateExportHeader)
include(FetchContent)
#include(${PROJECT_SOURCE_DIR}/common/cmake_aux/NMFLIB_FindARMA.cmake)

# the settings below will be automatically configured by the rest of this script

#add_library(sparse_nmf OBJECT SparseNMFDriver.cpp)
#add_library(dense_nmf OBJECT NMFDriver.cpp)
add_library(nmflib
        nmf_lib.cpp bppnmf.cpp ../nnls/nnls_lib.cpp
        ../common/utils.cc ../common/data.cpp)
# add_executable(${DENSE_OR_SPARSE}_inmf inmf.cpp)
# target_compile_features(${DENSE_OR_SPARSE}_inmf PRIVATE cxx_std_17)
# add_compile_options(-O0)
target_include_directories(nmflib PUBLIC ${PLANC_COMMON} ${NNLS_SRC} ${OpenMP_CXX_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(nmflib INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(nmflib PUBLIC utils hw_detect detect_blas)
generate_export_header(nmflib)
if(BUILD_RCPP)
    if(WIN32)
        add_custom_command(TARGET nmflib POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E remove ${HighFive_SOURCE_DIR}/../highfive-subbuild/Makefile
                VERBATIM)
        add_custom_command(TARGET nmflib POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_CURRENT_BINARY_DIR}/../CMakeFiles/${CMAKE_VERSION}/CompilerIdC/CMakeCCompilerId.c
                VERBATIM)
        add_custom_command(TARGET nmflib POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_CURRENT_BINARY_DIR}/../CMakeFiles/${CMAKE_VERSION}/CompilerIdCXX/CMakeCXXCompilerId.cpp
                VERBATIM)
        add_custom_command(TARGET nmflib POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_CURRENT_BINARY_DIR}/Makefile
                VERBATIM)
        add_custom_command(TARGET nmflib POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E remove ${CMAKE_CURRENT_BINARY_DIR}/../Makefile
                VERBATIM)
    endif()
endif()
# target_link_libraries(${DENSE_OR_SPARSE}_inmf ${NMFLIB_LIBS})
#install(TARGETS  plancnmf nmflib
#        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
#        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} )
