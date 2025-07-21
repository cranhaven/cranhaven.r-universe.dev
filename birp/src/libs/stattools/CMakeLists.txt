cmake_minimum_required(VERSION 3.14)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Project
project(stattools LANGUAGES CXX)

# Library
add_library(${PROJECT_NAME} STATIC)

file(GLOB_RECURSE STATTOOLS_SOURCES CONFIGURE_DEPENDS core/*.h core/*.cpp)

include(FetchContent)
FetchContent_Declare(coretools
        GIT_REPOSITORY https://bitbucket.org/wegmannlab/coretools.git
        GIT_TAG master
        SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/coretools"
)

FetchContent_MakeAvailable(coretools)

# link with google profiler?
# If you want to run gperftools:
# 1) compile cmake with command "cmake -DPROFILING=ON .."
# 2) generate pdf from profile.prof by entering 'google-pprof --pdf ./stattools profile.prof > profiling.pdf' in the console
option(PROFILING "Linking with profiler" OFF)
if (PROFILING)
    set(Gperftools_DIR "${CMAKE_CURRENT_LIST_DIR}/")
    find_package(Gperftools)
    # add compile definition to source files (everything behind -D will be replaced with bool)
    if (GPERFTOOLS_FOUND)
        add_definitions(-DWITH_PROFILING)
    endif ()
endif ()

target_sources(${PROJECT_NAME} PRIVATE ${STATTOOLS_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/core"
        SYSTEM INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/core")
target_link_libraries(${PROJECT_NAME} PUBLIC coretools
        ${ZLIB_LIBRARIES} armadillo ${GPERFTOOLS_LIBRARIES})
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_EXTENSIONS OFF)
target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra)
target_compile_definitions(${PROJECT_NAME} PUBLIC DEVTOOLS)

# Test
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
  # Unit-Test
  add_executable("${PROJECT_NAME}_unitTests")

  file(GLOB_RECURSE UTEST_SOURCES core/stattools/commonTestCases/* tests/unittests/*)

  target_sources("${PROJECT_NAME}_unitTests" PRIVATE ${UTEST_SOURCES})
  target_include_directories("${PROJECT_NAME}_unitTests" PUBLIC tests/unittests ../coretools/tests ../coretools/core/IntegrationTests)
  target_link_libraries("${PROJECT_NAME}_unitTests" PUBLIC ${PROJECT_NAME} gtest_main gmock_main)
  target_compile_definitions("${PROJECT_NAME}_unitTests" PRIVATE CHECK_INTERVALS)
  target_compile_options("${PROJECT_NAME}_unitTests" PRIVATE -Wall -Wextra)

  # Integration-Test library
  add_executable(${PROJECT_NAME}_integrationTests EXCLUDE_FROM_ALL tests/integrationtests/TIntegrationTestMCMC.cpp)

  file(GLOB_RECURSE ITEST_SOURCES core/stattools/commonTestCases/* tests/integrationtests/integrationtests/*)

  target_sources("${PROJECT_NAME}_integrationTests" PRIVATE ${ITEST_SOURCES})
  target_include_directories("${PROJECT_NAME}_integrationTests" PUBLIC tests/integrationtests/integrationtests)
  target_compile_features(${PROJECT_NAME}_integrationTests PUBLIC cxx_std_17)
  target_link_libraries("${PROJECT_NAME}_integrationTests" PUBLIC ${PROJECT_NAME})
  target_compile_definitions("${PROJECT_NAME}_integrationTests" PRIVATE CHECK_INTERVALS)
  target_compile_options("${PROJECT_NAME}_integrationTests" PRIVATE -Wall -Wextra)
endif()
