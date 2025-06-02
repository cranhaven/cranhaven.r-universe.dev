cmake_minimum_required(VERSION 3.14)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Policies (for properly setting the timestamps of extracted contents)
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)

if (POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif (POLICY CMP0135)

# To avoid warnings when compaling on MacOS on CRAN
set(CMAKE_OSX_DEPLOYMENT_TARGET "13.7" CACHE STRING "Minimum OS X deployment version")

# Project
project(birp LANGUAGES CXX)

# Needed for Language Server Protocol
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Download coretools and stattools
include(FetchContent)
FetchContent_Declare(coretools
        URL https://bitbucket.org/wegmannlab/coretools/get/5ed27e9a555391f905af786fae83dd9cf15d5146.tar.gz
        SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}/coretools"
)

FetchContent_Declare(stattools
        URL https://bitbucket.org/wegmannlab/stattools/get/be368b7504bbad7b27eab89aa2d14f6344c3832b.tar.gz
        SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}/stattools"
)
FetchContent_MakeAvailable(coretools stattools)

#######################
# Create core library
#######################

# assemble list of cpp and h files in core folder
file(GLOB_RECURSE CORE_SOURCES core/*.h core/*.cpp)

#Core library
add_library(coreLib STATIC ${CORE_SOURCES})

target_sources(coreLib PRIVATE ${CORE_SOURCES})
target_include_directories(coreLib PRIVATE core SYSTEM INTERFACE core)
target_link_libraries(coreLib PUBLIC coretools stattools)
target_compile_features(coreLib PUBLIC cxx_std_17)
set_target_properties(coreLib PROPERTIES CXX_EXTENSIONS OFF)
target_compile_options(coreLib PRIVATE -Wall -Wextra)

#######################
# Executable
#######################

# add executable to be built
add_executable(${PROJECT_NAME}
        main.cpp
        )

# link libraries
target_link_libraries(${PROJECT_NAME} PUBLIC
        coreLib
        )
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_17)
target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra)

