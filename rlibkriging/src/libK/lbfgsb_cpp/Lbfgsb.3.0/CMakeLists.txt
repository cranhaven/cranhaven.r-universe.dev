cmake_minimum_required(VERSION 3.15)

option(EMBED_CUSTOM_BLAS "Embed blas primitive if not available" ON)

message(STATUS "Using Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Using Fortran library flags: ${Fortran_LINK_FLAGS}")

if (CMAKE_GENERATOR MATCHES "Visual Studio")
    message(FATAL_ERROR "This project cannot be build using '${CMAKE_GENERATOR}'")
else()
    message(STATUS "Using CMake generator: ${CMAKE_GENERATOR}")
endif()

project(lbfgsb Fortran)

# Prevent from root system installation
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "default install path" FORCE)
    # Force update for sub-libraries (to follow current installation directive)
    set(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT FALSE CACHE BOOL "Installation prefix has been set" FORCE)
endif ()

set(FORTRAN_SRC
    lbfgsb.F # capital .F to enable pre-processing with compilation directives
    linpack.F
    timer.F
)
set(HEADERS "include/${PROJECT_NAME}/lbfgsb.h")

if (NOT WIN32)
#    Fortran write(*,*) can be re-enabled using this directive
#    target_compile_options(${PROJECT_NAME} LBFGSB_WRITE)
endif()

if (EMBED_CUSTOM_BLAS)
    list(APPEND FORTRAN_SRC blas.F)
    message(WARNING "lbfgsb will use local implementation of blas functions")
endif()

add_library(${PROJECT_NAME} STATIC ${FORTRAN_SRC})

if (EMBED_CUSTOM_BLAS)
    # to avoid collision with outer blas
    # short rename to avoid truncate code interpretation after column #72
    target_compile_definitions(${PROJECT_NAME} PRIVATE dcopy=Wcopy)
    target_compile_definitions(${PROJECT_NAME} PRIVATE dscal=Wscal)
    target_compile_definitions(${PROJECT_NAME} PRIVATE daxpy=Waxpy)
    target_compile_definitions(${PROJECT_NAME} PRIVATE dnrm2=Wnrm2)
    target_compile_definitions(${PROJECT_NAME} PRIVATE ddot=Wdot)
endif()

set(FLIBS_LIST ${Fortran_LINK_FLAGS})
separate_arguments(FLIBS_LIST)

execute_process(COMMAND ${CMAKE_Fortran_COMPILER} -print-search-dirs
        RESULT_VARIABLE HINT_RESULT
        OUTPUT_VARIABLE HINT_OUTPUT
        ERROR_VARIABLE HINT_ERROR
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_STRIP_TRAILING_WHITESPACE)
if (HINT_RESULT)
    message(FATAL_ERROR "Cannot get default compiler search directory: ${HINT_ERROR}")
endif()

# requires CMake ≥3.9
if (HINT_OUTPUT MATCHES "libraries: =(.*)")
    # paths in CMAKE_MATCH_1
    if (WIN32)
        # still separate using ;
        set(HINT_PATHS ${CMAKE_MATCH_1})
    else()
        string(REPLACE ":" ";" HINT_PATHS ${CMAKE_MATCH_1})
    endif()
else()
    message(FATAL_ERROR "Cannot find default compiler search directory")
endif()

if (WIN32)
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a) # no .dll.a
    list(APPEND FLIBS_LIST "-lgcc") # required for timer
endif()

foreach(ITEM ${FLIBS_LIST})
    message(STATUS "Processing external lib option: ${ITEM}")
    string(SUBSTRING "${ITEM}" 0 2 SYMBOL)
    string(SUBSTRING "${ITEM}" 2 -1 VALUE)
    if (SYMBOL STREQUAL "-L")
        list(APPEND HINT_PATHS "${VALUE}")
        # Prefer full path link to prevent from adjustments (suffix,prefix,path) done MSVC when searching libs  
        # target_link_directories(${PROJECT_NAME} PRIVATE "${VALUE}")
    elseif (SYMBOL STREQUAL "-l")
        find_library(LIB_${VALUE} NAMES ${VALUE} HINTS ${HINT_PATHS})
        message(STATUS "Library ${VALUE} found at location ${LIB_${VALUE}}")
        target_link_libraries(${PROJECT_NAME} PUBLIC "${LIB_${VALUE}}")
    else()
        message(FATAL_ERROR "Unknown option: ${ITEM}")
    endif()
endforeach()

target_include_directories(
    ${PROJECT_NAME} INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

set_target_properties(${PROJECT_NAME} PROPERTIES POSITION_INDEPENDENT_CODE TRUE)
target_compile_options(${PROJECT_NAME} PRIVATE -frecursive)

# créer des variables de cache pour les destinations d'installation
include(GNUInstallDirs)

install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}Targets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT devel
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT runtime
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME} COMPONENT devel
        )

install(FILES ${HEADERS} DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}")

install(EXPORT ${PROJECT_NAME}Targets
        FILE ${PROJECT_NAME}Config.cmake
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION lib/cmake/${PROJECT_NAME}
)