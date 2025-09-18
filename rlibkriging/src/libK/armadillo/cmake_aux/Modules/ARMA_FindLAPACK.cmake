# - Find a LAPACK library (no includes)
# This module defines
#  LAPACK_LIBRARIES, the libraries needed to use LAPACK.
#  LAPACK_FOUND, If false, do not try to use LAPACK.
# also defined, but not for general use are
#  LAPACK_LIBRARY, where to find the LAPACK library.

SET(LAPACK_NAMES ${LAPACK_NAMES} lapack Rlapack)
FIND_LIBRARY(LAPACK_LIBRARY
  NAMES ${LAPACK_NAMES}
  PATHS ${CMAKE_SYSTEM_LIBRARY_PATH} /usr/lib64/atlas /usr/lib/atlas /usr/lib64 /usr/lib /usr/local/lib64 /usr/local/lib /opt/R-devel/lib64/R/lib
  )


IF (UNIX AND LAPACK_LIBRARY)
    # Could be a partial version of Lapack without simple precision arithmetics
    get_filename_component(LAPACK_LIBRARY_DIR "${LAPACK_LIBRARY}" DIRECTORY)
    get_filename_component(LAPACK_LIBRARY_NAME "${LAPACK_LIBRARY}" NAME_WE)
    IF (LAPACK_LIBRARY_NAME STREQUAL "libRlapack")
        # Sounds a lapack lib embedded with R: high risk
        include(CheckCSourceCompiles)
        set(CMAKE_REQUIRED_LINK_OPTIONS -L${LAPACK_LIBRARY_DIR} -lRlapack -lRblas)
        check_c_source_compiles("
            void cgees_(); // bad signature, doesn't matter; just check existing symbol
            void xerbla_() { } // usually provided by R
            int main() { cgees_(); }"
            HAS_SIMPLE_PRECISION_LAPACK)
        IF (NOT HAS_SIMPLE_PRECISION_LAPACK)
            message(STATUS "Requires extra slapack to get 32bits arithmetics")
            include(${CMAKE_CURRENT_SOURCE_DIR}/cmake_aux/Tools/build_external_project.cmake)
            build_external_project(slapack "${CMAKE_CURRENT_SOURCE_DIR}/../../slapack")
            # build_external_project(slapack "file:///local/path/for/testing")
            find_package(slapack REQUIRED PATHS "${CMAKE_CURRENT_BINARY_DIR}/ExternalProjects/slapack" NO_DEFAULT_PATH)
            get_target_property(SLAPACK_LIBRARY slapack::slapack IMPORTED_LOCATION_NOCONFIG)
        ENDIF()
    ENDIF()
ENDIF()


IF (LAPACK_LIBRARY)
  SET(LAPACK_LIBRARIES ${LAPACK_LIBRARY} ${SLAPACK_LIBRARY})
  SET(LAPACK_FOUND "YES")
ELSE (LAPACK_LIBRARY)
  SET(LAPACK_FOUND "NO")
ENDIF (LAPACK_LIBRARY)


IF (LAPACK_FOUND)
   IF (NOT LAPACK_FIND_QUIETLY)
      MESSAGE(STATUS "Found LAPACK: ${LAPACK_LIBRARIES}")
   ENDIF (NOT LAPACK_FIND_QUIETLY)
ELSE (LAPACK_FOUND)
   IF (LAPACK_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "Could not find LAPACK")
   ENDIF (LAPACK_FIND_REQUIRED)
ENDIF (LAPACK_FOUND)

# Deprecated declarations.
GET_FILENAME_COMPONENT (NATIVE_LAPACK_LIB_PATH ${LAPACK_LIBRARY} PATH)

MARK_AS_ADVANCED(
  LAPACK_LIBRARY
  )
