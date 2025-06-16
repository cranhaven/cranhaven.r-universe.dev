cmake_minimum_required(VERSION 3.15)
project(slapack Fortran)

# Prevent from root system installation
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "default install path" FORCE)
    # Force update for sub-libraries (to follow current installation directive)
    set(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT FALSE CACHE BOOL "Installation prefix has been set" FORCE)
endif ()

file(GLOB BLAS_SRC BLAS/*.f)
file(GLOB LAPACK_SRC LAPACK/*.f)
file(GLOB UTIL_SRC UTIL/*.f)
set(FORTRAN_SRC ${BLAS_SRC} ${LAPACK_SRC} ${UTIL_SRC})

# Generate position independent (aka -fPIC) code even for static libs
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
add_library(${PROJECT_NAME} STATIC ${FORTRAN_SRC})

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
