cmake_minimum_required(VERSION 3.10)
project(swipl-zlib)

include("../cmake/PrologPackage.cmake")

find_package(ZLIB)
if(ZLIB_FOUND)

swipl_plugin(
    zlib
    MODULE zlib4pl
    C_SOURCES zlib4pl.c
    C_LIBS ${ZLIB_LIBRARIES}
    PL_LIBS zlib.pl)
target_include_directories(
    plugin_zlib4pl BEFORE PRIVATE
    ${ZLIB_INCLUDE_DIR})

test_libs(
    zlib
    PACKAGES clib plunit
    TEST_DIRS tests)

pkg_doc(zlib)

endif(ZLIB_FOUND)
