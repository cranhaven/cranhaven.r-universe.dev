cmake_minimum_required(VERSION 3.10)
project(swipl-bdb)

include("../cmake/PrologPackage.cmake")

find_package(BDB)
if(BDB_FOUND)
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${BDB_INCLUDE_DIR})

include(CheckStructHasMember)
check_struct_has_member(DB_ENV set_rpc_server db.h HAVE_SET_RPC_SERVER)
check_struct_has_member(DB_ENV set_server     db.h HAVE_SET_SERVER)

configure_file(config.h.cmake config.h)

swipl_plugin(
    bdb4pl
    C_SOURCES bdb4pl.c
    THREADED C_LIBS ${BDB_LIBRARY}
    PL_LIBS bdb.pl)
target_include_directories(
    plugin_bdb4pl BEFORE PRIVATE
    ${BDB_INCLUDE_DIR})

install_dll(${BDB_LIBRARY})

pkg_doc(bdb4pl
	SOURCE bdb.pl)

test_libs(bdb)

endif(BDB_FOUND)
