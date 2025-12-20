cmake_minimum_required(VERSION 3.10)
project(swipl-yaml)

include("../cmake/PrologPackage.cmake")
find_package(LibYAML)

if(LIBYAML_FOUND)

AC_CHECK_FUNCS(poll)

configure_file(config.h.cmake config.h)

swipl_plugin(
    yaml
    MODULE yaml4pl
    C_SOURCES yaml4pl.c
    C_LIBS ${YAML_LIBRARY}
    C_INCLUDE_DIR ${LIBYAML_INCLUDE_DIR}
    PL_LIBS yaml.pl)

install_dll(${YAML_LIBRARY})

test_libs(
    yaml
    PACKAGES clib plunit
    TEST_DIRS examples)

pkg_doc(yaml
	SECTION
	SOURCE yaml.pl libyaml.tex)

endif(LIBYAML_FOUND)
