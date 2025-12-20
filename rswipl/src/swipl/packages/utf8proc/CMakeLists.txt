cmake_minimum_required(VERSION 3.10)
project(swipl-utf8proc)

include("../cmake/PrologPackage.cmake")
configure_file(config.h.cmake config.h)

swipl_plugin(
    utf8proc
    MODULE unicode4pl
    C_SOURCES unicode4pl.c utf8proc.c
    PL_LIBS unicode.pl)

pkg_doc(utf8proc
	SECTION
	    unicode.pl)
