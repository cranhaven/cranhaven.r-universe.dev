cmake_minimum_required(VERSION 3.10)
project(swipl-archive)

include("../cmake/PrologPackage.cmake")

if(POLICY CMP0074)
  cmake_policy(SET CMP0074 NEW)
endif()

# The native FindLibArchive is rather naive.  This allows overruling
# the libarchive dependencies
if(LibArchive_LIBRARIES AND LibArchive_INCLUDE_DIRS)
  set(LibArchive_FOUND ON)
else()
  find_package(LibArchive 3.0.0)
endif()

if(LibArchive_FOUND)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LibArchive_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${LibArchive_INCLUDE_DIRS})

AC_CHECK_FUNCS(archive_read_support_filter_bzip2
	       archive_read_support_filter_compress
	       archive_read_support_filter_gzip
	       archive_read_support_filter_grzip
	       archive_read_support_filter_lrzip
	       archive_read_support_filter_lzip
	       archive_read_support_filter_lzma
	       archive_read_support_filter_lzop
	       archive_read_support_filter_none
	       archive_read_support_filter_rpm
	       archive_read_support_filter_uu
	       archive_read_support_filter_xz)

AC_CHECK_FUNCS(archive_read_support_format_7zip
	       archive_read_support_format_ar
	       archive_read_support_format_cab
	       archive_read_support_format_cpio
	       archive_read_support_format_empty
	       archive_read_support_format_gnutar
	       archive_read_support_format_iso9660
	       archive_read_support_format_lha
	       archive_read_support_format_mtree
	       archive_read_support_format_rar
	       archive_read_support_format_raw
	       archive_read_support_format_tar
	       archive_read_support_format_xar
	       archive_read_support_format_zip)

#introduced in v3.0.0b
AC_CHECK_FUNCS(archive_read_set_seek_callback)

configure_file(config.h.cmake config.h)

swipl_plugin(
    archive
    MODULE archive4pl
    C_SOURCES archive4pl.c
    C_LIBS ${LibArchive_LIBRARIES}
    PL_LIBS archive.pl)
target_include_directories(
    plugin_archive4pl BEFORE PRIVATE
    ${LibArchive_INCLUDE_DIRS})

install_dll(${LibArchive_LIBRARIES})

test_libs(archive)

pkg_doc(archive
	SOURCE archive.pl archive4pl.tex
	DEPENDS archive)

endif(LibArchive_FOUND)
