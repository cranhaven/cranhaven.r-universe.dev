find_package(PkgConfig QUIET)
pkg_check_modules(PC_LIBYAML QUIET yaml)

find_path(LIBYAML_INCLUDE_DIR
	  NAMES yaml.h
	  HINTS ${PC_LIBYAML_INCLUDEDIR}
	        ${PC_LIBYAML_INCLUDE_DIRS})
find_library(YAML_LIBRARY
	     NAMES yaml
	     HINTS ${PC_LIBXML_LIBDIR}
	           ${PC_LIBXML_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LibYAML
    REQUIRED_VARS LIBYAML_INCLUDE_DIR YAML_LIBRARY)

mark_as_advanced(LIBYAML_INCLUDE_DIR
		 YAML_LIBRARY)
