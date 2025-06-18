# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.

# Common: Find C/C++ compilers, deal with ccache, find the architecture
# and find CMake.
is_windows = identical(.Platform$OS.type, "windows")
is_macos = identical(Sys.info()[['sysname']], "Darwin")

CC_RAW = r_cmd_config("CC")
CXX_RAW = r_cmd_config("CXX")

CC_ARGS = strsplit(CC_RAW, " ")[[1]]
CXX_ARGS = strsplit(CXX_RAW, " ")[[1]]

uses_ccache = FALSE
if (grepl("ccache", CC_ARGS[1])) {
	uses_ccache = TRUE
	CC = paste(CC_ARGS[-1], collapse = " ")
} else {
	CC = CC_ARGS[1]
}

if (grepl("ccache", CXX_ARGS[1])) {
	uses_ccache = TRUE
	CXX = paste(CXX_ARGS[-1], collapse = " ")
} else {
	CXX = CXX_ARGS[1]
}

CC_COMPILER = strsplit(CC, " ")[[1]][1]
CXX_COMPILER = strsplit(CXX, " ")[[1]][1]

CC_FULL = normalizePath(
	Sys.which(CC_COMPILER),
	winslash = "/"
)
CXX_FULL = normalizePath(
	Sys.which(CXX_COMPILER),
	winslash = "/"
)
TARGET_ARCH = Sys.info()[["machine"]]
PACKAGE_BASE_DIR = normalizePath(getwd(), winslash = "/")

syswhich_cmake = Sys.which("cmake")

if (syswhich_cmake != "") {
	CMAKE = normalizePath(syswhich_cmake, winslash = "/")
} else {
	if (is_macos) {
		cmake_found_in_app_folder = file.exists(
			"/Applications/CMake.app/Contents/bin/cmake"
		)
		if (cmake_found_in_app_folder) {
			CMAKE = normalizePath("/Applications/CMake.app/Contents/bin/cmake")
		} else {
			stop("CMake not found during configuration.")
		}
	} else {
		stop("CMake not found during configuration.")
	}
}

# Now, library specific below

# Use pkg-config (if available) to find a system library
package_name = "libImath"
static_library_name = "libImath-3_2"

package_version = "3.1.9"
lib_system = ""

pkgconfig_path = Sys.which("pkg-config")

lib_exists = FALSE
LIB_INCLUDE_LINE = ""
LIB_LINK_LINE = ""

if (nzchar(pkgconfig_path)) {
	pc_status = system2(
		pkgconfig_path,
		c("--exists", sprintf("'%s >= %s'", package_name, package_version)),
		stdout = FALSE,
		stderr = FALSE
	)

	lib_exists = pc_status == 0

	if (lib_exists) {
		message(
			"*** configure: system libImath-3_2 exists, using that for building the library"
		)

		lib_include = system2(
			pkgconfig_path,
			c("--cflags", package_name),
			stdout = TRUE
		)
		message(
			sprintf("*** configure: using include path '%s'", lib_include)
		)
		lib_link = system2(
			pkgconfig_path,
			c("--static", "--libs", package_name),
			stdout = TRUE
		)
		message(
			sprintf("*** configure: using link path '%s'", lib_link)
		)
		LIB_INCLUDE_LINE = sprintf("LIB_INCLUDE = %s", lib_include)
		LIB_LINK_LINE = sprintf("LIB_LINK = %s", lib_link)
	}
}

if (!lib_exists) {
	fallback_prefixes = c(
		"/opt/R/arm64",
		"/opt/R/x86_64",
		"/opt/homebrew",
		"/usr/local",
		"/usr"
	)

	for (prefix in fallback_prefixes) {
		lib_exists_check = file.exists(file.path(
			prefix,
			"lib",
			sprintf("%s.a", static_library_name)
		))
		header_exists = dir.exists(file.path(prefix, "include", "Imath"))

		if (lib_exists_check && header_exists) {
			lib_exists = TRUE
			lib_link = file.path(
				prefix,
				"lib"
			)
			lib_include = file.path(
				prefix,
				"include"
			)
			LIB_INCLUDE_LINE = sprintf("LIB_INCLUDE = %s", lib_include)
			LIB_LINK_LINE = sprintf("LIB_LINK = %s", lib_link)
			break
		}
	}
}


is_clang = grepl("clang", r_cmd_config("CXX"), fixed = TRUE)
use_libcpp = is_clang && is_macos

clang_flag = ""
add_pp = FALSE
if (is_clang) {
	clang_flag = if (use_libcpp) "-stdlib=libc++" else ""
}

if (lib_exists) {
	lib_dir = substr(strsplit(lib_link, " ")[[1]][1], 3, 500)
	message(
		sprintf(
			"*** configure.R: Found installed version of %s with correct version at '%s', will link in that version to the package.",
			package_name,
			lib_dir
		)
	)
}

define(
	PACKAGE_BASE_DIR = PACKAGE_BASE_DIR,
	TARGET_ARCH = TARGET_ARCH,
	CMAKE = CMAKE,
	CC_FULL = CC_FULL,
	CXX_FULL = CXX_FULL,
	LIB_EXISTS = as.character(lib_exists),
	LIB_INCLUDE_LINE = LIB_INCLUDE_LINE,
	LIB_LINK_LINE = LIB_LINK_LINE
)

# Everything below here is package specific CMake stuff
if (!dir.exists("src/Imath/build")) {
	dir.create("src/Imath/build")
}

file_cache = "src/Imath/build/initial-cache.cmake"
writeLines(
	sprintf(
		r"-{set(CMAKE_C_COMPILER "%s" CACHE FILEPATH "C compiler")
set(CMAKE_CXX_COMPILER "%s" CACHE FILEPATH "C++ compiler")
set(CMAKE_C_FLAGS "-g -fPIC -fvisibility=hidden" CACHE STRING "C flags")
set(CMAKE_CXX_FLAGS "%s -g -fPIC -fvisibility=hidden -fvisibility-inlines-hidden" CACHE STRING "C++ flags")
set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL "Position independent code")
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type")
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libs")
set(CMAKE_OSX_ARCHITECTURES "%s" CACHE STRING "Target architecture")}-",
		CC_FULL,
		CXX_FULL,
		clang_flag,
		TARGET_ARCH
	),
	file_cache
)


inst_dir = file.path(PACKAGE_BASE_DIR, "inst") # ${PACKAGE_BASE_DIR}/inst
dir.create(inst_dir, recursive = TRUE, showWarnings = FALSE)

include_dir = file.path(inst_dir, "include")
if (!dir.exists(include_dir)) {
	dir.create(include_dir)
}
lib_dir = file.path(inst_dir, "lib")
if (!dir.exists(lib_dir)) {
	dir.create(lib_dir)
}
lib_arch = file.path(lib_dir, TARGET_ARCH)

if (!dir.exists(lib_arch)) {
	dir.create(lib_arch)
}

build_dir = file.path(PACKAGE_BASE_DIR, "src/Imath/build") # already created earlier
src_dir = ".." # evaluated inside build/

cmake_cfg = c(
	src_dir,
	"-C",
	"../build/initial-cache.cmake",
	"-DCMAKE_INSTALL_PREFIX=\"../../../inst\"",
	paste0("-DCMAKE_INSTALL_LIBDIR=lib/", TARGET_ARCH),
	"-DCMAKE_INSTALL_INCLUDEDIR=include",
	"-DIMATH_INSTALL_PKG_CONFIG=ON",
	"-DBUILD_SHARED_LIBS=OFF",
	"-DIMATH_IS_SUBPROJECT=ON",
	"-DCMAKE_BUILD_TYPE=Release",
	"-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
)

setwd(build_dir)

status = system2(CMAKE, cmake_cfg)
if (status != 0) stop("CMake configure step failed")

setwd(PACKAGE_BASE_DIR)

lf_ify = function(path) {
	if (!file.exists(path)) return(invisible())
	txt = readLines(path, warn = FALSE) # strips CR automatically
	writeLines(txt, path, sep = "\n", useBytes = TRUE)
}

if (!is_windows) {
	configure_file("src/Makevars.in")
} else {
	configure_file("src/Makevars.win.in")
}

lf_ify("src/Makevars")
lf_ify("src/Makevars.win")
