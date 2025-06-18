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

#Use pkg-config (if available) to find a system library
LIB_SYSTEM = ""
package_name = "libdeflate"
package_version = "1.24"

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
			sprintf(
				"*** configure: system %s exists, using that for building the library",
				package_name
			)
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
			sprintf("%s.a", package_name)
		))
		header_exists = file.exists(file.path(prefix, "include", "libdeflate.h"))
		if (header_exists) {
			header_lines = readLines(file.path(prefix, "include", "libdeflate.h"))
			major_version_line = grepl("LIBDEFLATE_VERSION_MAJOR", header_lines)
			minor_version_line = grepl("LIBDEFLATE_VERSION_MINOR", header_lines)
			stopifnot(sum(major_version_line) == 1)
			stopifnot(sum(minor_version_line) == 1)
			major_version = as.integer(strsplit(
				header_lines[major_version_line],
				"\\s+"
			)[[1]][3])
			minor_version = as.integer(strsplit(
				header_lines[minor_version_line],
				"\\s+"
			)[[1]][3])
			if (major_version > 1 || minor_version < 24) {
				message(sprintf(
					"System install of libdeflate v%i.%i found, but is not of a suitable version (>= v1.24) ",
					major_version,
					minor_version
				))
				header_exists = FALSE
			}
		}
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

REASON_FOR_BUILDING = r"{"==> Building bundled libdeflate for linking and downstream packages that build and link bundled static libraries that use CMake"}"
if (lib_exists) {
	lib_dir = substr(strsplit(lib_link, " ")[[1]][1], 3, 500)
	message(
		sprintf(
			"*** configure.R: Found installed version of %s with correct version at '%s', will link in that version to the package.",
			package_name,
			lib_dir
		)
	)
	REASON_FOR_BUILDING = r"{"==> Building bundled libdeflate for downstream packages that build and link bundled static libraries that use CMake"}"
}

define(
	PACKAGE_BASE_DIR = PACKAGE_BASE_DIR,
	TARGET_ARCH = TARGET_ARCH,
	CMAKE = CMAKE,
	CC_FULL = CC_FULL,
	CXX_FULL = CXX_FULL,
	LIB_EXISTS = as.character(lib_exists),
	LIB_INCLUDE = LIB_INCLUDE_LINE,
	LIB_LINK = LIB_LINK_LINE,
	REASON_FOR_BUILDING = REASON_FOR_BUILDING
)

# Everything below here is package specific

if (!dir.exists("src/libdeflate/build")) {
	dir.create("src/libdeflate/build")
}

file_cache = "src/libdeflate/build/initial-cache.cmake"
writeLines(
	sprintf(
		r"-{set(CMAKE_C_COMPILER "%s" CACHE FILEPATH "C compiler")
  set(CMAKE_CXX_COMPILER "%s" CACHE FILEPATH "C++ compiler")
  set(CMAKE_C_FLAGS "-fPIC -fvisibility=hidden" CACHE STRING "C flags")
  set(CMAKE_CXX_FLAGS "-fPIC -fvisibility=hidden -fvisibility-inlines-hidden" CACHE STRING "C++ flags")
  set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL "Position independent code")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type")
  set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libs")
  set(CMAKE_OSX_ARCHITECTURES "%s" CACHE STRING "Target architecture")}-",
		CC_FULL,
		CXX_FULL,
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

build_dir = file.path(PACKAGE_BASE_DIR, "src/libdeflate/build") # already created earlier
src_dir = ".." # evaluated inside build/

cmake_cfg = c(
	src_dir,
	"-C",
	"../build/initial-cache.cmake",
	"-DCMAKE_INSTALL_PREFIX=\"../../../inst\"",
	paste0("-DCMAKE_INSTALL_LIBDIR=lib/", TARGET_ARCH),
	"-DCMAKE_INSTALL_INCLUDEDIR=include",
	"-DLIBDEFLATE_BUILD_GZIP=OFF",
	"-DLIBDEFLATE_BUILD_SHARED_LIB=OFF",
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
