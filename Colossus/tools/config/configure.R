# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

gcc_version <- function() {
  out <- tryCatch(processx::run("c++", "-v", stderr_to_stdout = TRUE),
                  error = function(cnd) list(stdout = ""))
  out0 <- stringr::str_match(out$stdout, "gcc version")[1]
  if (!is.na(out0)){
  	out <- "gcc"
  } else {
    out0 <- stringr::str_match(out$stdout, "clang version")[1]
    if (!is.na(out0)){
      out <- "clang"
    } else {
      out <- out$stdout
    }
  }
  out
}

Rcomp_version <- function() {
  out <- callr::rcmd("config","CC")
  out0 <- stringr::str_match(out$stdout, "clang")[1]
  if (!is.na(out0)){
  	out <- "clang"
  } else {
    out0 <- stringr::str_match(out$stdout, "gcc")[1]
    if (!is.na(out0)){
      out <- "gcc"
    } else {
      out <- out$stdout
    }
  }
  out
}

os <- get_os()
cpp_compiler <- gcc_version()
R_compiler <- Rcomp_version()


if (os=="linux"){
	if (cpp_compiler=="gcc"){
	    if (R_compiler=="gcc"){
            print("CONFIG NOTE: Identified linux system with c++ default to gcc and R compiled with gcc")
            print("CONFIG NOTE: Building linux with gcc and openmp support")
		    define(PKG_CXXFLAGS = "PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)")
		    define(PKG_LIBS = 'PKG_LIBS = `$(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()"` $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) $(SHLIB_OPENMP_CXXFLAGS)')
		    define(PKG_CPPFLAGS = "#PKG_CPPFLAGS")
		    define(LDFLAGS = "#LDFLAGS")
		    configure_file("src/Makevars.in")
	    } else {
            print("CONFIG NOTE: Identified linux system with c++ default to gcc and R compiled with clang")
            print("CONFIG NOTE: Building linux with clang and no openmp support")
	        define(PKG_CXXFLAGS = "#PKG_CXXFLAGS")
		    define(PKG_LIBS = 'PKG_LIBS = `$(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()"` $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)')
		    define(PKG_CPPFLAGS = "#PKG_CPPFLAGS")
		    define(LDFLAGS = "#LDFLAGS")
		    configure_file("src/Makevars.in")
	    }
	} else if (cpp_compiler=='clang'){
        print("CONFIG NOTE: Identified linux system with c++ default to clang")
        print("CONFIG NOTE: Building linux with clang and no openmp support")
		define(PKG_CXXFLAGS = "#PKG_CXXFLAGS")
		define(PKG_LIBS = 'PKG_LIBS = `$(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()"` $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)')
		define(PKG_CPPFLAGS = "#PKG_CPPFLAGS")
		define(LDFLAGS = "#LDFLAGS")
		configure_file("src/Makevars.in")
	}
} else if (os=="osx"){
    print("CONFIG NOTE: Building mac and allowing xclang openmp support")
    print("CONFIG NOTE: Building with no openmp support")
    define(PKG_CXXFLAGS = "PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)")
    define(PKG_LIBS = 'PKG_LIBS = `$(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()"` $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) $(SHLIB_OPENMP_CXXFLAGS) -lomp')
    define(PKG_CPPFLAGS = "CPPFLAGS += -Xclang -fopenmp")
    define(LDFLAGS = "LDFLAGS += -lomp")
    configure_file("src/Makevars.in")
} else {
    print("CONFIG NOTE: Building windows with openmp support")
    # print(paste("OS",os,sep=" "))
    define(PKG_CXXFLAGS = "PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)")
    define(PKG_LIBS = 'PKG_LIBS = `$(R_HOME)/bin/Rscript -e "Rcpp:::LdFlags()"` $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) $(SHLIB_OPENMP_CXXFLAGS)')
    configure_file("src/Makevars.win.in")
}
