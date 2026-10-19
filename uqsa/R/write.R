#' Construct Code
#'
#' Interpret the first argument and generate code in the specified
#' language for the model type.
#'
#' Whenever the model `Model` is of type `"cme"`, the LV parameter is
#' used to determine the actual number of molecules in the
#' system. Otherwise it is ignored.
#'
#' @param Model either CME or ODE model
#' @param language either C or R
#' @param LV Avogadro's constant multiplied by the system's volume in
#'     litres, only used for CME models
#' @export
#' @return a character vector with the code
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' C <- generate_code(o)
#' cat(head(C),sep="\n")
generate_code <- function(Model,language="C", LV=6.02214076e+8){
	if (is(Model,"ode") && tolower(language)=="c"){
		C <- generateCode(Model)
	} else if (is(Model,"ode") && tolower(language)=="r"){
		C <- generateRCode(Model)
	} else if(is(Model,"cme") && tolower(language)=="c"){
		C <- generateGillespieCode(Model,LV)
	} else {
		stop(sprintf("Requested source-code type not implemented (yet): %s in %s",class(Model),as.character(language)))
	}
	comment(C) <- Model$name
	return(C)
}

#' Compile C code to shared library
#'
#' Calls `R CMD SHLIB` to create the model's shared library.
#'
#' The first argument can be a raw character scalar with just the path
#' of the c code to be compiled, or alternatively an object that has
#' this information stored within it. The models returned by [as_cme]
#' and [as_ode] can both carry this information, attach it via
#' [c_path<-].
#'
#' @param file the c file that is to be compiled, OR an ODE/CME object
#'     with a c.file defined and recorded in it.
#' @param verbose print decition outcomes about the compiler options
#' @export
#' @return the path of the created shared library
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' C <- generate_code(o)
#' c_path(o) <- write_c_code(C)
#' so_path(o) <- shlib(o)
#' print(o)
#' if (file.exists(so_path(o))) cat("shared library exists.\n")
shlib <- function(file,verbose=getOption("uqsa.verbose",default=interactive())){
	if (is(file,"ode")) {
		so_name <- file$name
		file <- c_path(file)
	} else if (is(file,"cme")) {
		so_name <- file$name
		file <- c_path(file)
	} else {
		so_name <- sub("[.]c$","",basename(file))
	}
	if (is.null(file)) stop("No C file specified")
	file <- normalizePath(file,winslash='/',mustWork=FALSE)
	if (!file.exists(file)) stop(sprintf("%s does not exist.",file))

	so <- normalizePath(
		file.path(dirname(file), paste0(so_name, .Platform$dynlib.ext)),
		winslash='/',
		mustWork=FALSE
	)
	platform <- Sys.info()[["sysname"]]
	if (verbose) cli::cli_alert_info("We are on {platform}")
	if (nzchar(Sys.which("pkg-config")) && system2("pkg-config", c("--exists", "gsl"), stdout = FALSE, stderr = FALSE)==0) {
		cflags <- tryCatch(system2("pkg-config", c("--cflags", "gsl"), stdout = TRUE), error = function(e) sprintf("failure in 'pkg-config --cflags': %s",e))
		libs <- tryCatch(system2("pkg-config", c("--libs", "gsl"), stdout = TRUE), error = function(e) sprintf("failure in 'pkg-config --libs': %s",e))
		if (verbose) cli::cli_alert_info("pkg-config is installed and knows about GSL: {cflags}, {libs},")
	} else if (nzchar(Sys.which("gsl-config"))){
		cflags <- tryCatch(system2("gsl-config", "--cflags", stdout = TRUE), error = function(e) sprintf("failure in 'gsl-config --cflags': %s",e))
		libs   <- tryCatch(system2("gsl-config", "--libs", stdout = TRUE), error = function(e) sprintf("failure in 'gsl-config --cflags': %s",e))
		if (verbose) cli::cli_alert_info("gsl-config is installed: {cflags}, {libs},")
	} else if (platform == "Windows"){
		## perhaps pkg-config is available internally to R CMD SHLIB
		if (verbose) cli::cli_alert_info("neither pkg-config, nor gsl-config is available, trying pkg-config anyway and writing results to a temporary Makevars.win file.")
		cat( # writes to a file
			c(
				"PKG_CPPFLAGS = $(shell pkg-config --cflags gsl) -O3",
				"PKG_LIBS = $(shell pkg-config --libs gsl)"
			),
			sep="\n",
			file=file.path(dirname(file),"Makevars.win")
		)
		cflags = ""
		libs = ""
	} else {
		warning(
			"Neither pkg-config (with gsl.pc), nor gsl-config exist on this system (",
			platform,
			"), perhaps the GNU Scientific Library isn't installed?",
			"Trying hard-coded values for GSL location."
		)
		cflags <- Sys.getenv("GSL_CFLAGS", unset = "")
		libs <- Sys.getenv("GSL_LIBS", unset = "-lgsl -lgslcblas -lm")
		if (verbose) cli::cli_alert_info("cflags: {cflags}; libs: {libs}") # additional warning information
	}
	## Environment variables
	### 1. get current values to restor later
	old_cppflags <- Sys.getenv("PKG_CPPFLAGS", unset = NA)
	old_libs <- Sys.getenv("PKG_LIBS", unset = NA)
	on.exit(
		{
			if (is.na(old_cppflags)) Sys.unsetenv("PKG_CPPFLAGS") else Sys.setenv(PKG_CPPFLAGS = old_cppflags)
			if (is.na(old_libs)) Sys.unsetenv("PKG_LIBS") else Sys.setenv(PKG_LIBS = old_libs)
		},
		add = TRUE
	)
	### 2. set new values, determined by pkg-config
	cflags <- paste(cflags,"-march=native -O3")
	if (verbose) cli::cli_alert_info("Determined cc template: R CMD SHLIB {cflags} {basename(file)} {libs}.")
	if (nzchar(cflags)) Sys.setenv(PKG_CPPFLAGS = cflags)
	if (nzchar(libs)) Sys.setenv(PKG_LIBS = libs)
	### 3. R CMD SHLIB writes a file called symbols.rds
	###    we need to make sure that this file lands in `tempdir()`
	wd <- getwd()
	on.exit(setwd(wd))
	setwd(dirname(file))
	status <- system2(
		command = file.path(R.home("bin"), "R"),
		args = c("CMD", "SHLIB",basename(file)),
		stdout = TRUE,
		stderr = TRUE,
		wait = TRUE
	)
	if (!file.exists(so)) {
		warning(sprintf("Building %s failed.",so),status)
	}
	return(so)
}


#' Write the C code to a file
#'
#' This function does not compile the code, it only writes it to a
#' file in a temporary location (tempdir). By default, the name of the
#' file will contain the hash of the entire code.
#'
#' If instead of a character vector, an ode or cme object is passed,
#' this function will generate code from it with default options.
#'
#' @param C the code to write, as a character array.
#' @param model.name a string with no special characters, will be used in the file name
#' @param file override the default file name (based on hashing)
#' @export
#' @return the path of the written file
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' C <- generate_code(o)
#' c_path(o) <- write_c_code(C)
#' print(o)
#' if (file.exists(c_path(o))) cat("c file exists.\n")
write_c_code <- function(C, model.name=comment(C), file=NULL){
	if (missing(file)){
		file  <- normalizePath(
			file.path(
				normalizePath(tempdir(),winslash='/',mustWork=FALSE),
				digest::digest(C,"xxhash64"),
				paste0(model.name,".c")
			),
			winslash='/',
			mustWork=FALSE
		)
	}
	if (is(C,"ode")) { # an ode model was passed instead of code
		ode <- C
		model.name <- ode$name
		C <- generate_code(ode)
	} else if (is(C,"cme")){
		cme <- C
		model.name <- cme$name
		C <- generate_code(cme)
	}
	if (!dir.exists(dirname(file))){
		dir.create(dirname(file),recursive=TRUE)
	}
	cat(C,sep="\n",file=file) # writes to file
	return(file)
}

#' Writes code to file and compiles
#'
#' This function accepts an ode model, or cme model, generates code,
#' compiles it to a shared library, and returns a changed object.
#' possibly changed by the user. It writes the contents to a c file
#' named 'modelName_gvf.c'. This file is compiled to './modelName.so'
#' using normal command line tools, not `R CMD SHLIB`
#'
#' This entire function can be replaced with a call to `cat()` and
#' then compiling the written file in the system's shell.
#'
#' @export
#' @param M ode or cme Model for which code is generated and written to a file
#' @return a copy of `o` with file paths added to it
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- write_and_compile(as_ode(m))
#' print(o)
write_and_compile <- function(M){
	C <- generate_code(M)
	f <- tempfile(pattern=sprintf("%s_",M$name), fileext=".c")
	cat(C,sep="\n",file=f) # writes to file
	c_path(M) <- f
	so_path(M) <- shlib(f)
	return(M)
}
