#' Subset simulations with preserved class
#'
#' The normal list subset operation would drop the class from the
#' simulation object (which is fine in theory). With this override,
#' the class is preserved.
#'
#' @param x an object with class "simulation"
#' @param i an index-set
#' @param ... passed on the list-\[ function
#' @export
#' @return subset of experiments
`[.simulation` <- function(x, i, ...) {
  # Use NextMethod to perform the actual subsetting as a list
  out <- NextMethod("[")
  # Re-assign the class back to the result
  class(out) <- class(x)
  return(out)
}

#' simulates an ode model with extra work
#'
#' This function calls a C function which solves an initial value
#' problem, calculates the sensitivity of the solution, log-likelihood
#' value `ll`, gradient of `ll` and Fisher-Information.
#'
#' The model is always simulated using a shared library. The path to
#' the shared library can be passed in three different ways:
#'
#' 1. Character vector: `odeModel <- c("AKAKR4","/tmp/path/AKAR4.so")`
#' 2. A comment: `comment(odeModel) <- "/tmp/path/AKAR4.so"`
#' 3. As part of the ode object:
#'
#' ```
#' odeModel <- as_ode(m)`
#' so_path(odeModel) <- "/tmp/path/AKAR4.so"
#' ```
#'
#' The shared library needs to be created first. Either with `R CMD
#' SHLIB`, [shlib], or manually on the system's command line (bash, zsh, etc.).
#'
#' @param odeModel the name of the ODE model to simulate (a shared
#'     library of the same name will be dynamically loaded and needs
#'     to be created first). Alternatively this can be the ode object
#'     created by [as_ode], with a shared library path attached to it.
#' @param experiments a list of `N` simulation experiments (time,
#'     parameters, initial value, events)
#' @param p a matrix of parameters with M columns
#' @param abs.tol absolute tolerance, real scalar
#' @param rel.tol relative tolerance, real scalar
#' @param initial.step.size initial value for the step size; the step
#'     size will adapt to a value that observes the tolerances, real
#'     scalar
#' @param omit an integer that indicates how many of these to omit in
#'     this order: fisher information, gradient of the log-likelihood,
#'     log-likelihood
#' @param method integration method (see [method] and [name_method]).
#' @param time.out in seconds (early rejection due to long simulation
#'     time). This can trigger at measurement times (outputTime).
#' @param num.steps maximum number of steps the integration method is
#'     permitted to do; early rejection. This condition can trigger at any
#'     point during the integration.
#' @return a list of the solution trajectories `y(t;p)` for all
#'     experiments (named like the experiments), as well as the output
#'     functions
#' @export
#' @keywords ODE
#' @examples
#'   requireNamespace("errors")
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   o <- as_ode(m)
#'   ex <- experiments(m,o)
#'   C <- generate_code(o)
#'   c_path(o) <- write_c_code(C)
#'   so_path(o) <- shlib(o)
#'   print(o)
#'   y <- gsl_odeiv2_fi(o,ex,values(m$Parameter))
#'   if (interactive()){
#'      print(length(y))
#'      print(names(y[[1]]))
#'      oldpar <- par(mfrow=c(length(ex),1))
#'      for (i in seq_along(y)){
#'          plot(
#'              errors::as.errors(ex[[i]]$outputTimes),
#'              ex[[i]]$data,
#'              xlab="time",
#'              ylab=rownames(y[[i]]$data)[1],
#'              main=names(ex)[i],
#'              ylim=c(100,200)
#'          )
#'          lines(ex[[i]]$outputTimes,drop(y[[i]]$func),col='red')
#'      }
#'      par(oldpar)
#'   }
gsl_odeiv2_fi <- function(odeModel,experiments,p,abs.tol=1e-6,rel.tol=1e-5,initial.step.size=1e-3, method=0, omit=0, time.out = 1, num.steps = 0){
	if (is(odeModel,"ode")){
		so <- so_path(odeModel)
		odeModel <- odeModel$name
		comment(odeModel) <- so
	} else if (is.character(odeModel) && length(odeModel)==2){
		l <- endsWith(odeModel,".so")
		so <- odeModel[l]
		name <- odeModel[!l]
		odeModel <- name
		comment(odeModel) <- so
	}
	if (Sys.info()[["sysname"]] == "Windows") {
		# Flip forward slashes back to native Windows backslashes
		comment(odeModel) <- chartr("/", "\\", comment(odeModel))
	}
	if (!is.matrix(p)) p <- as.matrix(p)
	y <- .Call(
		"r_gsl_odeiv2_outer_fi",
		odeModel,
		experiments,
		p,
		abs.tol,
		rel.tol,
		initial.step.size,
		omit,
		method,
		as.double(time.out),
		num.steps
	)
	for (i in seq_along(experiments)){
		if ("initialState" %in% names(experiments[[i]]) && length(experiments[[i]]$initialState)==NROW(y[[i]]$state)){
			dimnames(y[[i]]$state) <- list(names(experiments[[i]]$initialState),NULL,NULL)
		}
		if ("data" %in% names(experiments[[i]]) && NROW(experiments[[i]]$data)==NROW(y[[i]]$func)){
			dimnames(y[[i]]$func) <- c(dimnames(experiments[[i]]$data),NULL)
		}
	}
	return(y)
}

#' simulates a CRNN ode model with extra work
#'
#' This function calls a C function which solves an initial value
#' problem, derived from a CRNN.
#'
#' @param name either the name of a file (shared library file) or the
#'     name of an ODE model to simulate (a shared library of the same
#'     name will be dynamically loaded and needs to be created
#'     first). If the name of the model is given, then the so file
#'     must have the same name in the current directory or a comment
#'     indicates its location.
#' @param experiments a list of `N` simulation experiments (time,
#'     parameters, initial value, events).
#' @param l a matrix of parameters with M columns, in log-space.
#' @param nu a stoichiometry matrix (N × R) where N is the number of
#'     state variables and R the number of reactions, all reactions
#'     are assumed to be reversible.
#' @param m modifiers -- similar to stoichiometry, but indicates
#'     whether the species takes part in the reaction without being
#'     consumed.
#' @param abs.tol absolute tolerance, real scalar.
#' @param rel.tol relative tolerance, real scalar.
#' @param initial.step.size initial value for the step size; the step
#'     size will adapt to a value that observes the tolerances, real
#'     scalar.
#' @param method one of the integration methods bundled with GSL (see
#'     [method] and [name_method]).
#' @param time.out time limit in seconds, checked at every measurement
#'     time-point (in the data).
#' @param nstep maximum number of ODE integrator steps, checked at
#'     every step, defaults to unlimited (0).
#' @return a list of the solution trajectories y(t;p) for all
#'     experiments (named like the experiments), as well as the output
#'     functions.
#' @export
#' @keywords ODE
#' @examples
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   ex <- experiments(m,as_ode(m,cla=FALSE))
#'   nu <- stoichiometric_matrix(m)
#'   l <- matrix(c(log(values(m$Parameter)),0),2,2,dimnames=list(rownames(m$Reaction),c("fwd","bwd")))
#'   C <- CRNN(NCOL(nu),initialValues=values(m$Compound),funcValues=formulae(m$Output))
#'   c.file <- tempfile("AKAR4_",fileext=".c")
#'   cat(C,file=c.file,sep='\n')
#'   so.file <- shlib(c.file)
#'   y <- gsl_odeiv2_CRNN(so.file,ex,l,nu,nu*0)
gsl_odeiv2_CRNN <- function(name,experiments,l,nu,m,abs.tol=1e-6,rel.tol=1e-5,initial.step.size=1e-3,method=0, time.out = 1, nstep=0){
	if (is.character(name) && endsWith(name,.Platform$dynlib.ext)){
		so <- name
		name <- "CRNN"
		comment(name) <- so
	} else if (is.character(comment(name))) {
		so <- comment(name)
	} else {
		so <- paste0("./",name,.Platform$dynlib.ext)
		comment(name)<-so
		message("looking for ", so)
	}
	if (!file.exists(so)){
            warning(sprintf("[gsl_odeiv2_CRNN] for model name \u00ab%s\u00bb, file \u00ab%s\u00bb not found.",name,so))
	}
	if (Sys.info()[["sysname"]] == "Windows") {
		# Flip forward slashes back to native Windows backslashes
		comment(name) <- chartr("/", "\\", comment(name))
	}
	if (!is.matrix(l) && is.numeric(l)) {
		l <- matrix(l,NCOL(nu),2,dimnames=list(colnames(nu),c("fwd","bwd")))
	}
	y <- .Call(
		"r_gsl_odeiv2_outer_CRNN",
		name,       # with comment about shared library
		experiments,
		l,nu,m,     # log-parameters, stoichiometry, and modifiers
		abs.tol,    # absolute tolerance
		rel.tol,    # relative tolerance
		initial.step.size,
		method,     # integrattion method
		as.double(time.out), # in seconds
		nstep
	)
	for (i in seq_along(experiments)){
		if ("initialState" %in% names(experiments[[i]])){
			dimnames(y[[i]]$state) <- list(names(experiments[[i]]$initialState),NULL,NULL)
		}
		if ("data" %in% names(experiments[[i]])){
			dimnames(y[[i]]$func) <- c(dimnames(experiments[[i]]$data),NULL)
		}
	}
	return(y)
}

#' scrnn returns a closure around gsl_odeiv2_CRNN()
#'
#' the returned value is a function of a variable p that encodes the
#' CRNN in some way. Three user supplied functions are used to extract
#' the three components of a CRNN:
#'
#' - kinetic rate coefficients (in log-space): `l <- parMap(p)`
#' - stoichiometric matrix: `nu <- stoichiometry(p)`
#' - modifier matrix: `m <- modifiers(p)`
#'
#' these three components (one numeric vector, and two matrices) are
#' passed to the simulation procedure. The vector l can be a matrix
#' with M columns. In that case, one simulation per column is
#' performed. The stoichiometry and modifiers remain unchanged
#' throughout.
#' @param experiments list of experiments (inputs are ignored).
#' @param modelName scalar string, can indicate a shared library with
#'     an attached comment attribute.
#' @param parMap (function) extracts kinetic rate coefficients from
#'     its argument.
#' @param stoichiometry (function) extracts the stoichiometry matrix
#'     from its argument.
#' @param modifiers (function) extracts the modifier matrix from its
#'     argument.
#' @param method (integer) integration method key (0:10) corresponds to
#'     these GSL methods: msbdf, msadams, bsimp, rk4imp, rk2imp,
#'     rk1imp, rk8pd, rkck, rkf45, rk4, rk2
#' @param time.out time limit for solution in seconds
#' @return closure that maps one argument (p) to simulation results (y).
#' @export
#' @examples
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   ex <- experiments(m)
#'   nu <- stoichiometric_matrix(m)
#'   l <- matrix(
#'     c(log(values(m$Parameter)),-1e6),
#'     2,2,
#'     byrow=TRUE,
#'     dimnames=list(rownames(m$Reaction),c("fwd","bwd"))
#'   )
#'   C <- CRNN(
#'     NCOL(nu),
#'     initialValues=values(m$Compound),
#'     funcValues=formulae(m$Output)
#'   )
#'   c.file <- tempfile("AKAR4",fileext=".c")
#'   cat(C,file=c.file,sep='\n')
#'   modelName <- "CRNN"
#'   comment(modelName) <- shlib(c.file)
#'   s <- scrnn(ex, modelName)
#'   p <- list(l=l,nu=nu,m=nu*0)
#'   y <- s(p)
#'   if (interactive()){
#'      plot(ex,y)
#'   }
scrnn <- function(experiments, modelName, parMap=\(p) p$l, stoichiometry=\(p) p$nu, modifiers=\(p) p$m, method = 0, time.out = 1){
	N <- length(experiments)
	sim <- function(parMCMC){
		nu <- stoichiometry(parMCMC)
		m <- modifiers(parMCMC)
		l <- parMap(parMCMC)
		yf <- gsl_odeiv2_CRNN(modelName,experiments,l,nu,m,method=0, as.double(time.out))
		if (N==length(yf)) {
			names(yf) <- names(experiments)
		} else {
			message(sprintf("experiments(%i) should be the same length as simulations(%i), but isn't.",length(experiments),length(yf)))
		}
		return(yf)
	}
	return(sim)
}

#' This creates a closure that simulates the model, similar to simulator.c
#'
#' This is a shorter alternative to simulator.c (C backend). It also
#' returns the log-likelihood, Fisher Information, and the gradient of
#' the log-likelihood, under the assumption that the measurement error
#' is Gaussian. No attempt is made to parallelize this call, all
#' simulations will be done in sequence.
#'
#' It returns a closure around:
#'     - experiments,
#'     - the model, and
#'     - parameter mapping
#'
#' The returned function depends only on parABC (the sampling
#' parameters).
#'
#' This version of the function does not use the parallel package at
#' all and cannot add noise to the simulations (unlike simulator.c).
#'
#' A hopeless simulation can be stopped early using the settings
#' `num.steps` and `time.out`. The value of `num.steps` applies to
#' every continuous simulation stretch (e.g. between two events), the
#' count of steps is reset whenever an event occurs or one simulation
#' ends (between different parameters and different experiments).
#'
#' The `time.out` is given in seconds and can trigger at measurement
#' time points (when `t_wallclock > time.out`), not between
#' points. How much time has passed is checked when the integrator is
#' stopped to record the state.
#'
#' The limit on the number of steps, on the other hand, is a feature
#' of the GSL ODE solvers and can trigger precisely.
#'
#' @param experiments a list of experiments to simulate: initial
#'     values, inputs, time vectors, initial times
#' @param odeModel Either the ode object created by [as_ode] (with a
#'     shared library field inserted), or a string (with a comment
#'     indicating an .so file) which points out the model to simulate
#' @param parMap the model will be called with parMap(parABC); so any
#'     parameter transformation can happen there.
#' @param method the integration method as an integer (higher numbers
#'     are simpler methods, lower numbers are more advanced methods, 0
#'     maps to 'msbdf')
#' @param omit integer, omit optional return values, in this order:
#'     Fisher Information, gradient of the log-likelihood, the
#'     log-likelihood, output functions. Omission includes all
#'     previous entries. `omit = 1` omits only the Fisher Information,
#'     `omit=3`, omits FI, grad-ll, and log-likelihood calculations.
#' @param time.out (in seconds); simulations are aborted at a time
#'     greater than this.
#' @param num.steps unlimited by default, setting this to a finite
#'     value can help to stop very stiff simulations early.
#' @export
#' @return a closure that returns the model's output for a given
#'     parameter vector, and approximate sensitivity matrices, for
#'     each state variable, function, time-point, and parameter
#'     vector.
#' @examples
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   o <- as_ode(m)
#'   ex <- experiments(m,o)
#'   C <- generate_code(o)
#'   c_path(o) <- write_c_code(C)
#'   so_path(o) <- shlib(o)
#'   s <- simfi(ex,o)
#'   y <- s(values(m$Parameter)) # simulates
#'   print(y)
simfi <- function(experiments, odeModel, parMap=identity, method = 0, omit = 0, time.out = 1, num.steps = 0){
	N <- length(experiments)
	if (is(odeModel,"ode")){
		so <- so_path(odeModel)
		odeModel <- odeModel$name
		comment(odeModel) <- so
	} else if (is.character(odeModel) && length(odeModel)==2){
		l <- endsWith(odeModel,".so")
		so <- odeModel[l]
		name <- odeModel[!l]
		odeModel <- name
		comment(odeModel) <- so
	}
	if (omit<3){ # create data matrices, if they don't exist
		for (E in experiments) stopifnot("data" %in% names(E))
	}
	return(
		function(parABC){
			modelPar <- parMap(parABC)
			m <- NCOL(parABC)
			yf <- gsl_odeiv2_fi(
				odeModel,
				experiments,
				as.matrix(modelPar),
				method=method,
				omit=min(omit,3),
				time.out=time.out,
				num.steps=num.steps
			)
			if (N==length(yf)) {
				names(yf) <- names(experiments)
			} else {
				message(
					sprintf(
						"experiments(%i) should be the same length as simulations(%i), but isn't.",
						length(experiments),
						length(yf)
					)
				)
			}
			class(yf) <- "simulation"
			return(yf)
		}
	)
}

#' This creates a closure that simulates the model
#'
#' This function will use the [parallel::mclapply] to do the
#' simulations simultaneously. Set `options(mc.cores=detectCores())`
#' or a similar sensible value: `options(mc.cores=length(experiments))`
#'
#' It returns a closure around:
#'     - experiments,
#'     - the model, and
#'     - parameter mapping
#'
#' The returned function depends only on the parameter vector (or
#' matrix if more than one simulation per experiment is desired). The
#' parameter vector this simulator accepts is probably derived from
#' the sampling space of a Bayesian method \eqn{\theta}{θ}, so in the list of
#' arguments, it is called `parABC` or (parMCMC would also have been a
#' valid choice). These sampling parameters can be mapped to values
#' the simulator can use via `parMap`. `parModel <- parMap(parABC)`,
#' where the ODE model is expected to work with `parModel`.  The model
#' can be specified by name (with a comment indicating a file
#' location)
#'
#' Some return values are optional and omitting them saves time.
#'
#' @param experiments a list of experiments to simulate: initial
#'     values, inputs, time vectors, initial times
#' @param modelName a string (with optional comment indicating an .so
#'     file) which points out the model to simulate if modelName is a
#'     cme object, the simulation will be done stochastically
#' @param parMap the model will be called with parMap(parABC); so any
#'     parameter transformation can happen there.
#' @param noise boolean variable. If `noise=TRUE`, Gaussian noise is
#'     added to the output of the simulations. The standard deviation
#'     of the Gaussian noise is equal to the measurement error. If
#'     `noise=FALSE` the output is the deterministic solution of the
#'     ODE system. noise and sensitivity calculations are mutually
#'     exclusive.
#' @param omit `omit=0` returns all optional return values form the
#'     simulator, `omit=1` will not calculate the fisher information
#'     (and thus not return it), `omit=2` will omit the gradient of
#'     the log-likelihood, and `omit=3` will omit the likelihood
#'     calculations altogether. Omission is cumulative: `omit=3`
#'     omits all the previously mentioned optional quantities.
#' @param method an integer offset, integration method (for ODE models), see
#'     [method] and [name_method]
#' @param time.out in seconds, for early stops.
#' @param num.steps maximum number of steps taken by the integrator
#'     (in the case of ODEs), or maximum number of total
#'     reaction-steps performed by the Gillespie algorithm (over time)
#'     for stochastic models.
#' @export
#' @return a closure that returns the model's output for a given
#'     parameter vector
#' @examples
#'   requireNamespace("errors")
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   o <- as_ode(m)
#'   ex <- experiments(m,o)
#'   C <- generate_code(o)
#'   c_path(o) <- write_c_code(C)
#'   so_path(o) <- shlib(o)
#'   s <- simulator.c(ex,o)
#'   y <- s(values(m$Parameter))
simulator.c <- function(experiments, modelName, parMap=identity, noise = FALSE, omit=3, method = 0, time.out=1, num.steps=0){
	if (is.na(pmatch("data",names(experiments[[1]]))) && omit < 3){
		warning(
			sprintf(
				paste(
					"log-likelihood calculations were requested with omit=%i,",
					"but no data matrices are present in experiments: ",
					"%s\nsetting omit to 3."
				),
				omit,
				paste(names(experiments[[1]]),collapse=", ")
			)
		)
		omit <- 3 # override
	}
	if (noise){
		sim <- function(parABC){
			modelPar <- parMap(parABC)
			yf <- unlist(
				mclapply(
					experiments,
					function(EX) {
						gsl_odeiv2_fi(
							modelName,
							list(EX),
							as.matrix(modelPar),
							method=method,
							omit=min(omit,3),
							time.out=time.out,
							num.steps=num.steps
						)
					}
				),
				recursive=FALSE
			)
			stopifnot(length(experiments)==length(yf))
			for(i in seq_along(experiments)){
				sd <- standard_error_matrix(experiments[[i]]$data) %otherwise% experiments[[i]]$stdv
				if (is.null(sd)) stop("failed to get standard error from experiments.")
				sd[is.na(sd)] <- 0.0
				yf[[i]]$func <- yf[[i]]$func + array(rnorm(prod(dim(sd)),0,sd),dim=dim(yf[[i]]$func))
			}
			class(yf) <- "simulation"
			return(yf)
		}
	} else if (is(modelName,"cme")) {
		sim <- function(parABC){
			modelPar <- parMap(parABC)
			so <- so_path(modelName)
			if (Sys.info()[["sysname"]] == "Windows") {
				# Flip forward slashes back to native Windows backslashes
				so <- chartr("/", "\\", so)
			}
			yf <- unlist(
				mclapply(
					experiments,
					\(EX) return(
						.Call(
							"gillespie",
							so,
							list(EX),
							as.matrix(modelPar),
							as.double(time.out),
							as.integer(num.steps)
						)
					)
				),
				recursive=FALSE
			)
			names(yf) <- names(experiments)
			for (i in seq_along(yf)){
				rownames(yf[[i]]$state) <- names(experiments[[i]]$initialState)
				rownames(yf[[i]]$func) <- rownames(experiments[[i]]$data)
			}
			stopifnot(length(experiments)==length(yf))
			class(yf) <- "simulation"
			return(yf)
		}
	} else {
		sim <- function(parABC){
			modelPar <- parMap(parABC)
			yf <- unlist(
				mclapply(
					experiments,
					function(EX) {
						tryCatch(
							gsl_odeiv2_fi(
								modelName,
								list(EX),
								as.matrix(modelPar),
								method=method,
								omit=min(omit,3),
								time.out=time.out,
								num.steps=num.steps
							),
							error = function(e) {print(e); return(NA)}
						)
					}
				), recursive=FALSE)
			stopifnot(length(experiments)==length(yf))
			class(yf) <- "simulation"
			return(yf)
		}
	}
	return(sim)
}

#' Establish the Existence of a simulation file for a given model
#'
#' If a shared library doens't exit, this function makes one using
#' only `cc` and `pkg-config` via `system2` calls. This function
#' returns the model name, with some additional comments about the
#' file to use for simulations.
#'
#' As an alternative to this function, it is sufficient to write
#'
#' ```
#' modelName <- "test_ode_model"               # or some other model name
#' comment(modelName) <- "./test_ode_model.so" # must exist
#' ```
#'
#' This function will not attempt to find a model file, other than in
#' the current directory. But, `check_model` will compile a GSL
#' compatible C source file into a shared object if `modelFile` ends
#' with `.c` and stop if that doesn't work. The compiler is called
#' using a system call, which may be incorrect for your system -- if
#' this function fails, you'll have to make the shared library of the
#' model using the correct compiler and options for your system.
#'
#' In contrast to [shlib], this function bypasses `R CMD SHLIB`
#' entierly and makes the shared library using only standard command
#' line tools.
#'
#' In any case, this function stops execution if the model file
#' doesn't exist, because simulations are not possible.
#'
#' @param modelName a string
#' @param modelFile a string, if the model file is different from
#'     "modelName.R". If the file name ends in .c, the c source will be
#'     compiled to a shared library.
#' @return modelName with an additional comment about which file to use for simulations
#' @noRd
#' @examples
#' \dontrun{
#'   modelName <- check_model("AKAR4","./AKAR4_gvf.c") # compiles the model
#'   modelName <- check_model("AKAR4","./AKAR4.so")    # only checks whether ./AKAR4.so exists
#'   comment(modelName)                                # will be "./AKAR4.so" in either case
#' }
check_model <- function(modelName,modelFile=paste0("./",modelName,c('.so','_gvf.c')),OPTS=c("-O2")){
	if (is.null(modelFile)) {
		modelFile <- modelFile[file.exists(modelFile)]
		modelFile <- modelFile[1]
	}
	stopifnot(length(modelFile)==1 && nzchar(modelFile))
	if (grepl('[.]c$',modelFile,useBytes=TRUE)){
		stopifnot(file.exists(modelFile))
		D <- dirname(modelFile)
		message('building a shared library from c source, and using GSL odeiv2 as backend (pkg-config is used here).')
		LIBS <- "`pkg-config --libs gsl`"
		CFLAGS <- "-shared -fPIC `pkg-config --cflags gsl`"
		so <- file.path(D,sprintf("%s.so",modelName))
		command_args <- sprintf("%s %s -o '%s' '%s' %s",CFLAGS,paste(OPTS,collapse=" "),so,modelFile,LIBS)
		message(paste("cc",command_args))
		system2("cc",command_args)
		stopifnot(file.exists(so))
		comment(modelName)<-so
	} else if (grepl('[.]so$',modelFile,useBytes=TRUE)) {
		stopifnot(file.exists(modelFile))
		message(sprintf('Will use pre-existing %s for simulations.',modelFile))
		comment(modelName) <- modelFile
	} else if (grepl('[.]R$',modelFile,useBytes=TRUE)){
		message(sprintf('will use the %s for simulations (deSolve backend)',modelFile))
		comment(modelName) <- modelFile
	}
	return(modelName)
}

#' default distance function for one experiment
#'
#' if each experiment corresponds to one simulation and is fully
#' quantified by itself, then calculating the overall distance
#' between data and experiment can be done one by one. This function
#' describes the default way a simulation is compared to data.
#'
#' If the data is more complex, and two or more simulations are needed
#' to calculate one distance value then the objective-Function needs
#' to be entirely user-supplied. This is the case with experiments
#' that have a "control" -- this is needed when the measurement is in
#' arbitrary units and only makes sense comparatively to a secondary
#' (control) scenario.
#'
#' This function will be used if none is provided by the user.
#'
#' The funcSim values need to be supplied as a matrix of size N×T with
#' N the length of the model's output vectors and T the amount of
#' measurement times (this is how the rgsl package returns the
#' simulation results).
#' @param funcSim a matrix, contains model solution (output values),
#'     columns of output vectors
#' @param dataVAL a matrix of experimental data, shaped like funcSim
#' @param dataERR a matrix of measurement errors, if available,
#'     defaults to the maximum data value.
#' @export
#' @return a numeric scalar, the distance between data `dataVAL` and
#'     simulation `funcSim`.
#' @examples
#' d <- defaultDistance(seq(7),seq(7)+rnorm(7,0,0.1),rep(0.1,7))
defaultDistance <- function(funcSim,dataVAL,dataERR=max(dataVAL)){
	if (all(is.finite(funcSim))){
		distance <- mean(abs(funcSim-dataVAL)/dataERR, na.rm=TRUE)
	} else {
		distance <- Inf
	}
	return(distance)
}

#' creates Objective functions from ingredients
#'
#' the returned objective function has only one argument: the ABC
#' variables that shall be mapped to ODE-model parameters.
#'
#' The user supplied distance function should accept three arguments:
#' distance(SIM, DATA, STDV), all three matrices.  SIM is the model
#' output (simulation), DATA is the measured data, while STDV
#' represents the standard error of that measurement. All three have
#' the same size: N×M, where N is the number of observables (outputs),
#' and M is the number of measurement time-points (length of the
#' time-series).
#'
#' @export
#' @param experiments a list of simulation experiments
#' @param simulate closure that simulates the model
#' @param distance a function that calculates ABC scores (distance between data and simulations)
#' @return an objective function
#' @examples
#'   f <- uqsa_example("AKAR4")
#'   m <- model_from_tsv(f)
#'   o <- as_ode(m)
#'   ex <- experiments(m,o)
#'   C <- generate_code(o)
#'   c_path(o) <- write_c_code(C)
#'   so_path(o) <- shlib(o)
#'   s <- simulator.c(ex,o)
#'   objFunc <- makeObjective(ex,s)
#'   print(objFunc(values(m$Parameter)))
makeObjective <- function(experiments,simulate,distance=defaultDistance){
	Objective <- function(parABC){
		out <- simulate(parABC)
		S <- matrix(Inf,nrow=length(experiments),ncol=NCOL(parABC))
		rownames(S) <- names(experiments)
		if (is.null(out)) return(S)
		stopifnot(length(out) == length(experiments))
		for(i in seq_along(experiments)){
			DATA <- experiments[[i]]$data
			STDV <- standard_error_matrix(DATA) %otherwise% experiments[[i]]$standardError %otherwise% 1.0
			if (inherits(DATA,"errors")) DATA <- errors::drop_errors(DATA) # prevents a big performance issue on windows
			status <- out[[i]]$status
			S[i,] <- apply(
				out[[i]]$func,
				MARGIN=3,
				function(FUNC) {
					distance(FUNC, DATA, STDV)
				}
			)
			S[i,as.logical(status)] <- Inf # any non-zero status
		}
		return(S)
	}
	return(Objective)
}


#' Reverse look-up of method name from key
#'
#' These are the methods in the gsl library (documented in the
#' official documentation), but in reverse order, as they are
#' approximately ordered by complexity, with more complex methods
#' usually being better (but slower).
#'
#' It is therefore a reasonable approach to try methods from the more
#' complex end of the list first and try the next method if the
#' solutions are too slow. But we need to check the accuracy/stability of the
#' result. The mapping between method names and keys:
#'
#' ```
#'      msbdf:	 0
#'    msadams:	 1
#'      bsimp:	 2
#'     rk4imp:	 3
#'     rk2imp:	 4
#'     rk1imp:	 5
#'      rk8pd:	 6
#'       rkck:	 7
#'      rkf45:	 8
#'        rk4:	 9
#'        rk2:	10
#' ```
#'
#' The returned value is an integer index.
#' @export
#' @param key an integer from 0 to 10 (this is used as an offset in c, for 11 items)
#' @return a string representation of the integration method.
#' @examples
#' print(name_method())
name_method <- function(key=seq(0,10)){
	charMethod <- c("msbdf","msadams","bsimp","rk4imp","rk2imp","rk1imp","rk8pd","rkck","rkf45","rk4","rk2")
	k <- round(key+1)
	if (all(0 < k) && all(k <= length(charMethod))){
		return(charMethod[k])
	} else {
		warning(sprintf("invalid key %i",key[k==0 | k>length(charMethod)]))
		return("")
	}
}

#' Find Integer
#'
#' Given a ODE solver name (from the GSL solver module odeiv2), return
#' an integer offset `{0..10}`. This integer can be passed as the
#' "method" argument for all ODE simulator functions ([simulator.c], [simfi])
#'
#' @param name character scalar, name of the method
#' @return an integer that is acceptable to [simfi] and [simulator.c]
#' @export
method <- function(name){
	m <- seq_along(c("msbdf","msadams","bsimp","rk4imp","rk2imp","rk1imp","rk8pd","rkck","rkf45","rk4","rk2"))-1
	names(m) <- c("msbdf","msadams","bsimp","rk4imp","rk2imp","rk1imp","rk8pd","rkck","rkf45","rk4","rk2")
	return(m[name])
}
