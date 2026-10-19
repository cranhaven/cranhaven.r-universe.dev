#' Prints an interpretation string of a unit
#'
#' The unit object is a tagged data frame, with these columns:
#' - multiplier
#' - kind
#' - scale
#' - exponent
#'
#' The interpretation is the same as in SBML units. This function also
#' prints an inferred unit id: a string that has no special characters
#' in it and can be used in places where such characters are not
#' allowed (e.g. SBML unit `id` attribute).
#'
#' The original string that a unit was derived from is attached to the
#' unit object as a [comment].
#'
#' Units are produced by the function [unit.from.string].
#'
#' @param x an object of type 'unit_of_measurement'
#' @param ... required by the generic print function.
#' @export
#' @return called for the side-effect; no value.
#' @examples
#' lapply(lapply(c("km/h","s^-2","1/s"),unit.from.string),print)
print.unit_of_measurement <- function(x,...){
	unit <- x
	cat(
		sprintf("\u00ab%s\u00bb has been interpreted as",comment(unit)),
		"the product of: ",
		sprintf("%30s",attr(unit,"id")),
		"==============================",
		sprintf(
			"(%g \u00D7 %s \u00D7 10^(%i))^(%i)",
			unit$multiplier,
			unit$kind,
			unit$scale,
			unit$exponent
		),
		sep='\n'
	)
	invisible(x)
}

#' prints the simulation experiments
#'
#' The experiments, if accidentally printed, are difficult to read.
#' This function prevents these accidental prints. It summarizes the
#' data and simulation experiments instead.
#' @param x simulation experiments with data
#' @param ... ignored.
#' @export
#' @return called for side-effect (printout); no value.
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' ex <- experiments(m,o)
#' print(ex)
print.experiments <- function(x,...){
	ex <- x
	cat(sprintf("number of simulation experiments: %i\n",length(ex)))
	for (i in seq_along(ex)){
		cat(sprintf("%42s",names(ex)[i]),"\n")
		cat(paste0(rep("-",42),collapse=""),"\n")
		for (j in seq_along(ex[[i]])){
			x <- ex[[i]][[j]]
			if (is.array(x)){
				cat(sprintf("%24s: %s (dim)\n",names(ex[[i]])[j],paste(dim(x),collapse=", ")))
			} else if (is.data.frame(x)){
				cat(
					sprintf(
						"%24s: %i columns (%s)\n",
						names(ex[[i]])[j],
						NCOL(x),
						paste(class(x),collapse=", ")
					)
				)
			} else if (is.matrix(x)){
				cat(sprintf("%24s: %i\u00D7%i (dim)\n",names(ex[[i]])[j],NROW(x),NCOL(x)))
			} else if (is.numeric(x) && length(x)==1) {
				cat(sprintf("%24s: %g\n",names(ex[[i]])[j],x))
			} else if (is.numeric(x)) {
				cat(sprintf("%24s: %i (length)\n",names(ex[[i]])[j],length(x)))
			} else {
				cat(
					sprintf(
						"%24s: %s (class), %s (type)\n",
						names(ex[[i]])[j],
						paste(class(x),collapse=", "),
						typeof(x)
					)
				)
			}
		}
		cat("\n")
	}
	cat("experiments: ",paste(names(ex),collapse=", "),"\n")
}

#' prints the simulation results
#'
#' The results, if accidentally printed, are difficult to read.  This
#' function prevents these accidental prints. It summarizes the
#' results instead.
#' @param x simulation results
#' @param ... requirement of print generic, not used.
#' @export
#' @return called for side-effect (printout); no value.
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' ex <- experiments(m,o)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' s <- simfi(ex,o)
#' y <- s(values(m$Parameter))
#' print(y)
print.simulation <- function(x,...){
	y <- x
	cat(sprintf("number of simulation experiments: %i\n",length(y)))
	for (i in seq_along(y)){
		cat(sprintf("%42s",names(y)[i]),"\n")
		cat(paste0(rep("-",42),collapse=""),"\n")
		for (j in seq_along(y[[i]])){
			x <- y[[i]][[j]]
			if (is.array(x)){
				cat(sprintf("%24s: %s (dim)\n",names(y[[i]])[j],paste(dim(x),collapse=", ")))
			} else if (is.numeric(x) && length(x)==1) {
				cat(sprintf("%24s: %g\n",names(y[[i]])[j],x))
			} else {
				cat(
					sprintf(
						"%24s: %s (class), %s (type)\n",
						names(y[[i]])[j],
						paste(class(x),collapse=", "),
						typeof(x)
					)
				)
			}
		}
		cat("\n")
	}
	cat("experiments: ",paste(names(y),collapse=", "),"\n")
}


#' Print a summary about the ode
#'
#' An ODE model was created by `as_ode` can be summarized here,
#' including information about the compiled version of the model.
#'
#' The ode model is for the most part a list of named vectors and
#' matrices which together encode the mathematical structure of the
#' ode.
#'
#' @param x the ode
#' @param ... requirement of print generic, not used.
#' @return NULL
#' @export
#' @return called for side-effect (printout); no value.
#' @examples
#' f <- uqsa_example("AKAR4")
#' m <- model_from_tsv(f)
#' o <- as_ode(m)
#' print(o)
print.ode <- function(x,...){
	o <- x
	cat(
		sprintf("%26s : %s","Model name",o$name),
		sprintf("%26s : %s [%s]","C file",o$c_path,o$c.date),
		sprintf("%26s : %s [%s]","shared library",o$so_path,o$c.date),
		sprintf("%26s : %i","Number of state variables",length(o$var)),
		sprintf("%26s : %i","Number of parameters",length(o$par)),
		sprintf("%26s : %i","Number of outputs",length(o$func)),
		sprintf("%26s : %i","Conservation laws",NROW(o$conservationLaws)),
		sprintf("%26s : %s","Transformations",ifelse(is.null(o$tf),"no","yes")),
		sep="\n"
	)
}

#' print information about the mcmc variable
#'
#' Some mcmc variables have many attributes, which clutter the screen
#' when accidentally printed. This function prevents these long
#' printouts.
#'
#' @param x the variable
#' @param ... requirement of print generic, not used.
#' @export
#' @return called for side-effect (printout); no value.
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' o <- as_ode(m)
#' c_path(o) <- write_c_code(generate_code(o))
#' so_path(o) <- shlib(o)
#' ex <- experiments(m,o)
#' dprior <- dNormalPrior(values(m$Parameter),m$Parameter$stdv)
#' s <- simfi(ex,o)
#' p <- mcmc_init(1.0,values(m$Parameter),s,dprior=dprior)
#' print(p)
print.mcmcVariable <- function(x,...){
	v <- x
	print(v[seq_along(v)])
	A <- c("simulations", "logLikelihood", "prior", "gradLogLikelihood","gradLogPrior","fisherInformation")
	for (a in A){
		if (v %has% a){
			if (is.list(attr(v,a))){
				cat(sprintf("%24s: %i (length)\n",a,length(attr(v,a))))
			} else if (is.numeric(attr(v,a)) && length(attr(v,a))==1){
				cat(sprintf("%24s: %g\n",a,attr(v,a)))
			} else if (is.numeric(attr(v,a)) && length(attr(v,a))>1){
				cat(sprintf("%24s: %i (length)\n",a,length(attr(v,a))))
			} else if (is.matrix(attr(v,a))) {
				cat(sprintf("%24s: %s (dim)\n",a,paste(dim(attr(v,a)),collapse="x")))
			} else {
				cat(
					sprintf(
						"%24s: %s (class) %s (type)\n",
						a,
							paste(class(attr(v,a)),collapse=", "),
							typeof(attr(v,a))
					)
				)
			}
		}
	}
}

#' Print a Summary about the CME model
#'
#' This information printed on screen omits the details about the
#' interactions, only the lengths of the vectors included in the data
#' structure `CME`.
#'
#' @param x a model created by [as_cme]
#' @param ... requirement of print generic, not used.
#' @return Nil
#' @export
#' @examples
#' f <- uqsa_example("AKAR4")
#' m <- model_from_tsv(f)
#' cmeModel <- as_cme(m)
#' print(cmeModel)
print.cme <- function(x,...){
	cmeModel <- x
	cat(
		sprintf("%26s : %s","Name",cmeModel$name),
		sprintf("%26s : %s [%s]","C file",cmeModel$c_path,cmeModel$c.date),
		sprintf("%26s : %s [%s]","shared library",cmeModel$so_path,cmeModel$so.date),
		sprintf("%26s : %i","Number of state variables",length(cmeModel$initialCount)),
		sprintf("%26s : %i","Number of parameters",length(cmeModel$par)),
		sprintf("%26s : %i","Number of outputs",length(cmeModel$output)),
		sprintf("%26s : %i","Number of constants",length(cmeModel$const)),
		sep="\n"
	)
}
