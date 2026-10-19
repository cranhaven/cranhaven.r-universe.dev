## Here we define functions that are meant to replace SBtab, by making
## it as simple as possible, with no need to remember anything.

#' Standard Error Matrix from an errors object
#'
#' If a matrix has an `errors` attribute, it is usually a vector.
#' This function returns the values of this attribute as a matrix (it
#' preserves the dimensions of the host matrix).
#'
#' @param M a matrix with errors (uncertainties)
#' @export
#' @return A matrix similar to E, with standard error values
#' @examples
#' M <- matrix(seq(12),3,4,dimnames=list(letters[seq(3)],LETTERS[seq(4)]))
#' errors::errors(M) <- abs(M*0.1 + 0.1)
#' E <- standard_error_matrix(M)
#' print(E)
standard_error_matrix <- function(M){
	if (is.matrix(M) && is(M,"errors")){
		d <- dim(M)
		E <- errors::errors(M)
		dim(E) <- d
		dimnames(E) <- dimnames(M)
	} else if (is.matrix(M) && M %has% "errors"){
		d <- dim(M)
		E <- attr(M,"errors")
		dim(E) <- d
		dimnames(E) <- dimnames(M)
	} else {
		warning("Argument is not a matrix and thus has no standard error matrix")
		E <- NULL
	}
	return(E)
}

#' model_from_tsv loads the content from a series of tsv files
#'
#' The argument can be either a series of tsv file-names, or a
#' directory with tsv files. If it is a directory, all tsv files
#' therein will be used.
#'
#' @export
#' @return a list of data.frames, one per file, named like the files.
#' @param src either a vector of files, or a directory with tsv files
model_from_tsv <- function(src="."){
	stopifnot(is.character(src))
	if (length(src)==1 && dir.exists(src)){
		modelName <- basename(normalizePath(src))
		src <- dir(src,pattern="tsv$")
	} else {
		modelName <- basename(dirname(normalizePath(src[1])))
	}
	stopifnot(all(file.exists(src)))
	if (is.null(names(src))){
		names(src) <- sub("[.]tsv$","",basename(src))
	}
	m <- lapply(src,read.delim,row.names=1)
	comment(m) <- modelName
	names(m) <- names(src)
	return(m)
}

#' Get j-th column with names
#'
#' When indexing a matrix or data.frame, rownames are lost. This function will
#' return a column of a matrix, as a vector (dropping rank so to
#' speak), but the vector will retain the rownames of the matrix
#'
#' @export
#' @param m a matrix
#' @param j a column index
#' @return a named vector
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAP79"))
#' u <- column(m$Parameter,"unit")
column <- function(m,j=1){
	v <- m[,j]
	names(v) <- rownames(m)
	return(v)
}

#' Find values in a data.frame that is derived from a tsv file or similar
#'
#' given a data.frame, this function will look for a column that
#' contains some kind of value and retrieve it. The returned numeric
#' vector will be named.
#'
#' If the values contain a standard error, the returned value is of
#' class _errors_.
#'
#' @param df a data frame with a "value" column
#' @return a named numeric vector
#' @export
values <- function(df){
	if (is.null(df)) return(NULL)
	else stopifnot(is.data.frame(df))
	##
	if ("value" %in% colnames(df)){
		v <- as.double(df$value)
	} else {
		v <- as.double(df[[grep("[Vv]alue",colnames(df))[1]]])
	}
	if (is.null(v)) stop("data.frame contains no values column.")
	if (is.character(v)){
		v <- parse_concise(v)
	}
	names(v) <- rownames(df)
	attr(v,"unit") <- units_from_table(df)
	return(v)
}

#' Find the uncertainty of values in a data.frame that is derived from a tsv file or similar
#'
#' given a data.frame, this function will look for a column that
#' contains some kind of standard error and retrieve it. The returned
#' numeric vector will be named. This function is not intended for
#' data, for data, the [values] function will retrieve both the value
#' and the standard error if it was specified.
#'
#' This function is for the case that the table specifies a
#' distribution with a mean and an range (of some sort). The type of
#' uncertainty found will be attached as a comment to the returned
#' value: "sd" standard deviation for normal distribution, "se"
#' standard error (for a normal prior), and "range" for a uniform
#' prior. Other priors are not recognized yet.
#'
#' The distinction between standard-error and standard-deviation
#' doesn't matter much here: either the value is some kind of mean and
#' the _uncertainty_ is the standard-error or standard-deviation of
#' the mean, or it is a raw data-point (not averaged) and we know the
#' standard deviation (noise) of the device that measured it, then
#' _uncertainty_ is the standard deviation of the noise
#' distribution. In either case, the value will be taken at face value
#' and the uncertainty is used as sigma in the default log-likelihood
#' function.
#'
#' Any entry of prior.distribution other than "uniform", will start a
#' search for some kind of standard deviation or standard error (or
#' sigma). As more priors are added, this function will look for the
#' parameters of those distributions.
#'
#' This function makes many assumptions specifically that all
#' variables in the table have the same type of prior distribution
#' (but not identically distributed).
#'
#' @param df a data frame with a "value" column
#' @return a named numeric vector
#' @export
uncertainty <- function(df){
	type <- list(
		sd=c("stdv","sd","st.dv","standard.deviation","sigma"),
		se=c("se","st.err","standard.error","uncertainty"),
		bounds=c("min","lb","lower.bound","max","ub","upper.bound")
	)
	dist <- df[[grep("distribution",tolower(colnames(df)),useBytes=TRUE)]] # distribution column
	if (all(grepl("uniform",dist,ignore.case=TRUE,useBytes=TRUE))){
		m <- na.omit(pmatch(type[["bounds"]],tolower(colnames(df))))
		if (length(m)==2){
			u <- abs(as.numeric(diff(t(df[,m]))))
			names(u) <- rownames(df)
			comment(u) <- "range"
			return(u)
		}
	} else { # if (all(grepl("normal",dist,ignore.case=TRUE,useBytes=TRUE))){
		for (j in seq(2)){
			m <- na.omit(pmatch(type[[j]],tolower(colnames(df))))
			if (length(m) > 0){
				i <- m[1]
				u <- df[[i]]
				names(u) <- rownames(df)
				comment(u) <- names(type)[j]
				return(u)
			}
		}
	}
	return(NULL)
}

#' Find a column that contains some kind of mathematic expression in a data.frame
#'
#' Given a data.frame that should contain a column that assigns a math
#' expression to a name (in row names), this function returns a named
#' character vector with the expressions. The formula column should be
#' named "formula" (if it exists, only this column will be used). But
#' some other spellings will also work as fallback. As a fallback
#' "value" is acceptable as well, because it makes sense to say
#' "the value of x is 'y/2+1'", even though it is not an atomic value
#' (but an expression).
#'
#' @param df a data.frame with a "formula" column
#' @return character vector with names taken from the row names of df
#' @export
#' @examples
#' df <- data.frame(formula=c("exp(x)","10^x","2*x + 3"),row.names=c("f1","f2",'f3'))
#' formulae(df)
#' df <- data.frame(value=c("exp(x)","10^x","2*x + 3"),row.names=c("f1","f2",'f3'))
#' formulae(df)
formulae <- function(df){
	if (is.null(df)) return(NULL)
	if ("formula" %in% colnames(df)){
		f <- df$formula
	} else {
		f <- df[[grep("[Ff]ormula|[Vv]alue|[Kk]inetic[.][Ll]aw",colnames(df))[1]]]
	}
	if (is.null(f)) stop("data.frame contains no formula column")
	names(f) <- rownames(df)
	attr(f,"unit") <- units_from_table(df)
	return(f)
}

#' Modifies a value
#'
#' This function does the same as `x <- x + sign*value`, but without
#' repeating `x`. The expression `modify(x) <- rnorm(length(x),0,1)`
#' will add Gaussian noise to it. This is meant as a replacement for
#' the `x += 1` syntax of C, it exists only for aesthetic reasons.
#'
#' Specifically, this function should work for matrices, and it is
#' possible to supply row and column index vectors: `x[i,j]` will be
#' modified.
#'
#' This function is quite useful if `x` has a very long name,
#' e.g. `experiments[[1]]$func`.
#'
#' @param x a numeric value to be modified
#' @param i row-indices of `x` to be modified
#' @param j column-indices of `x` to be modified
#' @param sgn modification
#' @param value a numeric value of appropriate size, depending on `i` and `j`
#' @return The value of x is modified in place: `x <- x + sgn*value` (value is additive)
#' @export
#' @examples
#'  x <- matrix(seq(12),3,4)
#'  modify(x,seq(2),seq(2)) <- 10
#'  print(x)
`modify<-` <- function(x,i=seq(NROW(x)),j=seq(NCOL(x)),sgn=+1,value){
	if (is.matrix(x)){
		x[i,j] <- x[i,j] + sgn*value
	} else {
		x[i] <- x[i] + sgn*value[i]
	}
	return(x)
}

#' Convert to linear space
#'
#' A number given in some logarithmic space can be transformed back to linear space
#' A call like `base(x) <- 10` means that x was provided in base-10 logarithm form.
#' This will adjust `x` so that it is now in linear space.
#'
#' If `x` was provided in logarithmic space, then it is an exponent to
#' the given base (value).
#'
#' @param x a numeric vector
#' @param i a subset of values in x, defaults to all values of x
#' @param value the base of the logarithm x was provided in
#' @return x will be changed to be in linear space
#' @export
#' @examples
#'  x <- 2
#'  base(x) <- 10
#'  print(x)
`base<-` <- function(x,i=seq_along(x),value){
	x[i] <- exp(log(value)*x[i])
	return(x)
}

`%without%` <- function(x,unwanted.names){
	l <- names(x) %in% unwanted.names
	return(x[!l])
}

`%with%` <- function(x,wanted.names){
	l <- names(x) %in% wanted.names
	return(x[l])
}


#' The stoichiometric matrix of a reaction network
#'
#' Given a model, described in tabular form (`m` is a list of
#' data-frames). The stoichiometric matrix is the linear map between
#' the model's flux vector and the ODE's right-hand-side vector field.
#' If the flux vector is `rr <- flux(t,x,p)`, which maps the state
#' variables `x` and parameters `p` to the reaction rate `rr` of each
#' reaction. The stoichiometric matrix `nu` (\eqn{\nu}{ν}), will map the reaction
#' rates to the rate of change of the state variables: dx/dt := nu %*%
#' flux(t,x,p).
#'
#' The matrix is usually sparse, but not extremely big. This function
#' attaches a sparse version of the same information as attributes to
#' the return-value, as two lists, for convenience.
#'
#' @param m list of data frames with at least the 'Reaction' table,
#'     and the 'Compound' table
#' @param compound.names all names of the reacting compounds
#' @export
#' @return the stoichiometric matrix, with some additional attributes.
#' @examples
#' the_reaction <- "A + B <=> C"
#' m <- list(
#'     Reaction=data.frame(reactants=c("A+B"),products=c("C"))
#' )
#' nu <- stoichiometric_matrix(m,c("A","B","C"))
stoichiometric_matrix <- function(m,compound.names=rownames(m$Compound)) {
	nu <- matrix(0,length(compound.names),NROW(m$Reaction),dimnames=list(compound.names,rownames(m$Reaction)))
	reactants <- stoichiometry(lapply(strsplit(m$Reaction$reactants,"+",fixed=TRUE),trimws))
	products <- stoichiometry(lapply(strsplit(m$Reaction$products,"+",fixed=TRUE),trimws))
	for (j in seq_along(reactants)){
		r <- reactants[[j]] %with% compound.names
		p <- products[[j]] %with% compound.names
		if (!is.null(r)) modify(nu,names(r),j) <- -r
		if (!is.null(p)) modify(nu,names(p),j) <- p
	}
	attr(nu,"reactants") <- reactants
	attr(nu,"products") <- products
	return(nu)
}

#' Fetch an Attribute
#'
#' This function differs from `rlang::%@%` in that it stops if the
#' attribute doesn't exist.
#'
#' This function tries to find a similarly named attribute
#' disregarding capitalization and using partial matching.
#'
#' The only way from this function to return NULL is when `x` is null
#' (the object that supposedly has the attribute). For the purposes of
#' this function , NULL objects are treated as optional things, and
#' thus their attributes do not matter. Non-NULL objects that should
#' have an attribute, but don't are considered erroneous.
#'
#' @param x an R object (variable with attributes)
#' @param a the name of an attribute
#' @return the value of the attribute: `attr(x,a)`
#' @name grapes-at-grapes
#' @rdname grapes-at-grapes
#' @export
#' @examples
#' x <- 1
#' attr(x,"unit") <- "m"
#' print(x %@% "unit")
`%@%` <- function(x,a){
	if (is.null(x)) return(NULL)
	stopifnot(is.character(a))
	if (x %has% a){
		return(attr(x,a))
	} else if (any(is.finite(pmatch(tolower(a),tolower(names(attributes(x))))))){
		return(attributes(x)[[pmatch(tolower(a),tolower(names(attributes(x))))]])
	} else {
		stop(sprintf("Attribute \u00ab%s\u00bb not found in the list of objects's attributes: %s.\n",a,paste0(names(attributes(x)),collapse=", ")))
	}
}

#' Interprets a character vector as names of logarithms
#'
#' The values in `x` are possibly given in a logarithmic space. The
#' parameter `str_scale` gives this logarithmic scale (provided in a
#' language agnostic form), by a human. An empty string causes no
#' transformations. Similarly, providing no scale at all causes no
#' transformations.
#'
#' The words in `str_scale` name a logarithm, e.g. "log10". Currently understood scales:
#' - log10
#' - log2, ld
#' - ln, log
#'
#' @param x values
#' @param str_scale character vector
#' @return a copy of `x`,  transformed into linear space
#' @export
#' @examples
#' x <- c(1,2,3,1,1,1)
#' attr(x,"scale") <- c("log10","log2","log","ln","ld","log5")
#' print(linear_scale(x))
linear_scale <- function(x,str_scale=attr(x,"scale")){
	if (is.character(str_scale)){
		# general case: logXX
		l <- grepl("^log[0-9]+$",str_scale)
		base(x,l) <- as.numeric(sub("^log([0-9]+)$","\\1",str_scale[l]))
		# some special cases:
		base(x,grepl("^log$|^ln$",str_scale)) <- exp(1)
		base(x,grepl("^ld$",str_scale)) <- 2
	}
	return(x)
}

`%&%` <- function(a,b) {
	return(paste0(a,b))
}

#' Add a Reaction to an ODE
#'
#' This function operates on an ODE vector field. It makes it possible
#' to construct the right hand side one reaction at a time:
#' `reaction(vf,r,p) <- flux` will add a flux term to the ODE's right
#' hand side (vector field), adding the given `flux` (reaction
#' kinetic) in the right places, informed by the reactants and
#' products.
#'
#' The reactants, e.g.: `c(A=2,B=1)` name which state variables are affected by the flux (negatively, for reactants)
#' The products, e.g.: `c(C=1)` name which state variables are affected positively by the flux.
#' @param vf a named character vector of the right length (number of state variables)
#' @param r a named vector of stoichiometric coefficients for the reactants
#' @param p a named vector of stoichiometric coefficients for the products
#' @param value a reaction rate (string)
#' @return updated vf
#' @noRd
#' @examples
#' Reaction <- "A + B <=> C"
#' r <- c(A=1,B=1)
#' p <- c(C=1)
#' vf <- c(A="",B="",C="") # empty
#' reaction(vf,r,p) <- "A*B-C"
#' print(vf)
`reaction<-` <- function(vf,r,p,value){
	stopifnot(vf %has% "names")
	if (value %has% "names") {
		f <- names(value)
	} else {
		f <- value
	}
	if (!is.null(r) && r %has% "names"){
		l <- names(vf) %in% names(r)
		k <- names(r) %in% names(vf)
		if (all(r==1)) F <- f
		if (all(r==1)){
			vf[l] <- sprintf("%s-%s",vf[l],f)
		} else {
			vf[l] <- sprintf("%s-%i*%s",vf[l],r[k],f)
		}
	}
	if (!is.null(p) && p %has% "names"){
		l <- names(vf) %in% names(p)
		k <- names(p) %in% names(vf)
		if (all(p==1)){
			vf[l] <- sprintf("%s+%s",vf[l],f)
		} else {
			vf[l] <- sprintf("%s+%i*%s",vf[l],p[k],f)
		}
	}
	return(vf)
}

#' Reduce the size of the system
#'
#' Given a stoichiometric matrix, this function performs model
#' reduction via linear algebra operations, with [pracma::null].
#' @param nu stoichiometric matrix
#' @param iv initial values
#' @param verbose if `TRUE`, this function will print the conservation laws on screen
#' @return a list of conservation laws
#' @export
#' @examples
#' f <- uqsa_example("AKAR4")
#' m <- model_from_tsv(f)
#' nu <- stoichiometric_matrix(m)
#' CL <- conservation_law_analysis(nu,values(m$Compound))
#' print(names(CL))
#' print(CL[,c('value','Formula')])
conservation_law_analysis <- function(nu,iv,verbose=getOption("uqsa.verbose", interactive())) {
	if (is.matrix(iv) && verbose){
		cli::cli_alert_info(
			paste(
				"[conservation_law_analysis] determines the default inputs (from initial values),",
				"not the experiment specific inputs; that is done by [experiments].",
				collapsee='\n'
			)
		)
	}
	N <- pracma::nullspace(t(nu))
	if (is.null(N)) return(NULL)
	C <- pracma::rref(t(N))
	stopifnot(norm(C %*% nu)<1e-6)
	nm <- rownames(nu)
	colnames(C) <- rownames(nu)
	K <- numeric(NROW(C))
	allText <- character(NROW(C))
	if (is.character(iv)) {
		cli::cli_alert_info("initial values are not numeric.")
		iv <- .Call("lstrtod",iv) # guarantees no NA values
	}
	for (i in seq(NROW(C))){
		l <- which(abs(C[i,])>1e-3)
		for (k in l){
			if (all(k!=K)){
				K[i] <- k
				break
			}
		}
		j <- l[l!=k]
		if (C[i,k]!=1){
			Text <- paste0(
				sprintf("(%s_ConservedConst - (",nm[k]),
				paste0(
					sprintf("%+i*%s",round(C[i,j]),nm[j]),
					collapse=""
				),
				sprintf("))*(%i)",1.0/C[i,k])
			)
		} else {
			Text <- paste0(
				sprintf("%s_ConservedConst - (",nm[k]),
				paste0(
					sprintf("%+i*%s",round(C[i,j]),nm[j]),
					collapse=""
				),
				sprintf(")")
			)
		}
		if (as.logical(verbose)){
			ddt <- paste0("d/dt(",paste(sprintf("%+i*%s",round(C[i,l]),nm[l]),collapse=" "),") == 0")
			eq <- sprintf("%10s = %s",nm[k],Text)
			cli::cli_alert_info(sprintf("%40s  \U21D2  %s",ddt,eq))
		}
		allText[i] <- Text
	}
	CL <- data.frame(
		Eliminates=rev(K),
		value=rev(C %*% iv),
		Constant=rev(C %*% iv),
		ConstantName=rev(sprintf("%s_ConservedConst",nm[K])),
		Formula=rev(gsub("+1*","+",allText,fixed=TRUE)),
		row.names=rev(nm[K])
	)
	attr(CL,"lawMatrix") <- t(C)
	return(CL)
}

#' Updates the named values of vector `v` with values mentioned in data.frame d
#'
#' Given a named vector, this function will create a mtarix, with
#' several copies of the vector, with different values, dependeing on
#' some context. The context is each row of a `data.frame`, with
#' columns referring to the names of `v`, and values for the entries
#' in `v` for that row's context. Example: the rows can be different
#' experinemnts, with each experiment assigning new values to some
#' members of `v` (but not necessarily all).
#'
#' @param v a named vector
#' @param d data.frame with column names that correspond to those of `v`
#' @param as_type a character scalar indicating a type ('character','numeric','logical',etc.)
#' @return a matrix of dimension: length(v) × NROW(d)
#' @noRd
#' @examples
#' f <- uqsa_example("AKAR4")
#' m <- model_from_tsv(f)
#' iv <- values(m$Compound)
#' IV <- update_values(iv,m$Experiments)
#' print(IV)
update_values <- function(v,d,as_type="numeric"){
	if (is.null(v)) return(NULL)
	if (is.matrix(v) && NCOL(v)==1) {
		v <- column(v,1)
	}
	if (is.null(names(v))) stop("[update_values] v must be named")
	if (is.null(d) || prod(dim(d))==0) stop("non-empty data.frame d is mandatory")
	ret <- matrix(v,length(v),NROW(d),dimnames=list(names(v),rownames(d)))
	for (i in seq(NCOL(ret))){
		j <- names(v) %in% colnames(d)
		if (any(j)){
			ret[j,i] <- as(d[i,names(v)[j]],as_type)
		}
	}
	return(ret)
}

#' Fill a matrix with values from a smaller matrix
#'
#' Given two matrices `M` and `m` we consider `M` to be the full size
#' of the matrix and `m` the sparse representation. But, the sparsity only
#' concerns rows: some rows in `M` are supposed to be empty (NA), while others
#' must contain the values provided in `m`.
#'
#' @param M big matrix with rownames
#' @param m smaller matrix with rownames
#' @noRd
#' @return a copy of `M` with `M[j,]=m[k,]` where `k` and `j` are
#'     selected based on matching names.
fill_matrix <- function(M,m){
	if (is(m,"errors")) {
		em <- standard_error_matrix(m)
	}
	j <- which(rownames(M) %in% rownames(m))
	k <- which(rownames(m) %in% rownames(M))
	stopifnot(length(j)==length(k))
	M[j,] <- m[k,]
	if (is(M,"errors")){
		EM <- standard_error_matrix(M)
		EM[j,] <- em[k,]
		errors::errors(M) <- EM
	}
	return(M)
}

empty_error_matrix <- function(n,m,dimnames=NULL){
	M <- errors::set_errors(rep(NA,n*m))
	dim(M) <- c(n,m)
	base::dimnames(M) <- dimnames
	return(M)
}

time_series_experiments <- function(m,E,iv,input,out=rownames(m$Output),verbose=getOption("uqsa.verbose", interactive())){
	if (is.null(E) || NROW(E)==0) return(NULL)
	D <- vector("list",NROW(E))
	eventSchedule <- E$event %otherwise% character(NROW(E))
	tr <- m$Transformation
	for (i in seq(NROW(E))){
		d <- m[[rownames(E)[i]]]
		ev <- m[[eventSchedule[i]]]
		if (is.null(d)) {
			if (verbose) cli::cli_alert_info("no data provided for experiment ",rownames(E)[i])
			DATA <- NULL
		} else {
			DATA <- fill_matrix(
				empty_error_matrix(length(out),NROW(d),dimnames=list(out,NULL)),
				parse_concise(t(d),use.errors=TRUE)  # this is a matrix
			)
		}
		if (is.null(m[[eventSchedule[i]]])){
			event_list <- NULL
		} else {
			stopifnot(!is.null(tr))
			event_list <- list(
				time=as.double(ev$time),
				label=as.integer(match(ev$transformation,rownames(tr))-1),
				dose=as.double(ev$dose)
			)
		}
		inp <- as.double(input[,i])
		names(inp) <- rownames(input)
		if (is.null(out)){
			df <- d
		} else {
			df <- as.data.frame(parse_concise(as.matrix(d)))
		}
		D[[i]] <- list(
			measurements=df, # a data frame (sparse)
			data=DATA,       # a matrix (with missing values)
			input=inp,
			initialTime=as.double(E$t0[i] %otherwise% min(d$time)),
			initialState=iv[,i],
			outputTimes=as.double(d$time),
			events=event_list
		)
	}
	names(D) <- rownames(E)
	return(D)
}

dose_response_experiments <- function(m,E,iv,input,out=rownames(m$Output), verbose=getOption("uqsa.verbose", interactive())){
	if (is.null(E) || NROW(E)==0) return(NULL)
	TS <- list() # list of time series experiments
	t0 <- E$t0
	tf <- E$tf %otherwise% E$time
	tr <- m$Transformation
	eventSchedule <- character(NROW(E))
	if (!is.null(E$event) && any(nzchar(E$event))){
		if (verbose) cli::cli_alert_info("Dose response experiments are not (yet) fully compatible with events, this script will try its best.")
		eventSchedule <- E$event
	}
	for (i in seq(NROW(E))){
		d <- m[[rownames(E)[i]]] # data table
		ev <- m[[eventSchedule[i]]]
		ts <- vector("list",length=NROW(d))
		DATA <- fill_matrix(
			empty_error_matrix(length(out),NROW(d),dimnames=list(out,NULL)),
			parse_concise(t(d),use.errors=TRUE)  # this is a matrix
		)
		if (is.null(m[[eventSchedule[i]]])){
			event_list <- NULL
		} else {
			stopifnot(!is.null(tr))
			event_list <- list(
				time=as.double(ev$time),
				label=as.integer(match(ev$transformation,rownames(tr))-1),
				dose=as.double(ev$dose)
			)
		}
		df <- as.data.frame(parse_concise(as.matrix(d)))
		for (j in seq_along(ts)){
			u <- input[,i]
			names(u) <- rownames(input)
			inputMatrix <- update_values(u,df)
			x <- iv[,i]
			names(x) <- rownames(iv)
			initialStateMatrix <- update_values(x,df)
			ts[[j]] <- list(
				outputTimes=as.double(tf[i] %otherwise% df$time[j]),
				measurements=df[j,,drop=FALSE],
				data=DATA[,j,drop=FALSE],
				input=column(inputMatrix,j),
				initialState=column(initialStateMatrix,j),
				initialTime=as.double(E$t0[i]),
				events=event_list
			) # several time series experiments per 1 dose response table
		}
		names(ts) <- sprintf("%s_dose_%i",rownames(E)[i],seq_along(ts))
		TS <- c(TS,ts)
	}
	return(TS)
}

#' Extract Measured Data and Simulation Experiment Instructions
#'
#' This function accepts the model obtained via `model_fromt_tsv` or a
#' similar function. It finds the data tables for this model (if any
#' are present), and finds the simulation instructions to reproduce
#' these data sets using the model.
#'
#' This function requires that the files the model is stored as
#' contains measurements (data) that can be interpreted fairly
#' easily. Each data file needs columns that are named like the
#' observable quantities listed in the Output table.
#'
#' If the data is very indirectly related to the model, then we don't
#' interpret the data files themselves and the user needs to write a
#' specialized likelihood function to relate the raw data in the files
#' with something that the model does. In such cases, don't use this function.
#'
#' The simulation experiments returned here, include model input
#' parameters. Whenever conservation law analysis is performed, the
#' conserved constants are set as input parameters, because the
#' conserved amount can differ between experiments. For this reason
#' the Experiment table is interpreted differently in the presence of
#' conservation laws. Otherwise (no conservation laws), the `o`
#' parameter can be omitted.
#'
#' The instructions must be organized in a table called Experiment(s).
#' @param m the model (with data), as obtained via `model_from_tsv()`,
#'     or similar.
#' @param o the ode derived from `m`, only necessary if the
#'     experiments need to take conservation laws into account
#' @param verbose logical, whether to preint diagnostic messages about the experiments
#' @return a list of simulation instructions
#' @export
#' @examples
#' f <- uqsa_example("AKAR4")
#' m <- model_from_tsv(f)
#' o <- as_ode(m)
#' ex <- experiments(m,o)
#' print(names(ex))
#' print(ex[[1]]$input)
experiments <- function(m,o=NULL,verbose=getOption("uqsa.verbose", interactive())){
	if (!is.list(m)) {
		stop("the first argument needs to be a list of file contents.")
	}
	if (is.finite(pmatch("Transformation",names(m))) && !is.null(o$conservationLaws) && verbose){
		cli::cli_alert_info(
			"CONFLICT: This model seems to have event based transformations and conservation laws. ",
			"These two concepts clash with one another whenever a compound is conserved, ",
			"but also changed by scheduled events."
		)
	}
	# The Experiment table is orchestrating the whole simulation procedure
	if (all(is.finite(pmatch('Experiment',names(m))))){
		E <- m$Experiment
	} else {
		if (verbose) cli::cli_alert_warning(paste(names(m),collapse=", ")) # part of error message
		stop("argument must contain an item named 'Experiment(s)'.")
	}
	# If there isn't an output table, then all state variables must be measurable
	if (all(is.finite(pmatch('Output',names(m))))){
		out <- rownames(m$Output)
	} else {
		out <- rownames(m$Compound)
	}

	iv <- update_values(values(m$Compound),m$Experiment) # by default
	input <- update_values(values(m$Input),E)
	if (missing(o) || is.null(o) || is.null(o$conservationLaws)){
		ConservedConst <- NULL
	} else {
		C <- o$conservationLaws %@% "lawMatrix"
		stopifnot(all(rownames(C)==rownames(iv)))
		ConservedConst <- pracma::flipud(t(C) %*% iv)
		rownames(ConservedConst) <- o$conservationLaws$ConstantName
		iv <- iv[-o$conservationLaws$Eliminates,]        # updated
	}
	input <- rbind(input, ConservedConst)

	if ("type" %in% colnames(E)){
		l <- grepl("[Dd]ose[- ]?[Rr]esponse",E$type)
	} else {
		l <- logical(NROW(E))
	}
	D <- c(
		time_series_experiments(m,E[!l,,drop=FALSE],iv[,!l,drop=FALSE],input[,!l,drop=FALSE],out),
		dose_response_experiments(m,E[l,,drop=FALSE],iv[,l,drop=FALSE],input[,l,drop=FALSE],out)
	)
	class(D) <- "experiments"
	return(D)
}

#' Subset experiments with preserved class
#'
#' The normal list subset operation would drop the class from the
#' experiment object (which is fine in theory). With this override,
#' the class is preserved.
#'
#' @param x an object with class "experiment"
#' @param i an index-set
#' @param ... passed on the list-\[ function
#' @export
#' @return subset of experiments
#' @examples
#' m <- model_from_tsv(uqsa_example("AKAR4"))
#' x <- experiments(m)
#' class(x)
#' class(x[seq(2)])
`[.experiments` <- function(x, i, ...) {
  # Use NextMethod to perform the actual subsetting as a list
  out <- NextMethod("[")
  # Re-assign the class back to the result
  class(out) <- class(x)
  return(out)
}
