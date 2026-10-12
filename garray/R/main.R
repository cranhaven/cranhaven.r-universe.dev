#options(error=recover)
#source("R/zzz.R")

#' Generalized and smart array
#'
#' Creates or tests for generalized arrays.
#'
#' Generalized arrays are generalized because they handle dimensions and
#'	subdimensions that are ragged; and they are also smart
#'	because they automatically match dimensions by margins
#'	(names of dimnames).
#' Margins is implemented similar to R's native class "table", i.e.,
#'	use names of dimnames to store the margins
#'
#' Attribute sdim denotes subdimensions, which are the subdivision of
#'	dimensions or grouping of members of a dimension, for organizing a
#'	ragged array.
#'	It is a named list of numeric vectors, each of which indicates the
#'	lengths of subdivision groups within a dimension.  Every name of the
#'	list prefixed with a margin of the generalized array. By the matching
#'	of sdim names and dim names, utility functions figure out which
#'	dimensions the sub dimensions reside in.  Sum of a vector of the
#'	list usually equals to the extent of the corresponding dimension.
#'	If they are not equal and the extent can not be divided exactly
#'	by the sum, the subdimension is invalid and will be dropped.
#'	If the extent can be divided exactly
#'	by the sum, the subdimension is still valid but non-canonical.
#'	Non-canonical subdimension can be provided to `garray()` and `sdim<-`
#'	as argument, and the two functions canonicalize it.
#'	Other utility functions cannot handle non-canonical subdimension,
#'	thus manually constructing objects of garray class is permitted but
#'	dangerous.
#'	Values of each vector of the list denotes the
#'	repeating times of subdimension residing in the coresponding dimension
#'	(called superdim). 
#'	More than 1 subdimension reside in the same superdim is allowed.
#'	This feature allows dividing a subdimension further,
#'	organizing the subdims
#'	into hierachy.
#'
#' By definition and for S3 dispatching, `class(.)="garray"` is required, but
#'	simple arrays with proper margins actually work correctly with most
#'	functionalities of this package.  For the sake of compatibility and
#'	reducing warning message, `is.garray.duck()` tests whether the array
#'	has proper margins.
#'
#' A still problem is that attributes in R are fragile, even indexing
#'	will drop most attributes.  Utility functions and methods for
#'	dispatching for 'garray' implemented in this package guaranttee to
#'	save the margins (names of dimnames) and subdimension (attr(*,'sdim')).
#' @param data  Usually a simple array, and can be a vector without dimensions.
#' @param dim  An integer vector giving the maximal indices in each dimension.
#' @param dimnames  A list (or it will be ignored) with for each dimension
#'	one component, either ‘NULL’ or a character vector.
#' @param margins  Override the names of dim and of dimnames.
#' @param sdim  Optional, a named list of numeric vectors indicating the
#'	subdivision of some of the dimensions.  The value vill become,
#'	after validated, the attribute sdim.  See 'Details'
#'	and '?sdim'.
#' @param x  An R object.
#' @param ...  Additional arguments to be passed to or from methods.
#' @examples
#'	a1 <- garray(1:27, c(A=3,B=9), sdim=list(AA=c(a=2,b=1),BB=c(a=3)))
#'	a2 <- garray(1:27, c(A=3,B=9), sdim=list(AA=c(a=2,b=1),BB=c(a=4)))
garray <- function(data, dim=NULL, dimnames=NULL,
		margins=NULL, sdim=attr(data, "sdim", exact=TRUE)) {
	if (is.null(dim)) {
		data <- as.array(data) 
		d <- dim(data)	# array() keeps the names of dim.
		dn <- if (is.null(dimnames)) dimnames(data) else dimnames
	} else {
		d <- dim
		names(dim) <- NULL
		data <- array(data, dim, dimnames)
		dn <- dimnames(data)
	}
	if (is.null(dn)) dn <- vector("list", length(d))
	if (is.null(margins)) {
		margins <- if (!is.null(names(dn))) names(dn)
			else if (!is.null(names(d))) names(d)
			else stop("need margins")
	}
	names(dn) <- margins
	dimnames(data) <- dn
	class(data) <- "garray"
	sdim(data) <- sdim
	data
}

#' @describeIn garray  A simple and faster version of garray(),
#'	mainly for internal usage.  Note that garray() is not generic function,
#'	thus garray.array() will never be called by dispatching.
garray.array <- function(x, sdim) {
	class(x) <- "garray"
	stopifnot(is.garray.duck(x))
	sdim(x) <- sdim
	x
}

#' @rdname garray
as.garray <- function(x, ...) UseMethod("as.garray")

#' @rdname garray
as.garray.garray <- function(x, ...)
	if (is.garray(x)) x else stop("broken array")
# Issuing a stop because this function is called by S3 dispatching.

#' @rdname garray
as.garray.default <- function(x, ...) garray(x, ...)

#' @describeIn garray  `is.garray` do simple validation, no check for validity
#'	of sdim because it is too expensive. 
#'	Operation of sdim by this package is always guaranteed the validity.
is.garray <- function(x) {
	n <- names(dimnames(x))
	is.array(x)&&!is.null(n)&&!anyNA(n)&&all(""!=n)&&inherits(x, "garray")
	#warning("a valid array but no class")
}

#' @describeIn garray  `is.garray.duck` do duck-typing validation, ignoring
#'	the class
is.garray.duck <- function(x) {
	n <- names(dimnames(x))
	is.array(x)&&!is.null(n)&&!anyNA(n)&&all(""!=n)
}	# duck-typing validation without warning

#' @describeIn garray  Test whether the vector or array is actually a scalar
#'	(`length(x)==1L`).
is.scalar <- function(x) { 1L==length(x) }

#' Coerce to a Data Frame
#'
#' Convert a 2D generalized array into a data.frame,
#'	making `print()` work correctly.
#' @param x  A generalized array object.
#' @param row.names,optional,stringsAsFactors,...  See the same arguments in
#'	?as.data.frame.
#' @param col.names  'NULL' or a character vector giving the column names.
as.data.frame.garray <- function(x, row.names=NULL, optional=FALSE,
		col.names=NULL, ..., stringsAsFactors=FALSE) {
	stopifnot(2==length(dim(x)))	# Make View(x) work for 2-D array.
	row.names <- if (is.null(row.names)) rownames(x)
	col.names <- if (is.null(col.names)) colnames(x)
	if (!is.null(spd <- .superdim(x))) {
		sd <- sdim(x)
		if (is.null(row.names)) {
			row.names <- do.call("paste", c(lapply(sd[names(
				spd[spd==margins(x)[1]])], function(reptimes) {
					rep.int(names(reptimes), reptimes)
				}), list(1:nrow(x)), sep="|"))
		}
		if (is.null(col.names)) {
			col.names <- do.call("paste", c(lapply(sd[names(
				spd[spd==margins(x)[2]])], function(reptimes) {
					rep.int(names(reptimes), reptimes)
				}), list(1:ncol(x)), sep="|"))
			colnames(x) <- col.names
		}	# as.data.frame.matrix() not has option col.names
	}
	as.data.frame.matrix(unclass(x), row.names, optional, ...)
}

#' Print Values
#'
#' Print out a generalized array and returns it _invisibly_.
#' @param x  A generalized array object.
#' @param ...  Additional arguments to be passed to or from methods.
print.garray <- function(x, ...) {
	dn <- dimnames(x)
	if (1==length(dn)&&is.null(dn[[1]])) cat(names(dn), sep="\n")
	NextMethod("print")
}


#' The margins and dimensions of a generalized array object
#'
#' Margins means the names of dimnames of an array.
#' `margins<-` and `remargins` are for renaming, but
#'	`margins<-` ignores the names of `value` while
#'	`remargins` according to the names of `value` renames the margins. Doing
#'	so, `remargins` may also keep sdim.  `margins<-` always removes sdim.
#'	For `remargins` the length of value can be shorter than that of the
#'	margins if the value has names.
#' @param x  A generalized array.
#' @param value  A character vector will become the margins (names of dimnames)
#'	of the generalized array.
#'	`margins<-` ignores the names of `value` while
#'	`remargins` according to the names of `value` renames the margins.
#'	For `remargins` the length of value can be shorter than that of the
#'	margins if the value has names.
margins <- function(x) {
	if (!is.garray(x)) stop("margins of invalid array may be incorrect")
	names(dimnames(x))
}
# `margins()` used to be `names()`, but I realized that even an array
#	can have attribute `names`, which is by default retieved by `names()`.

#' @rdname margins
`margins<-` <- function(x, value) {
	if (!is.array(x)) {
		stop("set margins of non-array")
	} else {
		if (!inherits(x, "garray")) class(x) <- "garray"
	}	# try fix it.
	if (is.null(value)||anyNA(value)||any(""==value))
		stop("illegal margins")
	dn <- dimnames(x)
	if (is.null(dn)) dn <- vector("list", length(value))
	attr(x, "sdim") <- NULL
	names(dn) <- value
	dimnames(x) <- dn	# if value not match dim(x), stop here
	x
}

#' @rdname margins
remargins <- function(x, value) {
	if (!is.array(x)) {
		stop("set margins of non-array")
	} else {
		if (!inherits(x, "garray")) class(x) <- "garray"
	}	# try fix it.
	if (is.null(value)||anyNA(value)||any(""==value))
		stop("illegal margins")
	dn <- dimnames(x)
	if (is.null(dn)) {
		dn <- vector("list", length(value))
	} else if (!is.null(nn <- names(value))) {
		sd <- sdim(x)
		if (!is.null(sd)) {
			spd <- .superdim(x)
			names(sd) <- vapply(names(sd), function(k) {
				if (spd[k]%in%nn) substr(k, 1, length(spd[k])
					) <- value[spd[k]]
				k
			}, "")
		}
		m <- names(dn)
		value <- value[m]
		m[!is.na(value)] <- value[!is.na(value)]
		value <- m
		attr(x, "sdim") <- sd
	} else {
		attr(x, "sdim") <- NULL
	}
	names(dn) <- value
	dimnames(x) <- dn
	x
}

#' Dimensions of a generalized array
#'
#' Retrieve or set the dimension of a generalized array.
#' 
#' The functions `dim` and `dim<-` are internal generic primitive functions.
#' Here `dim.garray` and `dim<-.garray` are methods for 'garray's, which
#'	returns and setting with the named dimensions (margins).  The two
#'	function is usually used as, for example, `dim(arr)` and
#'	`dim(arr) <- c(A=3,B=2)`.
#' Native R saves the names of dim but seldom uses it.  However, it is
#'	undocumented and not stable because some functions 
#'	  discard it (like: t()) .  This package will totally neglect it but
#'	keeps the margins in dimnames.
#' @param x  An generalized array.
#' @param value  An integer (can be coerced from double numeric) vector, with
#'	names.
#' @aliases dim dim<-
`dim.garray` <- function(x) {
	d <- NextMethod("dim")
	names(d) <- margins(x)	# names of dim() is kept by dimnames(). 
	d	# dim(x) can have names, but I do not respect it.
}

#' @rdname dim.garray
`dim<-.garray` <- function(x, value) {
	n <- names(value)
	if (is.null(n)||anyNA(n)||any(""==n)) {
		warning("array assigned dim without names")
		class(x) <- NULL
		# Passing garray object into a function that not awares of
		#	garray and changes dim may issue the warning.  To avoid
		#	it, unclass() the object and then passing.
		NextMethod("dim<-")
	} else {
		dn <- vector("list", length(n))
		names(dn) <- n
		dn[n] <-dimnames(x)[n]	# save dimnames
		names(value) <- NULL	# keep names(attr(x,"dim")) NULL
		x <- NextMethod("dim<-")	# default dim() remove dimnames
		dimnames(x) <- dn
		x
	}
}

#' Indexing for the garray class
#'
#' Indexing along margins as usual `[.array`, and along subdim.
#'
#' @aliases [ [<- [.garray [<-.garray
#' @usage
#'	\method{[}{garray}(..., drop=TRUE)
#'	#`[.garray`(..., drop=TRUE)
#'	#x[i]
#'	#x[i,j,...,drop=TRUE]
#'	#x[m]
#'	#x[l]
#'	#x[M=i,N=j,...]
#FIXME: uncommenting the above usage results in Warning by devtools::check()
#' @param x  A generalized array from which elements are extracted or replaced.
#' @param i,j,m,l,M,N,...  In addition to the native styles (`i`, `j`, etc.)
#'	accepted by `[`, can be:
#'	1. a matrix `m` with column names,
#'		which (`colnames(m)`) is a permutation of margins of the array.
#	    #TODO: a missing margin means select all along that margin
#	#TODO: 1.2 - the colnames(.) can be from names of sdim, the return
#	#	will be a list of arrays.
#'	2. an list `l <- list(i,j,...)`, can be unnamed or named,
#'		where NULL means to select all;
#'	3. arguments with names (`M`, `N`, etc), where `NULL` and missing
#'		means to select all.
#'
#'	These extensions make indexing 3 times slower than native indexing.
#'	Since it is hard to assign MissingArg in list(), at the moment
#'	MissingArg is only safe in native R subsettting style.
#'	Using NULL to select all like MissingArg is actually not consistent
#'	in semantic of other uses of NULL. As shown by what `c()` returns,
#'	NULL is a generalized form of `logical(0)`, `integer(0)`,
#'	and `character(0)`, all of which means to select none when indexing.
#'	So take care of NULL if indexing with variables.
#' @param drop  Whether indeces where 1==dim are removed.  Different from
#'	R's native `[`, a garray will become a garray or scalar, never a vector.
#' @param value  An array or a scalar.
#' @examples
#'	mm <- matrix(c(1:3,1), 2, 2, dimnames=list(NULL, c("B","A")))
#'	a <- garray(1:27, c(A=3,B=9), sdim=list(AA=c(a=2,b=1),BB=c(a=3)))
#'	b <- a[mm]
#'	c1 <- a[B=1:2,A=NULL]
#'	c2 <- a[B=1:2,A=]
#'	c3 <- a[B=1:2]
#'	c4 <- a[list(B=1:2)]
#'	c5 <- a[list(B=1:2,A=NULL)]
#'	c6 <- a[list(NULL,1:2)]
#'	d1 <- a[,] ; d1[B=1:2,A=NULL]       <- c1*10
#'	d2 <- a[,] ; d2[B=1:2,A=]           <- c1*10
#'	d3 <- a[,] ; d3[B=1:2]              <- c1*10
#'	d4 <- a[,] ; d4[list(B=1:2)]        <- c1*10
#'	d5 <- a[,] ; d5[list(B=1:2,A=NULL)] <- c1*10
#'	d6 <- a[,] ; d6[B=1:2,A=NULL] <- 1
#'	d7 <- a[,] ; d7[mm] <- 1000
#'	d8 <- a[,] ; d8[mm] <- 1:2*1000
#'	e1 <- a[AA=1,drop=FALSE]
#'	e11 <- a[AA=c(1,1),drop=FALSE]
#'	e2 <- a[AA="b",drop=FALSE]
#'	ebb <- a[AA=c("b","b"),drop=FALSE]
#'	e3 <- a[,] ; e3[AA="b"] <- e2*10
#'	e33 <- a[,] ; e33[AA=c("b","b")] <- c(e2*0.1, e2*100)
#'	# Work in the same manner of `e33[c(3,3),] <- c(e2*0.1, e2*100)`.
#'	e4 <- a[A=c(TRUE,FALSE,FALSE),drop=FALSE]
#'	e5 <- a[A=TRUE,drop=FALSE]
#'	e6 <- a[B=c(TRUE,FALSE,FALSE),drop=FALSE]
#'	e7 <- a[AA=TRUE,drop=FALSE]
#'	e8 <- a[AA=c(TRUE,FALSE),drop=FALSE]
`[.garray` <- function(..., drop=TRUE) {
	n <- margins(..1)
	arg <- match.call()[-c(1:2)]	# fast sys.call() not work for `[`(...)
	arg$drop <- NULL
	argn <- names(arg)
	exist2 <- !missing(..2)&&(is.null(argn)||""==argn[1])
	if (exist2&&is.matrix(..2)&&is.numeric(..2)&&
			1==length(arg)&&!is.null(colnames(..2))) {
		nn <- match(n, colnames(..2))
		if (anyNA(nn)||0L<anyDuplicated(nn)) {
			Z <- NextMethod("[", drop=FALSE)
		} else {
			Z <- .subset(..1, ..2[,nn])
		}
	} else if ((!is.null(argn)&&all(""!=argn))||
			(exist2&&is.list(..2)&&1==length(arg))) {
		pf <- parent.frame()
		idx <- .index_canonical(..1, lapply(arg, function(i)
			if (is.symbol(i)&&""==as.character(i)) NULL
			else eval(i, pf)))
		idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
		# double quote(), for do.call() and .subset().
		Z <- do.call(".subset", c(list(..1), idx, drop=FALSE))
	} else {
		Z <- NextMethod("[", drop=FALSE)
	}	# `[.array` not keep margins if elements of dimnames is NULL
	d <- dim(Z)
	if (length(n)==length(d)) {
		names(d) <- n
		spd <- .superdim(..1)
		sd <- sdim(..1)[names(spd)[spd%in%names(d)[d==dim(..1)&1L!=d]]]
		if (drop) d <- d[1L!=d]
		if (0L==length(d)) return(as.vector(Z))
		class(Z) <- "garray"
		dim(Z) <- d
		sdim(Z, warn=FALSE) <- sd
	}
	Z
}

#' @usage
#'	#\method{[}{garray}(...) <- value
#'	#`[<-.garray`(..., value)
#'	#x[i] <- value
#'	#x[i,j,...] <- value
#'	#x[m] <- value
#'	#x[l] <- value
#'	#x[M=i,N=j,...] <- value
#' @rdname sub-.garray
`[<-.garray` <- function(..., value) {
	cl <- oldClass(..1)
	n <- margins(..1)
	arg <- match.call()[-c(1:2)]
	arg$value <- NULL
	argn <- names(arg)
	exist2 <- !missing(..2)&&(is.null(argn)||""==argn[1])
	matrix2 <- exist2&&is.matrix(..2)&&is.numeric(..2)&&1==length(arg)
	if (!matrix2&&is.garray(value)) {
		sd <- c(sdim(..1), sdim(value))
		d <- dim.garray(value)	# d is named
		if (0L<length(newname <- n[!n%in%names(d)])) {
			dd <- rep.int(1L, length(newname))
			names(dd) <- newname
			d <- c(d, dd)
			dim(value) <- d
		}
		value <- aperm(value, perm=n)
	}
	if (matrix2&&!is.null(colnames(..2))) {
		nn <- match(n, colnames(..2))
		if (anyNA(nn)||0L<anyDuplicated(nn)) {
			Z <- NextMethod("[<-")
		} else {
			Z <- `[<-`(unclass(..1), ..2[,nn], value=value)
			#NextMethod("[<-", ..1, ..2[,nn], value=value) not work
		}
	} else if ((!is.null(argn)&&all(""!=argn))||
			(exist2&&is.list(..2)&&1==length(arg))) {
		pf <- parent.frame()
		idx <- .index_canonical(..1, lapply(arg, function(i)
			if (is.symbol(i)&&""==as.character(i)) NULL
			else eval(i, pf)))
		idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
		if (is.garray(value)) value <- .rep.garray(value, dim(do.call(
			".subset", c(list(..1), idx, drop=FALSE))), sd)
		Z <- do.call("[<-", c(list(unclass(..1)), idx, list(value=
			value)))	# Need a fast method for dim(value[...])
		# Need subsetting sdim(Z)
	} else {
		if (is.garray(value)) value <- .rep.garray(value,
			dim(.subset(..., drop=FALSE)), sd)
		Z <- NextMethod("[<-")
	}
	#sdim(Z) <- sdim(..1)
	class(Z) <- cl
	Z
}

.index_canonical <- function(x, idx) {
# In the extension style of indexing, I use NULL to denote something like
#	MissingArg and avoid non-standard evaluation in this function.
	n <- margins(x)
	idxn <- names(idx)
	if (is.list(idx[[1]])&&(is.null(idxn)||""==idxn[1])) {
		stopifnot(1==length(idx))
		idx <- idx[[1]]
		if (is.null(names(idx))) return(idx) else idxn <- names(idx)
	}
	d <- dim(x)
	l <- vector("list", length(n))
	l[] <- list(NULL)
	names(l) <- n
	idx_not_sd <- idxn%in%n
	l[idxn[idx_not_sd]] <- idx[idx_not_sd]
	if (!all(idx_not_sd)) {
		spd <- .superdim(x)
		sd <- sdim(x)
		for (k in idxn[!idx_not_sd]) {
			reptimes <- sd[[k]]
			if (is.null(reptimes))
				stop(sprintf("no sdim named `%s`", k))
			offset <- cumsum(reptimes)-reptimes
			i <- idx[[k]]
			if (is.logical(i)) i <- seq_along(reptimes)[i]
			l[[spd[k]]] <- if (1L<length(i))
				do.call("c", lapply(i, function(j)
					seq_len(reptimes[j])+offset[j]))
				else if (1L==length(i))
					seq_len(reptimes[i])+offset[i]
		}
	}
	names(l) <- NULL
	l
}


#' Subdimensions of an array
#'
#' Retireve or set the subdimension of an array.
#'
#' Validation of subdimension is expensive because its consistency with dim need
#'	checking.  Thus most of functions do not validate it. 
#'	Operations of subdimension with functions discussed here are guaranteed to
#'	be always keeping the consistency.
#' @param x  A generalized array.
#' @param warn  Whether issue warning when some of subdimensions are invalid
#'	and get dropped.
#' @param value A named list of numeric vectors indicating the
#'	subdivision of some of the dimensions.  The value vill become,
#'	after validated, the attribute sdim.  See '?garray'.
#' @return  The subdimensions (a non-empty list) or NULL
sdim <- function(x) {
	if (!is.garray(x)) {
		warning("sdim of non-array cannot be retrieved")
		return(NULL)
	}
	attr(x, "sdim", exact=TRUE)
}

#' @rdname sdim
`sdim<-` <- function(x, warn=TRUE, value) {
	if (0L==length(value)) {
		attr(x, "sdim") <- NULL
		return(x)
	} else if (!is.list(value)) {
		warning("sdim should be a non-empty list with names") 
		return(x)
	} else if (!is.garray(x)) warning("assign sdim to non-array object")
	d <- dim.garray(x)
	spd <- .validate_sdim(d, value, warn=warn)
	if (0L<length(spd)) {
		nsd <- names(spd)
		names(nsd) <- nsd	# so lapply() return named list
		attr(x, "sdim") <- lapply(nsd, function(k) {
			reptimes <- as.integer(value[[k]])
			na <- names(value[[k]])
			ti <- d[spd[k]]%/%sum(reptimes)
			if (1L<ti) {
				reptimes <- rep.int(reptimes, ti)
				if (!is.null(na)) na <- make.unique(
					rep.int(na, ti))
			}
			names(reptimes) <- na
			reptimes
		})
		# If `sdim<-`() always save canonical reptimes, I do not need
		#	`rep(reptimes, length.out[k]/sum(sd[[k]]))` later.
	} else {
		attr(x, "sdim") <- NULL
	}
	x
}


# Supressing or elevating subdims
#
# @param superdim  Named character.
#	If a value is "" or NA, the name should be the name of some subdim. And
#	if the reptimes of the subdimension equal an integer, the subdimension will be
#	elevated into a margin; if the reptimes are not equal, stop.
#	If a value is some margin, the name should be some other margin,
#	the later will become a subdimension (renamed to "value.name") residing in
#	the former margin.
#TODO: resdim <- function(x, superdim) { }


# Extract and validate superdim for compatible with subdim.
#	Extract is cheap, but validation is expensive.
.superdim <- function(x) {
	sd <- sdim(x)
	if (!is.list(sd) || 0L==length(sd)) return(character())
	sd[] <- names(sd)	# sd has names, which is respected by vapply().
	n <- margins(x)		# dim names (margins and superdim)
	vapply(sd, function(k) n[startsWith(k, n)], "")
	# If there are inconsistant superdim, vapply() will error.
}
.validate_sdim <- function(d, sd, warn=TRUE) {
	if (!is.list(sd) || 0L==length(sd)) return(character())
	n <- names(d)	# margins names (superdim), d can be dim of non garray
	m <- names(sd)	# subdimension names
	stopifnot(is.numeric(d), !is.null(n), !anyNA(n), ""!=n,
		!is.null(m), !anyNA(m), ""!=m)
	starts <- outer(m, n, startsWith)
	nsuperdim <- rowSums(starts)
	if (warn&&any(1!=nsuperdim)) warning(sprintf(
		"sdim not prefixed with exactly 1 dim: %s (%s)",
		paste(m[1!=nsuperdim], collapse=", "), paste(n, collapse=", ")))
	spd <- vapply(seq_along(m), function(i) n[starts[i,]][1], "")
	names(spd) <- m
	idx <- vapply(names(spd), function(k) {
		if (is.na(spd[k])) return(FALSE)
		s <- sum(sd[[k]])
		if (0L==d[spd[k]]%%s) TRUE else {
			if (warn) warning(sprintf(
				"sdim %s (sum=%d) unmatch margin %s (dim=%d)",
				k, s, spd[k], d[spd[k]]))
			FALSE	# remove it
		} }, TRUE)
	spd[idx]
}


#' General array transposition
#'
#' Restore garray attributes that discarded by aperm.default().
#'	Cannot permute between subdimension (means to promote subdimension of
#'	equal length
#'	into regular dim and reduce dim into subdimension), sorry.
#' @param a  A generalized array to be transposed.
#' @param perm  Desired margins after permutation, integer or character.
#' @param ...  Useless, potential arguments inherited from the S3 generic.
aperm.garray <- function(a, perm=NULL, ...) {
	if (all(perm==margins(a))||all(perm==seq_along(margins(a)))) return(a)
	Z <- aperm.default(a=a, perm=perm, resize=TRUE)
	class(Z) <- "garray"
	attr(Z, "sdim") <- attr(a, "sdim", exact=TRUE)	# no further validation
	Z
}


#' Combine generalized arrays
#'
#' Combine generalized arrays, similar to the manner cbind and rbind work. 
#'	Put a sequence of generalized arrays and get a single generalized array
#'	of the same or one more margins.
#'
#' Combine sdim correctly.
#'	Saving or dropping of subdimensions follow a few rules: subdimensions of
#'	the margin with bound along are dropped; of the other margins are save
#'	unless the names of subdimensions are the same; subdimensions of the
#'	same names are dropped except the first one.
#' @param ...  arrays, or a list of several arrays.  Margins of
#'	these arrays should be the same.
#' @param along  The dimension along which to bind the arrays. 
#'	The arrays may have different lengths along that dimension,
#'	and are bind along it, with addition subdimension '*.bind.from'
#'	indicating the composition
#'	of the `along` dimension.  Some arrays may not
#'	have that margin, then the dimension of these arrays expand to 1.
#'	If `along` is a totally new margin, it is created.
#'	In such case, all arrays should have the same dimension.
#' @param margins  Resulting margins.  If includes a totally new margin (not
#'	used by any arrays in `...`), then
#'	`along` is neglected and the new margin setting `along`.
#'	If no new name and `length(along)==0L`, the last `margins` become along.
#'	If `0L==length(margins)`, `along` will become last dimensions of output.
#' @examples
#'	a <- garray(1:24, c(4,6),
#'		dimnames=list(X=1:4, Y=letters[1:6]),
#'	 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#'	b <- garray(1:6/10,6,dimnames=list(Y=letters[1:6]))
#'	ab <- abind(a=a, b=b, along="X")
#'	#abind(a, b, margins=c("X","Y"))	# Error
#'	ab2 <- abind(a=a, b=b, margins=c("X","Y"), along="X")
#'	aa <- abind(a=a, a=a, along="Z")
#'	ab3 <- abind(a, b, along="X")
abind <- function(..., margins=character(), along=character()) {
	dots <- list(...)
	if (0L==length(dots)) return(NULL)
		else if (1L==length(dots) && is.list(dots[[1]]))
		dots <- dots[[1]]
	if (!all(vapply(dots, is.garray, TRUE))) stop("need one list of arrays")
	if (0L==length(margins))
		margins <- unique(c(do.call("c", lapply(dots, margins)), along))
	new <- !margins%in%margins(dots[[1]])
	stopifnot(2>sum(new))
	if (0L==length(along)) along <-
		if (0L==sum(new))  margins[length(margins)]
		else if (1L==sum(new)) margins[new]
		else stop("too many new margins")
	n <- c(margins[margins!=along], along)
	d <- dim(dots[[1]])
	d[along] <- 0
	d <- d[n]
	dn <- dimnames(dots[[1]])
	dn[[along]] <- character()
	dn <- dn[n]
	sd <- list()
	newsd <- integer()
	nam <- make.names(names(dots), unique=TRUE)
	if (0L==length(nam)) nam <- make.unique(rep(along, length(dots)))
	for (i in seq_along(dots)) {
		new <- !n%in%margins(dots[[i]])
		di <- dim(dots[[i]])
		stopifnot(2>sum(new))
		if (1L<sum(new)) {
			stop(sprintf("too many new margins for dots[[%d]]", i))
		} else if (1L==sum(new)) {
			di[n[new]] <- 1
			dim(dots[[i]]) <- di
		}	# append with one more margin (length 1)
		dots[[i]] <- aperm(dots[[i]], n)
		if (any((dim(dots[[i]])!=dim(dots[[1]]))[-length(n)]))
			stop("too many dimensions not match")
		d[along] <- d[along]+dim(dots[[i]])[along]
		newsd[nam[i]] <- d[along]
		dn[[along]] <- c(dn[[along]], paste(nam[i],
			dimnames(dots[[i]])[[along]], sep="."))
		sdi <- sdim(dots[[i]])
		for (k in names(sdi)) {
			if (startsWith(k, along)) next
			#warning(sprintf("sdim named `%s` is overridden", k))
			if (is.null(sd[[k]])) sd[[k]] <- sdi[[k]]
			else if (!identical(sd[[k]], sdi[[k]])) warning(sprintf(
				"discard divergence sdim %s for %d", k, i))
		}
	}	# used to be based on lapply(), but `<<-` is tricky.
	sd[[paste0(along, ".bind.from")]] <- diff(c(0, newsd))
	aperm(garray(do.call("c", dots), d, dn, margins=n, sdim=sd), margins)
}


# Simplified list to vector of some type (usually atomic) if possible
# @param value1  A prototype of the return.
# @return  An list (if impossible) or an simple array.
.simplify2array <- function(x, value1=x[[1]]) {
	if (!is.list(x)) stop("non-list need not simplifying")
	l <- unique.default(lengths(x))
	if (is.recursive(value1)) return(x)
	if (1!=length(l)||l!=length(value1)||is.null(value1)) {
		#any(vapply(x, typeof, "")!=typeof(value1))	# time consume
		warning("impossible to simplify the result to array")
		return(x)
	}
	if (is.array(value1)) {
		d1 <- dim(value1)
		dn1 <- dimnames(value1)
		n1 <- names(dn1)
		if (0L==length(n1)||anyNA(n1)||any(""==n1))
			warning("elements without graceful names")
	} else if (1L<l) {
		d1 <- length(value1) 
		dn1 <- list(VALUE1=names(value1))
	} else {
		d1 <- integer()
		dn1 <- NULL
	}	# if 0L==length(value1), dim(Z) will have 0 and 0==length(Z)
	Z <- do.call("c", x)
	dim(Z) <- c(d1, length(x))
	dimnames(Z) <- c(dn1, list(NULL))
	Z
}

# @param length.out  The desired dimensions of the output. 
#	Note that `length(length.out)==length(dim(x))` is mandatory.
#	There are softwares that assume margins new to x to be 1 (like MATLAB
#	and numpy for Python), and I once consider implement the feature,
#	but I finally realize the feature is bug-prone and users do not need it.
#	In circumstance the feature is useful (`amap` and `[<-`),
#	I hard code it there.
# @param sdim  Provide subdimension for help matching dimensions of x and length.out.
#	Usually `x` does not has such information.
#	(x has dimensions smaller than length.out.  In function like `amap`,
#	this information is carried by arrays with larger dimensions).
.rep.garray <- function(x, length.out, sdim=NULL) {
	#tail=utils::tail(which(1<dim(x)), 1)
	# Dimension after that will not be repeated, mainly
	#	because .mapply() will do repeating automatically and quickly. 
	#	Use NULL to disable detecting of tail.
	d <- dim(x)
	n <- names(d)
	if (setequal(names(length.out), n)) {
		length.out <- length.out[n]
	} else {
		stopifnot(is.null(names(length.out)))
		names(length.out) <- n
	}
	stopifnot(length(length.out)==length(d), length.out>=d, 0L<d)
	if (all(length.out==d)) return(x)
	spd <- if (is.null(sdim)) character(0)
		else .validate_sdim(length.out, sdim)
	if (0==length(spd)||all(d==1L|d==length.out)) {
		head <- which(d==length.out)
		perm <- c(head, seq_along(length.out)[-head])
		return(aperm(array(x, length.out[perm]), order(perm)))
	}	# sweep() does it in this manner, seems faster than .subset().
	idx <- lapply(seq_along(d), function(i) {
		if (d[i]==length.out[i]) NULL
		else if (1L==d[i]) rep.int(1L, length.out[i])
		else if (any(ii <- spd==n[i])) {
			j <- names(spd)[ii]
			jj <- which(length.out[i]==vapply(sdim[j], sum, 0L)&
				d[i]==vapply(sdim[j], length, 0L))
			if (0L==length(jj)) stop("subdim match none")
			if (1<length(jj)) {
				warning("subdim match many")
				jj <- jj[1]
			}
			reptimes <- sdim[j][[jj]]
			rep.int(seq_along(reptimes), reptimes)
		}
		else stop("dimension not match")
	})
	idx[d==length.out] <- list(quote(quote(expr=)))
	#idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
	do.call(".subset", c(list(x), idx, drop=FALSE))
}	# mainly for internal usage, thus return only data, no dimnames.


#' Parallel summary, inspired by pmax() and pmin().
#'
#' Functions of Summary group are all, any, max, min, prod, range, and sum,
#'	which reduce a vector into a scalar (except range), thus the name
#'	of psummary().  Of course, other FUN can be passed-in,
#'	but functions like range() that returns a non-scalar vector result
#'	in unpredictable return.  For arguments of different
#'	size, pmin() and pmax() make fractional recycling and issue warning,
#'	but psummary() error since as.data.frame() do not fractionally recycle.
#' @param ...  Usually in the form `psummary(x, y, z, FUN=sum, na.rm=TRUE)`,
#'	alternaitvely `psummary(list(x, y, z), FUN=sum, na.rm=TRUE)`.
psummary <- function(...) UseMethod("psummary")

#' @rdname psummary
psummary.garray <- function(...) {
	dots <- list(...)
	ifun <- which(vapply(dots, is.function, TRUE))
	if (0L==length(ifun)) {
		args <- list()
		FUN <- sum
	} else {
		stopifnot(1<ifun[1])
		args <- dots[-(1:ifun[1])]
		FUN <- dots[[ifun[1]]]
		dots <- dots[1:(ifun[1]-1)]
	}
	if (1L==length(dots) && is.list(dots[[1]])) { dots <- dots[[1]] }
	stopifnot(vapply(dots, is.atomic, TRUE))
	do.call("amap", c(FUN, dots, args))
}

#' @rdname psummary
psummary.default <- function(...) {
	dots <- list(...)
	ifun <- which(vapply(dots, is.function, TRUE))
	if (0L==length(ifun)) {
		args <- list()
		FUN <- sum
	} else {
		stopifnot(1<ifun[1])
		args <- dots[-(1:ifun[1])]
		FUN <- dots[[ifun[1]]]
		dots <- dots[1:(ifun[1]-1)]
	}
	if (1L==length(dots) && is.list(dots[[1]])) { dots <- dots[[1]] }
	stopifnot(vapply(dots, is.atomic, TRUE))
	m <- as.matrix(as.data.frame(lapply(dots, as.vector)))
	# as.data.frame() do non-fractionally recycling
	s <- do.call("apply", c(list(m, 1, FUN), args))
	l <- vapply(dots, length, 0L)
	attributes(s) <- if (1L==which.max(l)) { attributes(dots[[1]]) }
	s
}


#' Read a complex table and return array in basic storagemode. 
#'
#' A complex table has
#'	several row and colume headers, some of which indicate the hierarchy
#'	of the dimensions (thus the returned array may have more dimensions than
#'	row and column), some of which are real row.names and
#'	col.names that will be turn into dimnames, and some of which are
#'	additional attributes.  Cells of non-header are all in a same format
#'	(like double).  The original header are preserved as attributes.
#'	The dimnames strictly save the layout of the matrix, thus row.names
#'	and col.names should be carefully chosen for matching the actual
#'	dimension.  Sometime aperm() is needed after this function.
#'	Sparse table, where dimnames is not complete (unbalanced) and needs
#'	non-fractionally recycling, is not supported.
#' @param file  The name of the file to be read in, see ('file' in ?read.table).
#' @param header  hierarchy structure of the header, a list of length 2,
#'	assigning the index of row and col header,
#'	affecting parsing of the input table;
#' @param row.names,col.names  a list, whose elements can be
#'	a character vector assigning the name of one dimension directly, or
#'	a integer scalar which is the index ({icol,irow}) of {row,col}-header
#'	that will be extracted; since the row and the col headers can have
#'	hierarchy in the input table and the hierarchy will be organized into
#'	addisional dimensions, rownames indicate the names of dimensions that
#'	are from the row of the input table, while colnames, the col; in the
#'	input table, the header can be in two pattern: AAA and AOO;
#'	in the AAA pattern, the names are repeated for the cells that are in
#'	the same index in high dimension, while in the AOO pattern, the names
#'	appear once at the first and the other cells that are in the same index
#'	in high dimension are left blank; in the
#'	output array, {row,col}.names are not necessary `dimnames[[1]]` and
#'	`dimnames[[2]]`;
#'	if all elements of the list are a integer scalar, then the list can
#'	also be coded as a integer vector (since they are the same for lapply);
#' @param ...  Further arguments to be passed to ‘read.table’.
#' @param storagemode  The storagemode of return matrix, usually 'double'.
# @examples
# express.prof <- read.ctable(
#	file.path("http://deleteome.holstegelab.nl/data/downloads",
#		"deleteome_responsive_mutants_ex_wt_var_controls.txt"),
#	header=list(1:2, 1:3), sep="\t", quote="\"",
#	row.names=list(1), col.names=list(2, 1))
read.ctable <- function(file, header, row.names, col.names, ..., storagemode="double") {
	dat <- utils::read.table(file, header=FALSE, dec=".",
		colClasses="character", ...)
	arr      <- unname(as.matrix(dat[-header[[1]],-header[[2]]]))
	storage.mode(arr) <- storagemode
	headrow  <- unname(as.matrix(dat[ header[[1]],-header[[2]]]))
	headcol  <- unname(as.matrix(dat[-header[[1]], header[[2]]]))
	headhead <- unname(as.matrix(dat[ header[[1]], header[[2]]]))
	dimnames <- c( lapply(row.names, function(i) {
		if (is.numeric(i) && 1L==length(i)) {
			i <- unique(headcol[,i])
			i <- i[""!=i]	# neglect empty rownames
		}
		return(i) }), lapply(col.names, function(i) {
		if (is.numeric(i) && 1L==length(i)) {
			i <- unique(headrow[i,])
			i <- i[""!=i]
		}
		return(i) }) )
	# Now arr is 2-dim, dimnames does not match dim(arr), but after runing
	#	the following line, dim(arr) is updated and dimnames(arr) gets
	#	discarded.  If run `dimnames(arr) <- dimnames` first, error.
	dim(arr) <- vapply(dimnames, length, 0)
	# Better than `arr <- array()` since it check for dimension match.
	dimnames(arr) <- dimnames
	attr(arr, "head") <- list(head=headhead, row=headrow, col=headcol)
	return(arr)
}


#' Mapping matching dimension of arrays with a function
#'
#' Generalized and smart mapply()/Map()/outer()/sweep() for data mapping.
#'	Matching is checked automatically.
#' @param FUN  Known vectorized function is recognized if passed in
#'	as character.  Other functions will be vectorized by `.mapply()`.
#' @param ...  Arrays with margins (names of dimnames) and maybe with sdim. 
#'	Orders of their margins can be different, but the extent along a
#'	margin is matched. Unmatched margins are broadcasting like outer(). 
#'	Scalar (length 1 vector) do not contribute margins and
#'	not broadcast here (they will broadcast by `.mapply()` later).
#' @param MoreArgs  a list of other arguments to 'FUN', no matching of margins.
#' @param SIMPLIFY  logical, attempt to reduce the result to exclude recursive
#'	structure (no list hierachy but plain generalized array).
#' @param VECTORIZED  Whether FUN is vectorized will affect the behaviours.
#'	Some combination of FUN and VECTORIZED is not simply slowing down,
#'	but produces meaningless results or even stop (e.g., cumsum).
#'	TRUE - call `FUN` once with arrays being reorganize on dimensions;
#'	FALSE - call `FUN` many times (via `.mapply`), with each cell of arrays.
#' @param e1,e2  Generalized arrays, being operands.
#' @return The dimensions is deduced from inputs.
#' @examples
#' a <- garray(1:24, c(4,6,2), dimnames=list(X=1:4, Y=letters[1:6], Z=NULL),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' b <- garray(1:6/10,6,dimnames=list(Y=letters[1:6]))
#' c <- garray(1:4/100,c(X=4))
#' d <- garray(1:4/1000,c(Y=4))
#' e <- garray(1:2/1000,c(X=2))
#' f <- garray(0,c(Z=2))
#' g <- garray(0,c(ZZ=2))
#' m1 <- amap(psummary,c,a,b,      0.0001, VECTORIZED=FALSE)
#' m2 <- amap(sum,     c,a,b,      0.0001, VECTORIZED=FALSE)
#' m3 <-               c+a+b+      0.0001
#' n1 <- amap(sum,     c,a,b,d,    0.0001, VECTORIZED=FALSE)
#' n2 <- amap(sum,     c,a,b,e,    0.0001, VECTORIZED=FALSE)
#' n3 <- amap(sum,     c,a,b,e,f,  0.0001, VECTORIZED=FALSE)
#' p1 <- amap(sum,     c,a,b,e,f,g,0.0001, VECTORIZED=FALSE)
#' q1 <- amap(sum, c,a,b,e,f,g,0.0001, SIMPLIFY=FALSE, VECTORIZED=FALSE)
#' q2 <- amap(c,   c,a,b,e,f,g,0.0001, SIMPLIFY=FALSE, VECTORIZED=FALSE)
#' q3 <- amap(list,c,a,b,e,f,g,0.0001, SIMPLIFY=FALSE, VECTORIZED=FALSE)
#' m1==m2
#' m2==m3
#' m2==aperm(m3, 3:1)
amap<- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=TRUE, VECTORIZED=NA) {
	dots <- lapply(list(...), function(X) if (is.scalar(X)) X
		else as.garray(X))
	if (is.na(VECTORIZED)) {
		VECTORIZED <- is.character(FUN)&&FUN%in%c(
			"abs", "sign", "sqrt","floor", "ceiling", "trunc",
			"round", "signif",
			"exp", "log", "expm1", "log1p",
			"cos", "sin", "tan","cospi", "sinpi", "tanpi",
			"acos", "asin", "atan",
			"cosh", "sinh", "tanh","acosh", "asinh", "atanh",
			"lgamma", "gamma", "digamma", "trigamma",
			"cumsum", "cumprod", "cummax", "cummin",
			"+", "-", "*", "/", "^", "%%",
			"&", "|", "!",
			"==", "!=", "<", "<=", ">=", ">",
			"Arg", "Conj", "Im", "Mod", "Re",
			"pmax", "pmin",
			"logsumexp")
		if (isTRUE(!VECTORIZED))
			warning("non-vectorized function is slow")
	}
	# Seldom run, because Ops is always called with VECTORIZED=TRUE when
	#	dispatching; and Math need not amap() at all, since they accept
	#	1 argument and preserve class=garray and sdim.
	n <- unique(do.call("c", lapply(dots, function(X)
		if (is.scalar(X)) character() else margins(X))))
	l <- length(n)
	ddat <- matrix(vapply(seq_along(dots), function(i) {
		if (is.scalar(dots[[i]])) return(rep.int(1L, l))
		d <- dim(dots[[i]])
		if (0L<length(newname <- n[!n%in%names(d)])) {
			dd <- rep.int(1L, length(newname))
			names(dd) <- newname
			d <- c(d, dd)
			dim(dots[[i]]) <<- d
		}
		d[n]
	}, integer(l)), l, length(dots), dimnames=list(n, names(dots)))
	#TODO: May be feature of moving tail is good, need reviving.
	# @param SAFE 
	# FALSE - use unsafe but fast implementation, all inputs are repeated as
	# few as possible, assuming that `FUN` recycles short vectors
	# (like basic operators "+"). 
	# TRUE - slower, all inputs are organized properly to have the same
	# dimension and passed to FUN.  For VECTORIZED=FALSE, SAFE is neglected
	# because `.mapply` recycle short vectors.
	#if (1L<l) ddat <-	# margin len 1 moved to tail
	#	ddat[sort(rowSums(1==ddat), index.return=TRUE)$ix,,drop=FALSE]
	#n <- rownames(ddat)
	d <- vapply(n, function(k) max(ddat[k,], na.rm=TRUE), 0L,USE.NAMES=TRUE)
	dn <- lapply(n, function(k) {
		na <- character(0)
		for (i in seq_along(dots)) {
			nn <- dimnames(dots[[i]])[[k]]
			if (d[k]==length(nn)&&length(nn)>length(na)) na <- nn
			if (length(na)>1) break
		}
		na
	})
	names(dn) <- n
	sd <- do.call("c", lapply(dots, function(X)
		if (is.garray(X)) sdim(X) else NULL))
	sd <- sd[unique(names(sd))]
	dots <- lapply(dots, function(X) as.vector(if (is.scalar(X)) X
		else .rep.garray(aperm(X, perm=n), d, sd)))
	# aperm(X) consume less memory than .rep.garray(X)
	names(d) <- NULL
	FUN <- match.fun(FUN)
	# avoid "object 'FUN' of mode 'function' was not found",
	#	should call once here, not depend on match.fun() within
	#	mapply(), apply() or tapply().
	if (VECTORIZED) {
		X <- do.call(FUN, c(dots, MoreArgs))
		dim(X) <- d	#X <- aperm(., n)
		dimnames(X) <- dn
		if (isTRUE(!SIMPLIFY))
			warning("always return an array for vectorized fun")
	} else {
		X <- .MAPPLY(FUN, dots, MoreArgs)
		if (isTRUE(SIMPLIFY)) {
			X <- .simplify2array(X)
			last <- length(dim(X))
			dnX <- dimnames(X)
			dim(X) <- c(dim(X)[-last], d)
			dimnames(X) <- c(dnX[-last], dn)
		} else {
			dim(X) <- d
			dimnames(X) <- dn
		}
	}
	class(X) <- "garray"	# garray() is too expensive
	sdim(X) <- sd
	X
}

#' @rdname amap
Ops.garray <- function(e1, e2) {	# Not include %*%
	FUN <- get(.Generic, envir=parent.frame(), mode="function")
	if (1L==nargs()) amap(FUN, e1, VECTORIZED=TRUE)
		else amap(FUN, e1, e2, VECTORIZED=TRUE)
}


#' Generalized and smart apply()/Reduce()/tapply() for data folding.
#'
#' @param FUN  Usually a summary function (like `all` and `sum`).
#' @param X  A garray, with margins (names of dimnames) and maybe with sdim.
#' @param MARGIN  Some margins of X and names of sdim.
#'	MARGIN=character() means to reduce all margins (over no margin).
#'	In such case, areduce() is not needed actually.
#' @param ...  Further arguments to 'FUN', no matching of margins.
#' @param SIMPLIFY  TRUE - simplifies the result list to vector of atomic
#'	if possible, and triggers warning and not simplifies if impossible;
#'	FALSE - not simplifies for non speed-up function, and issues warning
#'	(and have to simplify) for speed-up function; NA - simplifies but no
#'	warning if impossible.
#' @param SAFE  TRUE - use safe but slow implementation, in which data splited
#'	from the array are reorganized into small arrays (as are being subset
#'	by `[]`) and passed to FUN (other attributes are dropped, however);
#'	FALSE - faster, data are passed to FUN as dimension-less vectors.
#' @return A matrix (similar to return of apply() or tapply()), with the
#'	trailing margins the same as MARGIN, while the leading margins
#'	depend on FUN and SIMPLIFY.  If FUN returns a scalar or SIMPLIFY=FALSE,
#'	then no leading margins.  In MARGIN, subdimension is replaced with superdims.
#' @examples
#'	a <- garray(1:24, c(4,6),
#'		dimnames=list(X=LETTERS[1:4], Y=letters[1:6]),
#'		sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#'	x1 <- areduce("sum", a, c("X"))
#'	x2 <- areduce(`sum`, a, c("X"))
#'	stopifnot(garray(c(66,72,78,84), margins="X")==x1, x2==x1)
#'	yy1 <- areduce("sum", a, c("YY"))
#'	yy2 <- areduce(`sum`, a, c("YY"))
#'	stopifnot(garray(c(10,68,58,164), margins="Y")==yy1, yy2==yy1)
#'	xyy1 <- areduce("sum", a, c("X","YY"))
#'	xyy2 <- areduce(`sum`, a, c("X","YY"))
#'	stopifnot(xyy1==xyy2)
#'	xxyy1 <- areduce("sum", a, c("XX","YY"))
#'	xxyy2 <- areduce(`sum`, a, c("XX","YY"))
#'	stopifnot(garray(c(6,4,48,20,42,16,120,44), c(X=2,Y=4))==xxyy1)
#'	stopifnot(xxyy2==xxyy1)
#'	b <- garray(1:24, c(3,4,2),
#'		dimnames=list(X=LETTERS[1:3], Y=letters[1:4], Z=NULL),
#'		sdim=list(XX=c(x1=2,x2=1), YY=c(y1=1,y2=1)))
#'	xxyyz1 <- areduce("sum", b, c("XX","YY","Z"))
#'	xxyyz2 <- areduce(`sum`, b, c("XX","YY","Z"))
#'	stopifnot(xxyyz1==xxyyz2)
#'	xyz1 <- areduce(identity, b, c("XX","YY","Z"), SIMPLIFY=FALSE)
#'	xyz2 <- areduce("c",      b, c("XX","YY","Z"), SIMPLIFY=FALSE)
#'	xy1 <- areduce(identity, b, c("XX","YY"), SIMPLIFY=FALSE, SAFE=TRUE)
#'	stopifnot(identical(dimnames(xy1[2,3][[1]]), list(X="C",Y="c",Z=NULL)))
#'	# garray of lists, cannot use `xyz1==xyz2` etc to compare.
areduce <- function(FUN, X, MARGIN, ..., SIMPLIFY=TRUE, SAFE=FALSE) {
	stopifnot(is.garray(X), is.character(MARGIN))
	n <- margins(X)
	d <- dim(X)
	dn <- dimnames(X)
	sd <- sdim(X)
	na.rm <- ifelse(is.null(list(...)$na.rm), FALSE, list(...)$na.rm)
	if (all(MARGIN%in%n)) {
		n1 <- n[!n%in%MARGIN]
		X <- aperm(X, perm=c(n1, MARGIN))
		d1 <- prod(d[n1])
		d2 <- prod(d[MARGIN])	# prod(NULL)==1
		if (is.character(FUN)&&FUN%in%c("sum","mean")) {
			FUN <- c(sum=`.colSums`, mean=`.colMeans`)[[FUN]]
			# col{Sum,Mean}s are 3 times faster than row{Sum,Mean}s
			Z <- if (is.complex(X)) 
				FUN(Re(X), d1, d2, na.rm) +
				FUN(Im(X), d1, d2, na.rm)*1i
				else	# need not set dim(X) <- c(nrow, ncol)
				FUN(   X,  d1, d2, na.rm)
			if (isTRUE(!SIMPLIFY)||isTRUE(SAFE)) warning(
				"always return an array for sum and mean")
			if (0L<length(MARGIN)) {
				dim(Z) <- d[MARGIN]
				dimnames(Z) <- dn[MARGIN]
			}
		} else {
			class(X) <- NULL
			dim(X) <- c(d1, d2)
			FUN <- match.fun(FUN)
			if (SAFE) {
				Z <- .LAPPLY(seq_len(d2), function(i)
					forceAndCall(1, FUN, garray(X[,i],
						d[n1], dn[n1], sdim=sd), ...))
			} else {
				Z <- .LAPPLY(seq_len(d2), function(i)
					forceAndCall(1, FUN, X[,i], ...))
			}
			if (isTRUE(SIMPLIFY)) Z <- .simplify2array(Z)
			last <- length(dim(Z))
			dnZ <- dimnames(Z)
			if (0L<length(MARGIN)) {
				d <- c(dim(Z)[-last], d[MARGIN])
				dn <- c(dnZ[-last], dn[MARGIN])
			} else {
				d <- dim(Z)[-last]
				dn <- dnZ[-last]
			}
			if (0L==length(d)) {
				dim(Z) <- NULL
			} else {
				dim(Z) <- d
				dimnames(Z) <- dn
			}
		}
	} else {
		spd <- .superdim(X)
		if (!all(MARGIN%in%c(names(spd), n))) stop("foreign MARGIN")
		if (any(MARGIN%in%spd[MARGIN]))
			stop("some MARGINs are subdim residing in MARGINs")
		sdMARGIN <- MARGIN%in%names(spd)
		n0 <- spd[MARGIN[sdMARGIN]]	# along sdim (partial reduced)
		n2 <- MARGIN[!sdMARGIN]		# along dim
		n1 <- n[!n%in%c(n0, n2)]	# reduced dim
		X <- aperm(X, perm=c(n0,n1,n2))
		d0 <- prod(d[n0])
		d1 <- prod(d[n1])
		d2 <- prod(d[n2])
		class(X) <- NULL
		dim(X) <- c(d0*d1, d2)
		ngroup <- vector("list", sum(sdMARGIN))
		names(ngroup) <- spd[MARGIN[sdMARGIN]]
		group <- rep.int(1L, d0)
		extent <- 1L
		din <- cumprod(d[n0])	# inner dimension
		for (k in MARGIN[sdMARGIN]) {
			nn <- spd[k]
			reptimes <- sd[[k]]
			#storage.mode(reptimes) <- "integer"
			# Guarantee integer, or rowsum() will return all 0.
			na <- names(reptimes)
			ngroup[[nn]] <- if (is.null(na)) #||anyDuplicated(na)
				seq_along(reptimes) else na
			group <- group+extent*(rep.int(rep.int(seq_along(
				reptimes), reptimes*(din[nn]/d[nn])),
				d0/din[nn])-1L)
			extent <- extent*length(reptimes)
		}
		sd0 <- sd[MARGIN[sdMARGIN]]
		sd[MARGIN[sdMARGIN]] <- NULL	# subdimension going to be reduced
		if (extent>.Machine$integer.max) stop(sprintf(
			"total number of levels > %d", .Machine$integer.max))
		MARGIN[sdMARGIN] <- n0
		if (identical("sum", FUN)) {	# Speed-up (10 times faster)
			#ugroup <- seq_len(extent)
			#Z <- .Internal(rowsum_matrix(X,
			#	rep.int(group, prod(d[n1])), ugroup,
			#	na.rm, as.character(ugroup)))
			Z <- rowsum.default(X,
				rep.int(group, prod(d[n1])), reorder=FALSE,
				na.rm=na.rm)
			# group should be integer; if double, result in 0.
			if (isTRUE(!SIMPLIFY))
				warning("always return an array for sum")
			dim(Z) <- c(lengths(ngroup), d[n2])
			dimnames(Z) <- c(ngroup, dn[n2])
			Z <- aperm(Z, MARGIN)
		} else {
			# Implementations UNLIST(Z) - Faster
			if (SAFE) {
				nn <- spd[names(sd0)]
				names(sd0) <- nn
				lsd0 <- lengths(sd0)
				extent1 <- cumprod(lsd0)
				extent2 <- extent1/lsd0
				subdn <- function(dn, reptimes, i) {
					offset <- cumsum(reptimes)-reptimes
					dn[seq_len(reptimes[i])+offset[i]]
				}
				f <- function(i, XX, ...) {
					idx <- (i-1L)%%extent1%/%extent2+1L
					n[n2] <- NULL
					FUN(aperm(garray(XX[[i]], c(.mapply(
						.subset, list(sd0, idx), NULL),
						d[n1]), c(.mapply(subdn,
						list(dn[nn], sd0, idx), 
						NULL), dn[n1]),
						c(nn, n1), sdim=sd), n), ...)
				}
				Z <- .LAPPLY(seq_len(d2), function(i) {
					XX <- split(X[,i], group)
					.LAPPLY(seq_along(XX), f, XX, ...)
				})
			} else {
				Z <- .LAPPLY(seq_len(d2), function(i)
					.LAPPLY(split(X[,i], group), FUN, ...))
			}
			Z <- do.call("c", Z)
			# Implementations REP(GROUP) - Slower
			#group <- rep.int(group, d2)+
			#	extent*rep(seq_len(d2), each=d0*d1)
			#Z <- lapply(split(X, group), FUN, ...)
			if (isTRUE(SIMPLIFY)) Z <- .simplify2array(Z)
			last <- length(dim(Z))
			dnZ <- dimnames(Z)
			dim(Z) <- c(dim(Z)[-last], lengths(ngroup), d[n2])
			dimnames(Z) <- c(dnZ[-last], ngroup, dn[n2])
			Z <- aperm(Z, c(names(dnZ)[-last], MARGIN))
		}
	}
	if (0L<length(MARGIN)) {
		class(Z) <- "garray"	# garray() is too expensive
		sdim(Z, warn=FALSE) <- sd
	}
	Z
}


#' Generalized array's sweep() for data cleaning.
#'
#' Return a generalized array, by wiping out a summary statistic.
#'
#' @param X  A generalized array.
#' @param FUN  The wiping function.
#' @param STATS  Numeric array or function.
#' @param MARGIN  NULL - STATS is an array; character - STATS is a function,
#'	and by X being reduced along MARGIN, X is wiped.  Length 0 character
#'	vector means reducing along no margin, resulting in a scalar (in
#'	this case, for example, `areduce(sum, X)` is the same as `sum(X)`.
#' @param MoreArgs,SIMPLIFY,VECTORIZED  Argument used by 'amap()'.
#' @param ...  Argument used by 'areduce()'.
#' @examples
#' a <- garray(1:24, c(4,6), list(X=LETTERS[1:4], Y=letters[1:6]),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' m1 <- awipe(a, MARGIN="XX")
#' m2 <- awipe(a, `-`, mean, "XX")
awipe <- function(X, FUN="-", STATS="mean", MARGIN=NULL,
		MoreArgs=NULL, ..., SIMPLIFY=TRUE, VECTORIZED=NA) {
	if (!is.null(MARGIN)) STATS <- areduce(STATS, X, MARGIN, ...)
	amap(FUN, X, STATS, MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY,
		VECTORIZED=VECTORIZED)
}


#' Generalized array multiplication. 
#'
#' Default to Einstein summation convention, without explicitly subscripts.
#'
#' Margins shared by X and Y are parallelly mapped by FUN,
#'	and then reduced by SUM (inner product like `%*%`); 
#'	margins in BY and shared by X and Y are simply mapped by FUN
#'	but excluded from reducing (parallel product like `*`);
#'	other margins are extended repeatly (outer product like `%o%`).
#'	Shared margins not to be mapped have to be renamed (like outer product).
#'	For special FUN and SUM, fast algorithms are implemented.
#' @param X,Y  Generalized arrays that can be multiplied.
#' @param FUN  The 'multiply' function.
#' @param SUM  The 'reduce' function.
#' @param BY  margins excluded from summary by SUM.
#' @param MoreArgs,SIMPLIFY,VECTORIZED  Argument used by 'amap()'.
#' @param ...  Argument used by 'areduce()'.
#' @examples
#' a <- garray(1:24, c(4,6), list(X=LETTERS[1:4], Y=letters[1:6]),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' b <- garray(1:20, c(Z=5, X=4))
#' c <- garray(1:120, c(X=4,Y=6,Z=5))
#' m1 <- amult(a, b)
#' m2 <- amult(a, b, `*`, sum)
#' m3 <- amult(b, a)
#' all.equal(m1, m2)
#' all.equal(m1, m3)
#' all.equal(m1, t(m3))
#' n1 <- amult(a, c, `*`, sum)
#' n2 <- a%X%c
#' all.equal(n1, n2)
#' amult(garray(1:5,margins="I"), garray(1:8,margins="J"))
#' amult(garray(1:8,c(I=2,J=4)), garray(1:9,c(K=3,L=3)))
##@keywords garray, `%*%`
amult <- function(X, Y, FUN="*", SUM="sum", BY=NULL,
		MoreArgs=NULL, ..., SIMPLIFY=TRUE, VECTORIZED=TRUE) {
	nX <- margins(X)
	nY <- margins(Y)
	ni <- intersect(nX, nY)
	nx <- setdiff(nX, ni)
	ny <- setdiff(nY, ni)
	no <- union(nx, ny)
	dX <- dim(X)
	dY <- dim(Y)
	dnX <- dimnames(X)
	dnY <- dimnames(Y)
	sd <- c(sdim(X), sdim(Y))
	if (identical("*",FUN)&&identical("sum",SUM)&&0==length(BY)
			&&all(dX[ni]==dY[ni])) {
		if (length(ni)==1&&length(nX)==2&&length(nY)==2) {
			FUN <- array(list(crossprod, `%*%`,
				function(X,Y) t(Y%*%X), tcrossprod),
				c(2,2))[nX==ni,nY==ni][[1]]
			Z <- FUN(X, Y)
		} else {
			X <- aperm(X, perm=c(nx, ni))
			Y <- aperm(Y, perm=c(ni, ny))
			if (any(dX[ni]!=dY[ni])) {
				d <- pmax(dX[ni], dY[ni])
				X <- .rep.garray(X, c(dX[nx], d), sd)
				Y <- .rep.garray(Y, c(d, dY[ny]), sd)
			} else {
				d <- dX[ni]
			}
			attr(X, "dim") <- c(prod(dX[nx]), prod(d[ni]))
			attr(Y, "dim") <- c(prod(d[ni]), prod(dY[ny]))
			Z <- X%*%Y
		}
		dZ <- c(dX[nx],dY[ny])
		names(dZ) <- NULL
		dim(Z) <- dZ
		dimnames(Z) <- c(dnX[nx], dnY[ny])
		class(Z) <- "garray"
		sdim(Z, warn=FALSE) <- sd
		return(Z)
		# By definition, margins is important but not order.
		#	Sometimes seems saving commutative property of %*%.
	}	# Speed-up for special cases
	areduce(match.fun(SUM), amap(match.fun(FUN), X, Y, MoreArgs=MoreArgs,
		SIMPLIFY=SIMPLIFY, VECTORIZED=VECTORIZED), union(no, BY), ...)
}

#' @rdname amult
`%X%` <- function(X, Y) amult(X, Y)
# %*% cannot be used since it is S4 generic but not S3 generic.
# Cannot simply do `%X%` <- amult, see r-base-3.5.1/src/library/base/outer.R

#' Function composition operator
#'
#' Composite functions `a` and `b` into `a(b(...))`.
#'
#' @param a  A function that can be called with one argument.
#' @param b  A function that can be called with one or more argument,
#'	and result of `b()` can be passed to `a()`.
#' @return  A new function, whose 
#'	arguments are what `b()` can accept, and whose result is what `a()`
#'	can return.
#' @examples
#'	lse <- log%+%sum%+%exp
#'	lse(1:10)
#'	#logsumexp(1:10)	# actual logsumexp() is more sophistic
#'	log(sum(exp(1:10)))
#'	sum <- sd
#'	lse(1:10)	# lse() is fixed at definition
#'	log(sum(exp(1:10)))
#'	(log%+%sum%+%exp)(1:10)	# now is (log%+%sd%+%exp)
`%+%` <- function(a, b) {
	force(a)
	force(b)
	function(...) a(b(...))
}
# `+.function` not work since Ops dispatches on oldClass (attr("class"))
#	and not on class ('function' is implicit class).

# Generalized array cross product 
#
# Generalizaion of `%*%`, dist() and cor(), allowing cross along dimensions
#	of the same margins one by one.
# In general, for 2 matrices to 1 mapping,
#	`%*%` runs on row*column style ([2x4]%*%[4x3]=[2x3]),
#	var/cov/cor/crossprod on column*column style (cor([4x2], [4x3])=[2x3]),
#	dist2()/tcrossprod on row*row style (dist2([2x4], [3x4])=[2x3]).
#across <- function(X, Y, by, byX=by, byY=by, FUN) { }
