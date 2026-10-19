#' Read Concise Error Notation
#'
#' Convert a vector of strings of the form: c("1.2(3)E-4","1.2(3)E-2")
#' to a matrix with two rows:
#' 1. values,
#' 2. uncertainties.
#'
#' If the errors package is available, then an _errors_ object is
#' returned instead (uncertainties are an attribute). In that case the
#' dimensions of `v` are preserved on output. You can override this
#' choice using the second argument `use.errors`.
#'
#' Concise error notation means that a floating point number is
#' followed by an integer in parentheses which indicates the
#' uncertainty of the last digits of the value:
#' \deqn{1.2345(12) = 1.2345 \pm 0.0012}{1.2345(12) = 1.2345 ± 0.0012}.
#'
#' If the errors package is installed, then it will be used to
#' represent the return value.
#'
#' @export
#' @param v a character vector of numbers in concise error notation
#' @param use.errors if TRUE, the errors package will be used to
#'     return an object of type "errors" (from that
#'     package). Otherwise, the errors will be attached as an
#'     attribute (also called "errors" to be consistent with the
#'     errors package)
#' @param na a two element vector which will replace NA values,
#'     e.g. c(NA,NA); na=c(0,Inf) means _infinite uncertainty_ for missing values
#' @return either a numeric object with class errors (with the same
#'     dimensions as `v`), or a numeric matrix of values and
#'     uncertainties (2 rows), dimensions of original object are lost
#' @examples
#' x <- parse_concise(c("1.23(4)","0.51099895069(16)","1.25663706127(20)e-6","1.3±1.6","5;1"))
#' print(as.data.frame(x))
parse_concise <- function(v,use.errors=requireNamespace("errors"), na=c(NA,NA)){
	d <- dim(v)
	w <- .Call("concise",as.character(v))
	w[,is.na(v) | grepl("NA",v,ignore.case=TRUE)] <- na

	if (use.errors){
		w <- errors::set_errors(w[1,],w[2,])
		dim(w) <- d
		dimnames(w) <- dimnames(v)
	} else {
		u <- w[2,]
		w <- w[1,]
		attr(w,"errors") <- u
		names(w) <- names(v)
	}
	return(w)
}
