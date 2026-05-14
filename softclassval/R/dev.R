##' Mark operator as deviation measure
##'
##' The operators measure either a performance (i.e. accordance between reference and prediction) or
##' a deviation. \code{dev (op) == TRUE} marks operators measuring deviation.
##'
##' @param op the operator (function)
##' @return logical indicating the type of operator. \code{NULL} if the attribute is missing.
##' @author Claudia Beleites
##' @seealso \code{\link{sens}} \code{\link{postproc}}
##' @export
##' @include softclassval-package.R
##' @include unittestdata.R
##'
##' @examples
##'
##' dev (wRMSE)
##' myop <- function (r, p) p * (r == 1)
##' dev (myop) <- TRUE
##'

dev <- function (op)
  attr (op, "dev")

##' @usage dev (op) <- value
##' @rdname dev
##' @param value logical indicating the operator type
##' @export "dev<-"
"dev<-" <- function (op, value){
  stopifnot (is.logical (value), !is.na (value))

  attr (op, "dev") <- value

  op
}

.test (dev) <- function (){
  myop <- function (){}
  checkTrue (is.null (dev (myop)))
  dev (myop) <- TRUE
  checkTrue (dev (myop))
  dev (myop) <- FALSE
  checkTrue (!dev (myop))
  checkException (dev (myop) <- NULL)
  checkException (dev (myop) <- NA)
}
