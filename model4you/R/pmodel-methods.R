
#' Objective function of personalised models
#'
#' Get the contributions of an objective function (e.g. likelihood contributions)
#' and the sum thereof (e.g. log-Likelihood).
#'
#' @param x,object object of class pmodel_identity (obtained by \code{pmodel(..., fun = identity)}). 
#' @param ... additional parameters passed on to \code{\link{objfun}}.
#' @param add_df it is not very clear what the degrees of freedom are in personalised models.
#'  With this argument you can add/substract degrees of freedom at your convenience. Default
#'  is \code{0} which means adding up the degrees of freedom of all individual models.
#'
#' For examples see \code{\link{pmodel}}.
#' 
#' @export
objfun.pmodel_identity <- function(x, ...) {
  
  nobs <- length(x)
  data <- attr(x, "data")
  if(is.null(data)) stop("Object pmodel_identity has to have a data attribute for objfun().")
  
  comp_of <- function(i) objfun(x[[i]], newdata = data[i, ], ...)
  ofl <- lapply(seq_len(nobs), comp_of)
  of <- unlist(ofl)
  
  attr(of, "class") <- "objfun"
  attr(of, "nobs") <- nobs
  attr(of, "df") <- sapply(ofl, attr, "df")
  
  return(of)
}

#' @rdname objfun.pmodel_identity
#' 
#' @export
logLik.pmodel_identity <- function(object, add_df = 0, ...) {
  
  islm <- sapply(object, function(x) {
    cx <- class(x)
    "lm" %in% cx & !("glm" %in% cx)
  })
  if(any(islm)) stop("logLik not yet implemented for pesonalised lm().")
  
  of <- objfun.pmodel_identity(object, ...)
  
  structure(
    sum(of),
    df = sum(attr(of, "df")) + add_df,
    nobs = object$nobs,
    class = "logLik"
  )
}