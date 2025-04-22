#' @title Local Interface Functions
#' @name LocalInterface
#' @param quote [\code{logical(1)}]\cr
#'   Quote all variables to cover invalid names.
#'   Use \code{\link[base]{make.names}} as an alternative.
#' @param x [\code{character(1)}]\cr
#'   String to check for invalid characters related to \code{\link[base]{make.names}}.
#'   Add backticks, if necessary.
#' @param data [\code{\link{data.frame}}]\cr
#'   Dataset with named columns. The names correspond to predictors and responses.
#' @param name [\code{character(1)}]\cr
#'   Name for output to Cornerstone.
#' @param brush [\code{logical(1)}]\cr
#'   Brushing of output dataset in Cornerstone across the R object.
#' @param width [\code{numeric(1)}]\cr
#'   Width of exported plotting object. See \code{\link[grDevices]{pdf}}.
#' @param height [\code{numeric(1)}]\cr
#'   Height of exported plotting object. See \code{\link[grDevices]{pdf}}.
#' @param R_object [\code{list}]\cr
#'   List of exported R objects to Cornerstone.
#' @description 
#'   CS-R interface functions are defined in package namespace via this file. Each function
#'   overwrites itself with the corresponding counterpart defined in the global environment
#'   from CS.
invokeFromR = function() {
  !exists("cs.out.dataset", where = pos.to.env(1), inherits = FALSE)
}

#' @rdname LocalInterface
cs.in.auxiliaries = function(quote = FALSE) {
  if (invokeFromR()) return()
  cs.in.auxiliaries = get0("cs.in.auxiliaries", envir = pos.to.env(1))
  cs.in.auxiliaries(quote = quote)
}

#' @rdname LocalInterface
cs.in.brushed = function() {
  if (invokeFromR()) return()
  cs.in.brushed = get0("cs.in.brushed", envir = pos.to.env(1))
  cs.in.brushed()
}

#' @rdname LocalInterface
cs.in.dataset = function() {
  if (invokeFromR()) return()
  cs.in.dataset = get0("cs.in.dataset", envir = pos.to.env(1))
  cs.in.dataset()
}

#' @rdname LocalInterface
cs.in.excluded = function() {
  if (invokeFromR()) return()
  cs.in.excluded = get0("cs.in.excluded", envir = pos.to.env(1))
  cs.in.excluded()
}

#' @rdname LocalInterface
cs.in.groupvars = function(quote = FALSE) {
  if (invokeFromR()) return()
  cs.in.groupvars = get0("cs.in.groupvars", envir = pos.to.env(1))
  cs.in.groupvars(quote = quote)
}

#' @rdname LocalInterface
cs.in.predictors = function(quote = FALSE) {
  if (invokeFromR()) return()
  cs.in.predictors = get0("cs.in.predictors", envir = pos.to.env(1))
  cs.in.predictors(quote = quote)
}

#' @rdname LocalInterface
cs.in.responses = function(quote = FALSE) {
  if (invokeFromR()) return()
  cs.in.responses = get0("cs.in.responses", envir = pos.to.env(1))
  cs.in.responses(quote = quote)
}

#' @rdname LocalInterface
cs.in.Robject = function(name = NA) {
  if (invokeFromR()) return()
  cs.in.Robject = get0("cs.in.Robject", envir = pos.to.env(1))
  cs.in.Robject(name = name)
}

#' @rdname LocalInterface
cs.in.scriptvars = function(name = NA) {
  if (invokeFromR()) return()
  cs.in.scriptvars = get0("cs.in.scriptvars", envir = pos.to.env(1))
  cs.in.scriptvars(name = name)
}

#' @rdname LocalInterface
cs.in.subsets = function() {
  if (invokeFromR()) return()
  cs.in.subsets = get0("cs.in.subsets", envir = pos.to.env(1))
  cs.in.subsets()
}

#' @rdname LocalInterface
cs.in.subsets.current = function() {
  if (invokeFromR()) return()
  cs.in.subsets.current = get0("cs.in.subsets.current", envir = pos.to.env(1))
  cs.in.subsets.current()
}

#' @rdname LocalInterface
cs.quote = function(x) {
  if (invokeFromR()) return()
  cs.quote = get0("cs.quote", envir = pos.to.env(1))
  cs.quote(x = x)
}

#' @rdname LocalInterface
cs.out.dataset = function(data, name = NA, brush = FALSE) {
  if (invokeFromR()) return()
  cs.out.dataset = get0("cs.out.dataset", envir = pos.to.env(1), inherits = FALSE)
  cs.out.dataset(data = data, name = name, brush = brush)
}

#' @rdname LocalInterface
cs.out.emf = function(name = NULL, width = 10, height = 10) {
  if (invokeFromR()) return()
  cs.out.emf = get0("cs.out.emf", envir = pos.to.env(1))
  cs.out.emf(name = name, width = width, height = height)
}

#' @rdname LocalInterface
cs.out.png = function(name = NULL, width = 480, height = 480) {
  if (invokeFromR()) return()
  cs.out.png = get0("cs.out.png", envir = pos.to.env(1))
  cs.out.png(name = name, width = width, height = height)
}

#' @rdname LocalInterface
cs.out.Robject = function(R_object, name = NA) {
  if (invokeFromR()) return()
  cs.out.Robject = get0("cs.out.Robject", envir = pos.to.env(1))
  cs.out.Robject(R_object = R_object, name = name)
}
