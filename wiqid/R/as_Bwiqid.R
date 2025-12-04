

# This file has the S3 generic 'as.Bwiqid' function and a series of methods.

as.Bwiqid <- function(object, ...) UseMethod("as.Bwiqid")

as.Bwiqid.default <- function(object, ...) {
  name <- deparse(substitute(object))
  header <- paste("MCMC values from object", sQuote(name))
  warning("Class 'Bwiqid' is deprecated, please use class 'mcmcOutput'.", call.=FALSE)
  message("Returning a class 'mcmcOutput' object.")
  mcmcOutput(object, header, ...)
}
# ...................................................................

