
# Constructor methods to create mcmcArray objects

mcmcOutput <- function(object, ...) UseMethod("mcmcOutput")

mcmcOutput.default <- function(object, ...) {
    stop(paste("No applicable method for class", class(object)[1]))
}
# ...................................................................

mcmcOutput.array <- function(object, ...) {
    stop(paste("No applicable method for class", class(object)[1]))
}
# ...................................................................

# Class mcmcOutput, may seem odd but catches coercion
mcmcOutput.mcmcOutput <- function(object, ...) {
    return(object)
}
# ...................................................................

# Class coda::mcmc.list, used by rjags::coda.samples, jagsUI::jags.basic
mcmcOutput.mcmc.list <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- length(object)
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from mcmc.list object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "mcpar") <- attr(object[[1]], "mcpar")

  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# .......................................................................

# Class coda::mcmc for a single chain, rarely used
mcmcOutput.mcmc <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- 1
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from mcmc object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "mcpar") <- attr(object, "mcpar")

  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# .......................................................................

# Class jagsUI::jagsUI used by jagsUI::jags
# code updated 2020-05-17 to deal with both CRAN and GitHub versions
mcmcOutput.jagsUI <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- mcmcOutput(object$samples)
  if(missing(header))
    header <- paste("MCMC values from jagsUI object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$modfile
  runDate <- object$run.date
  if(is.null(runDate))  # new 'jagsUI' class format
    runDate <- object$run.info$end.time
  attr(mcMat, "runDate") <- runDate
  timeTaken <- object$mcmc.info$elapsed.mins
  if(is.null(timeTaken))  # new 'jagsUI' class format
    timeTaken <- difftime(object$run.info$end.time, object$run.info$start.time,
        units="mins")
  attr(mcMat, "timeTaken") <- c(unclass(timeTaken)) * 60  # convert to secs
  attr(mcMat, "adaptationOK") <- object$mcmc.info$sufficient.adapt
  return(mcMat)
}
# .......................................................................

# Class bugs from R2WinBUGS package and R2OpenBUGS
mcmcOutput.bugs <- function(object, header, ...) {
  name <- deparse(substitute(object))
  # Can't use sims.matrix as the chains are scrambled. Need to start from sims.array
  mcMat <- matrix(object$sims.array, ncol=dim(object$sims.array)[3])
  colnames(mcMat) <- dimnames(object$sims.array)[[3]]
  attr(mcMat, "nChains") <- dim(object$sims.array)[2]
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from bugs object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$model.file
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class rjags from R2jags package
mcmcOutput.rjags <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- object$BUGSoutput$sims.matrix
  attr(mcMat, "nChains") <- object$BUGSoutput$n.chains
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from rjags object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$model.file
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class runjags from runjags package
mcmcOutput.runjags <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- mcmcOutput(object$mcmc)
  # attr(mcMat, "nChains") <- length(object$samples)
  # attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from runjags object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "timeTaken") <- as.numeric(object$timetaken, units="secs")
  # class(mcMat) <- c("mcmcOutput", "matrix", "array") # not needed
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class numeric (but not a matrix or array)
mcmcOutput.numeric <- function(object, nChains=1, header, ...) {
  name <- deparse(substitute(object))
  nc <- attr(object, "nChains")
  if(!is.null(nc))
    nChains <- nc
  mcMat <- matrix(object, ncol=1)
  colnames(mcMat) <- name
  attr(mcMat, "nChains") <- nChains
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from numeric vector", sQuote(name))
  attr(mcMat, "header") <- header
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class matrix
mcmcOutput.matrix <- function(object, nChains=1, header, ...) {
  name <- deparse(substitute(object))
  nc <- attr(object, "nChains")
  if(!is.null(nc))
    nChains <- nc
  if(is.null(colnames(object)))  # mcmcObject must have column names
    colnames(object) <- paste0("V[", 1:ncol(object), "]")
  mcMat <- object
  attr(mcMat, "nChains") <- nChains
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from matrix", sQuote(name))
  attr(mcMat, "header") <- header
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class data frame
mcmcOutput.data.frame <- function(object, nChains=1, header, ...) {
  name <- deparse(substitute(object))
  nc <- attr(object, "nChains")
  if(!is.null(nc))
    nChains <- nc
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- nChains
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from data frame", sQuote(name))
  attr(mcMat, "header") <- header
  class(mcMat) <- c("mcmcOutput", "matrix", "array") # not needed
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


