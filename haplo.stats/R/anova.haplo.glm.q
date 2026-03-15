
## Purpose: Anova function for haplo.glm object
##          borrow blueprint from anova.glm
##
## Authors: Jason Sinnwell and Dan Schaid, Mayo BSI
## Created: 9/28/2011
## Updated: 10/18/2011

anova.haplo.glm <- function(object, ..., dispersion=NULL, test="Chisq") {

  ## for more than one fitted object, allow anova.glmlist to compare,
  ## which will only work for nested haplo.glm callsworks because the defiances
  
  dotargs <- list(...)
  named <- if (is.null(names(dotargs))) 
    rep(FALSE, length(dotargs))
  else (names(dotargs) != "")
  if (any(named)) 
    warning("the following arguments to 'anova.haplo.glm' are invalid and dropped: ", 
            paste(deparse(dotargs[named]), collapse = ", "))
  dotargs <- dotargs[!named]
  is.glm <- unlist(lapply(dotargs, function(x) inherits(x, "glm")))
  dotargs <- dotargs[is.glm]
  if (length(dotargs)) {
    return(anova.haplo.glmlist(c(list(object), dotargs),
                               dispersion = dispersion,  test = test))
  } else {
    warning(paste(" This method only allows comparison between two or \n",
          "  more glm/haplo.glm model fits.\n"))
  }
}


anova.haplo.glmlist <- function(object, test="Chisq", ...) {

  ## Compare multiple glm/haplo.glm objects
  ## check that each object inherits glm or haplo.glm
  ## check the response names the same
  ## check that the rows, or effective rows, are the same for the fits

  if (!is.list(object)) 
    stop("First argument must be a list")

  ## check same responses
  responses <- as.character(unlist(lapply(object, function(x) deparse(formula(x)[[2]]))))
  sameresp <- (responses == responses[1])
  if (!all(sameresp)) {
    object <- object[sameresp]
    warning(paste("Models with response", deparse(responses[!sameresp]), 
                  "removed because response differs from", "model 1"))
  }

  nsubj <- sapply(object, FUN=function(x) {
    if(is.null(x$haplo.post.info)) sum(x$prior.weights)
    else sum(x$prior.weights*x$haplo.post.info$post) })
  
  if (any(nsubj != nsubj[1])) 
        stop("models were not all fitted to the same size of dataset")

  nmodels <- length(object)
  if (nmodels == 1) 
    return(anova(object[[1L]], dispersion = dispersion, test = test))
  

  #####
  resdf <- as.numeric(sapply(object, function(x) {
    if(is.null(x$haplo.post.info)) sum(x$prior.weights) - length(x$coeff)
    else sum(x$prior.weights * x$haplo.post.info$post) - length(x$coeff) } ))
                      
  ##as.numeric(lapply(object, function(x) x$df.residual))
  resdev <- as.numeric(lapply(object, function(x) x$deviance))
  table <- data.frame(resdf, resdev, c(NA, -diff(resdf)), c(NA, -diff(resdev)))
  variables <- lapply(object, function(x) paste(deparse(formula(x)), 
                                                collapse = "\n"))
  dimnames(table) <- list(1L:nmodels, c("Resid. Df", "Resid. Dev", 
                                        "Df", "Deviance"))
  title <- "Analysis of Deviance Table\n"
  topnote <- paste("Model ", format(1L:nmodels), ": ", variables, 
                   sep = "", collapse = "\n")
  if (!is.null(test)) {
    bigmodel <- object[[order(resdf)[1L]]]
    dispersion <- residScaledGlmFit(bigmodel)$a.phi
    df.dispersion <- if (dispersion == 1) Inf else min(resdf)
    
    table <- stat.anova(table = table, test = test, scale = dispersion, 
                        df.scale = df.dispersion, n = nsubj[1])
  }
  
  structure(table, heading = c(title, topnote),
            class = c("anova","data.frame"))
  
}
