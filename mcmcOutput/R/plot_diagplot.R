# Based closely on wiqid::diagPlot, now for mcmcOutput

# Function to do multiple trace and density plots
diagPlot <- function(object, params, howMany, chains,
  maxRows=4, RhatBad=1.05, precision=c("MCEpc", "n.eff"), ask=NULL, ...) {

  name <- deparse(substitute(object))
  precision <- match.arg(precision)
  object <- mcmcOutput(object)

  # Plotting parameters
  dots <- list(...)
  if(length(dots) == 1 && inherits(dots[[1]], "list"))
    dots <- dots[[1]]
  mainTitle <- dots$main # this goes in outer margin
  dots$main <- NULL
  if(is.null(mainTitle))
    mainTitle <- paste("Diagnostics for", name)
  defaultArgs <- list(xlab="", ylab="", type='l', lty=1)
  useArgsT <- useArgsD <- modifyList(defaultArgs, dots)
  selPlot <- names(useArgsT) %in%
    c(names(as.list(args(title))), names(par(no.readonly=TRUE)))
  titleArgs <- mainTitleArgs <- useArgsT[selPlot]
  mainTitleArgs$main <- mainTitle
  mainTitleArgs$line <- dots$line
  mainTitleArgs$adj <- dots$adj
  mainTitleArgs$col.main <- dots$col.main
  mainTitleArgs$outer <- TRUE

  # Conversion to 3d array
  niter <- nrow(object)
  nChains <- attr(object, "nChains")
  draws.per.chain <- niter / nChains
  npars <- ncol(object)
  parnames <-  colnames(object)
  if(is.null(parnames))  # should not happen!
    parnames <- paste0("V", 1:npars)
  mcmc3d <- array(object, dim=c(draws.per.chain, nChains, npars))
  dimnames(mcmc3d) <- list(NULL, 1:nChains, parnames)

  # Deal with subsetting
  if(!missing(params)) {
    params <- matchStart(params, parnames)
    if(length(params) == 0)
      stop("No columns match the specification in 'params'.", call.=FALSE)
    mcmc3d <- mcmc3d[, , params, drop=FALSE]
    parnames <- parnames[params]
    npars <- length(params)
  }
  if(!missing(howMany) && abs(howMany) < draws.per.chain) {  ##### this is wrong
    if(howMany > 0) {
      mcmc3d <- mcmc3d[1:howMany, , , drop=FALSE]
    } else {
      mcmc3d <- mcmc3d[(draws.per.chain+howMany+1):draws.per.chain, , , drop=FALSE]
    }
    draws.per.chain <- dim(mcmc3d)[1]
  }
  if(!missing(chains) && all(chains <= nChains)) {
    mcmc3d <- mcmc3d[, chains, , drop=FALSE]
    nChains <- length(chains)
    niter <- draws.per.chain * nChains
  }

  # Get Rhat and MCEpc or n.eff
  # Rhat : this is same as simplestRhat but with 3d array
  Rhat <- round(simpleRhat3d(mcmc3d), 3)
  # MCEpc
  if(precision=="MCEpc") {
    prec <- round(getMCEpc3d(mcmc3d), 2)
  } else {
    prec <- round(apply(mcmc3d, 3, safeNeff1))
  }

  # Do the plots
  # ------------
  old.par <- par(mar = c(2,2,2,0)+0.1, oma=c(1,1,1,1), "mfrow")
    on.exit(par(old.par))
  if(!is.null(mainTitle))
    par(oma=c(1,1,3,1))

  if(npars > maxRows) {
    if(is.null(ask))
      ask <- dev.interactive(orNone=TRUE)
    old.ask <- devAskNewPage(ask)
    on.exit(devAskNewPage(old.ask), add=TRUE)
  }
  nrows <- min(npars, maxRows)
  layout(matrix(1:(nrows*2), ncol=2, byrow=TRUE), widths=2:1)
  for(i in 1:npars) {
    redFlag <- !is.na(Rhat[i]) && Rhat[i] > RhatBad
    mat <- matrix(mcmc3d[, , i], ncol=nChains) # need 'matrix' if 1 chain
    if(any(is.na(mat))) {
      warning("The chain '", parnames[i], "' contains NAs and cannot be plotted.", call.=FALSE)
      next
    }
    # do trace plot
    useArgsT$ylab <- parnames[i]
    useArgsT$y <- mat
    do.call(matplot, useArgsT)
    abline(h=mean(mat))
    titleArgs$main <- paste0(parnames[i], ": Rhat = ", Rhat[i])
    titleArgs$line <- 0.3
    titleArgs$adj <- 1
    titleArgs$col.main <- 1 + redFlag
    titleArgs$outer <- FALSE
    do.call(title, titleArgs)
    if(redFlag)
      box(col=2, lwd=2)
    # do density plot
    densPlot0(mat, useArgsD)
    # titleArgs$main <- paste0("n.eff = ", n.eff[i])
    titleArgs$main <- paste0(precision, " = ", prec[i])
    titleArgs$adj <- 0
    do.call(title, titleArgs)
    if(redFlag)
      box(col=2, lwd=2)
    do.call(title, mainTitleArgs)
  }
}
# ..........................................................

