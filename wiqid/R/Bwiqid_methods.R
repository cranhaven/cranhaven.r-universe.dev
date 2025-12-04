# Print, summary, plot, window, head and tail methods for class Bwiqid, ie. MCMC output


print.Bwiqid <- function(x, digits=3, ...)  {
  if(!inherits(x, "data.frame"))
    stop("x is not a valid Bwiqid object")
  # call <- attr(x, "call")
  header <- attr(x, "header")
  n.chains <- attr(x, "n.chains")
  if(is.null(n.chains))
    n.chains <- 1
  Rhat <- attr(x, "Rhat")
  if(is.null(Rhat) & n.chains > 1)
    Rhat <- simpleRhat(x, n.chains)
  MCerror <- attr(x, "MCerror")
  if(is.null(MCerror))
    MCerror <- getMCerror(x, n.chains)
  # n.eff <- attr(x, "n.eff")
  timetaken <- attr(x, "timetaken")

  toPrint <- cbind(
    mean = colMeans(x),
    sd = apply(x, 2, sd),
    median = apply(x, 2, median),
    t(hdi(x)))
  colnames(toPrint)[4:5] <- c("HDIlo", "HDIup")
  if(!is.null(Rhat))
    toPrint <- cbind(toPrint, Rhat = Rhat)
  if(!is.null(MCerror))
    toPrint <- cbind(toPrint, 'MCE%' = round(100 * MCerror/toPrint[, 'sd'], 1))
  # if(!is.null(n.eff))
    # toPrint <- cbind(toPrint, n.eff = round(n.eff))

  toPrint0 <- unique(toPrint)

  # if(!is.null(call))
    # cat("Call:", call, "\n")
  if(is.null(header))
    header <- "MCMC fit results:"
  cat(header, "\n")
  cat(nrow(x), "draws saved.\n")
  if(nrow(toPrint0) < nrow(toPrint))
    cat("(Duplicate rows removed.)\n")
  print(toPrint0, digits = digits)
  cat("\n'HDIlo' and 'HDIup' are the limits of a 95% HDI credible interval.\n")
  if(!is.null(Rhat))
    cat("'Rhat' is the potential scale reduction factor (at convergence, Rhat=1).\n")
  if(!is.null(MCerror))
    cat("'MCE%' is the Monte Carlo error as a %age of the SD (should be less than 5%).\n")
  # if(!is.null(n.eff))
    # cat("'n.eff' is a crude measure of effective sample size.\n")
  if(!is.null(timetaken)) {
    took <- format(round(timetaken, 1))
    cat("MCMC chain generation:", took, "\n")
  }
}
# .........................................................


summary.Bwiqid <- function(object, digits=3, ...)  {
  if(!inherits(object, "data.frame"))
    stop("object is not a valid Bwiqid object")
  call <- attr(object, "call")
  header <- attr(object, "header")
  n.chains <- attr(object, "n.chains")
  if(is.null(n.chains))
    n.chains <- 1
  MCerror <- attr(object, "MCerror")
  if(is.null(MCerror))
    MCerror <-  getMCerror(object, n.chains)
  Rhat <- attr(object, "Rhat")
  if(is.null(Rhat) & n.chains > 1)
    Rhat <- simpleRhat(object, n.chains)
  # n.eff <- attr(object, "n.eff")
  timetaken <- attr(object, "timetaken")

  toPrint <- cbind(
    mean = colMeans(object),
    sd = apply(object, 2, sd),
    median = apply(object, 2, median),
    t(hdi(object)))
  colnames(toPrint)[4:5] <- c("HDIlo", "HDIup")

  if(!is.null(Rhat))
    toPrint <- cbind(toPrint, Rhat = Rhat)
  if(!is.null(MCerror))
    toPrint <- cbind(toPrint, 'MCE%' = round(100 * MCerror/toPrint[, 'sd'], 1))
  # if(!is.null(n.eff))
    # toPrint <- cbind(toPrint, n.eff = round(n.eff))

  if(is.null(header))
    header <- "MCMC fit results:"
  cat(header, "\n")
  if(is.null(n.chains)) {
    cat(nrow(object), "draws saved.\n")
  } else {
    cat(sprintf("%.0f chains x %.0f draws = %.0f total.\n",
        n.chains, nrow(object)/n.chains, nrow(object)))
  }
  if(!is.null(timetaken)) {
    took <- format(round(timetaken, 1))
    cat("MCMC chain generation:", took, "\n")
  }

  if(!is.null(Rhat)) {
    t1 <- sum(Rhat>1.1, na.rm=TRUE)
    t2 <- sum(is.na(Rhat))
    txt <- sprintf("\nRhat: largest is %.2f", max(Rhat, na.rm=TRUE))
    if(t1) {
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 1.10", t1, 100*t1/length(Rhat)))
    } else {
      txt <- c(txt, "; NONE are greater than 1.10")
    }
    if(t2 > 0)
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(Rhat)))
    txt <- c(txt, ".\n")
    cat(paste0(txt, collapse=""))
  }

  if(!is.null(MCerror)) {
    MCEpc <- round(100 * MCerror/apply(object, 2, sd), 1)
    t1 <- sum(MCEpc > 5, na.rm=TRUE)
    t2 <- sum(is.na(MCEpc))
    txt <- sprintf("\nMCerror (%% of SD): largest is %.1f%%", max(MCEpc, na.rm=TRUE))
    if(t1) {
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 5", t1, 100*t1/length(MCEpc)))
    } else {
      txt <- c(txt, "; NONE are greater than 5%")
    }
    if(t2 > 0)
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(MCEpc)))
    txt <- c(txt, ".\n")
    cat(paste0(txt, collapse=""))
  }

  # if(!is.null(n.eff)) {
    # n.eff[n.eff == 1] <- NA
    # t1 <- sum(n.eff < 1000, na.rm=TRUE)
    # t2 <- sum(is.na(n.eff))
    # txt <- sprintf("\nn.eff: smallest is %.0f", min(n.eff, na.rm=TRUE))
    # if(t1) {
      # txt <- c(txt, sprintf("; %.0f (%.0f%%) are smaller than 1000", t1, 100*t1/length(n.eff)))
    # } else {
      # txt <- c(txt, "; NONE are smaller than 1000")
    # }
    # if(t2 > 0)
      # txt <- c(txt, sprintf("; %.0f (%.0f%%) are 1 or NA", t2, 100*t2/length(Rhat)))
    # txt <- c(txt, ".\n")
    # cat(paste0(txt, collapse=""))
  # }
  cat("\n")
  return(invisible(round(toPrint, digits=digits)))
}
# .........................................................

plot.Bwiqid <-
function(x, which=NULL, credMass=0.95,
          ROPE=NULL, compVal=NULL, showCurve=FALSE,  showMode=FALSE,
          shadeHDI=NULL, ...) {
  # This function plots the posterior distribution for one selected item.
  # Description of arguments:
  # x is mcmc.list object of the type returned by B* functions in 'wiqid'.
  # which indicates which item should be displayed; if NULL, looks for a 'toPlot'
  #   attribute in x; if missing does first column.
  # ROPE is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE.
  # compVal is a scalar specifying the value for comparison.
  # showCurve if TRUE the posterior should be displayed as a fitted density curve
  #   instead of a histogram (default).

  # TODO additional sanity checks.
  # Sanity checks:
  if(!inherits(x, "data.frame"))
    stop("x is not a valid Bwiqid object")

  # Deal with ... argument
  dots <- list(...)
  # if(length(dots) == 1 && class(dots[[1]]) == "list")
  if(length(dots) == 1 && inherits(dots[[1]], "list"))  # Fixed 2022-06-06
    dots <- dots[[1]]

  if(is.null(which)) # && !is.null(attr(x, "defaultPlot")))
      which <- attr(x, "defaultPlot")
  if(is.null(which))
    which <- colnames(x)[1]
  if(!is.character(which))
    stop("'which' must be an object of class 'character'.") # Added 2017-09-27
  if(is.na(match(which, colnames(x))))
    stop(paste("Could not find", which, "in the output"))
  if(is.null(dots$xlab))
    dots$xlab <- which
  # Plot posterior distribution of selected item:
  out <- plotPost(x[[which]], credMass=credMass, ROPE=ROPE, compVal=compVal,
                  showCurve=showCurve, showMode=showMode, shadeHDI=shadeHDI,
                  graphicPars=dots)

  return(invisible(out))
}

# .........................................................


head.Bwiqid <- function(x, n=6L, ...) {
  head(as.data.frame(x), n=n, ...)
}

tail.Bwiqid <- function(x, n=6L, ...) {
  tail(as.data.frame(x), n=n, ...)
}


