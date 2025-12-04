
# S3 methods for mcmcOutput objects

# Needs helper 'summarise' function to build the summary matrix

summary.mcmcOutput <- function(object, digits=3, median=TRUE, mode=FALSE,
    CRItype=c("hdi", "symmetrical", "none"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE,
    overlap0=FALSE, f=FALSE, verbose=TRUE,...)  {

  CRItype <- match.arg(CRItype)
  sumtab <- summarise(object, median=median, mode=mode, CRItype=CRItype,
    CRImass=CRImass, Rhat=Rhat, MCEpc = MCEpc, n.eff=n.eff, overlap0=overlap0,
    f=f, ...)

  if(verbose) {
    header <- attr(object, "header")
    if(!is.null(header))
      cat(header, "\n")
    nChains <- attr(object, "nChains")
    draws <- nrow(object) / nChains
    nNodes <- ncol(object)
    cat("The object has", nNodes, "nodes with", draws, "draws for each of",
        nChains, "chains.\n")
    if(!is.null(CRImass) && !is.na(CRImass) && CRItype != "none") {
      if(CRItype == "hdi") {
        tmp <- paste0(c("l", "u"), round(CRImass * 100), collapse=" and ")
        cat(paste0(tmp, " are the limits of a ", CRImass*100,
            "% Highest Density Credible Interval.\n"))
      } else {
        tail <- (1 - CRImass)/2 * 100
        tmp <- sprintf("p%04.1f and p%04.1f", tail, 100-tail)
        cat(paste0(tmp, " are the limits of a ", CRImass*100,
            "% Symmetrical Credible Interval.\n"))
      }
    }
    if(Rhat && !is.null(sumtab$Rhat)) {
      cat("Rhat is the estimated potential scale reduction factor:\n")
      R <- sumtab[, 'Rhat']
      t1 <- sum(R > 1.1, na.rm=TRUE)
      t2 <- sum(is.na(R))
      txt <- sprintf("\tlargest is %.2f", max(R, na.rm=TRUE))
      if(t1) {
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 1.10", t1, 100*t1/length(R)))
      } else {
        txt <- c(txt, "; NONE are greater than 1.10")
      }
      if(t2 > 0)
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(R)))
      txt <- c(txt, ".\n")
      cat(paste0(txt, collapse=""))
    }
    if(MCEpc && !is.null(sumtab$MCEpc)) {
      cat("MCEpc is the Monte Carlo standard error as a percentage of the posterior SD:\n")
      MCe <- sumtab[, 'MCEpc']
      t1 <- sum(MCe > 5, na.rm=TRUE)
      t2 <- sum(is.na(MCe))
      txt <- sprintf("\tlargest is %.1f%%", max(MCe, na.rm=TRUE))
      if(t1) {
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 5", t1, 100*t1/length(MCe)))
      } else {
        txt <- c(txt, "; NONE are greater than 5%")
      }
      if(t2 > 0)
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(MCe)))
      txt <- c(txt, ".\n")
      cat(paste0(txt, collapse=""))
    }
    if(n.eff && !is.null(sumtab$n.eff)) {
      cat("n.eff is the effective number of draws allowing for autocorrelation:\n")
      ne <- sumtab[, 'n.eff']

      ne[ne <= 1] <- NA
      t1 <- sum(ne < 1000, na.rm=TRUE)
      t2 <- sum(is.na(ne))
      txt <- sprintf("\tsmallest is %.0f", min(ne, na.rm=TRUE))
      if(t1) {
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are smaller than 1000", t1, 100*t1/length(ne)))
      } else {
        txt <- c(txt, "; NONE are smaller than 1000")
      }
      if(t2 > 0)
        txt <- c(txt, sprintf("; %.0f (%.0f%%) are 1 or NA", t2, 100*t2/length(ne)))
      txt <- c(txt, ".\n")
      cat(paste0(txt, collapse=""))
    }
    if(overlap0 && !is.null(sumtab$overlap0))
      cat("overlap0 is TRUE if 0 falls in the credible interval.\n")
    if(f && !is.null(sumtab$f))
      cat("f is the proportion of the posterior with the same sign as the mean.\n")

    cat("\n")
  } # end verbose

  out <- round(sumtab, digits)
  if(!is.null(out$overlap0))
    out$overlap0 <- out$overlap0 > 0 # convert to TRUE/FALSE
  attr(out, "nChains") <- attr(object, "nChains")
  attr(out, "simsList") <- attr(object, "simsList")
  return(out)
}
# .........................................................

