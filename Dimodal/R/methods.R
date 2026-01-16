
# methods.R:
# S3 class methods for the data structures built in this library.
#
# c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - Marking voting cpt libraries somehow on plot.
#   


### Dimodal objects.

# Print the contents of Dimodal object x, depending on which analyses have
# been done and the feature(s) selected.
print.Dimodal <- function(x, feature=c("peaks", "flats", "cpt"), ...) {

  feature <- match.arg(feature, several.ok=TRUE)

  cat("\n")
  
  if ("data" %in% names(x)) {
    cat("Setup\n")
    print(x$data)
  }

  if (("peaks" %in% feature) && ("lp.peaks" %in% names(x))) {
    cat("Peaks in Low-Pass Spacing\n")
    cat("feature positions at middle of filter\n")
    print(x$lp.peaks)
  }
  if (("flats" %in% feature) && ("lp.flats" %in% names(x))) {
    cat("Flats in Low-Pass Spacing\n")
    cat("feature positions at middle of filter\n")
    print(x$lp.flats)
  }

  if (("peaks" %in% feature) && ("diw.peaks" %in% names(x))) {
    cat("Peaks in Interval Spacing\n")
    cat("feature positions at end of interval\n")
    print(x$diw.peaks)
  }
  if (("flats" %in% feature) && ("diw.flats" %in% names(x))) {
    cat("Flats in Interval Spacing\n")
    cat("feature positions at end of interval\n")
    print(x$diw.flats)
  }

  if (("cpt" %in% feature) && ("cpt" %in% names(x))) {
    cat("Changepoints in Spacing\n")
    cat("positions in spacing\n")
    print(x$cpt)
  }

  invisible(x)
}

# Summarize the contents of Dimodal object, depending on which analyses have
# been done and the feature(s) selected.
summary.Dimodal <- function(object, feature=c("peaks", "flats", "cpt"), ...) {

  feature <- match.arg(feature, several.ok=TRUE)

  x <- object

  summary(x$data)

  if (("peaks" %in% feature) && ("lp.peaks" %in% names(x))) {
    cat("Peaks in Low-Pass Spacing\n")
    cat("feature positions at middle of filter\n")
    summary(x$lp.peaks)
  }
  if (("flats" %in% feature) && ("lp.flats" %in% names(x))) {
    cat("Flats in Low-Pass Spacing\n")
    cat("feature positions at middle of filter\n")
    summary(x$lp.flats)
  }

  if (("peaks" %in% feature) && ("diw.peaks" %in% names(x))) {
    cat("Peaks in Interval Spacing\n")
    cat("feature positions at end of interval\n")
    summary(x$diw.peaks)
  }
  if (("flats" %in% feature) && ("diw.flats" %in% names(x))) {
    cat("Flats in Interval Spacing\n")
    cat("feature positions at end of interval\n")
    summary(x$diw.flats)
  }

  if (("cpt" %in% feature) && ("cpt" %in% names(x))) {
    cat("Changepoints in Spacing\n")
    cat("positions in spacing\n")
    summary(x$cpt)
  }

  invisible(object)
}

# Return a dummy object of class Dimodal, with mockups for all elements.
mockup.Dimodal <- function() {

  res <- list(data=mockup.Didata(), opt=NULL, cpt=mockup.Dicpt(),
              lp.peaks=mockup.Dipeak(), lp.flats=mockup.Diflat(),
              diw.peaks=mockup.Dipeak(), diw.flats=mockup.Diflat())
  class(res) <- "Dimodal"
  res
}


# Create a graph of Dimodal object x, plotting the low-pass spacing, a
# histogram of the data, and/or interval spacing per show.  Mark feature(s),
# which if 'none' or NULL or NA adds no annotations.  The features to mark in
# the histogram come from the first shown (ie. leftmost) analysis, lp or diw.
# Diopt options opt provides significance levels.  Uses layout to order the
# plots.
# Returns x invisibly.
plot.Dimodal <- function(x, show=c("lp", "histogram", "diw"),
                         feature=c("peaks", "flats", "cpt"), opt=Diopt(), ...) {

  show <- unique(tolower(show))
  show <- match.arg(show, several.ok=TRUE)
  i <- match("lp", show)
  if (((1 < length(i)) || !is.na(i)) && !("lp" %in% rownames(x$data))) {
    # If we have only generated changepoints, then plot spacing i.p.v. LP.
    if (!("Diw" %in% rownames(x$data)) &&
        ("cpt" %in% feature) && ("cpt" %in% names(x))) {
      show[i] <- "cpt"
    } else {
      show <- show[-i]
    }
  }
  i <- match("diw", show)
  if (((1 < length(i)) || !is.na(i)) && !("Diw" %in% rownames(x$data))) {
    show <- show[-i]
  }
  if (0 == length(show)) {
    stop("no data to show exists in Dimodal result")
  }

  feature <- tolower(feature)
  if ((0 == length(feature)) ||
      ((1 == length(feature)) && (is.na(feature) || ("none" == feature)))) {
    feature <- NULL
  } else {
    feature <- match.arg(feature, several.ok=TRUE)
    if (("lp" %in% show) && ("diw" %in% show)) {
      feat.src <- show[min(match(c("lp", "diw"), show))]      
    } else if ("lp" %in% show) {
      feat.src <- "lp"
    } else if ("diw" %in% show) {
      feat.src <- "diw"
    } else {
      feat.src <- "none"
    }
  }
  
  if ("Diopt" != class(opt)) {
    opt <- Diopt(opt)
  }

  layout(matrix(1:length(show), nrow=1))
  # Layout for a 3 grid forces cex to 0.66.  Force that even if fewer plots.
  oldpar <- par(cex=0.66)
  on.exit(par(oldpar))

  add.decile <- "histogram" %in% show
  for (i in seq_along(show)) {
    if ("histogram" == show[i]) {
      h <- plot_Dimodal_hist(x, opt, feat.src)
      if ("peaks" %in% feature) {
        if (("lp" == feat.src) && ("lp.peaks" %in% names(x))) {
          mark.Dipeak(x$lp.peaks, h, opt)
        } else if (("diw" == feat.src) && ("diw.peaks" %in% names(x))) {
          mark.Dipeak(x$diw.peaks, h, opt)
        }
      }
      if ("flats" %in% feature) {
        if (("lp" == feat.src) && ("lp.flats" %in% names(x))) {
          mark.Diflat(x$lp.flats, h, opt)
        } else if (("diw" == feat.src) && ("diw.flats" %in% names(x))) {
          mark.Diflat(x$diw.flats, h, opt)
        }
      }
      if (("cpt" %in% feature) && ("cpt" %in% names(x))) {
        mark.Dicpt(x$cpt, h, opt)
      }
    }
  
    if ("lp" == show[i]) {
      plot_Dimodal_lp(x, opt, add.decile)
      if (("peaks" %in% feature) && ("lp.peaks" %in% names(x))) {
        mark.Dipeak(x$lp.peaks, NULL, opt)
      }
      if (("flats" %in% feature) && ("lp.flats" %in% names(x))) {
        mark.Diflat(x$lp.flats, NULL, opt)
      }
      if (("cpt" %in% feature) && ("cpt" %in% names(x))) {
        mark.Dicpt(x$cpt, NULL, opt)
      }
    }
  
    if ("diw" == show[i]) {
      plot_Dimodal_diw(x, opt, add.decile)
      if (("peaks" %in% feature) && ("diw.peaks" %in% names(x))) {
        mark.Dipeak(x$diw.peaks, NULL, opt)
      }
      if (("flats" %in% feature) && ("diw.flats" %in% names(x))) {
        mark.Diflat(x$diw.flats, NULL, opt)
      }
      if (("cpt" %in% feature) && ("cpt" %in% names(x))) {
        mark.Dicpt(x$cpt, NULL, opt)
      }
    }

    if ("cpt" == show[i]) {
      plot_Dimodal_cpt(x, opt, add.decile)
      mark.Dicpt(x$cpt, NULL, opt)
    }
  }

  invisible(x)
}

# Plot the histogram of the raw data in Dimodal class object x.  Use graphics
# options in Diopt opt.  Plot a CDF atop, with axes, and jittered data below.
# The quantile axis label comes from feat.src, either 'lp', 'diw', or 'none'.
# Returns the histogram.
plot_Dimodal_hist <- function(x, opt, feat.src) {

  qs <- quantile(x$data["x",], c(0.05, 0.5, 0.95), names=FALSE)
  dmin <- qs[2] + (3 * (qs[1] - qs[2]))
  dmax <- qs[2] + (3 * (qs[3] - qs[2]))
  dhist <- x$data["x",(dmin<=x$data["x",]) & (x$data["x",]<=dmax)]
  # Want at least 5 points per bucket, on average.
  nbreaks <- length(dhist) %/% 5

  clr <- get.colors(opt$palette)
  hclr <- adjustcolor(clr[opt$colID.hist], 1, 0.9, 0.9, 0.9)
  hbdr <- adjustcolor(hclr, 1, 1.15, 1.15, 1.15)

  h <- hist(dhist, breaks=nbreaks, col=hclr, border=hbdr,
            xlab="data", ylab="count", main="Raw Data")
  # Draw the cumulative count and an axis.
  hcum <- max(h$counts) * cumsum(h$counts) / sum(h$counts)
  # ecdf aligns to end of breaks, not to mids.
  lines(h$breaks[-1L], hcum, col=clr[opt$colID.cdf], lwd=2)
  axis(4, line=NA, at=max(h$counts)*seq(0,1,by=0.1), labels=FALSE,
       col=clr[opt$colID.cdf])
  feat.src <- switch(feat.src, lp="LP   ", diw="Diw   ", none="none")
  if ("none" != feat.src) {
    mtext(feat.src, 4, at=0, adj=1, las=0, cex=par("cex")*par("cex.axis"))
  }
  rug(jitter(dhist))

  dhist <- sort(dhist)
  # Something big that will be off the graph to the right.
  xr <- 2 * dhist[length(dhist)]

  invisible(h)
}

# Plot the raw data and low-pass spacing in Dimodal class object x.  Use
# graphic options in Diopt opt.  If add.decile is TRUE draw the decile axis
# underneath.
plot_Dimodal_lp <- function(x, opt, add.decile) {

  raw <- x$data["Di",-1L]
  stID <- attr(x$data, "lp.stID")
  endID <- attr(x$data, "lp.endID")
  lp <- x$data["lp",stID:endID]

  clr <- get.colors(opt$palette)
  clr[opt$colID.data] <- adjustcolor(clr[opt$colID.data], 1, 0.9, 0.9, 0.9)
  
  ymax <- max(1.1*max(lp), quantile(raw, 0.98, names=FALSE))
  plot(0, 0, col=NA, xlim=c(0,ncol(x$data)), ylim=c(0,ymax),
       xlab="index", ylab="spacing", main="Low-Pass Spacing")
  points(2:ncol(x$data), raw, col=clr[opt$colID.data], pch=20,
         cex=ifelse(500 < ncol(x$data), 0.1, 0.5))

  raw[raw < (1.04 * ymax)] <- NA
  raw[!is.na(raw)] <- 1.03 * ymax
  points(raw, col=clr[opt$colID.data], pch=4)

  lines(stID:endID, lp, col=clr[opt$colID.filter], lwd=2, lty=1)
  if (add.decile) {
    axis(1, line=4, at=ncol(x$data)*seq(0,1,by=0.1), labels=FALSE,
         col=clr[opt$colID.cdf])
  }
}

# Plot the raw data and interval spacing in Dimodal class object x.  Use
# graphic options in Diopt opt.  If add.decile is TRUE draw the decile axis
# underneath.
plot_Dimodal_diw <- function(x, opt, add.decile) {

  raw <- x$data["Di",-1L]
  stID <- attr(x$data, "diw.stID")
  endID <- ncol(x$data)
  diw <- x$data["Diw",stID:endID]

  clr <- get.colors(opt$palette)
  clr[opt$colID.data] <- adjustcolor(clr[opt$colID.data], 1, 0.9, 0.9, 0.9)

  ylbl <- sprintf("spacing, interval = %.0f", attr(x$data,"wdiw"))

  # For large intervals the spacing will be too small to see differences.
  # Use the range of the interval spacing to increase the spacing for plotting.
  rscl <- max(1, min(attr(x$data,"wdiw")/2, floor(max(diw) / max(1,min(diw)))))
  raw <- raw * rscl
  ymax <- 1.1 * max(diw)

  plot(0,0, col=NA, xlim=c(0,ncol(x$data)), ylim=c(0,ymax),
       xlab="index", ylab=ylbl, main="Interval Spacing")
  points(2:ncol(x$data), raw, col=clr[opt$colID.data], pch=20,
         cex=ifelse(500 < ncol(x$data), 0.1, 0.5))
  if (1.1 < rscl) {
    mtext(sprintf("spacing (dots) scaled by %.0f", rscl), 4, 0.5,
          cex=par("cex"))
  }

  raw[raw < (1.04 * ymax)] <- NA
  raw[!is.na(raw)] <- 1.03 * ymax
  points(raw, col=clr[opt$colID.data], pch=4)

  lines(stID:endID, diw, col=clr[opt$colID.filter], lwd=2, lty=1)
  if (add.decile) {
    axis(1, line=4, at=ncol(x$data)*seq(0,1,by=0.1),
         labels=FALSE, col=clr[opt$colID.cdf])
  }
}

# Plot the raw data only in Dimodal class object x.  Use graphic options in
# Diopt opt.  If add.decile is TRUE draw the decile axis underneath.
plot_Dimodal_cpt <- function(x, opt, add.decile) {

  raw <- x$data["Di",-1L]
  
  clr <- get.colors(opt$palette)
  clr[opt$colID.data] <- adjustcolor(clr[opt$colID.data], 1, 0.75, 0.75, 0.75)
  
  ymax <- max(quantile(raw, 0.98, names=FALSE))
  plot(0, 0, col=NA, xlim=c(0,ncol(x$data)), ylim=c(0,ymax),
       xlab="index", ylab="spacing", main="Changepoints")
  points(2:ncol(x$data), raw, col=clr[opt$colID.data], pch=20, cex=0.5)

  raw[raw < (1.04 * ymax)] <- NA
  raw[!is.na(raw)] <- 1.03 * ymax
  points(raw, col=clr[opt$colID.data], pch=4)

  if (add.decile) {
    axis(1, line=4, at=ncol(x$data)*seq(0,1,by=0.1), labels=FALSE,
         col=clr[opt$colID.cdf])
  }
}

# Return the colors for palette pal, using HCL colors if it has prefix 'hcl:'.
get.colors <- function(pal) {

  if ("hcl:" == substr(pal, 1, 4)) {
    hcl.colors(8, substring(pal, 5))
  } else {
    palette.colors(8, pal)
  }
}


### Ditest objects.

# Pretty-print the Ditest class object x.  Other arguments are ignored.
# Returns x invisibly.
print.Ditest <- function(x, ...) {

  arglist <- c(x$statname, names(x$parameter))
  is.excur <- !is.null(x$xbase) && !is.null(x$base)
  is.markov <- !is.null(x$tmat) && !is.null(x$wt)
  if (is.markov) {
    arglist <- c(arglist, "tmat", "wt")
  }

  pnames <- NULL
  fixarg <- NULL
  if (0 == length(x$parameter)) {
    p <- cbind(x$statistic, x$p.value)
  } else {
    p <- matrix(x$statistic, ncol=1)
    for (i in seq_along(x$parameter)) {
      if (nrow(p) == length(x$parameter[[i]])) {
        p <- cbind(p, x$parameter[[i]])
        pnames <- c(pnames, names(x$parameter)[i])
      } else {
        fixarg <- c(fixarg,
                    paste0(names(x$parameter)[[i]], "=", format(x$parameter[[i]])))
      }
    }
    p <- cbind(p, x$p.value)
  }
  colnames(p) <- c(x$statname, pnames, "p.value")
  # The result may contain invalid entries.  Do not print those.
  p <- p[is.finite(p[,1]) & is.finite(p[,ncol(p)]), ,drop=FALSE]

  cat("\n")
  cat(strwrap(x$method, prefix="  "), sep="\n")
  cat("  evaluated with ", x$statfn, "(",
      paste0(arglist, collapse=", "), ")\n", sep="")
  # This prevents R from printing [1,] before the row.
  rownames(p) <- rep("  ", nrow(p))
  print(p, print.gap=3, max=ncol(p)*nrow(p))
  if (!is.null(fixarg)) {
    cat("    with ", paste0(fixarg, collapse=", "), "\n", sep="")
  }
  if (!is.null(x$alternative)) {
    cat("    for features ", x$alternative, " than or equal to ",
        x$statname, "\n", sep="")
  }
  if (is.markov) {
    cat("  Markov chain variables tmat, wt stored with result\n")
  }
  if (is.excur) {
    if ("signed" == x$base) {
      cat("  signed difference basis for draws stored with result\n")
    } else {
      cat("  raw data basis for heights stored with result\n")
    }
  }
  if (!is.null(x$model)) {
    params <- sapply(seq_along(x$model),
                     function(i) {
                       paste0(names(x$model)[i], "=", format(x$model[i])) })
    cat("  model parameters ", paste0(params, collapse=", "), "\n", sep="")
  }
  cat("\n")

  invisible(x)
}

# Print the test statistic and p values for Ditest class object.
# Other arguments are ignored.  Returns object invisibly.
summary.Ditest <- function(object, ...) {

  p <- cbind(object$statistic, object$p.value)
  colnames(p) <- c(object$statname, "p.value")
  rownames(p) <- rep("  ", nrow(p))
  cat("\n  ", object$method, "\n", sep="")
  print(t(p))
  cat("\n")

  invisible(object)
}


### Diopt objects.

# Pretty-print Diopt class object x.  Returns x invisibly.
print.Diopt <- function(x, ...) {

  wtag <- max(nchar(sub("(.+?)\\.", "", names(diopt.dflt)))) + 2
  wval <- wtag + 10

  cat("\nSpacing to Analyze:\n")
  cat(sprintf("  %s\n", printopt(x, "analysis", wtag)))
  cat("\nGeneral Data Preparation (data.*):\n")
  cat(sprintf("  %-*s\n", wval, printopt(x, "data.midq", wtag)))
  cat("Low-Pass Filter Options (lp.*):\n")
  cat(sprintf("  %-*s    %-*s\n",
              wval, printopt(x, "lp.kernel", wtag),
              wval, printopt(x, "lp.window", wtag)))
  cat(sprintf("  %s\n", printopt(x, "lp.tests", wtag)))
  cat(sprintf("  %s\n", printopt(x, "lp.param", wtag)))
  
  cat("Interval Spacing Options (diw.*):\n")
  cat(sprintf("  %-*s\n", wval, printopt(x, "diw.window", wtag)))
  cat(sprintf("  %s\n", printopt(x, "diw.tests", wtag)))
  cat(sprintf("  %s\n", printopt(x, "diw.param", wtag)))
  
  cat("Peak Detector Options (peak.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "peak.fht", wtag),
              wval, printopt(x, "peak.frelht", wtag),
              wval, printopt(x, "peak.fhtie", wtag)))
  cat(sprintf("  %-*s\n",
              wval, printopt(x, "peak.fhsupp", wtag)))
  
  cat("Flat Detector Options (flat.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "flat.fripple", wtag),
              wval, printopt(x, "flat.minlen", wtag),
              wval, printopt(x, "flat.fminlen", wtag)))
  cat(sprintf("  %-*s    %-*s\n",
              wval, printopt(x, "flat.noutlier", wtag),
              wval, printopt(x, "flat.distrib", wtag)))
  
  cat("Excursion Test Options (excur.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "excur.nrep", wtag),
              wval, printopt(x, "excur.ntop", wtag),
              wval, printopt(x, "excur.seed", wtag) ))
  
  cat("Permutation (runht) Test Options (perm.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "perm.nrep", wtag),
              wval, "",
              wval, printopt(x, "perm.seed", wtag) ))
  
  cat("Significance Thresholds (alpha.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "alpha.ht", wtag),
              wval, printopt(x, "alpha.pkexcur.lp", wtag),
              wval, printopt(x, "alpha.pkexcur.diw", wtag)))
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "alpha.len", wtag),
              wval, printopt(x, "alpha.ftexcur.lp", wtag),
              wval, printopt(x, "alpha.ftexcur.diw", wtag)))
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "alpha.runht", wtag),
              wval, printopt(x, "alpha.nrun", wtag),
              wval, printopt(x, "alpha.runlen", wtag)))
  cat(sprintf("  %-*s\n",
              wval, printopt(x, "mark.alpha", wtag)))
  
  cat("Changepoint Detector Options (cpt.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "cpt.fncpt.max", wtag),
              wval, printopt(x, "cpt.qvote", wtag),
              wval, printopt(x, "cpt.timeout", wtag)))
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "cpt.fsep", wtag),
              wval, printopt(x, "cpt.sep", wtag),
              wval, printopt(x, "cpt.libsep", wtag)))
  cat(sprintf("  %-*s\n", wval, printopt(x, "cpt.libs", wtag)))

  cat("Tracking Options (track.*):\n")
  cat(sprintf("  %-*s\n",
              wval, printopt(x, "track.maxwindow", wtag)))

  cat("Display Options:\n")
  cat(sprintf("  %-*s    %-*s\n",
              wval, printopt(x, "palette", wtag),
              wval, printopt(x, "digits", wtag)))
  cat("Display Options - Color (colID.*):\n")
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "colID.peak", wtag),
              wval, printopt(x, "colID.flat", wtag),
              wval, printopt(x, "colID.cpt", wtag)))
  cat(sprintf("  %-*s    %-*s    %-*s\n",
              wval, printopt(x, "colID.data", wtag),
              wval, printopt(x, "colID.filter", wtag),
              wval, printopt(x, "colID.hist", wtag)))
  cat(sprintf("  %-*s\n",
              wval, printopt(x, "colID.cdf", wtag)))
  
  cat("\n")

  invisible(x)
}

# Create a string from Diopt class x element tag, of format <tag>  <value>
# where tag is forced to a width of wtag.
printopt <- function(x, tag, wtag) {

  shorttag <- sub("(.+?)\\.", "", tag)
  if (("lp.tests" == tag) || ("diw.tests" == tag) || "cpt.libs" == tag) {
    paste0(sprintf("%-*s", wtag, shorttag), paste(x[[tag]], collapse=", "))
  } else if (("lp.param" == tag) || ("diw.param" == tag)) {
    if (is.null(x[[tag]])) {
      paste(sprintf("%-*s", wtag, shorttag), "-none-", sep="")
    } else {
      lval <- sapply(seq_along(x[[tag]]),
                     function(i) {
                       paste0(names(x[[tag]])[i], "=", x[[tag]][i], sep="")
                     })
      paste0(sprintf("%-*s", wtag, shorttag), paste(lval, collapse=", "))
    }
  } else if ("cpt.qvote" == tag) {
    paste0(sprintf("%-*s", wtag, shorttag),
           "(", paste(x[[tag]], collapse=", "), ")")
  } else if (1 == length(x[[tag]])) {
    paste(sprintf("%-*s", wtag, shorttag), x[[tag]], sep="")
  } else {
    paste0(sprintf("%-*s", wtag, shorttag), paste(x[[tag]], collapse=", "))
  }
}


### Dipeak objects.

# Pretty-print the Dipeak class object x.  Other arguments are ignored.
# Returns x invisibly.
print.Dipeak <- function(x, ...) {

  if (0 == nrow(x)) {
    cat("\n  no extrema found\n\n")
    return(invisible(x))
  }

  pkID <- which(!is.na(x$lminID) & !is.na(x$rminID))
  if (0 == length(pkID)) {
    cat("\n  no maxima found\n\n")
    return(invisible(x))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  hcol <- c("ht", "hexcur", "nrun", "runlen", "runht", "", "")
  pcol <- c("pht", "pexcur", "pnrun", "prunlen", "prunht", "ppeak", "pass")
  
  # This is to cleanly print the find.peaks example, where the raw position
  # and probabilities are not included.
  show.raw <- "x" %in% colnames(x)
  show.stats <- any(pcol %in% colnames(x))
  show.support <- any(((x$lminID != x$lsuppID) | (x$rminID != x$rsuppID))[pkID])
  
  spos <- format(x$pos[pkID])
  wpos <- max(nchar(spos))
  sx   <- format(x$x[pkID], digits=dgt)
  wx <- max(nchar(sx))
  slm  <- format(x$lminID[pkID])
  sxlm <- format(x$x[pkID-1], digits=dgt)
  srm  <- format(x$rminID[pkID])
  wmin <- max(nchar(slm), nchar(srm))
  sxrm <- format(x$x[pkID+1], digits=dgt)
  wxmin <- max(nchar(sxlm), nchar(sxrm))
  if (show.support) {
    sls  <- format(x$lsupp[pkID])    
    srs  <- format(x$rsupp[pkID])    ; wsupp <- max(nchar(sls), nchar(srs))
  } else {
    wsupp <- 0
  }

  cat("\n  location of", ifelse(1==length(pkID), "maximum", "maxima"), "\n")

  if (show.raw) {
    minhdr <- "minima pos (raw)"
  } else {
    minhdr <- "minima pos"
  }
  supphdr <- "support pos"

  if (show.raw) {
    wmintop <- 2 * (wmin + wxmin) + 9
  } else {
    wmintop <- 2 * wmin + 3
  }
  wmingap <- ceiling(max(0, nchar(minhdr) - wmintop) / 2)
  if (show.support) {
    wsupptop <- (2 * wsupp) + 3
    wsuppgap <- ceiling(max(0, nchar(supphdr) - wsupptop) / 2)
    if (show.raw) {
      posraw <- sprintf("%-*s    %*s", wpos, "pos", wx, "raw")
    } else {
      posraw <- sprintf("%-*s", wpos, "pos")
    }
    cat(sprintf("  %s     %*s    %*s\n",
                posraw, wmintop, format(minhdr, justify="c", width=wmintop),
                wsupptop, format(supphdr, justify="c", width=wsupptop)))
    for (i in seq_along(pkID)) {
      if (show.raw) {
        posraw <- sprintf("%*s   (%*s)", wpos, spos[i], wx, sx[i])
        minraw <- sprintf("%*s - %*s (%*s - %*s)", wmin, slm[i], wmin, srm[i],
                          wxmin, sxlm[i], wxmin, sxrm[i])
      } else {
        posraw <- sprintf("%*s", wpos, spos[i], wx, "")
        minraw <- sprintf("%*s - %*s", wmin, slm[i], wmin, srm[i])
      }
      cat(sprintf("  %s    %*s%s    %*s%*s - %*s\n",
                  posraw, wmingap, "", minraw,
                  wsuppgap, "", wsupp, sls[i], wsupp, srs[i]))
    }
  } else {
    if (show.raw) {
      posraw <- sprintf("%*s    %-*s", wpos, "pos", wx, "raw")
    } else {
      posraw <- sprintf("%*s    %-*s", wpos, "pos", wx, "")
    }
    cat(sprintf("  %s     %*s\n",
                posraw, wmintop, format(minhdr, justify="c", width=wmintop)))
    for (i in seq_along(pkID)) {
      if (show.raw) {
        posraw <- sprintf("%*s   (%*s)", wpos, spos[i], wx, sx[i])
        minraw <- sprintf("%*s - %*s (%*s - %*s)", wmin, slm[i], wmin, srm[i],
                  wxmin, sxlm[i], wxmin, sxrm[i])
      } else {
        posraw <- sprintf("%*s    %*s ", wpos, spos[i], wx, "")
        minraw <- sprintf("%*s - %*s", wmin, slm[i], wmin, srm[i])
      }
      cat(sprintf("  %s    %s\n", posraw, minraw))
    }
  }

  cstat <- c("pos", hcol)
  cpval <-  c("pos", pcol)
  sstat <- matrix("", nrow=length(pkID), ncol=length(cpval))
  spval <- matrix("", nrow=length(pkID), ncol=length(cpval))
  sok <- rep("", length(cpval))
  keep <- rep(FALSE, length(cpval))

  if (show.stats) {
    sstat[,1] <- format(x$pos[pkID])
    spval[,1] <- format(x$pos[pkID])
    keep[1] <- TRUE
    if ("pht" %in% colnames(x)) {
      sstat[,2] <- format(pmax(x$lht[pkID], x$rht[pkID]), digits=dgt)
      spval[,2] <- mark.p(x$pht[pkID], opt$alpha.ht)
      sok[2] <- mark.p(opt$alpha.ht, NA)
      keep[2] <- TRUE
    }
    if ("pexcur" %in% colnames(x)) {
      sstat[,3] <- format(x$hexcur[pkID], digits=dgt)
      if ("Diw" == attr(x, "source")) {
        spval[,3] <- mark.p(x$pexcur[pkID], opt$alpha.pkexcur.diw)
        sok[3] <- mark.p(opt$alpha.pkexcur.diw, NA)
      } else {
        spval[,3] <- mark.p(x$pexcur[pkID], opt$alpha.pkexcur.lp)
        sok[3] <- mark.p(opt$alpha.pkexcur.lp, -1)
      }
      keep[3] <- TRUE
    }
    if ("pnrun" %in% colnames(x)) {
      sstat[,4] <- format(x$nrun[pkID])
      spval[,4] <- mark.p(x$pnrun[pkID], opt$alpha.nrun)
      sok[4] <- mark.p(opt$alpha.nrun, NA)
      keep[4] <- TRUE
    }
    if ("prunlen" %in% colnames(x)) {
      sstat[,5] <- format(x$runlen[pkID])
      spval[,5] <- mark.p(x$prunlen[pkID], opt$alpha.runlen)
      sok[5] <- mark.p(opt$alpha.runlen, NA)
      keep[5] <- TRUE
    }
    if ("prunht" %in% colnames(x)) {
      sstat[,6] <- format(x$runht[pkID])
      spval[,6] <- mark.p(x$prunht[pkID], opt$alpha.runht)
      sok[6] <- mark.p(opt$alpha.runht, NA)
      keep[6] <- TRUE
    }
    if (any(!is.na(x$ppeak[pkID]))) {
      spval[,7] <- mark.p(x$ppeak[pkID], NA)
      sel <- 0 < x$naccept[pkID]
      spval[sel,8] <- sprintf("T (%.0f)", x$naccept[pkID])[sel]
      keep[c(7,8)] <- TRUE
    }

    if (any(keep)) {
      sstat <- sstat[,keep, drop=FALSE]
      spval <- spval[,keep, drop=FALSE]
      cstat <- cstat[keep]
      cpval <- cpval[keep]
      sok <- sok[keep]

      w <- apply(nchar_marked(rbind(sstat, spval, cstat, cpval)), 2, max)
      # Extra space after position, before total probability, pass columns
      w[1] <- w[1] + 2
      if (keep[7]) {
        w[length(w)-2] <- w[length(w)-2] + 2
        w[length(w)-1] <- w[length(w)-1] + 1
      }
      for (i in 1:ncol(sstat)) {
        sstat[,i] <- format(sstat[,i], width=w[i])
        spval[,i] <- pad.marked(spval[,i], width=w[i])
        cstat[i] <- format(cstat[i], width=w[i])
        cpval[i] <- format(cpval[i], width=w[i])
        sok[i] <- format(sok[i], width=w[i])
      }

      cat("\n  statistics\n")
      cat("  ", paste0(cstat, sep="  "), "\n", sep="")
      for (i in seq_along(pkID)) {
        cat("  ", paste0(sstat[i,], sep="  "), "\n", sep="")
      }
      cat("\n  probabilities\n")
      cat("  ", paste0(cpval, sep="  "), "\n", sep="")
      for (i in seq_along(pkID)) {
        cat("  ", paste0(spval[i,], sep="  "), "\n", sep="")
      }
      cat("\n  accept at\n")
      cat("  ", paste0(sok, sep="  "), "\n", sep="")
    }
  }   # show.stats
  cat("\n")

  invisible(x)
}

# Print the location of the peaks with test results.  Other arguments are
# ignored.  Uses default alpha values from Diopt to judge significance.
# Returns object invisibly.
summary.Dipeak <- function(object, ...) {

  # Stupid check requirement that changes the argument between print, summary.
  x <- object

  if (0 == nrow(x)) {
    cat("\n  no extrema found\n\n")
    return(invisible(object))
  }

  pkID <- which(!is.na(x$lminID) & !is.na(x$rminID))
  if (0 == length(pkID)) {
    cat("\n  no maxima found\n\n")
    return(invisible(object))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  cat("\n")

  if ("Diw" == attr(x, "source")) {
    aexcur <- opt$alpha.pkexcur.diw
  } else {
    aexcur <- opt$alpha.pkexcur.lp
  }
  spec <- data.frame(var=c('pht', 'pnrun', 'prunlen', 'prunht', 'pexcur'),
                     alpha=c(opt$alpha.ht, opt$alpha.nrun, opt$alpha.runlen,
                             opt$alpha.runht, aexcur),
                     name=c('height model', 'nrun', 'runlen',
                            'runht (permutation)', 'excursion'))
  passed <- list.passes(x[pkID,], spec)

  cat("  summary of ", ifelse(1==length(pkID), "maximum", "maxima"), "\n",
      sep="")
  
  spos <- format(x$pos[pkID])            ; wpos <- max(nchar(spos), 3)
  sx   <- format(x$x[pkID], digits=dgt)  ; wx <- max(nchar(sx), 3)
  if ("ppeak" %in% colnames(x)) {
    spval <- mark.p(x$ppeak[pkID], NA)   ;  wpval <- max(nchar(spval), 5)
    cat(sprintf("  %*s    %*s    %*s     passing tests\n",
                wpos, "pos", wx, "raw", wpval, "ppeak"))
  } else {
    cat(sprintf("  %*s    %*s      passing tests\n",
                wpos, "pos", wx, "raw"))
  }
  for (i in seq_along(pkID)) {
    if ("ppeak" %in% colnames(x)) {
      cat(sprintf("  %*s   (%*s)   %*s     %s\n",
                  wpos, spos[i], wx, sx[i], wpval, spval[i], passed[i]))
    } else {
      cat(sprintf("  %*s   (%*s)     %s\n",
                  wpos, spos[i], wx, sx[i], passed[i]))
    }
  }
  
  cat("\n")
  
  invisible(object)
}

# Build a list of passing tests in Dipeak or Diflat object x, where pspec is
# a data frame with columns $var the name of the variable, $alpha the
# acceptance elvel, and $name the full test name.  The passing test names are
# gathered in a comma-separated list, or "none" if empty.  Returns a vector
# of these results per row in x.
list.passes <- function(x, pspec) {

  passed <- matrix("", nrow=nrow(x), ncol=nrow(pspec))
  for (i in 1:nrow(pspec)) {
    if (pspec$var[i] %in% colnames(x)) {
      pass <- !is.na(x[,pspec$var[i]]) & (x[,pspec$var[i]] < pspec$alpha[i])
      passed[,i] <- ifelse(pass, pspec$name[i], "")
    }
  }
  keep <- 0 < nchar(passed)
  sapply(1:nrow(passed),
         function(i) {
           k <- keep[i,]
           ifelse(!any(k), "none", paste0(passed[i,k], collapse=", "))
         })
}

# Annotate a plot with the location of local peaks and minima in Dipeak
# object x.  Pass the histogram hist if drawing atop one, otherwise assume
# the plot is for spacing.  The Diopt options opt provide graphical parameters
# and other arguments are ignored.  Returns x invisibly.
mark.Dipeak <- function(x, hist=NULL, opt=Diopt(), ...) {

  if (nrow(x) <= 2) {
    return()
  }

  clr <- get.colors(opt$palette)[opt$colID.peak]
  
  # Automatically ignore the first and last entry in the table.
  x <- x[-c(1,nrow(x)),]

  if (is.null(hist)) {
    if ("ppeak" %in% colnames(x)) {
      sel <- x$ismax & (0 < x$naccept)
      abline(v=x$pos[sel], col=clr, lty="62", lwd=2)
      sel <- x$ismax & (is.na(x$naccept) | (0 == x$naccept))
      abline(v=x$pos[sel], col=clr, lty="26", lwd=1)
    } else {
      abline(v=x$pos[x$ismax], col=clr, lty="26", lwd=1)
    }
    abline(v=x$pos[!x$ismax], col=clr, lty=3, lwd=1)
  } else {
    # Something big off the graph to right.
    xr <- 2 * max(hist$breaks)
    y <- max(hist$counts) * x$pos / sum(hist$counts)
    if ("ppeak" %in% colnames(x)) {
      sel <- x$ismax & (0 < x$naccept)
      for (i in which(sel)) {
        lines(c(x$x[i], xr), c(y[i], y[i]),  col=clr, lty="62", lwd=2)
        lines(c(x$x[i], x$x[i]), c(0, y[i]), col=clr, lty="62", lwd=2)
      }
      sel <- x$ismax & (is.na(x$naccept) | (0 == x$naccept))
      for (i in which(sel)) {
        lines(c(x$x[i], xr), c(y[i], y[i]),  col=clr, lty="26", lwd=1)
        lines(c(x$x[i], x$x[i]), c(0, y[i]), col=clr, lty="26", lwd=1)
      }
    } else {
      for (i in which(x$ismax)) {
        lines(c(x$x[i], xr), c(y[i], y[i]),  col=clr, lty="26",  lwd=1)
        lines(c(x$x[i], x$x[i]), c(0, y[i]), col=clr, lty="26",  lwd=1)
      }
    }
  }

  invisible(x)
}

# Create an empty Dipeak object with 0 rows, which should be used as the
# check if there were any features found.
mockup.Dipeak <- function() {

  # These come from find.peaks().
  peakcols <- c("pos", "ismax", "valsd", "lht", "rht", "lminID", "rminID",
                "lsuppID", "rsuppID")

  m <- matrix(0, nrow=0, ncol=length(peakcols), dimnames=list(NULL, peakcols))
  df <- as.data.frame(m)
  class(df) <- c("Dipeak", class(df))
  df
}


### Diflat objects.

# Pretty-print the Diflat class object x.  Other arguments are ignored.
# Returns x invisibly.
print.Diflat <- function(x, ...) {

  if (0 == nrow(x)) {
    cat("\n  no flats found\n\n")
    return(invisible(x))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  hcol <- c("len", "hexcur", "", "")
  pcol <- c("plen", "pexcur", "pflat", "pass")
  show.raw <- ("xst" %in% colnames(x)) && ("xend" %in% colnames(x))
  show.stats <- any(pcol %in% colnames(x))

  sst <- format(x$stID)
  send <- format(x$endID)
  wpos <- max(nchar(sst), nchar(send))
  sst <- format(sst, width=wpos)
  send <- format(send, width=wpos)

  sxst <- format(x$xst, digits=dgt)
  sxend <- format(x$xend, digits=dgt)
  # Uniform widths.  format may treat st, end differently so per column.
  sxst <- format(sxst, width=max(nchar(sxst)))
  sxend <- format(sxend, width=max(nchar(sxend)))

  cstat <- c("endpoints", "raw", hcol, "")
  cpval <- c("endpoints", "", pcol, "")
  sstat <- matrix("", nrow=nrow(x), ncol=length(cpval))
  spval <- matrix("", nrow=nrow(x), ncol=length(cpval))
  sok <- rep("", length(cpval))
  keep <- rep(FALSE, length(cpval))

  sstat[,1] <- sapply(seq_along(sst),
                      function(i) { paste0(sst[i], " - ", send[i]) })
  spval[,1] <- sstat[,1]
  keep[1] <- TRUE
  if (show.raw) {
    sstat[,2] <- sapply(seq_along(sxst),
                        function(i) {
                          paste0("(", sxst[i], " ", sxend[i], ")") })
    keep[2] <- TRUE
  }
  sok[1] <- "accept at"

  if ("plen" %in% colnames(x)) {
    sstat[,3] <- format(x$len)
    spval[,3] <- mark.p(x$plen, opt$alpha.len)
    sok[3] <- mark.p(opt$alpha.len, NA)
    keep[3] <- TRUE
  }

  if ("pexcur" %in% colnames(x)) {
    sstat[,4] <- format(x$hexcur, digits=dgt)
    if ("Diw" == attr(x, "source")) {
      spval[,4] <- mark.p(x$pexcur, opt$alpha.ftexcur.diw)
      sok[4] <- mark.p(opt$alpha.ftexcur.diw, NA)
    } else {
      spval[,4] <- mark.p(x$pexcur, opt$alpha.ftexcur.lp)
      sok[4] <- mark.p(opt$alpha.ftexcur.lp, NA)
    }
    keep[4] <- TRUE
  }

  if (any(!is.na(x$pflat))) {
    spval[,5] <- mark.p(x$pflat, NA)
    sel <- 0 < x$naccept
    spval[sel,6] <- sprintf("T (%0.f)", x$naccept)[sel]
    keep[c(5,6)] <- TRUE
  }

  sstat <- sstat[,keep, drop=FALSE]
  spval <- spval[,keep, drop=FALSE]
  cstat <- cstat[keep]
  cpval <- cpval[keep]
  sok <- sok[keep]

  w <- apply(nchar_marked(rbind(sstat, spval, cstat, cpval, sok)), 2, max)
  # Extra space after position, before total probability, pass columns.
  if (show.raw) {
    rawcol <- 2
    w[rawcol] <- w[rawcol] + 2
  } else {
    rawcol <- 0
    w[1] <- w[1]
  }
  if (keep[5]) {
     w[length(w)-2] <- w[length(w)-2] + 2
     w[length(w)-1] <- w[length(w)-1] + 1
  }
  for (i in 1:ncol(sstat)) {
    sstat[,i] <- format(sstat[,i], width=w[i])
    spval[,i] <- pad.marked(spval[,i], width=w[i])
    cstat[i] <- format(cstat[i], width=w[i], justify=ifelse(rawcol==i,"c","l"))
    cpval[i] <- format(cpval[i], width=w[i], justify=ifelse(rawcol==i,"c","l"))
    sok[i] <- format(sok[i], width=w[i])
  }

  cat("\n")
  if (show.raw) {
    shdr1 <- format("", width=w[1]+w[2]+2)
  } else {
    shdr1 <- format("", width=w[1])
  }
  if (show.stats) {
    cat("  ", shdr1, "  statistics\n", sep="")
  }
  cat("  ", paste0(cstat, sep="  "), "\n", sep="")
  for (i in 1:nrow(x)) {
    cat("  ", paste0(sstat[i,], sep="  "), "\n", sep="")
  }
  if (show.stats) {
    cat("\n  ", shdr1, "  probabilities\n", sep="")
    cat("  ", paste0(cpval, sep="  "), "\n", sep="")
    for (i in 1:nrow(x)) {
      cat("  ", paste0(spval[i,], sep="  "), "\n", sep="")
    }
    cat("\n  ", paste0(sok, sep="  "), "\n", sep="")
  }
  cat("\n")

  invisible(x)
}

# Print the location of flats and their probabilities.  Other arguments are
# ignored.  Returns object invisibly.  Uses default alpha values from Diopt
# to judge significance.
summary.Diflat <- function(object, ...) {

  x <- object

  if (0 == nrow(x)) {
    cat("\n  no flats found\n\n")
    return(invisible(object))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  if ("Diw" == attr(x, "source")) {
    aexcur <- opt$alpha.ftexcur.diw
  } else {
    aexcur <- opt$alpha.ftexcur.lp
  }
  spec <- data.frame(var=c("plen", "pexcur"),
                     alpha=c(opt$alpha.len, aexcur),
                     name=c("length model", "excursion"))
  passed <- list.passes(x, spec)
  
  sst <- format(x$stID)
  send <- format(x$endID)
  wpos <- max(nchar(sst), nchar(send), ceiling((nchar("endpoints")-1)/2))
  sst <- format(sst, width=wpos)
  send <- format(send, width=wpos)
  spts <- sapply(seq_along(sst),
                 function(i) { paste0(sst[i], " - ", send[i]) })
  sposlbl <- format("endpoints", width=max(nchar(spts)), justify="l")

  sxst <- format(x$xst, digits=dgt)
  sxend <- format(x$xend, digits=dgt)
  # Uniform widths.  format may treat st, end differently so per column.
  sxst <- format(sxst, width=max(nchar(sxst)))
  sxend <- format(sxend, width=max(nchar(sxend)))
  sloc <- sapply(seq_along(sxst),
                 function(i) { paste0("(", sxst[i], " ", sxend[i], ")") })
  sloclbl <- format("raw", width=max(nchar(sloc)), justify="c")

  if ("pflat" %in% colnames(x)) {
    spflat <- mark.p(x$pflat, NA)
    wpflat <- max(nchar(spflat), nchar("pflat"))
    spflat <- format(spflat, width=wpflat)
    splbl <- format("pflat", width=wpflat)
  } else {
    wpflat <- 0
    spflat <- rep("", nrow(x))
    splbl <- ""
  }

  cat("\n")
  cat("  summary of ", ifelse(1==nrow(x), "flat", "flats"), "\n", sep="")
  cat("  ", sposlbl, "  ", sloclbl, "  ", splbl, "    passing tests\n", sep="")
  for (i in 1:nrow(x)) {
    cat("  ", spts[i], "  ", sloc[i], "  ", spflat[i], "    ", passed[i], "\n",
        sep="")
  }
  cat("\n")
  
  invisible(object)
}

# Annotate a plot with the location of local flats in Diflat object x.  Pass
# the histogram hist if drawing atop one, otherwise assume the plot is for
# spacing.  The Diopt options opt provide graphical parameters and other
# arguments are ignored.  Returns x invisibly.
mark.Diflat <- function(x, hist=NULL, opt=Diopt(), ...) {

  if (0 == nrow(x)) {
    return()
  }

  if ("pflat" %in% colnames(x)) {
    is.sig <- 0 < x$naccept
  } else {
    is.sig <- rep(FALSE, nrow(x))
  }

  clr <- get.colors(opt$palette)[opt$colID.flat]

  if (is.null(hist)) {
    if ("box" == opt$mark.flat) {
      # Make color transparent so it doesn't obscure the filtered value.
      rgbval <- col2rgb(clr)
      clr <- rgb(rgbval[1], rgbval[2], rgbval[3], 0x8f, maxColorValue=255)
    } else if ("bar" == opt$mark.flat) {
      tics <- axTicks(2)
      off <- 0.1 * diff(range(tics))
    } else {
      stop("internal error - unsupported flat marking")
    }
    
    for (i in 1:nrow(x)) {
      y <- x$srcval[i]
      dy <- x$ht[i] / 2
      if ("box" ==opt$mark.flat) {
        lwd <- ifelse(!is.na(is.sig[i]) & is.sig[i], 2.5, 1.5)
        rect(x$stID[i], y-dy, x$endID[i], y+dy, border=clr, lwd=lwd)
      } else if ("bar" == opt$mark.flat) {
        xends <- c(x$stID[i], x$endID[i])
        lwd <- ifelse(!is.na(is.sig[i]) & is.sig[i], 3, 1.5)
        if ((y - dy - tics[1]) < (tics[length(tics)] - (y + dy))) {
          lines(xends, c(y+dy+off,y+dy+off), col=clr, lwd=lwd)
        } else {
          lines(xends, c(y-dy-off,y-dy-off), col=clr, lwd=lwd)
        }
        points(xends, c(y, y), col=clr, cex=1.5)
      } else {
        stop("internal error - unsupported flat marking")
      }
    }
  } else {
    yflat <- 1.02 * max(hist$counts)
    for (i in 1:nrow(x)) {
      yflat <- max(hist$counts) * ifelse(is.sig[i], 1.025, 1.01)
      lwd <- ifelse(!is.na(is.sig[i]) & is.sig[i], 3, 1.5)
      lines(c(x$xst[i], x$xend[i]), c(yflat, yflat), col=clr, lwd=lwd)
    }
  }

  invisible(x)
}

# Create an empty Diflat object with 0 rows, which should be used as the
# check if there were any features found.
mockup.Diflat <- function() {

  # These come from find.flats().
  flatcols <- c("src", "stID", "endID", "len", "srcval", "ht", "htsd")

  m <- matrix(0, nrow=0, ncol=length(flatcols), dimnames=list(NULL, flatcols))
  df <- as.data.frame(m)
  class(df) <- c("Diflat", class(df))
  df
}


### Didata objects.

# Print tables with the setup of the analysis data (source, filters applied)
# and statistics of the raw data, spacing, and low-pass spacing and interval
# spacing if generated.  Extra arguments are ignored.  Returns x invisibly.
print.Didata <- function(x, ...) {

  if ((0 == nrow(x)) || (ncol(x) < 2)) {
    cat("\n  no data present\n\n")
    return(invisible(x))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  a <- attributes(x)

  have.lp <- "lp.kernel" %in% names(a)
  have.diw <- "diw.window" %in% names(a)

  xstat <- NULL
  idstat <- NULL
  wid = nchar(format(ncol(x)))
  sID <- paste0(format(1, width=wid), " - ", format(ncol(x), width=wid), sep="")
  slbl <- c("row", format("valid", width=nchar(sID), justify="c"),
            "range", "sd")
  xstat <- rbind(xstat, slbl)
  xstat <- rbind(xstat,
                 c("x", sID, format(diff(range(x["x",])), digits=dgt),
                   format(sd(x["x",]), digits=dgt)))
  sID <- paste0(format(2, width=wid), " - ", format(ncol(x), width=wid), sep="")
  xdi <- x["Di",2:ncol(x)]
  xstat <- rbind(xstat,
                 c("Di", sID, format(diff(range(xdi)), digits=dgt),
                   format(sd(xdi), digits=dgt)))

  cat("  data source       ", a$x, "\n")

  if (have.lp) {
    if (a$lp.window < 1.0) {
      flp <- a$lp.window
    } else {
      flp <- a$lp.window / ncol(x)
    }
    cat(sprintf("  low-pass spacing   with %3d (%5.3f) %s filter\n",
               a$wlp, flp, a$lp.kernel))
    sID <- paste0(format(a$lp.stID, width=wid), " - ",
                  format(a$lp.endID, width=wid), sep="")
    xlp <- x["Di",a$lp.stID:a$lp.endID]
    xstat <- rbind(xstat,
                   c("LP Di", sID, format(diff(range(xlp)), digits=dgt),
                     format(sd(xlp), digits=dgt)))
  }
  
  if (have.diw) {
    if (a$diw.window < 1.0) {
      fiw <- a$diw.window
    } else {
      fiw <- a$diw.window / ncol(x)
    }
    cat(sprintf("  interval spacing   with %3d (%5.3f) interval\n",
                a$wdiw, fiw))
    sID <- paste0(format(a$diw.stID, width=wid), " - ",
                  format(ncol(x), width=wid), sep="")
    xdiw <- x["Diw",a$diw.stID:ncol(x)]
    xstat <- rbind(xstat,
                   c("Diw", sID, format(diff(range(xdiw)), digits=dgt),
                     format(sd(xdiw), digits=dgt)))
    if (have.lp) {
      cat(sprintf("     positions at interval end; shift by %+d vs. low-pass\n",
                  ((a$lp.stID+a$lp.endID-1) - (a$diw.stID+ncol(x)-1)) %/% 2))
    } else {
      cat("     positions at interval end\n")
    }
  }
  
  if (!have.lp && !have.diw) {
    cat("  no filters applied\n")
  }

  xstat[,1] <- format(xstat[,1], width=max(nchar(xstat[,1])))
  xstat[,3] <- format(xstat[,3], width=max(nchar(xstat[,3])))
  xstat[,4] <- format(xstat[,4], width=max(nchar(xstat[,4])))

  cat("\nInformation\n")
  for (i in 1:nrow(xstat)) {
    cat("  ", paste(xstat[i,], collapse="    "), "\n", sep="")
  }
  cat("\n")

  invisible(x)
}

# Describe the setup used to generate the data: the data source and filters
# applied.  Extra arguments ignored.  Returns object invisibly.
summary.Didata <- function(object, ...) {

  x <- object

  if (0 == nrow(x)) {
    cat("\n  no data present\n\n")
    return(invisible(object))
  }

  a <- attributes(x)
  have.lp <- "lp.kernel" %in% names(a)
  have.diw <- "diw.window" %in% names(a)

  cat("\n")
  cat("Setup\n")
  cat("  data source       ", a$x, "\n")
  
  if (have.lp) {
    if (a$lp.window < 1.0) {
      flp <- a$lp.window
    } else {
      flp <- a$lp.window / ncol(x)
    }
    cat(sprintf("  low-pass spacing   with %3d (%5.3f) %s filter\n",
               a$wlp, flp, a$lp.kernel))
  }

  if (have.diw) {
    if (a$diw.window < 1.0) {
      fiw <- a$diw.window
    } else {
      fiw <- a$diw.window / ncol(x)
    }
    cat(sprintf("  interval spacing   with %3d (%5.3f) interval\n",
                a$wdiw, fiw))
  }

  if (!have.lp && !have.diw) {
    cat("  no filters applied\n")
  }
  cat("\n")

  invisible(object)
}

# Create an empty matrix with a fixed number of columns and no rows (test
# nrow to check if this is a dummy value).
mockup.Didata <- function() {

  m <- matrix(0, nrow=0, ncol=10, dimnames=list(NULL, NULL))
  attributes(m) <- c(attributes(m), list(x="mockup"))
  class(m) <- c("Didata", class(m))
  m
}


### Dicpt objects.

# Print table of the changepoints found in the spacing per the Dicpt class
# object x.  Other arguments are ignored.  Returns x invisibly.
print.Dicpt <- function(x, ...) {

  if (0 == nrow(x$cpt)) {
    cat("\n  no changepoints found\n\n")
    return(invisible(x))
  }

  opt <- Diopt()
  dgt <- opt$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  spos <- format(x$cpt$pos)
  wpos <- max(nchar(spos), nchar("pos"))
  spos <- format(spos, width=wpos, justify="r")
  sxpos <- format(x$cpt$x, digits=dgt)
  wloc <- max(nchar(sxpos), nchar("raw"))
  sxpos <- format(sxpos, width=wloc, justify="r")
  sval <- format(x$cpt$val, digits=dgt)
  wval <- max(nchar(sval), nchar("Di"))
  sval <- format(sval, width=wval, justify="r")
  snvote <- format(x$cpt$nvote)
  wnvote <- max(nchar(snvote), nchar("nvote"))
  snvote <- format(snvote, width=wnvote, justify="r")
  

  if (1 == nrow(x$cpt)) {
    cat("\n  changepoint\n")
  } else {
    cat("\n  changepoints\n")
  }

  cat(sprintf("  %*s     %*s     %*s    %*s\n",
              wpos, "pos", wloc, "raw", wval, "Di", wnvote, "nvote"))
  for (i in 1:nrow(x$cpt)) {
    cat(sprintf("  %*s    (%*s)    %*s    %*s\n",
                wpos, spos[i], wloc, sxpos[i], wval, sval[i],
                wnvote, snvote[i]))
  }

  cat(sprintf("\n  minimum vote needed: %4.1f\n", x$cptparam$minvote))
  for (s in format_libs(x$voting, "from libraries", 50)) {
    cat(s, "\n")
  }
  cat("  plot to see points during voting\n")
  cat("\n")

  invisible(x)
}

# Print the location of the changepoints in the Dicpt class object.  Other
# arguments are ignored.  Returns object invisibly.
summary.Dicpt <- function(object, ...) {

  x <- object

  if (0 == nrow(x$cpt)) {
    cat("\n  no changepoints found\n\n")
    return(invisible(object))
  }

  cat("\n")
  for (s in format_pos(x$cpt$pos, "changepoints at indices", 50)) {
    cat(s, "\n")
  }
  for (s in format_libs(x$voting, "from libraries", 50)) {
    cat(s, "\n")
  }
  cat("\n")

  invisible(object)
}

# Combine the point indices pos into comma-separated strings divided into
# lines of about w characters, with first with text txt in a header and the
# others with matching spaces.  Returns a vector of the strings, to cat
# one by one.
format_pos <- function(pos, txt, w) {

  p <- paste0(pos, collapse=", ")
  i <- nchar(p) %/% w
  while (1 <= i) {
    j <- (i * w) + 5
    while ((1 < j) && (" " != substr(p, j, j))) {
      j <- j - 1
    }
    substr(p, j,j) <- "\n"
    i <- i - 1
  }

  p <- strsplit(p, "\n", fixed=TRUE)[[1]]
  wtxt <- nchar(txt)
  for (i in seq_along(p)) {
    if (0 < nchar(txt)) {
      if (1 == i) {
        hdr <- sprintf("  %*s  ", wtxt, txt)
      } else {
        hdr <- sprintf("  %*s  ", wtxt, "")
      }
    } else {
      hdr <- ""
    }
    p[i] <- paste0(hdr, p[i], collapse="")
  }

  p
}

# Combine the libraries libs into comma-separated strings divided into lines
# of about w characters, the first with text txt in a header and the others
# with matching spaces.  Returns a vector of the strings, to cat one by one.
format_libs <- function(libs, txt, w) {

  votes <- paste0(libs, collapse=", ")
  i <- nchar(votes) %/% w
  while (1 <= i) {
    j <- (i * w) + 5
    while ((1 < j) && (" " != substr(votes, j, j))) {
      j <- j - 1
    }
    substr(votes, j,j) <- "\n"
    i <- i - 1
  }
  
  votes <- strsplit(votes, "\n", fixed=TRUE)[[1]]
  wtxt <- nchar(txt)
  for (i in seq_along(votes)) {
    if (0 < nchar(txt)) {
      if (1 == i) {
        hdr <- sprintf("  %*s  ", wtxt, txt)
      } else {
        hdr <- sprintf("  %*s  ", wtxt, "")
      }
    } else {
      hdr <- ""
    }
    votes[i] <- paste0(hdr, votes[i], collapse="")
  }

  votes
}

# Annotate a plot with the location of changepoints in Dicpt object x.  Pass
# the histogram hist if drawing atop one, otherwise assume the plot is for
# spacing.  The Diopt options opt provide graphical parameters and other
# arguments are ignored.  Returns x invisibly.
mark.Dicpt <- function(x, hist=NULL, opt=Diopt(), ...) {

  if (0 == nrow(x$cpt)) {
    return(x)
  }

  clr <- get.colors(opt$palette)[opt$colID.cpt]

  if (is.null(hist)) {
    axis(3, x$cpt$pos, labels=FALSE, tcl=2, lwd=0, lwd.ticks=1, col.ticks=clr)
  } else {
    axis(3, x$cpt$x, labels=FALSE, tcl=1.5, lwd=0, lwd.ticks=1, col.ticks=clr)
  }

  invisible(x)
}

# Create an empty Dicpt object with 0 row data frames for $test and $cpt,
# 0 element $voting, and NULL lists for $rawpts, $rawparam, $libpts,
# $cptparam.  0 rows/lengths should be the check used for no results.
mockup.Dicpt <- function() {

  tstcols <- c("library", "detector", "abbrev", "ncpt")
  mtst <- matrix(0, nrow=0, ncol=length(tstcols), dimnames=list(NULL, tstcols))
  dftst <- as.data.frame(mtst)

  cptcols <- c("pos", "x", "val", "nvote")
  mcpt <- matrix(0, nrow=0, ncol=length(cptcols), dimnames=list(NULL, cptcols))
  dfcpt <- as.data.frame(mcpt)

  vote <- logical()

  res <- list(test=dftst, rawpts=NULL, rawparam=NULL, libpts=NULL,
              voting=vote, cpt=dfcpt, cptparam=NULL)
  class(res) <- "Dicpt"
  res
}

# Create a tall graph with the per-detector changepoints in Dicpt object x,
# the library union, and the overall vote.  Use palette from options opt.
# Extra arguments are ignored.  Returns the class object invisibly.
plot.Dicpt <- function(x, opt=Diopt(), ...) {

  layout(matrix(c(1), nrow=1))
  oldpar <- par(mar=c(5,4,4,5)+0.1, cex=1.0)
  on.exit(par(oldpar))

  lkeep <- 0 < sapply(x$libpts, length)

  clrs <- get.colors(opt$palette)
  tmp <- clrs[4]
  clrs[4] <- clrs[7]
  clrs[7] <- tmp

  y0 <- 0
  pts <- NULL
  ypos <- NULL
  pch <- NULL
  cex <- NULL
  llbl <- NULL
  rlbl <- NULL
  ltic <- NULL
  rtic <- NULL
  detlib <- ""
  for (i in 1:nrow(x$test)) {
    if (0 < x$test$ncpt[i]) {
      ytst <- y0 + 0.5
      pts <- c(pts, list(x$rawpts[[i]]))
      ypos <- c(ypos, ytst)
      pch <- c(pch, 20)
      cex <- c(cex, 0.5)
      rtic <- c(rtic, ytst)
      rlbl <- c(rlbl, x$test$abbrev[i])
      y0 <- ytst
      detlib <- x$test$library[i]
    }
    if ((i == nrow(x$test)) ||
        ((x$test$library[i] != x$test$library[i+1]) &&
         (x$test$library[i] == detlib))) {
      ytst <- y0 + 0.5
      pts <- c(pts, list(x$libpts[[x$test$library[i]]]))
      ypos <- c(ypos, ytst)
      pch <- c(pch, 4)
      cex <- c(cex, 1.25)
      ltic <- c(ltic, ytst)
      llbl <- c(llbl, x$test$library[i])
      y0 <- y0 + 1.0
    }
    if ((i < nrow(x$test)) && (x$test$library[i] != x$test$library[i+1])) {
      detlib <- ""
    }
  }

  y0 <- y0 + 0.5
  pts <- c(pts, list(x$cpt$pos))
  ypos <- c(ypos, y0)
  pch <- c(pch, 4)
  cex <- c(cex, 1.25)
  ltic <- c(ltic, y0)
  llbl <- c(llbl, "all")

  plot(0, 0, col=NA, xlab="index", ylab=NA, main="Changepoint Locations",
       mar=par("mar")+c(0,0,0,2), yaxt="n",
       xlim=c(0,x$rawparam$nx+1), ylim=range(ypos))

  # Reserve color 1 for the top row.
  clrID <- 2
  for (i in seq_along(pts)) {
    if (i == length(pts)) {
      colID <- 1
    }
    points(pts[[i]], rep(ypos[i],length(pts[[i]])), col=clrs[clrID],
           pch=pch[i], cex=cex[i])
    if (4 == pch[i]) {
      for (p in pts[[i]]) {
        lines(c(p+x$cptparam$near, p-x$cptparam$near), c(ypos[i], ypos[i]),
              col=clrs[clrID])
      }
      
      clrID <- clrID + 1
      if (length(clrs) < clrID) {
        clrID <- 2
      }
    }
  }

  ylo <- ypos[1] - 0.2
  yhi <- ypos[length(ypos)-1] + 0.2
  for (p in x$cpt$pos) {
    rect(p-x$cptparam$near, ylo, p+x$cptparam$near, yhi,
         col=rgb(t(col2rgb(clrs[1]) / 255), alpha=0.1), border=NA)
  }

  # Stagger the library names to avoid dropping because of overlaps.  The
  # axis function doesn't actually tell us which labels were skipped,
  # otherwise we could just put them in the margin.
  axis(2, at=ltic, labels=FALSE)
  sel <- seq(1, length(ltic), by=2)
  axis(2, at=ltic[sel], labels=llbl[sel], tick=FALSE)
  sel <- seq(2, length(ltic), by=2)
  # lty=0 to not draw the line/ticks, only the label.
  axis(2, at=ltic[sel], labels=llbl[sel], line=1.5, lty=0)
  axis(4, at=rtic, labels=rlbl, las=2, cex.axis=0.8*par("cex.axis"))

  invisible(x)
}


### Common code

# Common formatting of p values, with the potential to mark those at or
# under the significance threshold alpha if the option mark.alpha is TRUE.
# Use a negative number (not zero) or NA for alpha to not mark and just
# format probabilities consistently.  Returns a vector of strings per p.
mark.p <- function(p, alpha) {

  pout <- sprintf("%.*f", 4, p)
  if (!is.na(alpha) && (0 <= alpha) && Diopt()$mark.alpha) {
    for (i in which(p<=alpha)) {
      pout[i] <- sprintf("\033[4m%s\033[24m", pout[i])
    }
  }
  pout
}

# Count the characters in character object x (vector, matrix), adjusting for
# any marked p values.  Returns the counts.
nchar_marked <- function(x) {

  nx <- nchar(x)

  # Do before nx changes.
  at.end <- which('\033[24m' == substring(x, nx-4))
  nx[at.end] <- nx[at.end] - 5
  
  at.st <- which('\033[4m' == substr(x, 1, 4))
  nx[at.st] <- nx[at.st] - 4

  nx
}

# Increase the width of character vector s elements to be width, by adding
# spaces at the end.  If an element is already longer than width, do nothing.
pad.marked <- function(s, width) {

  sapply(s,
         function(s2) {
           w <- nchar(s2)
           if ('\033[24m' == substring(s2, w-4)) {
             w <- w - 5
           }
           if ('\033[4m' == substr(s2, 1, 4)) {
             w <- w - 4
           }
           if (w < width) {
             paste0(s2, format(' ', width=width-w))
           } else {
             s2
           }
         })
}

