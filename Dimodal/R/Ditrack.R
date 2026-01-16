
# Ditrack.R:
# Mode tree / SiZer type tracking of feature significance as the kernel size/
# interval width varies.
#
# c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - 

## Manually Tested With:           LP      Diw
#    faithful$eruptions            p,f     p,f
#    faithful$waiting              p,f     p,f
#    iris$Petal.Length             p,f      p      LP: warns small kernel
#    iris$Petal.Width               p       p      LP: warns small kernel
#    quakes[,3]                    p,f     p,f     Diw graphs slant right
#    chickwts$weight                p       p      LP: warns small kernel
#    tstds1                        p,f     p,f
#    tstds2                        p,f     p,f
#    tstds3                        p,f     p,f
#    tstds4                        p,f     p,f
#    meuse$cadmium  (sp)           p,f     p,f     LP: warns small kernel
#    meuse$copper   (sp)           p,f     p,f     LP: warns small kernel
#    paulsen$y      (boot)         p,f     p,f
#    statfaculty    (diptest)       p       p      LP: warns small kernel
#    Mammals$speed  (quantreg)     p,f      p      LP: warns small kernel
#                                                  LP flat at only one window
#    stamps         (multimode)    p,f     p,f
#    galaxy         (multimode)    p,f     p,f     Diw flat plot crashes scale
#    acicdity       (multimode)    p,f     p,f     LP: warns small kernel
#    enzyme         (multimode)    p,f     p,f     LP: warns small kernel
#              



##### Public Interface

# Track the peaks and flats in data x in smooth spacing while the filter or
# interval size changes from 0.01 t/m minwindow.  All analysis is done with
# the opt options list; the [lp|diw].window value is overwritten and should
# not appear in the param option.  Returns a list of class 'Ditrack' with
# elements
#   $peaks   subset of Dipeak data frame w/ pos, ismax, ppeak, plus winpct
#   $flats   subset of Diflat data w/ stID, endID, pflat, plus winpct
#   $nx      length of the data x
#   $smooth  filter applied, 'lp' or 'diw'
# The winpct column is window/interval size as a percent, ie. 1 t/m 40.
# There is one row in peaks and flats for each feature, thus multiple rows
# per winpct.
Ditrack <- function(x, smooth=c("lp", "diw"), opt=Diopt()) {

  smooth <- match.arg(smooth)
  opt$analysis <- smooth

  pkcol <- c("pos", "ismax", "ppeak", "naccept")
  ftcol <- c("stID", "endID", "pflat", "naccept")

  # Other ideas from development for data and plotting:
  #  Add a minimum-minimum width column (rminID-lminID+1) to pk.
  #  Copy the htsd column to pk.
  #  Generate graph of matching features, ie. trace features as window changes.
  #  Measure depth along graph.
  maxwin <- opt$track.maxwindow
  if (maxwin < 1) {
    maxwin <- round(maxwin * 100)
  }
  feat <- lapply(1:maxwin,
                 function(i) {
                   if ("lp" == smooth) {
                     opt$lp.window <- 0.01 * i
                   } else if ("diw" == smooth) {
                     opt$diw.window <- 0.01 * i
                   } else {
                     stop("unsupported smoothing method")
                   }
                   
                   # We could try to screen specific warnings by using
                   # warning(warningCondition(msg, class='warn.track'))
                   # and suppressing by class, but there are several possible,
                   # including model OOB and kernel too small, so just ignore
                   # everything.
                   m <- suppressWarnings(Dimodal(x, opt))
                   if ("lp" == smooth) {
                     peaks <- m$lp.peaks
                     flats <- m$lp.flats
                   } else if ("diw" == smooth) {
                     peaks <- m$diw.peaks
                     flats <- m$diw.flats
                   } else {
                     stop("unsupported smoothing method")
                   }
                  
                   npk <- nrow(peaks)
                   if (2 < npk) {
                     pk <- peaks[-c(1,npk), pkcol, drop=FALSE]
                   } else {
                     pk <- matrix(0, nrow=0, ncol=length(pkcol),
                                  dimnames=list(NULL, pkcol))
                   }
                   pk <- cbind(winpct=rep(i, nrow(pk)), pk)
                   pk <- as.matrix(pk)
                   # Named by row in [lp|diw].peaks, copied into pk.  Ignore.
                   rownames(pk) <- NULL
                  
                   if (0 < nrow(flats)) {
                     ft <- flats[,ftcol, drop=FALSE]
                   } else {
                     ft <- matrix(0, nrow=0, ncol=length(ftcol),
                                  dimnames=list(NULL, ftcol))
                   }
                   ft <- cbind(winpct=rep(i, nrow(flats)), ft)
                   ft <- as.matrix(ft)
                   rownames(ft) <- NULL
                  
                   list(pk=pk, ft=ft)
                 })

  pk <- NULL
  ft <- NULL
  for (i in seq_along(feat)) {
    pk <- rbind(pk, feat[[i]]$pk)
    ft <- rbind(ft, feat[[i]]$ft)
  }
  
  trk <- list(peaks=pk, flats=ft, nx=length(x), smooth=smooth)
  class(trk) <- c("Ditrack", class(trk))
  trk
}

# Print a table of the number of significant features as the window size
# varies in Ditrack result x.  Extra arguments are ignored.  Returns
# x invisibly.
print.Ditrack <- function(x, ...) {

  if ((0 == nrow(x$peaks)) && (0 == nrow(x$flats))) {
    cat("\nno peaks or flats at any window size\n\n")
    return(invisible(x))
  }
  if ((5 != ncol(x$peaks)) || (5 != ncol(x$flats))) {
    cat("\ninternal error - bad Ditrack contents\n\n")
    return(invisible(x))
  }

  if (0 < nrow(x$peaks)) {
    pkwrng <- range(x$peaks[,"winpct"])
    pkcnt <- sapply(pkwrng[1]:pkwrng[2],
                    function(i) {
                      sel <- x$peaks[,"winpct"] == i
                      c(sum(x$peaks[sel,"ppeak"] <= 0.01, na.rm=TRUE),
                        sum(x$peaks[sel,"ppeak"] <= 0.05, na.rm=TRUE),
                        sum(x$peaks[sel,"naccept"]))
                    }, simplify="array")
  } else {
    pkwrng <- NULL
    pkcnt <- NULL
  }

  if (0 < nrow(x$flats)) {
    ftwrng <- range(x$flats[,"winpct"])
    ftcnt <- sapply(ftwrng[1]:ftwrng[2],
                    function(i) {
                      sel <- x$flats[,"winpct"] == i
                      c(sum(x$flats[sel,"pflat"] <= 0.01, na.rm=TRUE),
                        sum(x$flats[sel,"pflat"] <= 0.05, na.rm=TRUE),
                        sum(x$flats[sel,"naccept"]))
                    })
  } else {
    ftwrng <- NULL
    ftcnt <- NULL
  }

  if ("lp" == x$smooth) {
    wtxt <- "LP kernel size"
  } else if ("diw" == x$smooth) {
    wtxt <- "interval size"
  } else {
    stop("unsupported method generated features")
  }
  wtxt2 <- "at level"
  wwin <- max(nchar(wtxt), nchar(wtxt2))
  wcnts <- 16

  ptxt <- ifelse(is.null(pkcnt), "", format("peaks", width=wcnts, justify="c"))
  ftxt <- ifelse(is.null(ftcnt), "", format("flats", width=wcnts, justify="c"))
  cat("\n")
  cat("Significant Feature Counts\n")
  cat(sprintf("%*s    %*s    %*s\n", wwin, "", wcnts, ptxt, wcnts, ftxt))
  if (is.null(pkwrng)) {
    pk01 <- ""
    pk05 <- ""
  } else {
    pk01 <- "0.01"
    pk05 <- "0.05"
  }
  if (is.null(ftwrng)) {
    ft01 <- ""
    ft05 <- ""
  } else {
    ft01 <- "0.01"
    ft05 <- "0.05"
  }
  cat(sprintf("%*s    %4s  %4s  %4s    %4s  %4s  %4s\n",
              wwin, wtxt2, pk01, pk05, "pass", ft01, ft05, "pass"))
  cat(sprintf("%*s\n", wwin, wtxt))
  
  for (i in min(pkwrng[1],ftwrng[1]):max(pkwrng[2],ftwrng[2])) {
    if (is.null(pkwrng) || (i < pkwrng[1]) || (pkwrng[2] < i)) {
      pk01 <- ""
      pk05 <- ""
      pkpass <- ""
    } else {
      pk01 <- sprintf("%4d", pkcnt[1,i-pkwrng[1]+1])
      pk05 <- sprintf("%4d", pkcnt[2,i-pkwrng[1]+1])
      pkpass <- sprintf("%4d", pkcnt[3,i-pkwrng[1]+1])
    }
    if (is.null(ftwrng) || (i < ftwrng[1]) || (ftwrng[2] < i)) {
      ft01 <- ""
      ft05 <- ""
      ftpass <- ""
    } else {
      ft01 <- sprintf("%4d", ftcnt[1,i-ftwrng[1]+1])
      ft05 <- sprintf("%4d", ftcnt[2,i-ftwrng[1]+1])
      ftpass <- sprintf("%4d", ftcnt[3,i-ftwrng[1]+1])
    }
    
    cat(sprintf("%*d    %4s  %4s  %4s    %4s  %4s  %4s\n",
                wwin,i, pk01,pk05,pkpass, ft01,ft05,ftpass))
  }
  cat("\n")

  invisible(x)
}

# Summarize the tracking results object by printing the ranges of window sizes
# with accepted features, for both peaks and flats.  Extra arguments ignored.
summary.Ditrack <- function(object, ...) {

  cat("\n")
  if ("lp" == object$smooth) {
    cat("Window Sizes (lp.window) With Accepted Features\n")
  } else if ("diw" == object$smooth) {
    cat("Interval Sizes (diw.window) With Accepted Features\n")
  } else {
    stop("unsupported method generated filters")
  }

  cat("  peaks   ", paste0(pass.range(object$peaks), sep="   "), "\n")
  cat("  flats   ", paste0(pass.range(object$flats), sep="   "), "\n")
  cat("\n")

  invisible(object)
}

# Return a vector of strings with the continguous ranges of window sizes
# that have more than one passing (naccept > 0) feature for Ditrack
# peaks/flats tracker x.
pass.range <- function(x) {

  if (0 == nrow(x)) {
    return(c("none"))
  }
  
  npass <- c(0, sign(sapply(split(x[,'naccept'], x[,'winpct']), sum)), 0)
  st <- names(npass)[which(npass[-1L]>npass[-length(npass)])+1]
  end <- names(npass)[which(npass[-1L]<npass[-length(npass)])]

  if (length(st) < length(end)) {
    st <- c(st, rep(NA, length(end)-length(st)))
  } else if (length(end) < length(st)) {
    end <- c(st, rep(NA, length(st)-length(end)))
  }

  st <- as.integer(st)
  end <- as.integer(end)

  sapply(1:length(st),
         function(i) { sprintf("%4.2f-%4.2f", 0.01*st[i],0.01*end[i]) })
}

# Graph the peaks and/or flats as the low-pass filter/interval size changes,
# as stored in Ditrack object x (feature may be a vector), using parameters
# in options opt.  Extra arguments are ignored.  Returns x invisibly.  The
# function uses layout to place the plots, even if there is only one feature.
plot.Ditrack <- function(x, feature=c("peaks", "flats"), opt=Diopt(), ...) {

  feature <- match.arg(feature, several.ok=TRUE)

  layout(matrix(1:length(feature), nrow=1))
  # layout for a 3 grid, as in the Dimodal plot, forces cex to 0.66.
  # Duplicate that here.
  oldpar <- par(cex=0.66)
  on.exit(par(oldpar))
  
  maxwin <- opt$track.maxwindow
  if (maxwin < 1) {
    maxwin <- round(maxwin * 100)
  }

  if ("peaks" %in% feature) {
    plot_pktrack(x$peaks, x$nx, x$smooth, maxwin)
  }
  if ("flats" %in% feature) {
    plot_fttrack(x$flats, x$nx, x$smooth, maxwin)
  }

  invisible(x)
}


##### Implementation

# Plot the location of peaks and minima as the smooth filter width varies, as
# stored in the combined matrix pk over windows from 1 t/m maxwin.  nx is the
# number of points in the original data.
plot_pktrack <- function(pk, nx, smooth, maxwin) {

  if (is.null(pk) || (0 == nrow(pk)) || (5 != ncol(pk))) {
    dummyplot("no peaks found")
    return()
  }

  oldpar <- par(mar=c(5,4,4,4)+0.1)
  on.exit(par(oldpar))

  # Set up coloring of ppeak.  Color nbreak+1 will be for minima.
  nbreak <- 5
  breaks <- c(0, 0.005, 0.01, 0.05, 0.10, 1.0)
  brkID <- cut(pk[,"ppeak"], breaks, labels=FALSE, include.lowest=TRUE)
  brkID[is.na(brkID)] <- nbreak + 1
  ptcex <- seq(1.25, 0.75, len=nbreak)
  ptcex <- c(ptcex, 1.10)

  clrs <- track.pcol(nbreak)
  
  # ASCII 45 is '-'.  19/20 are larger/smaller dots.
  pch <- ifelse(is.na(pk[,"ppeak"]), 45, 20)
  pch <- sapply(1:nrow(pk),
                function(i) {
                  if (is.na(pk[i,"ppeak"])) {
                    45
                  } else if (0 == pk[i,"naccept"]) {
                    1
                  } else {
                    20
                  }
                })

  if ("lp" == smooth) {
    ylbl <- "low-pass kernel size"
  } else if ("diw" == smooth) {
    ylbl <- "interval width"
  } else {
    stop("unsupported smooth generated peaks")
  }
  
  plot(0,0, col=NA, xlim=c(0,nx), ylim=c(0,maxwin+1),
       xlab="index", ylab=ylbl, main="Peak Track\n", yaxt="n")
  abline(h=seq(5, maxwin, by=5), col="grey50", lty=3)

  ytick <- seq(0, maxwin, by=10)
  axis(2, at=ytick, labels=round(0.01 * ytick * nx))
  axis(4, at=ytick, labels=0.01*ytick)

  for (i in 1:nrow(pk)) {
    points(pk[i,"pos"], pk[i,"winpct"], col=clrs[brkID[i]], pch=pch[i],
           cex=ptcex[brkID[i]]*ifelse(1==pch[i],0.75,1.0))
  }

  xl <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  dxl <- 0.06 / 2
  yl <- 1.04 * (maxwin + 1)
  rect(nx*(xl-dxl), yl-1, nx*(xl+dxl), yl, col=clrs, border=NA)
  mtext(c("0.005", "0.01", "0.05", "0.10", "1.0"), at=nx*xl, cex=0.75)
  mtext("ppeak <=", line=1, at=nx*xl[1], cex=0.75)
}

# Plot the spans of flats as the smooth filter window varies, as stored in
# the combined matrix ft over windows from 1 t/m maxwin, coloring by the best
# probability for the feature.  nx is the number of points in the original
# data.
plot_fttrack <- function(ft, nx, smooth, maxwin) {

  if (is.null(ft) || (0 == nrow(ft)) || (5 != ncol(ft))) {
    dummyplot("no flats found")
    return()
  }

  oldpar <- par(mar=c(5,4,4,4)+0.1)
  on.exit(par(oldpar))

  nbreak <- 5
  breaks <- c(0, 0.005, 0.01, 0.05, 0.10, 1.0)
  brkID <- cut(ft[,"pflat"], breaks, labels=FALSE, include.lowest=TRUE)
  lwd <- seq(2.5, 1.25, len=nbreak)

  clrs <- track.pcol(nbreak)
 
  if ("lp" == smooth) {
    ylbl <- "low-pass kernel size"
  } else if ("diw" == smooth) {
    ylbl <- "interval width"
  } else {
    stop("unsupported smooth generated flats")
  }
  
  plot(0,0, col=NA, xlim=c(0,nx), ylim=c(0,maxwin+1),
       xlab="index", ylab=ylbl, main="Flat Track\n", yaxt="n")
  abline(h=seq(5, maxwin, by=5), col="grey50", lty=3)

  ytick <- seq(0, maxwin, by=10)
  axis(2, at=ytick, labels=round(0.01 * ytick * nx))
  axis(4, at=ytick, labels=0.01*ytick)

  if (1 == nrow(ft)) {
    # cbind below would turn ftplt into a vector.
    ftplt <- matrix(c(ft, brkID[1]), nrow=1,
                    dimnames=list(NULL, c(colnames(ft), "clrID")))
  } else {
    # In case of overlapping flats, draw more significant on top = later.
    # Not going to worry about marking accepted flats for this reason.
    o <- order(ft[,"pflat"], decreasing=TRUE)
    # If you pass vectors as x,y, ex. ft[o,c('stID','winpct')] and endID,winpct,
    # then lines will connect between flats, ie. must plot one row at a time.
    ftplt <- cbind(ft[o,], clrID=brkID[o])
  }
  apply(ftplt, 1,
        function(r) {
          y <- r["winpct"]
          lines(r[c("stID","endID")], c(y,y), col=clrs[r["clrID"]],
                lwd=lwd[r["clrID"]]) })

  xl <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  dxl <- 0.06 / 2
  yl <- 1.04 * (maxwin + 1)
  rect(nx*(xl-dxl), yl-1, nx*(xl+dxl), yl, col=clrs, border=NA)
  mtext(c("0.005", "0.01", "0.05", "0.10", "1.0"), at=nx*xl, cex=0.75)
  mtext("pflat <=", line=1, at=nx*xl[1], cex=0.75)
}

# Generate a list of nclr+1 colors for plotting the p values from the tracks.
# The palette choice is fixed (Plasma, with the last color from YlOrRed).
track.pcol <- function(nclr) {

 clrs <- hcl.colors(nclr, "Plasma")
 clrs[nclr] <- hcl.colors(nclr+1, "YlOrRd")[nclr]
 # Last color duplicated so minima at least significant value.
 c(clrs, clrs[length(clrs)])
}

# Create an empty plot with the text message msg in the center.
dummyplot <- function(msg) {

  plot(0,0, col=NA, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE)
  box()
  text(0.5,0.5, msg, adj=c(0.5,0.5), cex=1)
}


# Plot all HCL palettes pals with n colors, to pick a good gradient
# for the p values.  Can pass colorspace::[deu|tri|pro]tan to see how
# they look for the color blind.
plot_hclpals <- function(n, cblindfn=identity, pals=hcl.pals("sequential")) {

  # Interesting choice:  Plasma, Inferno, Viridis, Ballow, ag_Sunset

  pals <- sort(pals)
  ncol <- ceiling(sqrt(length(pals)))
  nrow <- ceiling(length(pals) / ncol)
  clrs <- sapply(pals, function(p) { cblindfn(hcl.colors(p, n=n)) })

  plot(0,0, col=NA, xlim=c(0,ncol), ylim=c(0,nrow), ann=FALSE, axes=FALSE)
  rID <- 0
  cID <- 0
  for (i in seq_along(pals)) {
    xl <- cID + (0:(n-1) / n)
    xr <- cID + ((1:n) / n)
    rect(xl, rID+0.1, xr, rID+0.7, col=clrs[,i], border=clrs[,i])
    rect(xl[1], rID+0.1, xr[n], rID+0.7, col=NA, border="white")
    text(cID+0.5, rID+0.85, pals[i], adj=c(0.5,0.5), cex=1.5)
    cID <- cID + 1
    if (cID == ncol) {
      cID <- 0
      rID <- rID + 1
    }
  }
}
