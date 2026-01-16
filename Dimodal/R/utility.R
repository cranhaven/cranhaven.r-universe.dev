
# utility.R:
# A grab-bag of useful helper functions for the Dimodal results.
#
# c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - 
#

##### Public Interface

# Estimate the quantiles q of data x using piecewise linear segments to
# approximate the quantile function for discrete/tied data, with type
# determining the choice of endpoint (1,3 at middle of jumps in x; 2,3 at
# middle of jumps in quantile/index ; 4 between middle of runs, 0
# auto-determine using 3 for quantized data, 4 else).  The default q is at
# the indices of x and re-maps any tied values.  feps is the relative
# tolerance for matching values as per find.runs.  Returns the estimate,
# a vector the same length  as x.
midquantile <- function(x, q=((1:length(x))-1)/(length(x)-1),
                        type=0, feps=0.0) {

  # The default type is best for non-continuous data, as it runs through
  # single data points.  If there are ties, 3 or 1 would be better.  1 is
  # recommended in the literature but can lie entirely to one side of the
  # data; see test case 1.  3 generates a smaller difference between the
  # actual and approximated data than 1 does.  We use a simple heuristic -
  # if the number of unique values is 10% of the data = quantized - to
  # decide.  signif rounds numeric inaccuracies if x is the spacing.
  if (0 == type) {
    if ((10 * length(unique(signif(x)))) < length(x)) {
      type <- 3
    } else {
      type <- 4
    }
  }

  x <- sort(x)
  s <- .Call("C_midq", x, as.integer(type), feps, PACKAGE="Dimodal")
  # The default q shifts indices to 0-based, including max index/count.
  .Call("C_eval_midq", s$x, s$q, q, PACKAGE="Dimodal")
}

# Convert the find.runs result runs from data x into an rle structure, a
# list with elements
#   $lengths   integer vector with the length of each run
#   $values    corresponding values from x, of the same type as x
# Additionally it adds
#   $nskip     integer vector counting non-finite values within each run
runs.as.rle <- function(runs, x) {

  if (length(runs$runs) != length(x)) {
    stop(paste0("runs generated for data of length ", length(runs$runs),
                " but x has ", length(x), " points"))
  }

  sel <- runs$runs != 0
  lens <- runs$runs[sel]
  vals <- x[sel]
  nskip <- runs$nskip[sel]

  structure(list(lengths=lens, values=vals, nskip=nskip), class="rle")
}


# The Dipeak structure contains minima and peaks that cannot be tested because
# they are at the edge of the data, with a minimum to only one side.  Returns
# only those rows of peak result pk with valid maxima.  Returned value will
# have zero rows if there are no valid peaks, or if pk is not a Dipeak object.
select.peaks <- function(pk) {

  if (!inherits(pk, "Dipeak") || (nrow(pk) <= 2) ||
      !("ismax" %in% colnames(pk))) {
    mockup.Dipeak()
  } else {
    pk[pk$ismax & c(FALSE, rep(TRUE, nrow(pk)-2), FALSE),]
  }
}


# Shift any interval spacing feature positions in Dimodal result m to align
# with the raw data.  For peaks this is the 'pos' column, for flats
# the 'src', 'stID', and 'endID' columns - the original data values have
# already been shifted.   Returns the modified x, unless it does not contain
# both spacings or peaks or flats, in which case return x unchanged.
center.diw <- function(m) {

  a <- attributes(m$data)
  if ("diw.window" %in% names(a)) {
    # This can differ from the shift in print.Didata if both the LP and Diw
    # windows are odd, but we don't require m have low-pass results.
    shft <- -(a$wdiw %/% 2)
    if (("diw.peaks" %in% names(m)) && (0 < nrow(m$diw.peaks))) {
      m$diw.peaks$pos <- pmax(1, m$diw.peaks$pos + shft)
    }
    if (("diw.flats" %in% names(m)) && (0 < nrow(m$diw.flats))) {
      m$diw.flats$src <- pmax(1, m$diw.flats$src + shft)
      m$diw.flats$stID <- pmax(1, m$diw.flats$stID + shft)
      m$diw.flats$endID <- pmax(1, m$diw.flats$endID + shft)
    }
  }

  m
}

# Match low-pass and interval features in Dimodal results m.  Peaks must be
# within near points of each other, or if near is a fraction less than 1
# as that fraction of the data length.  The overlap of flats must be at least
# the fraction foverlap of the length of both.  Returns a list with four
# maps of rows in the low-pass and interval spacing features, using nomatch
# (coerced to integer) if there is no corresponding peak/flat.  These are
#   $peak.lp2diw      $peak.diw2lp
#   $flat.lp2diw      $flat.diw2lp
# Also print the matches unless quiet is TRUE.
match.features <- function(m, near=10, foverlap=0.70, nomatch=NA_integer_,
                           quiet=FALSE) {

  if (near <= 0) {
    stop("near must be positive")
  } else if (near < 1.0) {
    near <- round(near * ncol(m$data))
  }

  if ((foverlap <= 0.0) || (1.0 < foverlap)) {
    stop("foverlap not between 0 (excl.) and 1")
  }

  morig <- m
  m <- center.diw(m)

  nomatch <- as.integer(nomatch)
  
  if (("lp.peaks" %in% names(m)) && ("diw.peaks" %in% names(m))) {
    if (is.finite(nomatch) && is.integer(nomatch) && (1 <= nomatch) &&
        ((nomatch <= nrow(m$lp.peaks)) || (nomatch <= nrow(m$diw.peaks)))) {
      stop("nomatch value may conflict with row numbers in result")
    }
    
    lp <- select.peaks(m$lp.peaks)
    diw <- select.peaks(m$diw.peaks)

    if ((0 == nrow(lp)) || (0 == nrow(diw))) {
      pk.lp <- rep(nomatch, nrow(lp))
      pk.diw <- rep(nomatch, nrow(diw))
    } else {
      pk.lp <- sapply(m$lp.peaks$pos,
                      function(p) {
                        if (p %in% lp$pos) {
                          sep <- abs(p - diw$pos)
                          map <- which.min(sep)
                          if (sep[map] <= near) {
                            which(m$diw.peaks$pos == diw$pos[map])
                          } else {
                            nomatch
                          }
                        } else {
                          nomatch
                        }
                      })
      pk.diw <- match(1:nrow(m$diw.peaks), pk.lp, nomatch=nomatch)
    }

    if (!quiet) {
      print_peakmatch(m$lp.peaks, morig$diw.peaks, pk.lp, near)
    }
  }

  if (("lp.flats" %in% names(m)) && ("diw.flats" %in% names(m))) {
    if ((0 == nrow(m$lp.flats)) || (0 == nrow(m$diw.flats))) {
      ft.lp <- rep(nomatch, nrow(m$lp.flats))
      ft.diw <- rep(nomatch, nrow(m$diw.flats))
    } else {
      if (is.finite(nomatch) && is.integer(nomatch) && (1 <= nomatch) &&
          ((nomatch <= nrow(m$lp.flats)) || (nomatch <= nrow(m$diw.flats)))) {
        stop("nomatch value may conflict with row numbers in result")
      }
      ft.lp <- sapply(1:nrow(m$lp.flats),
                      function(i) {
                        olap <- pmin(m$lp.flats$endID[i], m$diw.flats$endID) -
                                pmax(m$lp.flats$stID[i], m$diw.flats$stID) + 1
                        map <- which.max(olap)
                        longest <- max(m$lp.flats$len[i], m$diw.flats$len[map])
                        if (foverlap <= (olap[map] / longest)) {
                          map
                        } else {
                          nomatch
                        }
                      })
      ft.diw <- match(1:nrow(m$diw.flats), ft.lp, nomatch=nomatch)
    }

    if (!quiet) {
      print_flatmatch(m$lp.flats, morig$diw.flats, ft.lp, foverlap)
    }
  }

  if (!quiet) { cat("\n") }

  list(peak.lp2diw=pk.lp, peak.diw2lp=pk.diw,
       flat.lp2diw=ft.lp, flat.diw2lp=ft.diw)
}

# Move the position/endpoints of peaks or flats feat by offset-1 data points
# to align with the spacing grid (use the 'lp.stID' or 'diw.stID' attributes
# from the gen.data matrix).  Add columns 'x' (for peaks), 'xst, 'xend' to
# the matrix with the position mapped back into the original data using the
# mid-distribution xmid, per the discrete quantile correction of
# Ma/Genton/Parzen.  Additional offset midoff is added to the index when
# addressing xmid; use 0 for low-pass/raw spacing positions or half the
# interval.  Returns the modified feat.
shiftID.place <- function(feat, offset, xmid, midoff) {

  if (0 == nrow(feat)) {
    return(feat)
  }

  # The points are located on-grid so no need to interpolate in xmid.

  if ("pos" %in% colnames(feat)) {
    feat$pos <- feat$pos + offset - 1
    feat$x <- xmid[feat$pos+midoff]
  }
  if ("lminID" %in% colnames(feat)) {
    feat$lminID <- feat$lminID + offset - 1
  }
  if ("rminID" %in% colnames(feat)) {
    feat$rminID <- feat$rminID + offset - 1
  }
  if ("lsuppID" %in% colnames(feat)) {
    feat$lsuppID <- feat$lsuppID + offset - 1
  }
  if ("rsuppID" %in% colnames(feat)) {
    feat$rsuppID <- feat$rsuppID + offset - 1
  }
  if ("stID" %in% colnames(feat)) {
    feat$stID <- feat$stID + offset - 1
    feat$xst <- xmid[feat$stID+midoff]
  }
  if ("endID" %in% colnames(feat)) {
    feat$endID <- feat$endID + offset - 1
    feat$xend <- xmid[feat$endID+midoff]
  }

  feat
}


##### Implementation

# Print matching peaks in the Dipeak low-pass data lp and interval spacing diw,
# using the row map lp2diw between the two (NA entries for non-matching rows).
# near is the separation between between maxima to 'match'.
print_peakmatch <- function(lp, diw, lp2diw, near) {

  cat("\n  matching peaks within ", near, " points after centering intervals\n")
  if ((0 == length(lp2diw)) || !any(is.finite(lp2diw))) {
    cat("    none\n")
    return()
  }

  dgt <- Diopt()$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }

  sel.lp <- which(is.finite(lp2diw) &
                  (0 < lp2diw) & (lp2diw <= length(diw$pos)))
  sel.diw <- lp2diw[sel.lp]
      
  spos.lp <- format(lp$pos[sel.lp])
  spos.diw <- format(diw$pos[sel.diw])
  wpos <- max(nchar(spos.lp), nchar(spos.diw), nchar("pos"))

  sx.lp <- format(lp$x[sel.lp], digits=dgt)
  sx.diw <- format(diw$x[sel.diw], digits=dgt)
  wx <- max(nchar(sx.lp), nchar(sx.diw), nchar("raw"))

  spass.lp <- format(lp$naccept[sel.lp])
  spass.diw <- format(diw$naccept[sel.diw])
  wpass <- max(nchar(spass.lp), nchar(spass.diw), nchar("#pass"))
  
  wcol <- wpos + wx + wpass + 8
  cat(paste0("   ", format("low-pass", width=wcol, justify="c"), "   ",
             "   ", trimws(format("interval", width=wcol, justify="c"), "right"),
             "\n"))
  shdr <- sprintf("   %*s    %*s    %*s",
                  wpos, "pos", wx, "raw", wpass, "#pass")
  cat(paste0(shdr, "   ", shdr, "\n"))
  for (i in seq_along(sel.lp)) {
    slp <- sprintf("   %*s   (%*s)   %*s",
                   wpos, spos.lp[i], wx, sx.lp[i], wpass, spass.lp[i])
    sdiw <- sprintf("   %*s   (%*s)   %*s",
                    wpos, spos.diw[i], wx, sx.diw[i], wpass, spass.diw[i])
    cat(paste0(slp, "   ", sdiw, "\n"))
  }
}

# Print matching flats in the Dflat low-pass data lp and interval spacing diw,
# using the row map lp2diw between the two (NA entries for non-matching rows).
# foverlap is the minimum fraction of each flat's length that is common.
print_flatmatch <- function(lp, diw, lp2diw, foverlap) {

  cat(sprintf("\n  matching flats each overlapping by %.2f after centering intervals\n",
              foverlap))
  if ((0 == length(lp2diw)) || !any(is.finite(lp2diw))) {
    cat("    none\n")
    return()
  }

  dgt <- Diopt()$digits
  if (0 == dgt) {
    dgt <- options()$digits
  }
  
  sel.lp <- which(is.finite(lp2diw))
  sel.diw <- lp2diw[sel.lp]

  sst.lp <- format(lp$stID[sel.lp])
  send.lp <- format(lp$endID[sel.lp])
  spos.lp <- sapply(seq_along(sst.lp),
                    function(i) { paste0(sst.lp[i], " - ", send.lp[i]) })
  sst.diw <- format(diw$stID[sel.diw])
  send.diw <- format(diw$endID[sel.diw])
  spos.diw <- sapply(seq_along(sst.diw),
                     function(i) { paste0(sst.diw[i], " - ", send.diw[i]) })
  wpos <- max(nchar(spos.lp), nchar(spos.diw), nchar("pos"))

  sxst.lp <- format(lp$xst[sel.lp], digits=dgt)
  sxend.lp <- format(lp$xend[sel.lp], digits=dgt)
  sx.lp <- sapply(seq_along(sxst.lp),
                  function(i) { paste0("(", sxst.lp[i], " ", sxend.lp[i], ")") })
  sxst.diw <- format(diw$xst[sel.diw], digits=dgt)
  sxend.diw <- format(diw$xend[sel.diw], digits=dgt)
  sx.diw <- sapply(seq_along(sxst.diw),
                   function(i) { paste0("(", sxst.diw[i], " ", sxend.diw[i], ")") })
  wx <- max(nchar(sx.lp), nchar(sx.diw), nchar("raw"))
            
  spass.lp <- format(lp$naccept[sel.lp])
  spass.diw <- format(diw$naccept[sel.diw])
  wpass <- max(nchar(spass.lp), nchar(spass.diw), nchar("#pass"))

  wcol <- wpos + wx + wpass + 8
  cat(paste0("  ", format("low-pass", width=wcol, justify="c"),
             "  ", trimws(format("interval", width=wcol, justify="c"), "right"),
             "\n"))
  shdr <- sprintf("   %s  %s  %*s",
                  format("pos", width=wpos, justify="c"),
                  format("raw", width=wx, justify="c"), wpass, "#pass")
  cat(paste0(shdr, "   ", shdr, "\n"))
  for (i in seq_along(sel.lp)) {
    slp <- sprintf("   %*s  %*s  %*s",
                   wpos, spos.lp[i], wx, sx.lp[i], wpass, spass.lp[i])
    sdiw <- sprintf("   %*s  %*s  %*s",
                    wpos, spos.diw[i], wx, sx.diw[i], wpass, spass.diw[i])
    cat(paste0(slp, "   ", sdiw, "\n"))
  }
}
