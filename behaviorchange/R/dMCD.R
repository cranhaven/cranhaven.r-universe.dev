#' Estimate Cohen's d corresponding to a Meaningful Change Definition
#'
#' This function uses a base rate (Control Event Rate, argument `cer`) and a
#' Meaningful Change Definitions (MCD, argument `mcd`) to compute the
#' corresponding Cohen's d. See Gruijters & Peters (2019) for details.
#'
#' @param cer The Control Event Rate (or base rate): how many people already
#' perform the target behavior in the population (as a proportion)?
#' @param mcd  The Meaningful Change Definitions: by which percentage (as a
#' proportion) should the event rate increase to render an effect meaningful?
#' @param eer Instead of the MCD, it is also possible to specify the Experimental
#' Event Rate (EER), in which case the MCD is computed by taking the difference
#' with the CER.
#' @param plot Whether to show a plot.
#' @param mcdOnX Whether to plot the Meaningful Change Definition on the X axis
#' (by default, the CER is plotted on the X axis).
#' @param plotResultValues Whether to plot the result values.
#' @param resultValueLineColor,resultValueLineSize If plotting the result values,
#' lines of this color and size are used.
#' @param returnLineLayerOnly Whether to only return a layer with the plotted
#' line (which can be used to quickly stack lines for different MCDs).
#' @param theme The `ggplot2` theme to use.
#' @param highestPossibleEER The highest possible EER to include in the plot.
#' @param xLab,yLab The labels for the X and Y axes.
#' @param dist,distArgs,distNS Used to specify the distribution to use to convert
#' between Cohen's d and the CER and EER. distArgs can be used to specify
#' additional arguments to the corresponding `q` and `p` functions, and
#' distNS to specify the namespace (i.e. package) from where to get the
#' distribution functions.
#' @param x The object to print (i.e. a result from a call to dMCD).
#' @param ... Any additional arguments to dMCD are passed on to the `ggplot2::geom_line`
#' used to draw the line showing the different Cohen's d values as a function of
#' the base rate (or MCD) on the X axis. Additional arguments for the print method
#' are passed on to the default print method.
#'
#' @references Gruijters, S. L. K., & Peters, G.-J. Y. (2020). Meaningful change
#' definitions: Sample size planning for experimental intervention research.
#' *PsyArXiv*. \doi{10.31234/osf.io/jc295}
#'
#' @return The Cohen's d value, optionally with a `ggplot2` plot stored in an
#' attribute (which is only a `ggplot2` layer if `returnLineLayerOnly=TRUE`).
#' @rdname dMCD
#' @examples dMCD(.2, .05);
#' @export
dMCD <- function(cer,
                 mcd = NULL,
                 eer = NULL,
                 plot=TRUE,
                 mcdOnX = FALSE,
                 plotResultValues = TRUE,
                 resultValueLineColor = "blue",
                 resultValueLineSize = 1,
                 returnLineLayerOnly = FALSE,
                 theme = ggplot2::theme_bw(),
                 highestPossibleEER = .999999999,
                 xLab = ifelse(mcdOnX,
                               "Meaningful Change Definition",
                               "Control Event Rate"),
                 yLab = "Cohen's d",
                 dist = "norm",
                 distArgs=list(),
                 distNS="stats",
                 ...) {

  providedArgs <- as.list(environment());

  if (!xor(is.null(mcd), is.null(eer))) {
    stop("Specify exactly one of `mcd` and `eer`. If you specify ",
         "the meaningful change definition (`mcd`), I will use it ",
         "to compute the experimental event rate; otherwise, ",
         "the other way around.");
  }

  if (!is.null(eer)) {
    mcd <- numeric();
    d <- numeric();
    for (i in cer) {
      for (j in eer) {
        mcd <- c(mcd,
                 j - i);
        d <- c(d,
               ufs::convert.cer.to.d(cer=i,
                                     eer=j,
                                     dist=dist,
                                     distArgs=distArgs,
                                     distNS=distNS));
      }
    }
    mcd <- matrix(mcd,
                  ncol=length(eer),
                  byrow=TRUE,
                  dimnames = list(cer = cer,
                                  eer = eer));
    d <- matrix(d,
                ncol=length(eer),
                byrow=TRUE,
                dimnames = list(cer = cer,
                                eer = eer));
    if (getOption("ufs.debug", FALSE)) {
      print(mcd);
      print(d);
    }
  } else {
    eer <- numeric();
    d <- numeric();
    for (i in cer) {
      for (j in mcd) {
        eer <- c(eer,
                 i + j);
        d <- c(d,
               ufs::convert.cer.to.d(cer=i,
                                     eer=min(i+j, highestPossibleEER),
                                     dist=dist,
                                     distArgs=distArgs,
                                     distNS=distNS));
      }
    }
    eer[eer > 1] <- highestPossibleEER;
    eer <- matrix(eer,
                  ncol=length(mcd),
                  byrow=TRUE,
                  dimnames = list(cer = cer,
                                  mcd = mcd));
    d <- matrix(d,
                ncol=length(mcd),
                byrow=TRUE,
                dimnames = list(cer = cer,
                                mcd = mcd));
    if (getOption("ufs.debug", FALSE)) {
      print(eer);
      print(d);
    }
  }

  res <- d;

  if ((!is.null(providedArgs$mcd) && length(providedArgs$mcd) == 1)) {

    plot <- ggplot2::ggplot() +
      theme;

    if (mcdOnX) {

      ### Generate plot
      mcdSeq <- seq(0.001,
                    0.999,
                    by=.001);
      eerSeq <- providedArgs$cer + mcdSeq;
      eerSeq <- ifelse(eerSeq < highestPossibleEER,
                       eerSeq,
                       NA);
      dSeq <-
        ufs::convert.cer.to.d(cer=rep(cer, length(eerSeq)),
                              eer=eerSeq,
                              dist=dist, distArgs=distArgs, distNS=distNS);

      if (plotResultValues) {
        plot <- plot +
          ggplot2::geom_hline(yintercept = d,
                              color=resultValueLineColor,
                              size = resultValueLineSize) +
          ggplot2::geom_vline(xintercept = mcd,
                              color=resultValueLineColor,
                              size = resultValueLineSize) +
          ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(name="",
                                                                   breaks=mcd,
                                                                   labels=paste0("MCD=",
                                                                                 round(mcd, 3)))) +
          ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name="",
                                                                   breaks=d,
                                                                   labels=paste0("d=",
                                                                                 round(d, 2))));
      }

      lineLayer <-
        ggplot2::geom_line(data = data.frame(mcd=mcdSeq,
                                             d = dSeq),
                           mapping = ggplot2::aes_string(x='mcd',
                                                         y='d'),
                           na.rm=TRUE,
                           ...);

    } else {

      ### Generate plot
      cerSeq <- seq(0.001,
                    0.999,
                    by=.001);
      eerSeq <- cerSeq + providedArgs$mcd;
      eerSeq <- ifelse(eerSeq < highestPossibleEER,
                       eerSeq,
                       NA);
      dSeq <-
        ufs::convert.cer.to.d(cer=cerSeq,
                              eer=eerSeq,
                              dist=dist, distArgs=distArgs, distNS=distNS);

      if (plotResultValues) {
        plot <- plot +
          ggplot2::geom_hline(yintercept = d,
                              color=resultValueLineColor,
                              size = resultValueLineSize) +
          ggplot2::geom_vline(xintercept = cer,
                              color=resultValueLineColor,
                              size = resultValueLineSize) +
          ggplot2::scale_x_continuous(
            sec.axis = ggplot2::dup_axis(
              name="",
              breaks=cer,
              labels=paste0("CER=",
                            round(cer, 3))
            )
          ) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::dup_axis(
              name="",
              breaks=d,
              labels=paste0("d=",
                            round(d, 2))
            )
          );
      }

      lineLayer <-
        ggplot2::geom_line(data = data.frame(cer=cerSeq,
                                             d = dSeq),
                           mapping = ggplot2::aes_string(x='cer',
                                                         y='d'),
                           ...,
                           na.rm=TRUE);

    }


    if (returnLineLayerOnly) {
      return(lineLayer);
    }

    attr(res, 'plot') <-
      plot +
      lineLayer +
      ggplot2::xlab(xLab) +
      ggplot2::ylab(yLab);
  }

  class(res) <- "dMCD";

  return(res);

}

#' @rdname dMCD
#' @method print dMCD
#' @export
print.dMCD <- function(x, ...) {
  ### Work-around - no idea why printing the plot throws these warnings:
  ###   Warning messages:
  ###   1: In min(x) : no non-missing arguments to min; returning Inf
  ###   2: In max(x) : no non-missing arguments to max; returning -Inf
  ###   3: In min(x) : no non-missing arguments to min; returning Inf
  ###   4: In max(x) : no non-missing arguments to max; returning -Inf
  y <- x;
  attr(y, "plot") <- NULL;
  print(unclass(y));
  suppressWarnings(print(attr(x, "plot")));
  return(invisible(x));
}
