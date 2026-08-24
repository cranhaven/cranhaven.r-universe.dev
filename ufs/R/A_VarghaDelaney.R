#' Vargha & Delaney's A
#'
#' @param control A vector with the data for the control condition.
#' @param experimental A vector with the data from the experimental condition.
#' @param bootstrap The number of bootstrap samples to use to compute
#' confidence intervals, or NULL to not compute confidence intervals.
#' @param conf.level The confidence level of the confidence intervals.
#' @param warn Whether to allow the [stats::wilcox.test()] function to emit
#' warnings, for example if ties are encountered.
#'
#' @return A numeric vector of length 1 with the A value, named 'A'.
#' @export
#'
#' @examples ufs::A_VarghaDelaney(1:8, 3:12);
A_VarghaDelaney <- function(control,
                            experimental,
                            bootstrap = NULL,
                            conf.level = .95,
                            warn = FALSE) {
  if (is.null(bootstrap)) {
    return(
      compute_A_VarghaDelaney(
        control = control,
        experimental = experimental,
        warn = warn
      )
    );
  } else if ((is.numeric(bootstrap)) && (bootstrap > 99)) {
    dat <- data.frame(condition = c(rep(0, length(control)),
                                    rep(1, length(experimental))),
                      x = c(control,
                            experimental));
    bootstrappedAs <-
      replicate(bootstrap,
                {
                  bootstrappedDat <- dat[sample(1:nrow(dat),
                                                   size=nrow(dat),
                                                   replace=TRUE), ];
                  compute_A_VarghaDelaney(
                    control=bootstrappedDat[bootstrappedDat$condition==0, 'x'],
                    experimental=bootstrappedDat[bootstrappedDat$condition==1, 'x']
                  );
                });
    ci <-
      stats::quantile(
        bootstrappedAs,
        probs = c((1-conf.level) / 2,
                  1 - (1-conf.level) / 2));
    res <- t(ci);
    colnames(res) <- c("lo", "hi");
    rownames(res) <-
      paste0("A = ",
             compute_A_VarghaDelaney(
               control = control,
               experimental = experimental,
               warn = warn
             ));
    return(res);
  } else {
    stop("If providing a value for `bootstrap`, please provide ",
         "a number of at least 100.");
  }
}

compute_A_VarghaDelaney <- function(control,
                                    experimental,
                                    warn = FALSE) {
  n1 <- length(control);
  n2 <- length(experimental);
  if (warn) {
    W <- stats::wilcox.test(experimental, control)$statistic;
  } else {
    suppressWarnings(W <- stats::wilcox.test(experimental, control)$statistic);
  }
  U <- (n1 * n2) - W;
  return(stats::setNames(((n1 * n2) - U) /
                          (n1 * n2),
                 "A"));
}
