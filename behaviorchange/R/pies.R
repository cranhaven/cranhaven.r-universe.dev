#' Practically Important Effect Sizes
#'
#' @param data Optionally, if you want to get A, a data frame.
#' @param controlCol,expCol Optionally, if you want to get A, the names of
#' the columns with control and experimental data.
#' @param d Cohen's d.
#' @param cer The control even rate (see [behaviorchange::nnt()]).
#' @param n The sample size.
#' @param r,threshold,mean,sd Arguments for the [behaviorchange::nnt()]
#' function.
#' @param bootstrapA Whether to use bootstrapping to compute A.
#' @param conf.level The confidence level of confidence intervals.
#'
#' @return A dataframe with all values.
#' @export
#'
#' @examples pies(d = .5, n = 100, cer = .2, threshold = 2);
pies <- function(data = NULL, controlCol = NULL, expCol = NULL,
                 d = NULL, cer = NULL, r = 1, n = NULL,
                 threshold = NULL, mean = 0, sd = 1,
                 bootstrapA = FALSE, conf.level = .95) {

  if (is.null(d) || is.null(cer) || is.null(n) ||
      is.null(threshold)) {
    stop("You must provide Cohen's d (`d`), the control event rate (`cer`), ",
         "the sample size (`n`), and threshold for converting d to ",
         "and the experimental event rate (`threshold`).");
  }


  U3 <- stats::pnorm(d) * 100;
  CLES <- (stats::pnorm((d)/sqrt(2))) * 100;

  # U3 <- compute.es_des(d = d, n.1 = n, n.2 = n, verbose=FALSE)$U3.d;
  # CLES <- compute.es_des(d = d, n.1 = n, n.2 = n, verbose=FALSE)$cl.d;
  NNT <- behaviorchange::nnt(d = d, cer = cer, r = r, n = n,
                             threshold = threshold, mean = mean, sd = sd);

  ### cer, d, threshold, n ->
  ### Compute eer
  ### pass

  if (!is.null(d) && !is.null(cer) && !is.null(threshold) && !is.null(n)) {
    arr <-
      ufs::arr(expPos = round(attr(NNT, "eer") * n),
               expN = n,
               conPos = cer * 10^10,
               conN = 10^10,
               conf.level=conf.level);
    nnt_from_arr <-
      1 / arr$estimate;
    if (all(arr$conf.int < 0) || all(arr$conf.int > 0)) {
      nnt_ci_from_arr <-
        1 / arr$conf.int;
    } else {
      ### Confidence interval must contain infinity, see
      ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1114210/
      nnt_ci_from_arr <-
        sort(1 / arr$conf.int);

      ### Think about how to represent this CI

    }
  }

  if ((!is.null(data)) && is.data.frame(data)) {
    if ((controlCol %in% names(data)) &&
        (expCol %in% names(data))) {
      A <- ufs::A_VarghaDelaney(data[, controlCol],
                                data[, expCol],
                                bootstrap = bootstrapA,
                                conf.level = conf.level);
      ### Potentially implement pretty printing using the `ufs` function
      return(data.frame(NNT = NNT,
                        NNT_from_ARR = nnt_from_arr,
                        NNT_lo = nnt_ci_from_arr[1],
                        NNT_hi = nnt_ci_from_arr[2],
                        ARR = arr$estimate,
                        ARR_lo = arr$conf.int[1],
                        ARR_hi = arr$conf.int[2],
                        CLES = CLES,
                        U3 = U3,
                        A = A));
    } else {
      stop("Not all columns exist in the dataframe!");
    }
  } else {
    ### Potentially implement pretty printing using the `ufs` function
    return(data.frame(NNT = NNT,
                      NNT_from_ARR = nnt_from_arr,
                      NNT_lo = nnt_ci_from_arr[1],
                      NNT_hi = nnt_ci_from_arr[2],
                      ARR = arr$estimate,
                      ARR_lo = arr$conf.int[1],
                      ARR_hi = arr$conf.int[2],
                      CLES = CLES,
                      U3 = U3));
  }

}
