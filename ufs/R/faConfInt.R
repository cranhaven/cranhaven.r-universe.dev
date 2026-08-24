#' Extract confidence bounds from psych's factor analysis object
#'
#' This function contains some code from a function in [psych::psych-package]
#' that's not exported `print.psych.fa.ci` but useful nonetheless. It
#' basically takes the outcomes of a factor analysis and extracted the
#' confidence intervals.
#'
#' THis function extract confidence interval bounds and combines them with
#' factor loadings using the code from the `print.psych.fa.ci` in
#' [psych::psych-package].
#'
#' @param fa The object produced by the [psych::fa()] function from the
#' [psych::psych-package] package. It is important that the `n.iter` argument
#' of[psych::fa()] was set to a realistic number, because otherwise, no
#' confidence intervals will be available.
#' @return A list of dataframes, one for each extracted factor, with in each
#' dataframe three variables: \item{lo}{lower bound of the confidence interval}
#' \item{est}{point estimate of the factor loading} \item{hi}{upper bound of
#' the confidence interval}
#' @author William Revelle (extracted by Gjalt-Jorn Peters)
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @examples
#'
#' \dontrun{
#' ### Not run because it takes too long to run to test it,
#' ### and may produce warnings, both because of the bootstrapping
#' ### required to generate the confidence intervals in fa
#' faConfInt(psych::fa(Thurstone.33, 2, n.iter=100, n.obs=100));
#' }
#'
#' @export faConfInt
faConfInt <- function(fa) {

  if (('psych' %in% tolower(class(fa))) &&
      ('fa.ci' %in% tolower(class(fa)))) {
    ### Combine both confidence intervals and factor loadings, using
    ### the code from the 'psych:::print.psych.fa.ci' function
    lc <- data.frame(unclass(fa$loadings), fa$ci$ci);
    ### Create list for CIs per factor
    CIs <- list();
    for (i in 1:fa$factors) {
      CIs[[i]] <- lc[, c(i + fa$factors, i, i + fa$factors * 2)];
      names(CIs[[i]]) <- c('lo', 'est', 'hi');
    }
    return(CIs);
  } else {
    stop("This function can only process the resulting objects ",
         "as produced by the `fa` function in the `psych` package!)");
  }
}
