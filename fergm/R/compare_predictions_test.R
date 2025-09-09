#' KS Test for Difference in Predictive Performance
#'
#' This is a plot function to determine whether the distributions of predictions from ERGM and FERGM objects differ.  It does so by using a Kolmogorov-Smirnov Test.
#' @param compare_predictions_out Matrix of correctly predicted ties produced by the \code{compare_predictions} function.
#' @param alpha_level The significance level that should be used for the test.
#' @keywords Fit GOF Prediction KS Kolmogorov-Smirnov Test
#' @return Returns \code{ks.test} output to determine if the percent of correctly predicted ERGM ties are less than those of the FERGM and prints a message to assist with interpretation
#' @examples
#' # load example data
#' data("ergm.fit")
#' data("fergm.fit")
#' data("mesa")
#' # Use built in compare_predictions function to compare predictions of ERGM and FERGM,
#' # few replications due to example
#'  net <- ergm.fit$network
#' predict_out <- compare_predictions(ergm.fit = ergm.fit, fergm.fit = fergm.fit,
#'                                    replications = 10, seed = 12345)
#'
#' # We can also conduct a KS test to determine if the FERGM fit it statistically
#'  # distinguishable from the ERGM fit
#' compare_predictions_test(predict_out)
#' @export

compare_predictions_test <- function(compare_predictions_out = NULL, alpha_level = 0.05){
  ergm_predictions <- compare_predictions_out[,1]
  fergm_predictions <- compare_predictions_out[,2]

  # less than alpha means you reject the null that the empirical distribution for FERGM
    # predictions was drawn from the reference distribution of ERGM predictions
  ks.test.out <- stats::ks.test(x = ergm_predictions, y= fergm_predictions, alternative = "greater")

  if(ks.test.out$p.value < alpha_level){
    cat(paste("A two-sample Kolmogorov-Smirnov Test returns a p-value of ", ks.test.out$p.value,
        " which is less than the set alpha level of ", alpha_level,
        ".  This means that the null hypothesis that the cumulative distribution function of ERGM predictions",
        " is not greater than, or to the left of, that of the FERGM, is rejected.  The alternative hypothesis ",
        "that the cumulative distribution of ERGM predictions lies below that of the FERGM is accepted. ",
        "This is interpreted as meaning that in comparing these samples, the FERGM predicts ties better than the ERGM.", sep = ""))
  }

  if(!ks.test.out$p.value < alpha_level){
    cat(paste("A two-sample Kolmogorov-Smirnov Test returns a p-value of ", ks.test.out$p.value,
              " which is not less than the set alpha level of ", alpha_level,
              ".  This means that the null hypothesis that the cumulative distribution function of ERGM predictions",
              " is not greater than, or to the left of, that of the FERGM, is held as true  The alternative hypothesis ",
              "that the cumulative distribution of ERGM predictions lies below that of the FERGM is rejected ",
              "This is interpreted as meaning that in comparing these samples, there is not evidence that the FERGM outperforms the ERGM.", sep = ""))
  }

  ks.test.out$data.name <- c("ERGM and FERGM percent of correctly predicted ties inhereted from the output of the compare_predictions function")

  return(ks.test.out)


}

