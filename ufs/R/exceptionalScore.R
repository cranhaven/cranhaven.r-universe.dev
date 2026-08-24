#' Find exceptional scores
#'
#' This function can be used to detect exceptionally high or low scores in a
#' vector.
#'
#' Note that of course, by definition, `prob` or `2 * prob` percent of
#' the values is exceptional, so it is usually not a wise idea to remove scores
#' based on their 'exceptionalness'. Instead, use [exceptionalScores()],
#' which calls this function, to see how often participants answered
#' exceptionally, and remove them based on that.
#'
#' @param x Vector in which to detect exceptional scores.
#' @param prob Probability that a score is exceptionally positive or negative;
#' i.e. scores with a quartile lower than `prob` or higher than
#' 1-`prob` are considered exceptional (if both is `TRUE`, at least). So,
#' note that a `prob` of .025 means that if `both=TRUE`, the most
#' exceptional 5% of the values is marked as such.
#' @param both Whether to consider values exceptional if they're below
#' `prob` as well as above 1-`prob`, or whether to only consider
#' values exceptional if they're below `prob` is `prob` is < .5, or
#' above `prob` if `prob` > .5.
#' @param silent Can be used to suppress messages.
#' @param quantileCorrection By how much to correct the computed quantiles;
#' this is used because when a distribution is very right-skewed, the lowest
#' quantile is the lowest value, which is then also the mode; without
#' subtracting a correction, almost all values would be marked as
#' 'exceptional'.
#' @param quantileType The algorithm used to compute the quantiles; see
#' [stats::quantile()].
#'
#' @return A logical vector, indicating for each value in the supplied vector
#' whether it is exceptional.
#'
#' @examples exceptionalScore(
#'   c(1,1,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,7,8,20),
#'   prob=.05
#' );
#'
#' @export
exceptionalScore <- function(x, prob=.025, both=TRUE, silent=FALSE,
                             quantileCorrection = .0001, quantileType = 8) {

  belowLower <-
    x < (stats::quantile(x, probs=min(c(prob, 1-prob)),
                         na.rm=TRUE, type=quantileType) - quantileCorrection);
  aboveUpper <-
    x > (stats::quantile(x, probs=max(c(prob, 1-prob)),
                         na.rm=TRUE, type=quantileType) + quantileCorrection);
  if (both) {
    return(belowLower | aboveUpper);
  } else {
    if (prob < .5) {
      return(belowLower);
    } else {
      return(aboveUpper);
    }
  }
}
