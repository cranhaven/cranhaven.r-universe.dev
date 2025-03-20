#' @title \code{TwoPart}
#'
#' @description \code{TwoPart} conducts a 2-part Wilcoxon test.
#'
#'
#' @param data is a vector containing the values for the 2-part test
#' @param group is a vector containing the two groups for comparison (in binary, 0,1)
#' @param test which type of test to use, "t.test" or "wilcoxon"
#' @param point.mass point mass
#'
#' @return ans is a list containing the test statistic & p-value corresponding to the
#'         two-part Wilcoxon test.
#'
#' @details The code from the two part test is taken from Taylor & pollard (2009) (reference below) which illustrates how to create a composite test from zero-inflated data.
#'
#' @references Taylor, Sandra, and Katherine Pollard. "Hypothesis tests for point-mass mixture data #' with application toomics data with many zero values." Statistical applications in genetics and #' molecular biology 8.1 (2009).
#'
#' @noRd

# Function for calculating two-part statistics
TwoPart <- function(data, group, test = "wilcoxon", point.mass = 0) {
  Index1 <- c(group == 1)
  Group1 <- data[Index1]
  Group0 <- data[!Index1]
  n1 <- length(Group1)
  n2 <- length(Group0)
  obs <- c(n1, n2)
  success <- c(sum(Group1 != point.mass), sum(Group0 != point.mass))
  pointmass <- obs - success

  if (sum(success) == 0) {
    T2 <- 0
    B2 <- 0
  } else if ((success[1] == 0) | (success[2] == 0)) {
    T2 <- 0
    B2 <- stats::prop.test(pointmass, obs)$statistic
  } else if ((success[1] == 1) | (success[2] == 1)) {
    T2 <- 0
    B2 <- stats::prop.test(pointmass, obs)$statistic
  } else {
    uniq1 <- length(unique(Group1[Group1 != point.mass]))
    uniq2 <- length(unique(Group0[Group0 != point.mass]))
    if ((uniq1 < 2) & (uniq2 < 2)) {
      T2 <- 0
      if (sum(pointmass) == 0) {
        B2 <- 0
      } else {
        B2 <- stats::prop.test(pointmass, obs)$statistic
      }
    } else if (sum(pointmass) == 0) {
      B2 <- 0
      if (test == "t.test") {
        T2 <- stats::t.test(data ~ group)$statistic^2
      }
      if (test == "wilcoxon") {
        W <- stats::wilcox.test(data ~ group, exact = FALSE)$statistic
        mu <- (n1 * n2) / 2
        sigma <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
        T2 <- ((abs(W - mu) - 0.5) / sigma)^2
      }
    } else {
      B2 <- stats::prop.test(pointmass, obs)$statistic
      contIndex <- data != point.mass
      cont <- data[contIndex]
      cGroup <- group[contIndex]
      n1c <- sum(cGroup == 1)
      n2c <- sum(cGroup == 0)

      if (test == "t.test") {
        T2 <- stats::t.test(cont ~ cGroup)$statistic^2
      }
      if (test == "wilcoxon") {
        W <- stats::wilcox.test(cont ~ cGroup, exact = FALSE)$statistic
        mu <- (n1c * n2c) / 2
        sigma <- sqrt((n1c * n2c * (n1c + n2c + 1)) / 12)
        T2 <- ((abs(W - mu) - 0.5) / sigma)^2
      }
    }
  }
  X2 <- B2 + T2
  if ((T2 == 0) | (B2 == 0)) {
    X2pv <- 1 - stats::pchisq(X2, 1)
  } else {
    X2pv <- 1 - stats::pchisq(X2, 2)
  }
  ans <- list(statistic = X2, pvalue = X2pv)
  return(ans)
}
