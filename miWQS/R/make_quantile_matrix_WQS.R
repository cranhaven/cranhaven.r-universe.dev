#' Making Quantiles of Correlated Index
# (Added 8/22/17)
#'
#' @family wqs
#' @seealso \code{\link[stats]{quantile}}
#' @keywords wqs

#' @description Scores quantiles from a numeric matrix. If the matrix has values missing between zero and some threshold, say the detection limit, all these missing values  (indicated by NA) are placed into the first quantile.
#'
#' @details
#' Produces sample quantiles for a matrix \emph{X} using \code{\link[stats]{quantile}}() function. Names are kept and the 7th quantile algorithm is used. As ties between quantiles may exist, \code{\link[base]{.bincode}}() is used.
#'
#' When there is missing data (as indicated by NA's), \code{make.quantile.matrix} places all of the censored data into the first quantile. The remaining quantiles are evenly spread over the observed data. A printed message is displaced what the function does.
#'
#' @note
#' Developed as an accessory function for \code{estimate.wqs()}.
#'
#' @param X       A numeric matrix. Any missing values are indicated by NA's.
#' @inheritParams estimate.wqs
# n.quantiles, verbose: passed from estimate.wqs
#' @inheritParams stats::quantile
# ...
# #' @param ... Further arguments passed from other methods to stats::quantile().

#' @return A matrix of quantiles with rows = nrow(X) and with columns = n.quantiles.

#' @examples
#' # Example 1: Make quantiles for first nine chemicals using complete chemical data
#' data(simdata87)
#' q <- make.quantile.matrix(simdata87$X.true[, 1:9], 4)
#' q <- apply(q, 2, as.factor)
#' summary(q)
#'
#' # Example 2: Place missing values of first nine chemicals in first quantiles
#' q2 <- make.quantile.matrix(simdata87$X.bdl[, 1:9], 4, verbose = TRUE)
#' summary(q2)
#' @import stats
#' @export

make.quantile.matrix <- function(
  X,
  n.quantiles,
  place.bdls.in.Q1 = if (anyNA(X)) TRUE else FALSE,
  ...,
  verbose = FALSE
) {

  # Check: X must be numeric
  no.not.numeric <- sum(apply(X, 2, class) != "numeric")
  a <- ifelse(no.not.numeric == 0,
              "",
              stop("At least one chemical in X is not numeric")
  )
  # If (place.bdls.in.Q1 is null, any missing values in X are automatically placed into the first quantile; otherwise, if complete X values are observed, it is false.
  # if (is.null(place.bdls.in.Q1))  place.bdls.in.Q1 <- anyNA(X)

  if (!place.bdls.in.Q1) {
    message("#> No missing values in matrix detected. Regular quantiles computed.")

    # Find the quantile for each columns
    q <- matrix(-20, dim(X)[1], dim(X)[2])
    I <- dim(X)[2]
    for (i in 1:I) {
      x.break <- stats::quantile(
        X[, i],
        probs = c(0:n.quantiles / n.quantiles),
        na.rm = FALSE,
        names = TRUE,
        type = 7,
        digits = 7,  #Added in R 4.0.5
        ...
        )
       print(x.break)
      # q[, i] <- q.cut <- cut(X[, i], breaks = x.break, labels = FALSE, include.lowest = TRUE) -1
      q[, i] <- .bincode(
        X[, i],
        breaks = x.break,
        include.lowest = TRUE
        ) - 1
    }

  } else {
    message("#> All BDLs are placed in the first quantile")

    q <- matrix(-10, dim(X)[1], dim(X)[2])
    I <- dim(X)[2]
    for (i in 1:I) {
      # For those observed, divide up the quantiles - 1.
      x.break <- stats::quantile(
        X[, i],
        probs = c(0:(n.quantiles - 1) / (n.quantiles - 1)),
        na.rm = TRUE,
        names = TRUE,
        type = 7,
        digits = 7,  #Added in R 4.0.5
        ...
      )
      q[, i] <-  cut(
        X[, i],
        breaks = x.break,
        labels = FALSE,
        include.lowest = TRUE
      )

      # Let the first quantile be all the BDLs.
      q[which(is.na(q[, i])), i] <- 0
    }
  }

  # Checking the method: show only if verbose = TRUE.
  if (verbose) {
    cat("##> Summary of Quantiles \n")
    print(apply(q, 2, table))
    cat("##> Total Number of NAs--Q1 (The first row) should match.\n")
    cat(t(apply(X, 2, function(i) {sum(is.na(i)) })))
    # cat ("\n Summaries: (a) Matrix \n")
    #  print(   apply(X, 2, my.summary ) )
    # cat("\n Summaries: (a) Quantiles \n")
    #  print(   apply(q, 2, my.summary ) )
  }

  # Return the quantiles.
  return(q)
}


# Log: make.quantile.bdl
# Modified 8 /14 /17: Added error check to make it easier to read.
# Modified 8/ 22/17: Added conventional practice of placing all the BDLs in the first quantile.
# Modified 9/3/17: Add example and copied to WQS_Analysis#Add this parameter to general WQS function.
# place.bdls.in.Q1 #logical. If TRUE, uses the default techinque of placing the BDLs in the first quantile.
# Modified 10/16/18: Changed name to be unique and not related to S3 generic quantile. Added check to make sure that numeric is used.
