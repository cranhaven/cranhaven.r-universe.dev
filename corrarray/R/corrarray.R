#' @title Correlation Arrays and 2-Sample Correlation Matrices
#'
#' @description This function creates a multi-sample correlation array
#'   combining the correlation matrices for each level of the specified
#'   grouping variable.
#'   Given two levels of the grouping variable, this function creates a single
#'   correlation matrix displaying the individual triangular matrices on
#'   opposite sides of the principal diagonal.
#'
#' @usage corrarray(x, group = NULL, lower = NULL, upper = NULL,
#'   output = c("matrix", "array", "sig.matrix", "sig.array"),
#'   use = c("complete.obs", "everything", "all.obs",
#'   "na.or.complete", "pairwise.complete.obs"),
#'   method = c("pearson", "kendall", "spearman"))
#'
#' @param x a matrix or data frame. Variables can be quantitative or categorical.
#'
#' @param group the grouping variable name. If no group is specified (default),
#'   then a single correlation matrix for the entire sample will be generated.
#'
#' @param lower the level of the grouping variable to be placed in the
#'   lower triangular matrix. If no level is specified (default), then
#'   the first level in the data set is treated as the lower level.
#'
#' @param upper the level of the grouping variable to be placed in the
#'   upper triangular matrix. If no level is specified (default), then
#'   the second level in the data set is treated as the lower level.
#'
#' @param output If a group that has 2 or more levels is specified,
#'   then "\code{matrix}" (default) returns the corresponding 2-sample
#'   correlation matrix, and "\code{array}" returns the
#'   correlation array. "\code{sig.matrix}" and "\code{sig.array}" return
#'   the significance values of the corresponding correlations.
#'
#' @param use an optional character string giving a method for
#'   computing correlations in the presence of missing values.
#'   This must be one of the strings "\code{complete.obs}" (default),
#'   "\code{everything}", "\code{all.obs}", "\code{na.or.complete}",
#'   or "\code{pairwise.complete.obs}".
#'   The default option removes rows with missing values from calculations.
#'
#' @param method a character string indicating which correlation coefficient
#'  is to be computed: "\code{pearson}" (default), "\code{kendall}",
#'  or "\code{spearman}".
#'
#' @details If multiple values are provided for \code{group},
#'   \code{lower}, or \code{upper}, then only the first value is used.
#'   Apart from the grouping variable, all other variables whose values are
#'   not numeric are removed from the correlation matrices. The grouping
#'   variable's values, even if numeric, are automatically treated as
#'   different levels.
#'
#' @return \code{corrarray} returns an array or matrix of correlations
#'   as numeric values from \code{-1} to \code{1} (inclusive),
#'   or of significance values of the corresponding correlations,
#'   and with row and column names as the variable names.
#'
#' @export
#'
#' @seealso \code{\link{cor}} for further descriptions of the \code{use}
#'   and \code{method} parameters, and \code{\link{rcorr}} for significance
#'   tests of correlations.
#'
#' @importFrom stats cor
#'
#' @importFrom Hmisc rcorr
#'
#' @examples
#' ## All observations: 1-sample correlation matrix.
#' corrarray(iris)
#'
#' ## Stratify by the three species: 3-sample correlation array.
#' corrarray(iris, "Species", output = "array")
#'
#' ## Specify lower and upper samples: 2-sample correlation matrix.
#' corrarray(iris, "Species", lower = "setosa", upper = "virginica")
#'
corrarray <- function(x, group = NULL, lower = NULL, upper = NULL,
                      output = c("matrix", "array", "sig.matrix", "sig.array"),
                      use = c("complete.obs", "everything", "all.obs",
                              "na.or.complete", "pairwise.complete.obs"),
                      method = c("pearson", "kendall", "spearman")){

  # Check multi-option input parameters.
  output <- match.arg(output)
  use <- match.arg(use)
  method <- match.arg(method)

  # If group = NULL and output = "matrix", return 1-sample correlation matrix.
  if (is.null(group) && output == "matrix") {
    # Remove columns that are not numeric.
    new.x <- x[,unlist(lapply(x, FUN = is.numeric))]
    return(cor(new.x, use = use, method = method))
  }

  # If group = NULL and output = "sig.matrix", return 1-sample sig.matrix.
  if (is.null(group) && output == "sig.matrix") {
    # Remove columns that are not numeric.
    new.x <- x[,unlist(lapply(x, FUN = is.numeric))]
    return(rcorr(as.matrix(new.x), type = method)$P)
  }

  # If multiple elements are specified, choose only first element.
  group <- group[1]
  lower <- lower[1]
  upper <- upper[1]

  # Identify whether group is a valid column name.
  if (!(group %in% names(x))) {
    stop(paste("Grouping variable '", group,
               "' is not found in data set.", sep = ""))
  }

  # If group is not referring to a factor variable, coerce it to factor.
  new.x <- x
  if (!is.factor(x[group])) {
    new.x[group] <- lapply(x[group] , as.factor)
  }

  # Remove columns that are not numeric vectors, keeping the grouping variable.
  new.x <- x[, (unlist(lapply(x, FUN = is.numeric)) | names(x) == group)]

  # Determine the number of groups and variables.
  grplevels <- unique(new.x[, group])
  ngroups <- length(grplevels)
  nvar <- ncol(new.x) - 1

  # Create a list of vectors with observation row numbers for each group.
  obs_groups <- list()
  for (i in 1:ngroups) {
    obs_groups[[i]] <- which(new.x[, group] == grplevels[i])
  }

  # Create a modified data frame with grouping variable column removed.
  new.x <- new.x[, names(new.x) != group]

  # Create a correlation and significance array based on number of groups.
  corr.array <- array(dim = c(nvar, nvar, ngroups),
                      dimnames = list(names(new.x), names(new.x),
                                      Sample = grplevels))

  if (any(output == c("sig.matrix", "sig.array"))) {
    sig.array <- array(dim = c(nvar, nvar, ngroups),
                       dimnames = list(names(new.x), names(new.x),
                                       Sample = grplevels))
  }

  # Assign correlations and significance values to array for each group.
  for (i in 1:ngroups) {
    corr.array[,,i] <- cor(new.x[obs_groups[[i]],], use = use,
                           method = method)
    if (any(output == c("sig.matrix", "sig.array"))) {
      sig.array[,,i] <- rcorr(as.matrix(new.x[obs_groups[[i]],]),
                              type = method)$P
    }

  }

  # Output array if output = "array".
  if (output == "array") {
    return(corr.array)
  }

  # Output significance array if output = "sig.array".
  if (output == "sig.array") {
    return(sig.array)
  }

  # Compute number of correlation combinations using formula 'ncor'='nvar'C2.
  ncor <- (factorial(nvar))/(2*factorial(nvar-2))

  # For 2-sample correlation matrix, define specified lower and upper groups.
  if (!is.null(lower) || !is.null(upper)) {
    if (!is.null(lower) && !(lower %in% grplevels)) {
      stop("Name of sample entered into 'lower' is not found in data set.")
    }

    else if (!is.null(upper) && !(upper %in% grplevels)) {
      stop("Name of sample entered into 'upper' is not found in data set.")
    }

    else if (!is.null(lower) && lower == upper) {
      stop("Names of samples entered into 'lower' and 'upper' are the same.")
    }

    else {
      lower <- which(grplevels == lower)
      upper <- which(grplevels == upper)
    }
  }

  else if (is.null(lower) && is.null(upper)) {
    lower <- 1
    upper <- 2
  }

  # Create matrices of proper dimensions. ####
  M <- matrix(data = NA, nrow = nvar, ncol = nvar,
              dimnames = list(Sample1=names(new.x),
                              Sample2=names(new.x)))

  sig.M <- matrix(data = NA, nrow = nvar, ncol = nvar,
              dimnames = list(Sample1=names(new.x),
                              Sample2=names(new.x)))


  # Comment with the factor level referred to by the sample numbers.
  comment(M) <- c(paste("Sample1 (lower triangular matrix) is '",
                        grplevels[lower], "' (n=",
                        sum(x[, group] == grplevels[lower]), ").",
                        sep = ""),
                  paste("Sample2 (upper triangular matrix) is '",
                        grplevels[upper], "' (n=",
                        sum(x[, group] == grplevels[upper]), ").",
                        sep = ""))

  comment(sig.M) <- c(paste("Sample1 (lower triangular matrix) is '",
                        grplevels[lower], "' (n=",
                        sum(x[, group] == grplevels[lower]), ").",
                        sep = ""),
                  paste("Sample2 (upper triangular matrix) is '",
                        grplevels[upper], "' (n=",
                        sum(x[, group] == grplevels[upper]), ").",
                        sep = ""))

  # Fill in correlation matrix with corresponding correlations.
  for (i in 1:nvar) {
    for (j in 1:nvar) {
      if (j <= i) {
        M[i,j] <- corr.array[i,j,lower]
        if (any(output == c("sig.matrix", "sig.array"))) {
          sig.M[i,j] <- sig.array[i,j,lower]
        }
      }
      else if (j > i) {
        M[i,j] <- corr.array[i,j,upper]
        if (any(output == c("sig.matrix", "sig.array"))) {
          sig.M[i,j] <- sig.array[i,j,upper]
        }
      }
    }
  }

  # Output matrix and show the name of each sample if output is "matrix".
  if(output == "matrix") {
    print(attr(x = M, which = "comment"))
    return(M)
  }

  # Output significance matrix if output is "sig.matrix".
  if(output == "sig.matrix") {
    print(attr(x = sig.M, which = "comment"))
    return(sig.M)
  }

}
