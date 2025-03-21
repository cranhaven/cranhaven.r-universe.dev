#  In makeJournalTables: Don't export.
#   Formatting Confidence Intervals
#
#   @family format
#   @keywords format
#
#   @description  \code{formatCI} formats a mean and a confidence interval as  mean ( lower, upper)
#   #               like  1.25 (1.10, 1.34)
#   @details  x can be a data frame, numeric, or character matrix with any number of rows and 3 columns
#   #          consisting of the mean, the lower bound of the confidence interval, and the upper bound#          of the confidence interval IN ORDER. I printed a warning that this is assumed.
#   @note This function can be used in conjunction with aggregate or apply, provided that 3 columns are
#   #      passed to \code{formatCI}
#   @param x Matrix with 3 columns in order: mean, lower, upper.
#   @param digits Number of significant digits; passed to \code{\link[base]{formatC}}.
#   @param ... Additional parameters passed to \code{\link[base]{formatC}}.
#   @return A dataframe consisting of formatted confidence interval: mean ( lower, upper) like  1.25 (1.10, 1.34).
#
#   @examples
#   \dontrun{
#   data("CO2")
#   calculate.tCI <- function(x){  #x is a numeric vector...
#   #   n <- length(x)#   width <- qt(0.975, n-1)*sd(x)/sqrt(n)#   A <- mean(x) + cbind(0, -width, width);#   colnames(A) <- NULL#   return(A)}
#   #
#   #Example 1
#   v <- calculate.tCI(CO2$conc)
#   v
#   #     [,1]     [,2]     [,3]
#   # [1,]  435 370.7805 499.2195
#   formatCI(v)
#   #                   CI.lab
#   # 1 435.00 (370.78, 499.22)
#   formatCI(v, digits = 3, mode = "integer")
#   #v must have 3 columns: The mean, lower limit and upper limit.
#   #If it doesn't, the function will give an error.
#   #\donttest{ formatCI(v[ , 1:2]) }
#
#   #Example 2: Using aggregate
#   v <- aggregate( conc ~ Type, data = CO2, calculate.tCI)
#   v
#   cbind( Type = v$Type, formatCI(v[ ,"conc"]) )
#   # This can be copied or exported to a csv or text file using
#   # the write.table or write.csv functions.
#   }

formatCI <- function(x, digits = 2,  ...) {
  v <- x  # let v be a matrix, not a vector.

  # Warnings and Checks
  warning("Matrix with columns mean, lower, upper in order.")
  if (is.character(v) | is.numeric(v)) {  v <- as.matrix(v)}
  stopifnot(ncol(v) == 3)

  # Format the mean(m), lower(l), and upper(u); then combine in one label.
  m <-  formatC (v[, 1],  format = "f", digits = digits, ...)
  l <-  formatC (v[, 2],  format = "f", digits = digits, ...)
  u <-  formatC (v[, 3],  format = "f", digits = digits, ...)
  d <- data.frame(CI.lab = paste0(m,  " (", l, ", ", u, ")"))

  # Reassign rownames of v to output.
  if (!is.null(dimnames(v)[[1]])) {
    dimnames(d)[[1]] <- dimnames(v)[[1]]
  }
  return(d)
}

# Although formatCI() is not a realization for format(), the parameter "x" needs to be used instead of "v"
# so that this function is consistent with format().
