#  In makeJournalTables: Don't export.
# #'@title Formatting Means and MSE
# #'@family format
# #'@keywords format
# #'
# #'@description  \code{formatMSE} formats output as mean ( MSE ) like 6 (8).
# #'
# #'@inheritParams
# #'
# #'@inherit formatMeanSd
# #'
# #'@examples
# #'\dontrun{
# #' #Uses the mtcars dataset from dataset package
# #' data( datasets::mtcars)
# #'
# #' #Using formatMSE: If MSE is in second column.
# #' x.sum <- mean(mtcars$mpg)
# #' MSE <- mean ( (mtcars$mpg - 23)^2 )  #Say the true mpg is 23
# #' cbind(x.sum, MSE)
# #' formatMSE( cbind(x.sum, MSE), digits = 1)
# #' formatMSE( cbind(x.sum, MSE), digits = 2, nsmall = 1 )
# #' }


# #' In makeJournalTables: Don't export.

formatMSE <-   function(x, digits = 2, ...) {
  # Warnings and Checks
  v <- x
  print("Mean (MSE) ")
  if (is.character(v) | is.numeric(v)) {  v <- as.matrix(v)}
  warning("Mean is assumed to be in 1st column; MSE in second.")
  stopifnot(ncol(v) == 2)

  # Format mean and mse, collapse into single line.
  v.look <- format(v, trim = TRUE, digits = digits, drop0trailing = FALSE,  ...)
  v.formatted <- rep(NA, nrow(v))
  names(v.formatted) <- rownames(v)
  for (j in 1:nrow(v)) {
    v.formatted[j]   <-  paste0(v.look[j, 1], " (", v.look[j, 2], ")")
  }
  return(as.matrix(v.formatted))
}

# Although format.mean.mse() is not a realization for format(), the parameter "x" needs to be used instead of "v" so that
# this function aligns with format(). Additionally, the name of the function had to be changed to
# formatMSE to distinguish from S3 generic format().
