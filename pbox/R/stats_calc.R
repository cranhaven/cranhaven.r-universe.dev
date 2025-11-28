#' Calculate Basic Statistics
#'
#' Computes basic statistics such as mean and median for specified variables in a data frame or data table
#' based on a set of operations specified in the `matches` data frame. This function updates the `varSet`
#' with the computed results for each variable.
#' @name stats_calc
#' @export
#' @param data A data frame or data table containing the data to analyze.
#' @param matches A data frame specifying the variables and operations to apply.
#'   This data frame should have columns `Operator` and `Varnames2`, where `Operator` is the operation
#'   (either "mean" or "median") and `Varnames2` is a comma-separated list of variable names to which the
#'   operation will be applied.
#' @param varSet A data frame that contains variable names that may be updated with the results of the calculations.
#' @return Returns a modified version of `varSet` with updated values based on the calculations.
#' @importFrom data.table as.data.table
#' @importFrom stats median
#' @import methods
setGeneric("stats_calc",
           def = function(data, matches, varSet) {
             standardGeneric("stats_calc")
           })

#' @rdname stats_calc
#' @description Method implementation for calculating statistics using 'data.table' and 'stats'.
#' This method allows the computation of mean and median for subsets of data defined
#' in `matches` and updates `varSet` with these results.
#' @param data A data frame or data table.
#' @param matches A data frame describing the operations to apply.
#' @param varSet A data frame to be updated with results.
#'

setMethod("stats_calc",
          definition=function(data, matches,varSet) {
  for (i in 1:nrow(matches)) {
    operator <- matches$Operator[i]
    varnames <- unlist(strsplit(matches$Varnames2[i], ","))

    if (operator == "mean") {
      result <- colMeans(data[, ..varnames])
    } else if (operator == "median") {
      result <- apply(data[,..varnames], 2, median)
    } else {
      stop("Unsupported operator. Only 'mean' and 'median' are supported.")
    }
    result<-as.data.table(result,keep.rownames ="Varnames")
    # Replace the matching values in the dataframe with the calculated result
    varSet[match(result$Varnames, varSet$Varnames),]<- result
  }

  return(varSet)
})
