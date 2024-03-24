#' Conditional Structure
#'
#' For variables defined as conditional variables in the formula, fuction to drive creating a list of simple table structures
#' 
#' @param data data.frame
#' @param byvars variables in addition to the group and label variables listed in the formula
#' @export
conditional.struct <-
function(data, byvars)
  {
    cnms <- colnames(data)[!colnames(data) %in% byvars]
    if (length(byvars)==1)
      { concat.cond <- data[,byvars] # paste(rep(byvars, ncol(data)), data[,byvars], sep="=")
        }
    else
      {
        l <- vector("list", length(byvars))
        for (byvars.i in 1:length(byvars))
          {
            l[[byvars.i]] <- paste(rep(byvars[byvars.i], ncol(data)), data[, byvars[byvars.i]], sep="=")
          }

          concat.cond <- apply(data.frame(l), 1, paste, collapse=", ")
      }
    uniq.concat.cond <- unique(concat.cond)
    data <- data[, -which(colnames(data) %in% byvars)]
    if (!is.data.frame(data))
      {data <- as.data.frame(data); colnames(data) <- cnms; }
   return(list(concat.cond=concat.cond, uniq.concat.cond=uniq.concat.cond, data=data))
  }

