#' List to Data.frame
#'
#' Convert a list of vectors, of varying length, to a data frame. Elements are NA where lengths of vectors are smaller than the length of the largest vector.
#'
#' @param lst this is a list returned from string split
#' @export
list.to.df <-
function(lst # this is a list returned from string split
                                  )
{
  lst.df <- NULL
  if (sum(unlist(lapply(lst, FUN=function(x) {length(x)}))) > 0)
  {
    mx <- max(unlist(lapply(lst, FUN=function(x) {length(x)})))
    for (i in 1:length(lst))
    {
     lst[[i]] <- c(lst[[i]], rep(NA, mx-length(lst[[i]])))
    }
    lst.df <- t(as.data.frame(lst, stringsAsFactors =F))
    colnames(lst.df) <- paste("C", 1:ncol(lst.df), sep="")
    rownames(lst.df) <- paste("R", 1:nrow(lst.df), sep="")
  }
  lst.df
}

