cmaRs.identical_function <- function(DMS, i, j, number, xfirst_data, n)
                                     # this function prepare DMS for evaluation.
                                     # DMS: symbolic derivative matrix
                                     # i:row index
                                     # j: column index  number
                                     # number: the number that is tested
                                     # xfirst_data: the data that includes
                                     # xfirst variables
# n: sample size
{
  if (identical(with(
    xfirst_data,
    eval(parse(text = DMS[i, j]))
  ), number)) {
    istr <- as.character(number)
    DMS[i, j] <- paste("matrix", "(",
      istr, ", nrow <- (n+1), ncol <- 1)",
      sep = ""
    )
  }
  return(DMS = DMS) # output
} # end of function identical_function
