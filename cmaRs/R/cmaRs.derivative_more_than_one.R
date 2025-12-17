#' @importFrom stats D
cmaRs.derivative_more_than_one <- function(BF, DMS, i, j)
                                           # this function take derivative
                                           # of a BF with respect to
                                           # ith and then jth x and
                                           # assign it to DMS
                                           # BF: a  basis function
                                           # DMS: symbolic derivative matrix
                                           # i: ith x
# j: jth x
{
  i.chr <- as.character(i) # convert scalar to character.
  j.chr <- as.character(j) # convert scalar to character.
  # convert string to expression.
  second_part <- paste("xfirst", as.character(i.chr), sep = "")
  # convert string to expression.
  third_part <- paste("xfirst", as.character(j.chr), sep = "")
  # convert string to expression.
  first_part <- parse(text = paste(BF, sep = ""))
  # take derivative with respect to ith x.
  derv1 <- stats::D(first_part, second_part)
  derv2 <- stats::D(derv1, third_part) # take derivative with respect to jth x.
  derv1.dms <- as.expression(derv2) # convert expression
  DMS[i, j] <- as.character(derv1.dms) # assign expression as an element of DMS
  return(list(DMS = DMS, BF = BF)) # output
} # end of function derivative_more_than_one
