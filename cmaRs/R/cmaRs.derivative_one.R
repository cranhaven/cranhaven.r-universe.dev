#' @importFrom stats D
cmaRs.derivative_one <- function(BF, DMS, i, b1)
                                 # this function take derivative
                                 # of a BF with respect to
                                 # ith x and assign it to DMS
                                 # BF: a  basis function
                                 # DMS: symbolic derivative matrix
                                 # i: ith x
# b1: any BF
{
  i.chr <- as.character(i)
  # return the parsed but unevaluated expressions in a list
  dd.expr <- parse(text = paste(b1, sep = ""))
  # return the parsed but unevaluated expressions in a list
  first_part <- parse(text = paste(BF, sep = ""))
  # convert string to expression.
  second_part <- paste("xfirst", i.chr, sep = "")
  derv1 <- stats::D(first_part, second_part) # take derivative
  derv1.dms <- as.expression(derv1) # convert expression
  # assign expression as an element of DMS
  DMS[i, i] <- as.character(derv1.dms)
  return(list(DMS = DMS, BF = BF, b1 = b1)) # output
} # end of function derivative_one
