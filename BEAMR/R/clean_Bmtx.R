#' Clean up bootstrap coefficient matrix
#'
#' @param B Matrix of bootstrap coefficients
#'
#' @returns Matrix of cleaned bootstrap coefficients
#' @export
#'
#' @examples
#'data(beam_stats)
#'B.mtx <- beam_stats$beam.stats[[1]]
#'B.cln <- clean_Bmtx(B.mtx)
clean_Bmtx=function(B)
{
  m=nrow(B)
  res=apply(B,1,replace_value)
  res=t(res)
}

############################
# replace values

replace_value=function(x)
{
  inf=!is.finite(x)
  na=is.na(x)
  mn=mean(x[na&!inf])
  x[na]=mn
  x[inf]=sign(x[inf])*max(abs(x[!inf]))
  return(x)
}
