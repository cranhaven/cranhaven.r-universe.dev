#########1#########2#########3#########4#########5#########6#########7#########8
#' Find Outliers
#'
#' Find the outliers in a vector of values.
#'
#' @param x vector
#' @return A list with a variable \code{idx} containing the indices of the
#' outliers and a variable \code{values} containing the values of the outliers.
#' @examples
#' x=c(100,30:40,101,25:28)
#' outliers(x)
#' @export
################################################################################
outliers<-function(x) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  idx=which(x<=stats::quantile(x,.25,na.rm=TRUE)-1.5*stats::IQR(x,na.rm=TRUE))
  idx=c(idx,which(x>=stats::quantile(x,.75,na.rm=TRUE)+
                    1.5*stats::IQR(x,na.rm=TRUE)))
  list(idx=idx,values=x[idx])
}
