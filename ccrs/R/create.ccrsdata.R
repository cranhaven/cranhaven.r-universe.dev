
#' Create a dataset for CCRS
#'
#' @description Creates a dataset for CCRS from a preference data matrix.
#' @usage create.ccrsdata(X,q=q)
#' @param X An n by m categorical data matrix.
#' @param q An integer indicating the maximum rating.
#' @return Returns a list with the following elements.
#' \item{\code{Fmat}}{An n by q-1 matrix of scaled rank-ordered boundary data.}
#' \item{\code{Mmat.q1}}{A q-1 by 3+1 matrix of I-spline basis functions, evaluated at the boundaries. +1 indicates all 0 intercepts.}
#' \item{\code{Mmat.q}}{A q by 3+1 matrix of I-spline basis functions, evaluated at the midpoints between boundaries.}
#' \item{\code{X}}{An n by m categorical data matrix same as the input \code{X}.}
#' @seealso \code{\link{correct.rs}}
#' @details For the difference between Mmat.q and Mmat.q1 in the resulting list, see Section 3.2 in reference paper.
#' @references Takagishi, M., Velden, M. van de & Yadohisa, H. (2019). Clustering preference data in the presence of response style bias, to appear in British Journal of Mathematical and Statistical Psychology.
#' @export


create.ccrsdata <- function(X,q=q){

  Fmat <- convert.X2F(X,q=q)
  Mmat.list <- create.Mmat(q)

  ccrsdata.list <- list(Fmat=Fmat,Mmat.q1=Mmat.list$Mmat.q1,Mmat.q=Mmat.list$Mmat.q,X=X)
  ccrsdata.list

}
