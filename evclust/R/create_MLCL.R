#' Random generation of Must-Link and Cannot-Link constraints
#'
#' \code{create_MLCL} randomly generates Must-Link (ML) and Cannot-Link (CL) constraints from a vector y
#' of class labels.
#'
#' @param y Vector of class labels.
#' @param nbConst Number of constraints.
#'
#' @return A list with two components:
#' \describe{
#'   \item{ML}{Matrix of ML constraints. Each row corresponds to a constraint.}
#'   \item{CL}{Matrix of ML constraints. Each row corresponds to a constraint.}
#' }
#'
#' @export
#' @import R.utils
#' @importFrom utils combn
#'
#' @seealso \code{\link{cecm}}
#'
#' @examples
#' y<-sample(3,100,replace=TRUE)
#' const<-create_MLCL(y,nbConst=10)
#' const$ML
#' const$CL
#'
create_MLCL<-function(y,nbConst){
  n=length(y)
  ## Random selection of the must-link and cannot link constraints
  pairs<-combn(1:n,2)
  N<-ncol(pairs)
  const<-pairs[,sample(N,nbConst)]
  ML=t(const[,y[const[1,]]==y[const[2,]]])
  CL=t(const[,y[const[1,]]!=y[const[2,]]])
  return(list(ML=ML,CL=CL))
}
