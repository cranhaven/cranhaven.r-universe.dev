#' Local minimum and maximum of a real-valued continuous function over an open interval
#' 
#' Gets local minimum and maximum of a given function expression
#' on an interval using basic calculus criteria
#' 
#' @param     f function expression
#' @param f1der function expression of first derivative of \code{f}
#' @param f2der function expression of second derivative of \code{f}
#' @param  what character. What to look for? A local \code{min} or a \code{max}?
#' @param    x0 numeric givin global minimum or maximum of \code{f} over the
#'              the interval \code{D}.
#' @param     D numeric vector specifying the interval over which \code{f}
#'              is optimized
#'
#' @export
#' 
#' @details This function looks for critical values
#' over the interval \code{[D[1],x0-1)} \eqn{\cup} \code{(x0+1, D[length(D)]]}.
#'
#' 
#' @importFrom rootSolve uniroot.all
#' 
#' @return A list containing:
#' \itemize{
#'    \item \code{x_opt} numeric giving the critical point where the local min or max
#'        is achieved. When local min or max cannot be determined, this function returns \code{NA}.
#'    \item \code{locals} numeric vector giving all critical points satisfying second derivative criteria.
#'    \item \code{crtPts} a list with 2 entries:
#'    \itemize{
#'        \item \code{x_d1} numeric vector with local critical points over \code{[D[1],x-1)}
#'        \item \code{x_d2} numeric vector with local critical points over \code{(x0+1,D[length(D)]]}
#'    }
#'    \item \code{type} character, what was found? A \code{min} or a \code{max}?
#' }
#' 
#' @seealso \code{\link{global_min_max}}, \code{\link{phenopar}}
#' 
local_min_max <- function(f, f1der, f2der, what=c("min", "max"), x0, D){
  if(x0 < D[1] | x0 > D[length(D)]){
    stop("x0 must satisfy that D[1] < x0 < D[length(D)]")
  }
  
  what <- match.arg(what)
  
  x0 <- ifelse(x0%%1 < 0.5, floor(x0), ceiling(x0))
  
  x_int1 <- uniroot.all(f=f1der, interval = c(D[1], x0-1))
  x_int2 <- uniroot.all(f=f1der, interval = c(x0+1, D[length(D)]))
  
  if(what == "min"){
    x_int1_valid <- which(f2der(x_int1)>0)
    x_int2_valid <- which(f2der(x_int2)>0)
    
    valid_x_int1 <- x_int1[x_int1_valid[which.min(f(x_int1[x_int1_valid]))]]
    valid_x_int2 <- x_int2[x_int2_valid[which.min(f(x_int2[x_int2_valid]))]]
    
    if( (length(valid_x_int1) != 0) & (length(valid_x_int2) != 0) ){
      
      xopt <- c(valid_x_int1, valid_x_int2)[which.min(c(f(valid_x_int1), f(valid_x_int2)))]
      
    } else {
      if( length(valid_x_int1) == 0 ){
        xopt <- ifelse( f(valid_x_int2) > f(x0), valid_x_int2, NA )
      } else {
        xopt <- ifelse( f(valid_x_int1) > f(x0), valid_x_int1, NA )
      }
    }
    
  } else {
    x_int1_valid <- which(f2der(x_int1)<0)
    x_int2_valid <- which(f2der(x_int2)<0)
    
    valid_x_int1 <- x_int1[x_int1_valid[which.max(f(x_int1[x_int1_valid]))]]
    valid_x_int2 <- x_int2[x_int2_valid[which.max(f(x_int2[x_int2_valid]))]]
    
    if( (length(valid_x_int1) != 0) & (length(valid_x_int2) != 0) ){
      
      xopt <- c(valid_x_int1, valid_x_int2)[which.max(c(f(valid_x_int1), f(valid_x_int2)))]
      
    } else {
      if( length(valid_x_int1) == 0 ){
        xopt <- ifelse( f(valid_x_int2) < f(x0), valid_x_int2, NA )
      } else {
        xopt <- ifelse( f(valid_x_int1) < f(x0), valid_x_int1, NA )
      }
    }
    
  }
  
  list(x_opt=xopt, locals=c(valid_x_int1, valid_x_int2),
       crtPts=list(x_d1=x_int1[x_int1_valid], x_d2=x_int2[x_int2_valid]),
       type=what)
  
}
