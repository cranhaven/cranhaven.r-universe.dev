#' Summary of meta-analysis with replicability-analysis results
#' 
#' @description Summary method for objects of class 'metarep'.
#'
#' @param  object An object of class 'metarep'.
#' @param ... Arguments to be passed to methods, see \code{summary.meta}
#' @return A list of the quantities for replicability analysis, as follows: 
#'   \item{meta-analysis results: }{Summary of the supplied 'meta' object.}
#'   \item{r.value: }{r-value of the tested alternative.}
#'   \item{u.increased: }{Maximal number of studies at which replicability of increasing effect can be claimed. It will be reported unless the alternative is 'less'.}
#'   \item{u.decreased: }{Maximal number of studies at which replicability of increasing effect can be claimed. It will be reported unless the alternative is 'greater'.}
#' 
#' @export
#'
#' @examples
#' n.i.1 <- c( 20, 208, 24, 190, 58, 36, 51)
#' a.i <- c( 2,79,0,98,15,34,9) 
#' n.i.2 <- c( 20, 119, 22, 185, 29, 51, 47)
#' c.i <- c(9,106,14,98,12,49,9) 
#' m1 <- metabin( event.e = a.i,n.e = n.i.1,event.c = c.i,n.c = n.i.2,
#'                studlab = paste0('Study ' , 1:7) , sm = 'OR' ,
#'                common = FALSE, random = TRUE )
#' mr1 <- metarep(  m1 , u = 2, common.effect = FALSE , t = 0.05 , 
#'                alternative = 'two-sided', report.u.max = TRUE)
#'                summary(mr1)
summary.metarep <- function(object, ...) {
  
  chkclass(object, "metarep")
  
  object.wo.metarep <- object
  class(object.wo.metarep) <-
    class(object.wo.metarep)[class(object.wo.metarep) != "metarep"]
  ##
  res <- summary(object.wo.metarep)
  ##
  
  res$r.value <- round( object$r.value , digits = 4 )
  if(!is.null(object$u_R) ) res$u.increased <- object$u_R
  if(!is.null(object$u_L) ) res$u.decreased <- object$u_L
  
  ##
  class(res) <- c("summary.metarep", class(res))
  ##
  res
}
