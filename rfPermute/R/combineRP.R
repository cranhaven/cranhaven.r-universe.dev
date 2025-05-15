#' @title Combine rfPermute objects
#' @description Combines two or more ensembles of \code{rfPermute} objects into 
#'   one, combining \code{randomForest} results, null distributions, 
#'   and re-calculating p-values.
#' 
#' @param \dots two or more objects of class \code{rfPermute}, to be combined 
#'   into one.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[randomForest]{combine}}
#' 
#' @examples
#' data(iris)
#' rp1 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp2 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp3 <- rfPermute(
#'   Species ~ ., iris, ntree = 50, norm.votes = FALSE, nrep = 100, num.cores = 1
#' )
#' rp.all <- combineRP(rp1, rp2, rp3)
#' rp.all
#' 
#' plotNull(rp.all) 
#' 
#' @export
#' 
combineRP <- function(...) {
  rp.list <- list(...)
  
  are.rp <- sapply(rp.list, inherits, what = "rfPermute")
  if(any(!are.rp)) stop("some objects in '...' are not rfPermute models.")
  
  rf <- do.call(randomForest::combine, lapply(rp.list, as.randomForest))
  null.dist <- sapply(c("unscaled", "scaled"), function(sc) {
    do.call(
      abind::abind,
      c(lapply(rp.list, function(x) x$null.dist[[sc]]), along = 3)
    )
  }, simplify = FALSE)
  
  start.time <- min(sapply(rp.list, function(x) x$start))
  end.time <- max(sapply(rp.list, function(x) x$end))
  
  result <- list(
    rf = rf, 
    null.dist = null.dist,
    pval = .calcImpPval(rf, null.dist), 
    num.rep = sum(sapply(rp.list, function(x) x$num.rep)), 
    start = as.POSIXct(start.time, origin = "1970-01-01"),
    end = as.POSIXct(start.time, origin = "1970-01-01")
  )
  class(result) <- "rfPermute"
  result
}