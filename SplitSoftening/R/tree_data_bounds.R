# #' Boundaries determined by given data to the splits in the tree.
# #'
# #' @param tr The soft tree
# #' @param ds The data set
# #'
# #' @export
tree.data.bounds <- function(tr, ds) {
  binary.path.to <- function( n ) {
    result <- NULL
    while ( 1 < n ) {
      result <- c( n%%2, result )
      n <- which.max(tr$childref[tr$childref <= n])
    }
    return(result)
  }

  node.data <- function( n ) {
    cur.data <- 1:nrow(ds)
    pb <- binary.path.to(n)
    tn <- 1
    for ( bn in pb ) {
      node <- tr[tn,]
      vals <- ds[[as.character(node$var)]][cur.data]
      if ( node$ncat %in% c( -1L, 1L ) ) {
        cmp.res <- vals <= node$splits
        if ( ( 1 == bn ) == ( -1L == node$ncat ) ) {
          cmp.res <- !cmp.res
        }
        cur.data <- cur.data[cmp.res]
      } else {
        stopifnot( typeof( vals ) == "integer" )
        if ( ( 1 == bn ) == ( -1L == node$ncat ) ) {
          cmp.res <- !is.even(floor(node$lb/2^(as.integer(vals)-1)))
        } else {
          cmp.res <- !is.even(floor(node$ub/2^(as.integer(vals)-1)))
        }
        cur.data <- cur.data[cmp.res]
      }
      tn <- node$childref + bn
    }
    return( cur.data )
  }

  ret.val <- data.frame(lb=rep(NA,nrow(tr)),ub=rep(NA,nrow(tr)))
  for (n in 1:nrow(tr)) {
    if ( tr$ncat[n] %in% c( -1L, 1L ) ) {
      nd <- node.data( n )
      rng <- range(ds[[as.character(tr$var[n])]][nd])
      ret.val$lb[n] <- rng[1]
      ret.val$ub[n] <- rng[2]
    }
  }
  return(ret.val)
}

#' Make split softening based on data ranges.
#'
#' This softening configures each softening parameter in the tree
#' according to `data ranges' appropriate to tree nodes.
#' The parameters are configured such that in each node the distance of the boundary of the softened area from split value is
#' \code{factor} * the distance from the split value to the furthest data point in the tree node
#' projected to the direction from the split value to the boundary.
#'
#'
#' @param tr The soft tree
#' @param ds The data set to be used for determining data boundaries
#' @param factor The scalar factor
#' @return The soft tree with the new softening parameters
#'
#' @examples
#'
#' if(require(tree)) {
#'   train.data <- iris[c(TRUE,FALSE),]
#'   test.data <- iris[c(FALSE,TRUE),]
#'   tr <- tree( Species~., train.data )
#'   
#'   # tree with "zero softening"
#'   s0 <- softsplits( tr )
#'   # softened tree
#'   s1 <- softening.by.data.range( s0, train.data, .5 )
#'   
#'   response0 <- predictSoftsplits( s0, test.data )
#'   response1 <- predictSoftsplits( s1, test.data )
#'   # get class with the highest response
#'   classification0 <- levels(train.data$Species)[apply( response0, 1, which.max )]
#'   classification1 <- levels(train.data$Species)[apply( response1, 1, which.max )]
#'   
#'   # compare classifiction to the labels
#'   table( classification0, test.data$Species )
#'   table( classification1, test.data$Species )
#' }
#'
#' 
#' @export
softening.by.data.range <- function( tr, ds, factor=1.0 ) {
  bounds <- tree.data.bounds( tr, ds )
  index <- !is.na( bounds$lb )
  params <- c( tr$splits[index]-bounds$lb[index], bounds$ub[index]-tr$splits[index] )
  return ( set.softening( tr, params*factor ) )
}

