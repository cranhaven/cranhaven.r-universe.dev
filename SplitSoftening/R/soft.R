# helper functions
catlist.char.codes <- c( letters, as.character( 0:5 ) )

catlist.code <- function( catlist ) {
  nums <- lapply( strsplit( catlist, "" ), FUN=function( x ) { lapply( x, FUN=function( y ) { which( y == catlist.char.codes ) } ) } )
  return( as.double( sapply( nums, FUN=function(x){ sum( 2^(unlist(x)-1) ) } ) ) )
}

is.even <- function( n ) { return( 0L == n%%2L ) }

#' Create `soft tree' structure from a tree object.
#' @param fit A tree object: must be a classification tree
#' @return A data structure suitable for softening splits in the tree
#'  and for evaluation of `soft tree' on submitted data.
#'  The returned object is ready for softening, but it is not yet softened.
#'  The result of prediction for some data with the returned object
#'      is still the same as with the original tree \code{fit}.
#' @seealso \code{\link{predictSoftsplits}}.
#' @export
softsplits <- function(fit)
{
  if (inherits(fit, "tree")) {
    ncat <- ifelse( fit$frame$splits[,"cutleft"] == "", 0L, ifelse( substring( fit$frame$splits[,"cutleft"], 0, 1 ) == ":", 2L, -1L ) )
    cutpoints <- rep(NA, nrow(fit$frame))
    cutpoints[ncat==-1L] <- as.double( substr(fit$frame$splits[ncat==-1L,"cutleft"],2L,100L) )
    lb <- ub <- cutpoints
    # Misusing lb, ub for encoding lists of categories
    lb[ncat>1L] <- catlist.code( substr(fit$frame$splits[ncat>1L,"cutleft"],2L,100L) )
    ub[ncat>1L] <- catlist.code( substr(fit$frame$splits[ncat>1L,"cutright"],2L,100L) )
#  } else if (inherits(fit, "rpart")) {
#      if (is.null(fit$splits)) {
#         cutpoints <- NA
#         ncat <- -1
#      }
#      else {
#         # find appropriate split values for the nodes
#         ff <- fit$frame
#         is.leaf <- (ff$var == "<leaf>")
#         whichrow <- !is.leaf
#         index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1*(!is.leaf)))
#         irow  <- index[c(whichrow, FALSE)] #we only care about the primary split
#         lb <- ub <- cutpoints <- rep(NA, nrow(ff))
#         ncat <- rep(NA, nrow(fit$frame))
#         cutpoints[whichrow] <- fit$splits[irow,4]
#         ncat[whichrow] <- fit$splits[irow,2]
#      }
  } else {
    stop(paste("softsplits not supported for object of class",
     class(fit)))
  }
  nodebin=as.integer(rownames(fit$frame))
  ordering <- sort.int(nodebin, index.return=TRUE)
  childref <- unlist(sapply( nodebin, FUN=function(b) (which( ordering$x==2*b)[1])))
  ret <- (data.frame(var=fit$frame$var,
    splits=cutpoints, ncat=ncat,
    lb=lb, ub=ub,
    childref=childref,
    row.names=rownames(fit$frame)))
  if (is.null(levels <- attr(fit, "ylevels"))) {
    stop("No ylevels attribute - probably not a classification tree.")
  }
  ll <- length(levels)
  if (inherits(fit, "rpart")) {
    ret$yval <- fit$frame$yval2[,(2+ll):(1+2*ll)]
  } else { # tree
    ret$yval <- fit$frame$yprob
  }
  ret <- ret[ordering$ix,]
  attr(ret, "ylevels") <- levels
  attr(ret, "terms") <- fit$terms
  #attr(ret, "term.labels") <- attr(fit$terms,"term.labels")

  return(ret)
}

# #' Set softening parameters to the soft tree.
# #'
# #' @param fit The soft tree.
# #' @param softness The softening parameters (XXX further explanation needed)
# #' @return The soft tree with the new softening parameters
# #'
# #' @export
set.softening <- function(fit, softness) {
  is.split <- fit$ncat==-1L | fit$ncat==1L
  split.count <- sum(is.split)
  softness <- rep_len(softness, 2*split.count)
  softness[1:split.count] <- -softness[1:split.count]
  fit[is.split,c("lb","ub")] <- rep_len(fit$splits[is.split],2*split.count)+softness
  return(fit)
}

