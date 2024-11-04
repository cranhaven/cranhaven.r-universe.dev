#' @title Randomly assign blocked units to treatment conditions
#' 
#' @description
#' Using an output object from \code{block}, assign elements 
#' of each row to treatment condition columns.  Each element is equally 
#' likely to be assigned to each column.
#' 
#' @details
#' \code{block.obj} can be specified directly by the user. It can be a
#' single dataframe or matrix with blocks as rows and treatment
#' conditions as columns. \code{assignment} is designed to take a list
#' with two elements. The first element should be named \code{$blocks},
#' and should be a list of dataframes. Each dataframe should have blocks
#' as rows and treatment conditions as columns. The second element
#' should be a logical named \code{$level.two}. A third element, such as
#' \code{$call} in a \code{block} output object, is currently ignored.
#' 
#' Specifying the random seed yields constant assignment, and thus allows 
#' for easy replication of experimental protocols.
#' 
#' If \code{namesCol = NULL}, then \dQuote{Treatment 1}, \dQuote{Treatment 2},
#' \ldots are used. If \code{namesCol} is supplied by the user and is of length 
#' \code{n.tr} (or 2*\code{n.tr}, where \code{level.two = TRUE}), then either 
#' \code{"Distance"} or \code{"Max Distance"} is appended to it as appropriate 
#' (consistent with \code{namesCol} usage in \code{block}). If \code{namesCol} 
#' is supplied and is of length \code{n.tr} + 1 (or 2 * \code{n.tr} + 1, where 
#' \code{level.two = TRUE}), then the last user-supplied name is used for the 
#' last column of each dataframe.
#' 
#' @param block.obj an output object from \code{block}, or a user-specified block object.
#' @param seed a user-specified random seed.
#' @param namesCol an optional vector of column names for the output table.
#' 
#' @return 
#' A list with elements
#' \itemize{
#'   \item \strong{assg}: a list of dataframes, each containing a group's 
#'   blocked units assigned to treatment conditions. If there are two treatment 
#'   conditions, then the last column of each dataframe displays the 
#'   multivariate distance between the two units. If there are more than two 
#'   treatment conditions, then the last column of each dataframe displays the 
#'   largest of the multivariate distances between all
#'   possible pairs in the block.
#'   \item \strong{call}: the original call to \code{assignment}.
#' }
#' 
#' @examples
#' data(x100)
#' 
#' # First, block
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), block.vars
#'           = c("b1", "b2"), algorithm="optGreedy", distance = "mahalanobis", 
#'           level.two = FALSE, valid.var = "b1", valid.range = c(0,500), 
#'           verbose = TRUE)
#'
#' # Second, assign
#' assigned <- assignment(out, seed = 123)
#' 
#' # assigned$assg contains 3 data frames
#' 
#' @seealso \code{\link{block}}, \code{\link{diagnose}}
#' 
#' @author Ryan T. Moore
#' 
#' @keywords design
#' 
#' @export

assignment <- function(block.obj, seed = NULL, namesCol = NULL){

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(is.matrix(block.obj) || is.data.frame(block.obj)){
    tmp <- list()
    tmp$blocks$"1" <- block.obj
    block.obj <- tmp
  }

  if(is.null(block.obj$level.two)){
    block.obj$level.two <- FALSE
  }
  
  if(!is.null(namesCol)){
    if(length(namesCol) != (ncol(block.obj$blocks[[1]]) - 1)){
      warning(paste0(
      "namesCol is not the same length as the number of treatment conditions.\n  Check output column names and respecify if needed."))
    }
  }

  out <- list()
  gp.names <- array(NA)

  # perform assignment w/in groups
  for(i in 1:length(block.obj$blocks)){ 

    gp.obj <- as.matrix(block.obj$blocks[[i]])

    ncol.tab <- ncol(gp.obj)
    
    if(is.null(namesCol)){
      namesCol <- c(rep(NA, ncol.tab-1), "Distance")
      if((ncol.tab>5) || ((ncol.tab)>3 && (block.obj$level.two ==
                                            FALSE))){
        namesCol[length(namesCol)] <- "Max Distance"
      }
      
      if(block.obj$level.two == FALSE){
        for(j in 1:(ncol.tab-1)){
          namesCol[j] <- paste("Treatment ", j, sep = "")
        }
      }else{
        for(j in 1:((ncol.tab-1)/2)){
          namesCol[(2*j-1):(2*j)] <- rep(paste("Treatment ", j, sep = ""),2)
        }
      }
    }else{ # if !is.null(namesCol)
      if(length(namesCol) == (ncol.tab - 1)){ # if user gives only names for assignments, not distance
        if(block.obj$call$n.tr == 2){
          namesCol <- append(namesCol, "Distance")
        }else{
          namesCol <- append(namesCol, "Max Distance")
        }
      }
    }
              
    ## Put units into treatment groups with pr(u_i in g_j) = 1/|g|    
    for(j in 1:(nrow(gp.obj))){
      tmp <- gp.obj[j, ]
      if(block.obj$level.two == FALSE){
        tmp[1:(ncol.tab-1)] <- tmp[sample(ncol.tab-1)]
      }else{
        s <- sample((1:(ncol.tab-1))[((1:(ncol.tab-1)) %% 2 == 1)]) # replaced [odd(...)] 8 April 2014
        tmp[1:(ncol.tab-1)] <- tmp[c(rbind(s, s+1))]
      }
      gp.obj[j,] <- tmp
    }
    gp.obj <- as.data.frame(gp.obj)
    gp.obj[,ncol(gp.obj)] <- as.numeric(as.character(gp.obj[, ncol(gp.obj)]))
    names(gp.obj) <- namesCol
    out[[i]] <- gp.obj
    gp.names[i] <- names(block.obj$blocks)[i]
  }

  names(out) <- gp.names

  output <- list(assg = out)
  output$call <- match.call()
  class(output) <- "assg"
  return(output)
}
