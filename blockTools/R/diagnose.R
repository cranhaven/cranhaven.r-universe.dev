#' @title Diagnose whether units assigned to different treatment conditions may be subject to interference or pairwise imbalance
#' 
#' @description
#' List all pairs of units assigned to different treatment conditions whose difference on a specified variable falls within a specified range.
#' 
#' @details
#' \code{object} requires rows to correspond to blocks and columns to correspond to treatment conditions, such as output from \code{assignment}.
#' 
#' \code{data} should include identifying variables and variable suspected of interference or imbalance.  Typically, \code{data} may be the same dataframe input into \code{block}.
#' 
#' An example of specified identifying variables is \code{id.vars = c("id", "id2")}. Unlike \code{block}, \code{diagnose} requires that the length of \code{id.vars} correspond to the level of the original blocking. See \code{block} documentation for details.
#' 
#' An example of specified suspect range is \code{suspect.var = "b2"}, \code{suspect.range = c(0,50)} identifies all units assigned to different treatment conditions no more than 50 units apart on variable \code{"b2"}. 
#' 
#' @param object a dataframe or list of dataframes of assigned units, such as output from \code{assignment}.
#' @param data a dataframe with auxiliary information on assigned units, including the specified variable \code{suspect.var}.
#' @param id.vars a required string or vector of two strings specifying which column(s) of \code{data} contain identifying information.
#' @param suspect.var a string specifying which column of \code{data} contains the variable suspected of interference or imbalance.
#' @param suspect.range a vector defining the range of \code{suspect.var} within which units in different treatment conditions must fall to be considered suspect.
#' 
#' @return A list of dataframes, each containing a group's pairs of units assigned to different treatments falling within \code{suspect.range} on the variable \code{suspect.var}. The last column of each dataframe displays the observed difference between the two units. 
#' 
#' @examples
#' data(x100)
#' 
#' # First, block
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), 
#'              block.vars = c("b1", "b2"), algorithm = "optGreedy", 
#'              distance = "mahalanobis", level.two = FALSE, valid.var = "b1", 
#'              valid.range = c(0,500), verbose = TRUE)
#' 
#' # Second, assign
#' assg <- assignment(out, seed = 123)
#' 
#' # Third, diagnose
#' diag <- diagnose(object = assg, data = x100, id.vars = "id", 
#'                  suspect.var = "b2", suspect.range = c(0,50))
#' 
#' @seealso \code{\link{assignment}}, \code{\link{block}}
#' 
#' @author Ryan T. Moore
#' 
#' @keywords design
#' 
#' @export

diagnose <- function(object, data, id.vars, suspect.var,
                     suspect.range = NULL){ 

  out <- list()

  if(is.matrix(object) || is.data.frame(object)){
    object <- list(as.data.frame(object))
  }

  if(is.matrix(data)){
    data <- as.data.frame(data)
  }

  if(!is.character(id.vars)){
    stop("id.vars is not a character string, or vector of character
strings.  See documentation and respecify id.vars.")
  }

  gp.names <- array(NA)

  data.diag <- data[, (names(data) %in% c(id.vars, suspect.var))]  

  lev <- length(id.vars)

  for(i in 1:length(object$assg)){  
    
    # assignment object
    assg.gp <- as.matrix(object$assg[[i]])
    
    # data from group
    data.gp <- data.diag[data.diag[,id.vars[lev]] %in%
                         as.matrix(assg.gp), ]

    if(nrow(data.gp) != 0){
      tmp <- data.gp[, !(names(data.gp) %in% id.vars)]
      d.mat <- expand.grid(tmp, tmp)

      diffs <- abs(d.mat[,1] - d.mat[,2])
      suspect.vec <- (suspect.range[1] <= diffs) & (diffs <= suspect.range[2])

      storage <- as.data.frame(matrix(NA, 1, 2 * lev + 1))
      ct <- 0

      for(j in which(suspect.vec)){
        ct <- ct + 1
        u1 <- j%%nrow(data.gp)
        u1[u1 == 0] <- nrow(data.gp)
        u2 <- ceiling(j/nrow(data.gp))
        storage[ct,] <- cbind(data.gp[u1, id.vars],
                              data.gp[u2, id.vars], 
                              diffs[j])
      }

      tmp.na <- array(NA)

      for(k in 1:nrow(storage)){
        col1 <- ceiling(which(object$assg[[i]] ==
                              storage[k, 1])/nrow(object$assg[[i]]))
        col2 <- ceiling(which(object$assg[[i]] ==
                              storage[k, (lev + 1)])/nrow(object$assg[[i]]))
        if(length(col1) == 0 || length(col2) == 0 || (col1 == col2)){
          tmp.na <- append(tmp.na, k)
        }
      }

      if(sum(is.na(tmp.na)) != length(tmp.na)){
        tmp.na <- tmp.na[2:length(tmp.na)]
        storage <- storage[-tmp.na, ]
      }

      if(sum(is.na(storage)) == prod(dim(storage))){
        storage <- "No units with different treatments are suspect."
      }     

      tmp.dup <- NULL
      if(!is.null(nrow(storage))){
        # cut duplicates
        tmp.dup <- rep(TRUE, nrow(storage))
        for(ll in 1:(nrow(storage) - 1)){
          for(mm in (ll + 1):nrow(storage)){
            if(sum(c(storage[ll, 1], storage[ll, lev + 1]) %in%
                   storage[mm, 1:(ncol(storage) - 1)]) == 2){
              tmp.dup[mm] <- FALSE
            }
          }
        }
      }

      if(length(tmp.dup) > 0){
        storage <- storage[tmp.dup, ]
      }

      if(sum(is.na(storage)) == prod(dim(storage))){
        storage <- "No units with different treatments are suspect."
      }
    }else{
      storage <- "No units with different treatments are suspect."
    }

    if(!is.null(nrow(storage))){
      # renumber rows
      rownames(storage) <- 1:nrow(storage)
      # order columns by value
      tmp1 <- storage[, 1:lev]
      storage[, 1:lev] <- storage[, (lev+1):(2*lev)]
      storage[, (lev+1):(2*lev)] <- tmp1
      # sort rows, ascending by distance
      o <- order(storage[, ncol(storage)])
      storage <- storage[o, ]
      # name rows
      rownames(storage) <- 1:nrow(storage)
      # name columns
      names(storage)[ncol(storage)] <- "Difference"
      reps <- floor(ncol(storage)/2)
      names(storage)[1:(ncol(storage)-1)] <- rep(paste("Unit ",
                                                 1:2, sep=""),
                                                 each=reps)
    }

    gp.names[i] <- names(object$assg)[i]
    out[[i]] <- storage
  }

  names(out) <- gp.names
  output <- list(diagnose = out)
  output$call <- match.call()
  output$suspect.var <- suspect.var
  output$suspect.range <- suspect.range
  class(output) <- "diagnose"
  return(output)
}
