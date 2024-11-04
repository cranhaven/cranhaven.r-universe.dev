#' Create vector of integers containing block identifiers
#'
#' Creates a vector of integers which represent unique blocks in an
#' object output from \code{block} or \code{assignment}.
#' 
#' @details 
#' Under the current implementation, \code{level.two} in \code{block}
#' should be set to \code{FALSE}.
#' 
#' If blocking was performed specifying a \code{groups} argument,
#' \code{createBlockIDs} will assign block ID values that are unique
#' across groups.  In other words, \code{createBlockIDs} does not restart
#' numbering when it encounters a new group of blocks.
#' 
#' @author Ryan T. Moore
#' 
#' @param obj An output object from \code{block} or \code{assignment}.
#' @param data The data frame that was input into \code{block} for blocking.
#' @param id.var A string specifying which column of \code{data} contains
#' identifying information.
#' 
#' @return A numeric vector of integers with \code{nrow(data)} elements with
#' lowest value equal to 1, corresponding to the block each unit is in. 
#' For units in \code{data} that are not in \code{obj}, the value of NA
#' is assigned.
#' 
#' @keywords design
#' 
#' @examples 
#' data(x100)
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), 
#'              block.vars = c("b1", "b2"))
#'              
#' createBlockIDs(out, x100, id.var = "id")
#' 
#' # (Block ID integers are unique, even with several groups.)
#' 
#' @seealso \code{\link{block}}, \code{\link{assignment}}
#' 
#' @export

createBlockIDs <- function(obj, data, id.var){
  
  if(length(obj[[1]]) == 1){
    obj.simp <- obj[[1]]$`1`
  }else{
    obj.simp <- NULL
    for(gp in obj[[1]]){
      obj.simp <- rbind(obj.simp, gp)
    }
  }
  
  row.n <- nrow(obj.simp)
  bbb <- rep(NA, nrow(data)) 
  
  for(col.idx in 1:(ncol(obj.simp) - 1)){ # only for level.two == FALSE
    tmp.colname <- paste("col", col.idx, sep = "")
    assign(tmp.colname, obj.simp[, col.idx])   
    tmp.col <- get(tmp.colname)
    
    if(is.factor(tmp.col)){
      tmp.col <- unfactor(tmp.col)
    }
    
    for(row.idx in 1:length(tmp.col)){
      bbb[data[[id.var]] == tmp.col[row.idx]] <- row.idx
    }
  }
  
  return(bbb)
}
