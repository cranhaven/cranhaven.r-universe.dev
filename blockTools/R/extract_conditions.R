#' Create vector of integers containing treatment condition identifiers
#'
#' Creates a vector of integers which represent unique treatment conditions 
#' in an object output from \code{assignment}.
#' 
#' Under the current implementation, \code{level.two} in \code{block} should be set to \code{FALSE}.
#' 
#' @author Ryan T. Moore
#' 
#' @param assg.obj An output object from \code{assignment}.
#' @param data The data frame that was input into \code{block} for blocking.
#' @param id.var A string specifying which column of \code{data} contains identifying information.
#' 
#' @return A numeric vector of integers with \code{nrow(data)} elements with
#' lowest value equal to 1, corresponding to the treatment condition column 
#' from the assignment object each unit is in. For example, if the columns of
#' the assignment object are \code{Treatment} and \code{Control} (in that
#' order), then \code{Treatment} will be represented by a \code{1} and
#' \code{Control} will be represented by a \code{2}. 
#' 
#' For units in \code{data} that are not in \code{assg.obj}, the value of NA
#' is assigned.
#' 
#' @keywords design
#' 
#' @examples 
#' data(x100)
#' out <- block(x100, groups = "g", n.tr = 2, id.vars = c("id"), 
#'              block.vars = c("b1", "b2"))
#'              
#' assg <- assignment(out)
#' 
#' extract_conditions(assg, x100, id.var = "id")
#' 
#' # (Treatment conditions are represented by integers.)
#' 
#' @seealso \code{\link{assignment}}
#' 
#' @export

extract_conditions <- function(assg.obj, data, id.var){
  
  condition <- rep(NA, nrow(data))
  class(condition) <- "integer"
  
  flat_assgs <- bind_rows(assg.obj$assg)
  
  for(col_idx in 1:(ncol(assg.obj$assg[[1]]) - 1)){
    
    wh_this_condition <- data[[id.var]] %in% flat_assgs[, col_idx]
    
    condition[wh_this_condition] <- col_idx
    
  }
  
  return(condition)
}
