#' Similar to expand.grid but with Sequences Reversed and Ability to Treat 
#' Variables as Sets
#' 
#' Loops over the last argument, then the second-last, and so on. It should be 
#' faster than \code{\link[base]{expand.grid}}.
#' 
#' @param ... Vectors you want all combinations of.
#' @param together Data frame of vectors, where each row is a set of parameter 
#' values that are always kept together.
#' 
#' @return Data table.
#' 
#' @examples
#' # Simple example of expand.grid vs. expand_grid
#' expand.grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' expand_grid(x = c("a", "b", "c"), y = c(1, 2), z = c(TRUE, FALSE))
#' 
#' # How to keep certain variables together
#' expand_grid(x = 1: 5, 
#'             together = data.frame(y = c("1a", "2a"), z = c("1b", "2b")))
#' 
#' @export
expand_grid <- function(..., together = NULL) {
  
  if (is.null(together)) {
    
    inputs.list <- list(...)
    n.levels <- vapply(inputs.list, length, integer(1))
    n.rows <- prod(n.levels)
    n.each <- n.rows / cumprod(n.levels)
    n.times <- n.rows / n.each / n.levels
    df <- mapply(
      FUN = function(x, y, z) {
        rep(rep(x, each = y), z)
      },  
      x = inputs.list, 
      y = n.each, 
      z = n.times, 
      SIMPLIFY = FALSE
    )
    setattr(df, "class", "data.table")
    return(df)
    
  } else {
    
    # Do expansion for vectors in ...
    inputs.list <- list(...)
    n.inputs <- length(inputs.list)
    nrow_together <- nrow(together)
    n.levels <- c(vapply(inputs.list, length, integer(1)), nrow_together)
    n.rows <- prod(n.levels)
    n.each <- n.rows / cumprod(n.levels)
    n.times <- n.rows / n.each / n.levels
    df <- mapply(
      FUN = function(x, y, z) {
        rep(rep(x, each = y), z)
      },  
      x = inputs.list, 
      y = n.each[-(n.inputs + 1)], 
      z = n.times[-(n.inputs + 1)], 
      SIMPLIFY = FALSE
    )
    setattr(df, "class", "data.table")
    
    # Add columns for variables in together data frame
    locs <- rep(seq_len(nrow_together), n.times[length(n.times)])
    df <- cbind(df, together[locs, ])
    setnames(df, c(names(inputs.list), names(together)))
    return(df)
    
  }
  
}