
#' @importFrom spatstat.geom is.hyperframe
#' @importFrom groupedHyperframe t.vectorlist
.prepare_add_ <- function(start.model, x, data, envir = parent.frame(), ...) {
  
  fom0 <- formula(start.model)
  
  y <- start.model$y
  if (!length(y)) stop('`start.model` response?')
  if (all(y %in% c(0, 1), na.rm = TRUE)) y <- as.logical(y)
  # packageDate('caret') # 2024-12-09
  # ?caret::createDataPartition **not** good with 'numeric' 0/1 response `y`
  # tzh will write to author again (after he responds with [statusPartition()])
  
  force(data)
  if (!is.hyperframe(data)) stop('`data` must be `hyperframe`')
  
  # reduce `data` to match `start.model`
  if (length(y) != nrow(data)) { # invokes ?spatstat.geom::dim.hyperframe
    start_na <- start.model$na.action
    if (!length(start_na)) stop('`start.model` `na.action` not available?')
    data <- data[-start_na, , drop = FALSE]
  }
  
  if (!is.language(x) || is.symbol(x) || x[[1L]] != '~' || length(x) != 2L) stop('`x` must be one-sided formula')
  if (!is.symbol(x. <- x[[2L]])) stop('rhs(x) must be a symbol')
  X <- data[[x.]] # 'list'; the hypercolumn
  
  x_ <- X[[1L]] |> 
    names() |> 
    lapply(FUN = \(j) {
      call(name = '[', x., j)
    })
  
  xval <- X |> 
    t.vectorlist()
  
  return(list(
    y = y,
    data = unclass(data)$df,
    x_ = x_,
    xval = xval
  ))
  
}



