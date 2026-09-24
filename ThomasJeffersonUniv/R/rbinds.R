


#' @title Row-Bind a \link[base]{list} of \link[base]{data.frame}
#' 
#' @description ..
#' 
#' @param x a \link[base]{list} of named \link[base]{data.frame}
#' 
#' @param .id \link[base]{character} value to specify the name of ID column, 
#' nomenclature follows \link[data.table]{rbindlist}
#' 
#' @param make.row.names,... additional parameters of \link[base]{rbind.data.frame}
#' 
#' @details 
#' Yet to look into `ggplot2:::rbind_dfs` closely.
#' 
#' Mine is slightly slower than the fastest alternatives, but I have more checks which are useful.
#' 
#' @returns 
#' Function [rbinds] returns a \link[base]{data.frame}.
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame}
#' 
#' @examples 
#' x = list(A = swiss[1:3, 1:2], B = swiss[5:9, 1:2]) # list of 'data.frame'
#' rbinds(x)
#' rbinds(x, make.row.names = TRUE)
#' 
#' @export
rbinds <- function(x, make.row.names = FALSE, ..., .id = 'idx') {
  
  x <- x[lengths(x) > 0L]
  if (!(nx <- length(x))) return(invisible())
  
  if (!is.list(x) || !all(vapply(x, FUN = inherits, what = 'data.frame', FUN.VALUE = NA))) stop('input must be a list of data.frame')
  
  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm)) || anyDuplicated.default(nm)) stop('names of list of data.frame must be unique')
  
  cnms <- lapply(x, FUN = names)
  if (!all(duplicated.default(cnms)[-1L])) stop('all data.frame\'s must have same column names')
  
  cls <- lapply(x, FUN = function(idat) lapply(idat, FUN = function(jcol) paste(class(jcol), collapse = '-')))
  if (!all(duplicated.default(cls)[-1L])) {
    tmp <- do.call(Map, args = c(list(f = c), cls))
    lapply(seq_along(tmp), FUN = function(i) {
      if (!all(duplicated.default(tmp[[i]])[-1L])) {
        cat(names(tmp)[i], '\n')
        print(tmp[[i]])
      }
    })
    stop('Columns of all data.frame must be the same class')
  }
  
  if (!is.character(.id) || length(.id) != 1L || anyNA(.id) || !nzchar(.id)) stop('illegal .id')
  if (.id %in% names(cls[[1L]])) stop(sQuote(.id), ' already in existing column names')
  
  ret <- do.call(rbind.data.frame, args = c(x, list(make.row.names = make.row.names, ...)))
  
  nr <- vapply(x, FUN = .row_names_info, type = 2L, FUN.VALUE = 0L)
  idx <- rep(seq_len(nx), times = nr)
  attr(idx, which = 'levels') <- nm
  class(idx) <- 'factor'
  ret[[.id]] <- idx
  return(ret)
  
}

