
#' @title Kernel \link[stats]{density} of \link[spatstat.geom]{marks}
#' 
#' @description
#' Kernel \link[stats]{density} of \link[spatstat.geom]{marks}.
#' 
#' @param x a \link[spatstat.geom]{ppp.object}, or `'ppplist'`
#' 
#' @param ... additional parameters of the function \link[stats]{density.default}
#' 
#' @note
#' The function `spatstat.explore::density.ppp()` is for \eqn{x}- and \eqn{y}-\link[spatstat.geom]{coords} only!
#' 
#' @details
#' The function [density_marks()] finds 
#' the kernel density of the \link[spatstat.geom]{marks}.
#' 
#' @returns 
#' The function [density_marks()] returns
#' a \link[base]{numeric} \link[base]{vector}.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name density_marks
#' @export
density_marks <- function(x, ...) UseMethod(generic = 'density_marks')


#' @rdname density_marks
#' @export density_marks.ppp
#' @export
density_marks.ppp <- function(x, ...) {
  
  m <- x |>
    marks(dfok = TRUE, drop = FALSE)
  
  x |>
    markformat() |>
    switch('dataframe' = {
      id <- m |>
        vapply(FUN = is.numeric, FUN.VALUE = NA)
      ret0 <- m[id] |> 
        lapply(FUN = density.default, ...)
      mapply(FUN = \(d, nm) {
        d$data.name <- nm |>
          sprintf(fmt = '$%s')
        d$call[[1L]] <- quote(density.default)
        d$call[[2L]] <- d$data.name |> 
          as.symbol() # illegal name! on purpose!
        return(d)
      }, d = ret0, nm = names(m[id]), SIMPLIFY = FALSE)
      
    }, 'vector' = {
      if (!is.numeric(m)) return(invisible())
      return(density.default(m, ...))
      
    }, 'none' = {
      return(invisible())
    })
  
}


#' @rdname density_marks
#' @export density_marks.ppplist
#' @export
density_marks.ppplist <- function(x, ...) {
  
  x |>
    lapply(FUN = density_marks.ppp, ...)
  
}




