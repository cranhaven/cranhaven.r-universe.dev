

#' @title Batch Operations of `'ppplist'` Object
#' 
#' @description
#' Batch operations for a `'ppplist'` input.
#' 
#' @param x a `'ppplist'` object
#' 
#' @param op workhorse \link[base]{function}, either [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()]
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}.
#' Default is the return of the function \link[parallel]{detectCores}.
#' 
#' @param ... additional parameters of workhorse functions 
#' [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()]
#' 
#' @details
#' The function [op_ppplist()] is a \pkg{parallel} batch process of 
#' the workhorse function [ppp_numeric2fv()], [ppp_multitype2fv()] or [ppp2dist()].
#' 
#' @returns 
#' The function [op_ppplist()] returns a \link[stats]{listof} 
#' \itemize{
#' \item function [ppp_numeric2fv()] returns, if `op = ppp_numeric2fv`.
#' \item function [ppp_multitype2fv()] returns, if `op = ppp_multitype2fv`.
#' \item function [ppp2dist()] returns, if `op = ppp2dist`.
#' }
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel detectCores mclapply makeCluster stopCluster
#' @importFrom spatstat.geom anylist
#' @export
op_ppplist <- function(
    x, 
    op,
    mc.cores = detectCores(),
    ...
) {
  
  n <- length(x) # needed for progress-printing!
  sq <- n |>
    seq_len()
  
  .rstudio <- identical(Sys.getenv('RSTUDIO'), '1') 
  # Sys.getenv('RSTUDIO') # returns '' in both vanilla R and Positron
  .positron <- identical(Sys.getenv('POSITRON'), '1')
  # Sys.getenv('POSITRON') # returns '' in both vanilla R and RStudio 
    
  foo <- if (.rstudio && (.Platform$OS.type == 'unix')) {
    # parameter name must be `.i` !!
    # [Gcross_.ppp()] carries parameter `i` in `...` !!!
    \(.i, x, ...) {
      sprintf(fmt = 'printf \'\r%d/%d done!    \'', .i, n) |> 
        # echo-command does not work with '\r' (carriage return)
        system() |> 
        on.exit()
      # this command 
      # .. kills vanilla R on Mac
      # .. kills Positron on Mac !!!
      # .. does not show up in RStudio on Windows
      x[[.i]] |> op(...)
    }
  } else {
    \(.i, x, ...) {
      x[[.i]] |> op(...)
    }
  }
  
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      ret0 <- sq |>
        mclapply(mc.cores = mc.cores, FUN = foo, x = x, ...) 
        #lapply(FUN = foo, x = x, ...) # when debugging
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      ret0 <- foreach(i = sq, .options.multicore = list(cores = mc.cores)) %dopar% foo(.i = i, x = x, ...)
      stopCluster(cl)
    })
  # `ret0`: 1st subject, 2nd mark
  
  message() |> on.exit()
  
  names(ret0) <- names(x)
  
  # re-organize the list!!
  # `ret`: 1st mark, 2nd subject
  #ret <- .mapply(FUN = list, dots = ret0, MoreArgs = NULL)
  ret <- .mapply(FUN = anylist, dots = ret0, MoreArgs = NULL) # 2025-09-24
  # using `anylist` obviously correct and better, but does it actually improve anything?
  
  nm <- names(ret0[[1L]])
  names(ret) <- nm
  
  if (all(vapply(ret0, FUN = inherits, what = 'fvlist', FUN.VALUE = NA))) {
    return(mapply(
      FUN = as.fvlist, 
      X = ret, data.name = nm,
      MoreArgs = NULL, SIMPLIFY = FALSE
    ))
  } 
  
  if (all(vapply(ret0, FUN = inherits, what = 'vectorlist', FUN.VALUE = NA))) {
    suffix. <- ret0[[1L]] |>
      attr(which = 'suffix', exact = TRUE)
    z <- ret |>
      lapply(FUN = \(i) {
        attr(i, which = 'suffix') <- suffix.
        return(i)
      })
    return(z)
  } 
  
  stop('should not come here')

}


