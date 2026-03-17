

#' @title Simulate (Marked) Point Pattern
#' 
#' @description
#' To simulate \link[spatstat.geom]{ppp.object}, 
#' with none or one or multiple \link[spatstat.geom]{marks}.
#' 
#' @param ... see vignettes
#' 
#' @param dots (for internal use) \link[base]{list} of one or more named \link[base]{list}s.
#' The first \link[base]{list} specifies the parameters to 
#' generate the \eqn{x}- and \eqn{y}-\link[spatstat.geom]{coords}.
#' The second to last \link[base]{list}s, if available, specify the parameters to
#' generate one or more \link[spatstat.geom]{marks}.
#' 
#' @param win \link[spatstat.geom]{owin.object}
#' 
#' @param n \link[base]{integer} scalar, 
#' number of \link[spatstat.geom]{ppp.object}s to generate.
#' Default `1L`.
#' 
#' @param element1 \link[base]{logical} scalar, whether to return 
#' a \link[spatstat.geom]{ppp.object}, 
#' instead of a \link[base]{length}-`1L` \link[spatstat.geom]{solist},
#' when `n==1L`. Default `TRUE`
#' 
#' @param envir \link[base]{environment}, in which to \link[base]{eval}uate the `...` \link[rlang]{dyn-dots} argument.
#' Default is the \link[base]{parent.frame}.
#' 
#' @return 
#' The function [.rppp()] returns a \link[spatstat.geom]{ppp.object} if `(n==1L)&element1`,
#' otherwise returns a \link[base]{length}-`n` \link[spatstat.geom]{solist}
#' (which also has \link[base]{class} `'ppplist'`).
#' 
#' The returned \link[spatstat.geom]{ppp.object}(s) contain only 
#' \eqn{x}- and \eqn{y}-\link[spatstat.geom]{coords}, 
#' if only one \link[base]{call} is present in the `...` \link[rlang]{dyn-dots} argument.
#' Otherwise, they contain one or more \link[spatstat.geom]{marks}
#' according to the rest of the \link[base]{call}(s) in the `...` argument.
#' 
#' @note
#' The name `rppp()` is too aggressive, which might be claimed in future by package \CRANpkg{spatstat.random}.
#' Therefore we name this function [.rppp()] as if it is hidden (see parameter `all.names` of the function \link[base]{ls}).
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/rppp.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom square superimpose.ppp marks<- marks<-.ppp
#' @export
.rppp <- function(
    ..., 
    dots,
    win = square(),
    n = 1L, 
    element1 = TRUE,
    envir = parent.frame()
) {
  
  if (missing(dots)) {
    cl. <- match.call() |> as.list.default()
    ag <- cl.[-1L]
    nm <- names(ag)
    cl <- if (!length(nm)) ag else ag[!nzchar(nm)]
    
    r <- cl |> 
      vapply(FUN = \(i) {
        (i[[1L]]) |> as.character()
      }, FUN.VALUE = '')
    names(cl) <- r # just easier for developer to debug
    dots <- cl |>
      lapply(FUN = \(i) { # (i = cl[[1L]])
        i[[1L]] <- quote(list)
        eval(i, envir = envir)
      })
  }
  
  r <- names(dots)
  names(r) <- r # just easier for developer to debug
  
  par0 <- dots |>
    unlist(recursive = FALSE) |>
    as.data.frame.list() # recycle parameter between all `r`s
  npar <- nrow(par0)
  
  par <- r |> 
    lapply(FUN = \(i) { # (i = 'rStrauss')
      z <- par0[startsWith(names(par0), prefix = i)]
      names(z) <- gsub(pattern = paste0('^', i, '\\.'), replacement = '', x = names(z))
      return(z) # 'data.frame'
    })
  
  if (!length(par)) stop('length(par) == 0L; not allowed')
  
  r[1L] |>
    sprintf(fmt = 'Point-pattern simulated by {.fun spatstat.random::%s}') |> 
    col_blue() |>
    cli_text() |> 
    message(appendLF = FALSE)
  
  r[-1L] |> 
    lapply(FUN = \(i) {
      pkg <- i |> get() |> environment() |> getNamespaceName()
      sprintf(fmt = 'Mark simulated by {.fun %s::%s}', pkg, i) |> 
        col_magenta() |>
        cli_text() |>
        message(appendLF = FALSE)
    })
  
  r1 <- get(r[1L]) # let err
  rnm <- names(formals(r1))
  winpar <- if ('win' %in% rnm) {
    list(win = win)
  } else if ('W' %in% rnm) {
    list(W = win)
  } else stop('shouldnt happen') # to take care of Dr. Baddeley's 'poor code management' :))
  
  fn <- \(j) { # (j = 1L)
    X <- c(winpar, unclass(par[[1L]][j, , drop = FALSE])) |> 
      do.call(what = r1, args = _) # `X$n` is randomly generated too!
    marks(X) <- mapply(
      # *it seems* that since R 4.5.1 for-loop messes up with random seed ??!!??!!
      # ?base::mapply does not mess up with seeds :)))
      FUN = \(par, r) {
        c(list(n = X$n), unclass(par[j, , drop = FALSE])) |> 
          do.call(what = r, args = _)
      }, 
      par = par[-1L], r = r[-1L], SIMPLIFY = FALSE) |>
      as.data.frame.list()
    return(X)
  } 
  
  ret <- replicate(n = n, expr = {
    npar |>
      seq_len() |>
      lapply(FUN = fn) |> 
      do.call(what = superimpose.ppp)
    # ?spatstat.geom::superimpose.ppp does not respect ncol-1 'dataframe' marks!! i.e. it forces `drop`
    # the last line of ?spatstat.geom::superimpose.ppp does not even have `drop` parameter hhahah
  }, simplify = FALSE)
  
  if ((n == 1L) && element1) return(ret[[1L]])
  
  class(ret) <- c('ppplist', 'solist', class(ret)) # see returned value of ?spatstat.geom::split.ppp
  # to make use of 
  # methods(class = 'solist')
  return(ret)
  
} 




if (FALSE) {
  
  stopifnot(identical(unit.square(), square(1)))
  stopifnot(identical(unit.square(), square()))
  
  # has `win`
  spatstat.random::rCauchy()
  
  # has `W`
  spatstat.random::rCauchyHom
  
}

