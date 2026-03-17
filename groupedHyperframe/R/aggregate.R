
#' @title Aggregate Hyper Data Frame
#' 
#' @param x a \link[spatstat.geom]{hyperframe}
#' 
#' @param by a one-sided \link[stats]{formula}, 
#' containing regular-column names of the input `x`
#' 
#' @param ... additional parameters of the function [aggregate.vectorlist()], 
#' most importantly the parameter `fun`
#' 
#' @returns 
#' The function [aggregate.hyperframe()] returns a \link[spatstat.geom]{hyperframe}.
#'  
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom is.ppplist is.imlist
#' @export aggregate.hyperframe
#' @export
aggregate.hyperframe <- function(
    x, 
    by,
    ...
) {
  
  x0 <- unclass(x)
  xdf <- x0$df
  xhc <- x0$hypercolumns
  
  if (!is.call(by) || by[[1L]] != '~' || length(by) != 2L) stop('`by` must be one-sided formula')
  if (!is.symbol(by. <- by[[2L]])) {
    new_by <- by. |>
      all.vars() |>
      vapply(FUN = \(i) deparse1(call(name = '~', as.symbol(i))), FUN.VALUE = '')
    by |> 
      deparse1() |> col_cyan() |>
      sprintf(fmt = 'grouped structure %s is not allowed') |>
      message()
    new_by |> 
      col_magenta() |>
      paste(collapse = ', ') |> 
      sprintf(fmt = 'please use either one of %s.') |>
      message()
    stop('`by` must be a formula and right-hand-side must be a symbol')
  }
  
  if (inherits(x, what = 'groupedHyperframe')) {
    group <- x |> 
      attr(which = 'group', exact = TRUE)
    # `group` 'up-to' `by.`
    # how to do it beautifully?
    # below is an ugly bandage fix
    g <- all.vars(group)
    id <- match(as.character(by.), table = g)
    if (is.na(id)) stop('`by` must match one of the hierarchy in groupedHyperframe')
    # end of ugly bandage fix
    # grouping structure must be specified by `$df` part!!
    f <- xdf[g[seq_len(id)]] |>
      interaction(drop = TRUE, sep = '.', lex.order = TRUE)
  } else {
    # grouping structure must be specified by `$df` part!!
    f <- xdf[[by.]] |> as.factor()
  }
  
  if (all(table(f) == 1L)) return(x) # exception handling
  
  xdf_ag <- xdf |> 
    mc_identical_by(f = f, ...)
  
  id_vector <- xhc |>
    vapply(FUN = is.vectorlist, mode = 'numeric', FUN.VALUE = NA)
  xhc_vector <- if (any(id_vector)) {
    xhc[id_vector] |> 
      lapply(FUN = aggregate.vectorlist, by = f, ...)
  } #else NULL
  
  # object-list supported by spatstat family
  id_ppp <- xhc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  id_im <- xhc |>
    vapply(FUN = is.imlist, FUN.VALUE = NA)
  id_lol <- id_ppp | id_im # list-of-list
  xhc_lol <- if (any(id_lol)) {
    xhc[id_lol] |> 
      lapply(FUN = split.default, f = f)
  } #else NULL
  
  # object-list *not* supported by spatstat family
  id_fv <- xhc |>
    vapply(FUN = is.fvlist, FUN.VALUE = NA) |>
    suppressMessages()
  xhc_fv <- if (any(id_fv)) {
    xhc[id_fv] |> 
      lapply(FUN = as.fvlist) |>
      lapply(FUN = split.default, f = f)
  } #else NULL
  
  ret <- do.call(
    what = cbind.hyperframe, 
    args = c(list(xdf_ag), xhc_vector, xhc_lol, xhc_fv)
  ) # returns 'hyperframe', *not* 'groupedHyperframe' !!
  
  return(ret)
  
}






#' @title Aggregate `vectorlist`
#' 
#' @param x a `vectorlist`
#' 
#' @param by \link[base]{factor}, of same \link[base]{length} as `x`
#' 
#' @param fun \link[base]{function}, aggregation method, 
#' currently supports functions
#' [pmean()], [pmedian()], \link[base]{pmax}, and \link[base]{pmin}.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @importFrom spatstat.geom anylapply
#' @export aggregate.vectorlist
#' @export
aggregate.vectorlist <- function(x, by, fun = pmean, ...) {
  
  fun_supported <- list(pmean, pmedian, pmax, pmin) |>
    vapply(FUN = identical, y = fun, FUN.VALUE = NA) |>
    any()
  if (!fun_supported) {
    '`fun`' |> 
      col_blue() |>
      sprintf(fmt = '%s must be one of {.fun groupedHyperframe::pmean}, {.fun groupedHyperframe::pmedian}, {.fun base::pmax} or {.fun base::pmin}') |> 
      cli_text() |> 
      message(appendLF = FALSE)
    stop()
  }
  
  if (!is.factor(by)) stop('`by` must be factor')
  if (length(by) != length(x)) stop('`by` and `x` must be of same length')
  
  fid <- split.default(seq_along(by), f = by)
  if (all(lengths(fid) == 1L)) {
    message('no need to aggregate')
    return(invisible(x))
  } 
  
  fid |>
    anylapply(FUN = \(i) { # (i = fid[[1L]])
      x[i] |> 
        do.call(what = fun, args = _)
    }) |>
    as.vectorlist(mode = 'numeric')
  
  # 'vectorlist' not respected by spatstat.geom::hyperframe(), yet
  
  
}


