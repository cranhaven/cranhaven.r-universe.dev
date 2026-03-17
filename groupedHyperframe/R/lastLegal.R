
#' @title Last Legal Index
#' 
#' @param v \link[base]{double} \link[base]{vector}
#' 
#' @details
#' Legal, meaning not `0`, not `NaN` and not `Inf`.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/nonS3/lastLegal.html}
#' 
#' @keywords internal
#' @export
lastLegal <- function(v) {
  
  vok <- is.finite(v) & (abs(v) > .Machine$double.eps) # not 0, not NaN, not Inf
  
  if (all(vok)) {
    
    id <- length(vok) # faster than [.diff()]
    
  } else {
    
    .diff <- \(x) {
      x[-1L] - x[-length(x)]
    } # faster than ?base::diff.default
    
    # if `vok` starts with `FALSE`
    if (!vok[1L]) {
      # tolerate `0`
      tmp <- (abs(v) < .Machine$double.eps)
      .notsame <- (cumsum(tmp) != cumsum(rep(TRUE, times = length(tmp))))
      vok[seq_len(min(which(.notsame)) - 1L)] <- TRUE
    }
    
    z <- vok |> 
      which() |>
      .diff()
    
    id <- if (all(z == 1L)) {
      length(z) + 1L
    } else min(which(z != 1L)) # smart!
    
  }
  
  attr(id, which = 'value') <- v[id]
  return(id)
  
}


#' @title Tentative Fix for Illegal Function Value in `fv.object`
#' 
#' @param X an \link[spatstat.explore]{fv.object}
#' 
#' @param key,.x \link[base]{character} scalars
#' 
#' @keywords internal
#' @name fv2theo
#' @export
.illegal2theo <- function(X, ...) UseMethod(generic = '.illegal2theo')

#' @rdname fv2theo
#' @export
.disrecommend2theo <- function(X, ...) UseMethod(generic = '.disrecommend2theo')


#' @rdname fv2theo
#' @importFrom spatstat.explore fvnames
#' @export .illegal2theo.fv
#' @export
.illegal2theo.fv <- function(
    X, 
    key = fvnames(X, a = '.y'), 
    .x = fvnames(X, a = '.x'),
    ...
) {
  
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  theo <- X$theo
  if (is.null(theo)) stop('this fv.object does not have theo ?')
  
  .y <- c(X[[key]]) # # drop attributes since \pkg{spatstat.explore} v3.5.3.9

  id0 <- .y |>
    lastLegal()
  
  if (id0 == length(.y)) return(X) # all legal; exception handling
  
  id <- id0 + 1L # first illegal
  X[[.x]][id] |>
    sprintf(
      fmt = 'r\u2265%.1f replaced with %s', 
      'theo' |> col_red() |> style_bold()
    ) |>
    message()
  sq <- id:length(.y)
  X[[key]][sq] <- theo[sq]
  
  return(X)
  
}


#' @rdname fv2theo
#' @importFrom spatstat.explore fvnames
#' @export .disrecommend2theo.fv
#' @export
.disrecommend2theo.fv <- function(
    X, 
    key = fvnames(X, a = '.y'), 
    .x = fvnames(X, a = '.x'),
    ...
) {
  
  force(key)
  force(.x)
  if (key == .x) stop('first column of `x` is not the output of `fv.object`')
  
  theo <- X$theo
  if (is.null(theo)) stop('this fv.object does not have theo ?')
  
  .y <- c(X[[key]]) # # drop attributes since \pkg{spatstat.explore} v3.5.3.9
  
  # in ?spatstat.explore::fv documentation
  # alim specifies the recommended range of the function argument.
  recommend_rmax <- attr(X, which = 'alim', exact = TRUE)[2L]
  id0 <- (X[[.x]] > recommend_rmax) |> 
    which()
  
  if (!length(id0)) return(X) # exception handling
  
  id <- min(id0)
  
  X[[.x]][id] |>
    sprintf(
      fmt = 'r\u2265%.1f replaced with %s', 
      'theo' |> col_red() |> style_bold()
    ) |>
    message()
  
  sq <- id:length(.y)
  X[[key]][sq] <- theo[sq]
  
  return(X)
  
}




#' @rdname fv2theo
#' @export .illegal2theo.fvlist
#' @export
.illegal2theo.fvlist <- function(X, ...) {
  tmp <- X |> 
    is.fvlist()
  key <- tmp |>
    attr(which = '.y', exact = TRUE)
  .x <- tmp |>
    attr(which = '.x', exact = TRUE)
  X |> 
    lapply(FUN = .illegal2theo.fv, key = key, .x = .x, ...) |>
    as.fvlist()
}

#' @rdname fv2theo
#' @export .disrecommend2theo.fvlist
#' @export
.disrecommend2theo.fvlist <- function(X, ...) {
  tmp <- X |> 
    is.fvlist()
  key <- tmp |>
    attr(which = '.y', exact = TRUE)
  .x <- tmp |>
    attr(which = '.x', exact = TRUE)
  X |> 
    lapply(FUN = .disrecommend2theo.fv, key = key, .x = .x, ...) |>
    as.fvlist()
}


#' @rdname fv2theo
#' @importFrom spatstat.geom as.list.hyperframe names.hyperframe
#' @export .illegal2theo.hyperframe
#' @export
.illegal2theo.hyperframe <- function(X, ...) {
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(X)[id]
  
  ret0 <- (as.list.hyperframe(X)[nm]) |>
    lapply(FUN = .disrecommend2theo.fvlist) |>
    suppressMessages()
  
  # to replace original fv-hypercolumn!!!
  z <- unclass(X)
  z$hypercolumns[nm] <- ret0
  class(z) <- class(X)
  return(z)
  
}

#' @rdname fv2theo
#' @importFrom spatstat.geom as.list.hyperframe names.hyperframe
#' @export .disrecommend2theo.hyperframe
#' @export
.disrecommend2theo.hyperframe <- function(X, ...) {
  
  if (!any(id <- (unclass(X)$vclass == 'fv'))) stop('input `x` must contain at least one `fv` column')
  nm <- names.hyperframe(X)[id]
  
  ret0 <- (as.list.hyperframe(X)[nm]) |>
    lapply(FUN = .disrecommend2theo.fvlist) |>
    suppressMessages()
  
  # to replace original fv-hypercolumn!!!
  z <- unclass(X)
  z$hypercolumns[nm] <- ret0
  class(z) <- class(X)
  return(z)
  
}