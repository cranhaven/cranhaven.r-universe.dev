
#' @title Default \eqn{r_\text{max}} of Various Functions in Package \CRANpkg{spatstat.explore}
#' 
#' @param X a \link[spatstat.geom]{ppp.object} with one \link[base]{numeric} or multi-type \link[spatstat.geom]{marks}
#' 
#' @param fun \link[base]{character} scalar, see (the un-documented) function \link[spatstat.explore]{rmax.rule}
#' 
#' @param i,j \link[base]{character} scalars, see functions 
#' \link[spatstat.explore]{Gcross}, 
#' \link[spatstat.explore]{Kcross}, 
#' \link[spatstat.explore]{Jcross}, etc.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name rmax
#' @export
.rmax <- function(X, ...) UseMethod(generic = '.rmax')

# fv.object |> attr(, 'alim') depends on user-input `r`!!
# use tzh's [.rmax] to get default `r` !!


#' @rdname rmax
#' @importFrom spatstat.explore rmax.rule
#' @importFrom spatstat.geom area.owin intensity marks.ppp is.marked.ppp is.ppp npoints.ppp unstack.ppp is.multitype.ppp handle.r.b.args ppsubset
#' @export .rmax.ppp
#' @export
.rmax.ppp <- function(X, fun, i, j, ...) {

  # S3 call to [intensity] is probably [intensity.ppp]
  
  if (!is.ppp(X)) stop('input `X` must be ppp.')
  if (!is.marked.ppp(X)) return(NA_real_) # exception handling
  
  npts <- npoints.ppp(X)
  W <- X$window
  
  has_i <- !missing(i)
  has_j <- !missing(j)
  
  if (has_i && has_j) {
    
    # fun = 'G'; ?spatstat.explore::Gmulti, i.e., ?spatstat.explore::Gcross when i!=j
    # fun = 'K'; ?spatstat.explore::Kmulti, i.e., ?spatstat.explore::Kcross when i!=j (naively use `area(W)`)
    # fun = 'J'; ?spatstat.explore::Jmulti, i.e., ?spatstat.explore::Jcross when i!=j
    # original: marx <- marks(X, dfok = FALSE) # need a little twick..
    xs <- unstack.ppp(X)
    id_mtt <- vapply(xs, FUN = is.multitype.ppp, FUN.VALUE = NA)
    if (!any(id_mtt)) stop('no multitype marks')
    marx_ <- xs[id_mtt] |> 
      lapply(FUN = marks.ppp, dfok = FALSE)
    id <- marx_ |> 
      vapply(FUN = `%in%`, x = j, FUN.VALUE = NA)
    if (sum(id) != 1L) stop('not programed, yet')
    J <- ppsubset(X = X, I = (marx_[[id]] == j), Iname = 'J')
    rmaxdefault <- rmax.rule(
      fun = fun, 
      W = W, 
      lambda = switch(fun, K =, G = sum(J)/area.owin(W), J = intensity(X[J]))
    )

  } else if (!has_i && !has_j) {
    
    # fun = 'K'; ?spatstat.explore::markcorr 
    # fun = 'G'; ?spatstat.explore::Gest, i.e., ?spatstat.explore::Gcross when i==j
    # fun = 'K'; ?spatstat.explore::Kest, i.e., ?spatstat.explore::Kcross when i==j
    # fun = 'J'; ?spatstat.explore::Jest, i.e., ?spatstat.explore::Jcross when i==j
    rmaxdefault <- rmax.rule(
      fun = fun, 
      W = W, 
      lambda = switch(fun, K =, G = npts/area.owin(W), J = intensity(X))
    )
    
  } else stop('wont happen')
  
  breaks <- handle.r.b.args(window = W, rmaxdefault = rmaxdefault)
  # rmax <- breaks$max
  # alim <- c(0, min(rmax, rmaxdefault)) # to remind tzh-self
  return(min(breaks$max, rmaxdefault))
    
}


#' @rdname rmax
#' @importFrom spatstat.explore fvnames
#' @export .rmax.fv
#' @export
.rmax.fv <- function(X, ...) {
  X[[fvnames(X = X, a = '.x')]] |>
    max()
}


#' @rdname rmax
#' @export .rmax.ppplist
#' @export
.rmax.ppplist <- function(X, ...) {
  X |>
    vapply(FUN = .rmax.ppp, ..., FUN.VALUE = NA_real_)
}


#' @rdname rmax
#' @importFrom spatstat.geom is.ppplist
#' @export .rmax.hyperframe
#' @export
.rmax.hyperframe <- function(X, ...) {
  
  # may handle multiple ppp-hypercolumns!!!
  
  hc <- unclass(X)$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA) |>
    which() 
  
  if (!length(hc_ppp)) return(invisible()) # exception handling
  
  ret <- hc[hc_ppp] |>
    lapply(FUN = .rmax.ppplist, ...)
  
  cat('\n')
  mapply(FUN = \(r, nm) {
    tb <- table(r)
    r0 <- r |> unique.default() |> sort.int()
    prt <- if (length(r0) == length(r)) {
      r0 |> 
        range.default() |>
        sprintf(fmt = '%.2f') |>
        paste(collapse = ' ~ ')
    } else {
      sprintf(fmt = '%d\u2a2f ', tb) |> col_br_magenta() |> style_bold() |>
        paste0(sprintf(fmt = '%.2f', r0), collapse = '; ')
    }
    paste(
      'Default', 
      'rmax' |> col_magenta() |> style_bold(),
      'for hypercolumn', 
      nm |> col_blue() |> style_bold(),
      'are',
      prt
    ) |> message()
  }, r = ret, nm = names(ret))
  
  return(invisible(ret))
  
}




