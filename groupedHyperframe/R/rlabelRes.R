
# confidence interval based on permutation

# permute `mark` across the points for `nsim` times
# for each permutation, calculate ?spatstat.explore::Kmark 
# with all Kmark from all permutation, create `envelope` (think it as a confidence band)
# calculate how likely original Kmark `belongs` to the envelope


#' @title Random Re-Labelling Envelope Residual
#' 
#' @param x see **Usage**
#' 
#' @param ... arguments of the function \link[spatstat.explore]{envelope.ppp}, 
#' other than parameters `simulate`, 
#' `savefuns` and `verbose`
#' 
#' @examples
#' set.seed(52); res1_anemones = spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark)
#' set.seed(52); res1_anemones_times = spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark, f = `*`)
#' stopifnot(!identical(res1_anemones, res1_anemones_times))
#'   
#' \donttest{
#' set.seed(52); spatstat.data::anemones |>
#'   rlabelRes(fun = spatstat.explore::Kmark, f = \(m1, m2) { m1*m2 }) |>
#'   identical(y = res1_anemones_times) |>
#'   stopifnot()
#' }
#' 
#' set.seed(12); res_ants = spatstat.data::ants |>
#'   rlabelRes(fun = spatstat.explore::Gcross)
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @name rlabelRes
#' @export
rlabelRes <- function(x, ...) UseMethod(generic = 'rlabelRes')


#' @title Global Envelope Test in Bath Process
#' 
#' @param x see **Usage**
#' 
#' @param ... parameters (except the first) of the function \link[GET]{global_envelope_test}
#' 
#' @name global_envelope_test_
#' @keywords internal
#' @export
global_envelope_test_ <- function(x, ...) UseMethod(generic = 'global_envelope_test_')





#' @rdname rlabelRes
#' @importFrom spatstat.explore envelope.ppp
#' @importFrom spatstat.random rlabel
#' @importFrom GET residual
#' @export rlabelRes.ppp
#' @export
rlabelRes.ppp <- function(x, ...) {
  x |>
    envelope.ppp(
      ...,
      simulate = expression(rlabel(X = x, permute = TRUE)),
      savefuns = TRUE, 
      verbose = FALSE
    ) |> # c('envelope', 'fv', 'data.frame')
    residual() # c('curve_set', 'list')
}

# 
# |> # 
#global_envelope_test() # c('global_envelope', 'data.frame')

#' @rdname rlabelRes
#' @importFrom spatstat.geom anylapply
#' @export rlabelRes.ppplist
#' @export
rlabelRes.ppplist <- function(x, ...) {
  x |>
    anylapply(FUN = rlabelRes.ppp, ...)
}



#' @rdname global_envelope_test_
#' @importFrom GET global_envelope_test
#' @export global_envelope_test_.anylist
#' @export
global_envelope_test_.anylist <- function(x, ...) {
  id <- x |>
    vapply(FUN = inherits, what = 'curve_set', FUN.VALUE = NA)
  if (!all(id)) return(invisible()) # exception handling
  x |>
    anylapply(FUN = global_envelope_test, ...)
}


#' @rdname rlabelRes
#' @export rlabelRes.hyperframe
#' @export
rlabelRes.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  hc_ppp <- hc |>
    vapply(FUN = is.ppplist, FUN.VALUE = NA)
  n_ppp <- sum(hc_ppp)
  if (!n_ppp) return(invisible()) # exception handling
  if (n_ppp > 1L) stop('does not allow more than 1 ppp-hypercolumn')
  
  z <- tryCatch(expr = {
    hc[[which(hc_ppp)]] |>
      rlabelRes.ppplist(...)
  }, error = identity)
  if (inherits(z, what = 'error')) return(x) # exception handling
  
  return(cbind( # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    x, 
    .rlabelRes = z
  ))
  
}

#' @rdname global_envelope_test_
#' @importFrom spatstat.geom names.hyperframe names<-.hyperframe
#' @export global_envelope_test_.hyperframe
#' @export
global_envelope_test_.hyperframe <- function(x, ...) {
  
  hc <- unclass(x)$hypercolumns
  
  z0 <- hc |>
    lapply(FUN = global_envelope_test_.anylist, ...)
  z <- z0[lengths(z0, use.names = FALSE) > 0L] |>
    do.call(what = hyperframe, args = _)
  names(z) <- paste(names.hyperframe(z), 'GET', sep = '.')
  
  return(cbind( # dispatch to \link[spatstat.geom]{cbind.hyperframe} or [cbind.groupedHyperframe()]
    x, 
    z
  ))
  
}
