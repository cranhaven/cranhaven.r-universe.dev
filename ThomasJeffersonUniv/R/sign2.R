
#' @title Sign of Difference of Two Objects
#' 
#' @description ..
#' 
#' @param e1,e2 two R objects, must be both 
#' \link[base]{numeric} \link[base]{vector}s,
#' or \link[base]{ordered} \link[base]{factor}s with the same \link[base]{levels}
#' 
#' @param name1,name2 two \link[base]{language} objects, or \link[base]{character} scalars
#' 
#' @param na.detail \link[base]{logical} scalar,
#' whether to provide the missingness details of `e1` and `e2`.
#' Default `TRUE`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' Function [sign2] extends \link[base]{sign} in the following ways
#' \itemize{
#' \item {two \link[base]{ordered} \link[base]{factor}s can be compared;}
#' \item {(detailed) information on missingness are provided.}
#' }
#' 
#' @returns 
#' 
#' Function [sign2] returns \link[base]{character} \link[base]{vector} when `na.detail = TRUE`, or
#' \link[base]{ordered} \link[base]{factor} when `na.detail = FALSE`.
#' 
#' @examples 
#' lv = letters[c(1,3,2)]
#' x0 = letters[1:3]
#' x = ordered(sample(x0, size = 100, replace = TRUE), levels = lv)
#' y = ordered(sample(x0, size = 50, replace = TRUE), levels = lv)
#' x < y # base R ok
#' pmax(x, y) # base R okay
#' pmin(x, y) # base R okay
#' x[c(1,3)] = NA
#' y[c(3,5)] = NA
#' table(sign(unclass(y) - unclass(x)))
#' table(sign2(x, y))
#' table(sign2(x, y, na.detail = FALSE), useNA = 'always')
#' 
#' @export
sign2 <- function(
    e1, e2, 
    name1 = substitute(e1), name2 = substitute(e2), 
    na.detail = TRUE,
    ...
) {
  name1 <- substr(if (is.language(name1)) deparse1(name1) else name1, start = 1L, stop = 20L)
  name2 <- substr(if (is.language(name2)) deparse1(name2) else name2, start = 1L, stop = 20L)
  
  ord1 <- inherits(e1, what = 'ordered')
  ord2 <- inherits(e2, what = 'ordered')
  if (xor(ord1, ord2)) stop('Both e1 and e2 must be ordered factors')
  if (ord1 && ord2 && !identical(
    attr(e1, which = 'levels', exact = TRUE), 
    attr(e2, which = 'levels', exact = TRUE)
  )) stop('ordered factors must have identical levels')
  
  tmp <- data.frame(e1, e2) # recycle the length
  e1 <- tmp[[1L]]
  e2 <- tmp[[2L]]
  n <- length(e1)
  
  ok1 <- !is.na(e1)
  ok2 <- !is.na(e2)
  ok <- ok1 & ok2
  e10 <- e1[ok]
  e20 <- e2[ok]
  .less <- (e10 < e20)
  .equal <- (e10 == e20)
  .greater <- (e10 > e20)
  
  lev <- paste0(name1, ' ', c('<', '=', '>'), ' ', name2)
  
  if (!all(ok1) && !all(ok2) && na.detail) { # returns 'character'
    ret <- character(length = n)
    ret[!ok1 & ok2] <- sprintf(fmt = 'missing %s', name1)
    ret[ok1 & !ok2] <- sprintf(fmt = 'missing %s', name2)
    ret[!ok1 & !ok2] <- 'missing both'
    ret[ok][.less] <- lev[1L]
    ret[ok][.equal] <- lev[2L]
    ret[ok][.greater] <- lev[3L]
  } else { # return 'ordered' 'factor'
    ret <- rep(NA_integer_, times = n)
    ret[ok][.less] <- 1L
    ret[ok][.equal] <- 2L
    ret[ok][.greater] <- 3L
    attr(ret, which = 'levels') <- lev
    class(ret) <- c('ordered', 'factor')
  }
  
  return(ret)
}






