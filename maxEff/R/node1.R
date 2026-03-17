
#' @title Dichotomize via 1st Node of Recursive Partitioning
#' 
#' @param object an \link[rpart]{rpart.object}
#' 
#' @param nm \link[base]{symbol}, or \link[base]{name}, of the variable being partitioned
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [node1()] returns an object of class `'node1'`, 
#' which is a \link[base]{function}
#' with one parameter `newx` taking a \link[base]{double} \link[base]{vector}.
#' 
#' @note
#' In future \link[base]{integer} and \link[base]{factor} predictors will be supported.
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/object/node1.html}
#' 
#' @examples
#' rpart::rpart(
#'  formula = survival::Surv(pgtime, pgstat) ~ age, 
#'  data = rpart::stagec[1:135,], 
#'  cp = .Machine$double.eps, maxdepth = 2L
#' ) |>
#'  node1()
#' 
#' @keywords internal
#' @export
node1 <- function(object, nm = as.symbol(rownames(s)[1L]), ...) {
  
  s <- object$splits
  if (!length(s)) {
    if (object$control$cp > .Machine$double.eps) {
      stop('re-run rpart(., cp = .Machine$double.eps) to force a split')
    } else stop('really?')
  }
  
  trm <- object |>
    terms() # ?stats:::terms.default
  dc <- trm |>
    attr(which = 'dataClasses', exact = TRUE)
  if (length(dc) != 2L) stop('`rpart` must have one endpoint and one predictor')

  labs <- labels(object) # ?rpart:::labels.rpart

  switch(EXPR = dc[[2L]], 'numeric' = {
    
    nd1 <- labs[2L] |> # first node!!!
      str2lang() 
    
    if (nd1[[1L]] == '<=') {
      nd1[[1L]] <- quote(`>`)
    } else if (nd1[[1L]] == '<') {
      nd1[[1L]] <- quote(`>=`)
    } # else if (nd1[[1L]] is '>' or '>=')  do nothing
    
    nd1[[2L]] <- quote(newx)
    
    nd1[[3L]] <- s[1L, 4L] # threshold, in case `labels` are truncated due to `digits`
    
    fn_ <- nm |>
      call(name = 'alist', newx = _) |> 
      eval()
    
    fn_[[2L]] <- call(
      name = '{',
      call(
        name = '<-', 
        quote(ret), 
        call(name = '(', nd1)
      ),
      call(name = '<-', quote(ret0), call(name = 'na.omit', quote(ret))),
      quote(if ((length(ret0) > 1L) && (all(ret0) || !any(ret0))) warning('Dichotomized values are all-0 or all-1')),
      quote(return(ret))
    )
    
  }, 'factor' = {
    
    nd1 <- strsplit(x = labs[2L], split = '=')[[1L]][2L]
    letter1 <- strsplit(x = nd1, split = '')[[1L]]
    if (length(letter1) > 26L) stop('rpart::rpart() should have error here..')
    id1 <- letter1 |> 
      match(table = letters)
    
    fn_ <- nm |>
      call(name = 'alist', newx = _) |> 
      eval()
    
    fn_[[2L]] <- call(
      name = '{',
      call(
        name = '<-', 
        quote(ret), 
        call(
          name = '%in%', 
          call(name = 'unclass', quote(newx)),
          do.call(what = 'call', args = c(list(name = 'c'), as.list(id1)))
        )
      ),
      call(name = '<-', quote(ret0), call(name = 'na.omit', quote(ret))),
      quote(if ((length(ret0) > 1L) && (all(ret0) || !any(ret0))) warning('Dichotomized values are all-0 or all-1')),
      quote(return(ret))
    )
    
  }, stop('not supported!'))
  
  
  .fn <- fn_ |> 
    # as.function.default(envir = new.env()) |> # NO!! read ?base::new.env very carefully! Default creates a child of current environment
    as.function.default()
  # prefix dot (.) will not show up in ls(., all.names = FALSE)
  
  class(.fn) <- c(
    dc[[2L]] |> sprintf(fmt = 'node1_%s'), 
    'node1', 
    class(.fn)
  )

  # clean the enclosure envir of `.fn` as much as possible
  rm(list = c(
    # '.fn', # no!! otherwise nothing to return ..
    '...', 
    'fn_', 'labs', 'nd1', 'nm', 'object', 's', 'dc', 'trm'
  ), envir = environment(.fn))
  
  return(.fn)
  
}



#' @title Predict by [node1()]
#' 
#' @param object a [node1] object
#' 
#' @param newdata a \link[base]{data.frame} or \link[spatstat.geom]{hyperframe}
#' 
#' @param ... place holder for `S3` generic
#' 
#' @keywords internal
#' @importFrom spatstat.geom with.hyperframe
#' @export predict.node1
#' @export
predict.node1 <- function(object, newdata, ...) {
  
  if (inherits(newdata, what = 'data.frame')) {
    
    formals(object)$newx |> 
      eval(envir = newdata) |>
      object()

  } else if (inherits(newdata, what = 'hyperframe')) {
    
    formals(object)$newx |>
      with.hyperframe(data = newdata, ee = _) |>
      object()
    
  }
  
}







#' @title Get Cutoff Value from a Dichotomizing Rule [node1()]
#' 
#' @description
#' To get the cutoff value from a Dichotomizing Rule [node1()].
#' 
#' @param x see Usage
#' 
#' @keywords internal
#' @name get_cutoff
#' @export
get_cutoff <- function(x) UseMethod(generic = 'get_cutoff')


#' @rdname get_cutoff
#' 
#' @returns
#' The `S3` method [get_cutoff.node1()] returns a \link[base]{numeric} scalar.
#' 
#' @export get_cutoff.node1
#' @export
get_cutoff.node1 <- function(x) {
  body(x)[[2L]][[3L]][[2L]][[3L]]
}



#' @title Find \link[base]{labels} from [node1] 
#' 
#' @param object a [node1] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The `S3` method [labels.node1()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export labels.node1
#' @export
labels.node1 <- function(object, ...) {
  
  z1 <- formals(object)$newx |> 
    deparse1()
  
  b. <- body(object)
  
  if (inherits(object, what = 'node1_numeric')) {
    z2 <- (b.[[2L]][[3L]][[2L]][c(1L,3L)]) |>
      deparse1()
    return(paste0(z1, z2))
  } else if (inherits(object, what = 'node1_factor')) {
    z2 <- (b.[[2L]][[3L]][[3L]]) |>
      deparse1()
    return(paste(z1, z2, sep = ' in levels '))
  } 

}


