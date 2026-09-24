
# this is a simplified version of experiment::SUBSET

#' @title Inspect a Subset of \link[base]{data.frame}
#' 
#' @description ..
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param subset \link[base]{logical} \link[base]{expression},
#' see function \link[base]{subset.data.frame}
#' 
#' @param select \link[base]{character} \link[base]{vector},
#' columns to be selected,
#' see function \link[base]{subset.data.frame}
#' 
#' @param select_pattern regular expression \link[base]{regex}
#' for multiple columns to be selected
#' 
#' @param avoid \link[base]{character} \link[base]{vector},
#' columns to be avoided
#' 
#' @param avoid_pattern regular expression \link[base]{regex},
#' for multiple columns to be avoided
#' 
#' @details 
#' Function [subset_] is different from 
#' \link[base]{subset.data.frame}, such that 
#' \itemize{
#' \item {if both `select` and `select_pattern` are missing, only variables mentioned in `subset` are selected;}
#' \item {be able to select all variables, except those in `avoid` and `avoid_pattern`;}
#' \item {always returns \link[base]{data.frame}, i.e., forces `drop = FALSE`}
#' }
#' 
#' @returns
#' Function [subset_] returns a \link[base]{data.frame}, with additional \link[base]{attributes}
#' \describe{
#' \item{`attr(,'vline')`}{\link[base]{integer} scalar,
#' position of a vertical line (see `?flextable::vline`)}
#' \item{`attr(,'jhighlight)'`}{\link[base]{character} \link[base]{vector},
#' names of columns to be `flextable::highlight`ed.}
#' }
#' 
#' 
#' @examples 
#' subset_(trees, Girth > 9 & Height < 70)
#' subset_(swiss, Fertility > 80, avoid = 'Catholic')
#' subset_(warpbreaks, wool == 'K')
#' 
#' @export
subset_ <- function(
    x, subset, 
    select, select_pattern, 
    avoid, avoid_pattern
) {
  
  if (!is.data.frame(x)) stop('input must be \'data.frame\'')
  if (isS4(x)) stop('Use S3 data.frame explicitly')
  
  nm <- names(x)
  .subset <- substitute(subset)

  select <- c(
    if (!missing(select)) select, 
    if (!missing(select_pattern)) grep(select_pattern, x = nm, value = TRUE)
  )
  
  avoid <- c(
    if (!missing(avoid)) avoid, 
    if (!missing(avoid_pattern)) grep(avoid_pattern, x = nm, value = TRUE)
  )
  
  var_subset <- intersect(all.vars(.subset), nm)
  
  var_sel <- if (!length(avoid)) {
    c(var_subset, select)
  } else if (!length(select)) {
    # `avoid`, but no `select`
    c(var_subset, setdiff(nm, avoid))
  } else {
    # `avoid`, `select`
    setdiff(x = c(var_subset, select), y = avoid)
  }
  var_sel <- nm[sort.int(match(unique.default(var_sel), table = nm))] # in original order
  
  rid <- which(eval(expr = .subset, envir = x)) # removes NA
  if (!length(rid)) {
    message('No subject satisfies that ', sQuote(deparse1(.subset)))
    return(invisible())
  }
  
  ret <- data.frame(
    '_Excel_' = rid + 1L,
    x[rid, var_sel, drop = FALSE],
    check.names = FALSE)
  attr(ret, which = 'vline') <- 1L
  attr(ret, which = 'jhighlight') <- var_subset
  return(ret)
}
