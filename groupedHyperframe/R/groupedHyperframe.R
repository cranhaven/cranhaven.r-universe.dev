
.info_groupedHyperframe <- \(x) {
  
  f <- x |>
    attr(which = 'group', exact = TRUE) |> 
    get_nested_factors(data = x)
  ns <- f |> 
    seq_along() |> 
    vapply(FUN = \(i) { # (i = 1L)
      f[seq_len(i)] |>
        interaction(drop = TRUE, lex.order = TRUE) |>
        levels() |>
        length()
    }, FUN.VALUE = NA_integer_) # names dropped by ?base::vapply
  
  mapply(
    FUN = \(n, g) {
      paste(n, g |> col_blue() |> style_bold())
    }, n = ns, g = names(f), SIMPLIFY = TRUE
  ) |> 
    rev.default() |> 
    paste(collapse = ' nested in\n')

}


#' @title Print [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [print.groupedHyperframe()] does not have a returned value.
#' 
#' @seealso `?nlme:::print.groupedData`
#' 
#' @keywords internal
#' @importFrom spatstat.geom as.data.frame.hyperframe
#' @export print.groupedHyperframe
#' @export
print.groupedHyperframe <- function(x, ...) {
  
  x |>
    attr(which = 'group', exact = TRUE) |>
    deparse1() |>
    sprintf(fmt = 'Grouped Hyper Data Frame: %s') |>
    cat()
  
  cat('\n\n')
  x |>
    .info_groupedHyperframe() |>
    cat()
  cat('\n\n')
  
  # see inside ?spatstat.geom::print.hyperframe
  x |>
    as.data.frame.hyperframe(discard = FALSE) |> 
    print(...)
  
}

#' @title Summary Information of [groupedHyperframe]
#' 
#' @param object a [groupedHyperframe]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [summary.groupedHyperframe()] returns an R object of class `'summary.groupedHyperframe'`.
#' 
#' @keywords internal
#' @importFrom spatstat.geom summary.hyperframe
#' @export summary.groupedHyperframe
#' @export
summary.groupedHyperframe <- function(object, ...) {
  
  z <- object |>
    summary.hyperframe()
  attr(z, which = 'group') <- object |>
    attr(which = 'group', exact = TRUE)
  attr(z, which = 'group_size') <- object |>
    .info_groupedHyperframe()
  class(z) <- c('summary.groupedHyperframe', class(z)) |>
    unique.default()
  return(z)
  
}






#' @title Print Summary Information of [groupedHyperframe]
#' 
#' @param x a [summary.groupedHyperframe] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [print.summary.groupedHyperframe()] does not have a returned value.
#' 
#' @keywords internal
# @importFrom spatstat.geom print.summary.hyperframe
#' @method print summary.groupedHyperframe
#' @export print.summary.groupedHyperframe
#' @export
print.summary.groupedHyperframe <- function(x, ...) {

  # see inside ?spatstat.geom::print.summary.hyperframe
  
  x |>
    attr(which = 'group', exact = TRUE) |>
    deparse1() |>
    sprintf(fmt = 'Grouped Hyper Data Frame: %s') |>
    cat()
  
  cat('\n\n')
  x |>
    attr(which = 'group_size', exact = TRUE) |>
    cat()
  cat('\n\n')
  
  if (any(x$storage == "dfcolumn")) {
    x$allcols |>
      print()
  } else {
    x$classes |> 
      noquote() |>
      print()
  }
  
  return(invisible())  
  
}
















#' @title Extract Subset of [groupedHyperframe]
#' 
#' @param x a [groupedHyperframe]
#' 
#' @param ... additional parameters of \link[spatstat.geom]{[.hyperframe}
#' 
#' @returns
#' The function \link{[.groupedHyperframe} returns a [groupedHyperframe] or a \link[spatstat.geom]{hyperframe}.
#' 
#' @keywords internal
#' @importFrom spatstat.geom [.hyperframe
#' @export [.groupedHyperframe
#' @export
`[.groupedHyperframe` <- function(x, ...) {
  
  # a super genius fix! 
  # working on the lowest function `[` :))
  # no longer needed to write
  # .. [subset.groupedHyperframe()]
  # .. probably [split.groupedHyperframe()]
  
  ret <- `[.hyperframe`(x, ...)
  
  # a bandage fix hahaha
  group <- attr(x, which = 'group', exact = TRUE)
  if (!all(all.vars(group) %in% names(ret))) return(ret) # just 'hyperframe'
  attr(ret, which = 'group') <- group
  class(ret) <- c('groupedHyperframe', class(ret)) |> unique.default()
  return(ret)
  
}




# @title Extract Grouping Formula from [groupedHyperframe]
# @description ..
# @param object a [groupedHyperframe]
# @param asList,sep place holders for S3 generic \link[nlme]{getGroupsFormula}
# @returns 
# Function [getGroupsFormula.groupedHyperframe()] returns a one-sided \link[stats]{formula}
# @note
# tzh mask this for now, does not want to import(nlme) only for this
# @keywords internal
# @importFrom nlme getGroupsFormula
# @export getGroupsFormula.groupedHyperframe
# @export
#getGroupsFormula.groupedHyperframe <- function(object, asList, sep) {
#  attr(object, which = 'group', exact = TRUE)
#}

