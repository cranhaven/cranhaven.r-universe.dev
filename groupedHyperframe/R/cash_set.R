

#' @title Syntactic Sugar for [groupedHyperframe] Object
#' 
#' @param x a groupedHyperframe
#' 
#' @param name,value see function \link[spatstat.geom]{$<-.hyperframe}
#' 
#' @note
#' The function \link[spatstat.geom]{$<-.hyperframe} drops \link[base]{attributes}.
#' 
#' @returns 
#' Syntactic sugar [$<-.groupedHyperframe()] returns a [groupedHyperframe].
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom $<-.hyperframe
#' @export $<-.groupedHyperframe
#' @export
`$<-.groupedHyperframe` <- function(x, name, value) {
  
  group <- attr(x, which = 'group', exact = TRUE)
  if (name %in% all.vars(group)) stop('do not allow changing variables in grouping structure')
  
  ret <- `$<-.hyperframe`(x, name, value)
  attr(ret, which = 'group') <- group
  class(ret) <- c('groupedHyperframe', class(ret)) |> 
    unique.default()
  return(ret)
  
}
