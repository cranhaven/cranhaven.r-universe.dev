


#' @title `cbind.groupedHyperframe()`
#' 
#' @param ... see the function \link[spatstat.geom]{cbind.hyperframe}, 
#' the first element must be `'groupedHyperframe'`
#' 
#' @references
#' \url{https://tingtingzhan.quarto.pub/groupedhyperframe/topics.html}
#' 
#' @keywords internal
#' @importFrom spatstat.geom cbind.hyperframe
#' @export cbind.groupedHyperframe
#' @export
cbind.groupedHyperframe <- function(...) {
  
  x1 <- list(...)[[1L]]
  
  ret <- cbind.hyperframe(...) # or NextMethod() ?

  if (inherits(x1, what = 'groupedHyperframe')) {
    attr(ret, which = 'group') <- attr(x1, which = 'group', exact = TRUE)
    class(ret) <- c('groupedHyperframe', class(x1)) |> 
      unique.default()
  } # a bandage fix, for now
  
  return(ret)

}