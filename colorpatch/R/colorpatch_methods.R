# Extends the colorspace class by a few methods.

#' Applies a function to each entry of a [colorspace::color-class] palette.
#'
#' @param X the color palette
#' @param FUN the function to be applied
#' @param ... extra arguments to FUN
#'
#' @return a list of each result of FUN applied to each entry in X
#' @import methods
#' @importClassesFrom colorspace color
#' @export
apply.color <- function(X, FUN, ...) {
  n <- length(X)
  R <- c()
  for (i in 1:n) {
    R <- c(R, FUN(X[i]), ...)
  }
  return(R)
}

.as.list.color <- function(from) {
  L <- list()
  for (i in 1:length(from)) {
    L[[i]] <- from[i]
  }
  return(L)
}

.append.color <- function(x, values, after = length(x)) {
  A <- colorspace::coords(as(x, "sRGB"))
  B <- colorspace::coords(as(values, "sRGB"))
  return(colorspace::sRGB(rbind(A, B)))
}

.length.color <- function(x) {
  return(nrow(colorspace::coords(x)))
}

#' Appends two palettes to form a single palette.
#'
#' Applies to the [colorspace::color-class] class.
#' 
#' @param x the color palette to be modified.
#' @param values another color palette to be appended
#' @param after currently unimplemented.
#' @rdname append-methods
#' @exportMethod append
#' @aliases append,color-method
setMethod("append", "color", .append.color)

#' Returns the length of a palette (the number of entries).
#'
#' Applies to the [colorspace::color-class] class.
#' @param x an color object
#' @rdname length-methods
#' @exportMethod length
#' @aliases length,color-method
setMethod("length", "color", .length.color)

#' Creates a list with single colors from a palette.
#' 
#' Applies to the [colorspace::color-class] class.
#' @name as.list
#' @rdname as.list-methods
#' @exportMethod as.list
#' @examples
#' data("OptimGreenRedLAB")
#' as.list(OptimGreenRedLAB)
setGeneric("as.list")

#' @param x color object to be coerced to a list
#' @param ... ignored for this class
#' @rdname  as.list-methods
#' @aliases as.list,color-method
setMethod("as.list", "color",
                   function(x, ...) return(.as.list.color(x)))

#' Transforms palette to list of single colors.
#'
#' Applies to the [colorspace::color-class] class.
#' @name as
#' @family color
#' @examples 
#' data("OptimGreenRedLAB")
#' as(OptimGreenRedLAB, "list")
setAs(from = "color", to = "list", .as.list.color)
