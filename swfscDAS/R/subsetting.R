#' Subsetting objects created using swfscDAS
#'
#' Subsetting \code{das_dfr} or \code{das_df} objects
#'
#' @param x object of class \code{das_dfr} or \code{das_df}
#' @param i,j,... elements to extract or replace, see \code{\link[base]{[.data.frame}}
#' @param drop logical, see \code{\link[base]{[.data.frame}}
#' @param name A literal character string or ..., see \code{\link[base]{[.data.frame}}
#' @param value A suitable replacement value, see \code{\link[base]{[.data.frame}}
#'
#' @details
#' When subsetting a \code{das_dfr} or \code{das_df} object, henceforth a \code{das_} object,
#' using any of the functions described in \code{\link[base]{[.data.frame}},
#' then then the \code{das_} class is simply dropped and the object is of class \code{data.frame}.
#' This is because of the strict format requirements of \code{das_} objects;
#' it is likely that a subsetted \code{das_} object will not have
#' the format required by subsequent swfscDAS functions,
#' and thus it is safest to drop the \code{das_} class.
#' If a data frame is passed to downstream \code{swfscDAS} functions that require a \code{das_} object,
#' then they will attempt to coerce the object to the necessary \code{das_} class
#' See \code{\link{as_das_dfr}} and \code{\link{as_das_df}} for more details.
#'
#' @name subsetting
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.read <- das_read(y)
#'
#' # All return a data frame:
#' class(y.read[1:10, ])
#' class(y.read[, 1:10])
#'
#' y.df <- y.read
#' y.df[, 1] <- "a"
#' class(y.df)
#'
#' y.df <- y.read
#' y.df$Event <- "a"
#' class(y.df)
#'
#' y.df <- y.read
#' y.df[["Event"]] <- "a"
#' class(y.df)


#' @rdname subsetting
#' @export
`[.das_dfr` <- function(x, i, j, ..., drop = TRUE) {
  class(x) <- setdiff(class(x), "das_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`$<-.das_dfr` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "das_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[<-.das_dfr` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "das_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[[<-.das_dfr` <- function(x, i, value) {
  class(x) <- setdiff(class(x), "das_dfr")
  NextMethod()
}



#' @rdname subsetting
#' @export
`[.das_df` <- function(x, i, j, ..., drop = TRUE) {
  class(x) <- setdiff(class(x), "das_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`$<-.das_df` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "das_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[<-.das_df` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "das_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[[<-.das_df` <- function(x, i, value) {
  class(x) <- setdiff(class(x), "das_df")
  NextMethod()
}
