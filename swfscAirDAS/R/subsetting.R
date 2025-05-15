#' Subsetting objects created using swfscAirDAS
#'
#' Subsetting \code{airdas_dfr} or \code{airdas_df} objects
#'
#' @param x object of class \code{airdas_dfr} or \code{airdas_df}
#' @param i,j,... elements to extract or replace, see \code{\link[base]{[.data.frame}}
#' @param drop logical, see \code{\link[base]{[.data.frame}}
#' @param name A literal character string or ..., see \code{\link[base]{[.data.frame}}
#' @param value A suitable replacement value, see \code{\link[base]{[.data.frame}}
#'
#' @details
#' When subsetting a \code{airdas_dfr} or \code{airdas_df} object, 
#' henceforth a \code{airdas_} object,
#' using any of the functions described in \code{\link[base]{[.data.frame}},
#' then then the \code{airdas_} class is simply dropped and the object is of class \code{data.frame}.
#' This is because of the strict format requirements of \code{airdas_} objects;
#' it is likely that a subsetted \code{airdas_} object will not have
#' the format required by subsequent swfscAirDAS functions,
#' and thus it is safest to drop the \code{airdas_} class.
#' If a data frame is passed to downstream \code{swfscAirDAS} functions 
#' that require a \code{airdas_} object,
#' then they will attempt to coerce the object to the necessary \code{airdas_} class
#' See \code{\link{as_airdas_dfr}} and \code{\link{as_airdas_df}} for more details.
#'
#' @name subsetting
#'
#' @examples
#' y <- system.file("airdas_sample.das", package = "swfscAirDAS")
#' y.read <- airdas_read(y)
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
`[.airdas_dfr` <- function(x, i, j, ..., drop = TRUE) {
  class(x) <- setdiff(class(x), "airdas_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`$<-.airdas_dfr` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "airdas_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[<-.airdas_dfr` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "airdas_dfr")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[[<-.airdas_dfr` <- function(x, i, value) {
  class(x) <- setdiff(class(x), "airdas_dfr")
  NextMethod()
}



#' @rdname subsetting
#' @export
`[.airdas_df` <- function(x, i, j, ..., drop = TRUE) {
  class(x) <- setdiff(class(x), "airdas_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`$<-.airdas_df` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "airdas_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[<-.airdas_df` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "airdas_df")
  NextMethod()
}

#' @rdname subsetting
#' @export
`[[<-.airdas_df` <- function(x, i, value) {
  class(x) <- setdiff(class(x), "airdas_df")
  NextMethod()
}
