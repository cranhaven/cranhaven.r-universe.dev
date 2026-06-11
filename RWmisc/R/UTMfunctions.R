#' UTM Convenience Functions
#'
#' Functions for converting latitude-longitude data to UTM.
#'
#' @name UTM.functions
#' @aliases UTM.functions
NULL

#' @rdname UTM.functions
#'
#' @param long A vector of longitude values.
#'
#' @return UTM vector of zone numbers.
#' @export
#'
#' @examples
#' long2UTM(c(-90, 0, 90))

long2UTM <- function(long) {

  floor((long + 180) / 6) %% 60 + 1

}

#' @rdname UTM.functions
#'
#' @return UTM vector of zone numbers.
#' @export
#'
#' @examples
#' UTMzones(c(-90, 90, 90))

UTMzones <- function(long) {

  unique(long2UTM(long))

}

#' @rdname UTM.functions
#'
#' @return UTM zone number.
#' @export
#'
#' @examples
#' chooseUTM(c(-90, -80, -70))

chooseUTM <- function(long) {

  zone <- round(mean(long2UTM(long)))

  zone

}
