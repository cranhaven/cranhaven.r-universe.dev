# Generic functions for dealing with haven labelled vectors while
# avoiding importing those packages, which have many dependencies.


#' @exportS3Method as.character haven_labelled
as.character.haven_labelled <- function(x, ...) {
    y <- character(length(x))
    y[seq_along(x)] <- x[seq_along(x)]

    return(y)
}


#' @exportS3Method as.integer haven_labelled
as.integer.haven_labelled <- function(x, ...) {
    y <- integer(length(x))
    y[seq_along(x)] <- x[seq_along(x)]

    return(y)
}


#' @exportS3Method as.double haven_labelled
as.double.haven_labelled <- function(x, ...) {
    y <- double(length(x))
    y[seq_along(x)] <- x[seq_along(x)]

    return(y)
}


# as.factor() is not a generic function, so I have to write my own instead of a
# as.factor.haven_labelled(), which cannot exist.
# As to why I even have this function in the package, it's so that I can use
# haven_to_factor() inside display_rows() to preview the contents of a
# haven_labelled variable while showing both its real value and its
# labelled value.
haven_to_factor <- function(x, ...) {
    lab  <- attr(x, "label")
    levs <- attr(x, "labels")

    # Prefix values on labels.
    labs <- sprintf("%i (%s)", levs, names(levs))

    fac <- factor(as.character(x),
                  levels = levs,
                  labels = labs,
                  ...)

    attr(fac, "label") <- lab

    return(fac)
}
