twobit_read <- function(filepath)
{
    filepath <- normarg_filepath(filepath)
    .Call("C_twobit_read", filepath, PACKAGE="Rtwobitlib")
}

twobit_write <- function(x, filepath, use.long=FALSE, skip.dups=FALSE)
{
    ## Check 'x'.
    if (!is.character(x))
        stop("'x' must be a character vector")
    x_names <- names(x)
    if (is.null(x_names))
        stop("'x' must have names")
    if (anyNA(x))
        stop("'x' cannot contain NAs")
    if (anyNA(x_names) || !all(nzchar(x_names)))
        stop("the names on 'x' cannot contain NAs or empty strings")

    filepath <- normarg_filepath(filepath, for.writing=TRUE)

    if (!isTRUEorFALSE(use.long))
        stop("'use.long' must be TRUE or FALSE")

    if (!isTRUEorFALSE(skip.dups))
        stop("'skip.dups' must be TRUE or FALSE")

    .Call("C_twobit_write", x, filepath, use.long, skip.dups,
                            PACKAGE="Rtwobitlib")
    invisible(filepath)
}

