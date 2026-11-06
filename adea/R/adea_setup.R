#' Standarize input and output
#'
#' Input and outputs are converted, if require, into a matrix.
#' The names of the columns and rows are also set in both.
#' When input and output arguments do not provide names, standard ones are given.
#' This functions does not check the arguments, so call the adea_check function first if necessary.
#'
#' @keywords internal
#' @inheritParams adea
#' @return a list with input and output converted
adea_setup <- function(input, output, ux = NULL, vy = NULL)
{
    ## Check for vectors and build matrix
    input <- as.matrix(input)
    output <- as.matrix(output)

    ## Setup DMU names
    if (!is.null(rownames(input)) && is.null(rownames(output))) rownames(output) <- rownames(input)
    if (is.null(rownames(input)) && !is.null(rownames(output))) rownames(input) <- rownames(output)
    
    l <- list(input = input, output = output)
    
    ## Check for vectors and build matrix, setup names
    if (!missing(ux)) {
        ux <- as.matrix(ux)
        colnames(ux) <- colnames(input)
        rownames(ux) <- rownames(input)
        l$ux <- ux
    }
    if (!missing(vy)) {
        vy <- as.matrix(vy)
        colnames(vy) <- colnames(output)
        rownames(vy) <- rownames(output)
        l$vy <- vy
    }

    l    
}
