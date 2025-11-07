#' removes ERCC peaks and duplicated genes
#' @param mat pre-filters and orders bulk rna-seq data
#'
#' @examples
#' library(seAMLess)
#'
#' data("exampleTCGA")
#'
#' exampleTCGA <- wrangleMat(exampleTCGA)
#' @return filtered and ordered count-matrix
#' @export
wrangleMat <- function(mat) {
    # Remove spike-ins for now (may not exists in your data)
    mat <- mat[!grepl("^ERCC", mat[, 1]), ]

    # Eliminate any homologs
    # TODO could be dangerous to do it this way, find a better version...
    mat[, 1] <- sapply(strsplit(mat[, 1], ".", fixed = TRUE), function(x) {
        x[1]
    })

    # Order genes according to their standard deviation in decreasing order
    mat <- mat[rev(order(apply(mat[, -1], 1, stats::sd))), ]

    # Remove duplicated genes
    mat <- mat[!duplicated(mat[, 1]), ]

    # Make the gene names the row names
    rownames(mat) <- mat[, 1]

    # Filter the genes
    mat <- mat[, -1]

    # Enforce all counts to be integers
    mat <- round(mat, 0)

    return(mat)
}



#' verboseFn
#'
#' returns a printing function to be used with in the script
#' @param verbose boolean, determines whether the output going be printed or not
#'
#' @examples
#' # Prints output
#' verbosePrint <- verboseFn(TRUE)
#' verbosePrint("Hello World!")
#' # > "Hello World!"
#'
#' # Does not print
#' verbosePrint <- verboseFn(FALSE)
#' verbosePrint("Hello World!")
#' @return print function
#' @export
verboseFn <- function(verbose) {
    if (verbose) {
        return(function(...) {
            message(...)
        })
    } else {
        return(function(...) {
            return(invisible(NULL))
        })
    }
}
