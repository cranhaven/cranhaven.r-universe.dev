is_interactive <- function() {
    interactive() && (Sys.getenv("RSTUDIO") == "1" || isatty(stdin()))
}


# Log Time Difference with Customizable Output
.logDiffTime <- function(main = "", t1 = NULL, verbose = TRUE, addHeader = FALSE,
                         t2 = Sys.time(), units = "mins", header = "*****",
                         tail = "elapsed.", precision = 3) {
    if (verbose) {
        tryCatch(
            {
                # Calculate the time difference between t1 and t2
                dt <- abs(round(difftime(t2, t1, units = units), precision))

                # Format the message
                if (addHeader) {
                    msg <- sprintf(
                        "%s\n%s : %s, %s %s %s\n%s",
                        header, Sys.time(), main, dt, units, tail, header
                    )
                } else {
                    msg <- sprintf(
                        "%s : %s, %s %s %s",
                        Sys.time(), main, dt, units, tail
                    )
                }

                # Output the message
                message(msg)
            },
            error = function(x) {
                # If an error occurs, print the error message
                message("Time Error : ", x)
            }
        )
    }

    # Return invisibly to avoid unnecessary output
    return(invisible(0))
}

firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

wpca <- function(X, q, weighted = T) {
    if (!is.matrix(X)) stop("wpca: X must be a matrix!")
    if (q < 1) stop("wpca: q must be a positive integer!")
    X <- scale(X, scale = F) # centralize
    out <- wpcaCpp(X, q, weighted)
    return(out)
}

get_varfeature_fromSeurat <- function(seu, assay = NULL) {
    if (is.null(assay)) assay <- DefaultAssay(seu)

    if (inherits(seu[[assay]], "Assay5")) {
        var.features <- seu[[assay]]@meta.data$var.features
        var.features <- var.features[!is.na(var.features)]
    } else {
        var.features <- seu[[assay]]@var.features
    }
    return(var.features)
}

pdistance.matrix <- function(Ar, Br, eta = 1e-10) {
    dis <- pdistance_cpp(Ar, Br, eta = eta)
    rownames(dis) <- rownames(Ar)
    colnames(dis) <- rownames(Br)
    return(dis)
}

