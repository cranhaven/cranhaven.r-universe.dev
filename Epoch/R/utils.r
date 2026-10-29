tryToNum <- function(x) {
    x <- tryCatch(as.numeric(x), error = function(e) x, warning = function(w) x)
    if (is.numeric(x)) {
        return(x)
    } else {
        return(NULL)
    }
}


isWholeNumber <- function(x) {
    stopifnot(is.numeric(x))
    sapply(x, function(y) y %% 1 == 0)
}


.message <- function(verbose, ...) {
    if (verbose) {
        message(...)
    }
}


#' Check and keep valid index only
#'
#' @param indices Numeric or character index to check
#' @param names Character. All names corresponding to the indices
.checkIndex <- function(indices, names) {
    if (length(names) == 0) {
        return()
    }
    if (length(indices) == 0) {
        return()
    }
    if (is(indices, "numeric")) {
        allIndices <- seq_along(names)
        diffIndices <- setdiff(indices, allIndices)
        indicesFiltered <- indices[!indices %in% diffIndices]
        result <- indicesFiltered
    } else {
        diffIndices <- setdiff(indices, names)
        indicesFiltered <- indices[!indices %in% diffIndices]
        result <- which(names %in% indicesFiltered)
    }
    if (length(diffIndices)) {
        indicesMissing <- paste(diffIndices, collapse = ", ")
        indicesExist <- paste(indicesFiltered, collapse = ", ")
        warning(
            glue("Indices {indicesMissing} are out of range. I will keep the valid values {indicesExist}.")
        )
    }
    result
}



#' Standardize iEEG row data for plotting
#'
#' @param data Matrix or data frame of iEEG data
#' @param standardize Logical or numeric vector. If logical, indicates whether to standardize each row. If numeric, indicates the scaling factor for each row.
#' @param gap Numeric. The gap to separate different electrodes in the plot.
#' @return Standardized data matrix
.standardizeIEEG <- function(data, standardize = TRUE, gap = 2) {
    # Simple standardization - can be enhanced based on needs
    if (is.data.frame(data)) {
        data <- as.matrix(data)
    }
    # recycle to match length
    if (is.logical(standardize)) {
        if (length(standardize) == 1) {
            standardize <- rep(standardize, nrow(data))
        } else if (length(standardize) != nrow(data)) {
            stop("Length of logical standardize vector must be 1 or equal to the number of electrodes.")
        }
    } else if (is.numeric(standardize)) {
        if (length(standardize) != nrow(data)) {
            stop("Length of numeric standardize vector must be equal to the number of electrodes.")
        }
    } else {
        stop("standardize parameter must be either logical or numeric vector.")
    }

    mat <- data
    global_max <- max(data, na.rm = TRUE)
    global_min <- min(data, na.rm = TRUE)
    ratio <- gap / (global_max - global_min + 1) 
    mat <- (mat - global_min) * ratio - ((global_max - global_min) / 2)

    for (i in seq_len(nrow(data))) {
        mean_val <- mean(data[i, ], na.rm = TRUE)
        sd_val <- sd(data[i, ], na.rm = TRUE)
        
        if (is.logical(standardize)) {
            if (standardize[i]) {
                mat[i, ] <- (data[i, ] - mean_val) / (sd_val + 1)
            }
        } else if (is.numeric(standardize)) {
            mat[i, ] <- (data[i, ] - mean_val) * (standardize[i] / (sd_val + 1) )
        }
    }
    mat
}
