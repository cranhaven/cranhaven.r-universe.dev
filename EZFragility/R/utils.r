isWholeNumber <- function(x) {
    return(x %% 1 == 0)
}

# Helper function for rolling average
rolling_mean <- function(x, window) {
    if (length(x) < window) return(x)
    result <- rep(NA, length(x))
    for (i in window:length(x)) {
        result[i] <- mean(x[(i-window+1):i], na.rm = TRUE)
    }
    # Fill the beginning with the first valid value
    first_valid <- which(!is.na(result))[1]
    if (!is.na(first_valid)) {
        result[1:(first_valid-1)] <- result[first_valid]
    }
    result
}

# Shifts to the right all strings of a list with a number of blanks
shift <- \(strL, nBlanks = 0) {
    pre <- paste(rep(" ", nBlanks), collapse = "")
    lapply(strL, \(x) sprintf("%s%s", pre, x)) |> unlist()
}

#' Check and keep valid index only
#'
#' @param indices Numeric or character index to check
#' @param names Character. All names corresponding to the indices
checkIndex <- function(indices, names) {
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

# Try to convert a vector to numeric
# If not possible, this does not raise a warning or error
# but returns NULL
tryToNum <- function(x) {
    x <- tryCatch(as.numeric(x), error = function(e) x, warning = function(w) x)
    if (is.numeric(x)) {
        return(x)
    } else {
        return(NULL)
    }
}

# Only check a single logical condition
.ifelse <- function(test, yes, no) {
    if (test) {
        return(yes)
    } else {
        return(no)
    }
} 