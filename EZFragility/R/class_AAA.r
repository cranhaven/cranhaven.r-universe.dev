## This file will take precedence over all the other class files
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))

printSlotValue <- function(object, slotName, k = 3) {
    val <- methods::slot(object, slotName)
    if (is.null(val)) {
        msg <- glue::glue("{slotName}: NULL")
    } else if (is.matrix(val) || is.array(val)) {
        truncated <- if (length(as.vector(val)) > k) "..." else ""
        msg <- glue::glue(
            "{slotName} ({paste(dim(val), collapse=' x ')}): ",
            "{paste(head(as.vector(val), k), collapse=', ')}{truncated}"
        )
    } else if (is.numeric(val)) {
        if (length(val) > k) {
            msg <- glue::glue(
                "{slotName} ({length(val)}): ",
                "{paste(head(val, k), collapse=', ')}..."
            )
        } else {
            msg <-
                glue::glue(
                    "{slotName}: ",
                    "{paste(val, collapse=', ')}"
                )
        }
    } else {
        msg <- glue::glue("{slotName}: {val}")
    }
    cat(msg)
    cat("\n")
}

# Automatically collapses every vector len > 1 (joined by "j") and use "s" (sep)
jn <- \(..., j = "", s = "") {
    fn <- \(x) if (length(x) > 1) paste0(x, collapse = j) else x
    ARGS <- lapply(list(...), fn)
    do.call(paste, c(ARGS, sep = s))
}

# extract properties of the object's members
slotSpecs <- \(x, k = 3, dm = dim(x), vec = is.null(dm), len = length(x)) {
    val <- x[seq_len(min(k, len))]
    if (is.null(x)) {
        return(c(cl = "NULL", d = "[NULL]:", v = " NULL"))
    }
    if (is.double(x)) val <- sprintf("%7.4f", val)
    v <- jn(val, if (len > k) "...", j = ", ")
    if (length(v) == 0)
        v <- ""
    c(
        cl = class(x)[1L],
        d = jn("[", if (vec) len else dm, "]", j = ","),
        v = v
    )
}

# Class printing
printSlots <- \(object, nb = 1, slots = NULL) {
    colN <- c("Slot", "Class", "DIM/LEN", " Values")
    maxL <- sapply(colN, nchar)
    ftb <- \(i) paste0("%", -i, "s")
    meta <- list()
    slots <- if (is.null(slots)) methods::slotNames(object) else slots
    for (n in slots) {
        x <- c(name = n, slotSpecs(methods::slot(object, n)))
        for (i in seq_along(maxL)) maxL[i] <- max(nchar(x[[i]]), maxL[[i]])
        meta[[n]][names(x)] <- x
    }
    fmt <- list(fmt = sapply(maxL, ftb) |> jn(j = " | "))
    header <- do.call(sprintf, c(fmt, colN)) |> shift(nb)
    dash <- rep("-", nchar(header) + nb) |> jn()
    cat(dash, header, dash, sep = "\n")
    for (x in meta) {
        do.call(sprintf, c(fmt, x)) |>
            shift(nb) |>
            cat("\n")
    }
    dash |> cat("\n")
}
