### * All functions in this file are exported

### * Ops.topology()

# Based on
# ?groupGeneric
# ?.Generic
# https://stackoverflow.com/questions/35902360/r-implement-group-generics-ops-to-enable-comparison-of-s3-objects
# (cdeterman answer in the above)

#' Ops generics for \code{topology} objects
#'
#' @param e1 First operand
#' @param e2 Second operand
#'
#' @return Boolean (or throws an error for unsupported operators).
#'
#' @examples
#' topo(aquarium_mod) == topo(trini_mod)
#' topo(aquarium_mod) == topo(aquarium_mod)
#' 
#' @method Ops topology
#'
#' @export

Ops.topology <- function(e1, e2) {
    op <- .Generic[[1]]
    if (!(is(e1, "topology") & is(e2, "topology"))) {
        stop("Both objects must be topologies!")
    }
    switch(op,
           "==" = {
               # Equality
               if (ncol(e1) != ncol(e2)) {
                   return(FALSE)
               }
               if (!setequal(colnames(e1), colnames(e2))) {
                   return(FALSE)
               }
               e2 <- e2[match(colnames(e2), colnames(e1)), ]
               e2 <- e2[, match(rownames(e2), rownames(e1))]
               if (!all(unclass(e1) == unclass(e2))) {
                   return(FALSE)
               }
               e1_split <- attr(e1, "split")
               e2_split <- attr(e2, "split")
               split_compatible <- (
                   (is.null(e1_split) & is.null(e2_split)) |
                   (!is.null(e1_split) && !is.null(e2_split) &&
                    all(sort(e1_split) == sort(e2_split)))
               )
               if (!split_compatible) {
                   return(FALSE)
               }
               e1_steadyState <- attr(e1, "steadyState")
               e2_steadyState <- attr(e2, "steadyState")
               steadyState_compatible <- (
                   (is.null(e1_steadyState) & is.null(e2_steadyState)) |
                   (!is.null(e1_steadyState) && !is.null(e2_steadyState) &&
                    all(sort(e1_steadyState) == sort(e2_steadyState)))
               )
               if (!steadyState_compatible) {
                   return(FALSE)
               }
               return(TRUE)
           },
           "!=" = {
               # Difference
               return(!(e1 == e2))
           },
           stop("Undefined operation for topology objects: `", op, "`"))
}

### * Methods for nice display of topologies

### ** print.topology()

#' Pretty printing of a \code{topology} object
#'
#' @param x An object of class \code{topology}.
#' @param help If TRUE, display a short help after the topology object
#'     explaining e.g. the steady state or the split compartment symbols.
#' @param ... Not used.
#'
#' @return Mostly called for its side effect (printing).
#' 
#' @export

print.topology <- function(x, help = TRUE, ...) {
    attr(x, "class") <- NULL
    nComps <- ncol(x)
    steady <- attr(x, "steadyState")
    split <- attr(x, "split")
    merge <- attr(x, "merge")
    for (s in steady) {
        i <- which(colnames(x) == s)
        colnames(x)[i] <- paste0(s, "*")
        rownames(x)[i] <- paste0(s, "*")
    }
    for (s in split) {
        i <- which(colnames(x) == s)
        colnames(x)[i] <- paste0(s, "<|>")
        rownames(x)[i] <- paste0(s, "<|>")
    }
    attr(x, "steadyState") <- NULL
    attr(x, "split") <- NULL
    attr(x, "merge") <- NULL
    cat(paste0("<", nComps, " comps>"), "\n")
    print(x)
    if (help) {
        if (length(steady) > 0) {
            cat("[ * : steady-state]", "\n")
        }
        if (length(split) > 0) {
            cat("[<|>: split]", "\n")
        }
        if (!is.null(merge)) {
            for (i in seq_along(merge)) {
                cat("[", names(merge)[i], " = ", merge[[i]][1], " + ", merge[[i]][2], "]", "\n",
                    sep = "")
            }
        }
    }
    invisible(x)
}

### * as_tbl_graph() generic and method

#' Generic for as_tbl_graph()
#'
#' Convert a compatible object to a tbl_graph object (from the tidygraph package)
#'
#' @param x Object to convert to a tbl_graph.
#' @param ... Passed to the appropriate method.
#'
#' @return A tbl_graph object.
#'
#' @export

as_tbl_graph <- function(x, ...) {
    UseMethod("as_tbl_graph")
}

#' Convert a network topology to a tbl_graph
#'
#' @param x A network topology.
#' @param ... Not used.
#'
#' @return A tbl_graph object.
#'
#' @export

# Note for testing:
# https://community.rstudio.com/t/how-can-i-make-testthat-think-i-dont-have-a-package-installed/33441/2

as_tbl_graph.topology <- function(x, ...) {
    if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Package \"tidygraph\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("Package \"igraph\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    graph <- igraph::graph_from_adjacency_matrix(t(x))
    t_graph <- tidygraph::as_tbl_graph(graph)
    steady_state <- attr(x, "steadyState")
    split <- attr(x, "split")
    attributes <- tibble::tibble(name = colnames(x))
    attributes[["steady_state"]] <- attributes[["name"]] %in% steady_state
    attributes[["split"]] <- attributes[["name"]] %in% split
    t_graph <- dplyr::left_join(t_graph, attributes, by = "name")
    return(t_graph)
}
