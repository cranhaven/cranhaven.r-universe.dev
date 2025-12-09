### * All functions in this file are exported

### * Description

# Methods for \code{coda::mcmc.list} objects:
#
# - Provide simple math operations for mcmc.list objects and also allows to use
# math functions such as log() or exp() on mcmc.list objects.
#
# - Provide simple interface to select parameters based on their name.
#
# Those methods are not designed specifically for this package, and could be
# submitted e.g. for integration in the coda package.

### * Helper functions / generics

### * Ops.mcmc.list()

# Based on
# ?groupGeneric
# ?.Generic
# https://stackoverflow.com/questions/35902360/r-implement-group-generics-ops-to-enable-comparison-of-s3-objects
# (cdeterman answer in the above)

#' Ops generics for \code{\link[coda]{mcmc.list}} objects
#'
#' @param e1 First operand
#' @param e2 Second operand
#'
#' @return A \code{mcmc.list} object (with the added class
#'     \code{derived.mcmc.list}).
#'
#' @examples
#' \dontrun{
#' # aquarium_run is a coda::mcmc.list object shipped with the isotracer package
#' a <- aquarium_run
#' plot(a)
#' # The calculations below are just given as examples of mathematical
#' # operations performed on an mcmc.list object, and do not make any sense
#' # from a modelling point of view.
#' plot(a[, "upsilon_algae_to_daphnia"] - a[, "lambda_algae"])
#' plot(a[, "upsilon_algae_to_daphnia"] + a[, "lambda_algae"])
#' plot(a[, "upsilon_algae_to_daphnia"] / a[, "lambda_algae"])
#' plot(a[, "upsilon_algae_to_daphnia"] * a[, "lambda_algae"])
#' plot(a[, "upsilon_algae_to_daphnia"] - 10)
#' plot(a[, "upsilon_algae_to_daphnia"] + 10)
#' plot(a[, "upsilon_algae_to_daphnia"] * 10)
#' plot(a[, "upsilon_algae_to_daphnia"] / 10)
#' plot(10 - a[, "upsilon_algae_to_daphnia"])
#' plot(10 + a[, "upsilon_algae_to_daphnia"])
#' plot(10 * a[, "upsilon_algae_to_daphnia"])
#' plot(10 / a[, "upsilon_algae_to_daphnia"])
#' }
#' 
#' @method Ops mcmc.list
#'
#' @export

Ops.mcmc.list = function(e1, e2) {
    # Modified from cdeterman answer from:
    # https://stackoverflow.com/questions/35902360/r-implement-group-generics-ops-to-enable-comparison-of-s3-objects
    op = .Generic[[1]]
    if ("mcmc.list" %in% class(e1) & "mcmc.list" %in% class(e2)) {
        areCompatible = function(m1, m2) {
            COMPATIBLE = TRUE
            if (coda::nvar(m1) != coda::nvar(m2)) {
                COMPATIBLE = FALSE
            } else if (coda::nchain(m1) != coda::nchain(m2)) {
                COMPATIBLE = FALSE
            } else if (!all(coda::varnames(m1) == coda::varnames(m1))) {
                COMPATIBLE = FALSE
            } else if (!all(coda::mcpar(m1[[1]]) == coda::mcpar(m2[[1]]))) {
                COMPATIBLE = FALSE
            }
            return(COMPATIBLE)
        }
        if (!areCompatible(e1, e2)) {
            stop("Incompatible inputs")
        }
        if (coda::nvar(e1) != 1) {
            stop("Ops implemented only for single-parameter chains")
        }
        switch(op,
               "+" = {
                   # Addition
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] + e2[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               "-" = {
                   # Subtraction
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] - e2[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               "*" = {
                   # Multiplication
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] * e2[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               "/" = {
                   # Division
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] / e2[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               stop("Undefined operation for mcmc.list objects"))
    } else if ("mcmc.list" %in% class(e1)) {
        stopifnot("mcmc.list" %in% class(e1) & ! "mcmc.list" %in% class(e2))
        stopifnot(is.numeric(e2) & length(e2) == 1)
        switch(op,
               "+" = {
                   # Addition
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] + e2
                   }
                   return(as.derived.mcmc.list(c))
               },
               "-" = {
                   # Subtraction
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] - e2
                   }
                   return(as.derived.mcmc.list(c))
               },
               "*" = {
                   # Multiplication
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] * e2
                   }
                   return(as.derived.mcmc.list(c))
               },
               "/" = {
                   # Division
                   c = e1
                   for (i in 1:coda::nchain(e1)) {
                       c[[i]] = c[[i]] / e2
                   }
                   return(as.derived.mcmc.list(c))
               },
               stop("Undefined operation for mcmc.list object and numeric")
               )
    } else {
        stopifnot("mcmc.list" %in% class(e2) & ! "mcmc.list" %in% class(e1))
        stopifnot(is.numeric(e1) & length(e1) == 1)
        switch(op,
               "+" = {
                   # Addition
                   c = e2
                   for (i in 1:coda::nchain(e2)) {
                       c[[i]] = c[[i]] + e1
                   }
                   return(as.derived.mcmc.list(c))
               },
               "-" = {
                   # Subtraction
                   c = e2
                   for (i in 1:coda::nchain(e2)) {
                       c[[i]] = e1 - c[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               "*" = {
                   # Multiplication
                   c = e2
                   for (i in 1:coda::nchain(e2)) {
                       c[[i]] = c[[i]] * e1
                   }
                   return(as.derived.mcmc.list(c))
               },
               "/" = {
                   # Division
                   c = e2
                   for (i in 1:coda::nchain(e2)) {
                       c[[i]] = e1 / c[[i]]
                   }
                   return(as.derived.mcmc.list(c))
               },
               stop("Undefined operation for mcmc.list object and numeric")
               )
    }
}

### * Math.mcmc.list()

#' Math generics for mcmc.list objects
#'
#' @param x \code{\link[coda]{mcmc.list}} object
#' @param ... Other arguments passed to corresponding methods
#'
#' @return A \code{mcmc.list} object (with the added class
#'     \code{derived.mcmc.list}).
#' 
#' @method Math mcmc.list
#'
#' @export

Math.mcmc.list = function(x, ...) {
    out = lapply(x, .Generic, ...)
    out = lapply(out, coda::mcmc)
    out = coda::mcmc.list(out)
    out = as.derived.mcmc.list(out)
    return(out)
}

### * select.mcmc.list()

#' Select parameters based on their names
#'
#' @param .data A \code{coda::mcmc.list} object.
#' @param ... Strings used to select variables using pattern matching with
#'     \code{grepl}.
#'
#' @return An \code{mcmc.list} object, with the same extra class(es) as
#'     \code{.data} (if any).
#' 
#' @method select mcmc.list
#' @export

select.mcmc.list <- function(.data, ...) {
    params <- coda::varnames(.data)
    patterns <- rlang::ensyms(...)
    patterns <- purrr::map(patterns, rlang::as_string)
    matches <- lapply(patterns, function(p) which(grepl(p, params)))
    matches <- sort(unique(unlist(matches)))
    if (length(matches) == 0) { return(NULL) }
    out <- .data[, matches]
    class(out) <- class(.data)
    return(out)
}


### * c.mcmc.list()

#' Combine mcmc.list objects
#'
#' @param ... \code{mcmc.list} objects.
#'
#' @return A \code{mcmc.list} object.
#'
#' @method c mcmc.list
#' @export

c.mcmc.list <- function(...) {
    z <- list(...)
    is_mcmc.list <- sapply(z, function(x) is(x, "mcmc.list"))
    if (!all(is_mcmc.list)) {
        stop("Not all arguments are mcmc.list objects.")
    }
    if (length(z) == 1) {
        return(z)
    }
    # Check objects compatibility
    areCompatible <- function(m1, m2) {
        COMPATIBLE <- TRUE
        if (coda::nchain(m1) != coda::nchain(m2)) {
            COMPATIBLE <- FALSE
        } else if (coda::niter(m1) != coda::niter(m2)) {
            COMPATIBLE <- FALSE
        } else if (!all(coda::mcpar(m1[[1]]) == coda::mcpar(m2[[1]]))) {
            COMPATIBLE <- FALSE
        }
        return(COMPATIBLE)
    }
    if (is.null(coda::mcpar(z[[1]]))) {
        if (is.null(coda::mcpar(z[[1]][[1]]))) {
            stop("One mcmc.list object has no mcpar attribute.")
        }
        attr(z[[1]], "mcpar") <- coda::mcpar(z[[1]][[1]])
    }
    for (i in 2:length(z)) {
        if (is.null(coda::mcpar(z[[i]]))) {
            if (is.null(coda::mcpar(z[[i]][[1]]))) {
                stop("One mcmc.list object has no mcpar attribute.")
            }
            attr(z[[i]], "mcpar") <- coda::mcpar(z[[i]][[1]])
        }
        compatible <- areCompatible(z[[1]], z[[i]])
        if (!compatible) {
            stop("Not all provided mcmc.list objects are compatible.")
        }
    }
    # Check that no variable is unnamed
    var_names <- lapply(z, coda::varnames)
    for (i in seq_along(var_names)) {
        if (is.null(var_names[[i]])) {
            # Check that there is only one variable
            if (coda::nvar(z[[i]]) > 1) {
                stop("One mcmc.list does not have a variable name and contains more than one variable.")
            }
            # Check that a name was provided
            if (is.null(names(z)) || names(z)[i] == "") {
                stop("Some mcmc.list have unnamed variables.\n",
                     "You should pass those mcmc.list to c(...) using named arguments.")
            }
            # Name the variable
            x <- z[[i]]
            var_name <- names(z)[i]
            mcpars <- coda::mcpar(x)
            x <- lapply(x, function(y) {
                out <- array(as.vector(y), dim = c(length(as.vector(y)), 1))
                colnames(out) <- var_name
                out <- coda::as.mcmc(out)
                attr(out, "mcpar") <- attr(y, "mcpar")
                out
            })
            attr(x, "mcpar") <- attr(z[[i]], "mcpar")
            class(x) <- class(z[[i]])
            z[[i]] <- x
        }
    }
    # Check that no variable names is present twice
    var_names <- lapply(z, coda::varnames)
    if (length(var_names) != length(unique(var_names))) {
        stop("Some variable names are duplicated.")
    }
    # Combine the variables
    n_chains <- coda::nchain(z[[1]])
    out <- lapply(seq_len(n_chains), function(i) {
        variables <- lapply(z, function(y) y[[i]])
        combined <- coda::as.mcmc(do.call(cbind, variables))
        attr(combined, "mcpar") <- attr(z[[1]][[1]], "mcpar")
        combined
    })
    out <- coda::as.mcmc.list(out)
    attr(out, "mcpar") <- attr(z[[1]], "mcpar")
    return(as.derived.mcmc.list(out))
}

### * `[.networkModelStanfit`()

#' Subset method for \code{networkModelStanfit} objects
#'
#' @param x A \code{networkModelStanfit} object.
#' @param i A vector of iteration indices.
#' @param j A vector of parameter names or indices.
#' @param drop Boolean.
#'
#' @return A \code{networkModelStanfit} object.
#' 
#' @method [ networkModelStanfit
#' 
#' @export

`[.networkModelStanfit` <- function(x, i, j, drop = TRUE) {
    o <- NextMethod()
    class(o) <- c("networkModelStanfit", class(o))
    return(o)
}
