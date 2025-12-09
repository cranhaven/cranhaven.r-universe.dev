### * All functions in this file are exported

### * topo()

#' Return the list of topologies, or a unique topology if all identical
#'
#' @param nm A \code{networkModel} object.
#' @param simplify Boolean, return only a unique topology if all topologies are
#'     identical or if there is only one? Default is TRUE.
#'
#' @return A list of the \code{networkModel} topologies or, if all topologies
#'     are identical (or if there is only one) and \code{simplify} is TRUE, a
#'     single topology (not wrapped into a single-element list).
#'
#' @examples
#' aquarium_mod
#' topo(aquarium_mod)
#'
#' trini_mod
#' topo(trini_mod)
#' @export

topo <- function(nm, simplify = TRUE) {
    out <- nm[["topology"]]
    if (simplify && length(unique(out)) == 1) {
        return(unique(out)[[1]])
    }
    return(out)
}

### * prop_family()

#' Return the distribution family for observed proportions
#'
#' @param nm A \code{networkModel} object.
#' @param quiet Boolean for being quiet about explaining the role of eta
#'     (default is \code{FALSE}).
#'
#' @return A character string describing the distribution family used to model
#'     observed proportions.
#'
#' @examples
#' prop_family(aquarium_mod)
#' prop_family(trini_mod)
#' 
#' @export

prop_family <- function(nm, quiet = FALSE) {
    z <- attr(nm, "prop_family")
    if (!quiet) {
        describe_z_eta("eta", z)
    }
    z
}

### * size_family()

#' Return the distribution family for observed sizes
#'
#' @param nm A \code{networkModel} object.
#' @param quiet Boolean for being quiet about explaining the role of zeta
#'     (default is \code{FALSE}).
#'
#' @return A character string describing the distribution family used to model
#'     observed sizes.
#'
#' @examples
#' size_family(aquarium_mod)
#' size_family(trini_mod)
#' 
#' @export

size_family <- function(nm, quiet = FALSE) {
    z <- attr(nm, "size_family")
    if (!quiet) {
        describe_z_eta("zeta", z)
    }
    z
}

### * groups() method for networkModel

#' Get the grouping for a \code{networkModel} object
#'
#' @param x A \code{networkModel} object.
#'
#' @return A tibble giving the grouping variable(s) for the input network
#'     model. This tibble is in the same order as the rows of the input network
#'     model. If the input network model did not have any grouping variable,
#'     returns \code{NULL}.
#' 
#' @importFrom dplyr groups
#' @method groups networkModel
#'
#' @examples
#' groups(aquarium_mod)
#' groups(trini_mod)
#' 
#' @export

groups.networkModel <- function(x) {
    nm_get_groups(x, error = FALSE)
}

### * priors()

#' Return the tibble containing the priors of a networkModel
#'
#' @param nm A \code{networkModel} object.
#' @param fix_set_params If TRUE, parameters for which a value is set are given a
#'     fixed value (i.e. their prior is equivalent to a point value).
#' @param quiet Boolean to control verbosity.
#'
#' @return A tibble giving the current priors defined for the input network
#'     model.
#'
#' @examples
#' priors(aquarium_mod)
#' priors(trini_mod)
#' 
#' @export

priors <- function(nm, fix_set_params = FALSE, quiet = FALSE) {
    if (fix_set_params) {
        set_params <- attr(nm, "parameterValues")
        if (!is.null(set_params)) {
            for (i in seq_len(nrow(set_params))) {
                myPrior <- constant_p(value = set_params[["value"]][i])
                nm <- set_prior(nm, myPrior, param = set_params[["parameter"]][i],
                                use_regexp = FALSE, quiet = quiet)
            }
        }
    }
    out <- attr(nm, "priors")
    return(out)
}

### * missing_priors()

#' Get a table with parameters which are missing priors
#'
#' @param nm A \code{networkModel} object.
#'
#' @return A tibble containing the parameters which are missing a prior. If no
#'     priors are missing, the tibble contains zero row.
#'
#' @examples
#'# Using a subset of the topology from the Trinidad case study
#' m <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi, FBOM", "epi -> petro, pseph")
#'
#' # No prior is set by default
#' priors(m)
#'
#' # Set some priors
#' m <- set_priors(m, normal_p(0, 10), "lambda")
#' priors(m)
#'
#' # Which parameters are missing a prior?
#' missing_priors(m)
#'
#' @export

missing_priors <- function(nm) {
    p <- priors(nm)
    p <- p[sapply(p$prior, is.null), ]
    return(p)
}

### * params()

#' Return the parameters of a network model
#'
#' @param nm A \code{networkModel} object.
#' @param simplify If \code{TRUE}, return a vector containing the names of all
#'     model parameters (default: \code{FALSE}).
#'
#' @return A tibble containing the parameter names and their current value (if
#'     set). If \code{simplify} is \code{TRUE}, only return a sorted character
#'     vector containing the parameters names.
#'
#' @examples
#' params(aquarium_mod)
#' params(trini_mod)
#' params(trini_mod, simplify = TRUE)
#' 
#' @export

params <- function(nm, simplify = FALSE) {
    stopifnot("parameters" %in% colnames(nm))
    params <- dplyr::bind_rows(nm[["parameters"]])
    if (simplify) {
        return(sort(unique(params[["in_model"]])))
    }
    if (!"value" %in% colnames(params)) {
        params[["value"]] <- as.numeric(rep(NA, nrow(params)))
    }
    params <- unique(params[, c("in_model", "value")])
    params <- params[order(params[["in_model"]]), ]
    if (!all(table(params[["in_model"]]) == 1)) {
        stop("Some parameters have two different values assigned to them.")
    }
    return(params)
}

### * comps()

#' Return the compartments of a network model
#'
#' @param nm A \code{networkModel} object.
#'
#' @return A list of character vectors, with one list element per row of the
#'     input network model (list elements are in the same order as the input
#'     network model rows). Each list element containing the names of the
#'     compartments in the topology defined in the corresponding row of the
#'     input network model.
#'
#' @examples
#' aquarium_mod
#' comps(aquarium_mod)
#'
#' trini_mod
#' comps(trini_mod)
#' 
#' @export

comps <- function(nm) {
    comps <- nm[, "topology"]
    comps$compartments <- lapply(comps$topology, colnames)
    return(comps[["compartments"]])
}

