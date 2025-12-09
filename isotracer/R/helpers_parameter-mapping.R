### * None of the functions in this file is exported

### * add_param_mapping()

#' Build the (default) parameter mapping in a \code{networkModel} object
#'
#' For now this function just builds the default base mapping. Improvements
#' are: taking into account fixed effect of discrete variables and updating
#' gracefully when grouping or topology is modified. Maybe this is not doable
#' in a clean way, and the best option is just to reset the mapping when
#' grouping or topo is modified and warn the user.
#'
#' This function also sets the default priors
#'
#' @param nm A \code{networkModel} object.
#'
#' @return A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

add_param_mapping <- function(nm, use_default = FALSE) {
    # Build canonical mapping
    mapping <- lapply(seq_len(nrow(nm)), function(i) {
        params <- nm_base_params(nm[i, ])
        tibble::tibble(in_replicate = params,
                       in_model = params)
    })
    nm$parameters <- mapping
    # Set default priors
    priors <- tibble::tibble(in_model = nm_base_params(nm))
    if (use_default) {
        priors$prior <- lapply(seq_len(nrow(priors)), function(i) {
            hcauchy_p(scale = 0.1)
        })
        for (i in which(grepl("^portion[.]act_", priors$in_model))) {
            priors$prior[[i]] <- uniform_p(0, 1)
        }
    } else {
        priors$prior <- lapply(seq_len(nrow(priors)), function(i) {
            NULL
        })
    }
    attr(nm, "priors") <- priors
    # Return
    return(nm)
}

### * nm_base_params()

#' Get the vector of default parameter names
#'
#' @param nm A \code{networkModel} object
#'
#' @return A vector of strings
#' 
#' @keywords internal
#' @noRd

nm_base_params <- function(nm) {
    params <- c(lapply(nm$topology, topo_get_upsilon_names),
                lapply(nm$topology, topo_get_lambda_names),
                lapply(nm$topology, topo_get_portionAct_names),
                "eta")
    # (eta is a parameter for the variation of observed proportions)
    # Check if zeta should be compartment-specific
    # (zeta is a parameter for the variation of observed sizes)
    z <- attr(nm, "size_zeta_per_compartment")
    if (!is.null(z) && z) {
        comps <- unique(unlist(lapply(nm$topology, colnames)))
        zetas <- paste0("zeta_", comps)
        params <- c(params, zetas)
    } else {
        params <- c(params, "zeta")
    }
    params <- sort(unique(unlist(params)))
    return(params)
}

### * refresh_param_mapping()

#' Apply a formula on param mapping
#'
#' @param nm A \code{networkModel} object.
#' @param formula A list of formulas describing the parameter dependencies on
#'     covariates. Formulas are applied sequentially.
#' @param use_regexpr Boolean. Use regular expression to match the left-hand
#'     terms to replicate parameters?
#'
#' @return An updated \code{networkModel}
#'
#' @importFrom stats update
#' 
#' @keywords internal
#' @noRd

refresh_param_mapping <- function(nm, formula, use_regexpr = TRUE) {
    base_params <- nm_base_params(nm)
    # Parse formula
    leftTerms <- all.vars(update(formula, . ~ 0))
    rightTerms <- all.vars(update(formula, 0 ~ .))
    # Apply regexpr if needed
    if (use_regexpr) {
        for (lt in leftTerms) {
            matches <- any(grepl(lt, base_params))
            leftTerms <- c(leftTerms, base_params[grepl(lt, base_params)])
            if (matches && !lt %in% base_params) {
                # Terms with regexpr match are allowed to be dropped if needed
                leftTerms <- leftTerms[leftTerms != lt]
            }
        }
        leftTerms <- unique(leftTerms)
    }
    # Check that the left terms make sense (they are default parameter names)
    stopifnot(all(leftTerms %in% c(".", base_params)))
    # Get all the parameter names if "." was specified
    if ("." %in% leftTerms) { leftTerms <- base_params }
    # Check that the right terms make sense (they are grouping variables)
    stopifnot(!is.null(nm$group))
    groups <- groups(nm)
    stopifnot(all(rightTerms %in% c(colnames(groups), ".")))
    rightTerms <- rightTerms[rightTerms != "."]
    # Go over the rows of the design data frame
    for (i in seq_len(nrow(nm))) {
        mapping <- nm$parameters[[i]]
        if (length(rightTerms) > 0) {
            covariates <- groups[, rightTerms]
            covariatesString <- apply(covariates, 1, paste, collapse = "|")
            for (lt in leftTerms) {
                g <- paste(lt, covariatesString, sep = "|")
                mapping$in_model[mapping$in_replicate == lt] <- g[i]
            }
        } else {
            for (lt in leftTerms) {
                g <- lt
                mapping$in_model[mapping$in_replicate == lt] <- g
            }
        }
        nm$parameters[[i]] <- mapping
    }
    # Return
    return(nm)
}
