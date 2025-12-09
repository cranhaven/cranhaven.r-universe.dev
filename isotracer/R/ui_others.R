### * All functions in this file are exported

### * delta2prop()

#' Convert delta notation to proportion of heavy isotope
#'
#' For details and references about quantities used in expressing isotopic
#' ratios, see:
#'
#' - Figure 1 in Coplen, Tyler B. “Guidelines and Recommended Terms for
#' Expression of Stable-Isotope-Ratio and Gas-Ratio Measurement Results.” Rapid
#' Communications in Mass Spectrometry 25, no. 17 (September 15, 2011):
#' 2538–60. https://doi.org/10.1002/rcm.5129.
#'
#' - Table 2.1 in Fry, Brian. Stable Isotope Ecology. New York:
#' Springer-Verlag, 2006. //www.springer.com/gp/book/9780387305134.
#'
#' @section Ratios for reference standards:
#'
#' The ratios for reference standards are taken from the Table 2.1 from Fry
#' 2006. Note that the values used for oxygen isotopes are from the standard
#' mean ocean water (SMOW).
#'
#' Standards recognized by this function are: \code{c("d15N", "d2H", "d13C",
#' "d17O.SMOW", "d18O.SMOW", "d33S", "d34S", "d36S")}
#' 
#' @param x Vector of delta values.
#' @param Rstandard String describing the isotopic measurement, e.g. "d15N",
#'     "d13C" and used to set automatically Rstandards (see the Section
#'     "Ratios for reference standards" for more details). Alternatively, a
#'     numeric value to use for Rstandard, e.g. 0.0036765.
#'
#' @return A vector of same length of x, containing the proportion (numeric
#'     between 0 and 1) of heavy isotope based on the delta values and the
#'     Rstandard provided.
#'
#' @examples
#' deltas <- c(78, 5180, 263, 1065, NA, 153, 345)
#'
#' # Rstandard can be specified with a string for some preset references
#' prop15N <- delta2prop(deltas, "d15N")
#' prop13C <- delta2prop(deltas, "d13C")
#'
#' # Rstandard can also be specified manually for non-preset references
#' prop15N_manual <- delta2prop(deltas, 0.0036765)
#' prop13C_manual <- delta2prop(deltas, 0.011180)
#'
#' # Call delta2prop() to get the detail of available references
#' delta2prop()
#' 
#' @export
#'

delta2prop <- function(x = NULL, Rstandard = NULL) {
    # Known standards
    Rstandards <- list("d15N" = 0.0036765,
                       "d2H" = 0.00015576,
                       "d13C" = 0.011180,
                       "d17O.SMOW" = 0.0003799,
                       "d18O.SMOW" = 0.0020052,
                       "d33S" = 0.0078772,
                       "d34S" = 0.0441626,
                       "d36S" = 0.0001533)
    # Message about available standards
    msg <- paste0("Available standards are:")
    for (i in seq_along(Rstandards)) {
        label <- names(Rstandards)[i]
        string <- paste(label, paste(rep("_", 13 - nchar(label)), collapse = ""))
        value <- Rstandards[[i]]
        msg <- paste(msg, paste(string, value, collapse = ""), sep = "\n    ")
    }
    # Parse argument
    if (is.null(Rstandard)) {
        Rstandard <- "non-specified"
        if (is.null(x)) {
            message(paste(msg, sep = "\n"))
            return(invisible(NULL))
        }
    }
    if (length(Rstandard) > 1) {
      msg <- paste0(
        "The Rstandard argument must be of length 1 but the argument provided was ",
        ifelse(is.numeric(Rstandard), "a numeric vector", "an object"),
        " of length ", length(Rstandard), ".")
      if (is.numeric(Rstandard)) {
        msg <- paste0(
          msg, "\n",
          "If you did not want to pass a numeric vector but rather wanted to ",
          "pass a string defining the standard to use (such as \"d15N\"), maybe ",
          "you forgot the quotes?")
      }
      stop(msg)
    }
    if (is.character(Rstandard)) {
        if (!Rstandard %in% names(Rstandards)) {
            unk_std <- paste0("Rstandard argument (", Rstandard, "): unknown. ",
                              collapse = "")
            msg <- paste0(unk_std, msg, collapse = "")
            stop(msg)
        }
        Rstandard <- Rstandards[[Rstandard]]
    } else if (!is.numeric(Rstandard)) {
        stop("Provided value must the name of a known measurement type or a numeric.")
    }
    # Calculate proportions and return
    R0 <- Rstandard
    props <- R0 * (x/1000 + 1) / (R0 * (x/1000 + 1) + 1)
    return(props)
}

### * prop2delta()

#' Convert isotopic proportions to delta values
#'
#' This function performs the inverse of the operation performed by
#' \code{delta2prop()}.
#'
#' @param x Vector of proportions values.
#' @param Rstandard String describing the isotopic measurement, e.g. "d15N",
#'     "d13C" and used to set automatically Rstandards (see the Section
#'     "Ratios for reference standards" for more details). Alternatively, a
#'     numeric value to use for Rstandard, e.g. 0.0036765.
#'
#' @return A vector of same length of x, containing the delta values based on
#'   the proportions of heavy isotope provided as x and the Rstandard provided.
#'
#' @examples
#' prop15N <- c(0.00395, 0.02222, 0.00462, 0.00753, NA, 0.00422, 0.00492)
#'
#' # Rstandard can be specified with a string for some preset references
#' d15N <- prop2delta(prop15N, "d15N")
#' d15N
#'
#' # Rstandard can also be specified manually for non-preset references
#' d15N_manual <- prop2delta(prop15N, 0.0036765)
#' d15N_manual
#'
#' # Call delta2prop() to get the detail of available references
#' delta2prop()
#'
#' @export

prop2delta <- function(x = NULL, Rstandard = NULL) {
    # Known standards
    Rstandards <- list("d15N" = 0.0036765,
                       "d2H" = 0.00015576,
                       "d13C" = 0.011180,
                       "d17O.SMOW" = 0.0003799,
                       "d18O.SMOW" = 0.0020052,
                       "d33S" = 0.0078772,
                       "d34S" = 0.0441626,
                       "d36S" = 0.0001533)
    # Message about available standards
    msg <- paste0("Available standards are:")
    for (i in seq_along(Rstandards)) {
        label <- names(Rstandards)[i]
        string <- paste(label, paste(rep("_", 13 - nchar(label)), collapse = ""))
        value <- Rstandards[[i]]
        msg <- paste(msg, paste(string, value, collapse = ""), sep = "\n    ")
    }
    # Parse argument
    if (is.null(Rstandard)) {
        Rstandard <- "non-specified"
        if (is.null(x)) {
            message(paste(msg, sep = "\n"))
            return(invisible(NULL))
        }
    }
    if (length(Rstandard) > 1) {
      msg <- paste0(
        "The Rstandard argument must be of length 1 but the argument provided was ",
        ifelse(is.numeric(Rstandard), "a numeric vector", "an object"),
        " of length ", length(Rstandard), ".")
      if (is.numeric(Rstandard)) {
        msg <- paste0(
          msg, "\n",
          "If you did not want to pass a numeric vector but rather wanted to ",
          "pass a string defining the standard to use (such as \"d15N\"), maybe ",
          "you forgot the quotes?")
      }
      stop(msg)
    }
    if (is.character(Rstandard)) {
        if (!Rstandard %in% names(Rstandards)) {
            unk_std <- paste0("Rstandard argument (", Rstandard, "): unknown. ",
                              collapse = "")
            msg <- paste0(unk_std, msg, collapse = "")
            stop(msg)
        }
        Rstandard <- Rstandards[[Rstandard]]
    } else if (!is.numeric(Rstandard)) {
        stop("Provided value must the name of a known measurement type or a numeric.")
    }
    # Calculate deltas and return
    R0 <- Rstandard
    deltas <- (1/R0 * x / (1 - x) - 1) * 1000
    return(deltas)
}

### * filter_by_group

#' Filter a tibble based on the "group" column
#'
#' This function can be used to filter any tibble (e.g. network model object)
#' that has a "group" column. See the Examples for more details and syntax.
#'
#' @param .data A tibble that has a `group` column, such as a `networkModel`
#'     object.
#' @param ... Conditional expressions for filtering (see the Examples).
#'
#' @return A tibble similar to the input object, but with rows filtered based
#'     on \code{...}.
#' 
#' @examples
#' trini_mod
#' trini_mod$group
#' groups(trini_mod)
#' filter_by_group(trini_mod, stream == "LL", transect == "transect.1")
#' filter_by_group(trini_mod, transect == "transect.1")
#' \dontrun{
#' # The code below would raise an error because there is no "color" grouping variable.
#' filter_by_group(trini_mod, color == "red")
#' }
#'
#' @export
#'

filter_by_group <- function(.data, ...) {
    if (dplyr::is_grouped_df(.data)) {
        warning("\"groups\" attribute is not kept in returned object.")
    }
    is_grouped <- TRUE
    if (!"group" %in% colnames(.data)) {
        is_grouped <- FALSE
    }
    if (nrow(.data) == 0 |
        (nrow(.data) == 1 && is.null(.data$group[[1]]))) {
        is_grouped <- FALSE
    }
    if (!is_grouped) {
        stop("Input data does not have a valid \"group\" column.")
    }
    if (!length(unique(lapply(.data$group, names))) == 1) {
        stop("Variable names are not consistent in \"group\" column.")
    }
    grp <- tibble::as_tibble(do.call(rbind, .data$group))
    if (".my_index" %in% names(grp)) {
        stop("\"group\" column already has the reserved \".my_index\" field.")
    }
    grp$.my_index <- seq_len(nrow(grp))
    filtered <- dplyr::filter(grp, ...)
    return(.data[filtered$.my_index, ])
}

### * dic()

#' Calculate DIC from a model output
#'
#' Note that DIC might not be indicated for network models, as the posteriors
#' are often not multinormal distributions.
#'
#' LOO is probably not a good choice either since the data is akin to a time
#' series (so data points are not independent). Maybe WAIC could be an option?
#' (TODO: read about this.)
#'
#' DIC is calculated as:
#'
#' DIC = Dbar + pD
#'
#' where D are deviance values calculated as -2 * loglik for each MCMC
#' iteration, Dbar is the mean deviance value and pD is the effective number of
#' parameters in the model and can be calculated as var(D)/2 (Gelman 2003).
#' 
#' @param ... One or several \code{mcmc.list} objects, output(s) from
#'     \code{\link{run_mcmc}}.
#' @param weight Boolean, if TRUE calculate DIC weights based on Link and
#'     Barker 2010 (Link, W. A., and R. J. Barker. 2010. Bayesian Inference
#'     With Ecological Applications. Amsterdam Boston Heidelberg London:
#'     Elsevier/Academic Press).
#'
#' @return A tibble with one row per \code{mcmc.list} object provided in
#'     \code{...}. This tibble is sorted by DIC, so the row order might be
#'     different from the \code{mcmc.list} objects order.
#'
#' @examples
#' \donttest{
#' # Define two different models
#' m1 <- aquarium_mod
#' m2 <- set_topo(m1, c("NH4 -> algae -> daphnia -> NH4", "algae -> NH4"))
#' m2 <- set_priors(m2, priors(m1))
#' m2 <- set_priors(m2, normal_p(0, 0.5), "upsilon_algae_to_NH4")
#' # Run the models
#' r1 <- run_mcmc(m1, chains = 2)
#' r2 <- run_mcmc(m2, chains = 2)
#' # Model comparison with DIC
#' dic(r1, r2)
#' }
#' 
#' @export
#' 

# TODO add formula for DIC calculation in function doc.

dic <- function(..., weight = TRUE) {
    # Deparse based on https://stackoverflow.com/questions/51259346/how-to-get-names-of-dot-dot-dot-arguments-in-r
    # (but I don't understand how it works)
    names <- sapply(substitute(list(...))[-1], deparse)
    logliks <- lapply(list(...), function(x) attr(x, "loglik"))
    if (any(sapply(logliks, is.null))) {
        stop("No \"loglik\" attribute found for at least one input object.")
    }
    logliks <- lapply(logliks, unlist)
    D <- lapply(logliks, function(x) -2 * x)
    Dbar <- sapply(D, mean)
    pD <- sapply(D, function(x) var(x) / 2)
    out <- tibble::tibble(fit = names,
                          Dbar = Dbar,
                          pD = pD)
    out[["DIC"]] <- out$Dbar + out$pD
    min_DIC <- min(out[["DIC"]])
    out[["delta_DIC"]] <- out[["DIC"]] - min_DIC
    num <- exp(- out[["delta_DIC"]] / 2)
    denom <- sum(num)
    out[["weight"]] <- num / denom
    out <- out[order(out$delta_DIC), ]
    return(out)
}
