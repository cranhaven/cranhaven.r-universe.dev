#' @title Model Specification for iglm Terms
#'
#' @description
#'
#' The help pages of \code{\link{iglm}} describe the model with details on model fitting
#' and estimation.
#' Generally, a model is specified via its sufficient statistics,
#' that can be further decomposed into two parts:
#' \itemize{
#'   \item \strong{\eqn{\mathbf{g}_i(x_i^*,y_i^*) = \mathbf{g}_i(x_i,y_i)= (g_i(x_i,y_i))}}: A vector of unit-level functions (or "g-terms")
#'     that describe the relationship between an individual unit \eqn{i}'s
#'     predictors (\eqn{x_i}) and their own response (\eqn{y_i}).
#'   \item \strong{\eqn{\mathbf{h}_{i,j}(x_i^*,x_j^*, y_i^*, y_j^*, z) = \mathbf{h}_{i,j}(x,y,z)= (h_{i,j}(x,y,z))}}: A vector of pair-level functions (or "h-terms")
#'     that specify how the connections (\eqn{z}) and responses (\eqn{y_i, y_j})
#'     of a pair of units \eqn{\{i,j\}} depend on each other and the wider
#'     network structure.
#' }
#' Each term defines a component for the model's features, which
#' are a sum of unit-level components, \eqn{\sum_i g_i(x_i,y_i)}, and/or
#' pair-level components, \eqn{\sum_{i \ne j} h_{i,j}(x,y,z)}.
#' The implemented terms are grouped into three categories:
#' \enumerate{
#' \item \strong{Attribute Terms}: Depend only on individual attributes \eqn{x_i} or \eqn{y_i}.
#' \item \strong{Network Terms}: Depend only on the connections \eqn{z_{i,j}}.
#' \item \strong{Joint Attribute/Network Terms}: Depend on both individual attributes and connections.
#' }
#' @section Category 1: Attribute Terms:
#'
#' Below is a detailed description of terms that depend only on nodal attributes:
#' \itemize{
#'   \item \code{\link{attribute_x-term}}, \code{\link{attribute_y-term}}
#'   \item \code{\link{cov_x-term}}, \code{\link{cov_y-term}}
#'   \item \code{\link{attribute_xy-term}}
#' }
#'
#' @section Category 2: Network Terms:
#'
#' Below is a detailed description of terms that depend only on the network structure:
#' \itemize{
#'   \item \code{\link{edges-term}}, \code{\link{mutual-term}}
#'   \item \code{\link{cov_z-term}}, \code{\link{cov_z_in-term}}, \code{\link{cov_z_out-term}}
#'   \item \code{\link{degrees-term}}
#'   \item \code{\link{gwdegree-term}}, \code{\link{gwidegree-term}}, \code{\link{gwodegree-term}}
#'   \item \code{\link{gwesp-term}}, \code{\link{gwdsp-term}}
#'   \item \code{\link{transitive-term}}, \code{\link{nonisolates-term}}, \code{\link{isolates-term}}
#' }
#'
#' @section Category 3: Joint Attribute/Network Terms:
#'
#' Below is a detailed description of terms that depend on both attributes and the network:
#' \itemize{
#'   \item \code{\link{attribute_xz-term}}, \code{\link{attribute_yz-term}}
#'   \item \code{\link{inedges_x-term}}, \code{\link{inedges_y-term}}, \code{\link{outedges_x-term}}, \code{\link{outedges_y-term}}
#'   \item \code{\link{edges_x_match-term}}, \code{\link{edges_y_match-term}}
#'   \item \code{\link{spillover_xx-term}}, \code{\link{spillover_yy-term}}
#'   \item \code{\link{spillover_yx-term}}, \code{\link{spillover_xy-term}}, \code{\link{spillover_yc-term}}, \code{\link{spillover_yc_symm-term}}
#' }
#'
#' @section Argument Specification (Positional and Named Arguments):
#'
#' Terms in an \code{iglm} formula can be specified using named arguments,
#' positional arguments, or a combination of both:
#' \itemize{
#'   \item \strong{Positional arguments}: Unnamed arguments (e.g., \code{edges("local")} or \code{gwdegree(0.5, "local")})
#'     are mapped sequentially to the term's candidate parameters in the order of
#'     their definition (\code{mandatory}, \code{defaults}, and \code{expected}).
#'   \item \strong{Mixed arguments}: If some arguments are named (e.g., \code{cov_z(mat, mode = "local")}),
#'     named arguments are matched first, and positional arguments are mapped to the
#'     remaining candidate parameters in order.
#'   \item \strong{Excess arguments}: Providing more positional arguments than the term accepts
#'     (e.g., \code{gwdegree(0.5, "local", "extra")}) will produce an unexpected-argument error.
#' }
#'
#' @references
#'
#' Fritz, C., Schweinberger, M., Bhadra, S., and D.R. Hunter (2025). A Regression Framework for Studying Relationships among Attributes under Network Interference. Journal of the American Statistical Association, to appear.
#'
#' Schweinberger, M. and M.S. Handcock (2015). Local Dependence in Random Graph Models: Characterization, Properties, and Statistical Inference. Journal of the Royal Statistical Society, Series B (Statistical Methodology), 7, 647-676.
#'
#' Schweinberger, M. and J.R. Stewart (2020). Concentration and Consistency Results for Canonical and Curved Exponential-Family Models of Random Graphs. The Annals of Statistics, 48, 374-396.
#'
#' Stewart, J.R. and M. Schweinberger (2025). Pseudo-Likelihood-Based M-Estimation of Random Graphs with Dependent Edges and Parameter Vectors of Increasing Dimension. The Annals of Statistics, to appear.
#'
#' @name iglm-terms
#' @aliases iglm-terms iglm.terms
NULL

#' @title Initialize iglm Model Terms
#'
#' @description This is an internal generic function used to initialize mapping and data
#' for terms in a \code{\link{iglm.object}} formula.
#'
#' @param data_object An \code{\link{iglm.data}} object.
#' @param arglist A list of arguments passed to the term in the formula.
#' @param ... Additional arguments.
#' @return A list containing information for the C++ backend.
#' @keywords internal
InitIglmTerm <- function(data_object, arglist, ...) {
  # Prioritize base_name if it exists (set by rhs_terms_as_list)
  # Fall back to term_name or positional name for robustness
  term_name <- if (!is.null(arglist$base_name)) {
    arglist$base_name
  } else if (!is.null(arglist$term_name)) {
    arglist$term_name
  } else {
    # Fallback to the first element if it looks like a name
    sub("_.*", "", names(arglist)[1])
  }

  init_func_name <- paste0("InitIglmTerm.", term_name)

  # 1. Search in the current search path and iglm namespace
  init_func <- get0(init_func_name, mode = "function", inherits = TRUE)

  # 2. If not found, search in all loaded namespaces
  if (is.null(init_func)) {
    for (ns in loadedNamespaces()) {
      init_func <- get0(init_func_name, envir = asNamespace(ns), mode = "function", inherits = FALSE)
      if (!is.null(init_func)) break
    }
  }

  if (is.null(init_func)) {
    stop(paste0("Term '", term_name, "' not recognized. No '", init_func_name, "' found in search path or loaded namespaces."))
  }

  init_func(data_object = data_object, arglist = arglist, ...)
}

#' @title Check Arguments for iglm Model Terms
#'
#' @description This is an internal helper function used to validate and set defaults
#' for arguments passed to iglm model terms.
#'
#' @details
#' \code{check.IglmTerm} normalizes and validates arguments passed to model terms in formulas:
#' \enumerate{
#'   \item \strong{Positional Argument Normalization}: Arguments passed without parameter names
#'     are sorted by position and
#'     mapped sequentially to available candidate parameters. Candidate parameters are
#'     identified from \code{mandatory}, \code{defaults}, and \code{expected} (excluding metadata
#'     and arguments already supplied by name).
#'   \item \strong{Excess Positional Arguments}: Unmatched positional arguments (where position index
#'     exceeds the number of candidate parameters) remain in \code{arglist} and trigger
#'     an unexpected-argument error during validation.
#'   \item \strong{Type and Value Validation}: Validates mandatory arguments, allowed categorical
#'     values, numeric/matrix types, scalar constraints, and ensures no \code{NA}/\code{NaN} values
#'     are present in numeric inputs.
#'   \item \strong{Default Injection}: Injects default values for any omitted optional parameters.
#' }
#'
#' @param data_object The iglm.data object.
#' @param arglist The list of arguments passed to the term.
#' @param mandatory Character vector of mandatory argument names.
#' @param expected A list where keys are expected argument names and values
#'   are either a character vector of allowed values, or a type string ("numeric", "matrix").
#' @param defaults a list of default values for arguments.
#' @param directed Logical indicating if the term is only for directed (TRUE) or undirected (FALSE) networks.
#' @return A modified \code{arglist} with defaults applied and validated values.
#' @keywords internal
check.IglmTerm <- function(data_object, arglist, mandatory = character(0), expected = list(), defaults = list(), directed = NULL) {
  term_name <- if (!is.null(arglist$base_name)) {
    arglist$base_name
  } else if (!is.null(arglist$term_name)) {
    arglist$term_name
  } else if (!is.null(arglist$label)) {
    sub("\\(.*$", "", arglist$label)
  } else {
    caller_term <- NULL
    if (sys.nframe() > 1) {
      caller_call <- sys.call(-1)
      if (!is.null(caller_call) && length(caller_call) >= 1) {
        caller_name <- as.character(caller_call[[1]])[1]
        if (startsWith(caller_name, "InitIglmTerm.")) {
          caller_term <- sub("^InitIglmTerm\\.", "", caller_name)
        }
      }
    }
    caller_term
  }
  # Normalize unnamed positional arguments (..1, ..2, ...) to expected parameter names
  pos_args <- grep("^\\.\\.[0-9]+$", names(arglist), value = TRUE)
  if (length(pos_args) > 0) {
    pos_indices <- as.integer(sub("^\\.\\.", "", pos_args))
    pos_args <- pos_args[order(pos_indices)]

    candidate_params <- unique(c(mandatory, names(defaults), names(expected)))
    # Exclude special/metadata fields and parameters already explicitly named in arglist
    candidate_params <- candidate_params[!candidate_params %in% c("base_name", "term_name", "label", names(arglist))]

    for (k in seq_along(pos_args)) {
      pos_key <- pos_args[k]
      if (k <= length(candidate_params)) {
        target_param <- candidate_params[k]
        arglist[[target_param]] <- arglist[[pos_key]]
        arglist[[pos_key]] <- NULL
      }
    }
  }

  if (!is.null(directed) && data_object$directed != directed) {
    if (!is.null(term_name)) {
      stop(sprintf("Term '%s' is only for %s networks.", term_name, if (directed) "directed" else "undirected"))
    } else {
      stop(sprintf("Term is only for %s networks.", if (directed) "directed" else "undirected"))
    }
  }

  for (name in mandatory) {
    if (is.null(arglist[[name]])) {
      if (!is.null(term_name)) {
        stop(sprintf("Argument '%s' is mandatory for term '%s'.", name, term_name))
      } else {
        stop(sprintf("Argument '%s' is mandatory for this term.", name))
      }
    }
  }

  for (name in names(defaults)) {
    if (is.null(arglist[[name]])) {
      arglist[[name]] <- defaults[[name]]
    }
  }

  for (name in names(expected)) {
    val <- arglist[[name]]
    if (is.null(val)) next
    spec <- expected[[name]]
    if (is.character(spec) && length(spec) >= 1 && !(length(spec) == 1 && spec %in% c("numeric", "scalar_numeric", "numeric_scalar", "matrix"))) {
      if (length(val) != 1 || !val %in% spec) {
        if (!is.null(term_name)) {
          stop(sprintf("Argument '%s' of term '%s' must be one of: %s", name, term_name, paste(spec, collapse = ", ")))
        } else {
          stop(sprintf("Argument '%s' must be one of: %s", name, paste(spec, collapse = ", ")))
        }
      }
    } else if (!is.null(spec) && spec %in% c("numeric", "scalar_numeric", "numeric_scalar")) {
      if (!is.numeric(val)) {
        if (!is.null(term_name)) {
          stop(sprintf("Argument '%s' of term '%s' must be numeric.", name, term_name))
        } else {
          stop(sprintf("Argument '%s' must be numeric.", name))
        }
      }
      if (spec %in% c("scalar_numeric", "numeric_scalar") && length(val) != 1) {
        if (!is.null(term_name)) {
          stop(sprintf("Argument '%s' of term '%s' must be a single numeric value.", name, term_name))
        } else {
          stop(sprintf("Argument '%s' must be a single numeric value.", name))
        }
      }
    } else if (!is.null(spec) && spec == "matrix") {
      if (!is.matrix(val) && !is.numeric(val)) {
        if (!is.null(term_name)) {
          stop(sprintf("Argument '%s' of term '%s' must be a matrix or numeric vector.", name, term_name))
        } else {
          stop(sprintf("Argument '%s' must be a matrix or numeric vector.", name))
        }
      }
    }

    # Check for missing (NA/NaN) values in numeric/matrix arguments
    if (is.numeric(val) || is.matrix(val) || inherits(val, "Matrix")) {
      if (any(is.na(val))) {
        if (!is.null(term_name)) {
          stop(sprintf("Argument '%s' of term '%s' contains missing (NA/NaN) values.", name, term_name), call. = FALSE)
        } else {
          stop(sprintf("Argument '%s' contains missing (NA/NaN) values.", name), call. = FALSE)
        }
      }
    }
  }

  # Check for unexpected arguments
  allowed_args <- unique(c("base_name", "term_name", "label", mandatory, names(defaults), names(expected)))
  for (arg in names(arglist)) {
    if (!arg %in% allowed_args) {
      if (!is.null(term_name)) {
        stop(sprintf("Unexpected argument '%s' passed to term '%s'.", arg, term_name), call. = FALSE)
      } else {
        stop(sprintf("Unexpected argument '%s' passed to term.", arg), call. = FALSE)
      }
    }
  }

  return(arglist)
}

#' @description \code{degrees}: Degrees: Specifies node-level fixed effects. Estimation requires an MM algorithm constraint.
#' @name degrees-term
#' @rdname iglm-terms
NULL

#' @description \code{edges(mode = "global")}: Edges: Captures the baseline propensity of tie formation \eqn{z_{i,j}}, partitioned by structural boundary \eqn{c_{i,j}}.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) z_{i,j}}
#' }
#' @name edges-term
#' @rdname iglm-terms
NULL

InitIglmTerm.edges <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("edges_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{mutual(mode = "global")}: Mutual Reciprocity: Evaluates reciprocal tie formation in directed networks.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = z_{i,j} z_{j,i}} (for \eqn{i < j})
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} z_{i,j} z_{j,i}} (for \eqn{i < j})
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) z_{i,j} z_{j,i}} (for \eqn{i < j})
#' }
#' @name mutual-term
#' @rdname iglm-terms
NULL

InitIglmTerm.mutual <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("mutual_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{cov_z(data, mode = "global")}: Dyadic Covariate: Exogenous dyadic covariate \eqn{w_{i,j}} influence on edge formation.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = w_{i,j} z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} w_{i,j} z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) w_{i,j} z_{i,j}}
#' }
#' @name cov_z-term
#' @rdname iglm-terms
NULL

InitIglmTerm.cov_z <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(data = "matrix", mode = c("global", "local", "alocal"), type = "numeric"),
    defaults = list(data = matrix(1), mode = "global", type = 1)
  )
  res <- list(
    term_name = paste0("cov_z_", arglist$mode),
    coef_name = arglist$label
  )
  if (!all(arglist$data == 1) || length(arglist$data) != 1) res$data <- arglist$data
  if (arglist$type != 1) res$type <- arglist$type
  res
}

#' @description \code{cov_z_out(data, mode = "global")}: Covariate Sender: Exogenous monadic covariate \eqn{v_{i}} influence on generating an outgoing tie.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = v_i z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} v_i z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) v_i z_{i,j}}
#' }
#' @name cov_z_out-term
#' @rdname iglm-terms
NULL

InitIglmTerm.cov_z_out <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(data = "matrix", mode = c("global", "local", "alocal"), type = "numeric"),
    defaults = list(data = matrix(1), mode = "global", type = 1)
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = paste0("cov_z_out_", arglist$mode),
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  if (arglist$type != 1) res$type <- arglist$type
  res
}

#' @description \code{cov_z_in(data, mode = "global")}: Covariate Receiver: Exogenous monadic covariate \eqn{v_{j}} influence on receiving an incoming tie.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = v_j z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} v_j z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) v_j z_{i,j}}
#' }
#' @name cov_z_in-term
#' @rdname iglm-terms
NULL

InitIglmTerm.cov_z_in <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(data = "matrix", mode = c("global", "local", "alocal"), type = "numeric"),
    defaults = list(data = matrix(1), mode = "global", type = 1)
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = paste0("cov_z_in_", arglist$mode),
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  if (arglist$type != 1) res$type <- arglist$type
  res
}

#' @description \code{cov_x(data = v)}: Nodal Covariate (X): Effect of a unit-level exogenous covariate \eqn{v_i} on endogenous attribute \eqn{x_i}.
#' \eqn{g_i(x_i,y_i) = v_i x_i}
#' @name cov_x-term
#' @rdname iglm-terms
NULL

InitIglmTerm.cov_x <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(data = "matrix", type = "numeric"),
    defaults = list(data = matrix(1), type = 1)
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = "cov_x",
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  if (arglist$type != 1) res$type <- arglist$type
  res
}

#' @description \code{cov_y(data = v)}: Nodal Covariate (Y): Effect of a unit-level exogenous covariate \eqn{v_i} on endogenous attribute \eqn{y_i}.
#' \eqn{g_i(x_i,y_i) = v_i y_i}
#' @name cov_y-term
#' @rdname iglm-terms
NULL

InitIglmTerm.cov_y <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(data = "matrix", type = "numeric"),
    defaults = list(data = matrix(1), type = 1)
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = "cov_y",
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  if (arglist$type != 1) res$type <- arglist$type
  res
}

#' @description \code{attribute_xy(mode = "global")}: Nodal Attribute Interaction (X-Y): Interaction of attributes \eqn{x_i} and \eqn{y_i}.
#' \itemize{
#'   \item \code{global}: \eqn{g_i(x_i,y_i) = x_i y_i}
#'   \item \code{local}: \eqn{g_i(x_i,y_i) = x_i \sum_{j \in \mathcal{N}_i} y_j + y_i \sum_{j \in \mathcal{N}_i} x_j}
#'   \item \code{alocal}: \eqn{g_i(x_i,y_i) = x_i \sum_{j \notin \mathcal{N}_i} y_j + y_i \sum_{j \notin \mathcal{N}_i} x_j}
#' }
#' @name attribute_xy-term
#' @rdname iglm-terms
NULL

InitIglmTerm.attribute_xy <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("attribute_xy_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{attribute_yz(mode = "local")}: Attribute Sum (Y-Z): Models the additive effect of \eqn{y_i} and \eqn{y_j} on edge formation within local neighborhoods.
#' @name attribute_yz-term
#' @rdname iglm-terms
NULL

InitIglmTerm.attribute_yz <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("local")),
    defaults = list(mode = "local")
  )
  list(
    term_name = "attribute_yz_local",
    coef_name = arglist$label
  )
}

#' @description \code{attribute_xz(mode = "local")}: Attribute Sum (X-Z): Models the additive effect of \eqn{x_i} and \eqn{x_j} on edge formation within local neighborhoods.
#' @name attribute_xz-term
#' @rdname iglm-terms
NULL

InitIglmTerm.attribute_xz <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("local")),
    defaults = list(mode = "local")
  )
  list(
    term_name = "attribute_xz_local",
    coef_name = arglist$label
  )
}

#' @description \code{inedges_y(mode = "global")}: Attribute In-Degree (Y-Z): Influence of endogenous \eqn{y_j} on in-degree reception.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = y_j z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} y_j z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) y_j z_{i,j}}
#' }
#' @name inedges_y-term
#' @rdname iglm-terms
NULL

InitIglmTerm.inedges_y <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("inedges_y_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{outedges_y(mode = "global")}: Attribute Out-Degree (Y-Z): Influence of endogenous \eqn{y_i} on out-degree formation.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = y_i z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} y_i z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) y_i z_{i,j}}
#' }
#' @name outedges_y-term
#' @rdname iglm-terms
NULL

InitIglmTerm.outedges_y <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("outedges_y_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{inedges_x(mode = "global")}: Attribute In-Degree (X-Z): Influence of endogenous \eqn{x_j} on in-degree reception.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = x_j z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} x_j z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) x_j z_{i,j}}
#' }
#' @name inedges_x-term
#' @rdname iglm-terms
NULL

InitIglmTerm.inedges_x <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("inedges_x_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{outedges_x(mode = "global")}: Attribute Out-Degree (X-Z): Influence of endogenous \eqn{x_i} on out-degree formation.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = x_i z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} x_i z_{i,j}}
#'   \item \code{alocal}: \eqn{h_{i,j}(x,y,z) = (1 - c_{i,j}) x_i z_{i,j}}
#' }
#' @name outedges_x-term
#' @rdname iglm-terms
NULL

InitIglmTerm.outedges_x <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local", "alocal")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("outedges_x_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{attribute_x}: Attribute (X): Intercept for attribute \eqn{x}.
#' \eqn{g_i(x_i,y_i) = x_i}
#' @name attribute_x-term
#' @rdname iglm-terms
NULL

InitIglmTerm.attribute_x <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist)
  list(
    term_name = "attribute_x",
    coef_name = arglist$label
  )
}

#' @description \code{attribute_y}: Attribute (Y): Intercept for attribute \eqn{y}.
#' \eqn{g_i(x_i,y_i) = y_i}
#' @name attribute_y-term
#' @rdname iglm-terms
NULL

InitIglmTerm.attribute_y <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist)
  list(
    term_name = "attribute_y",
    coef_name = arglist$label
  )
}

#' @description \code{edges_x_match(mode = "global")}: Attribute Match (X-Z): Models homophily/matching on the binary attribute \eqn{x}.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = \mathbb{I}(x_i = x_j) z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} \mathbb{I}(x_i = x_j) z_{i,j}}
#' }
#' @name edges_x_match-term
#' @rdname iglm-terms
NULL

InitIglmTerm.edges_x_match <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("edges_x_match_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{edges_y_match(mode = "global")}: Attribute Match (Y-Z): Models homophily/matching on the binary attribute \eqn{y}.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = \mathbb{I}(y_i = y_j) z_{i,j}}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} \mathbb{I}(y_i = y_j) z_{i,j}}
#' }
#' @name edges_y_match-term
#' @rdname iglm-terms
NULL

InitIglmTerm.edges_y_match <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("edges_y_match_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{spillover_yy_scaled(mode = "global")}: Scaled Y-Y-Z Outcome Spillover: Normalizes the \eqn{y}-outcome spillover influence by the relevant out-degree topology.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = y_i y_j z_{i,j} / \text{deg}(i)}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} y_i y_j z_{i,j} / \text{deg}(i, \text{local})}
#' }
#' @name spillover_yy_scaled-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yy_scaled <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("spillover_yy_scaled_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{spillover_xx_scaled(mode = "global")}: Scaled X-X-Z Outcome Spillover: Normalizes the \eqn{x}-outcome spillover influence by the relevant out-degree topology.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = x_i x_j z_{i,j} / \text{deg}(i)}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} x_i x_j z_{i,j} / \text{deg}(i, \text{local})}
#' }
#' @name spillover_xx_scaled-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_xx_scaled <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("spillover_xx_scaled_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{spillover_yx_scaled(mode = "global")}: Scaled Y-X-Z Treatment Spillover: Normalizes cross-attribute \eqn{y_i \to x_j} spillover influence.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = y_i x_j z_{i,j} / \text{deg}(i)}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} y_i x_j z_{i,j} / \text{deg}(i, \text{local})}
#' }
#' @name spillover_yx_scaled-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yx_scaled <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("spillover_yx_scaled_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{spillover_xy_scaled(mode = "global")}: Scaled X-Y-Z Treatment Spillover: Normalizes cross-attribute \eqn{x_i \to y_j} spillover influence.
#' \itemize{
#'   \item \code{global}: \eqn{h_{i,j}(x,y,z) = x_i y_j z_{i,j} / \text{deg}(i)}
#'   \item \code{local}: \eqn{h_{i,j}(x,y,z) = c_{i,j} x_i y_j z_{i,j} / \text{deg}(i, \text{local})}
#' }
#' @name spillover_xy_scaled-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_xy_scaled <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = c("global", "local")),
    defaults = list(mode = "global")
  )
  list(
    term_name = paste0("spillover_xy_scaled_", arglist$mode),
    coef_name = arglist$label
  )
}

#' @description \code{gwesp(data, mode = "global", variant = "OSP", decay = 0)}: Geometrically Weighted Edgewise-Shared Partners: Models triadic closure propensity conditioning on existing edges.
#' Types dictate path constraint: OTP, ITP, OSP, ISP for directed; symm for undirected.
#' @name gwesp-term
#' @rdname iglm-terms
NULL

InitIglmTerm.gwesp <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(
      decay = "scalar_numeric",
      mode = c("global", "local"),
      variant = c("ITP", "ISP", "OTP", "OSP", "symm")
    ),
    defaults = list(
      decay = 0,
      mode = "global",
      variant = if (data_object$directed) "OSP" else "symm"
    )
  )
  if (data_object$directed && arglist$variant == "symm") stop("Variant 'symm' is only for undirected networks.")
  if (!data_object$directed && arglist$variant != "symm") stop(sprintf("Variant '%s' is only for directed networks.", arglist$variant))

  list(
    term_name = paste0("gwesp_", arglist$mode, "_", arglist$variant),
    data = matrix(arglist$decay),
    type = 0,
    coef_name = arglist$label
  )
}

#' @description \code{gwdsp(data, mode = "global", variant = "OSP", decay = 0)}: Geometrically Weighted Dyadwise-Shared Partners: Models triadic potential irrespective of the closing edge.
#' Types dictate path constraint: OTP, ITP, OSP, ISP for directed; symm for undirected.
#' @name gwdsp-term
#' @rdname iglm-terms
NULL

InitIglmTerm.gwdsp <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(
      decay = "scalar_numeric",
      mode = c("global", "local"),
      variant = c("ITP", "ISP", "OTP", "OSP", "symm")
    ),
    defaults = list(
      decay = 0,
      mode = "global",
      variant = if (data_object$directed) "OSP" else "symm"
    )
  )
  if (data_object$directed && arglist$variant == "symm") stop("Variant 'symm' is only for undirected networks.")
  if (!data_object$directed && arglist$variant != "symm") stop(sprintf("Variant '%s' is only for directed networks.", arglist$variant))

  list(
    term_name = paste0("gwdsp_", arglist$mode, "_", arglist$variant),
    data = matrix(arglist$decay),
    type = 0,
    coef_name = arglist$label
  )
}

#' @description \code{gwdegree(mode = "global", decay = 0)}: Geometrically Weighted Degree: Captures the degree distribution utilizing an exponential decay parameter.
#' @name gwdegree-term
#' @rdname iglm-terms
NULL

InitIglmTerm.gwdegree <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(
      decay = "scalar_numeric",
      mode = c("global", "local")
    ),
    defaults = list(decay = 0, mode = "global")
  )

  list(
    term_name = paste0("gwdegree_", arglist$mode),
    data = matrix(arglist$decay),
    type = 0,
    coef_name = arglist$label
  )
}

#' @description \code{gwidegree(mode = "global", decay = 0)}: Geometrically Weighted In-Degree: Captures the in-degree distribution utilizing an exponential decay parameter.
#' @name gwidegree-term
#' @rdname iglm-terms
NULL

InitIglmTerm.gwidegree <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(
      decay = "scalar_numeric",
      mode = c("global", "local")
    ),
    defaults = list(decay = 0, mode = "global")
  )

  list(
    term_name = paste0("gwidegree_", arglist$mode),
    data = matrix(arglist$decay),
    type = 0,
    coef_name = arglist$label
  )
}

#' @description \code{gwodegree(mode = "global", decay = 0)}: Geometrically Weighted Out-Degree: Captures the out-degree distribution utilizing an exponential decay parameter.
#' @name gwodegree-term
#' @rdname iglm-terms
NULL

InitIglmTerm.gwodegree <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(
      decay = "scalar_numeric",
      mode = c("global", "local")
    ),
    defaults = list(decay = 0, mode = "global")
  )

  list(
    term_name = paste0("gwodegree_", arglist$mode),
    data = matrix(arglist$decay),
    type = 0,
    coef_name = arglist$label
  )
}

#' @description \code{spillover_yc_symm(data = v, mode = "local")}: Symmetric Y-C-Z Treatment Spillover: Bidirectional mapping of exogenous covariate \eqn{v} and endogenous trait \eqn{y} interaction.
#' @name spillover_yc_symm-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yc_symm <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = FALSE,
    expected = list(data = "matrix", mode = "local"),
    defaults = list(data = matrix(1), mode = "local")
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = "spillover_yc_symm",
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  res
}

#' @description \code{spillover_xy(mode = "local")}: Directed X-Y-Z Treatment Spillover: Maps cross-attribute \eqn{x_i \to y_j} treatment assignment.
#' @name spillover_xy-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_xy <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = "local"),
    defaults = list(mode = "local")
  )
  list(
    term_name = "spillover_xy",
    coef_name = arglist$label
  )
}

#' @description \code{spillover_yc(mode = "local")}: Directed Y-C-Z Treatment Spillover: Exogenous covariate \eqn{v} interacting with endogenous trait \eqn{y}.
#' @name spillover_yc-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yc <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    directed = TRUE,
    expected = list(data = "matrix", mode = "local"),
    defaults = list(data = matrix(1), mode = "local")
  )
  data <- if (is.matrix(arglist$data)) arglist$data else matrix(arglist$data, nrow = 1)
  res <- list(
    term_name = "spillover_yc",
    coef_name = arglist$label
  )
  if (!all(data == 1) || length(data) != 1) res$data <- data
  res
}

#' @description \code{spillover_yx(mode = "local")}: Directed Y-X-Z Treatment Spillover: Maps cross-attribute \eqn{y_i \to x_j} treatment assignment.
#' @name spillover_yx-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yx <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = "local"),
    defaults = list(mode = "local")
  )
  list(
    term_name = "spillover_yx",
    coef_name = arglist$label
  )
}

#' @description \code{spillover_yy(mode = "local")}: Symmetric Y-Y-Z Outcome Spillover: Propagates \eqn{y}-outcome spillover effects.
#' @name spillover_yy-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_yy <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = "local"),
    defaults = list(mode = "local")
  )
  list(
    term_name = "spillover_yy",
    coef_name = arglist$label
  )
}

#' @description \code{spillover_xx(mode = "local")}: Symmetric X-X-Z Outcome Spillover: Propagates \eqn{x}-outcome spillover effects.
#' @name spillover_xx-term
#' @rdname iglm-terms
NULL

InitIglmTerm.spillover_xx <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist,
    expected = list(mode = "local"),
    defaults = list(mode = "local")
  )
  list(
    term_name = "spillover_xx",
    coef_name = arglist$label
  )
}

#' @description \code{transitive}: Transitivity (Local): Indicator evaluating the presence of a local transitive triad configuration.
#' @name transitive-term
#' @rdname iglm-terms
NULL

InitIglmTerm.transitive <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist)
  list(
    term_name = "transitive",
    coef_name = arglist$label
  )
}

#' @description \code{nonisolates}: Non-Isolates: Captures frequency of nodes with degree strictly greater than zero.
#' @name nonisolates-term
#' @rdname iglm-terms
NULL

InitIglmTerm.nonisolates <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist)
  list(
    term_name = "nonisolates",
    coef_name = arglist$label
  )
}

#' @description \code{isolates}: Isolates: Captures frequency of nodes with degree zero.
#' @name isolates-term
#' @rdname iglm-terms
NULL

InitIglmTerm.isolates <- function(data_object, arglist, ...) {
  arglist <- check.IglmTerm(data_object, arglist)
  list(
    term_name = "isolates",
    coef_name = arglist$label
  )
}
