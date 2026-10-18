#' @importFrom Rcpp compileAttributes
#' @importFrom RcppArmadillo fastLm
#' @importFrom Matrix bdiag sparseMatrix
# Utility: deparse to a single string
.deparse1 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")

.split_plus <- function(expr) {
  out <- list()
  rec <- function(e) {
    if (is.call(e) && identical(e[[1L]], as.name("+"))) {
      rec(e[[2L]])
      rec(e[[3L]])
    } else {
      out[[length(out) + 1L]] <<- e
    }
  }
  rec(expr)
  out
}

find_ranks <- function(x) {
  match(x, sort(unique(x)))
}

get_rank <- function(new_vals, ref_data) {
  ref_sorted <- sort(ref_data)
  n_less <- findInterval(new_vals, ref_sorted, left.open = TRUE)
  n_less_equal <- findInterval(new_vals, ref_sorted)
  ((n_less + 1) + (n_less_equal + 1)) / 2
}

update_formula_remove_terms <- function(formula, terms_to_remove) {
  rhs_terms <- attr(terms(formula), "term.labels")
  rhs_terms_updated <- rhs_terms[!rhs_terms %in% terms_to_remove]
  new_formula <- as.formula(paste(
    deparse(formula[[2]]), "~",
    paste(rhs_terms_updated, collapse = " + ")
  ))
  environment(new_formula) <- environment(formula)
  new_formula
}

.make_unique_name <- function(nm, existing) {
  if (!(nm %in% existing)) {
    return(nm)
  }
  k <- 2L
  while (paste0(nm, "_", k) %in% existing) k <- k + 1L
  paste0(nm, "_", k)
}

eval_change <- function(formula, additional_args = NULL, object) {
  term_labels <- attr(terms(formula), "term.labels")
  # Initialize a list to hold all results
  all_results <- list()
  # Iterate over each term label
  for (i in seq_along(term_labels)) {
    term_string <- term_labels[i]
    # Convert the string term back into an expression object
    expression_obj <- parse(text = term_string)[[1]]
    # Convert the expression into a list of its components
    # The first element is the function name (a symbol), the rest are arguments
    call_list <- as.list(expression_obj)
    # Function name is the first element, converted to a character string
    func_name <- as.character(call_list[[1]])
    # Arguments are the remaining elements
    args <- call_list[-1]
    if (!is.null(additional_args) && !is.null(additional_args[[i]])) {
      arg_name <- if (!is.null(names(additional_args)) && nzchar(names(additional_args)[i])) names(additional_args)[i] else "value_range"
      args[[arg_name]] <- additional_args[[i]]
    }
    args$plot <- FALSE
    # The function/method to call is the named item inside the R6 object
    method_to_call <- object[[func_name]]
    if (is.function(method_to_call)) {
      # Execute the function call using the
      # iglm.data object's environment (self)
      result <- do.call(method_to_call, args)

      # Store the result
      all_results[[term_string]] <- result
    } else {
      warning(paste("Method not found for term:", func_name))
    }
  }
  return(all_results)
}

rhs_terms_as_list <- function(formula, env = NULL, evaluate_calls = FALSE) {
  formula <- as.formula(formula)
  if (is.null(env)) {
    env <- environment(formula)
    if (is.null(env)) env <- parent.frame()
  }
  rhs_expr <- formula[[3L]]
  terms_exprs <- .split_plus(rhs_expr)

  out <- list()
  taken_names <- character(0L)

  for (term_expr in terms_exprs) {
    if (is.symbol(term_expr)) {
      base_name <- as.character(term_expr)
      if (base_name %in% taken_names) {
        next
      }
      taken_names <- c(taken_names, base_name)

      out[[base_name]] <- list(
        label = .deparse1(term_expr),
        base_name = base_name
      )
    } else if (is.call(term_expr)) {
      fun_sym <- term_expr[[1L]]
      base_name <- if (is.symbol(fun_sym)) as.character(fun_sym) else .deparse1(fun_sym)

      # raw argument expressions
      arg_exprs <- as.list(term_expr)[-1L]
      arg_names <- names(arg_exprs)
      if (is.null(arg_names)) arg_names <- rep("", length(arg_exprs))

      # prepare container with named, evaluated arguments
      # unnamed arguments get positional names ..1, ..2, ...
      pos_names <- ifelse(arg_names == "", paste0("..", seq_along(arg_exprs)),
        arg_names
      )
      arg_vals <- list()
      for (i in seq_along(arg_exprs)) {
        val_expr <- arg_exprs[[i]]
        # If it's a character literal, keep it as character
        if (is.character(val_expr)) {
          val <- val_expr
        } else {
          val <- try(eval(val_expr, envir = env), silent = TRUE)
          if (inherits(val, "try-error")) {
            cond <- attr(val, "condition")
            err_msg <- if (!is.null(cond)) conditionMessage(cond) else as.character(val)
            stop(sprintf(
              "Could not evaluate argument '%s' in term '%s': %s",
              pos_names[i], base_name, trimws(err_msg)
            ), call. = FALSE)
          }
        }

        nm <- arg_names[i]
        if (nm == "") {
          arg_vals[[paste0("..", i)]] <- val
        } else {
          arg_vals[[nm]] <- val
        }
      }

      # optionally evaluate the whole call
      evaluated <- NULL
      if (evaluate_calls) {
        tmp <- try(eval(term_expr, envir = env), silent = TRUE)
        if (!inherits(tmp, "try-error")) evaluated <- tmp
      }

      entry <- c(
        list(label = gsub(pattern = '\\\"', replacement = "'", x = .deparse1(term_expr))),
        arg_vals,
        list(base_name = base_name)
      )
      if (!is.null(evaluated)) entry$.evaluated <- evaluated

      # For the elt_name (key in the output list), we still want the full name
      # but we MUST NOT change base_name in the entry list
      name_addon <- ""
      if (!is.null(arg_exprs$type)) name_addon <- paste0(name_addon, "_", .deparse1(arg_exprs$type))
      if (!is.null(arg_exprs$data)) name_addon <- paste0(name_addon, "_", .deparse1(arg_exprs$data))
      if (!is.null(arg_exprs$mode)) name_addon <- paste0(name_addon, "_", .deparse1(arg_exprs$mode))
      if (!is.null(arg_exprs$variant)) name_addon <- paste0(name_addon, "_", .deparse1(arg_exprs$variant))

      elt_name <- paste0(base_name, name_addon)
      # Ensure uniqueness
      if (elt_name %in% names(out)) {
        suffix <- 2
        while (paste0(elt_name, ".", suffix) %in% names(out)) suffix <- suffix + 1
        elt_name <- paste0(elt_name, ".", suffix)
      }
      out[[elt_name]] <- entry
    }
  }
  class(out) <- "iglm.formulainfo"
  out
}

#' @export
#' @method print iglm.formulainfo
print.iglm.formulainfo <- function(x, ..., max_items = 5) {
  n_terms <- length(x)
  cat(
    "<iglm.formulainfo> object with", n_terms,
    if (n_terms == 1L) "term" else "terms", "\n\n"
  )

  for (nm in names(x)) {
    term <- x[[nm]]

    cat("$", nm, " (", term$type, ")\n", sep = "")
    cat("  label: ", term$label, "\n", sep = "")

    if (term$type == "symbol") {
      # show value class or small preview
      val <- term$value
      if (is.null(val)) {
        cat("  value: NULL\n\n")
      } else {
        cat("  value: <", class(val)[1L], ">",
          if (is.atomic(val) && length(val) <= max_items) {
            paste0(" ", toString(val))
          },
          if (length(val) > max_items) " ...", "\n\n",
          sep = ""
        )
      }
    } else if (term$type == "call") {
      # show each argument entry
      arg_names <- setdiff(names(term), c("name", "type", "label", ".evaluated"))
      if (length(arg_names) == 0L) {
        cat("  (no arguments)\n\n")
      } else {
        for (an in arg_names) {
          val <- term[[an]]
          cat("  ", an, " = ", sep = "")
          if (is.null(val)) {
            cat("NULL\n")
          } else if (is.atomic(val) && length(val) <= max_items) {
            cat(toString(val), "\n")
          } else {
            cat("<", class(val)[1L], ">", sep = "")
            if (is.data.frame(val)) cat(" [", nrow(val), "x", ncol(val), "]", sep = "")
            cat("\n")
          }
        }
        cat("\n")
      }
    }
  }
  invisible(x)
}


map_to_mat <- function(map, n_units) {
  # Generate empty network
  mat <- matrix(0, nrow = n_units, ncol = n_units)
  for (i in 1:n_units) {
    if (length(map[[i + 1]]) > 0) {
      mat[i, map[[i + 1]]] <- 1
    }
  }
  return(mat)
}

set_to_vec <- function(set, n_units) {
  # Generate empty vector
  vec <- numeric(length = n_units)
  vec[set] <- 1
  return(vec)
}

XZ_to_R <- function(x_attribute, z_network, n_units) {
  x_attribute <- set_to_vec(set = x_attribute, n_units = n_units)
  z_network <- map_to_mat(map = z_network, n_units = n_units)
  return(list(x_attribute = x_attribute, z_network = z_network))
}

XYZ_to_R <- function(x_attribute, y_attribute, z_network, n_units, return_adj_mat) {
  # x_attribute = set_to_vec(set = x_attribute,n_units = n_units)
  # y_attribute = set_to_vec(set = y_attribute,n_units = n_units)
  if (return_adj_mat) {
    z_network_tmp <- map_to_mat(map = z_network, n_units = n_units)
  } else {
    z_network_tmp <- do.call(rbind, lapply(1:n_units, FUN = function(x) {
      tmp <- z_network[[x + 1]]
      if (length(tmp) == 0) {
        return(NA)
      } else {
        return(cbind(x, tmp))
      }
    }))
    z_network_tmp <- z_network_tmp[!is.na(z_network_tmp[, 1]), , drop = FALSE]
    if (length(z_network_tmp) == 0 || !is.matrix(z_network_tmp)) {
      z_network_tmp <- matrix(numeric(0), nrow = 0, ncol = 2)
    }
    colnames(z_network_tmp) <- c("from", "to")
  }
  return(list(
    x_attribute = x_attribute, y_attribute = y_attribute,
    z_network = z_network_tmp
  ))
}

check_overlap <- function(mat_1, mat_2) {
  colnames(mat_1) <- colnames(mat_2)
  combined <- rbind(mat_1, mat_2)
  return(duplicated(combined, fromLast = TRUE)[seq_len(nrow(mat_1))])
}

iglm.data.neighborhood <- function(neighborhood, directed = NA, n_units = NA) {
  if (!is.matrix(neighborhood) && !is.data.frame(neighborhood)) {
    if (length(neighborhood) == 0) {
      neighborhood <- matrix(numeric(0), nrow = 0, ncol = 2)
    } else {
      stop("`neighborhood` must be a matrix or data frame.", call. = FALSE)
    }
  }

  if (nrow(neighborhood) == 0) {
    if (ncol(neighborhood) != 0 && ncol(neighborhood) != 2) {
      stop("Empty neighborhood matrix must have 0 or 2 columns (edgelist).", call. = FALSE)
    }
    res <- list(
      neighborhood = matrix(numeric(0), nrow = 0, ncol = 2),
      overlap = matrix(numeric(0), nrow = 0, ncol = 2)
    )
    class(res) <- "iglm.data.neighborhood"
    return(res)
  }
  if (is.na(n_units)) {
    if (ncol(neighborhood) > 2) {
      n_units <- nrow(neighborhood)
    } else {
      n_units <- max(neighborhood)
    }
  }
  if (is.na(n_units)) {
    stop("n_units could not be inferred. Please provide n_units.")
  }
  if (ncol(neighborhood) == 2) {
    sp_nb <- spMatrix(
      nrow = n_units, ncol = n_units,
      i = neighborhood[, 1], j = neighborhood[, 2], x = rep(1, length(neighborhood[, 2]))
    )
    sp_nb_trans <- sparseMatrix(i = sp_nb@j + 1, j = sp_nb@i + 1, dims = sp_nb@Dim)

    overlap <- sp_nb %*% sp_nb_trans
    overlap <- as(overlap, "TsparseMatrix")
    overlap <- cbind(
      overlap@i + 1,
      overlap@j + 1
    )
    overlap <- overlap[overlap[, 1] != overlap[, 2], ]
  } else {
    positions <- which(neighborhood == 1, arr.ind = T)
    sp_nb <- spMatrix(
      nrow = ncol(neighborhood), ncol = ncol(neighborhood),
      i = positions[, 1], j = positions[, 2], x = rep(1, length(positions[, 2]))
    )
    sp_nb_trans <- sparseMatrix(i = sp_nb@j + 1, j = sp_nb@i + 1, dims = sp_nb@Dim)

    overlap <- as.matrix(sp_nb %*% sp_nb_trans > 0)
    diag(overlap) <- 0
    overlap <- which(overlap == 1, arr.ind = T)
    neighborhood <- which(neighborhood == 1, arr.ind = T)
  }
  res <- list(
    neighborhood = neighborhood,
    overlap = overlap
  )
  class(res) <- "iglm.data.neighborhood"
  return(res)
}

get_i <- function(x, i) {
  stopifnot(is.list(x), length(i) == 1L, is.numeric(i), i >= 1L)
  k <- 1L
  for (el in x) {
    if (k == i) {
      return(el)
    }
    k <- k + 1L
  }
  stop("subscript out of bounds")
}

#' @export
#' @method [[ iglm.data.list
`[[.iglm.data.list` <- function(x, i, ...) {
  item <- get_i(x, i)
  # browser()
  item$set_neighborhood_overlap(
    attr(x, "neighborhood")$neighborhood,
    attr(x, "neighborhood")$overlap
  )
  item
}

append_iglm.data <- function(x, y) {
  tmp <- c(x, y)
  attributes(tmp) <- attributes(x)
  class(tmp) <- "iglm.data.list"
  return(tmp)
}
#' @export
#' @method [ iglm.data.list
`[.iglm.data.list` <- function(x, i, ...) {
  # browser()
  res <- list()
  k <- 1
  for (j in i) {
    item <- get_i(x, j)
    item$set_neighborhood_overlap(
      attr(x, "neighborhood")$neighborhood,
      attr(x, "neighborhood")$overlap
    )
    res[[k]] <- item
    names(res)[k] <- j
    k <- k + 1
  }
  res
}

#' @export
#' @method print iglm.data.list
print.iglm.data.list <- function(x, ...) {
  # Header
  n_items <- length(x)
  cat(
    "List of iglm.data object with", n_items,
    if (n_items == 1L) "entry\n" else "entries\n"
  )

  # Summarize elements
  if (n_items == 0L) {
    cat("(empty list)\n")
    return(invisible(x))
  }

  nm <- names(x)
  if (is.null(nm)) nm <- paste0("[[", seq_len(n_items), "]]\n")

  for (i in seq_len(n_items)) {
    el <- x[[i]]
    name <- nm[i]
    cat(name, sep = "")
    print(el)
    cat("\n")
  }

  invisible(x)
}

formula_preprocess <- function(formula) {
  if (length(formula) != 3) {
    stop(
      "Formula must be two-sided with an 'iglm.data' object on the LHS.\nExample: my_iglm_data ~ edges() + attribute_y()",
      call. = FALSE
    )
  }

  lhs_expr <- formula[[2]]
  lhs_name <- deparse(lhs_expr)
  data_object <- tryCatch(
    eval(lhs_expr, envir = environment(formula)),
    error = function(e) {
      stop(
        sprintf(
          "The LHS of the formula ('%s') could not be found.\nUnlike standard glm where the response is a column name from a data frame (e.g., 'y ~ ...'), iglm requires an 'iglm.data' object on the LHS.\nWrap your attributes and network into an 'iglm.data' object first:\n  dat <- iglm.data(y_attribute = ..., x_attribute = ..., z_network = ...)\nand specify 'dat ~ ...'.",
          lhs_name
        ),
        call. = FALSE
      )
    }
  )

  if (!inherits(data_object, "iglm.data")) {
    stop(
      sprintf(
        "The LHS of the formula ('%s') is of class '%s', but iglm requires an 'iglm.data' object.\nUnlike standard glm where the response is a vector or column (e.g., 'y ~ x'), iglm models regression under network interference across an entire connected population.\nWrap your attributes and network into an 'iglm.data' object first:\n  dat <- iglm.data(y_attribute = %s, ...)\nand call:\n  fit <- iglm(dat ~ attribute_y() + spillover_yx() + edges())",
        lhs_name,
        class(data_object)[1],
        lhs_name
      ),
      call. = FALSE
    )
  }

  includes_degrees <- "degrees" %in% all.vars(formula)
  formula <- stats::update(formula, . ~ . - degrees)
  formula_info <- rhs_terms_as_list(formula)
  if (length(formula_info) == 0 && !includes_degrees) {
    stop("Formula must contain at least one term on the right-hand side.", call. = FALSE)
  }

  term_names <- character(length(formula_info))
  data_list <- list()
  type_list <- numeric(length(formula_info))
  coef_names <- character(length(formula_info))

  for (i in seq_along(formula_info)) {
    arglist <- formula_info[[i]]
    # Call the modular initialization system
    init <- InitIglmTerm(data_object = data_object, arglist = arglist)

    term_names[i] <- init$term_name
    data_list[[i]] <- if (is.null(init$data)) matrix(1) else init$data
    type_list[i] <- if (is.null(init$type)) 1L else init$type
    coef_names[i] <- init$coef_name
  }

  return(list(
    data_object = data_object,
    data_list = data_list,
    type_list = type_list,
    coef_names = coef_names,
    term_names = term_names,
    includes_degrees = includes_degrees
  ))
}

#' @title Format Term Names Using Variable Labels
#' @description Substitutes custom variable labels for canonical 'x', 'y', and 'z' in model term names.
#' @param term_names Character vector of term names.
#' @param data_object An \code{iglm.data} object or list with \code{label_x}, \code{label_y}, \code{label_z}.
#' @param canonical_names Logical. If \code{TRUE}, returns canonical term names without label substitution.
#' @return A character vector of formatted term names.
#' @noRd
format_term_names <- function(term_names, data_object, canonical_names = FALSE) {
  if (isTRUE(canonical_names) || is.null(data_object) || length(term_names) == 0) {
    return(term_names)
  }
  lx <- if (!is.null(data_object$label_x) && length(data_object$label_x) == 1 && !is.na(data_object$label_x)) data_object$label_x else "x"
  ly <- if (!is.null(data_object$label_y) && length(data_object$label_y) == 1 && !is.na(data_object$label_y)) data_object$label_y else "y"
  lz <- if (!is.null(data_object$label_z) && length(data_object$label_z) == 1 && !is.na(data_object$label_z)) data_object$label_z else "z"

  if (lx == "x" && ly == "y" && lz == "z") {
    return(term_names)
  }

  # Discover all registered terms from iglm and any loaded namespaces
  ns_list <- unique(c("iglm", loadedNamespaces()))
  registered_terms <- unique(unlist(lapply(ns_list, function(ns) {
    if (isNamespaceLoaded(ns)) {
      sub("^InitIglmTerm\\.", "", ls(asNamespace(ns), pattern = "^InitIglmTerm\\."))
    }
  })))

  # Mapping tokens for xyz dimensions
  token_map <- c(
    "x" = lx,
    "y" = ly,
    "z" = lz,
    "xx" = paste0(lx, "_", lx),
    "yy" = paste0(ly, "_", ly),
    "xy" = paste0(lx, "_", ly),
    "yx" = paste0(ly, "_", lx),
    "xz" = paste0(lx, "_", lz),
    "yz" = paste0(ly, "_", lz),
    "yc" = paste0(ly, "_c")
  )

  map_term <- function(term) {
    parts <- strsplit(term, "_")[[1]]
    parts <- sapply(parts, function(p) if (p %in% names(token_map)) token_map[[p]] else p, USE.NAMES = FALSE)
    paste(parts, collapse = "_")
  }

  term_translation <- stats::setNames(sapply(registered_terms, map_term, USE.NAMES = FALSE), registered_terms)

  sapply(term_names, function(name) {
    base <- sub("\\(.*", "", name)
    suffix <- sub("^[^(]+", "", name)
    if (base %in% names(term_translation)) {
      paste0(term_translation[[base]], suffix)
    } else {
      name
    }
  }, USE.NAMES = FALSE)
}

is_string_a_function_execution <- function(s) {
  obj <- try(str2lang(s), silent = TRUE)

  if (inherits(obj, "try-error")) {
    return(FALSE)
  }

  if (!is.call(obj)) {
    return(FALSE)
  }
  head_obj <- obj[[1]]

  if (is.symbol(head_obj)) {
    func_name <- as.character(head_obj)

    # List of special operators that are 'calls' but not 'executions'.
    special_operators <- c(
      "(", "[", "[[", "{", "$",
      "+", "-", "*", "/", "^", "%%", "%/%", "%*%",
      "<", "<=", "==", "!=", ">=", ">",
      "&", "&&", "|", "||", "!",
      "~", ":", "=", "<-", "<<-", "->", "->>"
    )
    if (func_name %in% special_operators) {
      return(FALSE)
    }

    return(TRUE)
  }

  return(TRUE)
}

#' @noRd
draw_simulation_envelope <- function(x, sim_matrix, color, alpha = 0.4, lwd_mean = 2) {
  x_poly <- c(x, rev(x))
  y_poly <- c(colMins(sim_matrix), rev(colMaxs(sim_matrix)))
  graphics::polygon(x_poly, y_poly, col = add_alpha(color, alpha_level = alpha), border = NA)
  graphics::lines(x, colMeans(sim_matrix), type = "l", col = color, lwd = lwd_mean)
  graphics::lines(x, colMins(sim_matrix), type = "l", col = color, lwd = 1)
  graphics::lines(x, colMaxs(sim_matrix), type = "l", col = color, lwd = 1)
}

#' @noRd
extract_assessment_matrix <- function(sim_list, metric, subkey = NULL) {
  sims <- lapply(sim_list, function(x) {
    val <- x[[metric]]
    if (!is.null(subkey)) val <- val[[subkey]]
    val
  })
  sims <- sims[!vapply(sims, is.null, logical(1))]
  if (length(sims) == 0) return(matrix(numeric(0), nrow = 0, ncol = 0))

  first_names <- names(sims[[1]])
  if (is.null(first_names) || all(vapply(sims, function(s) identical(names(s), first_names), logical(1)))) {
    mat <- do.call("rbind", sims)
    if (!is.null(first_names)) colnames(mat) <- first_names
    return(mat)
  }

  all_names <- unique(unlist(lapply(sims, names)))
  num_names <- suppressWarnings(as.numeric(all_names))
  all_names <- if (!any(is.na(num_names))) as.character(num_names[order(num_names)]) else sort(all_names)

  aligned <- lapply(sims, function(v) {
    res <- stats::setNames(rep(0, length(all_names)), all_names)
    res[names(v)] <- as.numeric(v)
    res
  })
  mat <- do.call("rbind", aligned)
  colnames(mat) <- all_names
  mat
}

#' @noRd
plot_assessment_multi <- function(observed, sim_main, sim_dots = list(),
                                  model_names, colors, xlab, ylab = "Percentage",
                                  x_at = NULL, x_labels = NULL, x_positions = NULL,
                                  x_margin = 0.3, lwd_mean = 2, use_envelope = FALSE) {
  all_sims <- c(list(sim_main), sim_dots)

  if (use_envelope) {
    x <- if (!is.null(x_positions)) {
      as.numeric(x_positions)
    } else if (!is.null(names(observed)) && all(is.finite(suppressWarnings(as.numeric(names(observed)))))) {
      as.numeric(names(observed))
    } else {
      seq_along(observed)
    }
    xlim <- c(min(x) - x_margin, max(x) + x_margin)
    all_vals <- c(as.numeric(observed), unlist(all_sims))
    ylim <- range(all_vals, na.rm = TRUE)

    plot(x, as.vector(observed),
         type = "n", xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim, las = 1, axes = FALSE
    )

    if (is.null(x_at)) {
      x_at <- pretty(range(x), n = 10)
    }

    if (is.null(x_labels)) {
      axis(side = 1, at = x_at, lwd = 0, lwd.ticks = 1)
    } else {
      axis(side = 1, at = x_at, labels = x_labels, lwd = 0, lwd.ticks = 1)
    }
    axis(side = 2, las = 1, lwd = 0, lwd.ticks = 1)

    box(bty = "l", lwd = 1)

    draw_simulation_envelope(x, all_sims[[1]], color = colors[1], alpha = 0.1, lwd_mean = lwd_mean)

    if (length(all_sims) > 1) {
      for (m in 2:length(all_sims)) {
        draw_simulation_envelope(x, all_sims[[m]], color = colors[m], alpha = 0.1, lwd_mean = lwd_mean)
      }
    }
    lines(x, as.vector(observed), type = "l", col = "black", lwd = 2)

    legend("topright",
           legend = c("Observed", model_names),
           col = c("black", colors),
           lty = c(1, 1, rep(1, max(0, length(model_names) - 1))),
           lwd = c(2, rep(lwd_mean, length(model_names))),
           bty = "n"
    )
    return(invisible(NULL))
  }
  
  # Check if observed or simulation matrices have column names to align on
  all_names <- names(observed)
  for (s in all_sims) {
    if (!is.null(colnames(s))) all_names <- union(all_names, colnames(s))
  }
  
  if (!is.null(all_names)) {
    num_names <- suppressWarnings(as.numeric(all_names))
    if (!any(is.na(num_names))) {
      all_names <- as.character(num_names[order(num_names)])
      if (all(is.finite(num_names))) {
        x <- as.numeric(all_names)
      } else {
        x <- seq_along(all_names)
      }
    } else {
      x <- seq_along(all_names)
    }
    
    obs_aligned <- stats::setNames(rep(0, length(all_names)), all_names)
    common_obs <- intersect(names(observed), all_names)
    obs_aligned[common_obs] <- as.numeric(observed[common_obs])
    observed <- obs_aligned
    
    all_sims <- lapply(all_sims, function(sim_mat) {
      if (!is.null(colnames(sim_mat))) {
        aligned <- matrix(0, nrow = nrow(sim_mat), ncol = length(all_names), dimnames = list(NULL, all_names))
        common <- intersect(colnames(sim_mat), all_names)
        aligned[, common] <- sim_mat[, common]
        aligned
      } else {
        sim_mat
      }
    })

    if (!is.null(x_positions) && length(x_positions) == length(all_names)) {
      x <- as.numeric(x_positions)
    } else {
      if (!is.null(x_labels) && length(x_labels) != length(all_names)) {
        x_labels <- all_names
        x_at <- x
      }
    }
  } else {
    x <- if (!is.null(x_positions)) {
      as.numeric(x_positions)
    } else if (!is.null(names(observed)) && all(is.finite(suppressWarnings(as.numeric(names(observed)))))) {
      as.numeric(names(observed))
    } else {
      seq_along(observed)
    }
  }
  
  xlim <- c(min(x) - x_margin, max(x) + x_margin)
  all_vals <- c(as.numeric(observed), unlist(all_sims))
  ylim <- range(all_vals, na.rm = TRUE)
  
  plot(x, as.vector(observed),
       type = "n", xlab = xlab, ylab = ylab,
       xlim = xlim, ylim = ylim, las = 1, axes = FALSE
  )
  
  if (is.null(x_at)) {
    if (all(x == floor(x), na.rm = TRUE)) {
      if (diff(range(x)) <= 10) {
        x_at <- seq(min(x), max(x))
      } else {
        p <- pretty(range(x))
        x_at <- unique(p[p == floor(p)])
      }
    } else {
      x_at <- pretty(range(x), n = 10)
    }
  }
  
  if (is.null(x_labels)) {
    axis(side = 1, at = x_at, lwd = 0, lwd.ticks = 1)
  } else {
    axis(side = 1, at = x_at, labels = x_labels, lwd = 0, lwd.ticks = 1)
  }
  axis(side = 2, las = 1, lwd = 0, lwd.ticks = 1)
  
  box(bty = "l", lwd = 1)
  
  draw_simulation_envelope(x, all_sims[[1]], color = colors[1], alpha = 0.1, lwd_mean = lwd_mean)
  
  if (length(all_sims) > 1) {
    for (m in 2:length(all_sims)) {
      draw_simulation_envelope(x, all_sims[[m]], color = colors[m], alpha = 0.1, lwd_mean = lwd_mean)
    }
  }
  lines(x, as.vector(observed), type = "l", col = "black", lwd = 2)
  
  legend("topright",
         legend = c("Observed", model_names),
         col = c("black", colors),
         lty = c(1, 1, rep(1, max(0, length(model_names) - 1))),
         lwd = c(2, rep(lwd_mean, length(model_names))),
         bty = "n"
  )
}

#' @noRd
plot_assessment_single <- function(observed, sim_matrix, xlab, ylab = "Percentage",
                                   box_col = "#87CEEB80", line_col = "#D55E00",
                                   x_at = NULL, x_labels = NULL, x_positions = NULL,
                                   x_margin = 0.3, use_envelope = FALSE) {
  if (use_envelope) {
    x <- if (!is.null(x_positions)) {
      as.numeric(x_positions)
    } else if (!is.null(names(observed)) && all(is.finite(suppressWarnings(as.numeric(names(observed)))))) {
      as.numeric(names(observed))
    } else {
      seq_along(observed)
    }
    xlim <- c(min(x) - x_margin, max(x) + x_margin)
    ylim <- range(c(sim_matrix, as.numeric(observed)), na.rm = TRUE)

    plot(x, as.vector(observed),
         type = "n", xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim, las = 1, axes = FALSE
    )

    if (is.null(x_at)) {
      x_at <- pretty(range(x), n = 10)
    }

    if (is.null(x_labels)) {
      axis(side = 1, at = x_at, lwd = 0, lwd.ticks = 1)
    } else {
      axis(side = 1, at = x_at, labels = x_labels, lwd = 0, lwd.ticks = 1)
    }
    axis(side = 2, las = 1, lwd = 0, lwd.ticks = 1)

    box(bty = "l", lwd = 1)

    draw_simulation_envelope(x, sim_matrix, color = box_col, alpha = 0.4, lwd_mean = 2)
    lines(x, as.vector(observed), type = "l", col = line_col, lwd = 2)
    return(invisible(NULL))
  }

  all_names <- names(observed)
  if (!is.null(colnames(sim_matrix))) all_names <- union(all_names, colnames(sim_matrix))
  
  if (!is.null(all_names)) {
    num_names <- suppressWarnings(as.numeric(all_names))
    if (!any(is.na(num_names))) {
      all_names <- as.character(num_names[order(num_names)])
      if (all(is.finite(num_names))) {
        x <- as.numeric(all_names)
      } else {
        x <- seq_along(all_names)
      }
    } else {
      x <- seq_along(all_names)
    }
    
    obs_aligned <- stats::setNames(rep(0, length(all_names)), all_names)
    common_obs <- intersect(names(observed), all_names)
    obs_aligned[common_obs] <- as.numeric(observed[common_obs])
    observed <- obs_aligned
    
    if (!is.null(colnames(sim_matrix))) {
      aligned <- matrix(0, nrow = nrow(sim_matrix), ncol = length(all_names), dimnames = list(NULL, all_names))
      common <- intersect(colnames(sim_matrix), all_names)
      aligned[, common] <- sim_matrix[, common]
      sim_matrix <- aligned
    }

    if (!is.null(x_positions) && length(x_positions) == length(all_names)) {
      x <- as.numeric(x_positions)
    } else {
      if (!is.null(x_labels) && length(x_labels) != length(all_names)) {
        x_labels <- all_names
        x_at <- x
      }
    }
  } else {
    x <- if (!is.null(x_positions)) {
      as.numeric(x_positions)
    } else if (!is.null(names(observed)) && all(is.finite(suppressWarnings(as.numeric(names(observed)))))) {
      as.numeric(names(observed))
    } else {
      seq_along(observed)
    }
  }
  
  xlim <- c(min(x) - x_margin, max(x) + x_margin)
  ylim <- range(c(sim_matrix, as.numeric(observed)), na.rm = TRUE)
  
  # 1. Initialize plot (axes = FALSE suppresses default axes and box)
  plot(x, as.vector(observed),
       type = "n", xlab = xlab, ylab = ylab,
       xlim = xlim, ylim = ylim, las = 1, axes = FALSE
  )
  
  if (is.null(x_at)) {
    if (all(x == floor(x), na.rm = TRUE)) {
      if (diff(range(x)) <= 10) {
        x_at <- seq(min(x), max(x))
      } else {
        p <- pretty(range(x))
        x_at <- unique(p[p == floor(p)])
      }
    } else {
      x_at <- pretty(range(x), n = 10)
    }
  }
  
  # 2. Draw ticks and labels directly on the plot boundary (suppress axis lines)
  if (is.null(x_labels)) {
    axis(side = 1, at = x_at, lwd = 0, lwd.ticks = 1)
  } else {
    axis(side = 1, at = x_at, labels = x_labels, lwd = 0, lwd.ticks = 1)
  }
  axis(side = 2, las = 1, lwd = 0, lwd.ticks = 1)
  
  # 3. Draw the exact continuous L-shaped corner along the plot boundary
  box(bty = "l", lwd = 1)
  
  # 4. Plot data layers
  boxplot(sim_matrix,
          at = x, boxwex = 0.5,
          add = TRUE, col = box_col, axes = FALSE, las = 1
  )
  
  lines(x, as.vector(observed), type = "l", col = line_col, lwd = 2)
}

#' @noRd
adjust_margin_for_yaxis <- function(y_vals, default_line = 2.5, default_mar_left = 4.1) {
  r <- range(y_vals, na.rm = TRUE)
  if (any(!is.finite(r))) {
    return(list(line = default_line, mar_left = default_mar_left))
  }
  ticks <- pretty(r)
  labels <- format(ticks, trim = TRUE)
  cex_axis <- if (!is.null(par("cex.axis"))) par("cex.axis") else 1
  csi <- if (!is.null(par("csi")) && par("csi") > 0) par("csi") else 0.2
  w_inches <- tryCatch(max(graphics::strwidth(labels, units = "inches", cex = cex_axis)), error = function(e) 0)
  w_lines <- w_inches / csi
  ylab_line <- max(default_line, 1 + w_lines + 0.8)
  mar_left <- max(default_mar_left, ylab_line + 1.2)
  list(line = ylab_line, mar_left = mar_left)
}

#' @noRd
plot_multitrace <- function(mat, xlab = "Iteration", ylab = "Coefficients", las = 1, bty = "l", ...) {
  mat <- as.matrix(mat)
  r <- range(mat, na.rm = TRUE)
  if (any(!is.finite(r))) r <- c(0, 1)

  adj <- adjust_margin_for_yaxis(r)
  old_mar <- par(mar = c(par("mar")[1], adj$mar_left, par("mar")[3], par("mar")[4]))
  on.exit(par(old_mar), add = TRUE)

  plot(NA,
    xlim = c(1, max(1, nrow(mat))), ylim = r,
    xlab = xlab, ylab = "", las = las, bty = bty, ...
  )
  title(ylab = ylab, line = adj$line)
  for (tmp in seq_len(ncol(mat))) {
    lines(y = mat[, tmp], x = seq_len(nrow(mat)), col = tmp)
  }
}

#' @noRd
build_constrained_xlab <- function(base_label, x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL,
                                   family_x = "binomial", family_y = "binomial") {
  format_token <- function(spec, var_name, idx, family) {
    if (is.null(spec)) return(NULL)
    if (is.function(spec)) {
      return(paste0(var_name, '[', idx, '] == "fn"'))
    }
    if (family != "binomial" && identical(spec, 1)) {
      paste0(var_name, "[", idx, "] > bar(", var_name, ")")
    } else if (family != "binomial" && identical(spec, 0)) {
      paste0(var_name, "[", idx, "] <= bar(", var_name, ")")
    } else {
      dep <- paste(deparse(spec), collapse = " ")
      if (length(spec) > 1 || !grepl("^[0-9.-]+$", dep)) {
        paste0(var_name, '[', idx, '] == "', gsub('"', "'", dep), '"')
      } else {
        paste0(var_name, "[", idx, "] == ", dep)
      }
    }
  }
  parts <- c(
    format_token(x_i, "x", "i", family_x),
    format_token(x_j, "x", "j", family_x),
    format_token(y_i, "y", "i", family_y),
    format_token(y_j, "y", "j", family_y)
  )
  if (length(parts) == 0) return(base_label)
  expr_str <- paste0('paste("', base_label, ' (", ', paste(parts, collapse = ', ", ", '), ', ")")')
  parse(text = expr_str)[[1]]
}

#' @noRd
get_assessment_constraint_xlab <- function(base_label, name, base_name,
                                           family_x = "binomial", family_y = "binomial") {
  raw_suffix <- sub(paste0("^", base_name, "_?"), "", name)
  tokens <- if (nzchar(raw_suffix)) unlist(strsplit(raw_suffix, "[,]+")) else character(0)
  parts <- c()
  has_i <- FALSE
  has_j <- FALSE
  for (token in tokens) {
    if (grepl("^(x|y)_(i|j)_(.+)$", token)) {
      var_base <- sub("^(x|y)_(i|j)_(.+)$", "\\1", token)
      idx <- sub("^(x|y)_(i|j)_(.+)$", "\\2", token)
      val <- sub("^(x|y)_(i|j)_(.+)$", "\\3", token)
      family <- if (var_base == "x") family_x else family_y
      if (idx == "i") has_i <- TRUE
      if (idx == "j") has_j <- TRUE
      
      if (family != "binomial" && val == "1") {
        parts <- c(parts, paste0(var_base, "[", idx, "] > bar(", var_base, ")"))
      } else if (family != "binomial" && val == "0") {
        parts <- c(parts, paste0(var_base, "[", idx, "] <= bar(", var_base, ")"))
      } else {
        parts <- c(parts, paste0(var_base, "[", idx, "] == ", val))
      }
    } else if (grepl("^mode_(.+)$", token)) {
      val <- sub("^mode_(.+)$", "\\1", token)
      if (val != "local") {
        parts <- c(parts, paste0('"', val, '"'))
      }
    } else if (nzchar(token)) {
      parts <- c(parts, paste0('"', gsub("_", " ", token), '"'))
    }
  }
  if (length(parts) > 0) {
    expr_str <- paste0('paste("', base_label, ' (", ', paste(parts, collapse = ', ", ", '), ', ")")')
    return(parse(text = expr_str)[[1]])
} else {
    return(base_label)
  }
}

#' @noRd
filter_nodes <- function(attr_vec, spec, family = "binomial") {
  if (is.null(spec)) return(rep(TRUE, length(attr_vec)))
  if (is.function(spec)) {
    res <- tryCatch(
      as.logical(spec(attr_vec)),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (is.null(res) || length(res) != length(attr_vec)) {
      res <- as.logical(vapply(attr_vec, spec, logical(1)))
    }
    return(res)
  }
  if (family == "binomial") {
    attr_vec %in% spec
  } else {
    m <- mean(attr_vec)
    res <- rep(FALSE, length(attr_vec))
    if (1 %in% spec) res <- res | (attr_vec > m)
    if (0 %in% spec) res <- res | (attr_vec <= m)
    other_spec <- spec[!spec %in% c(0, 1)]
    if (length(other_spec) > 0) res <- res | (attr_vec %in% other_spec)
    res
  }
}

#' @noRd
get_candidate_dyads <- function(directed, n_units = NULL, overlap = NULL, mode = "global",
                                x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL,
                                x_attribute = NULL, y_attribute = NULL,
                                family_x = "binomial", family_y = "binomial") {
  if (is.null(n_units) || n_units < 2) {
    return(matrix(integer(0), ncol = 2))
  }

  has_i_constr <- !is.null(x_i) || !is.null(y_i)
  has_j_constr <- !is.null(x_j) || !is.null(y_j)
  if (!directed) {
    if (!has_i_constr && has_j_constr) {
      x_i <- x_j
      y_i <- y_j
      x_j <- NULL
      y_j <- NULL
      has_i_constr <- TRUE
      has_j_constr <- FALSE
    }
  }
  has_constraints <- has_i_constr || has_j_constr

  if (mode == "local") {
    if (is.null(overlap) || nrow(overlap) == 0) {
      return(matrix(integer(0), ncol = 2))
    }
    if (!directed) {
      wrong_idx <- overlap[, 1] > overlap[, 2]
      correct_idx <- overlap[, 1] < overlap[, 2]
      overlap_clean <- rbind(
        overlap[correct_idx, c(1, 2), drop = FALSE],
        overlap[wrong_idx, c(2, 1), drop = FALSE]
      )
      dyads <- overlap_clean[!duplicated(overlap_clean), , drop = FALSE]
    } else {
      dyads <- overlap[overlap[, 1] != overlap[, 2], , drop = FALSE]
    }
  } else {
    if (!directed) {
      row_u <- unlist(lapply(2:n_units, function(j) seq_len(j - 1)))
      col_u <- rep(2:n_units, times = seq_len(n_units - 1))
      dyads <- cbind(row_u, col_u)
    } else {
      row_d <- rep(1:n_units, each = n_units - 1)
      col_d <- unlist(lapply(1:n_units, function(i) (1:n_units)[-i]))
      dyads <- cbind(row_d, col_d)
    }
  }

  if (nrow(dyads) == 0) {
    return(matrix(integer(0), ncol = 2))
  }

  if (has_constraints) {
    cond_sender <- filter_nodes(x_attribute, x_i, family_x) &
                   filter_nodes(y_attribute, y_i, family_y)
    cond_receiver <- filter_nodes(x_attribute, x_j, family_x) &
                     filter_nodes(y_attribute, y_j, family_y)
    units_sender <- which(cond_sender)
    units_receiver <- which(cond_receiver)

    if (directed) {
      qualifies <- (dyads[, 1] %in% units_sender) & (dyads[, 2] %in% units_receiver)
    } else {
      if (has_i_constr && has_j_constr) {
        qualifies <- ((dyads[, 1] %in% units_sender) & (dyads[, 2] %in% units_receiver)) |
                     ((dyads[, 1] %in% units_receiver) & (dyads[, 2] %in% units_sender))
      } else {
        qualifies <- (dyads[, 1] %in% units_sender) | (dyads[, 2] %in% units_sender)
      }
    }
    dyads <- dyads[qualifies, , drop = FALSE]
  }

  return(dyads)
}

#' Check for Misused Arguments from Standard GLMs
#'
#' Intercepts arguments commonly passed to base R's \code{glm()} that are either invalid,
#' unsupported, or handled differently in \code{iglm()}.
#'
#' @param args A list of arguments captured from \code{...}.
#' @return \code{invisible(NULL)} if no unexpected arguments are present.
#' @noRd
check_glm_arguments <- function(args) {
  if (length(args) == 0) {
    return(invisible(NULL))
  }
  arg_names <- names(args)

  # Check for standard glm arguments
  if (!is.null(arg_names)) {
    if ("data" %in% arg_names) {
      stop(
        "iglm does not accept a 'data' argument in the standard glm sense.\n",
        "Attributes (x, y) and network adjacency (z) must be bundled into an 'iglm.data' object ",
        "and passed as the response on the LHS of the formula: iglm(my_iglm_data ~ ...).\n",
        "Use `iglm.data(...)` to construct your data container first.",
        call. = FALSE
      )
    }

    if ("family" %in% arg_names) {
      stop(
        "'family' is not an argument to iglm().\n",
        "The distributional family is defined when creating the 'iglm.data' object ",
        "via 'family_y' and 'family_x' (e.g., iglm.data(..., family_y = 'binomial', family_x = 'normal')).",
        call. = FALSE
      )
    }

    # Any other named arguments
    named_extra <- arg_names[nzchar(arg_names)]
    if (length(named_extra) > 0) {
      stop(
        sprintf(
          "Unrecognized argument(s) passed to iglm(): %s.",
          paste(paste0("'", named_extra, "'"), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Unnamed positional arguments
  stop("Unrecognized positional argument(s) passed to iglm().", call. = FALSE)
}

