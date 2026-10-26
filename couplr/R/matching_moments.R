# ==============================================================================
# Linear moment constraints on a matched sample
# ==============================================================================
# A balance constraint that is not flow-representable is a linear constraint on
# the matched sample, and this file is its representation.
#
# Write x_ij = 1 when left unit i is paired with right unit j, and k = sum x_ij
# for the number of pairs. A standardized-difference bound on a covariate v,
#
#   | mean_matched_left(v) - mean_matched_right(v) |  <=  delta * s_v,
#
# is linear in x only when the denominator is a constant, so s_v is fixed once
# from the full pools rather than from the sample being chosen. The form is the
# pooled one standardized_difference() uses,
#
#   s_v = sqrt( (var(left[[v]]) + var(right[[v]])) / 2 ),
#
# taken through .weighted_moments() so that the constraint and the diagnostic
# that later reports on it are the same definition rather than two of them.
#
# Multiplying by k clears the means and leaves two one-sided linear rows,
#
#   sum_ij x_ij * (  (v_i - v_j) - delta*s_v )  <=  0
#   sum_ij x_ij * ( -(v_i - v_j) - delta*s_v )  <=  0,
#
# and a mean-difference bound is the same shape with delta*s_v replaced by the
# bound itself. Every row that results therefore has coefficients
#
#   a_ij = u_i - w_j - b,     u_i = d*t_i,  w_j = d*t_j,  b = d-signed bound,
#
# with d the direction of the row and t the covariate, possibly transformed.
#
# That additive decomposition is why the representation is three vectors of
# length n, m and 1 rather than a coefficient matrix. The Lagrangian relaxation
# reprices every admissible arc at every iteration, and repricing one arc under
# one row is two vector lookups and a subtraction. No n by m object is built at
# any point, which is what keeps a few hundred thousand admissible arcs against
# millions of cells affordable.
#
# Missing values are refused rather than dropped. standardized_difference()
# drops NA pairwise because it describes a sample that is already fixed; a
# constraint describes a sample still being chosen, where dropping a unit
# changes k, and the multiplication by k above is only valid when every pair
# contributes a coefficient. A covariate with NA has no linear representation
# here, so .moment_specs() errors instead of quietly matching on a different
# constraint than the one asked for.
#
# A pooled spread of zero has two causes, and they are not the same constraint.
# When both pools are constant at the same level, every matched subset has mean
# difference exactly 0 and the bound holds for any non-negative delta. That row
# is kept rather than dropped, with all-zero coefficients, so that the list of
# rows lines up one for one with what the caller asked for and a multiplier
# vector indexes it without a lookup; it reports itself as trivially satisfied.
#
# When both pools are constant at different levels the spread is still zero,
# but every matched subset carries the same nonzero mean difference, so the
# standardized difference is 0/0 and no subset achieves any bound on it. That
# is refused at specification time rather than carried as a satisfied row. A
# constraint that evaporates lets a solve certify an answer the caller did not
# ask for, which is the same failure that makes NA an error above. mean_diff
# states the constraint that is achievable there, its denominator being 1.
#
# A pool of fewer than two units is a third case. .weighted_moments() gives it
# an undefined variance rather than a zero one, so the pooled spread has no
# value and the bound delta * s_v names no number. Nothing is proven
# unachievable there: the quantity the bound refers to does not exist rather
# than being violated, and a one-unit pool is legitimate input that can supply
# at most one pair. Such a row is kept satisfied by default, and records that
# its denominator was undefined rather than zero, so a report can tell the two
# apart.
# ==============================================================================

.MOMENT_STATS <- c("std_diff", "mean_diff")

# A single finite number, the shape every bound in a spec has to take.
.moment_bound <- function(x, what, where) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", what, "` for ", where, " must be a single finite number.",
         call. = FALSE)
  }
  as.numeric(x)
}

# How a spec names its functional in a message or a report.
.moment_var_label <- function(vars, transform) {
  if (!is.null(transform)) {
    return(paste0("f(", paste(vars, collapse = ", "), ")"))
  }
  paste(vars, collapse = " * ")
}

# The covariate a spec constrains, read off one pool. A single variable reaches
# the transform as a numeric vector and several reach it as the data frame of
# those columns, whose default reduction is the row product.
.moment_values <- function(vars, transform, data, side) {
  cols <- data[, vars, drop = FALSE]
  if (is.null(transform)) {
    values <- if (length(vars) == 1L) cols[[1L]] else Reduce(`*`, as.list(cols))
  } else {
    values <- transform(if (length(vars) == 1L) cols[[1L]] else cols)
  }

  where <- .moment_var_label(vars, transform)
  if (!is.numeric(values) || is.matrix(values)) {
    stop("`transform` for ", where, " must return a numeric vector; ",
         "it returned ", class(values)[1L], ".", call. = FALSE)
  }
  if (length(values) != nrow(data)) {
    stop("`transform` for ", where, " must return one value per unit; ",
         "it returned ", length(values), " for a ", side, " pool of ",
         nrow(data), ".", call. = FALSE)
  }
  values <- as.numeric(values)
  if (anyNA(values) || any(!is.finite(values))) {
    stop("Moment constraint ", where, " is not finite everywhere in the ",
         side, " pool. A linear moment constraint is a sum over the matched ",
         "pairs, so every unit needs a value.", call. = FALSE)
  }
  values
}

# The columns a spec reads have to exist on both sides, be numeric, and be
# complete, before any transform gets to see them.
.moment_check_columns <- function(vars, left, right) {
  if (!is.character(vars) || !length(vars) || anyNA(vars) || any(vars == "")) {
    stop("`var` must be a non-empty character vector of column names.",
         call. = FALSE)
  }
  for (v in vars) {
    if (!v %in% names(left) || !v %in% names(right)) {
      stop("Moment constraint variable '", v,
           "' is not present in both pools.", call. = FALSE)
    }
    if (!is.numeric(left[[v]]) || !is.numeric(right[[v]])) {
      stop("Moment constraint variable '", v,
           "' must be numeric in both pools.", call. = FALSE)
    }
    if (anyNA(left[[v]])) {
      stop("Moment constraint variable '", v, "' carries NA in the left pool. ",
           "A linear moment constraint is a sum over the matched pairs, so ",
           "every unit needs a value.", call. = FALSE)
    }
    if (anyNA(right[[v]])) {
      stop("Moment constraint variable '", v, "' carries NA in the right ",
           "pool. A linear moment constraint is a sum over the matched pairs, ",
           "so every unit needs a value.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The pooled full-pool spread the standardized-difference bound divides by,
# through the package's own moment definition so the constraint and the
# diagnostic cannot drift apart. The per-pool moments come back alongside it,
# because a spread of zero has to be read together with the means to tell a
# satisfied constraint from an unachievable one.
.moment_spread <- function(values_left, values_right) {
  mL <- .weighted_moments(values_left, rep(1, length(values_left)))
  mR <- .weighted_moments(values_right, rep(1, length(values_right)))
  s <- suppressWarnings(sqrt((mL$var + mR$var) / 2))
  list(
    sd = if (length(s) != 1L || is.na(s) || !is.finite(s)) NA_real_ else s,
    var_left = mL$var, var_right = mR$var,
    mean_left = mL$mean, mean_right = mR$mean
  )
}

# Which of the degenerate readings a standardized-difference bound falls into,
# or NA when the spread is a usable number. Both pools constant at the same
# level is satisfied for free; both constant at different levels is refused by
# the caller of this function; an undefined variance is a spread that has no
# value rather than a bound that cannot hold.
.moment_degeneracy <- function(spread) {
  if (!is.na(spread$sd) && spread$sd != 0) return(NA_character_)
  if (isTRUE(spread$var_left == 0) && isTRUE(spread$var_right == 0)) {
    return(if (isTRUE(spread$mean_left == spread$mean_right)) {
      "constant"
    } else {
      "unachievable"
    })
  }
  "undefined_spread"
}

# One one-sided row. `limit` is the bound as the caller stated it, on the
# statistic itself; `bound` is what the coefficients carry, already through the
# denominator and already signed for this row's direction.
.moment_row <- function(vars, transform, stat, direction, limit, denominator,
                        origin, trivial_reason = NA_character_) {
  trivial <- !is.na(trivial_reason)
  if (trivial) {
    denominator <- 0
    bound <- 0
  } else {
    scale <- if (stat == "std_diff") denominator else 1
    bound <- if (direction > 0) limit * scale else -limit * scale
  }

  label <- sprintf("%s(%s) %s %s", stat, .moment_var_label(vars, transform),
                   if (direction > 0) "<=" else ">=", format(limit))
  if (trivial) {
    label <- paste0(label, switch(
      trivial_reason,
      constant = " [constant covariate: trivially satisfied]",
      undefined_spread = " [undefined pooled spread: trivially satisfied]"))
  }

  list(var = vars, stat = stat, transform = transform,
       direction = as.numeric(direction), limit = limit, bound = bound,
       denominator = denominator, trivial = trivial,
       trivial_reason = trivial_reason, origin = origin, label = label)
}

# One user-facing spec becomes one or two rows: a `max` gives the row that
# bounds the statistic from above, a `min` the row that bounds it from below,
# and a `std_diff` stating only `max` means the two-sided |.| <= max, which is
# both rows at once. A `mean_diff` stating only `max` is one-sided, because a
# mean difference has no symmetry to appeal to.
.moment_expand <- function(vars, transform, stat, max_bound, min_bound,
                           left, right, origin) {
  .moment_check_columns(vars, left, right)
  if (!is.null(transform) && !is.function(transform)) {
    stop("`transform` for ", .moment_var_label(vars, transform),
         " must be a function.", call. = FALSE)
  }

  where <- .moment_var_label(vars, transform)
  values_left <- .moment_values(vars, transform, left, "left")
  values_right <- .moment_values(vars, transform, right, "right")
  spread <- if (stat == "std_diff") {
    .moment_spread(values_left, values_right)
  } else {
    NULL
  }
  denominator <- if (is.null(spread)) 1 else spread$sd
  trivial_reason <- if (is.null(spread)) {
    NA_character_
  } else {
    .moment_degeneracy(spread)
  }

  has_max <- !is.null(max_bound)
  has_min <- !is.null(min_bound)
  if (!has_max && !has_min) {
    stop("Moment spec for ", where, " needs `max` or `min`.", call. = FALSE)
  }
  if (has_max) max_bound <- .moment_bound(max_bound, "max", where)
  if (has_min) min_bound <- .moment_bound(min_bound, "min", where)
  if (has_max && has_min && min_bound > max_bound) {
    stop("`min` (", format(min_bound), ") must not exceed `max` (",
         format(max_bound), ") for ", where, ".", call. = FALSE)
  }
  if (stat == "std_diff" && has_max && !has_min && max_bound < 0) {
    stop("A two-sided `std_diff` bound must not be negative; got ",
         format(max_bound), " for ", where, ".", call. = FALSE)
  }
  if (stat == "std_diff" && has_max && !has_min) {
    min_bound <- -max_bound
    has_min <- TRUE
  }

  if (identical(trivial_reason, "unachievable")) {
    stop("Moment constraint std_diff(", where, ") cannot be satisfied: ",
         where, " is constant at ", format(spread$mean_left),
         " in the left pool and at ", format(spread$mean_right),
         " in the right pool. The pooled spread is 0 while every matched ",
         "subset carries the same mean difference, ",
         format(spread$mean_left - spread$mean_right),
         ", so the standardized difference is 0/0 and no matched subset ",
         "achieves any standardized-difference bound on it. State the ",
         "achievable constraint with stat = \"mean_diff\", whose denominator ",
         "is 1.", call. = FALSE)
  }

  rows <- list()
  if (has_max) {
    rows[[length(rows) + 1L]] <- .moment_row(vars, transform, stat, 1,
                                             max_bound, denominator, origin,
                                             trivial_reason)
  }
  if (has_min) {
    rows[[length(rows) + 1L]] <- .moment_row(vars, transform, stat, -1,
                                             min_bound, denominator, origin,
                                             trivial_reason)
  }
  rows
}

# One entry of an explicit `moments` list, checked field by field.
.moment_from_entry <- function(entry, left, right) {
  if (!is.list(entry)) {
    stop("Each `moments` entry must be a list; got ", class(entry)[1L], ".",
         call. = FALSE)
  }
  known <- c("var", "stat", "transform", "max", "min")
  unknown <- setdiff(names(entry), known)
  if (length(unknown)) {
    stop("Unknown field(s) in a `moments` entry: ",
         paste(unknown, collapse = ", "), ". Known fields are ",
         paste(known, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(entry$var)) {
    stop("Every `moments` entry needs a `var`.", call. = FALSE)
  }

  stat <- entry$stat %||% "std_diff"
  if (!is.character(stat) || length(stat) != 1L || !stat %in% .MOMENT_STATS) {
    stop("`stat` must be one of ",
         paste0("\"", .MOMENT_STATS, "\"", collapse = ", "),
         "; got ", paste(format(stat), collapse = ", "), ".", call. = FALSE)
  }

  transform <- entry$transform
  if (!is.null(transform) && !is.function(transform)) {
    stop("`transform` for ",
         .moment_var_label(as.character(entry$var), transform),
         " must be a function.", call. = FALSE)
  }

  .moment_expand(as.character(entry$var), transform, stat,
                 entry$max, entry$min, left, right, origin = "moments")
}

# Two rows constrain the same thing when they are the same statistic on the
# same untransformed variable in the same direction. That is the only case in
# which keeping both would mean one of them was redundant rather than tighter.
.moment_identity <- function(row) {
  if (!is.null(row$transform) || length(row$var) != 1L) return(NA_character_)
  paste(row$stat, row$var, row$direction, sep = "\r")
}

# Normalize balance moment constraints
#
# `moments` is either NULL, a named numeric of standardized-difference bounds,
# one explicit spec, or a list of them. `max_std_diff`, when finite, states a
# standardized-difference bound on every variable in `vars`. Both routes end in
# the same list of one-sided rows.
#
# A variable constrained by both routes is an error rather than a silent
# preference for one of them, because a constraint that is dropped without
# anyone being told is a wrong answer that still validates and still solves.
.moment_specs <- function(moments = NULL, max_std_diff = NULL, vars = NULL,
                          left, right) {
  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("`left` and `right` must be data frames.", call. = FALSE)
  }

  rows <- list()

  if (!is.null(moments)) {
    if (is.numeric(moments) && !is.list(moments)) {
      if (length(moments)) {
        nms <- names(moments)
        if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
          stop("A numeric `moments` must name every bound, as in ",
               "c(x = 0.1, y = 0.05).", call. = FALSE)
        }
        if (anyDuplicated(nms)) {
          stop("A numeric `moments` names '", nms[anyDuplicated(nms)],
               "' more than once.", call. = FALSE)
        }
        for (v in nms) {
          rows <- c(rows, .moment_expand(v, NULL, "std_diff",
                                         max_bound = unname(moments[[v]]),
                                         min_bound = NULL, left, right,
                                         origin = "moments"))
        }
      }
    } else if (is.list(moments)) {
      entries <- if ("var" %in% names(moments)) list(moments) else moments
      for (entry in entries) {
        rows <- c(rows, .moment_from_entry(entry, left, right))
      }
    } else {
      stop("`moments` must be NULL, a named numeric vector, or a list of ",
           "specs; got ", class(moments)[1L], ".", call. = FALSE)
    }
  }

  if (!is.null(max_std_diff) && !identical(max_std_diff, Inf)) {
    if (!is.numeric(max_std_diff) || length(max_std_diff) != 1L ||
        is.na(max_std_diff)) {
      stop("`max_std_diff` must be a single number, or Inf for no bound.",
           call. = FALSE)
    }
    if (is.finite(max_std_diff)) {
      if (max_std_diff < 0) {
        stop("`max_std_diff` must not be negative; got ",
             format(max_std_diff), ".", call. = FALSE)
      }
      if (is.null(vars) || !length(vars)) {
        stop("`max_std_diff` bounds every matching variable, so `vars` must ",
             "name them.", call. = FALSE)
      }
      vars <- as.character(vars)
      taken <- vapply(rows, .moment_identity, character(1))
      for (v in unique(vars)) {
        default <- .moment_expand(v, NULL, "std_diff",
                                  max_bound = max_std_diff, min_bound = NULL,
                                  left, right, origin = "max_std_diff")
        clash <- intersect(vapply(default, .moment_identity, character(1)),
                           taken)
        if (length(clash)) {
          stop("`max_std_diff` and an explicit `moments` entry both bound the ",
               "standardized difference of '", v, "'. State it once; a ",
               "constraint that is dropped without saying so is a wrong ",
               "answer that still solves.", call. = FALSE)
        }
        rows <- c(rows, default)
      }
    }
  }

  rows
}

# Coefficients of one moment row
#
# Returns the additive decomposition `a_ij = u_i - w_j - b` as the three
# vectors it is, of length `nrow(left)`, `nrow(right)` and 1. A trivial row
# carries all-zero coefficients, so that its value is 0 on every pair set. The
# rows that reach this state are the ones for which that is true of the
# constraint as well: a covariate constant at one level on both sides, and a
# bound whose denominator was never defined.
.moment_coefficients <- function(spec, left, right) {
  if (!is.list(spec) || is.null(spec$stat) || is.null(spec$direction)) {
    stop("`spec` must be a normalized moment spec from `.moment_specs()`.",
         call. = FALSE)
  }
  if (isTRUE(spec$trivial)) {
    return(list(u = numeric(nrow(left)), w = numeric(nrow(right)), b = 0,
                label = spec$label))
  }
  values_left <- .moment_values(spec$var, spec$transform, left, "left")
  values_right <- .moment_values(spec$var, spec$transform, right, "right")
  list(u = spec$direction * values_left,
       w = spec$direction * values_right,
       b = spec$bound,
       label = spec$label)
}

# Coefficient objects are recognized by the decomposition they carry, so a
# caller may hand over one of them where a list of them is expected.
.is_moment_coefficients <- function(x) {
  is.list(x) && all(c("u", "w", "b") %in% names(x)) && is.numeric(x$u) &&
    is.numeric(x$w)
}

.as_moment_coefficient_list <- function(coefs) {
  if (is.null(coefs)) return(list())
  if (.is_moment_coefficients(coefs)) return(list(coefs))
  if (!is.list(coefs)) {
    stop("`coefs` must be a coefficient object or a list of them.",
         call. = FALSE)
  }
  coefs
}

# Value of a moment row on a set of pairs
#
# `sum_ij a_ij` over the pairs given as parallel index vectors, which is the
# amount by which the row is violated: 0 or below means the bound holds. Costs
# one pass over the pairs, never a pass over the pools.
.moment_violation <- function(coefs, i, j) {
  if (!.is_moment_coefficients(coefs)) {
    stop("`coefs` must be a coefficient object from `.moment_coefficients()`.",
         call. = FALSE)
  }
  i <- as.integer(i)
  j <- as.integer(j)
  if (length(i) != length(j)) {
    stop("`i` and `j` must name the same number of pairs; got ", length(i),
         " and ", length(j), ".", call. = FALSE)
  }
  if (!length(i)) return(0)
  if (anyNA(i) || anyNA(j) || any(i < 1L) || any(j < 1L) ||
      any(i > length(coefs$u)) || any(j > length(coefs$w))) {
    stop("`i` and `j` must index the left and right pools the coefficients ",
         "were built from.", call. = FALSE)
  }
  sum(coefs$u[i]) - sum(coefs$w[j]) - length(i) * coefs$b
}

# Reprice arcs under moment multipliers
#
# Adds `sum_r lambda_r * a^r_ij` to the cost of every arc, which is the
# Lagrangian cost the relaxed subproblem is solved at. One vectorized pass per
# row over the arcs, so the whole repricing is O(arcs * rows) and touches
# nothing of size n by m.
.moment_reprice <- function(arcs, coefs, lambda) {
  if (!is.data.frame(arcs) && !is.list(arcs)) {
    stop("`arcs` must be a data frame with `i`, `j` and `cost` columns.",
         call. = FALSE)
  }
  absent <- setdiff(c("i", "j", "cost"), names(arcs))
  if (length(absent)) {
    stop("`arcs` is missing the column(s) ", paste(absent, collapse = ", "),
         ".", call. = FALSE)
  }

  cost <- as.numeric(arcs$cost)
  coefs <- .as_moment_coefficient_list(coefs)
  if (!length(coefs)) return(cost)

  if (!is.numeric(lambda) || length(lambda) != length(coefs) ||
      anyNA(lambda) || any(!is.finite(lambda))) {
    stop("`lambda` must be ", length(coefs),
         " finite multipliers, one per moment row.", call. = FALSE)
  }

  i <- as.integer(arcs$i)
  j <- as.integer(arcs$j)
  for (r in seq_along(coefs)) {
    if (lambda[[r]] == 0) next
    cf <- coefs[[r]]
    if (!.is_moment_coefficients(cf)) {
      stop("`coefs[[", r, "]]` is not a coefficient object from ",
           "`.moment_coefficients()`.", call. = FALSE)
    }
    cost <- cost + lambda[[r]] * (cf$u[i] - cf$w[j] - cf$b)
  }
  cost
}
