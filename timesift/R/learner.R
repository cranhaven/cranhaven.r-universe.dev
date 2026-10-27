#' Define a learner
#'
#' A learner is a pair of functions: one that fits a model to a representation and a response, and
#' one that predicts from it. Everything the package fits goes through this pair, so a learner of
#' your own sits beside the ones that ship and needs no change to the ladder, the folds or the
#' scoring.
#'
#' A learner declares what it can be handed and how it covers several responses. `reads` is
#' `"tabular"` where the bins reach it as a block of predictors and `"sequence"` where their order
#' in time is what it reads, and `multi` is `"joint"` where one fitted model covers every response
#' and `"separate"` where the learner fits one per response. Either way it is handed the whole
#' response matrix and returns one column per response, so nothing above the learner layer has to
#' know which it was, and the block of predictors is built once for the fit rather than once for
#' every response of it.
#'
#' `data` pins a learner to one representation. Left `NULL` the learner runs across every
#' representation of the run.
#'
#' @param name Name the learner is reported under.
#' @param fit A function of `(x, y, ...)`, where `x` is a `[unit, bin, channel]` array and `y` the
#'   response matrix for the same units, returning a fitted object. A `fit` that declares a
#'   `control` argument is handed the resolved [train_control()], and one that declares a `head`
#'   argument is handed the registered response head, whose `loss` and `activation` say what it
#'   is fitting toward. The learners that ship read both from there and hold no response of their
#'   own. One that declares a `group` argument is handed the grouping the outer fold map keeps
#'   whole, one value per unit of `x` or `NULL`, so a split it draws inside the fit keeps the
#'   same groups whole.
#' @param predict A function of `(model, x)` returning a `[unit, variable]` matrix of predictions
#'   for the units of `x`, in that order.
#' @param data A representation the learner is pinned to, or `NULL` to run across every
#'   representation of the run.
#' @param reads `"tabular"` or `"sequence"`.
#' @param multi `"separate"` where the learner fits one model per response, `"joint"` where one
#'   model covers them all. It is handed the whole response matrix either way; this is what the
#'   learner says it does with it, and what a report says of the candidate.
#' @param control A [train_control()] for this learner alone. The settings it names override the
#'   control the run was given; everything else is taken from that one.
#' @param needs Packages the learner requires. A learner that cannot run says so at once rather
#'   than falling back to something else.
#' @param params Settings carried with the learner and passed to `fit`.
#'
#' @return A `timesift_learner`.
#'
#' @examples
#' # The bin means of a unit, fed to one logistic regression per variable.
#' flat_glm <- learner(
#'   "flat_glm",
#'   fit = function(x, y, ...) {
#'     f <- as.data.frame(apply(x, c(1, 3), mean))
#'     lapply(seq_len(ncol(y)), function(j)
#'       stats::glm(y[, j] ~ ., data = f, family = stats::binomial()))
#'   },
#'   predict = function(model, x) {
#'     f <- as.data.frame(apply(x, c(1, 3), mean))
#'     vapply(model, function(m) stats::predict(m, f, type = "response"), numeric(nrow(f)))
#'   }
#' )
#' flat_glm
#'
#' @export
learner <- function(name, fit, predict, data = NULL, reads = c("tabular", "sequence"),
                    multi = c("separate", "joint"), control = NULL, needs = character(),
                    params = list()) {
  if (!is.function(fit) || !is.function(predict)) {
    stop("a learner needs a `fit` and a `predict` function.", call. = FALSE)
  }
  reads <- match.arg(reads)
  multi <- match.arg(multi)
  if (!is.null(data) && !inherits(data, "timesift_representation")) {
    stop("`data` is a representation such as grain(\"week\"), or NULL, got ",
         class(data)[1L], ".", call. = FALSE)
  }
  structure(list(name = name, fit = fit, predict = predict, data = data, reads = reads,
                 multi = multi, control = if (is.null(control)) NULL else .as_control(control),
                 needs = needs, params = params),
            class = "timesift_learner")
}

#' @export
print.timesift_learner <- function(x, ...) {
  cat("<timesift learner>", x$name, "\n")
  cat("reads   :", x$reads, "; one model per response:",
      if (identical(x$multi, "joint")) "no, joint" else "yes, separate", "\n")
  cat("data    :", if (is.null(x$data)) "every representation of the run" else .rep_label(x$data),
      "\n")
  if (length(x$params)) {
    cat("settings:", paste(names(x$params), vapply(x$params, .describe, character(1L)),
                           sep = " = ", collapse = ", "), "\n")
  }
  if (!is.null(x$control)) {
    named <- attr(x$control, "given")
    cat("training:", paste(named, vapply(named, function(nm) .describe(x$control[[nm]]),
                                         character(1L)),
                           sep = " = ", collapse = ", "), "\n")
  }
  if (length(x$needs)) {
    cat("needs   :", paste(x$needs, collapse = ", "), "\n")
  }
  invisible(x)
}

# A reference carries every field of a learner but its two functions, so it prints as the learner
# it refers to without the registry having to be asked for one.
#' @export
print.timesift_learner_ref <- function(x, ...) {
  print.timesift_learner(x, ...)
}

# A representation names itself; the fitting layer builds the objects and this reads the label off
# one without knowing anything else about it.
.rep_label <- function(rep) {
  label <- rep$label
  if (is.null(label) || !nzchar(label)) class(rep)[1L] else label
}

.describe <- function(v) {
  if (is.null(v)) "NULL" else paste(format(v), collapse = "/")
}

#' Register a learner
#'
#' Makes a learner available by name to [grain_ladder()] and to [learners()]. The learners that
#' ship are registered the same way, so there is no list of names inside the fitting code.
#'
#' @param name Name the learner is asked for by.
#' @param constructor A function returning a [learner()].
#' @param overwrite Replace an existing registration.
#'
#' @return The constructor, invisibly.
#'
#' @examples
#' learners()
#'
#' @export
register_learner <- function(name, constructor, overwrite = FALSE) {
  if (!is.function(constructor)) {
    stop("a learner registration takes a function returning a learner().", call. = FALSE)
  }
  .learners_reg$set(name, constructor, overwrite)
}

#' @rdname register_learner
#' @export
learners <- function() .learners_reg$names()

#' Fit one learner at one grain
#'
#' @param learner A [learner()], or the name of a registered one.
#' @param x A [grain_matrix()] result.
#' @param y The response for the same units.
#' @param response Name of the registered response head. `"presence_absence"` ships.
#' @param control The run's [train_control()]. The learner's own control overrides it on the
#'   settings that control names, and a setting given in `...` overrides both.
#' @param group One value per unit of `x` naming the group it belongs to, or `NULL`. A learner
#'   whose `fit` declares `group` draws its inner splits by it. Under [grouped_cv()] the run
#'   hands every fit the grouping its fold map carries.
#' @param ... Passed to the learner's `fit`.
#'
#' @return A `timesift_fit`, which [stats::predict()] takes a new representation.
#'
#' @examples
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 120)
#' units <- sprintf("p%02d", 1:40)
#' d <- data.frame(plot = rep(units, each = length(t)), t = rep(t, length(units)),
#'                 temp = as.numeric(replicate(length(units), rnorm(length(t)))))
#' x <- grain_matrix(d, plot, t, temp, grain = "month")
#' y <- matrix(rbinom(80, 1, 0.4), nrow = 40, dimnames = list(units, c("sp1", "sp2")))
#' fit <- fit_learner(elasticnet(), x, y)
#' dim(stats::predict(fit, x))
#'
#' @export
fit_learner <- function(learner, x, y, response = "presence_absence", control = NULL,
                        group = NULL, ...) {
  learner <- .as_learner(learner)
  .require_packages(learner)
  .check_matrix(x)
  spec <- .responses_reg$get(response)
  y <- spec$prepare(y)
  y <- .align_response(y, dimnames(x)[[1L]])
  # A setting given here overrides the one the learner carries, rather than reaching `fit` twice.
  given <- list(...)
  carried <- learner$params[setdiff(names(learner$params), names(given))]
  args <- c(list(x = x, y = y), carried, given)
  # A learner that trains under a control declares one; the resolved control reaches it through
  # that argument and through nothing else, so a learner with no training settings never sees one.
  # The response head reaches a fit the same way, so what a learner fits toward is the
  # registration and never a second copy of it inside the learner.
  declared <- names(formals(learner$fit))
  if ("control" %in% declared) {
    args$control <- .resolve_control(control, learner$control)
  }
  if ("head" %in% declared) {
    args$head <- spec
  }
  if ("group" %in% declared) {
    if (!is.null(group) && length(group) != dim(x)[1L]) {
      stop("`group` must have one value per unit, got ", length(group), " for ", dim(x)[1L],
           ".", call. = FALSE)
    }
    args["group"] <- list(if (is.null(group)) NULL else as.character(group))
  }
  model <- do.call(learner$fit, args)
  # The bins and the channels the fit was made on travel with it, and a representation asked to
  # predict is checked against them once, before any learner sees it: a calendar grain's bins are
  # named by their starts, so a record from another period is refused by the first bin that
  # differs rather than read by position.
  structure(list(learner = .learner_ref(learner), model = model, response = response,
                 variables = colnames(y), grain = attr(x, "grain"),
                 stats = attr(x, "stats"), bins = dimnames(x)[[2L]],
                 channels = dimnames(x)[[3L]]),
            class = "timesift_fit")
}

# What a fit was made on and what it is asked to predict have to be one representation: the
# same channels, and the same bins in the same order.
.check_same_representation <- function(object, newdata) {
  channels <- dimnames(newdata)[[3L]]
  if (!identical(channels, object$channels)) {
    stop("the representation predicted on has different channels or bins from the fitted one: ",
         "channels ", paste(channels, collapse = ", "), " here and ",
         paste(object$channels, collapse = ", "), " in the fit.", call. = FALSE)
  }
  bins <- dimnames(newdata)[[2L]]
  if (length(bins) != length(object$bins)) {
    stop("the representation predicted on has different channels or bins from the fitted one: ",
         .plural(length(bins), "bin"), " here and ", .plural(length(object$bins), "bin"),
         " in the fit.", call. = FALSE)
  }
  differs <- which(bins != object$bins)
  if (length(differs)) {
    i <- differs[1L]
    stop("the representation predicted on has different channels or bins from the fitted one: ",
         "bin ", i, " is ", bins[i], " here and ", object$bins[i], " in the fit. A calendar ",
         "grain is read by its bins' instants, so a fit predicts a record over the same period; ",
         "a lookback reads a span relative to each target and predicts any period.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# A fit refers to its learner rather than carrying it whole. R writes a closure's body out with the
# closure, so `saveRDS()` on a fit copies the code that made it, and a fit read back after an
# upgrade runs that copy inside the new package: this version's weights, the last version's model.
# A learner the registry can rebuild is written out as its name and its settings, and its two
# functions are looked up when they are needed; one defined outside any registry has no name to be
# rebuilt from and travels whole, which is the only form it has.
.learner_ref <- function(learner) {
  if (!.registry_owns(learner)) {
    return(learner)
  }
  structure(learner[setdiff(names(learner), c("fit", "predict"))],
            class = "timesift_learner_ref")
}

# The registry owns a learner where it holds that name and its constructor writes the same two
# functions. What is compared is the bodies rather than the closures, because a closure carries the
# frame of the constructor call that made it: `cnn(epochs = 5)` and `cnn()` are not the same
# object, and the registry rebuilds either, while a learner of one's own that happens to be called
# "cnn" it cannot rebuild at all.
.registry_owns <- function(learner) {
  if (!.learners_reg$has(learner$name)) {
    return(FALSE)
  }
  registered <- .learners_reg$get(learner$name)()
  identical(body(registered$fit), body(learner$fit)) &&
    identical(body(registered$predict), body(learner$predict))
}

# The two functions come from the registry and every setting from the reference, so a fit read back
# predicts through the code the package now holds, under the settings it was fitted with.
.rebuild_learner <- function(ref) {
  if (!.learners_reg$has(ref$name)) {
    stop("this fit was made by the \"", ref$name, "\" learner, which is not registered in this ",
         "session. Register it with register_learner(\"", ref$name,
         "\", ...) before predicting from the fit.", call. = FALSE)
  }
  built <- .learners_reg$get(ref$name)()
  # A setting held at NULL, `mtry = NULL` for a forest's default, is a setting; without
  # `keep.null` it would be dropped and the refit would be asked for an argument it never had.
  out <- utils::modifyList(unclass(built), unclass(ref), keep.null = TRUE)
  structure(out[names(unclass(built))], class = "timesift_learner")
}

# A response's own randomness starts from a seed of its own, so the model of one response is the
# same model whether it was fitted on its own or beside others, and in whatever order. The offset
# is taken from the response's name rather than from its position, because a learner that covers
# the responses one at a time is handed a single column and cannot see where it sat.
.variable_seeds <- function(seed, y) {
  labels <- colnames(y)
  offsets <- if (is.null(labels)) seq_len(ncol(y)) else vapply(labels, .name_offset, integer(1L))
  as.integer(seed) + as.integer(offsets)
}

.name_offset <- function(name) {
  code <- 0L
  for (b in utf8ToInt(name)) {
    code <- (code * 31L + b) %% 104729L
  }
  code
}

#' @param object A `timesift_fit`.
#' @param newdata A representation of the same channels for the units to predict.
#' @rdname fit_learner
#' @export
predict.timesift_fit <- function(object, newdata, ...) {
  .check_same_representation(object, newdata)
  p <- .as_learner(object$learner)$predict(object$model, newdata)
  p <- as.matrix(p)
  if (nrow(p) != dim(newdata)[1L]) {
    stop("the learner returned ", nrow(p), " rows for ", dim(newdata)[1L], " units.",
         call. = FALSE)
  }
  if (ncol(p) != length(object$variables)) {
    stop("the ", object$learner$name, " learner returned ", ncol(p), " columns for ",
         .plural(length(object$variables), "response"),
         ". A learner is handed the whole response matrix and returns one column per response, ",
         "in the order it was handed them.", call. = FALSE)
  }
  dimnames(p) <- list(dimnames(newdata)[[1L]], object$variables)
  p
}

#' @export
print.timesift_fit <- function(x, ...) {
  cat("<timesift fit>", x$learner$name, "at the", x$grain, "grain\n")
  cat("channels:", paste(x$stats, collapse = ", "), "\n")
  cat("response:", x$response, "on", .plural(length(x$variables), "variable"), "\n")
  invisible(x)
}

#' Combine learners, or representations, into a set
#'
#' `c()` on learners is the set of them, and on representations the set of those. A set handed to
#' `c()` again splices, so a set can be added to rather than rewritten, which is what `list()`
#' cannot do: `c(base, cnn())` where `base` is already a set.
#'
#' `models` and `sift` take either form. A length-one string is the name of a registered learner.
#'
#' @param ... Learners, representations, or sets of either.
#' @return A `timesift_models` for learners and a `timesift_sift` for representations.
#'
#' @examples
#' base <- c(elasticnet(), forest())
#' base
#' c(base, stepwise())
#'
#' c(grains("day", "week"), lookback("30 days"))
#'
#' @name combine
NULL

#' @rdname combine
#' @export
c.timesift_learner <- function(...) {
  .models(.splice(list(...), "timesift_learner"))
}

#' @rdname combine
#' @export
c.timesift_models <- function(...) {
  .models(.splice(list(...), "timesift_learner"))
}

#' @export
print.timesift_models <- function(x, ...) {
  cat("<timesift models>", .plural(length(x), "learner"), "\n")
  for (nm in names(x)) {
    cat(sprintf("  %-14s reads %s, %s\n", nm, x[[nm]]$reads, x[[nm]]$multi))
  }
  invisible(x)
}

# The set a `c()` of learners is, validated by the same call every entry point resolves `models`
# with, so a set and a bare list cannot disagree about what a valid one is.
.models <- function(x) {
  structure(.learner_list(x), class = "timesift_models")
}

.as_learner <- function(learner) {
  if (inherits(learner, "timesift_learner")) {
    return(learner)
  }
  if (inherits(learner, "timesift_learner_ref")) {
    return(.rebuild_learner(learner))
  }
  if (is.character(learner) && length(learner) == 1L) {
    return(.learners_reg$get(learner)())
  }
  stop("expected a learner() or the name of a registered one, got ", class(learner)[1L], ".",
       call. = FALSE)
}

# A learner that needs a package says so and stops. Two code paths, one with the package and one
# without, drift apart and the difference surfaces as a result nobody can place.
.require_packages <- function(learner) {
  missing <- learner$needs[!vapply(learner$needs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop("the ", learner$name, " learner needs ", paste(missing, collapse = " and "),
         ". Install it with install.packages(\"", missing[1L], "\").", call. = FALSE)
  }
  invisible(TRUE)
}

.align_response <- function(y, units) {
  if (identical(rownames(y), units)) {
    return(y)
  }
  missing <- setdiff(units, rownames(y))
  if (length(missing)) {
    stop(length(missing), " unit", if (length(missing) > 1L) "s are" else " is",
         " in the representation but not the response, first: ", missing[1L], ".", call. = FALSE)
  }
  y[units, , drop = FALSE]
}

# Flatten [unit, bin, channel] to [unit, bin * channel] in the array's own order, so the column
# names say which bin and which channel every predictor came from. A channel that holds the same
# number in every bin carries no bin of its own and is one predictor, read once: repeating it
# would put the same column in front of a penalised fit as often as the grain has bins, and give
# it that many chances of being drawn by a forest or picked by a forward search.
.flatten <- function(x) {
  d <- dim(x)
  channels <- dimnames(x)[[3L]]
  constant <- channels %in% (attr(x, "static") %||% character(0))
  moving <- unclass(x)[, , !constant, drop = FALSE]
  out <- matrix(as.numeric(moving), nrow = d[1L], ncol = d[2L] * sum(!constant))
  dimnames(out) <- list(dimnames(x)[[1L]],
                        paste(rep(channels[!constant], each = d[2L]),
                              rep(dimnames(x)[[2L]], times = sum(!constant)), sep = "@"))
  if (!any(constant)) {
    return(out)
  }
  once <- matrix(as.numeric(unclass(x)[, 1L, constant, drop = FALSE]), nrow = d[1L],
                 dimnames = list(dimnames(x)[[1L]], channels[constant]))
  cbind(out, once)
}

# The case weights a fit is made under, one per cell of the response it is handed: the head's
# where it carries a `weights`, and one everywhere where it does not. Every learner that ships
# reads them here, so what a rare response weighs is decided once, by the head. `fitting` marks
# the rows the model is fitted on, for a learner that holds some back: the head reads what it
# reads off those rows and weights every row.
.head_weights <- function(head, y, fitting = NULL) {
  if (is.null(head$weights)) {
    return(matrix(1, nrow(y), ncol(y), dimnames = dimnames(y)))
  }
  w <- head$weights(y, .fitting_rows(fitting, nrow(y)))
  if (!is.numeric(w) || !identical(dim(w), dim(y)) || anyNA(w) || any(w < 0)) {
    stop("a response head's `weights(y, fitting)` returns a numeric matrix of the response's ",
         "shape, with no missing or negative entry.", call. = FALSE)
  }
  w
}

# The family a learner fitting one model per response fits under is read off the response head's
# loss, so a head registered with a squared-error loss reaches the same learners as a
# presence-absence one and each fits the model that loss names.
.head_family <- function(head) {
  families <- c(binary_cross_entropy = "binomial", squared_error = "gaussian")
  if (!is.character(head$loss) || !head$loss %in% names(families)) {
    stop("a learner fitting one model per response has no family for the ",
         .describe(head$loss), " loss. It knows ", paste(names(families), collapse = " and "),
         ".", call. = FALSE)
  }
  families[[head$loss]]
}

# Inner folds for a fit that chooses a setting by cross-validation inside itself, dealt for one
# response under the fit's own seed, over the groups where the outer map carries a grouping. The
# deal is stratified on the response, so a rare outcome is spread over the inner folds as evenly as
# its count allows rather than wherever a plain deal drops it.
.inner_folds <- function(yj, v, seed, group = NULL) {
  key <- .group_key(group, length(yj))
  .check_fold_count(v, key, grouped = !is.null(group))
  .seeded_deal(.response_strata(as.numeric(tapply(yj, key, mean))), v, seed)[key]
}

# Whether every inner training set of a presence-absence response holds at least two of each
# outcome, the fewest a logistic path is fitted to. A response short of that has too few of one
# outcome to choose a penalty on.
.inner_fittable <- function(yj, inner) {
  all(vapply(unique(inner), function(k) {
    train <- yj[inner != k]
    sum(train == 1) >= 2L && sum(train == 0) >= 2L
  }, logical(1L)))
}

# The family the forward search fits under. The binomial one is the quasi-binomial: the
# coefficients and the deviance are the same, and it takes a case weight that is not a whole
# number without complaint, where the binomial family reads one as a fractional count of
# successes. The criterion is read off the deviance, see `.glm_aic()`.
.glm_family <- function(family) {
  switch(family, binomial = stats::quasibinomial(), gaussian = stats::gaussian())
}

# Akaike's criterion of a forward-search fit. For a 0/1 response the saturated log-likelihood is
# zero, so the criterion is the weighted deviance plus twice the rank, which is what the binomial
# family reports for unweighted data; the Gaussian family reports its own.
.glm_aic <- function(fit, family) {
  if (identical(family, "binomial")) fit$deviance + 2 * fit$rank else fit$aic
}

# Scaling belongs to the fold it is computed on: each column is centred on its mean and divided by
# its sample standard deviation, both taken over the fitting units alone. A linear model reads the
# bin-by-channel columns through it; a sequence encoder reads the channels through the same scaler,
# each channel laid out as one column over every unit and bin, which puts every channel on a
# workable scale while preserving the differences between units and between bins that carry the
# signal.
.scaler <- function(m) {
  centre <- colMeans(m)
  scale <- apply(m, 2L, stats::sd)
  scale[!is.finite(scale) | scale < 1e-8] <- 1
  list(centre = centre, scale = scale)
}

.apply_scaler <- function(s, m) {
  sweep(sweep(m, 2L, s$centre, "-"), 2L, s$scale, "/")
}
