library(CIMEHR)

local({
  collect <- function(expr) {
    warnings <- list()
    value <- withCallingHandlers(expr, warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart('muffleWarning')
    })
    list(value = value, warnings = warnings)
  }
  failures <- function(x, message) {
    stopifnot(length(x$value) == 2L,
      all(vapply(x$value, inherits, logical(1), 'cimehr_failure')),
      all(vapply(x$value, function(f) identical(f$error, message), logical(1))),
      !any(vapply(x$warnings, function(w)
        grepl('restarting interrupted promise', conditionMessage(w), fixed = TRUE), logical(1))))
  }
  methods <- c('lmm', 'cimehr')

  # Single evaluation of a failed data promise.
  calls <- 0L
  x <- collect(method_comparisons(
    data = { calls <- calls + 1L; stop('data failed once') }, methods = methods))
  failures(x, 'data failed once')
  stopifnot(calls == 1L, length(x$warnings) == 0L)

  # Controls evaluation failure.
  calls <- c(controls = 0L, dots = 0L, data = 0L)
  x <- collect(method_comparisons(
    data = { calls['data'] <- calls['data'] + 1L; data.frame() }, methods = methods,
    controls = { calls['controls'] <- calls['controls'] + 1L; stop('controls failed once') },
    visit_covs = { calls['dots'] <- calls['dots'] + 1L; character() }))
  failures(x, 'controls failed once')
  stopifnot(identical(unname(calls), c(1L, 0L, 0L)))

  # Dots evaluation failure and deferred arguments.
  calls <- c(first = 0L, late = 0L, data = 0L)
  x <- collect(method_comparisons(
    data = { calls['data'] <- calls['data'] + 1L; data.frame() }, methods = methods,
    visit_covs = { calls['first'] <- calls['first'] + 1L; stop('dots failed once') },
    long_covs = { calls['late'] <- calls['late'] + 1L; character() }))
  failures(x, 'dots failed once')
  stopifnot(identical(unname(calls), c(1L, 0L, 0L)))

  # Warning before an argument error.
  calls <- 0L
  x <- collect(method_comparisons(
    data = { calls <- calls + 1L; warning('caller warning'); stop('data failed after warning') },
    methods = methods))
  failures(x, 'data failed after warning')
  stopifnot(calls == 1L, length(x$warnings) == 1L,
    identical(conditionMessage(x$warnings[[1L]]), 'caller warning'))

  # Deferred arguments with an empty method list.
  calls <- c(methods = 0L, controls = 0L, dots = 0L, data = 0L)
  x <- collect(method_comparisons(
    data = { calls['data'] <- calls['data'] + 1L; stop('unused data') },
    methods = { calls['methods'] <- calls['methods'] + 1L; character() },
    controls = { calls['controls'] <- calls['controls'] + 1L; stop('unused controls') },
    visit_covs = { calls['dots'] <- calls['dots'] + 1L; stop('unused dots') }))
  stopifnot(identical(x$value, setNames(list(), character())),
    identical(unname(calls), c(1L, 0L, 0L, 0L)), length(x$warnings) == 0L)

  # Missing data argument.
  x <- collect(method_comparisons(methods = methods))
  stopifnot(length(x$value) == 2L, length(x$warnings) == 0L,
    all(vapply(x$value, function(f) inherits(f, 'cimehr_failure') &&
      grepl('"data" is missing', f$error, fixed = TRUE), logical(1))))

  # Quiet-state restoration after nested and exceptional exits.
  ns <- asNamespace('CIMEHR')
  quiet <- get('.cimehr_without_identification_signal', ns)
  state <- get('.cimehr_identification_signal', ns)
  stopifnot(identical(state$quiet, FALSE))
  tryCatch(quiet(stop('quiet restoration test')), error = function(e) NULL)
  stopifnot(identical(state$quiet, FALSE))
  quiet({
    stopifnot(identical(state$quiet, TRUE))
    tryCatch(quiet(stop('nested restoration test')), error = function(e) NULL)
    stopifnot(identical(state$quiet, TRUE))
  })
  stopifnot(identical(state$quiet, FALSE))

  # Public fit warnings inside a data expression.
  set.seed(813)
  n <- 600L
  z <- rbinom(n, 1, .5); xx <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  q <- rnorm(n, sd = .3); rx <- rnorm(n, sd = .3); rw <- rnorm(n, sd = .3)
  id <- rep(seq_len(n), each = 6L)
  d <- data.frame(id = id, time = rep(seq(.05, .95, length.out = 6L), n),
    C = 1, Z = z[id], X = xx[id], W = w[id])
  obs <- (.8 * u + q + rx * xx + rw * w)[id]
  d$R <- as.integer(-.2 + .8 * d$X + .4 * d$Z + obs + rnorm(length(id)) > 0)
  d$Y <- .2 + .15 * d$time + .8 * d$Z - .4 * d$X +
    (.6 * u + rnorm(n, sd = .4))[id] + rnorm(length(id), sd = .4)
  d$Y[d$R == 0] <- NA_real_
  calls <- c(data = 0L, controls = 0L, dots = 0L)
  x <- collect(method_comparisons(
    data = {
      calls['data'] <- calls['data'] + 1L
      fit_method('cimehr', d, visit_covs = c('Z', 'X'), long_covs = c('Z', 'X'),
        obs_fixed_covs = c('Z', 'X'), censor_col = 'C')
      d
    }, methods = 'cimehr',
    controls = { calls['controls'] <- calls['controls'] + 1L; list() },
    visit_covs = { calls['dots'] <- calls['dots'] + 1L; c('Z', 'X') },
    long_covs = c('Z', 'X'), obs_fixed_covs = c('Z', 'X'), censor_col = 'C'))
  stopifnot(identical(unname(calls), c(1L, 1L, 1L)), length(x$warnings) == 1L,
    inherits(x$warnings[[1L]], 'cimehr_identification_warning'),
    inherits(x$value[[1L]], 'cimehr_fit'),
    length(x$value[[1L]]$diagnostics$identification_warning) > 0L,
    identical(state$quiet, FALSE))
})
