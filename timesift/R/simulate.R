#' Simulate sensor records whose response acts at a known temporal grain
#'
#' Draws units carrying a year of sensor readings and a multi-variable presence-absence response
#' whose dependence on the record is a fixed linear functional of the record at one named grain.
#' The grain is therefore known before anything is fitted, which is what makes the output usable
#' for asking whether [select_grain()] finds it.
#'
#' @section What the true grain is:
#' The response is driven by `g_ij = sum_t w_j(t) * a_i(t)`, a weighted mean of unit `i`'s latent
#' *anomaly*: the record with the seasonal cycle every unit shares and the unit's own constant
#' offset taken out, since neither of those is temporally located and a grain of any width reports
#' both. The weights `w_j` are constant within the bins of one grain and zero outside
#' a short stretch of them, so `g` is exactly a linear combination of that grain's bin means. The
#' true grain of a mechanism is the **coarsest grain of [grain_matrix()] at which `g` is still an
#' exact linear functional of the representation**: at that grain and at every grain whose bins
#' nest inside it, no information about `g` has been averaged away, and at any coarser grain some
#' has. Finer grains keep the information but spread it over more coefficients, so they lose to
#' the true grain by variance rather than by bias, which is the tension the selection has to
#' resolve.
#'
#' @section The mechanisms:
#' \describe{
#'   \item{`"none"`}{No temporal signal. The driver is a standard normal drawn independently of the
#'     record, so nothing in the readings carries information about the response and no grain is
#'     correct. `grain` is `NA`.}
#'   \item{`"event"`}{An isolated event. The weights are uniform over three consecutive **day**
#'     bins at a fixed calendar position, one position per variable. True grain `"day"`: a week bin
#'     mixes the three days with four others and cannot be unmixed.}
#'   \item{`"season"`}{A smooth seasonal response. The weights are uniform over one whole
#'     **season** bin, one season per variable, cycled. True grain `"season"`: month and day bins
#'     nest inside a season so they are exact too, a year bin mixes all four seasons.}
#'   \item{`"lag"`}{A lagged, cumulative response. The weights decay geometrically over four
#'     consecutive **week** bins from a fixed anchor, one anchor per variable. True grain `"week"`:
#'     day bins nest inside weeks so they are exact too, month bins straddle week boundaries.}
#' }
#'
#' @section How the skill is set rather than emergent:
#' The driver is standardised to a standard normal by its population mean and standard deviation,
#' both computed in closed form from the generating parameters rather than from the drawn units, so
#' every draw and every chunk of a draw is on the same scale. The response is
#' `y_ij ~ Bernoulli(plogis(b0 + b1 z_ij))` with `b0` and `b1` solved by numerical integration so
#' that the marginal prevalence is `prevalence` and the population area under the ROC curve of `z`
#' is `auc`. `auc` is therefore a ceiling no fitted model reaches: the response is generated from
#' the latent record and the readings carry `sensor_sd` of measurement noise on top of it, and the
#' weights have to be estimated.
#'
#' @param n Number of units to draw.
#' @param mechanism Which generating mechanism, see The mechanisms.
#' @param variables Number of response variables. Each gets its own weights within the mechanism.
#' @param prevalence Marginal probability of presence, shared by every variable.
#' @param auc Population area under the ROC curve of the driver against the response.
#' @param from First reading instant, `"YYYY-MM-DD"`, read as UTC.
#' @param days Length of the record in days.
#' @param step_hours Sampling step in hours. Must divide 24.
#' @param seasonal Amplitude of the seasonal cycle shared by every unit, in reading units.
#' @param offset_sd Standard deviation of the unit-level thermal offset.
#' @param anomaly_sd Marginal standard deviation of each unit's AR(1) anomaly.
#' @param anomaly_days Correlation time of that anomaly, in days.
#' @param offset_effect Weight the unit-level offset enters the driver with. At the default `0`
#'   the driver reads the unit's *anomaly* alone, so a grain coarse enough to average the anomaly
#'   away loses the signal. At `1` the offset carries the response as well, and since every grain
#'   however coarse reports the offset, every grain is then equally good: that is the
#'   grain-invariant control, not a temporal mechanism.
#' @param sensor_sd Standard deviation of the measurement noise added to the latent record. The
#'   response is generated from the latent record; the readings returned carry this noise.
#' @param year_start `"MM-DD"` boundary of the hydrological year, passed to [grain_matrix()] when
#'   the mechanism's bins are located. Use the same value when representing the readings.
#' @param seed Seed of the design: the weights and the link coefficients. Two calls with the same
#'   `seed` and different `draw` share a design and draw independent units, which is what lets a
#'   held-out deployment sample be drawn from the same population as a training sample.
#' @param draw Seed of the unit draw. Also names the units, so two draws never collide.
#'
#' @return A `timesift_simulation`: a list with `readings`, the long table [grain_matrix()] takes;
#'   `y`, the `[unit, variable]` 0/1 response; `driver`, the standardised driver `z` behind it;
#'   `grain`, the true grain or `NA`; `weights`, the `[reading, variable]` weights defining the
#'   driver; `link`, the solved `b0` and `b1`; and `design`, the settings the draw is reproducible
#'   from.
#'
#' @seealso [select_grain()], which this exists to test, and [grain_matrix()], whose calendar the
#'   weights are defined on.
#'
#' @examples
#' sim <- simulate_records(n = 40L, mechanism = "event", variables = 2L, days = 60L)
#' sim
#' sim$grain
#' x <- grain_matrix(sim$readings, unit, time, reading, grain = c("day", "month"))
#' dim(x$day)
#'
#' @export
simulate_records <- function(n = 300L,
                             mechanism = c("none", "event", "season", "lag"),
                             variables = 10L,
                             prevalence = 0.1,
                             auc = 0.75,
                             from = "2021-09-01",
                             days = 365L,
                             step_hours = 3,
                             seasonal = 8,
                             offset_sd = 1,
                             anomaly_sd = 1,
                             anomaly_days = 2,
                             offset_effect = 0,
                             sensor_sd = 0.3,
                             year_start = "09-01",
                             seed = 1L,
                             draw = 1L) {
  mechanism <- match.arg(mechanism)
  n <- .whole(n, "n", 2L)
  variables <- .whole(variables, "variables", 1L)
  days <- .whole(days, "days", 1L)
  draw <- .whole(draw, "draw", 1L)
  if (24 %% step_hours != 0) {
    stop("`step_hours` must divide 24, got ", step_hours, ".", call. = FALSE)
  }
  if (prevalence <= 0 || prevalence >= 1) {
    stop("`prevalence` must lie strictly between 0 and 1, got ", prevalence, ".", call. = FALSE)
  }
  if (auc <= 0.5 || auc >= 1) {
    stop("`auc` must lie strictly between 0.5 and 1, got ", auc, ".", call. = FALSE)
  }

  per_day <- as.integer(24 / step_hours)
  when <- seq(as.POSIXct(from, tz = "UTC"), by = paste(step_hours, "hours"),
              length.out = days * per_day)
  phi <- exp(-step_hours / (24 * anomaly_days))
  season_wave <- seasonal * cos(2 * pi * (as.numeric(when) - as.numeric(when[1L])) /
                                  (365.25 * 86400))

  design <- .simulate_design(mechanism, variables, when, year_start, phi,
                             offset_effect * offset_sd, anomaly_sd, prevalence, auc, seed)

  units <- sprintf("d%03du%05d", draw, seq_len(n))
  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  set.seed(as.integer((seed + 100003 * draw) %% .Machine$integer.max))

  offset <- stats::rnorm(n, sd = offset_sd)
  anomaly <- .ar1_field(n, length(when), phi, anomaly_sd)
  reading <- anomaly + rep(season_wave, each = n) + offset +
    stats::rnorm(length(anomaly), sd = sensor_sd)

  z <- if (mechanism == "none") {
    matrix(stats::rnorm(n * variables), nrow = n)
  } else {
    sweep((anomaly + offset_effect * offset) %*% design$weights, 2L, design$sigma, "/")
  }
  z <- sweep(z, 2L, design$sign, "*")
  dimnames(z) <- list(units, sprintf("v%02d", seq_len(variables)))
  y <- matrix(stats::rbinom(length(z), 1L, stats::plogis(design$link$b0 + design$link$b1 * z)),
              nrow = n, dimnames = dimnames(z))

  out <- list(
    readings = data.frame(unit = rep(units, times = length(when)),
                          time = rep(when, each = n),
                          reading = as.numeric(reading),
                          stringsAsFactors = FALSE),
    y = y,
    driver = z,
    grain = design$grain,
    grain_stat = "mean",
    weights = design$weights,
    link = design$link,
    design = list(n = n, mechanism = mechanism, variables = variables, prevalence = prevalence,
                  auc = auc, from = from, days = days, step_hours = step_hours,
                  seasonal = seasonal, offset_sd = offset_sd, anomaly_sd = anomaly_sd,
                  anomaly_days = anomaly_days, offset_effect = offset_effect,
                  sensor_sd = sensor_sd, year_start = year_start,
                  seed = seed, draw = draw, bins = design$bins, anchor = design$anchor)
  )
  structure(out, class = "timesift_simulation")
}

#' @export
print.timesift_simulation <- function(x, ...) {
  d <- x$design
  cat("<timesift simulation>", d$mechanism, "over", .plural(d$n, "unit"), "x",
      .plural(nrow(x$weights), "reading"), "\n")
  cat("true grain:", if (is.na(x$grain)) "none, the response does not read the record"
      else paste0(x$grain, " (", d$bins, " bins), stat mean"), "\n")
  cat(sprintf("%d variables at prevalence %.3f (drawn %.3f), population AUC %.2f\n",
              d$variables, d$prevalence, mean(x$y), d$auc))
  invisible(x)
}

# The design is everything two draws must share: where the driver reads the calendar, how it is
# standardised, and the link that turns it into a response at the asked-for prevalence and skill.
# It depends on the seed and the reading grid, never on how many units are drawn, so a deployment
# sample and a training sample of different sizes are drawn from one population.
.simulate_design <- function(mechanism, variables, when, year_start, phi,
                             offset_sd, anomaly_sd, prevalence, auc, seed) {
  link <- .link_coefficients(prevalence, auc)
  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  set.seed(seed)
  direction <- rep_len(c(1, -1), variables)

  if (mechanism == "none") {
    return(list(grain = NA_character_, bins = NA_integer_, anchor = rep(NA_integer_, variables),
                weights = matrix(0, nrow = length(when), ncol = variables),
                sigma = rep(1, variables), sign = direction, link = link))
  }

  grain <- .mechanism_grain(mechanism)
  edges <- .bin_edges(when, grain, year_start)
  bin <- findInterval(as.numeric(when), as.numeric(edges$start))
  count <- tabulate(bin, nbins = length(edges$start))
  shape <- .mechanism_shape(mechanism)
  anchor <- .mechanism_anchors(mechanism, variables, edges$partial, length(shape))

  weights <- vapply(seq_len(variables), function(j) {
    k <- numeric(length(edges$start))
    k[anchor[j] + seq_along(shape) - 1L] <- shape
    k <- k / sum(k)
    k[bin] / count[bin]
  }, numeric(length(when)))
  sigma <- sqrt(offset_sd^2 + anomaly_sd^2 *
                  vapply(seq_len(variables), function(j) .ar1_quadform(weights[, j], phi),
                         numeric(1L)))
  list(grain = grain, bins = length(edges$start), anchor = anchor, weights = weights,
       sigma = sigma, sign = direction, link = link)
}

# The true grain of each mechanism, and the shape of the weights inside it. Written as a table so a
# fifth mechanism is a row rather than a branch in the generator.
.mechanism_grain <- function(mechanism) {
  c(event = "day", season = "season", lag = "week")[[mechanism]]
}

.mechanism_shape <- function(mechanism) {
  switch(mechanism,
         event = rep(1, 3L),
         season = 1,
         lag = exp(-(seq_len(4L) - 1L)))
}

# The anchors spread the variables over the record, over the positions at which the mechanism's
# whole stretch of bins falls on bins the record covers for their full calendar span. A variable
# never reads a partial bin, and two read the same stretch only when there are more variables than
# positions.
.mechanism_anchors <- function(mechanism, variables, partial, width) {
  starts <- seq_len(length(partial) - width + 1L)
  ok <- starts[vapply(starts, function(i) !any(partial[i + seq_len(width) - 1L]), logical(1L))]
  if (!length(ok)) {
    stop("the record holds no run of ", width, " whole ", .mechanism_grain(mechanism),
         " bins, which the ", mechanism, " mechanism needs. Lengthen `days`.", call. = FALSE)
  }
  if (variables >= length(ok)) {
    return(rep_len(ok, variables))
  }
  ok[as.integer(round(seq(1L, length(ok), length.out = variables)))]
}

# The bin boundaries the weights are defined on come from grain_matrix() itself, on a two-unit
# record over the same instants, so a mechanism is anchored to the calendar the representation will
# be built on rather than to a second copy of the binning rule.
.bin_edges <- function(when, grain, year_start) {
  probe <- data.frame(unit = rep(c("a", "b"), each = length(when)),
                      time = rep(when, times = 2L),
                      reading = 0, stringsAsFactors = FALSE)
  m <- grain_matrix(probe, "unit", "time", "reading", grain = grain, stats = "mean",
                     year_start = year_start)
  list(start = attr(m, "bin_start"), partial = attr(m, "bin_partial"))
}

# Var(sum_t w_t e_t) for an AR(1) with unit marginal variance, in one pass rather than through a
# T x T correlation matrix.
.ar1_quadform <- function(w, phi) {
  s <- numeric(length(w))
  acc <- 0
  for (i in seq_along(w)) {
    acc <- phi * acc + w[i]
    s[i] <- acc
  }
  2 * sum(w * s) - sum(w^2)
}

.ar1_field <- function(n, steps, phi, sd) {
  e <- matrix(stats::rnorm(n * steps, sd = sd * sqrt(1 - phi^2)), nrow = n, ncol = steps)
  e[, 1L] <- stats::rnorm(n, sd = sd)
  for (k in seq_len(steps - 1L) + 1L) {
    e[, k] <- phi * e[, k - 1L] + e[, k]
  }
  e
}

# b0 and b1 such that a standard normal driver produces the asked-for prevalence and area under the
# ROC curve, by integrating both on a fixed grid. The pair depends on nothing else, so it is solved
# once per (prevalence, auc) and kept.
.link_cache <- new.env(parent = emptyenv())

.link_coefficients <- function(prevalence, auc) {
  key <- sprintf("%.10f|%.10f", prevalence, auc)
  hit <- .link_cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  z <- seq(-8, 8, length.out = 8001L)
  dz <- z[2L] - z[1L]
  f <- stats::dnorm(z) * dz
  intercept <- function(b1) {
    span <- 8 * b1 + 40
    stats::uniroot(function(b0) sum(f * stats::plogis(b0 + b1 * z)) - prevalence,
                   c(-span, span), tol = 1e-10)$root
  }
  area <- function(b1) {
    p <- stats::plogis(intercept(b1) + b1 * z)
    pos <- f * p
    neg <- f * (1 - p)
    below <- cumsum(neg) - 0.5 * neg
    sum(pos * below) / (sum(pos) * sum(neg))
  }
  b1 <- stats::uniroot(function(b) area(b) - auc, c(1e-4, 20), tol = 1e-8)$root
  out <- list(b0 = intercept(b1), b1 = b1)
  .link_cache[[key]] <- out
  out
}

.whole <- function(x, name, least) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x) || x < least) {
    stop("`", name, "` must be a single whole number of at least ", least, ".", call. = FALSE)
  }
  x
}
