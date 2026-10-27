# The benchmark's design: the constants every cell shares, the candidate sets, the cell table, the
# identity stamp each replicate is written with, and the simulate-represent-deploy machinery every
# replicate runs. Sourced by run.R and by anything reading the results, so a cell is defined in one
# place and no launcher carries its own copy.
#
# The oracle every regret is read against. Each candidate is fitted on each outer training set,
# the fits being the ladder's own, and scored on the deployment sample. Its truth is the mean over
# the outer folds, which is the mean the procedure's own truth is taken over, and the oracle is the
# candidate whose mean is highest. The regret is then what the procedure gives up against the best
# fixed candidate under the same training draws: both legs are read on the same draws, so neither
# carries the variance of a draw the other did not see, and the single-loop truth is the fold mean
# of the candidate it names, over the same five draws its reported score is read across. What
# remains in the oracle leg is the deployment sample's own noise, which a maximum over the
# candidates turns into an upward term. It shrinks as `n_deploy` grows, and the per-fold rows every
# replicate writes (`candidate`, `true_fold`) are what a check of it reads.

BENCH <- list(
  scale        = "full",
  # The one default of each selector a launcher, a runner and a reader all have to agree on. The
  # launcher asks this file for them rather than carrying its own copy.
  results      = "benchmark-results",
  results_b4   = "benchmark-results-b4",
  device       = Sys.getenv("TIMESIFT_DEVICE", unset = "cpu"),
  variables    = 10L,
  prevalence   = 0.10,
  auc          = 0.75,
  days         = 365L,
  step_hours   = 3,
  year_start   = "09-01",
  outer        = 5L,
  # Repetitions of the nested cross-validation the interval for the procedure's risk is read off.
  # One repetition refits the procedure once per unordered pair of outer folds, so this is what
  # sets the run's cost beside the selection's own.
  ncv_repeats  = 1L,
  metric       = "roc_auc",
  grains      = c("halfday", "day", "week", "month", "season", "year"),
  elasticnet_squares = TRUE,
  elasticnet_n_inner  = 5L,
  cnn_epochs   = 40L,
  n_deploy     = 3000L,
  deploy_chunk = 750L,
  deploy_draw  = 100000L,
  mechanisms   = c("none", "event", "season", "lag"),
  # The labels a results directory can carry for a block, mapped to the block they name. A cell is
  # read by the label its own file declares, so either label reaches one learner and one candidate
  # set.
  block_alias  = c(glmnet = "elasticnet"),
  design_seed  = c(none = 101L, event = 102L, season = 103L, lag = 104L),
  true_grain   = c(none = NA_character_, event = "day", season = "season", lag = "week"),
  # The grains whose bins tile the generating grain exactly, so the driver is still an exact
  # linear functional of the representation there. A selection landing on one of these has lost
  # nothing to averaging; it has only spent more coefficients than it needed.
  nesting      = list(day = c("halfday", "day"),
                      week = c("halfday", "day", "week"),
                      season = c("halfday", "day", "month", "season"))
)

bench_block <- function(block) {
  alias <- BENCH$block_alias[block]
  if (is.na(alias)) block else unname(alias)
}

# A candidate is a (grain, summary) pair. Every block searches the same set over the same inner
# folds, so a selection rate is read against the same alternatives whichever learner produced it.
bench_candidates <- function() {
  summaries <- list(mean = "mean", mmm = c("min", "mean", "max"))
  out <- expand.grid(stat = names(summaries), grain = BENCH$grains,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  out <- out[order(match(out$grain, BENCH$grains), out$stat), c("grain", "stat")]
  out$candidate <- paste(out$grain, out$stat, sep = ".")
  out$channels <- I(unname(summaries[out$stat]))
  rownames(out) <- NULL
  out
}

bench_cells <- function() {
  smoke <- identical(BENCH$scale, "smoke")
  sizes <- if (smoke) c(120L, 200L) else c(300L, 900L)
  elasticnet_cells <- expand.grid(mechanism = BENCH$mechanisms, n_unit = sizes,
                              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  elasticnet_cells$block <- "elasticnet"
  inner <- if (smoke) 2L else 5L
  elasticnet_cells$inner <- inner
  elasticnet_cells$replicates <- if (smoke) 3L else 200L
  cnn_cells <- data.frame(mechanism = BENCH$mechanisms, n_unit = max(sizes), block = "cnn",
                          inner = inner,
                          replicates = if (smoke) 2L else 100L, stringsAsFactors = FALSE)
  out <- rbind(elasticnet_cells, cnn_cells)
  out$cell_id <- sprintf("%s%s-%s-n%d", if (smoke) "smoke-" else "", out$block, out$mechanism,
                         out$n_unit)
  out[c("cell_id", "block", "mechanism", "n_unit", "inner", "replicates")]
}

# Every launcher takes `--key=value`, and a key whose default is `FALSE` may also be given bare as
# a flag. A script declares what it accepts by the defaults it passes, and anything else stops the
# run rather than being carried silently into it.
bench_args <- function(defaults) {
  out <- defaults
  for (a in commandArgs(trailingOnly = TRUE)) {
    key <- sub("^--([a-z_]+)(=.*)?$", "\\1", a)
    if (!grepl("^--[a-z_]+(=|$)", a) || !key %in% names(defaults)) {
      stop("unrecognised argument: ", a, "\naccepted: ",
           paste0("--", names(defaults), collapse = ", "), call. = FALSE)
    }
    out[[key]] <- if (grepl("=", a, fixed = TRUE)) sub("^--[a-z_]+=", "", a) else TRUE
  }
  out
}

# The smoke scale is a different scale, not a smaller run of the same one: it shrinks the design and
# it renames every cell, so its rows land in their own directory and cannot be read as the design
# the paper reports.
bench_scale <- function(scale) {
  if (identical(scale, "full")) {
    return(invisible("full"))
  }
  if (!identical(scale, "smoke")) {
    stop("--scale is \"full\" or \"smoke\", got \"", scale, "\".", call. = FALSE)
  }
  BENCH$scale <<- "smoke"
  BENCH$variables <<- 4L
  BENCH$n_deploy <<- 600L
  BENCH$deploy_chunk <<- 600L
  BENCH$outer <<- 3L
  BENCH$cnn_epochs <<- 4L
  BENCH$ncv_repeats <<- 1L
  invisible("smoke")
}

# The number of nested cross-validation repetitions, when a run asks for other than the default
# above. Every row carries it in its stamp, and summarise.R will not pool rows that differ in it, so
# a run at another count goes to its own results directory.
bench_ncv_repeats <- function(repeats) {
  if (is.na(repeats)) {
    return(invisible(BENCH$ncv_repeats))
  }
  r <- suppressWarnings(as.numeric(repeats))
  if (length(r) != 1L || is.na(r) || r < 1 || r != round(r)) {
    stop("--ncv_repeats must be a positive whole number, got \"", repeats, "\".", call. = FALSE)
  }
  BENCH$ncv_repeats <<- as.integer(r)
  invisible(BENCH$ncv_repeats)
}

bench_learner <- function(block) {
  switch(bench_block(block),
         elasticnet = timesift::elasticnet(squares = BENCH$elasticnet_squares,
                                           n_inner = BENCH$elasticnet_n_inner),
         cnn = timesift::cnn(epochs = BENCH$cnn_epochs, device = BENCH$device),
         stop("unknown block: ", block))
}

# Two calls to grain_matrix() cover every candidate, one per summary, because naming several
# grains already returns one representation each. The set is renamed to the candidate labels so
# select_grain() reports the pair rather than only the grain.
bench_representation <- function(readings, candidates) {
  wanted <- unique(candidates$stat)
  parts <- lapply(wanted, function(s) {
    channels <- candidates$channels[[match(s, candidates$stat)]]
    m <- timesift::grain_matrix(readings, "unit", "time", "reading",
                                  grain = BENCH$grains, stats = channels,
                                  year_start = BENCH$year_start)
    stats::setNames(unclass(m), paste(names(m), s, sep = "."))
  })
  set <- do.call(c, parts)
  timesift::timesift_set(set[candidates$candidate])
}

# Every run says what it was fed before it is fed it: the cell, the seeds, the package it is
# exercising and the candidate set it will search. A row without this cannot be traced to a cell
# and is not a row.
#
# The package the rows are fitted by is the one installed, and the commit the stamp carries is the
# checkout's, so the two are held to each other here: the installed version has to be the
# checkout's DESCRIPTION, and a full-scale run refuses a checkout with uncommitted changes, because
# a commit stamped onto rows fitted by code that commit does not hold is a row nobody can trace.
bench_stamp <- function(cell, replicate, candidates, pkg_dir, learner) {
  commit <- tryCatch(
    system2("git", c("-C", shQuote(pkg_dir), "rev-parse", "HEAD"), stdout = TRUE, stderr = NULL),
    error = function(e) NA_character_)
  dirty <- tryCatch(
    length(system2("git", c("-C", shQuote(pkg_dir), "status", "--porcelain"), stdout = TRUE,
                   stderr = NULL)) > 0L,
    error = function(e) NA)
  bench_assert_package(pkg_dir, dirty)
  list(
    scale = BENCH$scale,
    cell_id = cell$cell_id,
    block = cell$block,
    mechanism = cell$mechanism,
    n_unit = cell$n_unit,
    inner = cell$inner,
    outer = BENCH$outer,
    ncv_repeats = BENCH$ncv_repeats,
    replicate = replicate,
    design_seed = unname(BENCH$design_seed[cell$mechanism]),
    draw = replicate,
    deploy_draw = BENCH$deploy_draw + replicate,
    true_grain = unname(BENCH$true_grain[cell$mechanism]),
    metric = BENCH$metric,
    n_candidate = nrow(candidates),
    candidates = paste(candidates$candidate, collapse = ","),
    candidate_digest = .bench_digest(paste(candidates$candidate, collapse = ",")),
    learner = learner$name,
    learner_digest = .bench_digest(paste(utils::capture.output(utils::str(learner$params)),
                                         collapse = "|")),
    pkg_version = as.character(utils::packageVersion("timesift")),
    pkg_commit = if (length(commit) == 1L) commit else NA_character_,
    pkg_dirty = isTRUE(dirty),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    device = BENCH$device
  )
}

bench_assert_package <- function(pkg_dir, dirty) {
  declared <- tryCatch(
    as.character(read.dcf(file.path(pkg_dir, "DESCRIPTION"), fields = "Version")[[1L]]),
    error = function(e) NA_character_)
  installed <- as.character(utils::packageVersion("timesift"))
  if (is.na(declared) || !identical(installed, declared)) {
    stop("the installed timesift is ", installed, " and the checkout at ", pkg_dir, " declares ",
         declared, ". Install the checkout before running the benchmark: launch.ps1 does, or ",
         "install.packages(\"", pkg_dir, "\", repos = NULL, type = \"source\").",
         call. = FALSE)
  }
  stale <- bench_stale_functions(pkg_dir)
  if (length(stale)) {
    stop("the installed timesift is not the checkout at ", pkg_dir, ": ", length(stale),
         " function(s) differ, among them ", paste(utils::head(stale, 5L), collapse = ", "),
         ". Install the checkout before running the benchmark.", call. = FALSE)
  }
  if (identical(BENCH$scale, "full") && isTRUE(dirty)) {
    stop("the checkout at ", pkg_dir, " has uncommitted changes, and a full-scale run stamps its ",
         "commit onto every row. Commit, or run --scale=smoke.", call. = FALSE)
  }
  invisible(TRUE)
}

# Every top-level function the checkout's R/ defines is compared, formals and body, with the one
# the installed namespace holds. The version string cannot tell two builds of one version apart;
# the code can. Returns the names that are missing from the namespace or differ from the checkout.
bench_stale_functions <- function(pkg_dir) {
  ns <- asNamespace("timesift")
  files <- list.files(file.path(pkg_dir, "R"), pattern = "[.][Rr]$", full.names = TRUE)
  stale <- character()
  for (file in files) {
    for (expr in parse(file, keep.source = FALSE)) {
      defines <- is.call(expr) && (identical(expr[[1L]], as.name("<-")) ||
                                     identical(expr[[1L]], as.name("="))) &&
        is.name(expr[[2L]]) && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
      if (!defines) next
      name <- as.character(expr[[2L]])
      checkout <- eval(expr[[3L]], baseenv())
      installed <- get0(name, envir = ns, inherits = FALSE)
      if (!is.function(installed) ||
          !identical(formals(checkout), formals(installed)) ||
          !identical(body(checkout), body(utils::removeSource(installed)))) {
        stale <- c(stale, name)
      }
    }
  }
  stale
}

# The candidate set actually searched is read back off the selection and checked against the one
# the stamp declares. A run that searched a different set than it recorded is stopped, not saved.
bench_assert_candidates <- function(selection, stamp) {
  searched <- paste(sort(unique(selection$candidates$grain)), collapse = ",")
  declared <- paste(sort(strsplit(stamp$candidates, ",", fixed = TRUE)[[1L]]), collapse = ",")
  if (!identical(searched, declared)) {
    stop("cell ", stamp$cell_id, " replicate ", stamp$replicate, " searched {", searched,
         "} but declares {", declared, "}.", call. = FALSE)
  }
  invisible(TRUE)
}

.bench_digest <- function(text) {
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  con <- file(f, open = "wb")
  writeBin(charToRaw(text), con)
  close(con)
  substr(unname(tools::md5sum(f)), 1L, 12L)
}

# Bind the deployment sample's chunks along the unit axis. The chunks share a design and a reading
# grid, so their bins are identical; that is asserted rather than assumed, and everything else about
# the representation comes from grain_matrix().
.bench_bind <- function(parts) {
  first <- parts[[1L]]
  for (p in parts[-1L]) {
    stopifnot(identical(dimnames(p)[[2L]], dimnames(first)[[2L]]),
              identical(dimnames(p)[[3L]], dimnames(first)[[3L]]),
              identical(attr(p, "bin_start"), attr(first, "bin_start")))
  }
  d <- dim(first)
  n <- vapply(parts, function(p) dim(p)[1L], integer(1L))
  out <- array(NA_real_, dim = c(sum(n), d[2L], d[3L]),
               dimnames = list(unlist(lapply(parts, function(p) dimnames(p)[[1L]])),
                               dimnames(first)[[2L]], dimnames(first)[[3L]]))
  at <- 0L
  for (p in parts) {
    out[at + seq_len(dim(p)[1L]), , ] <- p
    at <- at + dim(p)[1L]
  }
  for (a in c("grain", "stats", "year_start", "bin_start", "bin_end", "bin_partial")) {
    attr(out, a) <- attr(first, a)
  }
  attr(out, "bin_n") <- do.call(rbind, lapply(parts, function(p) attr(p, "bin_n")))
  class(out) <- c("timesift_matrix", "array")
  out
}

.bench_simulate <- function(cell, n, seed, draw) {
  simulate_records(n = n, mechanism = cell$mechanism, variables = BENCH$variables,
                   prevalence = BENCH$prevalence, auc = BENCH$auc, days = BENCH$days,
                   step_hours = BENCH$step_hours, year_start = BENCH$year_start,
                   seed = seed, draw = draw)
}

# The deployment sample is drawn fresh for every replicate, from the same design, and its
# representation is built one chunk of units at a time so peak memory is set by the chunk rather
# than by its size.
.bench_deployment <- function(cell, stamp, candidates) {
  sizes <- rep(BENCH$deploy_chunk, BENCH$n_deploy %/% BENCH$deploy_chunk)
  rest <- BENCH$n_deploy %% BENCH$deploy_chunk
  if (rest) sizes <- c(sizes, rest)
  parts <- vector("list", length(sizes))
  ys <- vector("list", length(sizes))
  for (i in seq_along(sizes)) {
    sim <- .bench_simulate(cell, sizes[i], stamp$design_seed, stamp$deploy_draw * 100L + i)
    parts[[i]] <- unclass(bench_representation(sim$readings, candidates))
    ys[[i]] <- sim$y
  }
  set <- timesift_set(stats::setNames(
    lapply(candidates$candidate, function(cc) .bench_bind(lapply(parts, `[[`, cc))),
    candidates$candidate))
  list(set = set, y = do.call(rbind, ys))
}

# Every arm is scored the same way a ladder is: the metric per variable, then the mean over
# variables, so a deployment number and a reported number are the same quantity. Several metrics
# read one set of predictions, so the deployment sample is predicted once whatever is asked of it.
.bench_deploy_score <- function(fit, x, y, metrics) {
  p <- stats::predict(fit, x)
  vapply(metrics, function(m) {
    score <- timesift:::.metrics_reg$get(m)
    mean(vapply(colnames(y), function(v) score(y[rownames(p), v], p[, v]), numeric(1L)))
  }, numeric(1L))
}

# Reading the rows back. A quantity is picked by the three things that identify it and returned in
# replicate order, so two quantities of the same cell can be differenced replicate by replicate
# rather than as two means; the margin is the Monte Carlo one the replicate count buys.
bench_pick <- function(d, arm, quantity, metric = BENCH$metric) {
  keep <- d$arm == arm & d$quantity == quantity
  if (!is.na(metric)) {
    keep <- keep & !is.na(d$metric) & d$metric == metric
  }
  d[keep, , drop = FALSE]
}

bench_by_replicate <- function(d, arm, quantity, metric = BENCH$metric) {
  picked <- bench_pick(d, arm, quantity, metric)
  stats::setNames(picked$value[order(picked$replicate)], sort(picked$replicate))
}

# Two quantities of one cell, combined replicate by replicate. Arithmetic on two named vectors
# pairs them by position and drops the second one's names, so two arms holding different replicates
# would be differenced across replicates without a word: equal lengths give a wrong answer and
# unequal lengths that divide give a recycled one. The keys are held to each other here, and every
# paired quantity the summary reports comes through this.
bench_align <- function(left, right, op = `-`, what = c("the first", "the second")) {
  if (!identical(names(left), names(right))) {
    only <- function(a, b) {
      missing <- setdiff(names(a), names(b))
      if (!length(missing)) "none" else paste(missing, collapse = ",")
    }
    stop(what[1L], " and ", what[2L], " are not the same replicates: ", what[1L], " alone holds {",
         only(left, right), "} and ", what[2L], " alone holds {", only(right, left),
         "}. Rerun the cell rather than pairing them.", call. = FALSE)
  }
  stats::setNames(op(unname(left), unname(right)), names(left))
}

# The same, for the two quantities named by their arm: the pair the summary asks for most.
bench_paired <- function(d, left, right, op = `-`, metric = BENCH$metric) {
  label <- function(pair) paste0(d$cell_id[1L], " ", pair[1L], "/", pair[2L])
  bench_align(bench_by_replicate(d, left[1L], left[2L], metric),
              bench_by_replicate(d, right[1L], right[2L], metric),
              op, c(label(left), label(right)))
}

bench_margin <- function(v) 1.96 * stats::sd(v) / sqrt(length(v))

bench_proportion <- function(hit) {
  p <- mean(hit)
  c(p = p, mc = 1.96 * sqrt(p * (1 - p) / length(hit)))
}

# Wilson's score interval for a coverage rate. The normal approximation is the one that says a
# coverage of 1.000 is certain and a coverage near the nominal rate is further from it than it is;
# this is the interval a miss is read against.
bench_wilson <- function(hit, level = 0.95) {
  n <- length(hit)
  p <- mean(hit)
  z <- stats::qnorm(1 - (1 - level) / 2)
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(p = p, lower = centre - half, upper = centre + half)
}

.bench_row <- function(stamp, arm, candidate, outer_fold, metric, quantity, value) {
  data.frame(stamp[c("scale", "cell_id", "block", "mechanism", "n_unit", "inner", "outer",
                     "ncv_repeats", "replicate",
                     "design_seed", "draw", "deploy_draw", "true_grain", "n_candidate",
                     "candidate_digest", "learner_digest", "pkg_version", "pkg_commit",
                     "pkg_dirty", "r_version", "platform", "device")],
             sel_metric = stamp$metric,
             arm = arm, candidate = candidate, outer_fold = outer_fold, metric = metric,
             quantity = quantity, value = value, stringsAsFactors = FALSE)
}
