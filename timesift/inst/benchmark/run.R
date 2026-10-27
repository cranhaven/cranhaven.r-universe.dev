#!/usr/bin/env Rscript
# One or more replicates of one benchmark cell. Every replicate is a self-contained unit of work:
# it draws its own units, runs the selection, the single-loop reading and the oracle against an
# independent deployment sample, and writes one tidy file. A run that is killed loses the replicate
# in flight and nothing else, and restarting the same command skips whatever is already on disk.
#
# Usage:
#   Rscript inst/benchmark/run.R --cell=elasticnet-event-n300 --reps=1:200 --out=<dir> [--force]
#   Rscript inst/benchmark/run.R --cell=elasticnet-none-n300 --ncv_repeats=10 --out=<other dir>
#   Rscript inst/benchmark/run.R --scale=smoke --list

suppressWarnings(suppressMessages({
  library(timesift)
}))

here <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(here)) dirname(normalizePath(sub("^--file=", "", here[1L]))) else getwd()
source(file.path(here, "design.R"))

opt <- bench_args(list(cell = NA_character_, reps = NA_character_, out = NA_character_,
                       force = FALSE, list = FALSE, pkg = NA_character_, scale = "full",
                       ncv_repeats = NA_character_))
bench_scale(opt$scale)
bench_ncv_repeats(opt$ncv_repeats)
pkg_dir <- if (is.na(opt$pkg)) dirname(dirname(here)) else opt$pkg

cells <- bench_cells()
if (opt$list) {
  print(cells)
  quit(save = "no")
}
if (is.na(opt$cell) || !opt$cell %in% cells$cell_id) {
  stop("--cell must name one of: ", paste(cells$cell_id, collapse = ", "), call. = FALSE)
}
cell <- cells[match(opt$cell, cells$cell_id), ]
reps <- if (is.na(opt$reps)) seq_len(cell$replicates) else eval(parse(text = opt$reps))
if (!is.numeric(reps) || anyNA(reps) || any(reps < 1L)) {
  stop("--reps must evaluate to positive whole numbers, e.g. 1:200 or c(3,7).", call. = FALSE)
}
out_dir <- file.path(if (is.na(opt$out)) file.path(getwd(), BENCH$results) else opt$out,
                     cell$cell_id)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

candidates <- bench_candidates()
learner <- bench_learner(cell$block)


bench_replicate <- function(cell, replicate, candidates, learner, pkg_dir) {
  stamp <- bench_stamp(cell, replicate, candidates, pkg_dir, learner)
  message(sprintf("[%s] replicate %d: n=%d, %s, %d candidates (%s), inner=%d, outer=%d, %s@%s",
                  stamp$cell_id, replicate, stamp$n_unit, stamp$mechanism, stamp$n_candidate,
                  stamp$candidate_digest, stamp$inner, stamp$outer, stamp$pkg_version,
                  substr(stamp$pkg_commit, 1L, 8L)))
  clock <- list()
  tick <- function(name, expr) {
    t0 <- Sys.time()
    out <- force(expr)
    clock[[name]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    out
  }

  sim <- tick("simulate", .bench_simulate(cell, cell$n_unit, stamp$design_seed, replicate))
  set <- tick("represent", bench_representation(sim$readings, candidates))
  dep <- tick("deployment", .bench_deployment(cell, stamp, candidates))
  folds <- fold_map(sim$y, v = BENCH$outer, seed = replicate)

  sel <- tick("select", select_grain(set, sim$y, learner, folds = folds, inner = cell$inner,
                                     metric = BENCH$metric, interval = "nested_cv",
                                     repeats = BENCH$ncv_repeats, verbose = FALSE))
  bench_assert_candidates(sel, stamp)
  lad <- tick("ladder", grain_ladder(set, sim$y, learner, folds = folds, metric = BENCH$metric,
                                      keep_fits = TRUE, verbose = FALSE))

  f <- attr(sel, "folds")
  levels <- sort(unique(f))
  fits <- attr(lad, "fits")

  # Every candidate scored on the deployment sample after each outer training set, through the
  # ladder's own per-fold fits: a [fold, candidate] grid of true scores. The oracle, the
  # single-loop reading and the procedure are read off this one grid, each averaged over the same
  # outer training sets; the oracle is defined in design.R.
  deployed <- tick("deploy", vapply(candidates$candidate, function(cc) {
    vapply(levels, function(k) {
      fit <- fits[[paste(cc, learner$name, k, sep = "|")]]
      if (is.null(fit)) {
        stop("the ladder kept no fit for ", cc, " on outer fold ", k, ".", call. = FALSE)
      }
      mean(.bench_deploy_score(fit, dep$set[[cc]], dep$y, BENCH$metric))
    }, numeric(1L))
  }, numeric(length(levels))))
  truth <- colMeans(deployed)

  # The refit the procedure makes at each outer fold, at line 12 of its own algorithm, is the
  # ladder's fit of the candidate it selected there. Scored on the deployment sample instead of on
  # the outer fold, it gives the reported number something to be honest about.
  picked <- sel$selected$grain[match(levels, sel$selected$fold)]
  true_fold <- deployed[cbind(seq_along(levels), match(picked, colnames(deployed)))]

  # The procedure fitted on every unit, which the nested interval is an interval for, scored on the
  # deployment sample the same way every other truth here is.
  true_full <- tick("deploy_full", mean(.bench_deploy_score(sel$final$fit,
                                                            dep$set[[sel$final$grain]], dep$y,
                                                            BENCH$metric)))

  grid <- summary(lad)
  single <- grid$grain[which.max(grid$score)]
  across <- sel$estimate[sel$estimate$interval == "variables", ]
  nested <- sel$estimate[sel$estimate$interval == "nested_cv", ]
  sel_est <- across[across$metric == BENCH$metric, ]
  ncv_est <- nested[nested$metric == BENCH$metric, ]
  ncv_all <- sel$nested_cv[sel$nested_cv$metric == BENCH$metric, ]

  rows <- list(
    .bench_row(stamp, "nested", NA_character_, NA_integer_, across$metric, "reported",
               across$score),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, across$metric, "reported_se",
               across$se),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "reported_lower",
               sel_est$lower),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "reported_upper",
               sel_est$upper),
    # The nested cross-validation interval, its centre and the quantities it is built from.
    .bench_row(stamp, "nested", NA_character_, NA_integer_, nested$metric, "center_ncv",
               nested$center),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, nested$metric, "se_ncv", nested$se),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "lower_ncv",
               ncv_est$lower),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "upper_ncv",
               ncv_est$upper),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "bias_ncv",
               ncv_all$bias),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "mse_ncv",
               ncv_all$mse_ncv),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "se_naive_ncv",
               ncv_all$se_naive),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "mse_center_ncv",
               ncv_all$mse_center),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "se_naive_center_ncv",
               ncv_all$se_naive_center),
    # The paper's own width, the mean squared error of the plain estimate, around the same centre.
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "se_bates_ncv",
               ncv_all$se_bates),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "lower_bates",
               ncv_est$center - stats::qnorm(0.975) * ncv_all$se_bates),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "upper_bates",
               ncv_est$center + stats::qnorm(0.975) * ncv_all$se_bates),
    .bench_row(stamp, "nested", sel$final$grain, NA_integer_, BENCH$metric, "true_full",
               true_full),
    .bench_row(stamp, "nested", NA_character_, NA_integer_, BENCH$metric, "true",
               mean(true_fold)),
    .bench_row(stamp, "nested", picked, levels, BENCH$metric, "true_fold", true_fold),
    .bench_row(stamp, "nested", sel$selected$grain, sel$selected$fold, BENCH$metric, "selected",
               sel$selected$inner_score),
    .bench_row(stamp, "single_loop", single, NA_integer_, BENCH$metric, "reported",
               max(grid$score)),
    .bench_row(stamp, "single_loop", single, NA_integer_, BENCH$metric, "true",
               unname(truth[single])),
    .bench_row(stamp, "oracle", names(truth)[which.max(truth)], NA_integer_, BENCH$metric, "true",
               max(truth)),
    .bench_row(stamp, "candidate", names(truth), NA_integer_, BENCH$metric, "true",
               unname(truth)),
    .bench_row(stamp, "candidate", rep(colnames(deployed), each = length(levels)),
               rep(levels, times = ncol(deployed)), BENCH$metric, "true_fold",
               as.vector(deployed)),
    .bench_row(stamp, "candidate", grid$grain, NA_integer_, BENCH$metric, "ladder", grid$score),
    .bench_row(stamp, "candidate", sel$inner$grain, sel$inner$fold, BENCH$metric, "inner",
               sel$inner$score),
    .bench_row(stamp, "stage", names(clock), NA_integer_, NA_character_, "secs",
               unlist(clock))
  )
  do.call(rbind, rows)
}

for (r in reps) {
  target <- file.path(out_dir, sprintf("rep_%05d.csv.gz", r))
  if (file.exists(target) && !opt$force) {
    message("[", cell$cell_id, "] replicate ", r, " already on disk, skipping")
    next
  }
  started <- Sys.time()
  rows <- bench_replicate(cell, r, candidates, learner, pkg_dir)
  tmp <- paste0(target, ".partial")
  con <- gzfile(tmp, open = "wt")
  utils::write.csv(rows, con, row.names = FALSE)
  close(con)
  file.rename(tmp, target)
  message(sprintf("[%s] replicate %d written in %.1f min", cell$cell_id, r,
                  as.numeric(difftime(Sys.time(), started, units = "mins"))))
}
