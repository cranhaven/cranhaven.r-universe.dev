#!/usr/bin/env Rscript
# The threshold arm of the benchmark. A skill statistic read as a maximum over cuts takes the cut
# that suits the very units it scores, which lifts it above the skill the same fit shows on units
# chosen independently. This asks whether a cut learned on the other outer folds removes that lift,
# with a deployment sample large enough to say what the skill was.
#
# It replays a run rather than repeating one. The grain each outer fold selected is on disk, so
# refitting at that grain reproduces the fit the procedure made without searching the candidates
# again, which is where almost all of a replicate's time goes. The replayed area under the curve
# has to reproduce the stored one fold by fold before any threshold is read off it; a replicate
# whose replay disagrees is written nowhere.
#
# Usage:
#   Rscript inst/benchmark/b4.R --cell=elasticnet-event-n300 --results=<dir run.R wrote> \
#     --out=<dir> [--reps=1:200] [--tol=1e-08] [--force]

suppressWarnings(suppressMessages({
  library(timesift)
}))

here <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(here)) dirname(normalizePath(sub("^--file=", "", here[1L]))) else getwd()
source(file.path(here, "design.R"))

opt <- bench_args(list(cell = NA_character_, reps = NA_character_,
                       results = BENCH$results, out = BENCH$results_b4,
                       tol = "1e-08", force = FALSE, pkg = NA_character_))
pkg_dir <- if (is.na(opt$pkg)) dirname(dirname(here)) else opt$pkg
tol <- as.numeric(opt$tol)

src_dir <- file.path(opt$results, opt$cell)
if (is.na(opt$cell) || !dir.exists(src_dir)) {
  stop("--cell must name a directory under --results, got ", src_dir, call. = FALSE)
}
out_dir <- file.path(opt$out, opt$cell)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# The cut that separates predicted presence from predicted absence, and the statistic read at a cut
# given from outside. Both sit on .sweep(), which is the one place in the package a score becomes a
# set of cuts, so a cut chosen here and a maximum taken by tss() are chosen among the same ones.
.b4_best_cut <- function(y, p) {
  s <- timesift:::.sweep(y, p)
  if (is.null(s)) {
    return(NA_real_)
  }
  s$thr[which.max(s$tp / s$n_pos - s$fp / s$n_neg)]
}

.b4_tss_at <- function(y, p, cut) {
  y <- as.integer(y)
  n_pos <- sum(y == 1L)
  if (n_pos == 0L || n_pos == length(y) || !is.finite(cut)) {
    return(NA_real_)
  }
  pred <- p >= cut
  mean(pred[y == 1L]) - mean(pred[y == 0L])
}

# The stamp the stored replicate declares, read back as the list every design function takes. The
# run that wrote it is the run being replayed, so its identity, not this session's, is what the
# replayed rows are keyed by.
.b4_stamp <- function(d) {
  keep <- c("scale", "cell_id", "block", "mechanism", "n_unit", "inner", "outer", "ncv_repeats",
            "replicate",
            "design_seed", "draw", "deploy_draw", "true_grain", "n_candidate", "candidate_digest",
            "learner_digest", "pkg_version", "pkg_commit", "pkg_dirty", "r_version", "platform",
            "device")
  one <- unique(d[keep])
  if (nrow(one) != 1L) {
    stop("the replicate file carries ", nrow(one), " stamps; it is not one replicate.",
         call. = FALSE)
  }
  stamp <- as.list(one)
  stamp$metric <- unique(d$sel_metric)
  stamp
}

# Which build of the package the refits came from. Two libraries can declare the same version, so
# the path a package was loaded from and the date it was built are what tell them apart, and a
# replay whose numbers disagree with the run is most often reading a different one.
.b4_build <- function() {
  lib <- tryCatch(dirname(find.package("timesift")), error = function(e) NA_character_)
  built <- tryCatch(utils::packageDescription("timesift")$Built, error = function(e) NA_character_)
  parts <- if (is.na(built)) character() else strsplit(built, "; ", fixed = TRUE)[[1L]]
  paste0("timesift ", utils::packageVersion("timesift"), " from ", lib,
         if (length(parts) >= 3L) paste0(", built ", parts[3L]) else "")
}

.b4_replay <- function(path) {
  d <- utils::read.csv(gzfile(path), stringsAsFactors = FALSE)
  stamp <- .b4_stamp(d)
  bench_scale(stamp$scale)
  if (!identical(as.integer(stamp$outer), as.integer(BENCH$outer))) {
    stop("replicate ", stamp$replicate, " ran with outer=", stamp$outer, ", the design now says ",
         BENCH$outer, ".", call. = FALSE)
  }
  cell <- list(cell_id = stamp$cell_id, block = stamp$block, mechanism = stamp$mechanism,
               n_unit = stamp$n_unit, inner = stamp$inner)
  candidates <- bench_candidates()
  declared <- .bench_digest(paste(candidates$candidate, collapse = ","))
  if (!identical(declared, stamp$candidate_digest)) {
    stop("replicate ", stamp$replicate, " searched candidate set ", stamp$candidate_digest,
         "; the design now builds ", declared, ".", call. = FALSE)
  }
  learner <- bench_learner(cell$block)

  # The fold each grain was selected for, as the run recorded it. Refitting there is the whole of
  # the replay: no candidate is scored again, and nothing is chosen here.
  sel <- d[d$arm == "nested" & d$quantity == "selected", c("candidate", "outer_fold")]
  stored <- d[d$arm == "nested" & d$quantity == "true_fold" & d$metric == BENCH$metric,
              c("outer_fold", "value")]
  sel <- sel[order(sel$outer_fold), ]
  stored <- stored[order(stored$outer_fold), ]
  if (nrow(sel) != BENCH$outer || !identical(sel$outer_fold, stored$outer_fold)) {
    stop("replicate ", stamp$replicate, " stores ", nrow(sel), " selections and ", nrow(stored),
         " fold truths for ", BENCH$outer, " outer folds.", call. = FALSE)
  }

  clock <- list()
  tick <- function(name, expr) {
    t0 <- Sys.time()
    out <- force(expr)
    clock[[name]] <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    out
  }

  sim <- tick("simulate", .bench_simulate(cell, cell$n_unit, stamp$design_seed, stamp$replicate))
  set <- tick("represent", bench_representation(sim$readings, candidates))
  dep <- tick("deployment", .bench_deployment(cell, stamp, candidates))
  f <- fold_map(sim$y, v = BENCH$outer, seed = stamp$replicate)
  levels <- sort(unique(f))
  if (!identical(as.integer(sel$outer_fold), as.integer(levels))) {
    stop("replicate ", stamp$replicate, " stores folds ", paste(sel$outer_fold, collapse = ","),
         "; the fold map now has ", paste(levels, collapse = ","), ".", call. = FALSE)
  }

  units <- names(f)
  variables <- colnames(sim$y)
  p <- matrix(NA_real_, nrow = length(units), ncol = length(variables),
              dimnames = list(units, variables))
  auc_replayed <- numeric(BENCH$outer)
  dep_p <- vector("list", BENCH$outer)

  tick("procedure", for (i in seq_along(levels)) {
    cc <- sel$candidate[i]
    idx <- which(f != levels[i])
    fit <- fit_learner(learner, timesift:::.subset_units(set[[cc]], idx),
                       sim$y[units[idx], , drop = FALSE])
    ph <- stats::predict(fit, timesift:::.subset_units(set[[cc]], which(f == levels[i])))
    p[rownames(ph), ] <- ph[, variables]
    dep_p[[i]] <- stats::predict(fit, dep$set[[cc]])
    auc_replayed[i] <- mean(vapply(variables, function(v)
      roc_auc(dep$y[rownames(dep_p[[i]]), v], dep_p[[i]][, v]), numeric(1L)))
  })

  # The replay is the recorded run or it is nothing. Everything read off these fits downstream
  # rests on this line and on no other check.
  gap <- abs(auc_replayed - stored$value)
  if (any(!is.finite(gap)) || max(gap) > tol) {
    stop("replicate ", stamp$replicate, " did not replay: the stored fold areas are ",
         paste(format(stored$value, digits = 15), collapse = ", "), " and the refits give ",
         paste(format(auc_replayed, digits = 15), collapse = ", "), " (largest gap ",
         format(max(gap), digits = 3), " > ", tol, "). The run was fitted at ",
         substr(as.character(stamp$pkg_commit), 1L, 12L), " and these refits come from ",
         .b4_build(), ".", call. = FALSE)
  }

  # Every fold's units carry a held-out probability now, so a cut can be learned on the folds a
  # unit is not in, the way it is read on the demonstration. The four readings of one fold are
  # averaged over the variables all four are defined for, so the comparison between them is not
  # also a comparison between two sets of variables. A variable a fold cannot score is counted
  # rather than dropped quietly.
  quantities <- c("reported_fold", "xfold_fold", "true_fold", "atcut_fold")
  scored <- vapply(seq_along(levels), function(i) {
    test <- units[f == levels[i]]
    rest <- units[f != levels[i]]
    pd <- dep_p[[i]]
    dy <- dep$y[rownames(pd), , drop = FALSE]
    per_variable <- vapply(variables, function(v) {
      cut <- .b4_best_cut(sim$y[rest, v], p[rest, v])
      c(tss(sim$y[test, v], p[test, v]),
        .b4_tss_at(sim$y[test, v], p[test, v], cut),
        tss(dy[, v], pd[, v]),
        .b4_tss_at(dy[, v], pd[, v], cut))
    }, numeric(4L))
    ok <- !apply(is.na(per_variable), 2L, any)
    c(rowMeans(per_variable[, ok, drop = FALSE]), n_variable = sum(ok))
  }, numeric(5L))
  rownames(scored) <- c(quantities, "n_variable")

  fold <- as.integer(levels)
  rows <- rbind(
    do.call(rbind, lapply(quantities, function(q)
      .bench_row(stamp, "nested", sel$candidate, fold, "tss", q, scored[q, ]))),
    do.call(rbind, lapply(quantities, function(q)
      .bench_row(stamp, "nested", NA_character_, NA_integer_, "tss",
                 sub("_fold$", "", q), mean(scored[q, ])))),
    .bench_row(stamp, "nested", sel$candidate, fold, "tss", "n_variable",
               scored["n_variable", ]),
    .bench_row(stamp, "nested", sel$candidate, fold, "roc_auc", "true_fold", auc_replayed),
    .bench_row(stamp, "nested", sel$candidate, fold, "roc_auc", "replay_gap",
               auc_replayed - stored$value),
    .bench_row(stamp, "stage", names(clock), NA_integer_, NA_character_, "secs", unlist(clock)))

  commit <- tryCatch(
    system2("git", c("-C", shQuote(pkg_dir), "rev-parse", "HEAD"), stdout = TRUE, stderr = NULL),
    error = function(e) NA_character_)
  rows$replay_commit <- if (length(commit) == 1L) commit else NA_character_
  rows$replay_version <- as.character(utils::packageVersion("timesift"))
  rows$replay_r_version <- paste(R.version$major, R.version$minor, sep = ".")
  rows
}

files <- list.files(src_dir, pattern = "^rep_[0-9]+[.]csv[.]gz$")
stored_reps <- as.integer(sub("^rep_0*([0-9]+)[.]csv[.]gz$", "\\1", files))
reps <- if (is.na(opt$reps)) sort(stored_reps) else intersect(eval(parse(text = opt$reps)),
                                                             stored_reps)
if (!length(reps)) {
  stop("no stored replicates to replay in ", src_dir, ".", call. = FALSE)
}
message(sprintf("[%s] replaying %d of %d stored replicates", opt$cell, length(reps),
                length(stored_reps)))

for (r in reps) {
  target <- file.path(out_dir, sprintf("rep_%05d.csv.gz", r))
  if (file.exists(target) && !opt$force) {
    message("[", opt$cell, "] replicate ", r, " already on disk, skipping")
    next
  }
  started <- Sys.time()
  rows <- .b4_replay(file.path(src_dir, sprintf("rep_%05d.csv.gz", r)))
  tmp <- paste0(target, ".partial")
  con <- gzfile(tmp, open = "wt")
  utils::write.csv(rows, con, row.names = FALSE)
  close(con)
  file.rename(tmp, target)
  message(sprintf("[%s] replicate %d replayed in %.1f min", opt$cell, r,
                  as.numeric(difftime(Sys.time(), started, units = "mins"))))
}
