#!/usr/bin/env Rscript
# Reads the replay files b4.R left on disk and answers the threshold question, one row per cell.
# Four readings of the same fits are differenced replicate by replicate, never as two means, so
# each difference carries the Monte Carlo margin the replicate count buys.
#
# Usage:
#   Rscript inst/benchmark/summarise_b4.R --out=<replay dir> [--csv=<file>]

here <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(here)) dirname(normalizePath(sub("^--file=", "", here[1L]))) else getwd()
source(file.path(here, "design.R"))

opt <- bench_args(list(out = BENCH$results_b4, csv = NA_character_))

files <- list.files(opt$out, pattern = "^rep_[0-9]+[.]csv[.]gz$", recursive = TRUE,
                    full.names = TRUE)
if (!length(files)) {
  stop("no replay files under ", opt$out, ".", call. = FALSE)
}
rows <- do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))

mixed <- unique(rows[c("cell_id", "scale", "candidate_digest", "pkg_commit", "replay_commit")])
clash <- names(which(table(mixed$cell_id) > 1L))
if (length(clash)) {
  stop("these cells hold rows from more than one run or one replay: ",
       paste(clash, collapse = ", "), ". Replay them under one.", call. = FALSE)
}

# The replay reproduced the run it replayed or the rows mean nothing, so the check is read before
# any threshold is, and it is read here as well as in the run that wrote it.
gap <- rows[rows$quantity == "replay_gap", ]
if (!nrow(gap) || any(!is.finite(gap$value))) {
  stop("the replay carries no finite identity check.", call. = FALSE)
}

per_cell <- lapply(split(rows, rows$cell_id), function(d) {
  reported <- bench_by_replicate(d, "nested", "reported", "tss")
  xfold <- bench_by_replicate(d, "nested", "xfold", "tss")
  true <- bench_by_replicate(d, "nested", "true", "tss")
  atcut <- bench_by_replicate(d, "nested", "atcut", "tss")

  # What the maximum over cuts takes, and what is left once the cut comes from elsewhere. The
  # away-cut is read against both targets: the skill the fits can reach, and the skill that cut
  # actually delivers on units it did not choose itself.
  inflation <- reported - true
  residual <- xfold - true
  carried <- xfold - atcut
  n_variable <- bench_pick(d, "nested", "n_variable", "tss")$value

  data.frame(
    cell_id = d$cell_id[1L], block = d$block[1L], mechanism = d$mechanism[1L],
    n_unit = d$n_unit[1L], replicates = length(reported), true_grain = d$true_grain[1L],
    reported = mean(reported), true = mean(true), xfold = mean(xfold), atcut = mean(atcut),
    inflation = mean(inflation), inflation_mc = bench_margin(inflation),
    residual = mean(residual), residual_mc = bench_margin(residual),
    carried = mean(carried), carried_mc = bench_margin(carried),
    variables_scored = min(n_variable),
    replay_gap = max(abs(d$value[d$quantity == "replay_gap"])),
    secs = mean(tapply(bench_pick(d, "stage", "secs", NA)$value,
                       bench_pick(d, "stage", "secs", NA)$replicate, sum)),
    stringsAsFactors = FALSE)
})
per_cell <- do.call(rbind, per_cell)
rownames(per_cell) <- NULL

cat("\n== identity check against the stored run\n")
cat("replicates replayed:", length(unique(paste(rows$cell_id, rows$replicate))),
    " largest area-under-curve gap:", format(max(abs(gap$value)), digits = 3), "\n")

cat("\n== per cell, max-TSS\n")
print(per_cell[c("cell_id", "replicates", "reported", "true", "inflation", "inflation_mc")],
      digits = 3)

cat("\n== per cell, the cut learned on the other folds\n")
print(per_cell[c("cell_id", "xfold", "atcut", "residual", "residual_mc", "carried", "carried_mc")],
      digits = 3)

if (!is.na(opt$csv)) {
  utils::write.csv(per_cell, opt$csv, row.names = FALSE)
  cat("\nwritten to", opt$csv, "\n")
}
