#!/usr/bin/env Rscript
# Reads the replicate files a run left on disk and answers the four questions the benchmark exists
# for, one row per cell, each with the Monte Carlo margin the replicate count buys. Selection rule,
# estimator and metric are read off the same stored rows, so a second reading costs nothing and no
# fit is repeated.
#
# Usage:
#   Rscript inst/benchmark/summarise.R --out=<results dir> [--csv=<file>]

here <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(here)) dirname(normalizePath(sub("^--file=", "", here[1L]))) else getwd()
source(file.path(here, "design.R"))

opt <- bench_args(list(out = BENCH$results, csv = NA_character_))

files <- list.files(opt$out, pattern = "^rep_[0-9]+[.]csv[.]gz$", recursive = TRUE,
                    full.names = TRUE)
if (!length(files)) {
  stop("no replicate files under ", opt$out, ".", call. = FALSE)
}
rows <- do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))

# A cell that mixed two candidate sets, two package builds, two scales or two counts of nested
# cross-validation repetitions is not one cell, and the rows cannot be pooled. Say so rather than
# averaging over it.
mixed <- unique(rows[c("cell_id", "scale", "candidate_digest", "learner_digest", "pkg_commit",
                       "ncv_repeats")])
clash <- names(which(table(mixed$cell_id) > 1L))
if (length(clash)) {
  stop("these cells hold rows from more than one design, build or repetition count: ",
       paste(clash, collapse = ", "), ". Rerun them under one.", call. = FALSE)
}

per_cell <- lapply(split(rows, rows$cell_id), function(d) {
  reps <- sort(unique(d$replicate))
  true_grain <- d$true_grain[1L]
  adequate <- if (is.na(true_grain)) character() else BENCH$nesting[[true_grain]]

  chosen <- bench_pick(d, "nested", "selected")
  by_rep <- vapply(reps, function(r) {
    pick <- chosen$candidate[chosen$replicate == r]
    grain <- sub("[.].*$", "", pick)
    c(exact = mean(pick == paste0(true_grain, ".mean")),
      grain = mean(grain == true_grain),
      adequate = mean(grain %in% adequate))
  }, numeric(3L))

  nested_rep <- bench_by_replicate(d, "nested", "reported")
  nested_true <- bench_by_replicate(d, "nested", "true")
  single_rep <- bench_by_replicate(d, "single_loop", "reported")

  # Every quantity below pairs two arms of the cell, and each pairing is made on the replicate the
  # two rows were written under rather than on the order they came back in.
  covered <- bench_align(
    bench_paired(d, c("nested", "reported_lower"), c("nested", "true"), `<=`),
    bench_paired(d, c("nested", "reported_upper"), c("nested", "true"), `>=`),
    `&`, paste(d$cell_id[1L], c("the lower interval", "the upper interval")))
  cov <- bench_wilson(covered)
  # The nested cross-validation interval is for the risk of the procedure fitted on every unit, so
  # it is read against that truth rather than against the mean over the outer training sets.
  covered_ncv <- bench_align(
    bench_paired(d, c("nested", "lower_ncv"), c("nested", "true_full"), `<=`),
    bench_paired(d, c("nested", "upper_ncv"), c("nested", "true_full"), `>=`),
    `&`, paste(d$cell_id[1L], c("the lower nested interval", "the upper nested interval")))
  cov_ncv <- bench_wilson(covered_ncv)
  # The paper's width around the same centre, read against the same truth.
  covered_bates <- bench_align(
    bench_paired(d, c("nested", "lower_bates"), c("nested", "true_full"), `<=`),
    bench_paired(d, c("nested", "upper_bates"), c("nested", "true_full"), `>=`),
    `&`, paste(d$cell_id[1L], c("the lower Bates interval", "the upper Bates interval")))
  cov_bates <- bench_wilson(covered_bates)
  # How wide the interval is against how wide it had to be: the spread of the error it has to
  # cover, over the standard error it was built from. Above one is too narrow.
  err_ncv <- bench_paired(d, c("nested", "center_ncv"), c("nested", "true_full"))
  ratio <- function(err, se) stats::sd(err) / mean(se)
  nested_bias <- bench_paired(d, c("nested", "reported"), c("nested", "true"))
  single_bias <- bench_paired(d, c("single_loop", "reported"), c("single_loop", "true"))
  regret <- bench_paired(d, c("oracle", "true"), c("nested", "true"))
  optimism <- bench_align(single_bias, nested_bias, `-`,
                          paste(d$cell_id[1L], c("the single-loop bias", "the nested bias")))

  data.frame(
    cell_id = d$cell_id[1L], scale = d$scale[1L], block = d$block[1L],
    mechanism = d$mechanism[1L], n_unit = d$n_unit[1L], ncv_repeats = d$ncv_repeats[1L],
    replicates = length(reps),
    true_grain = true_grain,
    select_exact = mean(by_rep["exact", ]), select_exact_mc = bench_margin(by_rep["exact", ]),
    select_grain = mean(by_rep["grain", ]), select_grain_mc = bench_margin(by_rep["grain", ]),
    select_adequate = mean(by_rep["adequate", ]),
    select_adequate_mc = bench_margin(by_rep["adequate", ]),
    nested_reported = mean(nested_rep), nested_true = mean(nested_true),
    nested_bias = mean(nested_bias), nested_bias_mc = bench_margin(nested_bias),
    single_reported = mean(single_rep), single_bias = mean(single_bias),
    single_bias_mc = bench_margin(single_bias),
    optimism_gap = mean(optimism), optimism_gap_mc = bench_margin(optimism),
    coverage = cov[["p"]], coverage_lo = cov[["lower"]], coverage_hi = cov[["upper"]],
    ratio = ratio(nested_bias, bench_by_replicate(d, "nested", "reported_se")),
    true_full = mean(bench_by_replicate(d, "nested", "true_full")),
    center_ncv = mean(bench_by_replicate(d, "nested", "center_ncv")),
    coverage_ncv = cov_ncv[["p"]], coverage_ncv_lo = cov_ncv[["lower"]],
    coverage_ncv_hi = cov_ncv[["upper"]],
    ratio_ncv = ratio(err_ncv, bench_by_replicate(d, "nested", "se_ncv")),
    coverage_bates = cov_bates[["p"]], coverage_bates_lo = cov_bates[["lower"]],
    coverage_bates_hi = cov_bates[["upper"]],
    ratio_bates = ratio(err_ncv, bench_by_replicate(d, "nested", "se_bates_ncv")),
    bias_ncv = mean(err_ncv), bias_ncv_mc = bench_margin(err_ncv),
    regret = mean(regret), regret_mc = bench_margin(regret),
    secs = mean(tapply(bench_pick(d, "stage", "secs", NA)$value,
                       bench_pick(d, "stage", "secs", NA)$replicate, sum)),
    stringsAsFactors = FALSE)
})
per_cell <- do.call(rbind, per_cell)
rownames(per_cell) <- NULL

stage <- bench_pick(rows, "stage", "secs", NA)
cat("\n== stage seconds per replicate, mean\n")
print(round(tapply(stage$value, list(stage$candidate, stage$cell_id), mean), 1))

cat("\n== where the selection landed\n")
chosen <- bench_pick(rows, "nested", "selected")
print(table(candidate = chosen$candidate, cell = chosen$cell_id))

cat("\n== per cell\n")
print(per_cell[c("cell_id", "replicates", "true_grain", "select_exact", "select_adequate",
                 "nested_reported", "nested_true", "nested_bias", "single_reported",
                 "optimism_gap", "coverage", "regret", "secs")], digits = 3)

cat("\n== what each interval covers, with its Wilson interval and the spread of the error it\n",
    "   has to cover over the standard error it was built from\n", sep = "")
print(per_cell[c("cell_id", "ncv_repeats", "replicates", "coverage", "coverage_lo", "coverage_hi",
                 "ratio",
                 "coverage_ncv", "coverage_ncv_lo", "coverage_ncv_hi", "ratio_ncv",
                 "coverage_bates", "coverage_bates_lo", "coverage_bates_hi", "ratio_bates",
                 "true_full", "center_ncv", "bias_ncv", "bias_ncv_mc")], digits = 3)

if (!is.na(opt$csv)) {
  utils::write.csv(per_cell, opt$csv, row.names = FALSE)
  cat("\nwritten to", opt$csv, "\n")
}
