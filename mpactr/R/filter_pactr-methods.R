####  filter 1: mismatched peaks    ###
filter_pactr$set(
  "public", "check_mismatched_peaks",
  function(ringwin,
           isowin,
           trwin,
           max_iso_shift,
           merge_peaks,
           merge_method = NULL) {
    l <- nrow(self$mpactr_data$get_peak_table())
    cli::cli_alert_info("Checking {l} peaks for mispicked peaks.")
    self$mpactr_data$set_peak_table(self$mpactr_data$get_peak_table()[
      order(self$mpactr_data$get_peak_table()$mz,
        decreasing = FALSE
      ),
    ])
    ion_filter_list <- list()
    results <- FilterMispickedIons(self$mpactr_data$get_peak_table(), ringwin,
                                   isowin, trwin, max_iso_shift)

    ion_filter_list[["cut_ions"]] <- results$cut_ions
    ion_filter_list[["merge_groups"]] <- results$merge_groups
    self$logger[["check_mismatched_peaks"]] <- ion_filter_list

    if (isTRUE(merge_peaks)) {
      cli::cli_alert_info(c("Argument merge_peaks is: {merge_peaks}. ",
                            "Merging mispicked peaks with method ",
                            "{merge_method}."))
      private$merge_ions(ion_filter_list, merge_method)
    } else {
      cli::cli_alert_warning(c("Argument merge_peaks is: {merge_peaks}. ",
                               "Mispicked peaks will not be merged."))
    }

    self$logger$list_of_summaries$mispicked <- summary$new(
      filter = "mispicked", failed_ions = results$cut_ions,
      passed_ions = self$mpactr_data$get_peak_table()$Compound
    )

    self$logger$list_of_summaries$mispicked$summarize()
  }
)

filter_pactr$set("private", "merge_ions", function(ion_filter_list, method) {
  if (is.null(method)) {
    cli::cli_abort(c("No method has been supplied for merging peaks. ",
                     "{.var method} must be one of: sum"))
  }

  if (length(ion_filter_list[["cut_ions"]]) <= 0) {
    return()
  }
  if (method == "sum") {
    dat <- melt(self$mpactr_data$get_peak_table(),
      id.vars = c("Compound", "mz", "rt", "kmd"),
      variable.name = "sample",
      value.name = "intensity",
      variable.factor = FALSE
    )

    for (ion in names(ion_filter_list$merge_groups)) {
      dat <- dat[Compound %in% c(ion, ion_filter_list$merge_groups[[ion]]),
        intensity := sum(intensity),
        by = .(sample)
      ]
    }
    self$mpactr_data$set_peak_table(dcast(dat,
      Compound + mz + kmd + rt ~ sample,
      value.var = "intensity"
    )[
      (!Compound %in% ion_filter_list$cut_ions),
    ])
  }
})

####  filter 2: group filter    ###
# Calculates statisics for each feature (rsd, n)
# across biological groups and technical replicates
filter_pactr$set("public", "filter_blank", function() {
  b <- data.table::melt(self$mpactr_data$get_peak_table(),
    id.vars = c("Compound", "mz", "rt", "kmd"), variable.name =
      "sample", value.name = "intensity", variable.factor = FALSE
  )[
    data.table(self$mpactr_data$get_meta_data()),
    on = .(sample = Injection)
  ][
    , .(
      average = mean(intensity),
      BiolRSD = rsd(intensity),
      Bioln = length(intensity)
    ),
    by = .(Compound, Biological_Group)
  ]

  t <- data.table::melt(self$mpactr_data$get_peak_table(),
    id.vars = c("Compound", "mz", "rt", "kmd"),
    variable.name = "sample",
    value.name = "intensity",
    variable.factor = FALSE
  )[
    data.table(self$mpactr_data$get_meta_data()),
    on = .(sample = Injection)
  ][
    , .(sd = rsd(intensity), n = length(intensity)),
    by = .(Compound, Biological_Group, Sample_Code)
  ][
    , .(techRSD = mean(sd), techn = mean(n)),
    by = .(Compound, Biological_Group)
  ]
  group_stats <- b[t, on = .(Compound, Biological_Group)]
  setorder(group_stats, Compound, Biological_Group)
  self$logger[["group_filter-group_stats"]] <- group_stats
})

# this function determines group ions above the group threshold
# given group statistics (see filter_blank).
# The result is a list of ions by group whose relative abundance
# is greater than the threshold.
filter_pactr$set(
  "public", "parse_ions_by_group",
  function(group_threshold = 0.01) {
    group_avgs <- dcast(self$logger[["group_filter-group_stats"]],
      Compound ~ Biological_Group,
      value.var = "average"
    )

    max <- self$logger[["group_filter-group_stats"]][
      , .(Compound, Biological_Group, average)
    ][
      , .(max = max(average)),
      by = Compound
    ]

    group_max <- group_avgs[max, on = .(Compound = Compound)][
      , lapply(.SD, function(x) {
        x / max
      }),
      .SDcols = unique(
        self$logger[["group_filter-group_stats"]]$Biological_Group
      )
    ]

    biol_groups <- as.list(group_max)
    biol_groups <- lapply(biol_groups, setNames, group_avgs[, Compound])
    group_filter_list <- lapply(biol_groups, function(x) {
      names(x)[which(x > group_threshold)]
    })
    self$logger[["group_filter-failing_list"]] <- group_filter_list
  }
)


# Given a group name, removes flagged ions from the peak table.
filter_pactr$set(
  "public", "apply_group_filter",
  function(group, remove_ions = TRUE) {
    groups <- unique(self$mpactr_data$get_meta_data()$Biological_Group)
    if (isFALSE(group %in% groups)) {

      cli::cli_abort(c("{.var group} {group} is not in ",
                       "{.var Biological_Group}.",
                       "Options are: {groups}"))
    }

    l <- nrow(self$mpactr_data$get_peak_table())
    cli::cli_alert_info("Parsing {l} peaks based on the sample group: {group}.")

    if (isFALSE(remove_ions)) {
      cli::cli_alert_warning(c("Argument remove_ions is {remove_ions}. ",
                               "Peaks from {group} will not be removed."))
      return()
    }
    cli::cli_alert_info(c("Argument remove_ions is: {remove_ions}.",
                          "Removing peaks from {group}."))

    ions <- self$logger[["group_filter-failing_list"]][[group]]
    self$mpactr_data$set_peak_table(self$mpactr_data$get_peak_table()[
      !(self$mpactr_data$get_peak_table()$Compound
        %in% ions),
    ])

    self$logger$list_of_summaries[[paste0("group-", group)]] <- summary$new(
      filter = group,
      failed_ions = ions,
      passed_ions = self$mpactr_data$get_peak_table()$Compound
    )


    self$logger$list_of_summaries[[paste0("group-", group)]]$summarize()
  }
)

####  filter 3: cv filter    ###
filter_pactr$set(
  "public", "cv_filter",
  function(cv_threshold = NULL) {
    if (is.null(cv_threshold)) {
      cli::cli_abort("{.var cv_threshold} must be supplied.")
    }

    ## abort if there are no technical replicates.
    if (isFALSE(self$mpactr_data$isMultipleTechReps())) {
      cli_abort(c("There are no technical replicates in the dataset provided. ",
                  "In order to run the replicability filter, technical ",
                  "replicates are required."))
    }

    input_ions <- self$mpactr_data$get_peak_table()$Compound
    cli <- cli::cli_alert_info
    n <- length(input_ions)
    cli("Parsing {n} peaks for replicability across technical replicates.")

    cv <- data.table::melt(self$mpactr_data$get_peak_table(),
      id.vars = c("Compound", "mz", "rt", "kmd"), variable.name =
        "sample", value.name = "intensity", variable.factor = FALSE
    )[
      self$mpactr_data$get_meta_data(),
      on = .(sample = Injection)
    ][order(Compound)]


    peak_table <- self$mpactr_data$get_peak_table()
    meta_data <- self$mpactr_data$get_meta_data()

    cv <-
      as.data.table(FilterCV(cv, unique(meta_data$Sample_Code), cv_threshold,
                             table(meta_data$Sample_Code)[[1]]))

    samples <- unique(meta_data$Sample_Code)
    for (i in seq_along(samples)) {
      peak_table[Compound %in% cv[Sample_Code == samples[[i]] &
                                    !PassesCvFilter]$Compound,
                 meta_data$Injection[which(meta_data$Sample_Code
                                           == samples[[i]])] := 0]
    }

    failed_indexes <- which(rowSums(peak_table[, meta_data$Injection,
                                               with = FALSE]) == 0)


    self$logger[["cv_values"]] <- cv
    failed_ions <- peak_table$Compound[failed_indexes]
    self$mpactr_data$set_peak_table(self$mpactr_data$get_peak_table()[
      Compound %in% setdiff(
        input_ions,
        failed_ions
      ),
    ])

    self$logger$list_of_summaries$replicability <- summary$new(
      filter = "cv_filter",
      failed_ions = failed_ions,
      passed_ions = self$mpactr_data$get_peak_table()$Compound
    )

    self$logger$list_of_summaries$replicability$summarize()
  }
)
####  filter 4: insource ions   ###

filter_pactr$set(
  "public", "filter_insource_ions",
  function(cluster_threshold = 0.95) {
    input_ions <- self$mpactr_data$get_peak_table()$Compound
    cli::cli_alert_info("Parsing {length(input_ions)} peaks for insource ions.")

    self$mpactr_data$set_peak_table(self$mpactr_data$get_peak_table()[
      , cor := private$deconvolute_correlation(.SD, cluster_threshold),
      by = .(rt)
    ][cor == TRUE, ])

    self$logger$list_of_summaries$insource <- summary$new(
      filter = "insource",
      failed_ions = setdiff(
        input_ions,
        self$mpactr_data$get_peak_table()$Compound
      ),
      passed_ions = self$mpactr_data$get_peak_table()$Compound
    )

    self$logger$list_of_summaries$insource$summarize()
  }
)

filter_pactr$set(
  "private", "deconvolute_correlation",
  function(group_1, cluster_threshold = 0.95) {
    if (nrow(group_1) <= 1) {
      return(TRUE)
    }

    dat <- melt(group_1,
      id.vars = c("Compound", "mz", "kmd"),
      variable.name = "sample",
      value.name = "intensity",
      variable.factor = FALSE
    )[
      , c("Compound", "sample", "intensity")
    ]
    data <- dcast(dat, sample ~ Compound, value.var = "intensity")
    data[, c("sample") := NULL]
    corr <- stats::cor(data, method = c("pearson"))
    dist <- stats::dist(corr, method = "euclidian")
    cluster <- stats::hclust(dist, method = "complete")
    cut_tree <- stats::cutree(cluster, h = 1 - cluster_threshold)

    x <- as.data.table(cut_tree, keep.rownames = "Compound")[
      group_1,
      on = .(Compound = Compound)
    ][
      , keep := private$cluster_max(mz),
      by = .(cut_tree)
    ]

    return(x$keep)
  }
)

# Given a list of mz values, this function will determine
# which value has the largest mz and modify its "KEEP" Column.
filter_pactr$set("private", "cluster_max", function(mz) {
  keep <- rep(FALSE, length(mz))
  keep[which.max(mz)] <- TRUE
  return(keep)
})
