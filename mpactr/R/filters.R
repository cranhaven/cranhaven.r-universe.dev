#######################
### Mismatched peaks ##
#######################

#' Mispicked ions filter
#'
#' @description
#' `filter_mispicked_ions()` identifies ions that were incorrectly split into
#' separate features during preprocessing. This filter checks the feature table
#' for similar ions in terms of mass and retention time. Peaks found to be
#' similar are merged into a single feature given `merge_peaks` is `TRUE`.
#'
#' The parameter `ringwin` is the detector saturation mass window, specific for
#' some instruments, such as Waters Synapse G2-Si-Q-ToF, to account for high
#' concentration samples.
#'
#' Parameter `isowin` is the isotopic mass window, which accounts for isotopic
#' peaks of the same precursor mass that were incorrectly assigned during
#' preprocessing.
#'
#'
#' `copy_object`: mpactr is built on an R6 class-system, meaning it operates on
#' reference semantics in which data is updated *in-place*. Compared to a
#' shallow copy, where only data pointers are copied, or a deep copy, where
#' the entire data object is copied in memory, any changes to the original
#' data object, regardless if they are assigned to a new object, result in
#' changes to the original data object. We recommend using the default
#' `copy_object = FALSE` as this makes for an extremely fast and
#' memory-efficient way to chain mpactr filters together; however, if you
#' would like to run the filters individually with traditional R style objects,
#' you can set `copy_object` to `TRUE` as shown in the filter examples.
#'
#' @param mpactr_object An `mpactr_object`. See [import_data()].
#' @param ringwin Ringing mass window or detector saturation mass window.
#' Default = 0.5 atomic mass units (AMU).
#' @param isowin Isotopic mass window. Default = 0.01 AMU.
#' @param trwin A `numeric` denoting the retention time threshold for assessing
#' if ions should be merged. Default = 0.005.
#' @param max_iso_shift A `numeric`. Default = 3.
#' @param merge_peaks A `boolean` parameter to determine if peaks found to
#' belong to the same ion should be merged in the feature table.
#' @param merge_method If merge_peaks is TRUE, a method for how similar peaks
#' should be merged. Can be one of "sum".
#' @param copy_object A `boolean` parameter that allows users to return a copied
#' object instead of modifying the object.
#' @return an `mpactr_object`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_mispicked_ions(data,
#'   ringwin = 0.5,
#'   isowin = 0.01,
#'   trwin = 0.005,
#'   max_iso_shift = 3,
#'   merge_peaks = TRUE,
#'   merge_method = "sum"
#' )
#'
filter_mispicked_ions <- function(mpactr_object,
                                  ringwin = 0.5,
                                  isowin = 0.01,
                                  trwin = 0.005,
                                  max_iso_shift = 3,
                                  merge_peaks = TRUE,
                                  merge_method = "sum",
                                  copy_object = FALSE) {

  if (isTRUE(mpactr_object$is_filter_run(filter = "mispicked"))) {

    cli::cli_abort("{.var filter_mispicked_ions()} has already been run on
                   {deparse(substitute(mpactr_object))}. Reload the data
                   and set {.var copy_object} to {.var TRUE} before
                   running the filter if you would like to run more than once.
                   See more by typing {.var ?? filter_mispicked_ions} in your
                   terminal.")
  }

  if (copy_object) {
    mpactr_object <- clone(mpactr_object)
  }
  mpactr_object$check_mismatched_peaks(
    ringwin = ringwin,
    isowin = isowin,
    trwin = trwin,
    max_iso_shift = max_iso_shift,
    merge_peaks = merge_peaks, merge_method = "sum"
  )
  return(mpactr_object)
}

#######################
### Group filter     ##
#######################
#' Filter Ions by Group
#'
#' @details
#' `filter_group()` removes feature ions that are present in a user-defined
#' group based on a relative abundance threshold. This could be particularly
#' useful to filter out features found present in solvent blank samples.
#' Further, this filter can be utilized to remove features in media blank
#' sample for experiments on microbial cultures.

#' The presence or absence of features in a group of samples is determined by
#' first averaging injection replicates and then averaging biological
#' replicates within each biological treatment group. A feature is present in
#' a group if its abundance is greater than the user-defined `group_threshold`.
#' The default is 0.01, meaning a feature is removed if its abundance is 1% of
#' that in the sample group in which it is most abundant. For example, blank
#' filtering can remove features whose mean abundance in solvent blank
#' injections is greater than 1% of their maximum mean abundance in experimental
#' samples.
#'
#' If you would like to remove features found in media blank
#' samples, we recommend testing the `group_threshold` parameter.
#'
#' `copy_object`: mpactr is built on an R6 class-system, meaning it operates on
#' reference semantics in which data is updated *in-place*. Compared to a
#' shallow copy, where only data pointers are copied, or a deep copy, where
#' the entire data object is copied in memory, any changes to the original
#' data object, regardless if they are assigned to a new object, result in
#' changes to the original data object. We recommend using the default
#' `copy_object = FALSE` as this makes for an extremely fast and
#' memory-efficient way to chain mpactr filters together; however, if you
#' would like to run the filters individually with traditional R style objects,
#' you can set `copy_object` to `TRUE` as shown in the filter examples.
#'
#'
#' @param mpactr_object An `mpactr_object`. See [import_data()].
#' @param group_threshold Relative abundance threshold at which to remove ions.
#' Default = 0.01.
#' @param group_to_remove Biological group name to remove ions from.
#' @param remove_ions A `boolean` parameter. If `TRUE` failing ions will be
#' removed from the peak table. Default = TRUE.
#' @param copy_object A `boolean` parameter that allows users to return a copied
#' object instead of modifying the object.

#' @return an `mpactr_object`.
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_group(data,
#'   group_threshold = 0.01,
#'   group_to_remove = "Blanks",
#'   remove_ions = TRUE
#' )
#'
filter_group <- function(mpactr_object,
                         group_threshold = 0.01,
                         group_to_remove,
                         remove_ions = TRUE,
                         copy_object = FALSE) {
  if (isTRUE(mpactr_object$is_filter_run(filter = "group",
                                         group = group_to_remove))) {

    cli::cli_abort("{.var filter_group()} has already been run for group
                   {group_to_remove} on {deparse(substitute(mpactr_object))}.
                   Reload the data and set {.var copy_object} to {.var TRUE}
                   before running the filter if you would like to run more
                   than once. See more by typing {.var ?? filter_group} in 
                   your terminal.")
  }

  if (copy_object) {
    mpactr_object <- clone(mpactr_object)
  }
  mpactr_object$filter_blank()
  mpactr_object$parse_ions_by_group(group_threshold = group_threshold)
  mpactr_object$apply_group_filter(
    group = group_to_remove,
    remove_ions = remove_ions
  )
  return(mpactr_object)
}

#######################
### CV filter        ##
#######################
#' Filter Non-reproducible ions
#'
#' @description
#' `filter_cv()` removes feature ions that are found to be non-reproducible
#' between technical injection replicates. Reproducibility is assessed via mean
#' or median coefficient of variation (CV) between technical replicates. As
#' such, this filter is expecting an input dataset with at least two replicate
#' injections per sample.
#'
#' `copy_object`: mpactr is built on an R6 class-system, meaning it operates on
#' reference semantics in which data is updated *in-place*. Compared to a
#' shallow copy, where only data pointers are copied, or a deep copy, where
#' the entire data object is copied in memory, any changes to the original
#' data object, regardless if they are assigned to a new object, result in
#' changes to the original data object. We recommend using the default
#' `copy_object = FALSE` as this makes for an extremely fast and
#' memory-efficient way to chain mpactr filters together; however, if you
#' would like to run the filters individually with traditional R style objects,
#' you can set `copy_object` to `TRUE` as shown in the filter examples.
#'
#' @param mpactr_object An `mpactr_object`. See [import_data()].
#' @param cv_threshold Coefficient of variation threshold.
#' A lower cv_threshold will result in more stringent filtering and higher
#' reproducibility. Recommended values between 0.2 - 0.5.
#' @param copy_object A `boolean` parameter that allows users to return a copied
#' object instead of modifying the object.
#'
#' @return an `mpactr_object`.
#' @export
#'
#' @examples
#'
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_cv(data,
#'   cv_threshold = 0.01,
#'   copy_object = TRUE
#' )
#'
filter_cv <- function(mpactr_object,
                      cv_threshold = NULL,
                      copy_object = FALSE) {

  if (isTRUE(mpactr_object$is_filter_run(filter = "replicability"))) {

    cli::cli_abort("{.var filter_cv()} has already been run on
                   {deparse(substitute(mpactr_object))}. Reload the data
                   and set {.var copy_object} to {.var TRUE} before
                   running the filter if you would like to run more than once.
                   See more by typing {.var ?? filter_cv} in your terminal.")
  }

  if (copy_object) {
    mpactr_object <- clone(mpactr_object)
  }

  mpactr_object$cv_filter(
    cv_threshold = cv_threshold
  )
  return(mpactr_object)
}

###########################
### Insource ions filter ##
###########################
#' Filter Insource ions
#'
#' @description
#' `filter_insource_ions()` identifies and removes in-source ion clusters based
#' on a Pearson correlation threshold. Groups of co-eluting features with
#' identical retention time are identified and used to generate Pearson
#' correlation matrices. Clusters with self-similarity greater than the
#' user-defined `cluster_threshold` within these matrices are identified as
#' likely belonging to a single precursor ion and is associated insource ion.
#' Highly correlated ions are identified and removed.
#'
#' `copy_object`: mpactr is built on an R6 class-system, meaning it operates on
#' reference semantics in which data is updated *in-place*. Compared to a
#' shallow copy, where only data pointers are copied, or a deep copy, where
#' the entire data object is copied in memory, any changes to the original
#' data object, regardless if they are assigned to a new object, result in
#' changes to the original data object. We recommend using the default
#' `copy_object = FALSE` as this makes for an extremely fast and
#' memory-efficient way to chain mpactr filters together; however, if you
#' would like to run the filters individually with traditional R style objects,
#' you can set `copy_object` to `TRUE` as shown in the filter examples.
#'
#' @param mpactr_object An `mpactr_object`. See [import_data()].
#' @param cluster_threshold Cluster threshold for ion deconvolution.
#' Default = 0.95.
#' @param copy_object A `boolean` parameter that allows users to return
#' a copied object instead of modifying the object.
#'
#' @return an `mpactr_object`
#' @export
#'
#' @examples
#' data <- import_data(
#'   example_path("coculture_peak_table.csv"),
#'   example_path("metadata.csv"),
#'   format = "Progenesis"
#' )
#'
#' data_filter <- filter_insource_ions(data,
#'   cluster_threshold = 0.95
#' )
#'
filter_insource_ions <- function(mpactr_object,
                                 cluster_threshold = 0.95,
                                 copy_object = FALSE) {

  if (isTRUE(mpactr_object$is_filter_run(filter = "insource"))) {

    cli::cli_abort("{.var filter_insource_ions()} has already been run on
                   {deparse(substitute(mpactr_object))}. Reload the data
                   and set {.var copy_object} to {.var TRUE} before
                   running the filter if you would like to run more than once.
                   See more by typing {.var ?? filter_insource_ions} in your
                   terminal.")
  }

  if (copy_object) {
    mpactr_object <- clone(mpactr_object)
  }
  mpactr_object$filter_insource_ions(cluster_threshold = cluster_threshold)
  return(mpactr_object)
}
