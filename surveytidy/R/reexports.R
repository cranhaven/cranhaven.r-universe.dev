# R/reexports.R
#
# Re-exports of dplyr and tidyr generics so that library(surveytidy) is
# sufficient — users do not need to separately load dplyr or tidyr to use
# these verbs on survey design objects.
#
# surveytidy registers S3 methods for all of these generics in .onLoad()
# (see R/zzz.R). Re-exporting the generics here makes them available on the
# search path when surveytidy is loaded.
#
# Primary verbs (those with @name NULL stubs in their source files) use
# @rdname to merge into the per-verb Rd file and avoid a duplicate alias in
# reexports.Rd. Secondary verbs (slice_*, ungroup, group_vars, rename_with,
# filter_out) go into reexports.Rd so they don't bring undocumented
# arguments into the per-verb Rd files.
#
# Internal dplyr machinery (dplyr_reconstruct, tidyselect::eval_select) is
# NOT re-exported — those are imported in R/surveytidy-package.R.

# ── dplyr verbs ───────────────────────────────────────────────────────────────

#' @rdname distinct
#' @export
dplyr::distinct

#' @rdname filter
#' @export
dplyr::filter

#' @export
dplyr::filter_out

#' @rdname select
#' @export
dplyr::select

#' @rdname mutate
#' @export
dplyr::mutate

#' @rdname rename
#' @export
dplyr::rename

#' @export
dplyr::rename_with

#' @rdname relocate
#' @export
dplyr::relocate

#' @rdname arrange
#' @export
dplyr::arrange

#' @rdname group_by
#' @export
dplyr::group_by

#' @export
dplyr::ungroup

#' @rdname rowwise
#' @export
dplyr::rowwise

#' @export
dplyr::group_vars

#' @rdname pull
#' @export
dplyr::pull

#' @rdname glimpse
#' @export
dplyr::glimpse

#' @rdname slice
#' @export
dplyr::slice

#' @export
dplyr::slice_head

#' @export
dplyr::slice_tail

#' @export
dplyr::slice_min

#' @export
dplyr::slice_max

#' @export
dplyr::slice_sample

# ── join verbs ────────────────────────────────────────────────────────────────

#' @rdname left_join
#' @export
dplyr::left_join

#' @rdname semi_join
#' @export
dplyr::semi_join

#' @rdname semi_join
#' @export
dplyr::anti_join

#' @rdname inner_join
#' @export
dplyr::inner_join

#' @rdname right_join
#' @export
dplyr::right_join

#' @rdname right_join
#' @export
dplyr::full_join

# ── tidyr verbs ───────────────────────────────────────────────────────────────

#' @rdname drop_na
#' @export
tidyr::drop_na

# ── surveycore: collection construction and setters ───────────────────────────
#
# Re-exported so library(surveytidy) alone is sufficient to construct and
# modify a survey_collection. surveycore's implementations remain the source
# of truth for property validation; no thin wrappers here. (Spec §VI.)

#' @export
surveycore::as_survey_collection

#' @export
surveycore::set_collection_id

#' @export
surveycore::set_collection_if_missing_var

#' @export
surveycore::add_survey

#' @export
surveycore::remove_survey
