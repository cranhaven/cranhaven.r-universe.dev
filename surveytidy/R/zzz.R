# R/zzz.R
#
# Package startup hook.
#
# S3 dispatch does NOT work for S7 objects using plain method names because
# S7 uses namespaced class names ("surveycore::survey_base"). S3 dispatch
# looks for "filter.surveycore::survey_base" which is not a valid function
# name and can never be found.
#
# Solution: use registerS3method() in .onLoad() with the exact namespaced
# class string as the `class` argument. This is the mechanism by which
# surveytidy dplyr verbs are wired to survey design objects.
#
# Reference: plans/phase-0.5-formal-specification.md — Section 2.7

.onLoad <- function(libname, pkgname) {
  # Register S7 methods for S7-aware generics (print, summary, format, etc.)
  S7::methods_register()

  # Register dplyr verb S3 methods for S7 survey classes.
  # "surveycore::survey_base" matches all subclasses (taylor, replicate,
  # twophase) via S3's class hierarchy walk.
  ns <- asNamespace(pkgname)

  # ── feature/distinct ──────────────────────────────────────────────────────

  registerS3method(
    "distinct",
    "surveycore::survey_base",
    get("distinct.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/filter ────────────────────────────────────────────────────────

  registerS3method(
    "filter",
    "surveycore::survey_base",
    get("filter.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "filter_out",
    "surveycore::survey_base",
    get("filter_out.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "dplyr_reconstruct",
    "surveycore::survey_base",
    get("dplyr_reconstruct.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "subset",
    "surveycore::survey_base",
    get("subset.survey_base", envir = ns),
    envir = baseenv()
  )

  # ── feature/select ────────────────────────────────────────────────────────

  registerS3method(
    "select",
    "surveycore::survey_base",
    get("select.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "relocate",
    "surveycore::survey_base",
    get("relocate.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "pull",
    "surveycore::survey_base",
    get("pull.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "glimpse",
    "surveycore::survey_base",
    get("glimpse.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/mutate ────────────────────────────────────────────────────────

  registerS3method(
    "mutate",
    "surveycore::survey_base",
    get("mutate.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/rename ────────────────────────────────────────────────────────

  registerS3method(
    "rename",
    "surveycore::survey_base",
    get("rename.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "rename_with",
    "surveycore::survey_base",
    get("rename_with.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/arrange ───────────────────────────────────────────────────────

  registerS3method(
    "arrange",
    "surveycore::survey_base",
    get("arrange.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice",
    "surveycore::survey_base",
    get("slice.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_head",
    "surveycore::survey_base",
    get("slice_head.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_tail",
    "surveycore::survey_base",
    get("slice_tail.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_min",
    "surveycore::survey_base",
    get("slice_min.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_max",
    "surveycore::survey_base",
    get("slice_max.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_sample",
    "surveycore::survey_base",
    get("slice_sample.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/group-by ──────────────────────────────────────────────────────

  registerS3method(
    "group_by",
    "surveycore::survey_base",
    get("group_by.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "ungroup",
    "surveycore::survey_base",
    get("ungroup.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/rowwise ───────────────────────────────────────────────────────

  registerS3method(
    "rowwise",
    "surveycore::survey_base",
    get("rowwise.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "group_vars",
    "surveycore::survey_base",
    get("group_vars.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── feature/drop-na ───────────────────────────────────────────────────────

  registerS3method(
    "drop_na",
    "surveycore::survey_base",
    get("drop_na.survey_base", envir = ns),
    envir = asNamespace("tidyr")
  )

  # ── feature/joins ─────────────────────────────────────────────────────────

  registerS3method(
    "left_join",
    "surveycore::survey_base",
    get("left_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "semi_join",
    "surveycore::survey_base",
    get("semi_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "anti_join",
    "surveycore::survey_base",
    get("anti_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "inner_join",
    "surveycore::survey_base",
    get("inner_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "right_join",
    "surveycore::survey_base",
    get("right_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "full_join",
    "surveycore::survey_base",
    get("full_join.survey_base", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_result verbs (PR 1 — passthrough) ──────────────────────────────

  registerS3method(
    "filter",
    "survey_result",
    get("filter.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "arrange",
    "survey_result",
    get("arrange.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "mutate",
    "survey_result",
    get("mutate.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice",
    "survey_result",
    get("slice.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice_head",
    "survey_result",
    get("slice_head.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice_tail",
    "survey_result",
    get("slice_tail.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice_min",
    "survey_result",
    get("slice_min.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice_max",
    "survey_result",
    get("slice_max.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "slice_sample",
    "survey_result",
    get("slice_sample.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "drop_na",
    "survey_result",
    get("drop_na.survey_result", envir = ns),
    envir = asNamespace("tidyr")
  )

  # ── survey_result verbs (PR 2 — meta-updating) ───────────────────────────

  registerS3method(
    "select",
    "survey_result",
    get("select.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "rename",
    "survey_result",
    get("rename.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )
  registerS3method(
    "rename_with",
    "survey_result",
    get("rename_with.survey_result", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_collection: data-mask verbs (PR 2a) ──

  registerS3method(
    "filter",
    "surveycore::survey_collection",
    get("filter.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "filter_out",
    "surveycore::survey_collection",
    get("filter_out.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "mutate",
    "surveycore::survey_collection",
    get("mutate.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "arrange",
    "surveycore::survey_collection",
    get("arrange.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_collection: tidyselect verbs (PR 2b) ──

  registerS3method(
    "select",
    "surveycore::survey_collection",
    get("select.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "relocate",
    "surveycore::survey_collection",
    get("relocate.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "rename",
    "surveycore::survey_collection",
    get("rename.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "rename_with",
    "surveycore::survey_collection",
    get("rename_with.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "distinct",
    "surveycore::survey_collection",
    get("distinct.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "drop_na",
    "surveycore::survey_collection",
    get("drop_na.survey_collection", envir = ns),
    envir = asNamespace("tidyr")
  )

  registerS3method(
    "rowwise",
    "surveycore::survey_collection",
    get("rowwise.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_collection: grouping verbs (PR 2c) ──

  registerS3method(
    "group_by",
    "surveycore::survey_collection",
    get("group_by.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "ungroup",
    "surveycore::survey_collection",
    get("ungroup.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "group_vars",
    "surveycore::survey_collection",
    get("group_vars.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  # `is_rowwise()` is a plain function with an internal class-check branch
  # for `survey_collection` — no S3 method registration. See R/rowwise.R.

  # ── survey_collection: slice verbs (PR 2d) ──

  registerS3method(
    "slice",
    "surveycore::survey_collection",
    get("slice.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_head",
    "surveycore::survey_collection",
    get("slice_head.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_tail",
    "surveycore::survey_collection",
    get("slice_tail.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_min",
    "surveycore::survey_collection",
    get("slice_min.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_max",
    "surveycore::survey_collection",
    get("slice_max.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "slice_sample",
    "surveycore::survey_collection",
    get("slice_sample.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_collection: collapsing verbs (PR 3) ──

  registerS3method(
    "pull",
    "surveycore::survey_collection",
    get("pull.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "glimpse",
    "surveycore::survey_collection",
    get("glimpse.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  # ── survey_collection: join error stubs (PR 4) ──

  registerS3method(
    "left_join",
    "surveycore::survey_collection",
    get("left_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "right_join",
    "surveycore::survey_collection",
    get("right_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "inner_join",
    "surveycore::survey_collection",
    get("inner_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "full_join",
    "surveycore::survey_collection",
    get("full_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "semi_join",
    "surveycore::survey_collection",
    get("semi_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )

  registerS3method(
    "anti_join",
    "surveycore::survey_collection",
    get("anti_join.survey_collection", envir = ns),
    envir = asNamespace("dplyr")
  )
}
