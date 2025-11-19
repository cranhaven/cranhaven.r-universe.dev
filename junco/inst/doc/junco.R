## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages, message=FALSE---------------------------------------------
library(rtables)
library(junco)
library(tern)
library(dplyr)

## ----load_datasets, message=FALSE---------------------------------------------
adsl <- ex_adsl
adae <- ex_adae
advs <- ex_advs

## ----data_manipulation, message=FALSE-----------------------------------------
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"

non_ctrl_grp <- setdiff(levels(adsl[[trtvar]]), ctrl_grp)

adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
adsl$rrisk_header_vs <- "Difference in Mean Change (95% CI)"

# define colspan_trt_map
colspan_trt_map <- create_colspan_map(adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

# define reference group specification
ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

adae[["TRTEMFL"]] <- "Y"

# add adsl variables to adae
adae <- adae %>% left_join(., adsl)
advs <- advs %>% left_join(., adsl)

advs[advs[["ABLFL"]] == "Y", "CHG"] <- NA

## ----extra_args_definition----------------------------------------------------
## extra args for a_freq_j controlling specification of
## reference group, denominator used to calculate percentages,
## and other details
extra_args_rr <- list(
  denom = "n_altdf",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = c("count_unique_fraction")
)

## ----layout_definition, eval = TRUE-------------------------------------------
lyt <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  analyze("TRTEMFL",
    afun = a_freq_j,
    extra_args = append(extra_args_rr, list(val = "Y", label = "Number of subjects with AE"))
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr
  )

## ----build_table, eval = TRUE-------------------------------------------------
tbl <- build_table(lyt, adae, alt_counts_df = adsl)
head(tbl, 10)

## ----tern_layout_and_table----------------------------------------------------
lyt_tern <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by(trtvar,
    show_colcounts = TRUE,
    split_fun = add_riskdiff(arm_x = ctrl_grp, arm_y = non_ctrl_grp)
  ) %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event"
    ),
    riskdiff = TRUE
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_occurrences(
    var = "AEBODSYS",
    denom = "N_col",
    riskdiff = TRUE,
    .stats = c("count_fraction")
  ) %>%
  count_occurrences(
    vars = "AEDECOD",
    denom = "N_col",
    riskdiff = TRUE,
    .stats = c("count_fraction")
  )


tbl_tern <- build_table(lyt_tern, adae, alt_counts_df = adsl)
head(tbl_tern, 10)

## ----method_switching, eval = TRUE--------------------------------------------
extra_args_rr[["method"]] <- "waldcc"

tbl2 <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = FALSE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  build_table(adae, alt_counts_df = adsl)

head(tbl2, 10)

## ----subgroup_tables, eval = TRUE---------------------------------------------
extra_args_rr_common <- list(
  denom = "n_altdf",
  denom_by = "SEX"
)

extra_args_rr <- append(
  extra_args_rr_common,
  list(
    riskdiff = FALSE,
    extrablankline = TRUE,
    .stats = c("n_altdf"),
    label_fstr = "Gender: %s"
  )
)

extra_args_rr2 <- append(
  extra_args_rr_common,
  list(
    riskdiff = TRUE,
    ref_path = ref_path,
    method = "wald",
    .stats = c("count_unique_denom_fraction"),
    na_str = rep("NA", 3)
  )
)

tbl <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) %>%
  ## main columns
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar, show_colcounts = TRUE) %>%
  ## risk diff columns, note nested = FALSE
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp),
    show_colcounts = FALSE
  ) %>%
  split_rows_by("SEX", split_fun = drop_split_levels) %>%
  summarize_row_groups("SEX",
    cfun = a_freq_j,
    extra_args = extra_args_rr
  ) %>%
  split_rows_by("TRTEMFL",
    split_fun = keep_split_levels("Y"),
    indent_mod = -1L,
    section_div = c(" ")
  ) %>%
  summarize_row_groups("TRTEMFL",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr2, list(label = "Subjects with >=1 AE", extrablankline = TRUE))
  ) %>%
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = TRUE
  ) %>%
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr2
  ) %>%
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr2
  ) %>%
  build_table(adae, alt_counts_df = adsl)

head(tbl, 30)

## ----extra_statistics, eval = TRUE--------------------------------------------
tern::get_stats("summarize_ancova")
tern::get_stats("analyze_vars_numeric")

## ----summarize_aval_chg_diff, eval = TRUE-------------------------------------
multivars <- c("AVAL", "AVAL", "CHG")

extra_args_3col <- list(
  format_na_str = rep(NA, 3),
  ref_path = ref_path,
  ancova = FALSE,
  comp_btw_group = TRUE,
  multivars = multivars
)


lyt_vs_p1 <- basic_table(
  show_colcounts = FALSE,
  colcount_format = "N=xx"
) %>%
  ### first columns
  split_cols_by("colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map),
    show_colcounts = FALSE
  ) %>%
  split_cols_by(trtvar,
    show_colcounts = TRUE, colcount_format = "N=xx"
  ) %>%
  ## set up a 3 column split
  split_cols_by_multivar(multivars,
    varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
  ) %>%
  split_rows_by("PARAM",
    label_pos = "topleft",
    split_label = "Parameter",
    section_div = " ",
    split_fun = drop_split_levels
  ) %>%
  ## note the child_labels = hidden for AVISIT, these labels will be taken care off by
  ## applying function summarize_aval_chg_diff further in the layout
  split_rows_by("AVISIT",
    label_pos = "topleft",
    split_label = "Study Visit",
    split_fun = drop_split_levels,
    child_labels = "hidden"
  )


lyt_vs <- lyt_vs_p1 %>%
  ### restart for the rrisk_header columns - note the nested = FALSE option
  ### also note the child_labels = "hidden" in both PARAM and AVISIT
  split_cols_by("rrisk_header_vs", nested = FALSE) %>%
  split_cols_by(trtvar,
    split_fun = remove_split_levels(ctrl_grp),
    labels_var = "rrisk_label",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) %>%
  ### difference columns : just 1 column & analysis needs to be done on change
  split_cols_by_multivar(multivars[3],
    varlabels = c(" ")
  ) %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
  analyze("STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result_vs <- build_table(lyt_vs, advs, alt_counts_df = adsl)

## ----display_result_vs, eval = TRUE-------------------------------------------
head(result_vs, 15)

## ----alternative_table, eval = TRUE-------------------------------------------
multivars <- c("AVAL", "AVAL", "CHG")

extra_args_3col <- list(
  format_na_str = rep(NA, 3),
  ancova = FALSE,
  comp_btw_group = FALSE,
  multivars = multivars
)

lyt_vs2 <- lyt_vs_p1 %>%
  ### the variable passed here in analyze is not used (STUDYID), it is a dummy var passing,
  ### the function a_summarize_aval_chg_diff_j grabs the required vars from cols_by_multivar calls
  analyze("STUDYID",
    afun = a_summarize_aval_chg_diff_j,
    extra_args = extra_args_3col
  )

result_vs2 <- build_table(lyt_vs2, advs, alt_counts_df = adsl)

## ----display_alternative_table, eval = TRUE-----------------------------------
head(result_vs2, 15)

