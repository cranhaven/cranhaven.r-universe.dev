
#### Import the pipe operator from magrittr ####
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' @importFrom dplyr anti_join arrange bind_cols bind_rows case_when count desc distinct ends_with everything filter group_by inner_join lead left_join mutate n n_distinct pull recode rename rename_all sample_n select slice starts_with summarise ungroup
#' @importFrom ggplot2 aes annotate element_blank element_text facet_grid facet_wrap geom_boxplot geom_col geom_hex geom_histogram geom_hline geom_jitter geom_point geom_rect geom_segment geom_tile geom_vline ggplot ggtitle guide_legend guides scale_colour_manual scale_fill_manual scale_fill_viridis_c theme theme_bw xlab ylab
#' @importFrom purrr map map2 map2_int map_chr map_int map_lgl
#' @importFrom readr cols read_delim read_lines read_table2
#' @importFrom stats runif
#' @importFrom stringr str_c str_detect str_replace str_split str_split_fixed
#' @importFrom tibble as_tibble enframe tibble
#' @importFrom tidyr extract gather nest pivot_longer pivot_wider replace_na separate unnest
#' @importFrom utils combn unzip
#' @importFrom Rcpp evalCpp
#' @useDynLib CKMRpop
NULL


# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      "ID",
      "ID_1",
      "ID_2",
      "X2",
      "X3",
      "aes",
      "age",
      "age_1",
      "age_2",
      "amm",
      "amm_as_tib",
      "anc_match_matrix",
      "ancestors_1",
      "ancestors_2",
      "arrow",
      "assp",
      "born_year",
      "born_year_1",
      "born_year_2",
      "cc_1",
      "cc_2",
      "child_num",
      "cluster",
      "cohort",
      "conn_comp",
      "dom_relat",
      "dom_relat-max_hit",
      "dr",
      "dr_hits",
      "event",
      "extract",
      "female",
      "first",
      "from",
      "fract",
      "id",
      "id_1",
      "id_2",
      "ind_1",
      "ind_2",
      "indiv",
      "kid",
      "kid_id",
      "kid_idx",
      "kid_year",
      "kin_prob",
      "ma",
      "ma_1",
      "ma_2",
      "ma_age",
      "ma_id",
      "ma_surv_y",
      "ma_year",
      "male",
      "max_hit",
      "mean_fract",
      "mean_surv",
      "name",
      "no_relat",
      "nodes",
      "nojit_age1",
      "nojit_age2",
      "num_mates",
      "num_offs",
      "pa",
      "pa_1",
      "pa_2",
      "pa_age",
      "pa_id",
      "pa_year",
      "pair_type",
      "parent",
      "parent_idx",
      "pop",
      "pop_1",
      "pop_post",
      "pop_post_1",
      "prim_anc_1",
      "prim_anc_2",
      "primary_shared_ancestors",
      "psa_tibs",
      "relationship_zone_names",
      "relatives",
      "rero_1",
      "rero_ac",
      "sad_fem_counts",
      "samp_year_1",
      "samp_year_2",
      "samp_years_list",
      "samp_years_list_dur",
      "samp_years_list_post",
      "samp_years_list_pre",
      "samp_years_list_1",
      "samp_years_list_2",
      "sampling_year",
      "sampling_year_1",
      "sampling_year_2",
      "second",
      "sex",
      "sex_1",
      "sex_2",
      "surv_fract",
      "sy",
      "sy_1",
      "sy_2",
      "syears",
      "syears_dur",
      "syears_post",
      "syears_pre",
      "tag",
      "times_encountered",
      "tmp_1",
      "tmp_2",
      "to",
      "tot_dom",
      "tot_prob",
      "trio",
      "upper_member",
      "val",
      "value",
      "which_matrix",
      "x",
      "x_1",
      "x_2",
      "xjit",
      "xlab",
      "xmax",
      "xmin",
      "y",
      "y_1",
      "y_2",
      "year",
      "yjit",
      "ylab",
      "ymax",
      "ymin",
      "zone"
    )
  )
}
