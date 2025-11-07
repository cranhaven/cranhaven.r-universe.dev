#' Add miRNA targets from miRTarBase version 8.0
#'
#' Add miRNA targets from miRTarBase version 8.0 to a data frame.
#'
#' Add miRNA targets from miRTarBase version 8.0 to a data frame.
#' `join_mirtarbase()` can return two different data frames, regulated by `reduce`:
#' 1. If `reduce = FALSE`, `join_mirtarbase()` adds targets from miRTarBase 8.0
#'   to the data frame in a new column. These targets then correspond
#'   to the targets determined in the research paper, but do not necessarily correspond
#'   to the miRNA names mentioned in the abstract.
#' 2. If `reduce = TRUE`, `join_mirtarbase()` adds targets from
#'   miRTarBase 8.0 to the data frame in a new column. However, an
#'   altered data frame is returned, containing the PubMed-IDs, targets, and
#'   miRNAs from miRTarBase 8.0.
#'
#' miRTarBase was published in
#'
#' Hsi-Yuan Huang, Yang-Chi-Dung Lin, Jing Li, et al.,
#' miRTarBase 2020: updates to the experimentally validated microRNA–target
#' interaction database, Nucleic Acids Research, Volume 48, Issue D1,
#' 08 January 2020, Pages D148–D154, https://doi.org/10.1093/nar/gkz896
#'
#' @param df Data frame containing PubMed-IDs
#' that the miRNA targets shall be joined to.
#' @param col.pmid.df Symbol. Column containing PubMed-IDs in `df`.
#' @param col.topic.df Symbol. Optional. Only important if \code{reduce = TRUE}.
#' If given, adds a topic column to the reduced data.frame.
#' @param filter_na Boolean. If `filter_na = TRUE`, drops all rows containing
#' `NA` in column `Target`.
#' @param reduce Boolean. If `reduce = FALSE`, adds a new column containing miRNA
#' targets to `df`.
#' If `reduce = TRUE`, adds two new
#' columns containing miRNA names and miRNA targets to `df`. All
#' other columns except for the PubMed-ID column and (optionally) the
#' topic column are dropped.
#'
#' @return Data frame containing miRNA targets.
#'
#' @family target functions
#'
#' @export
join_mirtarbase <- function(df,
                            col.pmid.df = PMID,
                            col.topic.df = NULL,
                            filter_na = TRUE,
                            reduce = FALSE) {

    df <- df %>%
        dplyr::rename(PMID = {{col.pmid.df}})

    df_joined <- df %>%
        dplyr::left_join(df_mirtarbase, by = "PMID")

    if(filter_na == TRUE) {
        df_joined <-  df_joined %>%
            dplyr::filter(!is.na(Target))
    }

    if(reduce == TRUE) {
        df_reduced <- df_joined %>%
            dplyr::select(PMID, Target, miRNA_tarbase) %>%
            dplyr::filter(!is.na(miRNA_tarbase)) %>%
            dplyr::distinct()
    } else {
        return(df_joined)
    }

    if(!missing(col.topic.df)) {
        df_topic <- df %>%
            dplyr::select(PMID, {{col.topic.df}}) %>%
            dplyr::distinct()

        df_reduced <- df_reduced %>%
            dplyr::left_join(df_topic)
    }

    df_reduced <- df_reduced %>%
        dplyr::rename(miRNA = miRNA_tarbase)

    return(df_reduced)
}
