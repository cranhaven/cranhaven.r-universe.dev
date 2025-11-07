#' Add miRNA targets from an xlsx-file to a data frame
#'
#' Add miRNA targets from an external xlsx-file to a data frame.
#'
#' Add miRNA targets from an external xlsx-file to a data frame. To add the targets to the
#' data frame, the xlsx-file and the data frame need to have one column in
#' common, such as PubMed-IDs.
#' `join_targets()` can return two different data frames, regulated by `reduce`:
#' 1. If `reduce = FALSE`, `join_targets()` adds targets from an
#'   excel-file to the data frame in a new column. These targets then correspond
#'   to the targets determined in the research paper, but do not necessarily correspond
#'   to the miRNA names mentioned in the abstract.
#' 2. If `reduce = TRUE`, `join_targets()` adds targets from an
#'   xlsx-file to the data frame in a new column. However, an
#'   altered data frame is returned, containing the PubMed-IDs, targets, and
#'   miRNAs from the excel-file. For `reduce = TRUE` to work, the xlsx-file provided
#'   must contain a column with miRNA names.
#'
#' @param df Data frame containing PubMed-IDs
#' that the miRNA targets shall be joined to.
#' @param excel_file xlsx-file. xlsx-file containing miRNA targets and
#' PubMed-IDs.
#' @param col.pmid.excel String. Column containing PubMed-IDs of the
#' `excel_file`.
#' @param col.target.excel String. Column containing targets of the
#' `excel_file`.
#' @param col.mir.excel String. Optional. Column containing miRNAs of the
#' `excel_file`. Needed if `reduce = TRUE`.
#' @param col.pmid.df Symbol. Column containing PubMed-IDs in `df`.
#' @param col.topic.df Symbol. Optional. Only important if \code{reduce = TRUE}.
#' If given, adds a topic column to the reduced data.frame.
#' @param filter_na Boolean. If `filter_na = TRUE`, drops all rows containing
#' `NA` in column `Target`.
#' @param stem_mir_excel Boolean. If `stem_mir_excel = TRUE`, miRNA names
#' provided in `col.mir.excel` are reduced to their stem, e.g. "miR-20a-5p" becomes
#' "miR-20".
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
join_targets <- function(df,
                         excel_file,
                         col.pmid.excel,
                         col.target.excel,
                         col.mir.excel = NULL,
                         col.pmid.df = PMID,
                         col.topic.df = NULL,
                         filter_na = TRUE,
                         stem_mir_excel = TRUE,
                         reduce = FALSE) {

    if(reduce) {
        if(is.null(col.mir.excel)) {
            stop("Cannot apply 'reduce' if col.mir.excel = NULL. Please provide
            an excel-table with a column containing miRNAs and specify
                 col.mir.excel.")
        }
    }

    if(is.null(col.mir.excel) & stem_mir_excel == TRUE) {
        stem_mir_excel <- FALSE
    }

    target_df_complete <- readxl::read_excel(excel_file) %>%
        dplyr::as_tibble()

    target_df <- target_df_complete %>%
        dplyr::select(col.pmid.excel,
                      col.target.excel) %>%
        dplyr::rename(PMID = 1,
                      Target = 2)

    if(!is.null(col.mir.excel)) {
        target_df <- target_df %>%
            dplyr::mutate(miRNA_excel = target_df_complete[[col.mir.excel]])
    }

    if(stem_mir_excel == TRUE) {

        pattern_stem <- "[mM][iI][cC]?[rR][oO]?[rR]?[nN]?[aA]?[-]?\\d+"

        target_df <- target_df %>%
            dplyr::mutate(miRNA_excel = stringr::str_extract(miRNA_excel,
                                                                 pattern = pattern_stem))
    }

    df <- df %>%
        dplyr::rename(PMID = {{col.pmid.df}})

    df_joined <- df %>%
        dplyr::left_join(target_df, by = "PMID")

    if(filter_na == TRUE) {
        df_joined <-  df_joined %>%
            dplyr::filter(!is.na(Target))
    }

    if(reduce == TRUE) {
        df_reduced <- df_joined %>%
            dplyr::select(PMID, Target, miRNA_excel) %>%
            dplyr::filter(!is.na(miRNA_excel)) %>%
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

    return(df_reduced)
}
