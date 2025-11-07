#' Count miRNAs above a percent threshold - Helper.
#'
#' Helper function. Count miRNAs above a certain percent threshold.
#'
#' Helper function. Count miRNAs above a certain percent threshold. %
#' relies on number of abstracts a miRNA is mentioned in divided by the total
#' number of abstracts.
#' @param df Data frame containing miRNA names.
#' @param threshold Float. Float between 0 and 1. Determining percent threshold.
#' @param col.mir Symbol. Column with miRNAs.
#'
#' @noRd
#'
#' @importFrom magrittr %>%
count_mir_perc_threshold <- function(df,
                                     threshold = threshold,
                                     col.mir = miRNA) {
    count <- df %>%
        dplyr::filter(perc >= threshold) %>%
        dplyr::select(-perc, -total, -n) %>%
        dplyr::select({{col.mir}}) %>%
        dplyr::pull() %>%
        unique() %>%
        length()

    return(count)
}

#' Count miRNAs above an absolute threshold - Helper.
#'
#' Helper function. Count miRNAs above an absolute threshold.
#'
#' Helper function. Count miRNAs above an absolute threshold. Absolute number
#' relies on number of abstracts a miRNA is mentioned in divided by the total
#' number of abstracts.
#' @param df Data frame containing miRNA names.
#' @param threshold Integer. Integer >= 1. Determining absolute threshold.
#' @param col.mir Symbol. Column with miRNAs.
#'
#' @noRd
#'
#' @importFrom magrittr %>%
count_mir_total_threshold <- function(df,
                                      threshold = threshold,
                                      col.mir = miRNA) {
    count <- df %>%
        dplyr::filter(n >= threshold) %>%
        dplyr::select(-perc, -total, -n) %>%
        dplyr::select({{col.mir}}) %>%
        dplyr::pull() %>%
        unique() %>%
        length()

    return(count)
}
