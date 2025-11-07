#' Helper - Pull miRNA names from data frame
#'
#' Pull miRNA names from data frame.
#'
#' @param df Data frame containing miRNAs names.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Character vector containing miRNA names.
#'
#' @noRd
pull_mirnas <- function(df,
                        col.mir = miRNA) {
    # Select miRNA column and pull miRNAs
    mirnas <- df %>%
        dplyr::select({{col.mir}}) %>%
        dplyr::pull() %>%
        unique()

    return(mirnas)
}

#' Helper - Get shared miRNAs of topics
#'
#' Get shared miRNAs of topics
#'
#' @param df Data frame containing miRNA names and consisting of topics. Topics
#' can be more than two topics.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.topic Symbol. Column containing topic names.
#'
#' @return Character vector of shared miRNAs between two topics.
#'
#' @noRd
get_all_shared_mir <- function(df,
                               col.mir = miRNA,
                               col.topic = Topic) {

    no_topics <- df %>%
        dplyr::select({{col.topic}}) %>%
        dplyr::pull() %>%
        unique() %>%
        length()

    shared_mirs <- df %>%
        dplyr::distinct({{col.topic}}, {{col.mir}}) %>%
        dplyr::add_count({{col.mir}}) %>%
        dplyr::filter(n == no_topics) %>%
        dplyr::select({{col.mir}}) %>%
        dplyr::pull() %>%
        unique

    return(shared_mirs)
}
