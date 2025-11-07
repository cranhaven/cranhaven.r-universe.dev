#' Get miRNA names in common between two vectors
#'
#' Get miRNA names in common between two vectors.
#'
#' Get miRNA names in common between two vectors.
#' `get_shared_mir_vec()` compares two vectors containing miRNA names and
#' returns the miRNA names that are in both vectors.
#'
#' @param mirna.vec.1 Character vector. First vector containing miRNA
#' names.
#' @param mirna.vec.2 Character vector. Second vector containing miRNA
#' names.
#'
#' @return Character vector containing miRNA names in common between two
#' vectors.
#'
#' @family get functions
#'
#' @export
get_shared_mir_vec <- function(mirna.vec.1, mirna.vec.2) {

  shared <- intersect(mirna.vec.1, mirna.vec.2)

  return(shared)
}

#' Get top miRNA names in common between two topics of a data frame
#'
#' Get top miRNA names in common between two topics of a data frame.
#'
#' Get top miRNA names in common between two topics of a data frame.
#' `get_shared_mir_df()` compares the top miRNA names of two topics
#' in a data frame and returns the miRNA names in common.
#'
#' @param df Data frame containing at least two topics and miRNA names.
#' @param top Integer. Number of top miRNA names to extract for both topics.
#' @param topic String. Vector of strings containing topic names to compare
#' miRNA names for. If `topic = NULL`, `topic` defaults to all topic names contained
#' in `col.topic` in `df`. `topic` must only contain two topic names.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Character vector containing miRNA names in common between two
#' topics.
#'
#' @family get functions
#'
#' @export
get_shared_mir_df <- function(df,
                              top = 5,
                              topic = NULL,
                              col.topic = Topic,
                              col.mir = miRNA,
                              col.pmid = PMID) {
    if(is.null(topic)) {
        topic <- df %>%
            dplyr::select({{col.topic}}) %>%
            dplyr::pull() %>%
            unique()
    }

    if(length(topic) != 2) {
        stop("The number of topics exceeds 2. Please define two topics in
             'topic'. These topics must be contained as strings in 'col.topic'
             in df.")
    }

    df_topics <- df %>%
        dplyr::filter({{col.topic}} %in% topic) %>%
        dplyr::group_split(as.factor({{col.topic}}))

    mirna_vec <- purrr::map(df_topics,~get_mir(df = .x, top = top,
                                               col.mir = {{col.mir}},
                                               col.pmid = {{col.pmid}}))

    mirna.vec.1 <- mirna_vec[[1]]
    mirna.vec.2 <- mirna_vec[[2]]

    shared <- intersect(mirna.vec.1, mirna.vec.2)

    return(shared)
}

#' Identify miRNA names distinct for one vector compared to another vector
#'
#' Identify miRNA names distinct for one vector compared to another vector.
#'
#' Get distinct miRNA names of one vector compared to another vector.
#' `get_distinct_mir()` compares two vectors containing miRNA names and
#' returns the miRNA names that are exclusive for `mirna.vec.1`.
#'
#' @param mirna.vec.1 Character vector. First vector containing miRNA
#' names.
#' @param mirna.vec.2 Character vector. Second vector containing miRNA
#' names.
#'
#' @return Character vector containing miRNA names distinct for `mirna.vec.1`
#' compared to `mirna.vec.2`.
#'
#' @family get functions
#'
#' @export
get_distinct_mir_vec <- function(mirna.vec.1, mirna.vec.2) {

    different_mir <- setdiff(mirna.vec.1, mirna.vec.2)

  return(different_mir)
}

#' Identify top miRNA names distinct for one topic compared to another topic
#'
#' Identify top miRNA names distinct for one topic compared to another topic in a
#' data frame.
#'
#' Get top distinct miRNA names of one topic compared to another topic in a
#' data frame.
#' `get_distinct_mir_df()` compares the top miRNA names of two topics and
#' returns the miRNA names that are exclusive for `distinct`.
#'
#' @param df Data frame containing at least two topics and miRNA names.
#' @param distinct String. Name of topic top distinct miRNAs shall be identified
#' for. `distinct` must be contained in the topic names provided in `topic`.
#' @param top Integer. Number of top miRNA names to extract for both topics.
#' @param topic String. Vector of strings containing topic names to compare
#' miRNA names for. If `topic = NULL`, `topic` defaults to all topic names contained
#' in `col.topic` in `df`. `topic` must only contain two topic names.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Character vector containing miRNA names distinct for `distinct`
#' compared to the second topic provided in `topic`.
#'
#' @family get functions
#'
#' @export
get_distinct_mir_df <- function(df,
                                distinct,
                                top = 5,
                                topic = NULL,
                                col.topic = Topic,
                                col.mir = miRNA,
                                col.pmid = PMID) {

    if(is.null(topic)) {
        topic <- df %>%
            dplyr::select({{col.topic}}) %>%
            dplyr::pull() %>%
            unique()
    }

    if(length(topic) != 2) {
        stop("The number of topics exceeds 2. Please define two topics in
             'topic'. These topics must be contained as strings in 'col.topic'
             in df.")
    }

    if(!distinct %in% topic) {
        stop("Topic given in 'distinct' is not in 'topic'.
             Please provide a topic for 'distinct' that is contained
             in 'topic'.")
    }

    topic_distinct <- distinct
    topic_other <- topic[!topic %in% distinct]

    mirna.vec.1 <- df %>%
        dplyr::filter({{col.topic}} == topic_distinct) %>%
        get_mir(top = top,
                col.mir = {{col.mir}},
                col.pmid = {{col.pmid}})

    mirna.vec.2 <- df %>%
        dplyr::filter({{col.topic}} == topic_other) %>%
        get_mir(top = top,
                col.mir = {{col.mir}},
                col.pmid = {{col.pmid}})

    different_mir <- setdiff(mirna.vec.1, mirna.vec.2)

    return(different_mir)
}
