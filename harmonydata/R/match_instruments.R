# MIT License
#
# Copyright (c) 2023 Ulster University (https://www.ulster.ac.uk)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the 'Software'), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Match Instruments Function
#'
#' This function takes a list of instruments, converts it to a format acceptable by the database,
#' and matches the instruments using the 'Harmony Data API'. It returns the matched instruments.
#'
#' @param instruments A list of instruments to be matched.
#' @param topics A list of topics with which to tag the questions. Default is empty.
#' @param is_negate A boolean indicating whether to apply negation-based preprocessing. Default is TRUE.
#'
#' This option addresses a common limitation in large language model (LLM) embeddings, where antonyms (e.g., "happy" and "sad") may be treated as similar due to contextual overlap.
#' When \code{is_negate = TRUE}, the function prepends negation terms such as "not" or "didn't" to the input sentences and evaluates whether this increases or decreases their cosine similarity.
#' If the similarity increases after negation, the model interprets the sentences as antonyms and returns a negative similarity score.
#'
#' When \code{is_negate = FALSE}, negation is skipped and most similarity values returned will be positive.
#'
#' The Harmony API defaults to \code{is_negate = TRUE}, as some users prefer detecting antonymy through negative similarity values, while others may prefer only positive scores.'
#'
#' @param clustering_algorithm A string value to select the clustering algorithm to use. Must be one of: "affinity_propagation", "kmeans", "deterministic", "hdbscan". Default is "affinity_propagation".
#'
#' @param ... Optional named arguments:
#' \describe{
#'   \item{model}{The HuggingFace model to be used by the matcher.
#'     Currently recommended models:
#'     \itemize{
#'       \item \code{sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2}
#'       \item \code{sentence-transformers/paraphrase-multilingual-mpnet-base-v2}
#'       \item \code{harmonydata/mental_health_harmonisation_1}
#'     }
#'   }
#' }
#'
#' @return A list containing the matched instruments retrieved from the Harmony Data API. The returned object includes attributes such as the similarity matrix, identified clusters, associated cluster topics, and other relevant metadata.
#'
#' @examples
#' \donttest{
#'
#' instrument_A <- create_instrument_from_list(list(
#'   "How old are you?",
#'   "What is your gender?"
#' ))
#'
#' instrument_B <- create_instrument_from_list(list(
#'   "Do you smoke?"
#' ))
#'
#' instruments <- list(instrument_A, instrument_B)
#'
#' matched_instruments <- match_instruments(
#'   instruments,
#'   topics = list("anxiety", "depression")
#' )
#'
#' # with optional arguments
#' matched_instruments <- match_instruments(
#'   instruments,
#'   topics = list("anxiety", "depression"),
#'   is_negate = TRUE,
#'   clustering_algorithm = "affinity_propagation",
#'   model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
#' )
#' }
#'
#' @import jsonlite
#' @import httr
#'
#'
#' @export
#' @author Ulster University [cph]


match_instruments <- function(instruments,
                              topics = list(),
                              is_negate = TRUE,
                              clustering_algorithm = "affinity_propagation",
                              ...
                              ) {
    #most of the work is simply creating the body
    #steps to create the body
    #take a list of instruments and convert it to a format that is acceptable by the databse
    headers <- c(
        `accept` = "application/json",
        `Content-Type` = "application/json"
    )

    if (! is.null(names(instruments))) { # the case where only one instrument is passed but not enclosed as a list
        instruments <- list("instruments" = list(instruments))
    } else { # the case where a list is passed
        instruments <- list("instruments" = instruments)
    }

    for (i in seq_along(instruments[["instruments"]])){
        instruments[["instruments"]][[i]][["study"]] <- NULL
        instruments[["instruments"]][[i]][["sweep"]] <- NULL
        instruments[["instruments"]][[i]][["metadata"]] <- NULL
        #now clean the questions
        for (j in seq_along(instruments[["instruments"]][[i]][["questions"]])) {
            instruments[["instruments"]][[i]][["questions"]][[j]][["instrument_name"]] <- NULL
            instruments[["instruments"]][[i]][["questions"]][[j]][["topics_auto"]] <- NULL
            instruments[["instruments"]][[i]][["questions"]][[j]][["nearest_match_from_mhc_auto"]] <- NULL
        }
    }

    # add the topics
    instruments[["topics"]] <- topics

    # allow the user to select which llm to use
    kwargs <- list(...)
    if ("model" %in% names(kwargs)) {
      instruments[["parameters"]] <- list(
        "framework" = "huggingface",
        "model" = kwargs$model
      )
    }

    #from questions u need to delete anything after source page
    bod <- jsonlite::toJSON(instruments, pretty = TRUE, auto_unbox = TRUE)

    res <- httr::POST(
                      url = paste0(
                          pkg_globals$url,
                          "/text/match?is_negate=", is_negate,
                          "&clustering_algorithm=", clustering_algorithm
                      ),
                      httr::add_headers(.headers = headers), body = bod, encode = "json")
    #contents
    conten <- content(res)

    # for the clusters, we need to add 1 to the item_ids since R indexes from 1 (whereas python indexes from 0)
    for (i in seq_along(conten$clusters)) {
        for (j in seq_along(conten$clusters[[i]]$item_ids)) {
            conten$clusters[[i]]$item_ids[[j]] <- conten$clusters[[i]]$item_ids[[j]] + 1
        }

        # we also add 1 to each cluster_id
        conten$clusters[[i]]$cluster_id <- conten$clusters[[i]]$cluster_id + 1
    }

    # here we will convert the questions matrix to a data frame to avoid users boilerplating this code
    df <- data.frame(conten$matches[[1]])
    for (x in seq_along(conten$matches)) {
        df[x, ] <- conten$matches[[x]]
    }
    colnames(df) <- make.unique(unlist(lapply(conten$questions, function(x) paste(x$question_no, x$question_text, sep = " "))))
    rownames(df) <- make.unique(unlist(lapply(conten$questions, function(x) paste(x$question_no, x$question_text, sep = " "))))
    conten$matches <- df

    # here we will also convert the response options similarity matrix to a data frame
    if (length(conten$response_options_similarity) > 0) {
        df_response_options <- data.frame(conten$response_options_similarity[[1]])
        for (x in seq_along(conten$response_options_similarity)) {
            df_response_options[x, ] <- conten$response_options_similarity[[x]]
        }
        colnames(df_response_options) <- make.unique(unlist(lapply(conten$questions, function(x) paste(x$question_no, x$question_text, sep = " ")))[seq_len(ncol(df_response_options))])
        rownames(df_response_options) <- make.unique(unlist(lapply(conten$questions, function(x) paste(x$question_no, x$question_text, sep = " ")))[seq_len(nrow(df_response_options))])
        conten$response_options_similarity <- df_response_options
    }

    return(conten)
}
