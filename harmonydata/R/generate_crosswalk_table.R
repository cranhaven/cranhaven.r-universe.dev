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

#' Generate Crosswalk Table Function
#'
#' This function generates a crosswalk table using a list of instruments and a similarity matrix,
#' produced by the \code{\link{match_instruments}} function.
#'
#' @description
#' A crosswalk is a table that lists matched variables from different studies or instruments,
#' enabling data harmonization across datasets.
#'
#' @details
#' A crosswalk is a mapping between conceptually similar items (e.g., survey questions or variables)
#' from different instruments. It is used to identify and align comparable variables across datasets
#' that use different formats or wordings. This is especially useful in meta-analysis, data integration,
#' and comparative research, where consistent constructs need to be analyzed across multiple sources.
#'
#' The similarity matrix passed to this function is usually obtained from \code{\link{match_instruments}}.
#' @param instruments The original list of instruments, each containing a question. The sum of the number of questions in all instruments is the total number of questions which should equal both the width and height of the similarity matrix.
#' @param similarity The cosine similarity matrix that is outputed from the \code{\link{match_instruments}} function.
#' @param threshold The minimum threshold that we consider a match. This is applied to the absolute match value. So if a question pair has similarity 0.2 and threshold = 0.5, then that question pair will be excluded. Leave as None if you don't want to apply any thresholding.
#' @param is_allow_within_instrument_matches Defaults to False. If this is set to True, we include crosswalk items that originate from the same instrument, which would otherwise be excluded by default.
#' @param is_enforce_one_to_one Defaults to False.  If this is set to True, we force all variables in the crosswalk table to be matched with exactly one other variable.
#' @return  A crosswalk table as a DataFrame.
#'
#' @importFrom purrr map set_names
#' @importFrom assertthat assert_that
#'
#' @examples
#' \donttest{
#' instrument_A = create_instrument_from_list(list(
#'   "How old are you?",
#'   "What is your gender?"
#' ))
#'
#' instrument_B = create_instrument_from_list(list(
#'   "Do you smoke?"
#' ))
#'
#' instruments = list(instrument_A, instrument_B)
#'
#' match_response = match_instruments(instruments)
#' instrument_list = match_response$instruments
#' similarity_matrix = match_response$matches
#'
#' crosswalk_table.df = generate_crosswalk_table(
#'   instrument_list, similarity_matrix, threshold = 0.7,
#'   is_allow_within_instrument_matches = FALSE, is_enforce_one_to_one = TRUE
#' )
#'
#' }
#'
#' @export
#' @author Alex Nikic
#' @author Omar Hassoun


generate_crosswalk_table <- function(
    instruments,
    similarity,
    threshold,
    is_allow_within_instrument_matches = FALSE,
    is_enforce_one_to_one = FALSE
) {
    # match$questions are the questions
    # match$matches is the similarity matrix

    matching_pairs <- data.frame(
        pair_name = character(),        # Name of the survey pair
        question1_id	= character(), # Name of the first question pair
        question1_no = character(),     # ID of question from first survey
        question1_text = character(),   # Text of question from first survey
        question2_id	= character(), # Name of the second question pair
        question2_no = character(),     # ID of question from second survey
        question2_text = character(),   # Text of question from second survey
        match_score = numeric()         # Similarity score between the questions
    )


    instrument_lookup <- purrr::set_names(
        purrr::map_chr(instruments, "instrument_name"),
        purrr::map_chr(instruments, "instrument_id")
    )
    questions <- unlist(lapply(instruments, function(x) x$questions), recursive = FALSE)

    questions <- purrr::map(questions, function(question) {
        question$instrument_name <- instrument_lookup[[question$instrument_id]]
        question
    })

    # Initialize sets to track used questions (only if needed)
    used_question1 <- c()
    used_question2 <- c()
    # Iterate through all pairs of questions
    for (i in seq_along(questions)) {
        for (j in seq_along(questions)) {
            # Check for non-duplicate and similarity above threshold
            if (j > i && similarity[[i]][[j]] > threshold) {

                # If one-to-one matching is enforced, skip already matched questions
                if (is_enforce_one_to_one) {
                    if (i %in% used_question1 || j %in% used_question2) {
                        next
                    }
                }

                # If within_instrument_matches is FALSE, skip if the questions are from the same instrument
                if (!is_allow_within_instrument_matches) {
                    assertthat::assert_that(!is.null(questions[[i]]$instrument_id))
                    assertthat::assert_that(!is.null(questions[[j]]$instrument_id))

                    if (questions[[i]]$instrument_id == questions[[j]]$instrument_id) {
                        next
                    }
                }

                # Append to dataframe
                matching_pairs <- rbind(matching_pairs, data.frame(
                    pair_name = paste(questions[[i]]$instrument_name,  questions[[i]]$question_no,
                                      questions[[j]]$instrument_name, questions[[j]]$question_no, sep = "_"),
                    question1_id = paste(questions[[i]]$instrument_name, questions[[i]]$question_no, sep = "_"),
                    question1_no = questions[[i]]$question_no,
                    question1_text = questions[[i]]$question_text,
                    question2_id = paste(questions[[j]]$instrument_name, questions[[j]]$question_no, sep = "_"),
                    question2_no = questions[[j]]$question_no,
                    question2_text = questions[[j]]$question_text,
                    match_score = as.numeric(similarity[[i]][[j]])
                )
                )

                # If enforcing one-to-one matching, mark questions as used
                if (is_enforce_one_to_one) {
                    used_question1 <- c(used_question1, i)
                    used_question2 <- c(used_question2, j)
                }
            }
        }
    }

    matching_pairs
}
