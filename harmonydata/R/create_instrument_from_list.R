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

#' Create instrument from list
#'
#' This function creates an instrument from a list of questions.
#'
#' @param question_texts A character vector of question texts.
#' @param question_numbers A character vector of question numbers. If not provided, the question number will be the index of the question text.
#' @param instrument_name A character string of the instrument name.
#'
#' @examples
#' \donttest{
#' instrument = create_instrument_from_list(
#'   list("How old are you?",
#'     "What is your gender?",
#'     "What is your name?")
#' )
#' }
#'
#'
#'
#' @export
#' @author Alex Nikic


create_instrument_from_list <- function(question_texts, question_numbers = NULL, instrument_name = "My instrument") {
    file_name <- "random file"
    data <- list(
        file_id = paste(uuid::UUIDgenerate(output = "raw"), collapse = ""),
        instrument_id = paste(uuid::UUIDgenerate(output = "raw"), collapse = ""),
        instrument_name = instrument_name,
        file_name = file_name
    )

    # add in questions
    for (i in seq_along(question_texts)) {
        question <- list(
            question_text = question_texts[[i]],
            question_no = ifelse(is.null(question_numbers[[i]]), paste(i), question_numbers[[i]]),
            instrument_id = data$instrument_id,
            instrument_name = data$instrument_name
        )

        data$questions[[i]] <- question
    }

    data
}
