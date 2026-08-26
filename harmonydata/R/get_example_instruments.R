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

#' Retrieve Example Instruments from 'Harmony Data API'
#'
#' This function retrieves example instruments from the 'Harmony Data API'
#' using an HTTP POST request.
#'
#' @usage
#' get_example_instruments()
#'
#' @return
#' A list representing example instruments retrieved
#' from the 'Harmony Data API'.
#'
#' @examples
#' \donttest{
#' # Load required libraries (httr) and call the function
#' require(httr)
#' instruments <- get_example_instruments()
#'
#' # Print the retrieved JSON content
#' print(instruments)
#' }
#'
#' @importFrom httr POST content
#'
#' @export
#' @author Ulster University [cph]


get_example_instruments <- function() {
    headers <- c(
        `accept` = "application/json",
        `content-type` = "application/x-www-form-urlencoded"
    )

    res <- httr::POST(url = paste0(pkg_globals$url, "/text/examples"), httr::add_headers(.headers = headers))

    cont <- content(res)

    # assign keys to each instrument based on their instrument_name
    output <- list()
    for (i in seq_along(cont)) {
        output[[cont[[i]]$instrument_name]] <- cont[[i]]
    }

    return(output)
}