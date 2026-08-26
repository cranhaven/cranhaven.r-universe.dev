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

#' Load Instruments from File
#'
#' This function loads instruments from a file specified by the \code{path}
#' parameter and sends the file content to an API for further processing.
#' It also accepts a URL leading to a file.
#'
#' @param path The path to the file to load instruments from.
#' @return A list of instruments returned from the API.
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom base64enc base64encode
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @importFrom tools file_ext
#'
#' @examples
#' \donttest{
#' # Load instruments from a PDF file
#' pdf_file <- "https://www.apa.org/depression-guideline/patient-health-questionnaire.pdf"
#' response <- load_instruments_from_file(pdf_file)
#' }
#'
#' @export
#' @author Ulster University [cph]


load_instruments_from_file <- function(path) {
    file_string <- ""

    #if the file is text, it just returns the text file as a string
    if (tools::file_ext(path) == "txt") {
        # Read the text file as lines
        file_lines <- readLines(path)

        # Concatenate the lines into a single string
        file_string <- paste(file_lines, collapse = "")
    } else {
        file_string <- tobase64(path)
        if (tools::file_ext(path) == "pdf") {
            file_string <- paste("data:application/", tools::file_ext(path), ";base64,", file_string, sep = "")
        } else {
            file_string <- paste("data:application/",
                                 "vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                 ";base64,",
                                 file_string, sep = "")
        }
    }
    #create info you need to send to api
    #headers
    headers <- c(
        `accept` = "application/json",
        `Content-Type` = "application/json"
    )

    # Generate a random UUID
    random_uuid <- paste(uuid::UUIDgenerate(output = "raw"), collapse = "")
    #app might not necessarily be a base64

    data <- list(list(file_id =  random_uuid,
                      file_name = basename(path),
                      file_type = tools::file_ext(path),
                      content = file_string))
    #convert to json
    data_json <- jsonlite::toJSON(data, pretty = FALSE, auto_unbox = TRUE)
    #add brackets
    #send contents
    #change url to be an enviroment variable
    res <- httr::POST(url = paste0(pkg_globals$url, "/text/parse"), encode = "json",
                      httr::add_headers(.headers = headers), body = data_json)
    #contents
    conten <- content(res)

    return(conten)

}

tobase64 <- function(path) {
    is_link <- FALSE
    if (file.exists(path)) {
        is_link <- FALSE
    } else {
        # Check if the string is a valid URL
        is_link <- is_url_file(path)
    }
    # Check if the file is a local file (not a link)
    is_local_file <- !is_link

    if (is_link) {
        # Define the link to the PDF file
        link <- path

        # Download the PDF file
        temp_file <- tempfile()  # Create a temporary file to store the downloaded PDF
        utils::download.file(link, temp_file, mode = "wb")  # 'wb' mode for binary download

        pdf_binary <- temp_file

        # Convert the binary data to base64 encoded string
        pdf_base64 <- base64enc::base64encode(pdf_binary)

        # Remove the temporary file
        unlink(temp_file)
    } else {
        # Read the downloaded PDF as binary
        pdf_binary <- readBin(path, "raw", file.info(path)$size)

        # Convert the binary data to base64 encoded string
        pdf_base64 <- base64enc::base64encode(pdf_binary)
    }
    return(pdf_base64)

}

is_url_file <- function(url) {
    # Perform an HTTP HEAD request to get the headers
    response <- httr::HEAD(url)

    # Get the content type from the headers
    content_type <- httr::headers(response)$`content-type`

    # Check if the content type indicates a file
    is_file <- grepl("^application\\/|^text\\/", content_type)

    return(is_file)
}