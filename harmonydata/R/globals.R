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

pkg_globals <- new.env()
pkg_globals$url <- "https://api.harmonydata.ac.uk"
#' Set 'Harmony' URL
#'
#' This function sets the 'Harmony' API URL to be used in the package. By default,
#' it uses the production 'Harmony' API ('https://api.harmonydata.ac.uk'), but you can
#' override it by providing a different URL.
#'
#' @param harmony_url The 'Harmony' API URL to be set. (default: 'https://api.harmonydata.ac.uk')
#' @keywords internal
#'
#' @details
#' The \code{pkg.globals$url} variable is a global variable that holds the 'Harmony' API URL
#' to be used in the package. Once you set the URL using this function, it will be
#' used in all the relevant functions within the package.
#'
#' @examples
#' \donttest{
#' set_url('https://staging.harmonydata.org')
#' set_url() # Will set the URL back to the default 'https://api.harmonydata.ac.uk'
#' }
#'
#' @return
#' No return value, called for side effects.
#'
#' @export
#' @author Ulster University [cph]
set_url <- function(harmony_url = "https://api.harmonydata.ac.uk") {
    pkg_globals$url <- harmony_url
}
