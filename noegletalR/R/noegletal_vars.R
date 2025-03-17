#' List the available variables on 'noegletal.dk'
#'
#' @description
#' When called, `noegletal_vars()` will go to 'nøgletal.dk' to retrieve an
#' updated list of available variables (nøgletal) including their definitions
#' and return this data as a [tibble::tibble()] with a row for each variable and
#' three columns: 'variable_id', 'variable_name', 'variable_definition'.
#'
#' The primary use of the function is to get the ID's for the variables that a
#' user wishes to retrieve with the [noegletalR::noegletal_get()] function.
#' Additionally, the function is also useful for probing potential variables
#' (including their definitions) that might be of interest for analysis.
#'
#' A useful way to browse the list is to run the following code from the RStudio
#' console `View(noegletalR::noegletal_vars())` which will open the interactive
#' data viewer in RStudio.
#'
#' Since the list of variables is retrieved anew on every call,
#' `noegletal_vars()` can only be executed when connected to the internet.
#' Response caching is on the development roadmap for the [noegletalR] package,
#' but is not implemented yet and does come with its own set of challenges.
#'
#' @returns A [tibble::tibble()] where each row is a variable and the columns are
#'   'variable_id', 'variable_name' and 'variable_definition'.
#' @export
#' @examples
#' noegletal_vars()
noegletal_vars <- function() {

  source_code <- rvest::read_html("https://www.noegletal.dk/noegletal/ntInfo24A.html",
                                  encoding = "latin1")

  var_headings <- source_code |>
    rvest::html_elements("div.ntal > a:first-child")

  variable_ids <- var_headings |>
    rvest::html_attr("name") |>
    substr(3, 5)

  variable_names <- var_headings |>
    rvest::html_text(trim = TRUE)

  variable_definitions <- source_code |>
    rvest::html_elements("div.ntal") |>
    lapply(function(x) {
      siblings <- x |>
        rvest::html_nodes(xpath = "following-sibling::*")

      # Extract all siblings until the next div.ntal
      relevant_siblings <- siblings[cumsum(rvest::html_name(siblings) == "div" &
                                             grepl("ntal", rvest::html_attr(siblings, "class"))) == 0]

      # Extract text from the HTML from the relevant siblings a combine into one string
      paste(sapply(relevant_siblings, rvest::html_text2), collapse = "\n\n")
    }) |>
    unlist()

  cbind(variable_ids, variable_names, variable_definitions) |>
    tibble::as_tibble()
}
