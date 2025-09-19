#' Contents of Complete Works of William Shakespeare (dataframe)
#'
#' A dataframe containing the full text of all of the complete works of William
#' Shakespeare, as provided by Project Gutenberg.
#'
#' @format A data frame with 166340 rows and 4 variables:
#' \describe{
#'   \item{name}{short (or common) name of the work}
#'   \item{content}{the full contents of the work. Each line is ~70 characters}
#'   \item{full_name}{the complete name of the work, as listed}
#'   \item{genre}{whether the work is poetry, history, comedy, or tragedy}
#' }
#' @source \url{http://www.gutenberg.org/files/100/100-0.txt}
#'
#' @examples
#' works <- bardr::all_works_df
#' subset(works, works$genre == "History")
"all_works_df"

#' Contents of Complete Works of William Shakespeare (list)
#'
#' A list containing the full text of all of the complete works of William
#' Shakespeare, as provided by Project Gutenberg.
#'
#' @format A list with 44 elements, each one containing a character vector
#' containing the full text of a work, given in the element name.
#' @source \url{http://www.gutenberg.org/files/100/100-0.txt}
"all_works_list"
