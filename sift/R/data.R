#' 2020 New York Times Headlines
#'
#' Includes selected headlines and additional metadata for NYT articles throughout 2020. This dataset is not a comprehensive account of all major events from 2020.
#'
#' @format A data frame with 1,830 rows and 6 variables:
#' \describe{
#'   \item{headline}{Article Headline}
#'   \item{abstract}{Brief summary of article}
#'   \item{byline}{Contributing Writers}
#'   \item{pub_date}{Date of Publication}
#'   \item{section_name}{NYT section in which article was published}
#'   \item{web_url}{Article URL}
#'   ...
#' }
#'
#' @source Obtained using \href{https://developer.nytimes.com/}{NYT Developer Portal} (Archive API)
"nyt2020"

#' Fragments of US & UK population & leaders
#'
#' These datasets are intended to demonstrate usage of \code{sift::break_join}.
#'
#' @source See \code{tidyr::\link[tidyr]{who}} and \code{ggplot2::\link[ggplot2]{presidential}}.
#' @format NULL
"us_uk_pop"

#' @rdname us_uk_pop
#' @format NULL
"us_uk_leaders"

#' Simulated records of radio station communications.
#'
#' Dataset intended to demonstrate usage of \code{sift::conjecture}.
"comms"
