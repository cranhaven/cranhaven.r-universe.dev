#' ANES 2024 Time Series Study (subset)
#'
#' A subset of the 2024 American National Election Study (ANES) Time Series face-to-face sample, containing demographic and vote choice variables for 966 respondents.
#' Useful for demonstrating survey raking workflows.
#'
#' @format A tibble with 966 rows and 7 columns:
#' \describe{
#'   \item{state}{Two-letter US state abbreviation. `NA` for respondents whose state is not identified (106 missing).}
#'   \item{sex}{Respondent sex: `"Male"` or `"Female"` (5 missing).}
#'   \item{race}{Race/ethnicity: `"White"`, `"Black"`, `"Hispanic"`, `"Asian"`, or `"Other"` (11 missing).}
#'   \item{income}{Household income bracket: `"Under $50k"`, `"$50k-$100k"`, or `"Over $100k"` (47 missing).}
#'   \item{education}{Education: `"Less than HS"`, `"High school"`, `"Some college"`, `"Bachelor's"`, or `"Graduate"` (451 missing).}
#'   \item{married}{Marital status: `"Married"`, `"Widowed"`, `"Divorced"`, `"Separated"`, or `"Never married"` (277 missing).}
#'   \item{presidential}{2024 presidential vote choice: `"Harris"` or `"Trump"` (335 missing).}
#' }
#'
#' @source \url{https://electionstudies.org/data-center/2024-time-series-study/}
#'
#' @references American National Election Studies. 2025. \emph{ANES 2024 Time Series Study Full Release}
#'   (dataset and documentation). August 8, 2025 version.
#'   \url{https://www.electionstudies.org/}
'anes24'
