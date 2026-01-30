# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Adds a column with new labels (H)igh and (L) for a given colName (within a given grade and jobID)
#'
#' This function calculates the entropy of a system with discrete states
#' @param d tibble, a tibble with team data columns as defined in the documentation (at least the column colName (as set by next parameter), 'grade', and 'jobID')
#' @param colName the name of the columns that contains the factor object to be used as explaining dimension for the paygap (defaults to 'gender')
#' @param value1 character, the label to be used for the first half of observations (the smallest ones)
#' @param value2 character, the label to be used for the second half of observations (the biggest ones)
#' @param newColName the value in new column name that will hold the values value1 and value2
#' @keywords calculate the controlled paygap for a dataset d - controlled by grade (seniority) and jobID (type of role)
#' @returns dataframe (with columns grade, jobID, salary_selectedValue, salary_others, n_selectedValue, n_others, paygap, confidence) , where "confidence" is one of the following: NA = not available (numbers are too low), "" = no bias detectable, "." = there might be some bias, but we're not sure, "*" = bias detected wit some degree of confidence, "**" = quite sure there is bias, "***" = trust us, this is biased.
#' @export
#' @importFrom dplyr group_by summarise full_join if_else mutate
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @examples
#' df <- div_add_median_label(div_fake_team())
#' colnames(df)

div_add_median_label <- function(d,
                       colName    = 'age',
                       value1     = 'T',
                       value2     = 'F',
                       newColName = 'isYoung'
                       ) {
  # global variables:
  grade <- jobID <-  NULL
  # local variables:
  XX <- NULL

  # Relative to grade and jobID
  medians <- d %>%
    dplyr::group_by(grade, jobID) %>%
    dplyr::summarise(median(get(colName)))
  colnames(medians)[3] <- "XX"
  x <-  dplyr::full_join(d, medians) %>%
    dplyr::mutate(!!newColName := dplyr::if_else(get(colName) < XX, value1, value2))
  colnames(x)[ncol(x) - 1] <- paste0(colName, "_median")
  return(x)
}
