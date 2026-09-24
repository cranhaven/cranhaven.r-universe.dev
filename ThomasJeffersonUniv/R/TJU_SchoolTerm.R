
#' @title TJU School Term
#' 
#' @description ..
#' 
#' @param x \link[base]{Date} object
#' 
#' @returns 
#' \link{TJU_SchoolTerm} returns a \link[base]{character} \link[base]{vector}
#' 
#' 
#' @examples 
#' TJU_SchoolTerm(as.Date(c('2021-03-14', '2022-01-01', '2022-05-01')))
#' 
#' @importFrom lubridate year month
#' @export
TJU_SchoolTerm <- function(x) {
  trm <- cut.default(month(x), breaks = c(1, 4, 7, 9, 12), include.lowest = TRUE, right = FALSE, 
                     labels = c('Winter', 'Spring', 'Summer', 'Fall'))
  paste(as.character.factor(trm), year(x))
}

