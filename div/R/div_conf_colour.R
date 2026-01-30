# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' return a colour code given a number of stars for the confidence level of bias
#'
#' This function returns a colour (R named colour) based on the confidence level
#' @param x the string associated to the paygap confidence: NA, '', ',', '*', '***', '***'
#' @keywords parse paygap
#' @returns string (named colour)
#' @export
#' @importFrom dplyr if_else
#' @examples
#' div_conf_colour("*")
#'
# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0
div_conf_colour <- function(x) {




  if (knitr::is_latex_output()) {
    theColour <- if_else(is.na(x), "Gray",
          if_else(x == "***", "red",
                  if_else(x == "**", "BurntOrange",
                          if_else(x == "*", "Yellow",
                                  if_else(x == ".", "GreenYellow", "green")))))
  } else { # assume html
    theColour <- if_else(is.na(x), "grey",
                         if_else(x == "***", "red",
                                 if_else(x == "**", "orange",
                                         if_else(x == "*", "yellow",
                                                 if_else(x == ".", "khaki", "green")))))

  }
  return(theColour)
}


#div_conf_colour <- Vectorize(div_conf_col)
