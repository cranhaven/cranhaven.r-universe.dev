# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Prepare the paygap matrix to be published in LaTeX
#'
#' This function formats the paygap matrix (created by div_paygap()) and prepares it for printing via the function knitr::kable()
#' @param pg paygap object as created by div::div_paygap(). This is an S3 object with a specific structure
#' @param label character, the label to be used in the caption of the kable object
#' @param min_nbr_show numeric, if provided then only groups that have more than min_nbr_show employees in both categories (selectedValue and others) will be shown
#' @param max_length_jobID numeric, if provided the maximal length of the column jobID (in characters)
#' @param max_length_colnames numeric, if provided the maximal length of the column names (in characters)
#' @keywords parse paygap
#' @returns knitr::kable object (for LaTeX)
#' @export
#' @importFrom kableExtra row_spec column_spec kable_styling
#' @importFrom dplyr arrange
#' @importFrom stringr str_replace_all str_sub
#' @examples
#' d  <- div_fake_team()
#' pg <- div_paygap(d)
#' div_parse_paygap(pg)
#'
# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0
div_parse_paygap <- function(pg,
                             label               = NULL,
                             min_nbr_show        = NULL,
                             max_length_jobID    = 12,
                             max_length_colnames = 9) {
  # Global variables:
  paygap <- NULL

  if (class(pg) != "paygap") stop("div ERROR: div_parse_paygap() called with a non-paygap object.")

  tmp <- div_round_paygap(pg) # extract the rounded data from the paygap object

  colnames(tmp) <- pg$colnames

  # Delete the rows with not enough observations if requested:
  if (!is.null(min_nbr_show)) {
      tmp <- tmp[tmp[,5] >= min_nbr_show & tmp[,6] >= min_nbr_show,]
      tmp <- tmp[complete.cases(tmp),]
    }

  # Make the column names shorter:
  colnames(tmp) <- colnames(tmp)     %>%
      str_replace_all("salary","sal")  %>%
      str_replace_all("other","oth")   %>%
      str_sub(1, max_length_colnames)

  # trim the jobIDs
  tmp$jobID <- str_sub(tmp$jobID, 1, max_length_jobID)

  # Set a label -- but note that knitr will add a prefix "tabl:" (so let's not do it double)
  if (is.null(label)) {label <- paste0("pg:", pg$x)}

  # Finally order the tmp tibble before starting the formatting:
  tmp <- tmp %>% dplyr::arrange(paygap)
  bootstr_opts <- ifelse(nrow(tmp) > 12,
                         c("striped", "hover", "condensed", "responsive"),
                         c("striped", "hover", "responsive"))

  # Parse the kable object and return it for printing
  x <- knitr::kable(tmp,
    caption = paste0("The paygap for ", gsub("_", " ", pg$x), " (in terms of ", gsub("_", " ", pg$y), ") as a ratio, along with  the confidence level that this paygap is significant alongside the control variable ", pg$ctrl_var, "."),  # \\label{", label, "}"
    format.args = list(big.mark = ",", scientific = FALSE),
    label = label)                              %>%
    kable_styling(latex_options = "scale_down") %>%
    row_spec(0, bold  = T)                      %>%
    column_spec(1, bold = T)                    %>%
    column_spec(c(3,5,7,9), border_left = T)    %>%
    column_spec(1:11, bold = T, color = "black", background =   div_conf_colour(tmp$`conf.`)) %>%
    kable_styling(bootstrap_options = bootstr_opts)
    return(x)
}
