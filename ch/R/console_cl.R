#' @title  Remove '>' and '+' from the console and
#' add '#' to the run result.
#' @author  Chai
#' @importFrom clipr read_clip
#' @importFrom  magrittr %>%
#' @importFrom crayon bold blue
#' @description First you need to copy the console area to the clipboard,
#' then run the console_cl() function to add a comment to the line
#' where the output is, and to cancel the > on the original line.
#' Finally, the result of the run is saved to the clipboard.
#' @param  prefix  The prefix for code.The default is '#>'.
#' You can edit it according to your own preference, but 
#'  '#' should be  the first character.
#' @return  the result of the run is saved to the clipboard.
#' @export
console_cl <- function(prefix = "#>") {
  str_a <- function(x, pref = pref) {
    x <- trimws(x)
    pre_len <- nchar(pref)
    x1 <- substr(x, 1, 1)
    x2 <- substr(x, 1, pre_len)
    if (x1 == ">" | x1 == "+") {
      x <- substring(x, 2)
    } else if (x2 == pref) {
      x <- paste0(" ", x, "\n")
    } else {
      x <- paste0(" ", pref, x, "\n")
    }
    return(x)
  }
  a <- clipr::read_clip()
  if (is.character(a) == T) {
    cat(bold("text from the clipboard:\n"))
    blue(a) %>% cat("\n")
    pre_1 <- substr(prefix, 1, 1)
    if (pre_1 == "#") {
      pref <- prefix
    } else {
      pref <- paste0("#", prefix)
      message(yellow(paste0("prefix:", pref, "\n")))
    }
    sapply(a, str_a, pref = pref) %>%
      cat(file = "clipboard")
    cat(bold("Output -> clipboard,please check it."))
  }
  else {
    return("Clipboard is wrong!")
  }
}
