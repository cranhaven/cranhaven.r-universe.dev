#' Progress Bar
#' @description Print a customisable progress bar in the console.
#' @param p Numeric, percentage of finished progress, between 0 and 1.
#' @param txt1 String to put before the progress bar
#' @param txt2 String to put after the progress bar
#' @param style The display style. 1 is single-lined; 2 is double-lined; 3 display the progress in a 5-lined block.
#' @param label Character labels for "finished", "un-finished", and "side bars".
#' @return No return value, called for side effects.
#' @keywords internal
myMiniProgressBar <- function(p, txt1 = '', txt2 = '', style = 1, label = c('=', '-', '|')) {
  stopifnot(style %in% c(1, 2, 3))
  label <- as.character(label)
  stopifnot(length(label) == 3)
  stopifnot(all(nchar(label) <= 1))
  width <- options('width')[[1]] + 3
  txt1 <- as.character(txt1)
  txt2 <- as.character(txt2)
  finished <- label[1]
  unfinished <- label[2]
  bar <- label[3]
  n1 <- nchar(txt1)
  n2 <- nchar(txt2)
  
  if (style == 1) {
    n3 <- ((n1 + 1) %/% width + 1) * width - n1 - n2 - 3
  } else if (style == 2) {
    txt2 <- stringr::str_pad(txt2, ((n2 + 1) %/% width + 1) * width - 2, "right")
    n3 <- ((n1 + 1) %/% width + 1) * width - n1 - 2
  } else { # style == 3
    txt1 <- stringr::str_pad(txt1, ((n1 + 1) %/% width + 1) * width - 2, "right")
    txt2 <- stringr::str_pad(txt2, ((n2 + 1) %/% width + 1) * width - 2, "right")
    n3 <- width * 5
  }
  if (n3 <= 4) {
    stop('not enough line width')
  }
  if (p < 0) p <- 0
  if (p > 1) p <- 1
  done <- round((n3 - 2 * nchar(bar)) * p)
  progress <- paste0(bar, strrep(finished, done),
                     strrep(unfinished, n3 - 2 * nchar(bar) - done), bar)
  
  if (style == 3) {
    cat('\r', txt1, progress, txt2)
  } else {
    cat('\r', txt1, progress, txt2)
  }
  invisible(NULL)
}

# x in second
myTimeStr <- function(x) {
  x <- round(as.numeric(x))
  y <- c(x %/% 3600, (x %% 3600) %/% 60, (x %% 3600) %% 60)
  if (y[1] >= 1) {
    paste0(y[1:2], c('h', 'm'), collapse = ' ')
  } else if (y[2] >= 1) {
    paste0(y[2:3], c('m', 's'), collapse = ' ')
  } else {
    paste0(y[3], 's')
  }
}
