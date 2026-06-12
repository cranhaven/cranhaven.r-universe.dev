#' @importFrom stats quantile
completed <- function(nsim) {
  out <- list(
    j = 1,
    pr = floor(quantile(1:nsim)),
    lab = c("0%", "25%", "50%", "75%", "100%"),
    nsim = nsim
  )
  structure(out, class = "completed")
}

nextc <- function(x, i) {
  UseMethod("nextc")
}

nextc.completed <- function(x, i) {
  message(x$lab[x$j], " completed")
  x$j <- x$j + 1
  return(x)
}


#  justify text in reports-------------------------------
justify <- function(string, width = 70, fill = "right") {
  paragraphs <- gsub(
    "^\\s+|\\s+$", "",
    unlist(strsplit(x = string, split = "\n", fixed = TRUE))
  )
  paragraphs <- paragraphs[nchar(paragraphs) > 0]
  formatted_text <- lapply(paragraphs, function(paragraph) {
    strs <- strwrap(paragraph, width = width)
    paste(fill_spaces(strs, width, fill), collapse = "\n")
  })
  paste0(unlist(formatted_text, recursive = FALSE), collapse = "\n")
}

#' @importFrom utils head tail
fill_spaces <- function(lines, width, fill) {
  tokens <- strsplit(lines, "\\s+")
  res <- lapply(head(tokens, -1L), function(x) {
    nspace <- length(x) - 1L
    extra <- width - sum(nchar(x)) - nspace
    reps <- extra %/% nspace
    extra <- extra %% nspace
    times <- rep.int(if (reps > 0) reps + 1L else 1L, nspace)
    if (extra > 0) {
      if (fill == "right") {
        times[1:extra] <- times[1:extra] + 1L
      } else if (fill == "left") {
        times[(nspace - extra + 1L):nspace] <- times[(nspace - extra + 1L):nspace] + 1L
      } else {
        times[inds] <- times[(inds <- sample(nspace, extra))] + 1L
      }
    }
    spaces <- c("", unlist(lapply(times, formatC, x = " ", digits = NULL)))
    paste(c(rbind(spaces, x)), collapse = "")
  })
  c(res, paste(tail(tokens, 1L)[[1]], collapse = " "))
}
