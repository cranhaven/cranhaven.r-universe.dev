#' Choices Object
#'
#' Convert a set of preferences to a list of choices, alternatives, and
#' preferences.
#'
#' @param preferences a [`preferences`][preferences] object, or an object that can be
#' coerced by `as.preferences`.
#' @param names logical: if `TRUE` use the object names in the returned
#' `choices` object, else use object indices.
#' @return A data frame of class `choices` with elements:
#' \describe{
#' \item{choices}{A list where each element represents the items chosen for a
#' single rank in the ordering.}
#' \item{alternatives}{A list where each element represents the alternatives
#' (i.e. the set of remaining items to choose from) for a single rank.}
#' \item{ordering}{A list where each element represents the ordering
#' that the choice belongs to.}
#' }
#' The list stores the number of choices and the names of the objects as the
#' attributes `nchoices` and `objects` respectively.
#' @examples
#' R <- matrix(c(
#'   1, 2, 0, 0,
#'   4, 1, 2, 3,
#'   2, 1, 1, 1,
#'   1, 2, 3, 0,
#'   2, 1, 1, 0,
#'   1, 0, 3, 2
#' ), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- preferences(R, format = "ranking")
#'
#' actual_choices <- choices(R, names = TRUE)
#' actual_choices[1:6, ]
#'
#' coded_choices <- choices(R, names = FALSE)
#' coded_choices[1:2, ]
#' as.data.frame(coded_choices)[1:2, ]
#' attr(coded_choices, "objects")
#'
#' @export
choices <- function(preferences, names = FALSE) {
  # check preferences are valid
  if (!inherits(preferences, "preferences")) {
    preferences <- as.preferences(preferences)
  }
  # treat as matrix for faster indexing
  preferences <- unclass(preferences)
  preferences[is.na(preferences)] <- 0L
  n <- ncol(preferences)
  j <- apply(preferences, 1L, max)
  onames <- colnames(preferences)
  opt <- seq_len(n)
  if (names && !is.null(onames)) {
    opt <- onames
  }
  choices <- alternatives <- list()
  ordering <- NULL
  for (i in seq_len(max(j))) {
    ## j-th choices
    cho <- apply(
      (preferences == i)[j >= i, , drop = FALSE], 1L,
      function(z) opt[z]
    )
    if (is.matrix(cho)) {
      cho <- unname(split(cho, col(cho)))
    }
    choices <- c(choices, cho)
    ## j-th alternatives
    alt <- apply(
      (preferences > i - 1L)[j >= i, , drop = FALSE], 1L,
      function(z) opt[z]
    )
    if (is.matrix(alt)) {
      alt <- unname(split(alt, col(alt)))
    }
    alternatives <- c(alternatives, alt)
    ordering <- c(ordering, which(j >= i))
  }
  ii <- order(ordering)
  nchoices <- length(choices)
  out <- data.frame(matrix(NA, nrow = nchoices, ncol = 0L))
  out$choices <- choices[ii]
  out$alternatives <- alternatives[ii]
  out$ordering <- ordering[ii]
  attr(out, "nchoices") <- nchoices
  attr(out, "objects") <- onames
  class(out) <- c("choices", class(out))
  out
  ## Alow weights per choice/alternatives combination?
}

#' @method print choices
#' @export
print.choices <- function(x, ...) {
  preferences <- x$ordering
  for (i in unique(preferences)) {
    cat("Preference Set:", i, "\n")
    cat("-------------- \n")
    ccho <- x$choices[preferences == i]
    calt <- x$alternatives[preferences == i]
    for (j in seq_along(ccho)) {
      ch <- paste0("{", toString(ccho[[j]]), "}")
      al <- paste0("{", toString(calt[[j]]), "}")
      cat(ch, "from", al, "\n")
    }
    cat("============== \n")
  }
}
