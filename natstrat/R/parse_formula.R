#' Parse the nonstandard balance formulas
#'
#' This function takes nonstandard formulas as inputs and returns regular formulas
#' as well as lists of weights to be used in \code{\link{generate_constraints}()}.
#'
#' @param balance_formula a formula that may have multiple terms on the left hand side,
#' "." on the left hand side, and coefficients, making it not a standard formula.
#' @param data a data frame containing the relevant covariates in the columns. The number
#' of rows should equal the length of \code{treated}.
#' @param default_rhs the list of \code{balance_formulas} can also contain entries
#' that are just the character corresponding to a covariate to balance. If so,
#' the covariate will be balanced according to \code{default_rhs}.
#' @return A named list containing \code{new_formula}: the standard formula to be used
#' in \code{\link{generate_constraints}}, \code{rhs_weights}: a named list with the
#' coefficients of the terms on the right hand side, and \code{lhs_weights}:
#' a named list with coefficients of the terms on the left hand side.
#'
#' @importFrom stats as.formula
#' @importFrom utils type.convert combn
#' @keywords internal


parse_formula <- function(balance_formula, default_rhs = NULL, data = NULL) {
  if (!plyr::is.formula(balance_formula)) {
    if (is.null(default_rhs)) {
      stop("If any entry of \"balance_formulas\" is not a formula, \"default_rhs\" must be provided.",
           call. = FALSE)
    }
    balance_formula <- as.formula(paste0(balance_formula, " ~ ", default_rhs))
  }

  rhs <- paste(deparse(balance_formula[[3]]), collapse=' ')
  rhs <- strsplit(rhs, split = " + ", fixed = TRUE)[[1]]
  rhs_weights <- sapply(rhs, function(x) strsplit(x, " ")[[1]][1])
  rhs_weights <- lapply(as.list(rhs_weights), type.convert, as.is = TRUE)
  rhs_weights[!sapply(rhs_weights, is.numeric)] <- 1
  minus_terms <- NULL
  for(i in 1:length(rhs_weights)) {
    name_split <- unlist(strsplit(names(rhs_weights)[i], split = " * ", fixed = TRUE))
    if (is.numeric(type.convert(name_split[1], as.is = FALSE))) {
      if (length(name_split) == 1) {
        names(rhs_weights)[i] <- "1"
      } else if (length(name_split) == 2) {
        split_minus <- unlist(strsplit(name_split[2], split = " - "))
        names(rhs_weights)[i] <- split_minus[1]
        if (length(split_minus) > 1) {
          minus_terms <- c(minus_terms, split_minus[2:length(split_minus)])
        }
      } else {
        names(rhs_weights)[i] <- paste(name_split[-1], collapse = " * ")
      }
    }
  }

  for(i in 1:length(rhs_weights)) {
    name_split <- unlist(strsplit(names(rhs_weights)[i], split = " * ", fixed = TRUE))
    if (length(name_split) > 1) {
      for (m in 1:(length(name_split) - 1)) {
        combs <- combn(name_split, m, simplify = FALSE)
        for (comb in combs) {
          new_term <- paste(comb, collapse = ":")
          rhs_weights[new_term] <- rhs_weights[i]
        }
      }
      names(rhs_weights)[i] <- paste(name_split, collapse = ":")
    }
  }

  if (length(rhs_weights[names(rhs_weights) != '1']) > 0) {
    new_rhs <- paste0("`", names(rhs_weights)[names(rhs_weights) != '1'], "`", collapse = " + ")
    if ('1' %in% names(rhs_weights)) {
      new_rhs <- paste0("1 + ", new_rhs)
    }
  } else {
    new_rhs <- "1"
  }


  if (!is.null(minus_terms)) {
    new_rhs <- paste(new_rhs, " - ", paste(minus_terms, collapse = " - "))
  }
  rhs_weights <- unlist(rhs_weights)

  lhs <- paste(deparse(balance_formula[[2]]), collapse=' ')
  lhs <- strsplit(lhs, split = " + ", fixed = TRUE)[[1]]
  minus_terms <- NULL

  lhs_split <- strsplit(lhs, split = " * ", fixed = TRUE)
  lhs_weights <- NULL
  for (lhs_term in lhs_split) {
    if (length(lhs_term) > 1) {
      split_minus <- unlist(strsplit(lhs_term[2], split = " - ", fixed = TRUE))
      if (length(split_minus) > 1) {
        minus_terms <- c(minus_terms, split_minus[2:length(split_minus)])
        lhs_weights[split_minus[1]] <- as.numeric(lhs_term[1])
      } else {
        lhs_weights[lhs_term[2]] <- as.numeric(lhs_term[1])
      }
    } else {
      split_minus <- unlist(strsplit(lhs_term[1], split = " - ", fixed = TRUE))
      if (length(split_minus) > 1) {
        minus_terms <- c(minus_terms, split_minus[2:length(split_minus)])
        lhs_weights[split_minus[1]] <- 1
      } else {
        lhs_weights[lhs_term[1]] <- 1
      }
    }
  }

  if ("." %in% names(lhs_weights)) {
    for (cov in colnames(data)) {
      if (!cov %in% minus_terms) {
        lhs_weights[cov] <- lhs_weights["."]
      }
    }
    lhs_weights <- lhs_weights[names(lhs_weights) != "."]
  }

  new_lhs <- paste0("`", names(lhs_weights), "`", collapse = " + ")

  new_formula <- as.formula(paste(new_lhs, new_rhs, sep = " ~ "))

  return(list(new_formula = new_formula, rhs_weights = rhs_weights,
              lhs_weights = lhs_weights))
}
