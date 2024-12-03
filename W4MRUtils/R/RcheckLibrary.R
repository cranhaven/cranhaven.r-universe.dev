# @title R check Tools
#
# @name R Check Tools
#
# @author M.Petera,
# @description
# R functions to use in R scripts
# (management of various generic subroutines)
#
# V0: script structure + first functions
# V1: More detailed error messages in match functions

#' @title Check Errors
#'
#' @description check_err
#' Generic function stop in error if problems have been encountered
#'
#' @param err_stock vector of results returned by check functions
#' @return \code{NULL}
#'
#' @author M.Petera
#' @export
check_err <- function(err_stock) {
  if (length(err_stock) != 0) {
    stop("\n- - - - - - - - -\n", err_stock, "\n- - - - - - - - -\n")
  }
}

#' @title Table match check functions
#'
#' @description match2
#' To check if data_matrix and (variable or sample)metadata match regarding
#'  identifiers
#'
#' @param data_matrix data.frame containing data_matrix
#' @param metadata data.frame containing sample_metadata or variable_metadata
#' @param metadata_type "sample" or "variable" depending on metadata content
#' @return \code{character} vector a list of errors encountered
#' @author M.Petera
#' @export
match2 <- function(data_matrix, metadata, metadata_type) {

  # error vector
  err_stock <- NULL

  id2 <- metadata[, 1]
  if (metadata_type == "sample") {
    id1 <- colnames(data_matrix)[-1]
  }
  if (metadata_type == "variable") {
    id1 <- data_matrix[, 1]
  }

  missing_from_id1 <- id1[!(id1 %in% id2)]
  missing_from_id2 <- id2[!(id2 %in% id1)]
  if (length(missing_from_id1) != 0 || length(missing_from_id2) != 0) {
    err_stock <- c(
      "\nData
      matrix and ",
      metadata_type,
      " metadata do not
       match regarding ",
      metadata_type,
      " identifiers."
    )
    if (length(missing_from_id1) != 0) {
      if (length(missing_from_id1) < 4) {
        err_stock <- c(err_stock, "\n    The ")
      } else {
        err_stock <- c(err_stock, "\n    For example, the ")
      }
      err_stock <- c(
        err_stock, "following identifiers found in the data matrix\n",
        "    do not appear in the ", metadata_type, " metadata file:\n"
      )
      missing_from_id1 <- missing_from_id1[
        seq_len(min(3, length(missing_from_id1)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id1, collapse = "\n    "),
        "\n"
      )
    }
    if (length(missing_from_id2)) {
      if (length(missing_from_id2) < 4) {
        err_stock <- c(err_stock, "\n    The ")
      } else {
        err_stock <- c(err_stock, "\n    For example, the ")
      }
      err_stock <- c(
        err_stock,
        "following identifiers found in the ",
        metadata_type,
        " metadata file\n",
        "    do not appear in the data matrix:\n"
      )
      missing_from_id2 <- missing_from_id2[
        seq_len(min(3, length(missing_from_id2)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id2, collapse = "\n    "),
        "\n"
      )
    }
    err_stock <- c(err_stock, "\nPlease check your data.\n")
  }

  return(err_stock)
}


#' @title match3
#'
#' @description match3
#' To check if the 3 standard tables match regarding identifiers
#'
#' @param data_matrix data.frame containing data_matrix
#' @param sample_metadata data.frame containing sample_metadata
#' @param variable_metadata data.frame containing variable_metadata
#' @return \code{character} vector a list of errors encountered
#'
#' @author M.Petera
#' @export
match3 <- function(data_matrix, sample_metadata, variable_metadata) {

  err_stock <- NULL

  id1 <- colnames(data_matrix)[-1]
  id2 <- sample_metadata[, 1]
  id3 <- data_matrix[, 1]
  id4 <- variable_metadata[, 1]

  missing_from_id1 <- id1[!(id1 %in% id2)]
  missing_from_id2 <- id2[!(id2 %in% id1)]
  missing_from_id3 <- id3[!(id3 %in% id4)]
  missing_from_id4 <- id4[!(id4 %in% id3)]

  if (length(c(missing_from_id1, missing_from_id2)) != 0) {
    err_stock <- c(
      err_stock,
      paste(
        "\nData matrix and sample metadata do not match regarding",
        "sample identifiers."
      )
    )
    if (length(missing_from_id1) != 0) {
      err_stock <- c(
        err_stock, (
          if (length(missing_from_id1) < 4) "\n    The "
          else "\n    For example, the "
        ),
        "following identifiers found in the data matrix\n",
        "    do not appear in the sample metadata file:\n"
      )
      missing_from_id1 <- missing_from_id1[
        seq_len(min(3, length(missing_from_id1)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id1, collapse = "\n    "),
        "\n"
      )
    }
    if (length(missing_from_id2) != 0) {
      err_stock <- c(
        err_stock, (
          if (length(missing_from_id2) < 4) "\n    The "
          else "\n    For example, the "
        ),
        "following identifiers found in the sample metadata file\n",
        "    do not appear in the data matrix:\n"
      )
      missing_from_id2 <- missing_from_id2[
        seq_len(min(3, length(missing_from_id2)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id2, collapse = "\n    "),
        "\n"
      )
    }
  }

  if (length(c(missing_from_id3, missing_from_id4)) != 0) {
    err_stock <- c(
      err_stock,
      paste(
        "\nData matrix and variable metadata do not match regarding",
        "variable identifiers."
      )
    )
    if (length(missing_from_id3) != 0) {
      err_stock <- c(
        err_stock, (
          if (length(missing_from_id3) < 4) "\n    The "
          else "\n    For example, the "
        ),
        "following identifiers found in the data matrix\n",
        "    do not appear in the variable metadata file:\n"
      )
      missing_from_id3 <- missing_from_id3[
        seq_len(min(3, length(missing_from_id3)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id3, collapse = "\n    "),
        "\n"
      )
    }
    if (length(missing_from_id4) != 0) {
      err_stock <- c(
        err_stock, (
          if (length(missing_from_id4) < 4) "\n    The "
          else "\n    For example, the "
        ),
        "following identifiers found in the variable metadata file\n",
        "    do not appear in the data matrix:\n"
      )
      missing_from_id4 <- missing_from_id4[
        seq_len(min(3, length(missing_from_id4)))
      ]
      err_stock <- c(
        err_stock,
        "    ",
        paste(missing_from_id4, collapse = "\n    "),
        "\n"
      )
    }
  }

  if (length(err_stock) != 0) {
    err_stock <- c(err_stock, "\nPlease check your data.\n")
  }

  return(err_stock)
}
