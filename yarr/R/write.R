# yarr, Yet Another ARFF Reader
# Copyright (C) 2019 David Charte & Francisco Charte
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#
# Functions to export a dataset onto an ARFF file
#

#'@title Write a data.frame onto an ARFF file
#'@description Takes a data frame and records it in ARFF (Attribute-Relation
#'  File Format).
#'@param x A data.frame
#'@param relation Name of the dataset (optional, it may be inferred from the
#'  `relation` attribute or the name of the variable passed as argument)
#'@param types A character vector indicating the type of each variable
#'  (optional, may be inferred from the `attributes` attribute or computed from
#'  the class of each variable)
#'@param file Name of the file where the data is to be written. Use `""` to
#'  write to standard output
#'@param sparse Logical: write in sparse format?
#'@param append Logical: append to an existing file?
#'@param ... Extra parameters for internal functions
#'@return Invisibly, the name of the file.
#' @examples
#'
#' library(yarr)
#'\donttest{
#' write.arff(iris, "iris", file = tempfile())
#'}
#'@export
write.arff <- function(x, relation = NULL, types = NULL, file = "", sparse = FALSE, append = FALSE, ...) {
  if (is.null(relation)) {
    relattr <- attr(x, "relation")
    relation <- if (is.null(relattr))
      substitute(x)
    else
      relattr
  }

  if (is.null(types)) {
    types <- compute_types(x)
  }

  arffConnection <- if (file == "") stdout() else base::file(file, open = if (append) "a" else "w")
  on.exit(if (file != "") close(arffConnection))
  export.arff(x, relation, types, sparse, arffConnection, ...)
  invisible(file)
}

compute_types <- function(x) UseMethod("compute_types")

compute_types.arff_data <- function(x) attr.types(x)

compute_types.default <- function(x) {
  types <- vector("character", ncol(x))
  for (i in 1:length(types)) {
    types[i] <- if (is.numeric(x[[i]])) {
      "numeric"
    } else if (length(unique(x[[i]])) < length(x[[i]])) {
        paste0("{", paste(unique(x[[i]]), collapse = ","), "}")
    } else {
      "string"
    }
  }

  names(types) <- colnames(x)
  types
}

export.arff <- function(x, relation, types, sparse, con, ...) {
  writeLines(export.header(relation), con)
  writeLines(export.arff.attributes(types), con)
  export.arff.data(x, sparse, con = con, ...)
}

export.header <- function(relation) {
  paste0("@relation ", relation)
}

export.arff.attributes <- function(types) {
  attr_names <- names(types)
  attr_names <- ifelse(grepl("(\\s|\"|\')", attr_names),
                      paste0("'", gsub(
                        "'", "\\'", attr_names, fixed = T
                      ), "'"),
                      attr_names)
  paste("@attribute",
        attr_names,
        types)
}

export.arff.data <- function(x, sparse, con, header = "@data\n", ...) {
  x[is.na(x)] <- '?'

  cat(header, file = con)
  export.arff.chunks(x, con = con, sparse = sparse, ...)
}


export.dense.arff.data <- function(data) {
  do.call(paste, c(unname(data), list(sep = ',')))
}

export.sparse.arff.data <- function(data) {
  nonzero <- sapply(data, function(col) {
    if (is.numeric(col)) col != 0
    # else if (is.factor(col)) col != levels(col)[1]
    else rep(TRUE, length(col))
  })
  ch_data <- sapply(data, as.character)

  sapply(1:nrow(data), function(i) {
    select <- nonzero[i, ]
    paste0(
      "{",
      paste(
        which(select) - 1,
        ch_data[i, select],
        sep = " ",
        collapse = ","
      ),
      "}"
    )
  })
}

export.arff.chunks <-
  function(data,
           con,
           chunk_size = floor(1e6 / ncol(data)),
           sparse = F,
           fun = if (sparse)
             export.sparse.arff.data
           else
             export.dense.arff.data) {
    num_instances <- dim(data)[1]
    chunks <- floor((num_instances - 1) / chunk_size)

    finished <- FALSE
    ch <- 0

    while (!finished) {
      start <- 1 + ch * chunk_size
      end <- (ch + 1) * chunk_size
      end <- if (end < num_instances) {
        end
      } else {
        finished <- TRUE
        num_instances
      }
      chunk <- data[start:end, ]

      writeLines(fun(chunk), con)
      ch <- ch + 1
    }
  }
