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

#' @title Dataset utilities
#'
#' @description Some tools to access the metadata in ARFF files. `attr.names`
#'   retrieves the names of the attributes, `attr.types` returns the ARFF type
#'   of each variable, `relation` shows the name/relation specified in the
#'   `@relation` section of the file.
#'
#' @param x A dataset read using `read.arff`
#' @rdname utils
#' @export
attr.names <- function(x) {
  colnames(x)
}

#' @rdname utils
#' @export
attr.types <- function(x) {
  attr(x, "attributes")
}

#' @rdname utils
#' @export
relation <- function(x) {
  attr(x, "relation")
}

#' @title Display functions
#' @param x A data.frame read from an ARFF file
#' @param max_attrs Maximum of attributes to be inspected
#' @param max_values Maximum of values to be shown for each attribute
#' @param ... Extra parameters for the corresponding S3 method for class
#'   `data.frame`
#' @rdname display
#' @export
print.arff_data <- function(x, max_attrs = 10, max_values = 5, ...) {
  typ <- attr.types(x)

  cat("An ARFF dataset:", relation(x), "\n")
  cat(length(typ), "attributes and", nrow(x), "instances\n")

  sep <- "  ...\n"
  if (length(typ) <= max_attrs) {
    sep <- ""
    max_attrs <- length(typ)
  }

  etc <- "..."
  if (nrow(x) <= max_values) {
    max_values <- nrow(x)
    etc <- ""
  }

  summ <- function(variable) {
    if (is.numeric(variable)) {
      s <- summary(variable)
      paste0("(min = ", s["Min."], ", mean = ", s["Mean"], ", max = ", s["Max."], ")")
    } else {
      t <- table(variable)
      mx <- min(length(t), 3)
      etc <- if (length(t) > 3) "..." else ""

      paste0("(", paste0(names(t)[1:mx], ": ", t[1:mx], collapse = ", "), etc, ")")
    }
  }

  vals <- function(variable) {
    if (max_values < 1) "" else paste0(paste0(variable[1:max_values], collapse = ", "), etc)
  }

  range <- if (max_attrs > 1) 1:(max_attrs - 1) else 1
  for (i in range) {
    cat("  ", names(typ)[i], ": ", typ[i], " ", summ(x[[i]]), " ", vals(x[[i]]), "\n", sep = "")
  }
  if (max_attrs > 2) {
    cat(sep, "  ", names(typ)[length(typ)], ": ", typ[length(typ)], " ", summ(x[[length(typ)]]), " ", vals(x[[length(typ)]]), "\n", sep = "")
  }
}
