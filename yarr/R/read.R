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
# Functions to read ARFF files in different formats (sparse and dense)
#

#'@title Read an ARFF file
#'@description Reads a dataset from an ARFF file, parsing each section and
#'  converting the data section into a `data.frame`.
#'@param file Name of the file to read the data from
#'@param stringsAsFactors Logical: should string attributes be converted to
#'  factors? (nominal attributes are always converted to factors)
#'@return A `data.frame` with some attributes:
#'
#'  - attributes: a named vector indicating the type of each variable
#'  - relation: the original `@relation` of the dataset
#'
#'Use `attr.names()`, `attr.types()` and `relation()` to consult attribute
#'names, types and the name of the dataset, respectively.
#'
#'@examples
#'
#' library(yarr)
#'\donttest{
#' yeast <- read.arff("yeast.arff")
#'}
#'@export
# character -> logical -> data.frame
read.arff <- function(file, stringsAsFactors = FALSE) {
  contents <- fix_types(read_arff_internal(file), stringsAsFactors)

  structure(contents,
            class = c("arff_data", class(contents)))
}

# data.frame -> logical -> data.frame
fix_types <- function(contents, stringsAsFactors) {
  types <- attr(contents, "attributes")
  contents[contents == "?"] <- NA

  for (i in 1:length(types)) {
    if (types[i] %in% c("numeric", "integer", "real")) {
      contents[[i]] <- as.numeric(contents[[i]])
    } else if (grepl("^\\s*\\{", types[i])) {
      contents[[i]] <- factor(contents[[i]], levels = read_factor_levels(types[i])[[1]])
    } else if (stringsAsFactors) {
      contents[[i]] <- factor(contents[[i]])
    } # else, contents[[i]] is already a character vector
  }

  contents
}

# Extracts all useful data from an ARFF file in an
# R object
#
# @param arff_file Path to the file
# @return data.frame with "variables" and "relation" attributes
# character -> ... -> data.frame
read_arff_internal <- function(arff_file, ...) {
  file_con <- file(arff_file, "rb")

  if (!isOpen(file_con)) {
    open(file_con, "rb")
  }

  # Read whole file
  file_data <- strsplit(readChar(file_con, nchars = file.info(arff_file)$size, useBytes = TRUE),
                        "\\\r\\\n|\\\r|\\\n", fixed = FALSE, useBytes = TRUE)[[1]]

  close(file_con)

  # Split into relation, attributes and data
  relation_at <- grep("@relation", file_data, ignore.case = TRUE)
  data_start <- grep("@data", file_data, ignore.case = TRUE)

  if (is.na(relation_at)) stop("Missing @relation or not unique.")
  if (is.na(data_start)) stop("Missing @data mark or not unique.")

  relation <- read_header(file_data[relation_at])

  # Get attribute vector
  attributes <- parse_attributes(file_data[(relation_at + 1):(data_start - 1)])
  num_attrs <- length(attributes)

  if (any(grepl("date", attributes))) warning("Date attributes will be read as strings")
  if (any(grepl("relational", attributes))) stop("Relational attributes not supported at the moment")

  # Ignore blank lines and comments before data
  data_start <- data_start + 1
  rawdata <- file_data[data_start:length(file_data)]
  empty <- grep("^\\s*(%(.*?))?$", rawdata)
  rawdata <- if (length(empty) > 0) rawdata[-empty] else rawdata

  # Build character matrix with @data section
  dataset <- if (detect_sparsity(rawdata)) {
    parse_sparse_data(rawdata, defaults = sparse_defaults(attributes), ...)
  } else {
    parse_nonsparse_data(rawdata, num_attrs, ...)
  }

  rm(rawdata)

  dataset <- as.data.frame(dataset, stringsAsFactors = FALSE)
  colnames(dataset) <- names(attributes)
  rownames(dataset) <- NULL

  return(structure(dataset,
                   relation = relation,
                   attributes = attributes))
}

# Reads the attributes section of an ARFF file
#
# @param arff_attrs Lines containing the attributes
# @return A vector containing, for each
#  attribute, its name and its type
# character[] -> named character[]
parse_attributes <- function(arff_attrs) {
  # Extract attribute definitions

  #-----------------------------------------------------------------------------------------------------
  # Finding adequate spaces to split the attribute definition into 3 parts:
  #    @attribute attr_name {0, 1}   -> c("@attribute", "attr_name", "{0, 1}")
  #    @attribute 'Attr. name' {0,1} -> c("@attribute", "'Attr. name'", "{0,1}")
  #    @attribute 'David\'s attribute' {0,1} -> c("@attribute", "'David\'s attribute'", "{0,1}")
  #-----------------------------------------------------------------------------------------------------
  # Using the technique described under "Perl/PCRE Variation" in this StackOverflow answer:
  #    (Regex Pattern to Match, Excluding when...) http://stackoverflow.com/a/23589204/5306389
  # We capture any spacing character ignoring those within braces or single quotes,
  # allowing the appearance of escaped single quotes (\').
  #-----------------------------------------------------------------------------------------------------
  # Regex tested in https://regex101.com/r/tE5mP1/20
  #-----------------------------------------------------------------------------------------------------

  rgx <- "(?:{(?:.*?)}\\s*$|(?<!\\\\)'[^'\\\\]*(?:.*?)(?<!\\\\)'|(?<!\\\\)\"(?:.*?)(?<!\\\\)\"|(\\s*?)@)(*SKIP)(*F)|\\s+"
  att_list <- strsplit(arff_attrs, rgx, perl = TRUE)

  # Structure by rows
  att_mat <-
    matrix(unlist(att_list[sapply(att_list, function(row) {
      length(row) == 3
    })]),
    ncol = 3, byrow = T)
  rm(att_list)
  # Filter any data that is not an attribute
  att_mat <- att_mat[grepl("^\\s*@attribute", att_mat[, 1], ignore.case = TRUE), 2:3, drop = FALSE]
  att_mat <- gsub("^'(.*?)'$", "\\1", att_mat, perl = T)
  att_mat <- gsub('^"(.*?)"$', "\\1", att_mat, perl = T)
  att_mat[, 1] <- gsub("\\'", "'", att_mat[, 1], fixed = T)
  att_mat[, 1] <- gsub('\\"', '"', att_mat[, 1], fixed = T)

  # Create the named vector
  att_v <- att_mat[, 2, drop = TRUE]
  names(att_v) <- att_mat[, 1, drop = TRUE]

  rm(att_mat)
  return(att_v)
}

# Reads the name and potential Meka parameters in the header of an
# ARFF file
#
# @param arff_relation "relation" line of the ARFF file
# @return Number of labels in the dataset
# character -> character
read_header <- function(arff_relation) {
  rgx <- regexpr("[\\w\\-\\._]+\\s*:\\s*-[Cc]\\s*-?\\d+", arff_relation, perl = TRUE)
  hdr <- strsplit(regmatches(arff_relation, rgx), "\\s*:\\s*-[Cc]\\s*")

  if (length(hdr) > 0) {
    # Meka header
    structure(
      hdr[[1]][1],
      c = as.numeric(hdr[[1]][2])
    )
  } else {
    # Normal header, unquoted or quoted (these should not match at the same
    # time)
    c(regmatches(
      arff_relation,
      regexpr(
        "(?<=\\s)([^\\s'\"]+?)(?=\\s*$)",
        arff_relation,
        perl = TRUE
      )
    ),
    regmatches(
      arff_relation,
      regexpr(
        "(?<=\\s')(([^']|\\\\')+?)(?='\\s*$)",
        arff_relation,
        perl = TRUE
      )
    ),
    regmatches(
      arff_relation,
      regexpr(
        "(?<=\\s\")(([^\"]|\\\\\")+?)(?=\"\\s*$)",
        arff_relation,
        perl = TRUE
      )
    ))[1] # ensure scalar
  }
}

# Detects whether an ARFF file is in sparse format
#
# @param arff_data Content of the data section
# @return Boolean, TRUE when the file is sparse
# character[] -> logical
detect_sparsity <- function(arff_data) {
  grepl("^\\s*\\{", arff_data[1])
}

# Builds a data.frame out of non-sparse ARFF data
#
# @param arff_data Content of the data section
# @return character matrix containing data values
# character -> integer -> character[,]
parse_nonsparse_data <- function(arff_data, num_attrs) {
  matrix(
    unlist(strsplit(arff_data, ",", fixed = T)),
    ncol = num_attrs,
    byrow = T
  )
}

# Builds a data.frame out of sparse ARFF data
#
# @param arff_data Content of the data section
# @return character matrix containing data values
# character -> character[] -> character[,]
parse_sparse_data <- function(arff_data, defaults) {
  # Extract data items
  arff_data <- strsplit(gsub("\\s*[\\{\\}]\\s*", "", arff_data), "\\s*,\\s*")

  dataset <- vapply(arff_data, function(item) {
    row <- unlist(strsplit(item, "\\s+"))

    # Build complete row with data
    complete <- defaults
    complete[as.integer(row[c(T, F)]) + 1] <- row[c(F, T)]
    complete
  }, defaults)

  matrix(dataset, ncol = length(defaults), byrow = T)
}

# The default value for a sparse variable is:
#  - 0, if the attribute is numeric
#  - The first value of the factor, if the attribute is categorical
#  - ""? if the type is string (the dataset was probably badly exported)
# character[] -> character[]
sparse_defaults <- function(attributes) {
  defaults <- vector(mode = "character", length = length(attributes))

  # Detect factors, extract values
  factors <- which(grepl("^\\s*\\{", attributes))
  values <- read_factor_levels(attributes[factors])
  defaults[factors] <- sapply(values, function(v) v[1])

  numeric <- which(attributes %in% c("numeric", "integer", "real"))
  defaults[numeric] <- "0" # will be converted to numeric later

  strings <- setdiff(1:length(attributes), union(factors, numeric))
  defaults[strings] <- ""

  defaults
}

# character -> character[]
read_factor_levels <- function(definition) {
  strsplit(gsub("\\s*[\\{\\}]\\s*", "", definition), "\\s*,\\s*")
}
