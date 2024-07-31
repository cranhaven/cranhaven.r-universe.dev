#' Parse R-Package Dataset Documentation
#'
#' @description Reads and parses R documentation (Rd) files.
#'
#' @param ...
#'   Arguments to be passed to the [`tools::Rd_db`] function,
#'   such as `package`, a character string naming an installed package.
#' @param doc_type 'character' vector.
#'   Document type, such as `data` for dataset objects.
#'
#' @return
#'   A named list containing the documentation sections as character strings.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' l <- parse_rd_db(package = "inldata")
#' str(l, 1)

parse_rd_db <- function(..., doc_type = "data") {

  # check arguments
  checkmate::assert_character(doc_type,
    any.missing = FALSE,
    min.len = 1,
    null.ok = TRUE
  )

  # get list of Rd objects
  db <- tools::Rd_db(...)

  # filter on document type
  if (!is.null(doc_type)) {
    doc_types <- get_rd_doc_types(db)
    checkmate::assert_subset(doc_type, choices = doc_types)
    idxs <- doc_types %in% doc_type |> which()
    db <- db[idxs]
  }

  # remove .Rd extension from list names
  names(db) <- names(db) |> tools::file_path_sans_ext()

  # get text for each Rd object
  lapply(db, parse_rd)
}


# Function to get Rd document types
get_rd_doc_types <- function(db) {
  checkmate::assert_list(db, types = "Rd", names = "named")
  vapply(db, function(rd) {
    doc_type <- attr(rd, "meta")$docType
    if (length(doc_type) == 0) character(1) else doc_type
  }, character(1))
}


# Function to parse Rd object
parse_rd <- function(rd) {

  # check arguments
  checkmate::assert_class(rd, classes = "Rd")
  tools::checkRd(rd, unknownOK = FALSE) # |> print(minlevel = -1)

  # initialize output list
  out <- list(
    "arguments_table" = get_rd_sect_table(rd, section = "arguments"),
    "author" = get_rd_sect_text(rd, section = "author"),
    "description" = get_rd_sect_text(rd, section = "description"),
    "details" = get_rd_sect_text(rd, section = "details"),
    "format" = get_rd_sect_text(rd, section = "format"),
    "format_table" = get_rd_sect_table(rd, section = "format"),
    "name" = get_rd_sect_text(rd, section = "name"),
    "note" = get_rd_sect_text(rd, section = "note"),
    "references" = get_rd_sect_text(rd, section = "references"),
    "source" = get_rd_sect_text(rd, section = "source"),
    "title" = get_rd_sect_text(rd, section = "title"),
    "value" = tryCatch(
      get_rd_sect_text(rd, section = "value"),
      error = function(e) NULL
    ),
    "value_table" = get_rd_sect_table(rd, section = "value")
  )

  # remove null values
  Filter(Negate(is.null), out)
}


# Function to find tags in an Rd object,
# Code adapted from the tools:::.Rd_find_nodes_with_tags function in R 4.3,
# released under the "GNU General Public License" version 2 license.
find_rd_tags <- function(rd, tags) {
  if (length(rd) == 0) return(list())
  checkmate::assert_class(rd, classes = "Rd")
  checkmate::assert_character(tags, any.missing = FALSE, min.len = 1)
  nodes <- list()
  recurse <- function(x) {
    is <- attr(x, "Rd_tag") == tags
    if (any(is)) {
      nodes <<- c(nodes, list(x))
    }
    if (is.list(x)) {
      lapply(x, recurse)
    }
  }
  lapply(rd, recurse)
  nodes
}


# Function to get Rd tags
get_rd_tags <- function(rd) {
  checkmate::assert_class(rd, classes = "Rd")
  tags <- lapply(rd, attr, "Rd_tag")
  if (length(tags) > 0) {
    simplify2array(tags, FALSE)
  } else {
    character(0)
  }
}


# Function to get Rd object for section
get_rd_sect <- function(rd, section) {
  checkmate::assert_class(rd, classes = "Rd")
  checkmate::assert_string(section)
  is <- get_rd_tags(rd) %in% paste0("\\", section)
  rd <- rd[is]
  if (length(rd) > 0) {
    rd <- structure(rd[[1]], class = "Rd")
  }
  rd
}


# Function to get table from section of Rd object
get_rd_sect_table <- function(rd, section) {
  checkmate::assert_class(rd, classes = "Rd")
  checkmate::assert_string(section)
  item_tags <- get_rd_sect(rd, section = section) |>
    find_rd_tags(tags = "\\item")
  len <- vapply(item_tags, length, integer(1))
  item_tags <- item_tags[len > 0]
  if (length(item_tags) == 0) {
    return(NULL)
  }
  l <- lapply(item_tags, function(x) unlist(parse_item_tag(x)))
  do.call(rbind, l) |> as.data.frame()
}


# Function to get section text
get_rd_sect_text <- function(...) {
  get_rd_sect(...) |> rd2text()
}


# Function to convert an Rd object to text
rd2text <- function(rd) {
  if (length(rd) == 0) {
    return(NULL)
  }
  checkmate::assert_class(rd, classes = "Rd")
  txt <- NULL
  op <- options(useFancyQuotes = FALSE)
  on.exit(options(op))
  out <- textConnection(
    object = "txt",
    open = "w",
    local = TRUE
  )
  on.exit(close(out), add = TRUE)
  tools::Rd2txt_options(
    width = 10000000,
    showURLs = TRUE,
    code_quote = TRUE,
    underline_titles = FALSE
  )
  tools::Rd2txt(rd,
    out = out,
    outputEncoding = "UTF-8",
    fragment = TRUE
  )
  txt[nzchar(txt)] |> trimws() |> paste(collapse = "\n\n")
}


# Function to parse an item tag
parse_item_tag <- function(item) {
  checkmate::assert_list(item, min.len = 1)
  class(item[[1]]) <- "Rd"
  class(item[[2]]) <- "Rd"
  name <- item[[1]] |> rd2text()
  name <- gsub(
    pattern = "(^')|('$)",
    replacement = "",
    x = name
  )
  value <- item[[2]] |> rd2text()
  list("name" = name, "value" = value)
}
