#' Read Ordinal Preference Data From PrefLib
#'
#' Read orderings from `.soc`, `.soi`, `.toc` or `.toi` files storing
#' ordinal preference data format as defined by
#' \href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}
#' into a `preferences` object.
#'
#' Note that PrefLib refers to the items being ordered by "alternatives".
#'
#' The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#'
#' The numerically coded orderings and their frequencies are read into a
#' data frame, storing the item names as an attribute. The
#' `as.aggregated_preferences` method converts these to an
#' [`aggregated_preferences`][aggregate.preferences] object with the
#' items labelled by name.
#'
#' A PrefLib file may be corrupt, in the sense that the ordered alternatives do
#' not match their names. In this case, the file can be read in as a data
#' frame (with a warning), but `as.aggregated_preferences` will throw an error.
#'
#' @return An [`aggregated_preferences`][aggregate.preferences] object
#' containing the PrefLib data.
#'
#' @param file A preferential data file, conventionally with extension `.soc`,
#' `.soi`, `.toc` or `.toi` according to data type.
#' @param from_preflib A logical which, when `TRUE` will attempt to source
#' the file from PrefLib by adding the database `HTTP` prefix.
#' @param preflib_url The URL which will be preprended to `file`, if
#' `from_preflib` is `TRUE`.
#' @note The Netflix and cities datasets used in the examples are from
#' Caragiannis et al (2017) and Bennet and Lanning (2007) respectively. These
#' data sets require a citation for re-use.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' Bennett, J. and Lanning, S. (2007) The Netflix Prize.
#' \emph{Proceedings of The KDD Cup and Workshops}.
#'
#' @examples
#'
#' # Can take a little while depending on speed of internet connection
#'
#' \donttest{
#' # strict complete orderings of four films on Netflix
#' netflix <- read_preflib("netflix/00004-00000138.soc", from_preflib = TRUE)
#' head(netflix)
#' names(netflix$preferences)
#'
#' # strict incomplete orderings of 6 random cities from 36 in total
#' cities <- read_preflib("cities/00034-00000001.soi", from_preflib = TRUE)
#' }
#' @importFrom utils read.csv
#' @export
read_preflib <- function(file,
                         from_preflib = FALSE,
                         preflib_url = "https://www.preflib.org/static/data") {
  if (from_preflib) {
    file <- file.path(preflib_url, file)
  }
  # Read the file into memory
  lines <- readLines(file)

  # Filter the header lines and convert them to key:value attributes
  header_lines <- sub("# ", "", grep("^#", lines, value = TRUE))
  metadata <- regmatches(
    header_lines,
    regexpr(": ", header_lines),
    invert = TRUE
  )
  preflib_attributes <- lapply(metadata, function(x) x[2L])
  names(preflib_attributes) <- sapply(metadata, function(x) x[1L])

  # Assert that the minimum required attributes are present
  required_attributes <- c(
    "NUMBER UNIQUE ORDERS",
    "NUMBER VOTERS",
    "NUMBER ALTERNATIVES"
  )

  if (suppressWarnings(
    anyNA(as.integer(unlist(preflib_attributes[required_attributes])))
  )) {
    stop(
      "PrefLib datafile is corrupt: 'NUMBER UNIQUE ORDERS', ",
      "'NUMBER VOTERS' and 'NUMBER ALTERNATIVES' must be able to be ",
      "coerced into integers."
    )
  }
  if (!"NUMBER ALTERNATIVES" %in% names(preflib_attributes)) {
    stop(
      "PrefLib datafile is corrupt: ",
      "missing 'NUMBER ALTERNATIVES' metadata."
    )
  }
  n_alternatives <- as.integer(preflib_attributes[["NUMBER ALTERNATIVES"]])
  required_attributes <- c(
    required_attributes,
    paste(
      "ALTERNATIVE NAME",
      seq_len(n_alternatives)
    )
  )
  if (!all(required_attributes %in% names(preflib_attributes))) {
    missing_attributes <- paste(
      required_attributes[
        which(!required_attributes %in% names(preflib_attributes))
      ],
      sep = ", "
    )
    stop(
      "PrefLib datafile is corrupt: ",
      "missing required metadata (",
      missing_attributes,
      ")."
    )
  }

  # Concatenate all the 'ALTERNATIVE NAME X' into a list of names
  alternative_names <- unname(unlist(
    preflib_attributes[
      paste(
        "ALTERNATIVE NAME",
        seq_len(n_alternatives)
      )
    ]
  ))

  # Filter the data lines and 'encourage' them into a csv format
  data_lines <- grep("^[^#]", lines, value = TRUE)
  data_lines <- chartr("{}", "''", data_lines)
  csv_string <- paste(
    sub(
      "^([0-9]+): ",
      "\\1,",
      data_lines
    ),
    collapse = "\n"
  )
  preferences <- read.csv(
    text = csv_string,
    header = FALSE,
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    quote = "'",
    col.names = paste0("V", 1L:(n_alternatives + 1L)),
    colClasses = rep("character", n_alternatives)
  )
  # Replace character columns with lists
  preferences[, seq(2L, ncol(preferences))] <- as.data.frame(
    do.call(
      rbind,
      apply(
        preferences[, seq(2L, ncol(preferences))],
        1L,
        strsplit,
        split = ","
      )
    )
  )
  # Convert all data in the output to integer-valued vectors
  preferences <- rapply(
    preferences,
    as.integer,
    how = "replace"
  )
  names(preferences) <- c(
    "Frequency",
    paste("Rank", seq_len(ncol(preferences) - 1L))
  )
  frequencies <- preferences[, 1L]
  preferences <- preferences[, -1L]
  preferences <- as.preferences(
    preferences,
    "ordering",
    item_names = alternative_names
  )
  aggregated_prefs <- aggregate(preferences, frequencies = frequencies)
  attr(aggregated_prefs, "preflib") <- preflib_attributes

  # Ensure we have the expected number of unique and total orderings.
  n_unique_orders <- as.integer(preflib_attributes[["NUMBER UNIQUE ORDERS"]])
  n_voters <- as.integer(preflib_attributes[["NUMBER VOTERS"]])
  if (length(aggregated_prefs$preferences) != n_unique_orders) {
    warning(
      "Expected ",
      n_unique_orders,
      " unique orderings but only ",
      length(aggregated_prefs$preferences),
      " were recovered when reading the PrefLib datafile.",
      " The file may be corrupt."
    )
  }
  if (sum(aggregated_prefs$frequencies) != n_voters) {
    warning(
      "Expected ",
      n_voters,
      " total orderings but only ",
      sum(aggregated_prefs$frequencies),
      " were recovered when reading the PrefLib datafile.",
      " The file may be corrupt."
    )
  }

  return(aggregated_prefs)
}

#' Write Ordinal Preference Data to PrefLib Formats
#'
#' Write `preferences` to `.soc`, `.soi`, `.toc` or `.toi` file types, as
#' defined by the PrefLib specification:
#' \href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}.
#'
#' @details The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#'
#' The PrefLib format specification requires some additional metadata. Note
#' that the additional metadata required for the PrefLib specification is not
#' necessarily required for the `write_preflib` method; any missing fields
#' required by the PrefLib format will simply show "NA".
#' \describe{
#' \item{TITLE (required)}{
#'   The title of the data file, for instance the year of the election
#'   represented in the data file.
#' }
#' \item{DESCRIPTION (optional)}{
#'   A description of the data file, providing additional information about it.
#' }
#' \item{RELATES TO (optional)}{
#'   The name of the data file that the current file relates to, typically the
#'   source file in case the current file has been derived from another one.
#' }
#' \item{RELATED FILES (optional)}{
#'   The list of all the data files related to this one, comma separated.
#' }
#' \item{PUBLICATION DATE (required)}{
#'   The date at which the data file was published for the first time.
#' }
#' \item{MODIFICATION TYPE (required)}{
#'   The modification type of the data. One of:
#'   \describe{
#'   \item{original}{Data that has only been converted into a PrefLib format.}
#'   \item{induced}{Data that has been induced from another context. For
#'                  example, computing a pairwise relation from a set of strict
#'                  total orders. No assumptions have been made to create these
#'                  files, just a change in the expression language.}
#'   \item{imbued}{Data that has been imbued with extra information. For
#'                 example, extending an incomplete partial order by placing
#'                 all unranked candidates tied at the end.}
#'   \item{synthetic}{Data that has been generated artificially.}
#'   }
#' }
#' \item{MODIFICATION DATE (optional)}{
#'   The last time the data was modified.
#' }
#' }
#'
#' In addition to these fields, some required PrefLib fields will be generated
#' automatically depending on arguments to `write_preflib()` and the attributes
#' of the `aggregated_preferences` object being written to file:
#' \describe{
#' \item{FILE NAME}{The name of the output file.}
#' \item{DATA TYPE}{The data type (one of `soc`, `soi`, `toc` or `toi`).}
#' \item{NUMBER ALTERNATIVES}{The number of items.}
#' \item{ALTERNATIVE NAME `X`}{The name of each item, where `X` ranges from
#'                             `0` to `length(items)`.}
#' \item{NUMBER VOTERS}{The total number of orderings.}
#' \item{NUMBER UNIQUE ORDERS}{The number of distinct orderings.}
#' }
#'
#' Note that PrefLib refers to the items as "alternatives".
#' The "alternatives" in the output file will be the same as the "items" in the
#' `aggregated_preferences` object.
#'
#' @param x An `aggregated_preferences` object to write to file. If `x` is of a
#' different class, it attempts to coerce `x` into an `aggregated_preferences`
#' object via `as.aggregated_preferences()`.
#' @param file Either a character string naming the a file or a writeable,
#' open connection. The empty string `""` will write to stdout.
#' @param title The title of the data file, for instance the name of the
#' election represented in the data file. If not provided, we check for the
#' presence of `attr(x, "preflib")`, and if it exists we check for `TITLE`.
#' @param description A description of the data file, providing additional
#' information about it. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `DESCRIPTION`.
#' @param publication_date The date at which the data file was published for the
#' first time. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `PUBLICATION DATE`.
#' @param modification_type The modification type of the data: one of
#' `original`, `induced`, `imbued` or `synthetic` (see `Details`). If not
#' provided, we check for the presence of `attr(x, "preflib")`, and if it exists
#' we check for `MODIFICATION TYPE`.
#' @param modification_date The last time the data was modified. If not
#' provided, we check for the presence of `attr(x, "preflib")`, and if it exists
#' we check for `MODIFICATION DATE`.
#' @param relates_to The name of the data file that the current file relates to,
#' typically the source file in case the current file has been derived from
#' another one. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `RELATES TO`.
#' @param related_files The list of all the data files related to this one,
#' comma separated. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `RELATED FILES`.
#' @return No return value. Output will be written to file or stdout.
#'
#' @export
write_preflib <- function(x, # nolint: cyclocomp_linter
                          file = "",
                          title = NULL,
                          publication_date = NULL,
                          modification_type = NULL,
                          modification_date = NULL,
                          description = NULL,
                          relates_to = NULL,
                          related_files = NULL) {
  pl_attr <- NULL
  if ("preflib" %in% names(attributes(x))) {
    pl_attr <- attributes(x)$preflib
  }

  if (missing(title)) {
    if (is.list(pl_attr) && "TITLE" %in% names(pl_attr)) {
      title <- pl_attr[["TITLE"]]
      message(
        "`title` inferred from attributes of `x`: ",
        title
      )
    } else {
      title <- NA
      warning(
        "Missing `title`: the PrefLib format requires a ",
        "title to be specified. Using `NA`."
      )
    }
  }

  if (missing(publication_date)) {
    if (is.list(pl_attr) && "PUBLICATION DATE" %in% names(pl_attr)) {
      publication_date <- pl_attr[["PUBLICATION DATE"]]
      message(
        "`publication_date` inferred from attributes of `x`: ",
        publication_date
      )
    } else {
      publication_date <- format(Sys.time(), "%Y-%m-%d")
      warning(
        "Missing `publication_date`, using today's date(",
        publication_date,
        ")."
      )
    }
  }

  if (missing(modification_date)) {
    if (is.list(pl_attr) && "MODIFICATION DATE" %in% names(pl_attr)) {
      modification_date <- pl_attr[["MODIFICATION DATE"]]
      message(
        "`modification_date` inferred from attributes of `x`: ",
        modification_date
      )
    } else {
      modification_date <- format(Sys.time(), "%Y-%m-%d")
      warning(
        "Missing `modification_date`, using today's date(",
        modification_date,
        ")."
      )
    }
  }

  if (missing(modification_type)) {
    if (is.list(pl_attr) && "MODIFICATION TYPE" %in% names(pl_attr)) {
      modification_type <- pl_attr[["MODIFICATION TYPE"]]
      message(
        "`modification_type` inferred from attributes of `x`: ",
        modification_type
      )
    } else {
      modification_type <- NA
      warning(
        "Missing `modification_type`: the PrefLib format requires ",
        "this to be specified. Using `NA`."
      )
    }
  } else {
    modification_type <- try(
      match.arg(
        modification_type,
        c(
          "original",
          "induced",
          "imbued",
          "synthetic"
        )
      ),
      silent = TRUE
    )
    if (inherits(modification_type, "try-error")) {
      stop(
        "`modification_type` must be one of \"original\", \"induced\",",
        "\"imbued\" or \"synthetic\"."
      )
    }
  }

  if (is.character(file) && file == "") {
    file <- stdout()
    file_name <- "NA"
  } else if (is.character(file)) {
    file_name <- basename(file)
    file <- file(file, "w")
  } else {
    file_name <- "NA"
    warning("File name could not be determined. Check output.")
  }

  if (!inherits(file, "connection")) {
    stop("'file' must be a character string.")
  }

  # Coerce into aggregated_preferences
  x <- as.aggregated_preferences(x)

  # Prepare lines for file
  lines <- c(
    # Required metadata
    paste("#", "FILE NAME:", file_name),
    paste("#", "TITLE:", title),
    paste("#", "DESCRIPTION:", description),
    paste("#", "DATA TYPE:", preftype(x$preferences)),
    paste("#", "MODIFICATION TYPE:", modification_type),
    paste("#", "RELATES TO:", paste(relates_to, collapse = ",")),
    paste("#", "RELATED FILES:", paste(related_files, collapse = ",")),
    paste("#", "PUBLICATION DATE:", publication_date),
    paste("#", "MODIFICATION DATE:", modification_date),
    # Derived metadata
    paste("#", "NUMBER ALTERNATIVES:", length(names(x$preferences))),
    paste("#", "NUMBER VOTERS:", sum(x$frequencies)),
    paste("#", "NUMBER UNIQUE ORDERS:", length(x$preferences))
  )

  items <- attr(x$preferences, "item_names")
  lines <- c(
    lines,
    sapply(
      seq_along(items),
      function(i) paste0("# ALTERNATIVE NAME ", i, ": ", items[i])
    )
  )

  # Format orders as strings
  orderings <- apply(
    as.matrix(x$preferences),
    1L,
    function(item_ranks) {
      paste0(
        sapply(
          seq_len(max(na.omit(item_ranks))),
          function(ri) fmt_eql_items(which(ri == item_ranks))
        ),
        collapse = ","
      )
    }
  )
  lines <- c(lines, paste0(x$frequencies, ": ", orderings))

  writeLines(lines, file, sep = "\n")
  if (file_name != "NA") {
    close(file)
  }
}

# Helper function for formatting list of items at equal rank
fmt_eql_items <- function(items) {
  if (length(items) > 1L) {
    paste0("{", paste0(items, collapse = ","), "}")
  } else {
    as.character(items)
  }
}
