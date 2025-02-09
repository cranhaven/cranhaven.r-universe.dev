#' Preferences Object
#'
#' Create a `preferences` object for representing Ordinal Preference datasets.
#'
#' Ordinal preferences can order every item, or they can order a subset. Some
#' ordinal preference datasets will contain ties between items at a given rank.
#' Hence, there are four distinct types of preferential data:
#' \describe{
#' \item{`soc`}{Strict Orders - Complete List}
#' \item{`soi`}{Strict Orders - Incomplete List}
#' \item{`toc`}{Orders with Ties - Complete List}
#' \item{`toi`}{Orders with Ties - Incomplete List}
#' }
#' The data type is stored alongside the `preferences` as an attribute
#' `attr(preferences, "preftype")`. The data type is determined automatically.
#' If every preference ranks every item, then the data type will be
#' "soc" or "soi". Similarly, if no preference contains a tie the data type
#' will be "toc" or "toi".
#'
#' A set of preferences can be represented either by `ranking` or by
#' `ordering`. These correspond to the two ways you can list a set of
#' preferences in a vector:
#' \describe{
#' \item{`ordering`}{The items are listed in order of most preferred to least
#'                        preferred, allowing for multiple items being in the
#'                        same place in the case of ties.}
#' \item{`ranking`}{A rank is assigned to each item.  Conventionally, ranks are
#'                        integers in increasing order (with larger values
#'                        indicating lower preference), but they can be any
#'                        ordinal values.  Any given rankings will be converted
#'                        to 'dense' rankings: positive integers from 1 to some
#'                        maximum rank, with no gaps between ranks.}
#' }
#' When reading preferences from an `ordering` matrix, the index on the
#' items is the order passed to the `item_names` parameter. When reading from
#' a `rankings` matrix, if no `item_names` are provided, the order is inferred
#' from the named columns.
#'
#' A `preferences` object can also be read from a long-format matrix, where
#' there are three columns: `id`, `item` and `rank`. The `id` variable groups
#' the rows of the matrix which correspond to a single set of preferences, which
#' the `item:rank`, pairs indicate how each item is ranked. When reading a
#' matrix from this format and no `item_names` parameter is passed, the order is
#' determined automatically.
#'
#' @param data A data frame or matrix in one of three formats:
#' \describe{
#' \item{"ordering"}{Orderings must be a data frame with list-valued
#'                   columns. Each row represents an ordering of the items
#'                   from first to last, representing ties by a list of
#'                   vectors corresponding to the items.}
#' \item{"ranking"}{Each row assigns a rank to each item, with columns
#'                  representing items.  Note that rankings will be converted
#'                  to 'dense' rankings in the output (see Details).}
#' \item{"long"}{Three columns: an `id` column grouping the rows which
#'                correspond to a single set of preferences, an
#'                `item` column specifying (either by index or by
#'                name) the item each row refers to, and a `rank`
#'                column specifying the rank for the associated
#'                item.}
#' }
#' @param format The format of the data: one of "ordering", "ranking", or
#' "long" (see above). By default, `data` is assumed to be in "long" format.
#' @param id For `data` in long-format: the column representing the
#' preference set grouping.
#' @param item For `data` in long-format: the column representing
#' the items by name or by index, in which case the
#' `item_names` parameter should also be passed, or the items will be named as
#' integers.
#' @param rank For `data` in long-format: the column representing the
#' rank for the associated item.
#' @param item_names The names of the full set of items. When loading data using
#' integer-valued indices in place of item names, the `item_names` character
#' vector should be in the correct order.
#' @param aggregate If `TRUE`, aggregate the preferences via
#' [`aggregate.preferences`][aggregate.preferences] before returning. This
#' returns an [`aggregated_preferences`][aggregate.preferences] object.
#' @param frequencies An optional integer vector containing the number of
#' occurences of each preference. If provided, the method will return a
#' [`aggregated_preferences`][aggregate.preferences] object with the
#' corresponding frequencies.
#' @param verbose If `TRUE`, diagnostic messages will be sent to stdout.
#' @param ... Unused.
#'
#' @return By default, a `preferences` object, which is a data frame with
#' list-valued columns corresponding to preferences on the items. This may
#' be an ordering on subsets of the items in the case of ties, or a
#' potentially-partial strict ordering. In the case of partial or tied
#' preferences, some entries may be empty lists.
#'
#' @examples
#' # create rankings from data in long form
#'
#' # Example long-form data
#' x <- data.frame(
#'   id = c(rep(1:4, each = 4), 5, 5, 5),
#'   item = c(
#'     LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'     LETTERS[3:5]
#'   ),
#'   rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3)
#' )
#'
#' # * Set #1 has two different ranks for the same item (item C
#' # has rank 1 and 2). This item will be excluded from the preferences.
#' # * All ranks are missing in set #2, a technically valid partial ordering
#' # * Some ranks are missing in set #3, a perfectly valid partial ordering
#' # * Set #4 has inconsistent ranks for two items, and a rank with a
#' # missing item.
#' # * Set #5 is not a dense ranking. It will be converted to be dense and then
#' # inferred to be a regular partial ordering with ties.
#' split(x, x$rank)
#'
#' # Creating a preferences object with this data will attempt to resolve these
#' # issues automatically, sending warnings when assumptions need to be made.
#' preferences(x, id = "id", item = "item", rank = "rank")
#'
#' # Convert an existing matrix of rankings to a preferences object.
#' rnk <- matrix(c(
#'   1, 2, 0, 0,
#'   4, 1, 2, 3,
#'   2, 1, 1, 1,
#'   1, 2, 3, 0,
#'   2, 1, 1, 0,
#'   1, 0, 3, 2
#' ), nrow = 6, byrow = TRUE)
#' colnames(rnk) <- c("apple", "banana", "orange", "pear")
#'
#' rnk <- as.preferences(rnk, format = "ranking")
#'
#' # Convert an existing data frame of orderings to a preferences object.
#' e <- character() # short-hand for empty ranks
#' ord <- preferences(
#'   as.data.frame(
#'     rbind(
#'       list(1, 2, e, e), # apple, banana
#'       list("banana", "orange", "pear", "apple"),
#'       list(c("banana", "orange", "pear"), "apple", e, e),
#'       list("apple", "banana", "orange", e),
#'       list(c("banana", "orange"), "apple", e, e),
#'       list("apple", "pear", "orange", e)
#'     )
#'   ),
#'   format = "ordering",
#'   item_names = c("apple", "banana", "orange", "pear")
#' )
#'
#' # Access the first three sets of preferences
#' ord[1:3, ]
#'
#' # Truncate preferences to the top 2 ranks
#' ord[, 1:2, by_rank = TRUE]
#'
#' # Exclude pear from the rankings
#' ord[, -4]
#'
#' # Get the highest-ranked items and return as a data.frame of orderings
#' ord[, 1, by_rank = TRUE, as.ordering = TRUE]
#'
#' # Convert the preferences to a ranking matrix
#' as.matrix(ord)
#'
#' # Get the rank of apple in the third preference-set
#' as.matrix(ord)[3, 1]
#'
#' # Get all the ranks assigned to apple as a vector
#' as.matrix(ord)[, "apple"]
#'
#' @export
preferences <- function(data,
                        format = c("long", "ordering", "ranking"),
                        id = NULL,
                        rank = NULL,
                        item = NULL,
                        item_names = NULL,
                        frequencies = NULL,
                        aggregate = FALSE,
                        verbose = TRUE,
                        ...) {
  fmt <- try(match.arg(format), silent = TRUE)
  # First we reformat the data into a matrix of rankings.
  if (inherits(fmt, "try-error")) {
    stop(
      "Format '", format, "' not implemented: Must be one ",
      "of 'ordering', 'ranking' or 'long'."
    )
  } else if (fmt == "long") {
    if (missing(id) || missing(item) || missing(rank)) {
      stop(
        "When creating \"preferences\" from long-format data, ",
        "`id`, `item` and `rank` must all specify columns in the data."
      )
    }
    if (!missing(frequencies)) {
      warning(
        "When creating \"preferences\" from long-format data, ",
        "`frequencies` parameter is ignored."
      )
      frequencies <- NULL
    }
    if (missing(item_names)) {
      item_names <- na.omit(unique(data[, item]))
    }
    ranking <- long_to_ranking(
      data,
      id,
      item,
      rank,
      item_names,
      verbose
    )
    prefs <- as.preferences.matrix(ranking,
      format = "ranking",
      item_names = item_names,
      ...,
      aggregate = FALSE,
      verbose = verbose
    )
  } else if (fmt == "ordering") {
    ranking <- ordering_to_ranking(data, item_names, verbose)
    prefs <- as.preferences.matrix(ranking,
      format = "ranking",
      ...,
      aggregate = FALSE,
      verbose = verbose
    )
  } else if (fmt == "ranking") {
    x <- data
    # Infer item names
    if (!is.null(colnames(x)) && missing(item_names)) {
      item_names <- colnames(x)
    } else if (is.null(colnames(x)) && missing(item_names)) {
      warning(
        "Item names could not be inferred from ",
        "ranked data. Defaulting to the integers 1-", dim(x)[2L]
      )
      item_names <- as.character(seq_len(dim(x)[2L]))
    }
    prefs <- as.preferences.matrix(data,
      format = "ranking",
      item_names = item_names,
      ...,
      aggregate = FALSE,
      verbose = verbose
    )
  }
  # Aggregate if necessary.
  if (aggregate) {
    return(aggregate(prefs))
  } else if (!is.null(frequencies)) {
    return(as.aggregated_preferences(prefs, frequencies = frequencies))
  } else {
    return(prefs)
  }
}

# A helper function to validate ordinal preferences in ordering format.
validate_ordering <- function(x,
                              item_names,
                              verbose = TRUE) {
  # Ensure the object is a data frame with "list" columns
  if (!is.data.frame(x) || !all(sapply(x, typeof) == "list")) {
    stop(
      "`data` in \"ordering\" format must be a \"data.frame\" with ",
      "\"list\"-valued columns."
    )
  }

  # Ensure items are only listed by index when item_names is passed
  item_types <- unique(rapply(x, class))
  if (missing(item_names) && "numeric" %in% item_types) {
    stop("Items listed by index but no `item_names` provided to method.")
  }
}

# A helper function to convert ordering-form data into a rankings matrix.
ordering_to_ranking <- function(data,
                                item_names,
                                verbose = TRUE) {
  validate_ordering(data, item_names, verbose)

  if (is.null(item_names)) {
    if (!is.null(attr(data, "item_names"))) {
      item_names <- attr(data, "item_names")
    } else {
      item_names <- as.character(unique(unlist(data)))
    }
  }
  # Convert the ordering data into a matrix of rankings.
  data <- rapply(data,
    function(a) {
      if (!is.numeric(a)) {
        return(match(a, item_names))
      } else {
        return(a)
      }
    },
    how = "replace"
  )
  n_ranks <- ncol(data)
  n_alts <- length(item_names)
  structure(
    matrix(
      t(apply(
        data,
        1L,
        function(x) {
          vals <- rep(NA, n_alts)
          vals[unlist(x)] <- rep(seq_len(n_ranks), sapply(x, length))
          vals
        }
      )),
      ncol = length(item_names)
    ),
    dimnames = list(NULL, item_names)
  )
}

# A helper function to validate ordinal preferences in long format.
validate_long <- function(data,
                          item_names,
                          verbose = TRUE) {
  # Raise error if `id`, `item` or `rank` are invalid.
  if (ncol(data) != 3L) {
    stop(
      "When using long-format, `id`, `item` and `rank` ",
      "must all specify names of columns in `data`."
    )
  }

  # Show warning if any columns contain NA
  if (anyNA(data)) {
    if (verbose) {
      message("Dropping rows containing `NA`.")
    }
  }

  # Find duplicated items
  if (
    anyDuplicated(paste(data[, "id"], data[, "item"], sep = ":")) &&
      verbose
  ) {
    message(
      "Duplicated rankings per item detected: ",
      "only the highest ranks will be used."
    )
  }

  # Validate items
  if (is.null(item_names)) {
    item_names <- unique(data[, "item"])
  }
  if (is.character(data[, "item"])) {
    if (length(setdiff(
      na.omit(unique(data[, "item"])),
      item_names
    )) > 0L) {
      stop("Found `item` not in `item_names`.")
    }
  } else if (is.numeric(data[, "item"])) {
    if (any(data[, "item"] > length(item_names))) {
      stop("`item` index out of bounds.")
    }
    data[, "item"] <- item_names[data[, "item"]]
  }

  # Validate rank
  orig_rank <- na.omit(data[, "rank"])
  int_rank <- as.integer(orig_rank)
  if (anyNA(int_rank) || any(int_rank != orig_rank)) {
    stop("`rank` must be integer-valued.")
  }
}

# A helper function to convert long-form data into a rankings matrix.
long_to_ranking <- function(data,
                            id,
                            item,
                            rank,
                            item_names = NULL,
                            verbose = TRUE) {
  if (dim(data)[1L] == 0L) {
    return(
      matrix(
        ncol = length(item_names),
        nrow = 0L,
        dimnames = list(NULL, item_names)
      )
    )
  }
  # Take only required columns
  data <- as.data.frame(data[, c(id, item, rank)])
  colnames(data) <- c("id", "item", "rank")

  validate_long(data, item_names, verbose)

  data[["rank"]] <- as.integer(data[["rank"]])

  # Process item_names
  if (is.null(item_names)) {
    item_names <- as.character(na.omit(unique(data[["item"]])))
  }
  if (is.numeric(data[["item"]])) {
    data[["item"]] <- item_names[data[["item"]]]
  }
  data[["item"]] <- factor(data[["item"]])

  # Return the ranking matrix
  return(data %>%
    # Filter rows containing NA
    dplyr::filter(!is.na(rank) & !is.na(item) & !is.na(id)) %>%
    # Filter duplicate rankings, keeping the highest ranking for an item.
    dplyr::group_by(id, item) %>%
    dplyr::top_n(1L, -rank) %>%
    # Convert rankings to dense rankings
    dplyr::ungroup(item) %>%
    dplyr::mutate(rank = dplyr::dense_rank(rank)) %>%
    # Convert long-format rankings to wide rankings
    tidyr::spread(item, rank, drop = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(item_names)) %>%
    as.matrix())
}

# A helper to convert a matrix of rankings into ordering format.
ranking_to_ordering <- function(ranking) {
  max_rank <- max(c(0L, na.omit(c(as.matrix(ranking)))))
  if (getRversion() < "4.1.0") {
    out <- as.data.frame(
      do.call(
        rbind,
        lapply(
          seq_len(dim(ranking)[1L]),
          function(i) {
            r <- ranking[i, ]
            sapply(
              seq_len(max_rank),
              function(x) list(names(r)[which(r == x)])
            )
          }
        )
      )
    )
  } else {
    out <- as.data.frame(
      do.call(
        rbind,
        apply(ranking,
          1L,
          function(r) {
            sapply(
              seq_len(max_rank),
              function(x) as.list(names(r)[which(r == x)])
            )
          },
          simplify = FALSE
        )
      )
    )
  }
  if (ncol(out) > 0L) {
    colnames(out) <- paste0("Rank", seq_len(max_rank))
  }
  out
}

#' @method Ops preferences
#' @export
Ops.preferences <- function(e1, e2) {
  op <- .Generic[[1]] # nolint: object_usage_linter
  switch(op,
    `==` = {
      minlen <- min(length(e1), length(e2))
      maxlen <- max(length(e1), length(e2))
      e1_names <- names(e1)
      e2_names <- names(e2)
      if (!identical(sort(e1_names), sort(e2_names))) {
        return(rep(FALSE, maxlen))
      }
      e1 <- unclass(e1)
      e2 <- unclass(e2)[, e1_names, drop = FALSE]
      if (anyNA(e1)) {
        e1[is.na(e1)] <- 0L
      }
      if (anyNA(e2)) {
        e2[is.na(e2)] <- 0L
      }
      cmp <- rowSums(e1[seq_len(minlen), , drop = FALSE] ==
        e2[seq_len(minlen), , drop = FALSE]) >= 1L
      if (minlen < maxlen) {
        warning("Objects being compared are not the same length.")
        cmp <- c(cmp, rep(FALSE, maxlen - minlen))
      }
      return(cmp)
    },
    `!=` = {
      return(!e1 == e2)
    },
    stop("Undefined operation for \"preferences\".")
  )
}

#' @rdname preferences
#' @method [ preferences
#' @param x The `preferences` object to subset.
#' @param i The index of the preference-set to access.
#' @param j The items to project onto, either by index or by name; if
#' `by.rank = TRUE`, the ranks to access.
#' @param j The item names or indices to project onto, e.g. if `j = 1` the
#' preferences will be projected only onto the first item; if `by.rank = TRUE`
#' `j` corresponds to the rank of the items to subset to, e.g. if `j = 1` then
#' preferences will be truncated to only contain their highest-preference.
#' @param by.rank When `FALSE`, the index `j` corresponds to items, when true
#' the index corresponds to rank.
#' @param as.ordering When `FALSE`, returns a `preferences` object:
#' internally rows \eqn{i} contain the ranking assigned to each item
#' in preference \eqn{p_i}. When `TRUE`, returns a data frame where
#' columns group the items by rank.
#' @param width The width in number of characters to format each preference,
#' truncating by "..." when they are too long.
#' @export
"[.preferences" <- function(x, # nolint: cyclocomp_linter
                            i,
                            j,
                            ...,
                            by.rank = FALSE,
                            as.ordering = FALSE) {
  if (!missing(j) && is.numeric(j) && any(abs(j) > ncol(x))) {
    stop("Subscript `j` out of bounds.")
  }
  if (!missing(i) && is.numeric(i) && any(abs(i) > nrow(x))) {
    stop("Subscript `i` out of bounds.")
  }

  # Accessing rankings
  if (missing(j)) {
    j <- TRUE
    if (missing(i)) {
      value <- unclass(x)
    } else {
      # always a vector if picking out elements of rankings matrix
      if (is.matrix(i)) {
        stop("Cannot subset by matrix.")
      }
      # else subset of rankings
      value <- .subset(x, i, j, drop = FALSE)
    }
  } else {
    # Subset items (never drop)
    if (missing(i)) {
      i <- TRUE
    }
    if (by.rank) {
      # Select only ranks specified in j:
      value <- .subset(x, i, TRUE, drop = FALSE)
      value <- replace(
        value,
        !value %in% j,
        NA
      )
    } else {
      # Subset normally
      value <- .subset(x, i, j, drop = FALSE)
    }
    # Convert to dense rank
    if (getRversion() < "4.1.0") {
      value <- do.call(
        rbind,
        lapply(
          seq_len(dim(value)[1L]),
          function(i) dplyr::dense_rank(value[i, ])
        )
      )
    } else {
      value <- do.call(
        rbind,
        apply(value,
          1L,
          dplyr::dense_rank,
          simplify = FALSE
        )
      )
    }
  }

  # Sort subset of items so that they appear in the same order as the
  # original preferences' items.
  items <- attr(x, "item_names")
  if (!by.rank) {
    if (!is.character(j)) {
      sub_items <- items[j]
    } else {
      sub_items <- j
    }
    ord <- order(match(sub_items, items))
    value <- value[, ord, drop = FALSE]
    sub_items <- sub_items[ord]
  } else {
    sub_items <- items
  }

  # Accessing orderings
  if (as.ordering) {
    colnames(value) <- sub_items
    ranking_to_ordering(value)
  } else {
    structure(value,
      dimnames = list(NULL, sub_items),
      item_names = sub_items,
      class = c("preferences", class(value)),
      preftype = preftype(value)
    )
  }
}

# A helper function to determine the type of the preference data:
# "soc", "soi", "toc", or "toi".
preftype <- function(prefs) {
  x <- as.matrix(prefs)
  complete <- !anyNA(x)
  ties <- FALSE
  for (i in seq_len(nrow(x))) {
    if (anyDuplicated(na.omit(x[i, ]))) {
      ties <- TRUE
      break
    }
  }
  if (complete) {
    if (ties) {
      "toc"
    } else {
      "soc"
    }
  } else {
    if (ties) {
      "toi"
    } else {
      "soi"
    }
  }
}


#' @rdname preferences
#' @export
as.preferences <- function(x, ...) { # nolint
  UseMethod("as.preferences")
}


#' @rdname preferences
#' @method as.preferences grouped_preferences
#' @export
as.preferences.grouped_preferences <- function(x,
                                               aggregate = FALSE,
                                               verbose = TRUE,
                                               ...) {
  prefs <- attr(x, "preferences")
  if (aggregate) {
    aggregate(prefs)
  } else {
    prefs
  }
}

#' @rdname preferences
#' @method as.preferences default
#' @export
as.preferences.default <- function(x,
                                   format = c("long", "ranking", "ordering"),
                                   id = NULL,
                                   item = NULL,
                                   rank = NULL,
                                   item_names = NULL,
                                   aggregate = FALSE,
                                   verbose = TRUE,
                                   ...) {
  format <- match.arg(format)
  # Convert orderings data frames into ranking matrices.
  if (
    format == "ordering" ||
      inherits(x, "data.frame") &&
        all(sapply(x, typeof) == "list")
  ) {
    x <- ordering_to_ranking(x, item_names, verbose)
    format <- "ranking"
  }
  if (missing(item_names) && format == "long") {
    item_names <- unique(x[, item])
  }
  x <- as.matrix(x)
  as.preferences.matrix(
    x,
    format,
    id,
    item,
    rank,
    item_names,
    aggregate,
    verbose
  )
}

#' @rdname preferences
#' @method as.preferences matrix
#' @export
as.preferences.matrix <- function(x,
                                  format = c("long", "ranking"),
                                  id = NULL,
                                  item = NULL,
                                  rank = NULL,
                                  item_names = NULL,
                                  aggregate = FALSE,
                                  verbose = TRUE,
                                  ...) {
  format <- match.arg(format)
  if (inherits(x, "preferences")) {
    format <- "ranking"
  }

  # Process item_names
  if (is.null(item_names)) {
    if (format == "ranking") {
      if (!is.null(colnames(x))) {
        item_names <- colnames(x)
      } else {
        if (verbose) {
          message(
            "Could not determine item names. ",
            "Using the integers 1-", ncol(x), "."
          )
        }
        item_names <- as.character(seq_len(ncol(x)))
        colnames(x) <- item_names
      }
    } else if (format == "long") {
      item_names <- na.omit(unique(x["item"]))
    }
  }

  # Reformat the data into a matrix of rankings.
  if (format == "long") {
    if (is.null(id) || is.null(item) || is.null(rank)) {
      stop(
        "When using long-format, `id`, `item` and `rank` ",
        "must all specify names of columns in `data`."
      )
    }
    prefs <- long_to_ranking(
      x,
      id,
      item,
      rank,
      item_names,
      verbose
    )
  } else if (format == "ranking") {
    if (all(dim(x) > 0L)) {
      prefs <- t(apply(x, 1L, dplyr::dense_rank))
    } else {
      prefs <- x
    }
    colnames(prefs) <- item_names
  } else {
    stop("Not implemented.")
  }
  class(prefs) <- c("preferences", class(prefs))
  attr(prefs, "item_names") <- item_names
  attr(prefs, "preftype") <- preftype(prefs)
  if (aggregate) {
    aggregate(prefs)
  } else {
    prefs
  }
}

#' @rdname preferences
#' @method as.preferences aggregated_preferences
#' @export
as.preferences.aggregated_preferences <- function(x, ...) {
  return(rep(
    x$preferences,
    x$frequencies
  ))
}

#' @method length preferences
#' @export
length.preferences <- function(x) {
  nrow(x)
}

#' @method is.na preferences
#' @export
is.na.preferences <- function(x) {
  # Valid dense rankings
  apply(
    x,
    1L,
    function(x) {
      x_sub <- na.omit(x)
      identical(dplyr::dense_rank(x_sub), x_sub)
    }
  )
}

#' @method print preferences
#' @export
print.preferences <- function(x, ...) {
  if (length(x) == 0L) {
    cat("preferences(0)\n")
  } else {
    print.default(format(x, ...), quote = FALSE)
  }
}

#' @method format preferences
#' @rdname preferences
#' @export
format.preferences <- function(x, width = 40L, ...) {
  f <- function(i, items) {
    keep <- !is.na(i) & i != 0L
    obj <- items[keep]
    i <- i[keep]
    ord <- order(i)
    if (length(obj) > 1L) {
      op <- ifelse(diff(i[ord]) == 0L, " = ", " > ")
      paste0(obj[ord], c(op, ""), collapse = "")
    } else if (length(obj) == 1L) {
      obj
    } else {
      "blank"
    }
  }
  value <- apply(x, 1L, f, items = attr(x, "item_names"))
  nc <- nchar(value)
  trunc <- !is.na(nc) & nc > (width - 2L)
  value[trunc] <- paste(strtrim(value[trunc], width - 6L), "...")
  paste0("[", value, "]")
}

#' @method rbind preferences
#' @export
rbind.preferences <- function(...) {
  # check contain the same items
  preflist <- list(...)
  nm <- lapply(preflist, colnames)
  ref <- nm[[1L]]
  ok <- vapply(nm, identical, TRUE, ref)
  item_names <- unique(unlist(nm))
  if (!all(ok)) {
    preflist <- lapply(
      preflist,
      function(x) {
        preflist <- matrix(0L,
          nrow = nrow(x),
          ncol = length(item_names),
          dimnames = list(NULL, item_names)
        )
        preflist[, colnames(x)] <- x
        preflist
      }
    )
  }
  # rbind all preferences matrices
  preflist <- do.call("rbind", lapply(preflist, unclass))
  structure(preflist,
    class = unique(c("preferences", class(preflist))),
    item_names = item_names,
    preftype = preftype(preflist)
  )
}

#' @method as.data.frame preferences
#' @export
# nolint start
as.data.frame.preferences <- function(x,
                                      row.names = NULL,
                                      optional = FALSE,
                                      ...,
                                      nm = paste(
                                        deparse(substitute(x),
                                          width.cutoff = 20L
                                        ),
                                        collapse = " "
                                      )) {
  value <- list(x)
  if (!optional) {
    names(value) <- nm
  } else {
    names(value) <- make.names(nm)
  }
  if (is.null(row.names) && !is.null(rownames(x))) {
    row.names <- rownames(x)
  }
  if (is.null(row.names)) {
    row.names <- .set_row_names(nrow(x))
  } else {
    if (is.object(row.names) || !is.integer(row.names)) {
      row.names <- as.character(row.names)
    }
    if (anyNA(row.names)) {
      stop("row names contain missing values")
    }
    if (anyDuplicated(row.names)) {
      dup_rows <- paste(unique(row.names[duplicated(row.names)]), collapse = ", ")
      stop(
        "duplicate row.names: ",
        dup_rows
      )
    }
  }
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}
# nolint end

#' @method as.matrix preferences
#' @export
as.matrix.preferences <- function(x, ...) {
  m <- unclass(x)
  colnames(m) <- attr(x, "item_names")
  attr(m, "item_names") <- NULL
  return(m)
}

#' @importFrom utils str
#' @export
utils::str

#' @method str preferences
str.preferences <- function(object, ...) {
  str(unclass(object))
}

#' @method rep preferences
#' @export
rep.preferences <- function(x, ...) {
  x[rep(seq_len(nrow(x)), ...)]
}

#' @method names preferences
#' @export
names.preferences <- function(x, ...) {
  return(attr(x, "item_names"))
}

#' @method "names<-" preferences
#' @export
"names<-.preferences" <- function(x, value) {
  if (anyNA(value) ||
    !identical(unique(value), value) ||
    length(value) != dim(x)[2L]) {
    stop("Item names must be unique and cannot be empty.")
  }
  attr(x, "item_names") <- value
  colnames(x) <- value
  x
}
