#' Group Preferences
#'
#' Create an object of class `grouped_preferences` which associates a
#' group index with an object of class `preferences`. This allows the
#' preferences to be linked to covariates with group-specific values.
#'
#' @param index A numeric vector or a factor with length equal to the number of
#' preferences specifying the subject for each set.
#' @param x A [`preferences`][preferences] object for `group()`; otherwise a
#' `grouped_preferences` object.
#' @param i Indices specifying groups to extract, may be any data type accepted
#' by `[`.
#' @param j Indices specifying items to extract.
#' object, otherwise return a matrix/vector.
#' @param max The maximum number of preferences to format per subject.
#' @param width The maximum width in number of characters to format the
#' preferences.
#' @param ... Additional arguments passed on to
#' [`as.preferences`][as.preferences] by `grouped_preferences`; unused by
#' `format`.
#' @return An object of class `grouped_preferences`, which is a vector of
#' of group IDs with the following attributes:
#' \item{preferences}{ The `preferences` object.}
#' \item{index}{ An index matching each preference set to each group ID.}
#' @examples
#'
#' # ungrouped preferences (5 preference sets, 4 items)
#' R <- as.preferences(
#'   matrix(c(
#'     1, 2, 0, 0,
#'     0, 2, 1, 0,
#'     0, 0, 1, 2,
#'     2, 1, 0, 0,
#'     0, 1, 2, 3
#'   ), ncol = 4, byrow = TRUE),
#'   format = "ranking",
#'   item_names = LETTERS[1:4]
#' )
#' length(R)
#'
#' # group preferences (first three in group 1, next two in group 2)
#' G <- group(R, c(1, 1, 1, 2, 2))
#' length(G)
#'
#' ## by default up to 2 preference sets are shown per group, "..." indicates if
#' ## there are further preferences
#' G
#' print(G, max = 1)
#'
#' ## select preferences from group 1
#' G[1, ]
#'
#' ## exclude item 3 from preferences
#' G[, -3]
#'
#' ## Project preferences in all groups to their first preference
#' G[, 1, by.rank = TRUE]
#'
#' ## preferences from group 2, excluding item 3
#' ## - note group 2 becomes the first (and only) group
#' G[2, -3]
#'
#' # Group preferences by a factor
#' G <- group(R, factor(c("G1", "G1", "G1", "G2", "G2")))
#'
#' G
#' print(G, max = 1)
#'
#' ## select preferences from group G1
#' G["G1"]
#'
#' @name group
#' @export
NULL

#' @rdname group
#' @export
group <- function(x, ...) {
  UseMethod("group")
}

#' @rdname group
#' @method group preferences
#' @export
group.preferences <- function(x, index, ...) {
  if (!((is.vector(index) || is.factor(index)) &&
    length(index) == length(x))) {
    stop("index must be a vector or factor with length equal to preferences")
  }
  if (is.factor(index)) {
    group_names <- levels(index)
  } else {
    group_names <- NULL
  }
  index <- as.numeric(index)
  structure(seq_len(max(index)),
    preferences = x,
    index = index,
    group_names = group_names,
    class = "grouped_preferences"
  )
}

#' @rdname group
#' @method [ grouped_preferences
#' @export
"[.grouped_preferences" <- function(x, i, j, ...) {
  group_names <- attr(x, "group_names")
  if (!missing(i)) {
    if (is.character(i)) {
      i <- match(i, group_names)
    }
    group_names <- group_names[i]
    if (missing(j)) {
      j <- TRUE
    }
    # always a vector if picking out elements of preferences matrix
    if (is.matrix(i)) {
      r <- split(seq_along(attr(x, "index")), attr(x, "index"))
      i1 <- unlist(r[i[, 1L]])
      i2 <- rep(i[, 2L], lengths(r))
      return(.subset(attr(x, "preferences"), cbind(i1, i2)))
    }
    # convert index of groups to index of preferences
    g <- .subset(x, i)
    # create index for preferences matrix
    i <- which(attr(x, "index") %in% g)
    groups <- split(i, attr(x, "index")[i])[as.character(g)]
    i <- unlist(groups)
    # update value and index to remove omitted groups
    value <- seq_along(groups)
    index <- rep(value, lengths(groups))
  } else {
    if (missing(j)) {
      return(x)
    }
    value <- x
    i <- TRUE
    index <- attr(x, "index")
  }
  # now subset preferences matrix
  preferences <- attr(x, "preferences")[i, j, ...]
  # convert preferences matrix to grouped_preferences
  if (!is.null(group_names)) {
    group(preferences, as.factor(group_names[index]))
  } else {
    group(preferences, index)
  }
}



#' @method as.data.frame grouped_preferences
#' @export
# nolint start
as.data.frame.grouped_preferences <-
  function(x, row.names = NULL, optional = FALSE, ...,
           nm = paste(deparse(substitute(x), width.cutoff = 20L),
             collapse = " "
           )) {
    if (is.null(row.names)) {
      row.names <- attr(x, "group_names")
    }
    value <- list(x)
    if (!optional) {
      names(value) <- nm
    }
    if (is.null(row.names) && !is.null(rownames(x))) {
      row.names <- rownames(x)
    }
    if (is.null(row.names)) {
      row.names <- .set_row_names(length(x))
    } else {
      if (is.object(row.names) || !is.integer(row.names)) {
        row.names <- as.character(row.names)
      }
      if (anyNA(row.names)) {
        stop("row names contain missing values")
      }
      if (anyDuplicated(row.names)) {
        stop(paste(
          "duplicate row.names: ",
          toString(unique(row.names[duplicated(row.names)]))
        ))
      }
    }
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
  }
# nolint end

#' @method print grouped_preferences
#' @export
print.grouped_preferences <- function(x, max = 2L, width = 20L, ...) {
  print.default(format(x, max = max, width = width, ...), quote = FALSE)
}

#' @rdname group
#' @method format grouped_preferences
#' @export
format.grouped_preferences <- function(x, max = 2L, width = 20L, ...) {
  tab <- tabulate(attr(x, "index"))
  rep <- numeric(length(attr(x, "index")))
  rep[order(attr(x, "index"))] <- sequence(tab)
  if (ncol(attr(x, "preferences")) > 0L) {
    j <- TRUE
  } else {
    j <- NULL
  }
  r <- attr(x, "preferences")[rep <= max, j]
  char <- format.preferences(r, width = width)
  value <- vapply(
    split(char, attr(x, "index")[rep <= max]),
    function(x) {
      if (all(is.na(x))) {
        return(NA_character_)
      }
      toString(x)
    },
    "a"
  )
  # add ... if more than max preferences
  trunc <- tab > max & !is.na(value)
  value[trunc] <- paste0(value[trunc], ", ...")
  group_names <- attr(x, "group_names")
  if (!is.null(group_names)) {
    names(value) <- group_names
  }
  value
}

#' @method na.omit grouped_preferences
#' @importFrom stats na.omit
na.omit.grouped_preferences <- function(object, ...) {
  omit <- seq_along(
    attr(
      object,
      "preferences"
    )
  )[is.na(attr(object, "preferences"))]
  if (length(omit) == 0L) {
    return(object)
  }
  nm <- names(object)
  index <- attr(object, "index")[-omit]
  index <- match(index, unique(index))
  names(omit) <- nm[omit]
  attr(omit, "class") <- "omit"
  structure(unique(index),
    preferences = attr(object, "preferences")[-omit, , drop = FALSE],
    index = index,
    na.action = omit,
    class = "grouped_preferences"
  )
}

#' @method na.exclude grouped_preferences
#' @importFrom stats na.exclude
na.exclude.grouped_preferences <- function(object, ...) {
  out <- na.omit(object)
  class(attr(out, "na.action")) <- "na.exclude" # nolint: object_name_linter
  out
}

#' @method is.na grouped_preferences
#' @export
is.na.grouped_preferences <- function(x) {
  out <- tapply(attr(x, "preferences"), attr(x, "index"), sum) == 0L
  names(out) <- names(x)
  out
}
