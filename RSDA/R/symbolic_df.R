#' Check duplicated names in a quo
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom rlang get_expr
check_quo_duplicated_names <- function(x) {
  duplicated_names <- which(duplicated(names(x), fromLast = T))
  fs <- purrr::map_chr(x[duplicated_names], ~ as.character(rlang::get_expr(.)[[1]]))
  names(x)[duplicated_names] <- paste0(names(x)[duplicated_names], "_", fs)
  return(x)
}

#' tbl_subset_col
#' @keywords internal
tbl_subset_col <- function(x, j, j_arg) {
  if (is.null(j)) return(x)
  vectbl_as_col_location <- getFromNamespace("vectbl_as_col_location","tibble")
  j <- vectbl_as_col_location(j, length(x), names(x), j_arg = j_arg, assign = FALSE)

  if (anyNA(j)) {
    cnd_signal(error_na_column_index(which(is.na(j))))
  }

  xo <- .subset(x, j)
  if (anyDuplicated(j)) {
    xo <- set_repaired_names(xo, repair_hint = FALSE, .name_repair = "minimal")
  }
  set_tibble_class <- function(x, nrow) {
    attr(x, "row.names") <- .set_row_names(nrow)
    tibble_class <- getFromNamespace("tibble_class","tibble")
    class(x) <- tibble_class
    x
  }
  fast_nrow <- getFromNamespace("fast_nrow","tibble")
  set_tibble_class(xo, nrow = fast_nrow(x))
}

#' Generate a symbolic data frame
#' @description Generate a symbolic data table from a classic data table.
#' @param x A data.frame.
#' @param concept These are the variable that we are going to use a concepts.
#' @param variables These are the variables that we want to include in the symbolic data table.
#' @param default.numeric function to use for numeric variables
#' @param default.categorical function to use for categorical variables
#' @param ... A vector with names and the type of symbolic data to use, the available types are type_histogram (), type_continuous (), type.set (), type.modal (), by default type_histogram () is used for numeric variables and type_modal () for the categorical variables.
#' @return a [tibble][tibble::tibble-package]
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @importFrom tidyselect vars_select everything
#' @importFrom rlang quos !! !!! syms
#' @importFrom dplyr group_by summarise select summarise_all left_join ungroup mutate mutate_if '%>%'
#' @importFrom  purrr compose
#' @importFrom forcats fct_unify
#' @export
classic.to.sym <- function(x = NULL,
                           concept = NULL,
                           variables = tidyselect::everything(),
                           default.numeric = sym.interval,
                           default.categorical = sym.modal,
                           ...) {
  .concept <- tidyselect::vars_select(colnames(x), !!rlang::enquo(concept))
  col.types <- rlang::quos(...)
  col.types <- check_quo_duplicated_names(col.types)
  var.names <- tidyselect::vars_select(colnames(x), c(!!rlang::enquo(concept),
                                                      !!rlang::enquo(variables)))
  var.names <- var.names[!var.names %in% names(col.types)]

  out1 <- x %>%
    dplyr::group_by(!!!rlang::syms(.concept)) %>%
    dplyr::summarise(!!!col.types)

  default_function <- function(x) {
    if (is.numeric(x)) {
      return(default.numeric(x))
    }
    return(default.categorical(x))
  }

  out2 <- x %>%
    dplyr::select(var.names) %>%
    dplyr::group_by(!!!dplyr::syms(.concept)) %>%
    dplyr::summarise_all(default_function)

  out <- dplyr::left_join(out1, out2, by = .concept) %>%
    dplyr::ungroup()

  concepts <- apply(out[, .concept], 1, function(x) paste0(x, collapse = ":"))
  out <- dplyr::select(out, -.concept)
  #out <- tibble::add_column(out, Concept = concepts, .before = 1)
  attr(out, "concept") <- concepts
  class(out) <- c("symbolic_tbl", class(out))
  return(out)
}

#' subset for symbolic table
#' @export
#' @importFrom utils getFromNamespace
#' @keywords internal
`[.symbolic_tbl` <- function(x, i, j, drop = FALSE, ...) {
  .concepts <- attr(x,"concept")
  tbl_subset_matrix <- getFromNamespace("tbl_subset_matrix","tibble")
  tbl_subset_col <- function(x, j, j_arg) {
    if (is.null(j)) return(x)
    vectbl_as_col_location <- getFromNamespace("vectbl_as_col_location","tibble")
    j <- vectbl_as_col_location(j, length(x), names(x), j_arg = j_arg, assign = FALSE)

    if (anyNA(j)) {
      cnd_signal(error_na_column_index(which(is.na(j))))
    }

    xo <- .subset(x, j)
    if (anyDuplicated(j)) {
      xo <- set_repaired_names(xo, repair_hint = FALSE, .name_repair = "minimal")
    }
    set_tibble_class <- function(x, nrow) {
      attr(x, "row.names") <- .set_row_names(nrow)
      tibble_class <- getFromNamespace("tibble_class","tibble")
      class(x) <- tibble_class
      x
    }
    fast_nrow <- getFromNamespace("fast_nrow","tibble")
    set_tibble_class(xo, nrow = fast_nrow(x))
  }
  tbl_subset_row <- function(x, i, i_arg) {
    if (is.null(i)) return(x)
    vectbl_as_row_index <- function(i, x, i_arg, assign = FALSE) {
      stopifnot(!is.null(i))
      fast_nrow <- getFromNamespace("fast_nrow","tibble")
      nr <- fast_nrow(x)

      if (is.character(i)) {
        is_na_orig <- is.na(i)

        if (has_rownames(x)) {
          i <- match(i, rownames(x))
        } else {
          i <- string_to_indices(i)
          fix_oob <- getFromNamespace("fix_oob","tibble")
          i <- fix_oob(i, nr, warn = FALSE)
        }
        fix_oob <- getFromNamespace("fix_oob","tibble")
        i <- fix_oob_invalid(i, is_na_orig)
        i
      } else if (is.numeric(i)) {
        fix_oob <- getFromNamespace("fix_oob","tibble")
        i <- fix_oob(i, nr)
        vectbl_as_row_location <- getFromNamespace("vectbl_as_row_location","tibble")
        vectbl_as_row_location(i, nr, i_arg, assign)
      } else {
        vectbl_as_row_location <- getFromNamespace("vectbl_as_row_location","tibble")
        vectbl_as_row_location(i, nr, i_arg, assign)
      }
    }
    vectbl_as_row_index <- getFromNamespace("vectbl_as_row_index","tibble")
    i <- vectbl_as_row_index(i, x, i_arg)
    xo <- lapply(unclass(x), vctrs::vec_slice, i = i)
    set_tibble_class <- function(x, nrow) {
      attr(x, "row.names") <- .set_row_names(nrow)
      tibble_class <- getFromNamespace("tibble_class","tibble")
      class(x) <- tibble_class
      x
    }
    set_tibble_class(xo, nrow = length(i))
  }
  tbl_subset2 <- getFromNamespace("tbl_subset2","tibble")
  vectbl_restore <- getFromNamespace("vectbl_restore","tibble")

  i_arg <- substitute(i)
  j_arg <- substitute(j)
  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  }
  else if (is.null(i)) {
    i <- integer()
  }
  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  }
  else if (is.null(j)) {
    j <- integer()
  }
  n_real_args <- nargs() - !missing(drop)
  if (n_real_args <= 2L) {
    if (!missing(drop)) {
      rlang::warn("`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`.")
      drop <- FALSE
    }
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
    if (is.matrix(j)) {
      return(tbl_subset_matrix(x, j, j_arg))
    }
  }
  xo <- tbl_subset_col(x, j = j, j_arg)
  if (!is.null(i)) {
    xo <- tbl_subset_row(xo, i = i, i_arg)
    .concepts <- .concepts[i]
  }
  if (drop && length(xo) == 1L) {
    xo <- tbl_subset2(xo, 1L, j_arg)
    attr(xo, "concept") <- .concepts
    xo
  }
  else {
    xo <- vectbl_restore(xo, x)
    attr(xo, "concept") <- .concepts
    xo
  }
}

#' Extract meta data
#' @keywords internal
extract_meta <- function(x, name = NA) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    out <- x
    out <- data.frame("$C" = "$C", out, check.names = F)
    colnames(out) <- c("$C", name)
    return(out)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    out <- as.data.frame(x)
    colnames(out) <- c(name, name)
    out <- data.frame("$I" = "$I", out, check.names = F)
    return(out)
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    out <- as.data.frame(x)
    out <- data.frame("$H" = "$H", hist = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    out <- as.data.frame(x)
    out <- data.frame("$M" = "$M", modal = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_set")) {
    out <- as.data.frame(x)
    out <- data.frame("$S" = "$S", modal = ncol(out), out, check.names = F)
    colnames(out)[2] <- name
    return(out)
  }
}

#' Extract data
#' @keywords internal
extract_data <- function(x, name = NA) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    out <- data.frame(x, check.names = F)
    colnames(out) <- name
    return(out)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    out <- as.data.frame(x)
    colnames(out) <- c(name, name)
    return(out)
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    out <- as.data.frame(x)
    return(out)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    out <- as.data.frame(x)
    return(out)
  }

  if (any(class(x) %in% "symbolic_set")) {
    out <- as.data.frame(x)
    return(out)
  }
}

#' Extract length
#' @export
#' @keywords internal
var.length <- function(x,...) {
  if (any(class(x) %in% c("numeric", "integer"))) {
    return(1)
  }

  if (any(class(x) %in% "symbolic_interval")) {
    return(2)
  }

  if (any(class(x) %in% "symbolic_modal")) {
    return(length(x[[1]]$var))
  }

  if (any(class(x) %in% "symbolic_set")) {
    return(length(levels(x[[1]])))
  }

  if (any(class(x) %in% "symbolic_histogram")) {
    return(length(x[[1]]$breaks) - 1)
  }
}


#' to.v2
#' @keywords internal
to.v2 <- function(x) {
  out <- list()
  out$N <- nrow(x)
  out$M <- ncol(x)
  out$sym.obj.names <- attr(x, "concept")
  out$sym.var.names <- colnames(x)

  meta <- lapply(colnames(x), function(y) {
    extract_meta(x[y][[1]], name = y)
  })
  meta <- do.call("cbind", meta)
  rownames(meta) <- attr(x, "row.names")

  types <- which(stringr::str_detect(colnames(meta), "\\$\\w"))
  out$sym.var.types <- as.character(na.omit(stringr::str_extract(colnames(meta), "\\$\\w")))
  out$sym.var.length <- unname(sapply(x[, out$sym.var.names], var.length))
  out$sym.var.starts <- ifelse(out$sym.var.types %in% c("$I", "$C"), 1, 2) + types
  out$meta <- meta
  out$data <- lapply(colnames(x), function(y) {
    extract_data(x[y][[1]], name = y)
  })
  out$data <- do.call("cbind", out$data)
  rownames(out$data) <- attr(x, "row.names")
  class(out) <- "sym.data.table"
  return(out)
}

#' to.v3
#' @keywords internal
to.v3 <- function(x) {

  out <- tibble::tibble(.rows = length(x$sym.obj.names))
  for (i in seq_len(x$M)) {
    if (x$sym.var.types[i] == "$I") {
      values <- x[, i]$data
      new_interval <- new.sym.intreval(values[, 1], values[, 2])
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_interval)
    }

    if (x$sym.var.types[i] == "$S") {
      values <- x[, i]$data
      new_set <- c()
      for (.i in seq_len(nrow(values))) {
        .f <- factor(colnames(values)[as.logical(values[.i, ])],
          levels = colnames(values)
        )
        new_set <- vctrs::vec_c(new_set, new.sym.set(.f))
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_set)
    }

    if (x$sym.var.types[i] == "$M") {
      values <- x[, i]$data
      new_modal <- c()
      for (.i in seq_len(nrow(values))) {
        .m <- list(
          var = colnames(values),
          prop = as.numeric(values[.i, ])
        )
        .m <- vctrs::new_vctr(list(.m), class = "symbolic_modal")
        new_modal <- vctrs::vec_c(new_modal, .m)
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_modal)
    }

    if (x$sym.var.types[i] == "$H") {
      values <- x[, i]$data
      .breaks <- as.numeric(unique(unlist(stringr::str_extract_all(colnames(values), "(\\d+\\.\\d+)"))))
      new_histogram <- c()
      for (.i in seq_len(nrow(values))) {
        .h <- list(
          breaks = .breaks,
          props = as.numeric(values[.i, ])
        )
        .h <- vctrs::new_vctr(list(.h), class = "symbolic_histogram")
        new_histogram <- vctrs::vec_c(new_histogram, .h)
      }
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := new_histogram)
    }
    if (x$sym.var.types[i] == "$C") {
      values <- x[, i]$data
      values <- as.numeric(values[[1]])
      name <- x$sym.var.names[i]
      out <- tibble::add_column(out, {
        {
          name
        }
      } := values)
    }
  }

  attr(out, "concept") <- x$sym.obj.names
  class(out) <- c("symbolic_tbl", class(out))
  return(out)
}
