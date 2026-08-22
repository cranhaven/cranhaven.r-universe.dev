## Hierarchy list ----

#' `Survstat` option accessor
#'
#' `Survstat` options are values that may have children.
#'
#' @param x the options
#' @param y the item
#'
#' @return the value of the list item or an error if it does not exist
#' @export
#' @concept transpose
#' @keywords internal
`$.survstat_option` <- function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab <- deparse(substitute(y))
  }
  sublist = attr(x, "children")
  if (is.null(sublist)) {
    return(NULL)
  }
  sublist[[ylab]]
}

# registerS3method("$", "survstat_option", `$.survstat_option`)

#' Support for auto suggests on `survstat_option`s
#'
#' @param x a `survstat_option`
#' @param pattern a matching pattern
#' @returns the names of the children
#' @exportS3Method utils::.DollarNames
#' @keywords internal
.DollarNames.survstat_option <- function(x, pattern) {
  sublist = attr(x, "children")
  if (is.null(sublist) || length(sublist) == 0) {
    return(character())
  }
  return(grep(pattern, names(sublist), value = TRUE, fixed = TRUE))
  # return(utils::findMatches(pattern, names(sublist)))
}

# registerS3method(
#   ".DollarNames",
#   "survstat_option",
#   .DollarNames.survstat_option
# )

#' @export
print.survstat_option = function(x, ...) {
  cat(format.survstat_option(x))
}

#' @export
format.survstat_option = function(x, ...) {
  cl = setdiff(class(x), c("survstat_option"))
  tmp = x
  chil = length(attr(x, "children"))
  attributes(tmp) = NULL
  paste0(c(format(tmp, ...), sprintf("<%d children>", chil)), collapse = "\n")
}


# registerS3method("print", "survstat_option", print.survstat_option)
# registerS3method("format", "survstat_option", format.survstat_option)

# This needs to be defined before being added to parents.
as.survstat_option = function(x, children) {
  if (!is.null(children) && length(children) > 0 && is.null(names(children))) {
    stop("children must be named")
  }
  if (!is.null(children)) {
    attr(x, "children") = .fix_names(children)
  }
  attr(x, "class") = unique(c("survstat_option", class(x)))
  x
}

is.survstat_option = function(x) {
  inherits(x, "survstat_option")
}

get_children = function(x) {
  return(attr(x, "children"))
}

.fix_names = function(children) {
  new_names = names(children)
  new_names = tolower(gsub("[^a-zA-Z0-9]+", "_", trimws(new_names)))
  names(children) = new_names
  return(children)
}

map_options = function(value, data, children_fn, names_fn, values_fn) {
  children_fn = .silently(children_fn)
  names_fn = .silently(names_fn)
  values_fn = .silently(values_fn)

  child_list = try(children_fn(data), silent = TRUE)
  if (
    inherits(child_list, "try-error") ||
      length(child_list) == 0 ||
      all(is.na(child_list))
  ) {
    return(value)
  }
  values = values_fn(data)
  child_names = names_fn(data)
  options = map_option_list(
    child_names,
    values,
    child_list,
    children_fn,
    names_fn,
    values_fn
  )
  # options = lapply(seq_along(child_list), function(i) {
  #   child_data = child_list[[i]]
  #   child_value = values[[i]]
  #   return(map_options(
  #     child_value,
  #     child_data,
  #     children_fn,
  #     names_fn,
  #     values_fn
  #   ))
  # })
  # names(options) = child_names
  return(as.survstat_option(value, options))
}

map_option_list = function(
  names,
  values,
  list_data,
  children_fn,
  names_fn,
  values_fn
) {
  children_fn = .silently(children_fn)
  names_fn = .silently(names_fn)
  values_fn = .silently(values_fn)

  options = lapply(seq_along(list_data), function(i) {
    child_data = list_data[[i]]
    child_value = values[[i]]
    return(map_options(
      child_value,
      child_data,
      children_fn,
      names_fn,
      values_fn
    ))
  })
  names(options) = names
  return(.fix_names(options))
}

.silently = function(fn) {
  fn = rlang::as_function(fn)
  return(fn)
  # return(function(...) {
  #   tmp = try(
  #     suppressMessages(suppressWarnings(
  #       fn(...)
  #     )),
  #     silent = TRUE
  #   )
  #   if (inherits(tmp, "try-error")) {
  #     return(NULL)
  #   }
  #   tmp
  # })
}

map_option_data = function(data, children_fn, names_fn, values_fn) {
  children_fn = .silently(children_fn)
  names_fn = .silently(names_fn)
  values_fn = .silently(values_fn)

  child_list = children_fn(data)
  values = values_fn(data)
  child_names = names_fn(data)

  return(map_option_list(
    child_names,
    values,
    child_list,
    children_fn,
    names_fn,
    values_fn
  ))
}


.match_values = function(hierarchy_id) {
  .traverse(hierarchy_id, hierarchy_list, value_list)
}

.traverse = function(hierarchy_id, hroot, vroot) {
  if (identical(hroot, hierarchy_id)) {
    return(get_children(vroot))
  }
  if (is.survstat_option(hroot)) {
    hbranch = get_children(hroot)
    vbranch = get_children(vroot)
    matches = lapply(names(hbranch), function(nm) {
      .traverse(hierarchy_id, hbranch[[nm]], vbranch[[nm]])
    })
    return(unlist(matches))
  }
  if (is.list(hroot)) {
    matches = lapply(names(hroot), function(nm) {
      .traverse(hierarchy_id, hroot[[nm]], vroot[[nm]])
    })
    return(unlist(matches))
  }
}
