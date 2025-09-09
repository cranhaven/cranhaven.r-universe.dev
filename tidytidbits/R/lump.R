# Copied from forcats,
# (c) Hadley Wickham, <hadley@rstudio.com>
# License: GPL-3
# Changed to a generic lump function which lumps for given, corresponding level and count vectors

#' Generic lumping
#'
#' Takes levels (labels, factor levels) and corresponding counts
#' and "lumps" according to specified criteria (either n or prop), i.e.
#' preserves some rows and summarises the rest in a single "Other" row
#'
#' @param levels Vector of levels
#' @param count Vector of corresponding counts
#' @param n If specified, n rows shall be preserved.
#' @param prop If specified, rows shall be preserved if their count >= prop
#' @param other_level Name of the "other" level to be created from lumped rows
#' @param ties.method Method to apply in case of ties
#'
#' @return A dictionary (named vector) of levels -> new levels
#' @export
lump <- function(levels, count, n, prop, other_level = "Other",
                 ties.method = c("min", "average", "first", "last", "random", "max"))
{
  ties.method <- match.arg(ties.method)
  if (length(levels) != length(count))
  {
    stop("lump: Levels and count must have the same length")
  }

  levels <- as.character(levels)

  if (!xor(missing(n), missing(prop)))
  {
    new_levels <- ifelse(!.in_smallest(count), levels, other_level)
  }
  else if (!missing(n))
  {
    if (n < 0)
    {
      rank <- rank(count, ties = ties.method)
      n <- -n
    }
    else
    {
      rank <- rank(-count, ties = ties.method)
    }

    new_levels <- ifelse(rank <= n, levels, other_level)
  }
  else if (!missing(prop))
  {
    if (prop < 0)
    {
      new_levels <- ifelse(count <= -prop, levels, other_level)
    }
    else
    {
      new_levels <- ifelse(count >= prop, levels, other_level)
    }
  }

  set_names(new_levels, levels)
}

# Lump together smallest groups, ensuring that the collective
# "other" is still the smallest group. Assumes x is vector
# of counts in descending order
.lump_cutoff <- function(x) {
  left <- sum(x)

  for (i in seq_along(x)) {
    # After group, there are this many left
    left <- left - x[i]

    if (x[i] > left)
      return(i + 1)
  }

  length(x) + 1
}

# Given vector of counts, returns logical vector if in
# smallest groups
.in_smallest <- function(x) {
  ord_x <- order(x, decreasing = TRUE)
  idx <- .lump_cutoff(x[ord_x])

  to_lump <- seq_along(x) >= idx
  # Undo initial ordering
  to_lump[order(ord_x)]
}
