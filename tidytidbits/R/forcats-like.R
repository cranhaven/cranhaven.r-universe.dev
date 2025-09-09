
#' Rename and reorder a factor.
#'
#' The factor will be recoded according to value_label_dict and, if requested,
#' also reordered by the order of this vector.
#' Secondly, the vector will be reordered according to reorder_vector, if given.
#'
#' @param .f A factor or vector (if .f is not yet a factor, it is made one)
#' @param value_label_dict a dictionary (named list or vector) of old->new factor levels
#' @param reorder_vector vector of factor levels (the new levels according to value_label_dict).
#'    It need not contain all levels, only those found will be reorderer first
#' @param reorder_by_value_label_dict Should the factor also be reordered following the order of value_label_dict?
#'
#' @return A renamed and reordered factor
#' @export
#'
#' @seealso \code{\link{rename_factor}}, \code{\link{order_factor_by}},
#'    \code{\link[forcats]{fct_recode}}, \code{\link[forcats]{fct_relevel}}
rename_reorder_factor <- function(.f,
                                  value_label_dict,
                                  reorder_vector,
                                  reorder_by_value_label_dict = T)
{
  if (!invalid(value_label_dict) && !is_dictionaryish(value_label_dict))
  {
    stop("value_label_dict must be a dictionary")
  }
  if (!is.factor(.f))
  {
    .f <- factor(.f)
  }
  if (!invalid(value_label_dict))
  {
    # recode wants the new level as the name and the old as the value, new:old.
    # the notion of a dictionary is IMO old:new
    .f <- do.call(forcats::fct_recode, c(list(.f), invert_value_and_names(value_label_dict)))
    if (reorder_by_value_label_dict)
    {
      new_order <- unique(value_label_dict)
      .f <- forcats::fct_relevel(.f, new_order)
    }
  }
  if (!invalid(reorder_vector))
  {
    .f <- forcats::fct_relevel(.f, reorder_vector)
  }
  .f
}

#' Rename a factor.
#'
#' Renames the levels of a factor.
#'
#' @param .f A factor or vector (if .f is not yet a factor, it is made one)
#' @param reorder Logical: If True, the levels will additionally be reordered
#'    in the order of first appearance in the arguments
#' @param ... Dictionaryish arguments, named by old level, value is new level ("old level" = "new level").
#'    You can pass single named arguments, or named vectors or named lists, which will be spliced.
#'
#' @return A renamed and reordered factor
#' @export
#'
#' @seealso \code{\link{rename_reorder_factor}}, \code{\link{order_factor_by}},
#'    \code{\link[forcats]{fct_recode}}, \code{\link[forcats]{fct_relevel}}
rename_factor <- function(.f,
                          ...,
                          reorder = F)
{
  # chr() splices bare lists and !!! lists as well as named vectors
  value_label_dict <- chr(...)
  rename_reorder_factor(.f, value_label_dict, reorder_vector = NULL,
                        reorder_by_value_label_dict = reorder)
}


#' Reorder a factor
#'
#' Makes f a factor ordered according to ... (which is passed to order)
#'
#' This is a thin wrapper around \code{\link[forcats]{fct_reorder}()}, which is unintuitive in conjunction with order().
#'
#' @param .f A factor
#' @param ... Passed to  \code{\link{order}()}. Should be vectors of the same size as .f.
#'
#' @return Reordered factor
#' @export
#' @seealso \code{\link{rename_reorder_factor}}, \code{\link{rename_factor}}, \code{\link[forcats]{fct_reorder}}
order_factor_by <- function(.f, ...)
{
  fct_reorder(.f, order(order(...)))
}
