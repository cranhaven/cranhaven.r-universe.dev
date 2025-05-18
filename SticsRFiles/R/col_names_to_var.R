#' @title  Convert columns names list to STICS variables names
#' @description Change the `varname.n.` or `varname_n` by `varname(n)`
#' or snake case names (var_name.n., var_long_name_n),
#' (n can numeric) other names are unchanged
#' @param var_list Columns names list
#'
#' @return List of STICS variables names
#'
#' @examples
#' col_names <- c("var1", "var2_n", "var3.1.")
#' valid_names <- col_names_to_var(col_names)
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom dplyr %>%
#'
col_names_to_var <- function(var_list = c()) {
  . <- NULL

  words <- strsplit(var_list, "_")

  # Getting the index of the variables to convert
  #
  idx_end_convert <- unlist(lapply(X = words, function(x) {
    end_conv <- grepl("[n | 0-9*]|nboite|nboite-1|ao|as",
                      x[length(x)])
    if (end_conv) return(TRUE)
    FALSE
  }))

  # to filter varname_12_20
  idx_end_convert <- idx_end_convert &
    !grepl("_\\d{1,2}_\\d{1,2}.{,1}$", var_list)

  # Nothing to do, t=returning the input vector
  if (any(idx_end_convert)) {
    conv_var_list <- var_list[idx_end_convert]
  } else {
    return(var_list)
  }

  # for varname_n, var_name_n,
  # for varname_1, var_name_1,
  # TODO: fix the case var_name_1_2
  conv_var_list <-
    gsub("_(n{1}|\\d{1,2}|nboite|nboite-1|ao|as)$",
         "(\\1",
         conv_var_list)

  any_opening <- grepl("\\(", conv_var_list)

  # Closing ) only if ( is present
  conv_var_list[any_opening] <-
    lapply(conv_var_list[any_opening], function(x) {
      if (!grepl(pattern = "\\)$", x)) x <- paste0(x, ")")
      return(x)
    })


  # for varname.n., or var_name.n.
  conv_var_list <-
    gsub("\\.$", ")", conv_var_list) %>%
    gsub("\\.", "(", .)

  # Setting values of the converted names
  var_list[idx_end_convert] <- conv_var_list

  return(var_list)
}
