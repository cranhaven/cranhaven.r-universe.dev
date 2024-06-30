#' check_all_fields
#'
#' checks all elements of the parameter table
#' @param parameter_table parameter table
#' @returns nothing
#' @keywords internal
check_all_fields <- function(parameter_table){
  if(any(!grepl(pattern = "^[a-zA-Z0-9_]+$", x = parameter_table$parameter_table$lhs)))
    stop("The following left hand side does not match the allowed pattern of letters",
         " digits and underscores:", paste0(parameter_table$parameter_table$lhs[
           which(!grepl(pattern = "^[a-zA-Z0-9_]+$", x = parameter_table$parameter_table$lhs))],
           collapse = ",")
    )
  if(any(!grepl(pattern = "^[a-zA-Z0-9_]+$", x = parameter_table$parameter_table$rhs)))
    stop("The following right hand side does not match the allowed pattern of letters",
         " digits and underscores:",
         paste0(parameter_table$parameter_table$rhs[
           which(!grepl(pattern = "^[a-zA-Z0-9_]+$", x = parameter_table$parameter_table$rhs))
         ],
         collapse = ","))
  if(any(!grepl(pattern = "^\\{", x = parameter_table$user_defined)))
    stop("The following syntax is not allowed: ", paste0(parameter_table$user_defined[
      which(!grepl(pattern = "^\\{", x = parameter_table$user_defined), ".")
    ],
    collapse = ","))

  if(any(!grepl(pattern = "\\}$", x = parameter_table$user_defined)))
    stop("The following syntax is not allowed: ",
         paste0(parameter_table$user_defined[
           which(!grepl(pattern = "\\}$", x = parameter_table$user_defined), ".")
         ],
         collapse = ","))
}
