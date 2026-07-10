#' Convert package description file to dataframe
#'
#' @param desc description file path
#'
#' @return One dataframe with column names of field
#' @export
#'
desc2df <- function(desc){
    fields <- desc::desc_fields(desc)
    field_content <- sapply(fields, function(i) desc::desc_get_field(key = i,
                                                                     default = NA,
                                                                     trim_ws = TRUE,
                                                                     file = desc))
    
    mt <- matrix(field_content,nrow = 1,dimnames = list(NULL,fields))
    data.frame(mt,check.names = FALSE,stringsAsFactors = FALSE)
}
