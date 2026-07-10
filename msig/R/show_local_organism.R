#' Show sub_collections of local MsigDB database
#'
#' @return A dataframe contains 2 columns. The first column is the name of the sub_collection.
#'     The second column is the number of frequencies it has.
#' @export
#'
#' @examples
#' show_local_sub_collection()
#'
show_local_sub_collection <- function(){
    df <- data.frame(table(msigdb$sub_collection))
    colnames(df)[1]='sub_collection'
    df
}

