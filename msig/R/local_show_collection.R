#' Show collections of local MsigDB database
#'
#' @return A dataframe contains 2 columns. The first column is the name of the collection.
#'     The second column is the number of frequencies it has.
#' @export
#'
#' @examples
#' show_local_collection()
#'
show_local_collection <- function(){
    df <- data.frame(table(msigdb$collection))
    colnames(df)[1]='collection'
    df
}

