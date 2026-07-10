#' Show contributors of local MsigDB database
#'
#' @return A dataframe contains 2 columns. The first column is the name of the contributor.
#'     The second column is the number of frequencies it has.
#' @export
#'
#' @examples
#' show_local_contributor()
#'
show_local_contributor <- function(){
    df <- data.frame(table(msigdb$contributor))
    colnames(df)[1]='contributor'
    df
}

