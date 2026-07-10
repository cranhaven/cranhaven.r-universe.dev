#' Show contributor_orgs of local MsigDB database
#'
#' @return A dataframe contains 2 columns. The first column is the name of the contributor_org.
#'     The second column is the number of frequencies it has.
#' @export
#'
#' @examples
#' show_local_contributor_org()
#'
show_local_contributor_org <- function(){
    df <- data.frame(table(msigdb$contributor_org))
    colnames(df)[1]='contributor_org'
    df
}

