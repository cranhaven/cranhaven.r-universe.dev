
#' Download MsigDB database
#'
#' @param version version
#'
#' @return dowanload the data to local PC
#' @export
#'
msig_download <- function(version){
    url <- 'https://data.broadinstitute.org/gsea-msigdb/msigdb/release/%s/msigdb_v%s.xml'
    url2 <- sprintf(url,version,version)
    download.file(url = url2,
                  destfile = sprintf('msigdb_v%s.xml',version))
}
