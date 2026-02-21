#' @export
#' @import RCurl
#' @import readr
#'
#' @title Update last HGNC correspondence database
#' @description
#' This function is used to update gene symbol correspondence from HGNC database
#' @name update_symbols
#' @usage
#' update_symbols(url=NULL)
#' @param url user can provide url (default is NULL)
#' @examples
#'
#' \donttest{
#' HGNC <- update_symbols()
#' }
#' @return returns a data.frame containing gene symbols with status, previous symbols and synonyms as well as their corresponding entrezIDs
#'

update_symbols <- function(url = NULL) {
  message("Fetching url...")
  if (is.null(url)) {
    # url <- "https://www.genenames.org/cgi-bin/download/custom?col=gd_app_sym&col=gd_status&col=gd_prev_sym&col=gd_aliases&col=gd_pub_eg_id&status=Approved&status=Entry%20Withdrawn&hgnc_dbtag=on&order_by=gd_app_sym_sort&format=text&submit=submit"
     url <- "https://www.genenames.org/cgi-bin/download/custom?col=gd_app_sym&col=gd_status&col=gd_prev_sym&col=gd_aliases&col=gd_pub_eg_id&col=gd_pub_ensembl_id&status=Approved&status=Entry%20Withdrawn&hgnc_dbtag=on&order_by=gd_app_sym_sort&format=text&submit=submit"
  }
  if (url.exists(url)) {
    message("Accessing data...")
    HGNC <- read_delim(url, "\t", escape_double = FALSE, trim_ws = TRUE)
    HGNC <- data.frame(HGNC)
    message("Checking validity...")
    # expected <- c("Approved.symbol", "Status", "Previous.symbols", "Alias.symbols", "NCBI.Gene.ID")
    expected <- c("Approved.symbol", "Status", "Previous.symbols", "Alias.symbols", "NCBI.Gene.ID","Ensembl.gene.ID")
    if (dim(HGNC)[2] != 6) {
      warning(paste(paste(expected, collapse = ", "), "	 should be in HGNC query"))
      stop(paste("HGNC table should contain the 6 columns
"))
    } else if (!identical(colnames(HGNC), expected)) {
      warning(paste(paste(expected, collapse = ", "), "	 not found in HGNC query"))
      stop(paste("HGNC table should contain the 6 columns
"))
    }

    name.and.status <- paste(HGNC$Approved.symbol, ifelse(HGNC$Status == "Symbol Withdrawn", "~withdrawn", ""), sep = "")
    HGNC$Approved.symbol <- name.and.status
    HGNC <- HGNC[, -c(2)]
    message("done...")
    return(HGNC)
  } else {
    warning("url not found. Process aborted, please refer to documentation.")
  }
}
