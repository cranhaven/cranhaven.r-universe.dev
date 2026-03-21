
#' collect_ids
#'
#' Function will parse all the files of BMRB nmr-star 3.1 repo and return all the available files that are available for downloading.
#' @import xml2 rvest
#' @param base_url (optional) The BMRB entry list page for nmr-star3.1, http://www.bmrb.wisc.edu/ftp/pub/bmrb/entry_lists/nmr-star3.1/.
#' @param to_list (optional) whether to output as a list of ids.
#' @return BMRB_files. This could be a list of ids if output, if 'to_list' is set to be True, otherwise, it will return a html table.
#' @examples
#' # collect_ids(to_list=TRUE) # It will take more than 5 sec
#' @export collect_ids

collect_ids <- function(base_url="http://www.bmrb.wisc.edu/ftp/pub/bmrb/entry_lists/nmr-star3.1/", to_list = FALSE) {
        print("Parsing data, it might take a while ...\n")
        webpage<- xml2::read_html(base_url)
        table <-rvest::html_nodes(webpage, "table")
        table <- rvest::html_table(table, fill = TRUE)[[1]]
        BMRB_files <- table[-c(1,2), -c(1,5)]
        if (to_list) {
                return(BMRB_files["Name"])
        }else{
                return(BMRB_files)
        }

}
