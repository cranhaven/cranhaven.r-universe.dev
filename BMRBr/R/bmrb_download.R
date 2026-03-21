#' bmrb_download
#'
#' Main function that download the BMRB file from www.bmrb.wisc.edu repo. It will download file in nmr-star3.1 format.
#' @import utils
#' @param id_list  (Required) A list of file ids that are corresponding to the requested files.
#' @param output_dir (Required) Location were downloaded file will be saved.
#' @param base_url Default location is http://www.bmrb.wisc.edu/ftp/pub/bmrb/entry_lists/nmr-star3.1/.
#' @param verbose Boolean parameter. If set to be "True", the downloader will output detailed results in the console.
#' @return Save file in the output_dir location
#' @examples
#' download_dir = tempdir()
#' bmrb_download(965, download_dir)
#' @export bmrb_download





bmrb_download <- function(id_list, output_dir, base_url = "http://www.bmrb.wisc.edu/ftp/pub/bmrb/entry_lists/nmr-star3.1", verbose=TRUE) {
        prefix <- "bmr"
        extension <- "str"
        if(!dir.exists(file.path(output_dir))) {dir.create(file.path(output_dir))}
        id_list <- gsub('([[:alpha:]]+)', '', id_list)
        print("Downloading...")

        for (bmrb_id in id_list) {
                file_url = paste0(base_url, "/", prefix, bmrb_id, ".", extension)
                file_name = paste0(output_dir, "/", prefix, bmrb_id, ".", extension)
                if (file.exists(file_name)) {
                        print(paste0("Skipping ", prefix, bmrb_id, ", it's already exists."))
                }
                else{
                        tryCatch(
                                {
                                        if (verbose) {
                                                cat(paste0("Processing: ", prefix, bmrb_id))
                                        }
                                        utils::download.file(file_url, file_name, quiet = TRUE)
                                        print(paste0("Downloaded: ", prefix, bmrb_id))
                                },

                                error = function(e) {
                                        print(e)
                                }
                          )
                }

        }

}
