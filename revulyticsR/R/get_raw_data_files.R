#' Get Raw Data Files
#' 
#' Retrieves a list of raw data file exports that are available for a 
#' list of product IDs and the download URL for each file.
#' 
#' Raw data files are an add-on service available through Revenera. If
#' these files are available they can be downloaded manually from the
#' user portal, or downloaded via R. This function uses the API to 
#' first retrieve the list of files, and then get the download URL for
#' each file.
#' 
#' It is not recommended that your username be stored directly in your
#' code. There are various methods and packages available that are more 
#' secure; this package does not require you to use any one in particular.
#' 
#' @param rev_product_ids A vector of revulytics product id's for which
#' you want active user data.
#' @param rev_session_id Session ID established by the connection to
#' Revulytics API. This can be obtained with revulytics_auth().
#' @param rev_username Revulytics username.
#' 
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom purrr "map_dfr"
#' @import httr
#' @import jsonlite
#' 
#' @return Data frame with available files and URLs.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' rev_user <- "my_username"
#' rev_pwd <- "super_secret"
#' product_ids_list <- c("123", "456", "789")
#' session_id <- revulytics_auth(rev_user, rev_pwd)  
#' files_df <- get_raw_data_files(product_ids_list, session_id, rev_user)
#' file_list <- dplyr::pull(files_df, var = file_name)
#' for (f in file_list){
#'   url <- dplyr::filter(files_df, file_name == f) %>% dplyr::pull(download_url)
#'   download.file(url, mode = "wb", destfile = "download_file_location.zip")
#' }
#' }


get_raw_data_files <- function(rev_product_ids, rev_session_id, rev_username) {
  
  . <- NA # prevent variable binding note for the dot in the get_by_product function
  get_by_product <- function(x) {
    get_files_body <- list(
      user = rev_username,
      sessionId = rev_session_id,
      productId = x
    )
    
    get_files_request <- httr::RETRY("POST",
                           url = "https://api.revulytics.com/rawEvents/download/listFiles",
                           body = get_files_body,
                           encode = "json",
                           times = 4,
                           pause_min = 10,
                           terminate_on = NULL,
                           terminate_on_success = TRUE,
                           pause_cap = 5)
    check_status(get_files_request)
    
    request_content <- httr::content(get_files_request, "text", encoding = "ISO-8859-1")
    content_json <- jsonlite::fromJSON(request_content, flatten = TRUE)
    files_df <- as.data.frame(content_json[2])
    file_list <- pull(files_df, 1)
  
    get_download_urls <- function(filenm){
      download_body <- list(
        user = rev_username,
        sessionId = rev_session_id,
        productId = x,
        fileName = filenm
      )
      download_request <- httr::RETRY("POST",
                                      url = "https://api.revulytics.com/rawEvents/download/getDownloadUrl",
                                      body = download_body,
                                      encode = "json",
                                      times = 4,
                                      pause_min = 10,
                                      terminate_on = NULL,
                                      terminate_on_success = TRUE,
                                      pause_cap = 5)
      request_content <- httr::content(download_request, "text", encoding = "ISO-8859-1")
      content_json <- jsonlite::fromJSON(request_content, flatten = TRUE)
      file_url_df <- as.data.frame(content_json[[2]]) %>%
        mutate(file_name = filenm) %>%
        left_join(files_df, by = c("file_name" = "fileList.fileName")) %>%
        rename(download_url = 1, file_date = 3, file_size_kb = 4)
      return(file_url_df)
    }
    all_file_url_df <- purrr::map_dfr(file_list, get_download_urls)
    return(all_file_url_df)
  }
  all_pids_df <- purrr::map_dfr(rev_product_ids, get_by_product)
  return(all_pids_df)
}

