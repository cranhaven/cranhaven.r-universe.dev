#' translate_legacy_keys
#' 
#' Gives ts_keys for the given legacy keys.
#' @param legacy_keys A vector of legacy keys for which to find new keys
#' @param chunksize integer to set chunksize in order to avoid URLs that are too long. 
#' @return A list of vectors of new ts_keys
#' @import httr
#' @import jsonlite
#' @export
translate_legacy_keys <- function(legacy_keys, chunksize = 100){
  .Defunct(msg = "The underlying endpoint is no longer operational. Please contact a system administrator if 
           you need to handle legacy identifiers.")
  # url <- "https://datenservice.kof.ethz.ch/api/v1/metadata/translatelegacykeys"
  # 
  # 
  # chunk_list <- split(legacy_keys,ceiling(seq_along(legacy_keys)/chunksize))
  # 
  # result <- lapply(chunk_list,function(x){
  #   l_keys <- paste(x, collapse=",")
  #   response <- GET(url, query = list(keys = l_keys))
  #   if(response$status_code != 200){
  #     stop(sprintf("web request not successful, http error %s returned",
  #                  as.character(response$status_code)))
  #   }
  #   result <- fromJSON(content(response, as="text"))
  # })
  # 
  # 
  # out <- unlist(result,recursive = F)
  # names(out) <- gsub("[0-9]+\\.","",names(out))
  # out
}



