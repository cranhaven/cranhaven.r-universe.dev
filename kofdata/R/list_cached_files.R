#' Show Cached Files of Specific User
#'
#' Some users get individually composed, cached files. If you ordered such 
#' a service you can list all files available to you. 
#' 
#' @inheritParams param_defs
#' @param username Your dataservice user name 
#' @examples 
#' available_files <- try(list_cached_files("kofdatapkg",
#' "313984fcd9f343d3961891319b0ed321"))
#' available_files
#' @import httr
#' @export 
list_cached_files <- function(username, api_key) {
  .get_cdc_files(username, api_key)$files$name
}
