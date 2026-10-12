#' list available lights
#' @template param_selector
#' @template param_token
#' @return a list with each item representing one light. Each item itself is a list with all relevant information about the light and it's state
#' @details each item in the returned list contains (depending on the type of lamp), the following named items:
#'
#' \itemize{
#'  \item{Reachability: }{\code{connected, last_seen, seconds_since_seen}}
#'  \item{Light identifiers / selectors: }{\code{id, uuid, label,  group, location}}
#'  \item{Status: }{\code{power, color, brightness, effect}}
#'  \item{Hardware information: }{\code{product}}
#' }
#'
#' @examples
#' \dontrun{
#' lx_list_lights()
#'
#' lights <- lx_list_lights(
#'   lx_selector(location = "kitchen")
#' )
#'
#' first_kitchen_light <- lights[[1]]
#'
#' first_kitchen_light$power
#' first_kitchen_light$color$hue
#' first_kitchen_light$color$saturation
#'
#' first_kitchen_light$group
#'
#' }
#' @export
lx_list_lights <- function(selector = "all", token = lx_get_token()) {
    list <- lx_GET(selector = selector, endpoint = "", token = token)
    rate_limits <- c(limit = as.numeric(list$headers$`x-ratelimit-limit`), remaining = as.numeric(list$headers$`x-ratelimit-remaining`),
                     reset = as.numeric(list$headers$`x-ratelimit-reset`))
    list <- httr::content(list)
    attributes(list)<-list("rate_limits" = rate_limits)
    list
}
