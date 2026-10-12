#' get 'LIFX' API rate limit
#' @template param_token
#' @return a named vector of three numbers:
#'
#' 1. `limit`: The rate limit
#' 2. `remaining`: how many calls are remaining
#' 3. `reset`: the Unix timestamp for when the next window begins. Usually every minute.
#' @examples
#' \dontrun{lx_rate_limit()}
#' @export
lx_rate_limit <- function(token = lx_get_token()) {
    light_list <- lx_list_lights(token = token)
    attr(light_list,which = "rate_limits")
}
