



#' picking a color by name or hsbk
#' @template param_colors
#' @template param_color_name
#' @param check if FALSE does not call the API to check if the color is valid
#' @template param_token
#' @examples
#'
#' \dontrun{
#'
#' strong_red <- lx_color_name(hue = 0, saturation = 1, brightness = 1)
#' lx_color(color_name = strong_red)
#'
#' dim_green <- lx_color_name(color_name = "#00FF00", saturation = 1, brightness = 0.1)
#' lx_color(color_name = dim_green)
#'
#' unsaturated_cyan <- lx_color_name(color_name = "cyan", saturation = 0.3)
#' lx_color(color_name = unsaturated_cyan)
#' }
#'
#' @return a character string specifying a light color as expected by the 'LIFX' API. Outputs from this function have their own class and printing style, but a pure character string can be used just as well.
#' @export
lx_color_name <- function(hue = NULL, saturation = NULL, brightness = NULL, kelvin = NULL, color_name = NULL, check = TRUE, token = lx_get_token()) {

    inputs <- list(hue, saturation, brightness, kelvin)
    names(inputs) <- c("hue", "saturation", "brightness", "kelvin")
    inputs <- unlist(inputs)
    # generate a string of color information as the 'LIFX' api expects it (https://api.developer.lifx.com/docs/colors)
    if (length(inputs) == 0) {
        color_string <- ""
    } else {
        color_string <- paste(names(inputs), ":", inputs, collapse = " ", sep = "")
    }

    color_string <- paste(color_name, color_string)
    # call api to validate color
    if(check){
    if (!lx_check_color(color_string, token = token)) {
        stop("invalid color specification.")
    }
    }
    class(color_string) <- c("lx_color_string", class(color_string))
    return(color_string)
}


#' @method print lx_color_string
#' @export
print.lx_color_string <- function(x, ...) {
    cat(crayon::silver("lx color:"))
    cat(crayon::italic(x))
    invisible(x)
}




#' check if 'LIFX' color name is valid
#' @param color_name a color string in 'LIFX' api format (can be made with \code{\link{lx_color_name}} )
#' @template param_token
#' @details calls the API endpoint https://api.lifx.com/v1/color to check if the color is valid.
#' @examples
#' \dontrun{
#' lx_check_color("INVALID_COLOR_NAME") # invalid 'LIFX' color string returns FALSE
#' lx_check_color("#FFFFFF") # valid 'LIFX' color string returns TRUE
#' lx_check_color("orange") # valid 'LIFX' color string returns TRUE
#' lx_check_color('brightness:1 hue:50') # valid 'LIFX' color string returns TRUE
#' }
#' @return logical TRUE if the color name is valid; FALSE if not; throws an error if the API could not be reached or another issue occurred.
#' @export
lx_check_color <- function(color_name,  token = lx_get_token()) {
  url <- paste0("https://api.lifx.com/v1/color?string=", utils::URLencode(color_name))
  header <- lx_auth(token)
  response <- httr::GET(url, header)
  if (response$status_code == 422) {
    return(FALSE)
  }
  if (response$status_code == 200) {
    return(TRUE)
  }
  if (response$status_code == 401) {
    stop("bad token - could not check color. see ?lx_safe_token")
  }

  stop(paste("Error:", response$status_code))
}


