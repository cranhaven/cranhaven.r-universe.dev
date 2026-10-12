# 'LIFX' light color
#'
#' change the state of 'LIFX' lamps
#'
#' @template param_colors
#' @template param_duration
#' @template param_infrared
#' @template param_color_name
#' @template param_fast
#' @param delta if set to TRUE, color values (hue, saturation, brightness, kelvin, infrared) are added to the lights' current values. Can not be used in combination with `color_name`
#' @template param_selector
#' @template param_power
#' @template param_token
#' @return an 'httr' response object (see \code{\link[httr]{response}})
#' @examples
#' \dontrun{
#' lx_color(hue = 200)
#' lx_color(saturation = 0.8)
#' lx_color(hue = 200, saturation = 0.5, brightness = 0.5)
#' lx_color(color_name = 'cyan', brightness = 1)
#' lx_color(kelvin = 5000, fast = TRUE)
#' lx_color(brightness = -0.3, delta = TRUE)
#' }
#' @export
lx_color <- function(hue = NULL, saturation = NULL, brightness = NULL, kelvin = NULL, duration = NULL, infrared = NULL, color_name = NULL,
    fast = FALSE, delta = FALSE, selector = "all", power = NULL, token = lx_get_token()) {

    # check inputs

    if (!is.null(color_name)) {
        if (delta) {
            stop("can not use parameter color_name when delta is set to TRUE")
        }
    }
    assertthat::assert_that(is.logical(fast))

    if (fast & delta) {
        warning("fast = TRUE has no effect if delta = TRUE")
    }

    # assertthat::assert_that(is.logical(delta))
    if (!is.null(hue)) {
        assertthat::assert_that(is.numeric(hue))
    }
    if (!is.null(saturation)) {
        assertthat::assert_that(is.numeric(saturation))
    }
    if (!is.null(brightness)) {
        assertthat::assert_that(is.numeric(brightness))
    }
    if (!is.null(kelvin)) {
        assertthat::assert_that(is.numeric(kelvin))
    }
    if (!is.null(duration)) {
        assertthat::assert_that(is.numeric(duration))
    }
    if (!is.null(infrared)) {
        assertthat::assert_that(is.numeric(infrared))
    }
    # if delta, don't check color (because delta values can be invald colors, such as negative numbers)
    check <- !delta

    if (!all(sapply(list(hue, saturation, brightness, kelvin, color_name), is.null))) {
        color_name <- lx_color_name(color_name = color_name, saturation = saturation, hue = hue, brightness = brightness, kelvin = kelvin,check = check)
    }

    if (!delta) {
        # set the state using 'lx_state' function (which mirrors the 'LIFX' api /state endpoint):
        response <- lx_state(power = power, color_name = color_name, brightness = brightness, infrared = infrared, duration = duration,
            selector = selector, fast = fast, token = token)

    }

    if (delta) {
        # set the state using 'lx_state' function (which mirrors the 'LIFX' api /state/delta endpoint):
        response <- lx_delta(hue = hue, saturation = saturation, brightness = brightness, kelvin = kelvin, duration = duration, infrared = infrared,
            power = power, selector = selector, token = token)
    }
    return(invisible(response))
}



